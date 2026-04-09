use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    ops::{Deref, DerefMut, Range},
    sync::{Arc, Weak},
    time::Duration,
};

use eframe::egui::{Color32, Grid, Label, Rect, RichText, TextWrapMode, Ui, Widget};
use parking_lot::{RwLock, RwLockWriteGuard};

use crate::{
    app::{App, SelectedItem},
    circuits::{
        CircuitBlueprint, CircuitImplData, CircuitUpdateReason, PinType, PropertyChangedParams, TransformSupport, props::{PropertyInfo, PropertyValue}
    },
    editor::{BoardEditor, InWorldError, SelectedBoardItem},
    pool::get_pooled,
    state::sim::UpdateTaskPool,
    str::ArcStaticStr,
    tabs::{TabCreation, TabImpl},
    vector::Vec2usize,
    time::{self, Instant, TimeProvider}
};

const INWORLD_ERROR_DURATION: Duration = Duration::from_secs(5);
const VALUE_ERROR_DURATION: Duration = Duration::from_secs(5);
const VALUE_ERROR_FADE_TIME: Duration = Duration::from_millis(500);

pub struct CircuitProps {
    last_selection_counter: Option<usize>,
    last_editor: Option<Weak<RwLock<BoardEditor>>>,

    visible_property_list: Vec<ArcStaticStr>,
    visible_property_map: HashMap<ArcStaticStr, PropertyInfo>,

    value_errors: HashMap<ArcStaticStr, (String, Instant)>,

    new_circuit_property_list: Vec<ArcStaticStr>,
    new_circuit_property_map: HashMap<ArcStaticStr, PropertyInfo>,

    old_value_error_id_range: Option<Range<usize>>,

    blueprint_property_list: Vec<PropertyInfo>,
}

struct OldCircuitGeometryData {
    /// None when size didn't change
    size: Option<Vec2usize>,
}

impl CircuitProps {
    fn try_applying_geometry_and_pin_changes(
        editor: &mut BoardEditor,
        circuit_locks: &mut BTreeMap<usize, RwLockWriteGuard<'_, CircuitImplData>>,
        reset: impl FnOnce(&mut BTreeMap<usize, RwLockWriteGuard<'_, CircuitImplData>>),
        in_world_errors: &mut Vec<InWorldError>,
    ) -> Result<(), String> {
        let mut changed_geometry_circuits = BTreeMap::new();
        let board = editor.board().clone();
        let circuits = board.circuits().read();

        let mut error = None::<String>;
        let mut multiple_errors = false;

        // Find circuits with changed deometry, remove them
        for (&id, circuit_imp) in circuit_locks.iter_mut() {
            let mut circuit_info = circuits.get(id).unwrap().info.write();

            let old_size = circuit_info.size;
            let new_size = circuit_imp.imp.size(circuit_info.transform);

            let new_size = circuit_info.transform.transform_size(new_size, Some(TransformSupport::Automatic));

            circuit_info.size = new_size;

            if old_size != new_size {
                editor.tiles.remove_circuit(id, circuit_info.pos, old_size);
                changed_geometry_circuits.insert(
                    id,
                    OldCircuitGeometryData {
                        size: Some(old_size),
                    },
                );
                continue;
            }

            let valid = editor.tiles.validate_circuit_geometry(
                id,
                circuit_info.pos,
                circuit_info.size,
                circuit_info.transform,
                &circuit_imp.imp,
            );
            if !valid {
                editor.tiles.remove_circuit(id, circuit_info.pos, old_size);
                changed_geometry_circuits.insert(id, OldCircuitGeometryData { size: None });
            }
        }

        let mut old_circuit_pins = BTreeMap::new();

        let mut fail = false;

        let mut disconnected_wires = BTreeSet::new();

        let mut tasks = get_pooled::<UpdateTaskPool>();

        // Update pins, place circuits with changed geometry
        for (&id, circuit_imp) in circuit_locks.iter_mut() {
            let circuit = circuits.get(id).unwrap();
            let circuit_info = circuit.info.read();
            let mut new_pins = circuit_imp.imp.describe_pins(circuit_info.transform);
            let mut circuit_pins = circuit.pins.write();

            let pins_eq = circuit_pins.len() == new_pins.len()
                && new_pins
                    .iter()
                    .zip(circuit_pins.iter())
                    .all(|(n, o)| n.functionally_equals(&o.desc));

            if !pins_eq {
                for p in circuit_pins.iter() {
                    let Some(wire) = p.pin.wire.write().take() else {
                        continue;
                    };

                    wire.remove_pin(id, p.pin.id);

                    editor.remove_needless_wire_point(
                        circuit_info.pos + p.desc.pos.convert(|v| v as isize),
                        &mut tasks,
                    );
                    tasks.clear(); // We do tasks manually in the correct order
                    disconnected_wires.insert(wire.id);
                }

                let orig_size = circuit_info.transform.transform_size(circuit_info.size, Some(TransformSupport::Automatic));

                circuit_info.transform.transform_pins(
                    orig_size,
                    &mut new_pins.iter_mut().map(|p| p.pos_dir_mut()),
                    Some(TransformSupport::Automatic),
                );

                let realized_pins = new_pins
                    .into_iter()
                    .enumerate()
                    .map(|(id, pin)| pin.into_realized(circuit.clone(), id))
                    .collect();

                let old_pins = std::mem::replace(circuit_pins.deref_mut(), realized_pins);
                old_circuit_pins.insert(id, old_pins);
            }

            let res = if changed_geometry_circuits.contains_key(&id) {
                let res = editor.tiles.place_circuit(
                    board.circuits().read().get(id).unwrap(),
                    circuit_info.pos,
                    circuit_info.size,
                    circuit_info.transform,
                    &circuit_imp.imp,
                    &circuit_pins,
                    false,
                );

                res.placement_error()
            } else if !pins_eq {
                editor
                    .tiles
                    .replace_pins(
                        id,
                        circuit_info.pos,
                        circuit_info.size,
                        circuit_pins.deref(),
                    )
                    .map_err(Into::into)
            } else {
                Ok(())
            };

            if let Err(e) = res {
                fail = true;

                let str = e.to_string();
                if !multiple_errors {
                    match &error {
                        Some(s) if s == &str => {}
                        Some(_) => multiple_errors = true,
                        None => error = Some(str.clone()),
                    }
                }
                in_world_errors.push(InWorldError::new(
                    Rect::from_min_size(
                        circuit_info.pos.convert(|v| v as f32).into(),
                        circuit_info.size.convert(|v| v as f32).into(),
                    ),
                    INWORLD_ERROR_DURATION,
                    str,
                ));
            }
        }

        // Roll everything back
        if fail {
            for (&id, data) in changed_geometry_circuits.iter() {
                let mut circuit_info = circuits.get(id).unwrap().info.upgradable_read();

                editor
                    .tiles
                    .remove_circuit(id, circuit_info.pos, circuit_info.size);

                if let Some(old_size) = data.size {
                    circuit_info.with_upgraded(|i| {
                        i.size = old_size;
                    });
                }
            }

            for (&id, pins) in old_circuit_pins.iter_mut() {
                if !changed_geometry_circuits.contains_key(&id) {
                    let info = circuits.get(id).unwrap().info.read();
                    editor
                        .tiles
                        .replace_pins(id, info.pos, info.size, pins)
                        .ok();
                }

                *circuits.get(id).unwrap().pins.write() = std::mem::take(pins);
            }

            reset(circuit_locks);

            for &id in changed_geometry_circuits.keys() {
                let circuit = circuits.get(id).unwrap();
                let circuit_info = circuit.info.read();
                let circuit_pins = circuit.pins.read();
                let circuit_imp = circuit_locks.get(&id).unwrap();

                editor.tiles.place_circuit(
                    board.circuits().read().get(id).unwrap(),
                    circuit_info.pos,
                    circuit_info.size,
                    circuit_info.transform,
                    &circuit_imp.imp,
                    &circuit_pins,
                    true, // it worked before all of this so must be fine now!
                );
            }
        }

        // At this point everything is there but circuits with changed pins have them disconnected from the wires

        let mut connected_wires = BTreeSet::new();

        // Connect pins and place wires
        for &id in circuit_locks.keys() {
            if !old_circuit_pins.contains_key(&id) {
                continue;
            }

            let circuit = circuits.get(id).unwrap();
            let circuit_info = circuit.info.read();
            let circuit_pins = circuit.pins.read();

            for p in circuit_pins.iter() {
                let pos = circuit_info.pos + p.desc.pos.convert(|v| v as isize);
                if editor.tiles.should_pin_wire_point_exist(pos) {
                    let wire = editor.set_wire_point(pos, None, true, &mut tasks);
                    connected_wires.insert(wire.id);
                    tasks.clear(); // Manual tasks
                }
            }
        }

        // Add relevant state tasks

        // Update pin states
        for &id in circuit_locks.keys() {
            if !old_circuit_pins.contains_key(&id) {
                continue;
            }

            let circuit = circuits.get(id).unwrap();
            let circuit_pins = circuit.pins.read();

            for p in circuit_pins.iter() {
                let wire = p.pin.wire.read().clone();
                match p.pin.ty {
                    PinType::Inside => {
                        if let Some(wire) = wire {
                            // If wire won't be updated
                            if !disconnected_wires.contains(&wire.id) {
                                tasks.add_update_input_task(id, p.pin.id, false);
                            }
                        } else {
                            tasks.add_drop_circuit_task(id, Some(p.pin.id));
                        }
                    }
                    PinType::Outside => {
                        // Circuit will be updated later, for now just drop it
                        tasks.add_drop_circuit_task(id, Some(p.pin.id));
                    }
                }
            }
        }

        // Update wires
        for &w in disconnected_wires.union(&connected_wires) {
            tasks.add_wire_task(w, false);
        }

        // Update circuits
        if !fail {
            for (id, lock) in &mut *circuit_locks {
                if !old_circuit_pins.contains_key(id) {
                    continue;
                }

                let circuit = circuits.get(*id).unwrap();

                tasks.add_circuit_task(*id, CircuitUpdateReason::NewPins);

                let lock = lock.deref_mut();

                lock.imp.pins_changed(circuit, &mut lock.instance);
            }
        }

        board.add_tasks(&tasks);

        for &id in circuit_locks.keys() {
            let circuit = circuits.get(id).unwrap();
            let mut circuit_info = circuit.info.write();

            circuit_info.render_size = circuit_info
                .transform
                .transform_size(circuit_info.size, Some(TransformSupport::Automatic));
        }

        if multiple_errors {
            Err("Multiple errors have happened".into())
        } else if let Some(err) = error {
            Err(err)
        } else {
            Ok(())
        }
    }

    fn in_world_circuits_ui(&mut self, app: &mut App, ui: &mut Ui) {
        let Some(editor) = app.last_active_editor.as_ref().and_then(Weak::upgrade) else {
            ui.centered_and_justified(|ui| {
                Label::new("No active editor").ui(ui);
            });
            return;
        };

        let board = editor.read().board().clone();

        let Some(editor_data) = app.editor_shared.get_mut(&board.uid()) else {
            ui.centered_and_justified(|ui| {
                Label::new("No active editor").ui(ui);
            });
            return;
        };

        if editor_data.selection.is_empty() {
            ui.centered_and_justified(|ui| {
                Label::new(
                    "\
                    Nothing selected.\n\
                    Select some citcuits on the board using the Selection tool \
                    or pick a configurable circuit from the component list.\
                ",
                )
                .wrap_mode(TextWrapMode::Wrap)
                .ui(ui);
            });
            return;
        }

        let mut changed = self
            .last_editor
            .as_ref()
            .is_none_or(|le| !Arc::downgrade(&editor).ptr_eq(le));

        if !changed {
            changed = self.last_selection_counter != Some(editor_data.selection.update_counter());
        }

        self.last_editor = Some(Arc::downgrade(&editor));
        self.last_selection_counter = Some(editor_data.selection.update_counter());

        let mut editor = editor.write();
        let board = editor.board().clone();
        let board_circuits = board.circuits().read();

        if changed {
            self.visible_property_list.clear();
            self.visible_property_map.clear();
            self.value_errors.clear();

            let mut first = true;

            for &item in editor_data.selection.iter() {
                let SelectedBoardItem::Circuit { id, .. } = item else {
                    continue;
                };

                let Some(circuit) = board_circuits.get(id) else {
                    continue;
                };

                self.new_circuit_property_list.clear();
                self.new_circuit_property_map.clear();

                let imp = circuit.imp.read();
                imp.imp.enum_properties(&mut |p| {
                    if self.new_circuit_property_map.contains_key(&p.id) {
                        return;
                    };

                    self.new_circuit_property_list.push(p.id.clone());
                    self.new_circuit_property_map
                        .insert(p.id.clone(), p.clone());
                });

                if first {
                    self.visible_property_list
                        .clone_from(&self.new_circuit_property_list);
                    self.visible_property_map
                        .clone_from(&self.new_circuit_property_map);
                    first = false;
                } else {
                    self.visible_property_map.retain(|id, prop| {
                        let new = self.new_circuit_property_map.get(id);
                        let Some(new) = new else {
                            return false;
                        };

                        if new.type_id != prop.type_id {
                            return false;
                        }

                        prop.affects_geometry_or_pins |= new.affects_geometry_or_pins;

                        true
                    });

                    self.visible_property_list
                        .retain(|id| self.visible_property_map.contains_key(id));
                }
            }

            self.new_circuit_property_list.clear();
            self.new_circuit_property_map.clear();
        }

        if self.visible_property_list.is_empty() {
            ui.centered_and_justified(|ui| {
                Label::new(
                    "\
                    Select some citcuits on the board using the Selection tool \
                    or pick a configurable circuit from the component list.\
                ",
                )
                .wrap_mode(TextWrapMode::Wrap)
                .ui(ui);
            });
            return;
        }

        CircuitPropertiesUi::new(ui).show(|mut prop_ui| {
            let mut circuit_locks = BTreeMap::new();
            for &item in editor_data.selection.iter() {
                let SelectedBoardItem::Circuit { id, .. } = item else {
                    continue;
                };

                let Some(circuit) = board_circuits.get(id) else {
                    continue;
                };

                let imp_lock = circuit.imp.write();
                circuit_locks.insert(id, imp_lock);
            }

            for id in self.visible_property_list.iter() {
                let Some(prop) = self.visible_property_map.get(id) else {
                    continue;
                };

                let first_circuit = circuit_locks.values_mut().next().expect("any circuit");
                let prop_value = first_circuit.imp.get_property_value(id);

                let error_text = if let Some((error, time)) = self.value_errors.get(id) {
                    let remaining_secs = time
                        .checked_duration_since(time::SYSTEM.now())
                        .map(|d| d.as_secs_f32())
                        .unwrap_or(0.0);

                    let fade = if remaining_secs >= VALUE_ERROR_FADE_TIME.as_secs_f32() {
                        1.0
                    } else {
                        remaining_secs / VALUE_ERROR_FADE_TIME.as_secs_f32()
                    };

                    Some(RichText::new(error.clone()).color(Color32::RED.gamma_multiply(fade)))
                } else {
                    None
                };

                let new = prop_ui.show_property(prop, prop_value, error_text);

                if let Some(new) = new {
                    let mut old_values = BTreeMap::new();

                    {
                        for (&circuit_id, circuit) in circuit_locks.iter_mut() {
                            let Some(prop) = circuit.imp.get_property_value(id) else {
                                continue;
                            };

                            let old = prop.clone_dyn();
                            new.clone_into_dyn(prop);

                            old_values.insert(circuit_id, old);
                        }
                    }

                    if let Some(range) = self.old_value_error_id_range.take() {
                        editor_data
                            .in_world_errors
                            .retain(|e| !range.contains(&e.id()));
                    }

                    let geometry_res = if !prop.affects_geometry_or_pins {
                        Ok(())
                    } else {
                        let next_error_id = InWorldError::read_next_id();
                        let res = Self::try_applying_geometry_and_pin_changes(
                            &mut editor,
                            &mut circuit_locks,
                            |cl| {
                                for (circuit_id, old) in old_values {
                                    let circuit = cl.get_mut(&circuit_id).unwrap();
                                    let value = circuit.imp.get_property_value(id).unwrap();
                                    old.clone_into_dyn(value);
                                }
                            },
                            &mut editor_data.in_world_errors,
                        );
                        let new_next_error_id = InWorldError::read_next_id();

                        if new_next_error_id > next_error_id {
                            self.old_value_error_id_range = Some(next_error_id..new_next_error_id)
                        }
                        res
                    };

                    match geometry_res {
                        Err(why) => {
                            self.value_errors
                                .insert(id.clone(), (why, time::SYSTEM.now() + VALUE_ERROR_DURATION));
                        }
                        Ok(()) => {
                            self.value_errors.remove(id);
                            for (&circuit_id, circuit) in circuit_locks.iter_mut() {
                                let circuit = circuit.deref_mut();

                                let mut params = PropertyChangedParams::default();

                                circuit.imp.property_changed(
                                    Some((
                                        board_circuits.get(circuit_id).unwrap(),
                                        &mut circuit.instance,
                                    )),
                                    id,
                                    &mut params,
                                );

                                if params.trigger_update {
                                    let mut tasks = get_pooled::<UpdateTaskPool>();

                                    tasks.add_circuit_task(circuit_id, CircuitUpdateReason::PropertyChanged(id.clone()));

                                    editor.board().add_tasks(&tasks);
                                }
                            }
                        }
                    }
                }
            }
        });

        let now = time::SYSTEM.now();
        self.value_errors.retain(|_, v| v.1 > now);
    }

    fn circuit_blueprint_ui(
        &mut self,
        ui: &mut Ui,
        blueprint: &mut CircuitBlueprint,
    ) {
        CircuitPropertiesUi::new(ui).show(|mut prop_ui| {
            self.blueprint_property_list.clear();

            blueprint.imp.enum_properties(&mut |info| {
                self.blueprint_property_list.push(info.clone());
            });

            for prop in self.blueprint_property_list.drain(..) {
                let mut value = blueprint.imp.get_property_value(&prop.id);

                let new = prop_ui.show_property(&prop, value.as_deref_mut(), None);

                let Some((new, value)) = new.zip(value) else {
                    continue;
                };

                new.clone_into_dyn(value);

                blueprint.imp.property_changed(
                    None,
                    &prop.id,
                    &mut PropertyChangedParams::default(),
                );

                blueprint.recalculate();
            }
        });
    }
}

impl TabCreation for CircuitProps {
    fn new(_: &mut App) -> Self {
        Self {
            last_selection_counter: None,
            last_editor: None,

            visible_property_list: Default::default(),
            visible_property_map: Default::default(),

            value_errors: Default::default(),

            new_circuit_property_list: Default::default(),
            new_circuit_property_map: Default::default(),

            old_value_error_id_range: None,
            blueprint_property_list: Default::default(),
        }
    }
}

impl TabImpl for CircuitProps {
    fn update(&mut self, app: &mut App, ui: &mut Ui) {
        let Some(SelectedItem::Circuit(selected_circuit)) = &app.selected_item else {
            self.in_world_circuits_ui(app, ui);
            return;
        };

        self.circuit_blueprint_ui(ui, &mut selected_circuit.write());
    }
}

struct CircuitPropertiesUi<'a>(&'a mut Ui);
struct CircuitPropertiesUiInner<'a>(&'a mut Ui);

impl<'a> CircuitPropertiesUi<'a> {
    fn new(ui: &'a mut Ui) -> Self {
        Self(ui)
    }

    fn show(self, add_contents: impl FnOnce(CircuitPropertiesUiInner)) {
        Grid::new("properties").num_columns(2).show(self.0, |ui| {
            add_contents(CircuitPropertiesUiInner(ui));
        });
    }
}

impl CircuitPropertiesUiInner<'_> {
    fn show_property(
        &mut self,
        info: &PropertyInfo,
        prop: Option<&mut dyn PropertyValue>,
        error_text: Option<RichText>,
    ) -> Option<Box<dyn PropertyValue>> {
        let ui = &mut *self.0;
        ui.label(info.display_name.deref());

        let res = ui.vertical(|ui| {
            let Some(prop) = prop else {
                ui.horizontal_wrapped(|ui| {
                    ui.label(
                        RichText::new(format!(
                            "Circuit enumerated this property (\"{}\") but returned no value",
                            info.id
                        ))
                        .color(Color32::RED),
                    );
                });
                return None;
            };
            let new = prop.ui(ui);

            if let Some(err) = error_text {
                ui.horizontal_wrapped(|ui| ui.label(err));
            }

            new
        });

        ui.end_row();

        res.inner
    }
}
