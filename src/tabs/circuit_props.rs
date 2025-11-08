use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    ops::{Deref, DerefMut},
    sync::Weak,
    time::{Duration, Instant},
};

use eframe::egui::{Color32, Grid, RichText, Ui};
use parking_lot::{RwLock, RwLockWriteGuard};

use crate::{
    app::App,
    circuits::{
        props::PropertyInfo, CircuitImplData, PinType, PropertyChangedParams, TransformSupport,
    },
    editor::BoardEditor,
    pool::get_pooled,
    state::sim::UpdateTaskPool,
    str::ArcStaticStr,
    tabs::{TabCreation, TabImpl},
    vector::Vec2usize,
};

const PROP_ERROR_FADE_TIME: Duration = Duration::from_millis(500);

pub struct CircuitProps {
    last_selection_counter: Option<usize>,
    last_editor: Option<Weak<RwLock<BoardEditor>>>,

    visible_property_list: Vec<ArcStaticStr>,
    visible_property_map: HashMap<ArcStaticStr, PropertyInfo>,

    value_errors: HashMap<ArcStaticStr, (String, Instant)>,

    new_circuit_property_list: Vec<ArcStaticStr>,
    new_circuit_property_map: HashMap<ArcStaticStr, PropertyInfo>,
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
    ) -> Result<(), String> {
        let mut changed_geometry_circuits = BTreeMap::new();
        let board = editor.board().clone();
        let circuits = board.circuits().read();

        // Find circuits with changed deometry, remove them
        for (&id, circuit_imp) in circuit_locks.iter_mut() {
            let mut circuit_info = circuits.get(id).unwrap().info.write();

            let old_size = circuit_info.size;
            let new_size = circuit_imp.imp.size(circuit_info.transform);

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
            let new_pins = circuit_imp.imp.describe_pins(circuit_info.transform);
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

                let realized_pins = Vec::from(new_pins)
                    .into_iter()
                    .enumerate()
                    .map(|(id, pin)| pin.into_realized(circuit.clone(), id))
                    .collect();

                let old_pins = std::mem::replace(circuit_pins.deref_mut(), realized_pins);
                old_circuit_pins.insert(id, old_pins);
            }

            if changed_geometry_circuits.contains_key(&id) {
                let res = editor.tiles.place_circuit(
                    board.circuits().read().get(id).unwrap(),
                    circuit_info.pos,
                    circuit_info.size,
                    circuit_info.transform,
                    &circuit_imp.imp,
                    &circuit_pins,
                    false,
                );

                if let Some(err) = res.get_placement_error() {
                    fail = true;
                }
            } else if !pins_eq {
                if let Err(err) = editor.tiles.replace_pins(
                    id,
                    circuit_info.pos,
                    circuit_info.size,
                    circuit_pins.deref(),
                ) {
                    fail = true;
                }
            };
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
                    editor.tiles.replace_pins(id, info.pos, info.size, pins).ok();
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
        for &id in old_circuit_pins.keys() {
            tasks.add_circuit_task(id, None);
        }

        board.add_tasks(&tasks);

        for &id in circuit_locks.keys() {

            let circuit = circuits.get(id).unwrap();
            let mut circuit_info = circuit.info.write();

            circuit_info.render_size = circuit_info.transform.transform_size(circuit_info.size, Some(TransformSupport::Automatic));
        }

        Ok(())
    }
}

impl TabCreation for CircuitProps {
    fn new(_: &App) -> Self {
        Self {
            last_selection_counter: None,
            last_editor: None,

            visible_property_list: Default::default(),
            visible_property_map: Default::default(),

            value_errors: Default::default(),

            new_circuit_property_list: Default::default(),
            new_circuit_property_map: Default::default(),
        }
    }
}

impl TabImpl for CircuitProps {
    fn update(&mut self, app: &mut App, ui: &mut Ui) {
        let Some(editor_data) = app.last_active_editor.as_ref() else {
            return;
        };

        if editor_data.selected_circuits.is_empty() {
            return;
        }

        let editor_changed = self
            .last_editor
            .as_ref()
            .is_none_or(|le| !editor_data.editor.ptr_eq(le));

        let Some(editor) = editor_data.editor.upgrade() else {
            return;
        };

        let selection_changed =
            self.last_selection_counter != Some(editor_data.selection_update_counter);

        self.last_editor = Some(editor_data.editor.clone());
        self.last_selection_counter = Some(editor_data.selection_update_counter);

        let mut editor = editor.write();
        let board = editor.board().clone();
        let board_circuits = board.circuits().read();

        if editor_changed || selection_changed {
            self.visible_property_list.clear();
            self.visible_property_map.clear();
            self.value_errors.clear();

            let mut first = true;

            for &id in editor_data.selected_circuits.iter() {
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
            return;
        }

        Grid::new("properties").num_columns(2).show(ui, |ui| {
            let mut circuit_locks = BTreeMap::new();
            for &id in editor_data.selected_circuits.iter() {
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

                ui.label(prop.display_name.deref());

                ui.vertical(|ui| {
                    let first_circuit = circuit_locks.values_mut().next().expect("any circuit");
                    let prop_value = first_circuit.imp.get_property_value(id);
                    let Some(prop_value) = prop_value else {
                        ui.horizontal_wrapped(|ui| {
                            ui.label(
                                RichText::new(format!(
                                "Circuit enumerated this property (\"{id}\") but returned no value"
                            ))
                                .color(Color32::RED),
                            );
                        });
                        return;
                    };
                    let new = prop_value.ui(ui);

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

                        let geometry_res = if !prop.affects_geometry_or_pins {
                            Ok(())
                        } else {
                            Self::try_applying_geometry_and_pin_changes(
                                &mut editor,
                                &mut circuit_locks,
                                |cl| {
                                    for (circuit_id, old) in old_values {
                                        let circuit = cl.get_mut(&circuit_id).unwrap();
                                        let value = circuit.imp.get_property_value(id).unwrap();
                                        old.clone_into_dyn(value);
                                    }
                                },
                            )
                        };

                        match geometry_res {
                            Err(why) => {
                                self.value_errors.insert(
                                    id.clone(),
                                    (why, Instant::now() + Duration::from_secs(5)),
                                );
                            }
                            Ok(()) => {
                                self.value_errors.remove(id);
                                for (&circuit_id, circuit) in circuit_locks.iter_mut() {
                                    let circuit = circuit.deref_mut();

                                    let mut params = PropertyChangedParams::default();

                                    circuit.imp.property_changed(
                                        Some((board_circuits.get(circuit_id).unwrap(), &mut circuit.instance)),
                                        id,
                                        &mut params,
                                    );

                                    if params.trigger_signal_update {
                                        let mut tasks = get_pooled::<UpdateTaskPool>();

                                        tasks.add_circuit_task(circuit_id, None);

                                        editor.board().add_tasks(&tasks);
                                    }
                                }
                            }
                        }
                    }

                    if let Some((error, time)) = self.value_errors.get(id) {
                        let remaining_secs = time
                            .checked_duration_since(Instant::now())
                            .map(|d| d.as_secs_f32())
                            .unwrap_or(0.0);

                        let fade = if remaining_secs >= PROP_ERROR_FADE_TIME.as_secs_f32() {
                            1.0
                        } else {
                            remaining_secs / PROP_ERROR_FADE_TIME.as_secs_f32()
                        };

                        ui.horizontal_wrapped(|ui| {
                            ui.label(
                                RichText::new(error.clone())
                                    .color(Color32::RED.gamma_multiply(fade)),
                            );
                        });
                    }

                });
                ui.end_row();
            }
        });

        let now = Instant::now();
        self.value_errors.retain(|_, v| v.1 > now);
    }
}
