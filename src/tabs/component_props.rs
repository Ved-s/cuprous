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
    components::{
        ComponentBlueprint, ComponentImplData, ComponentUpdateReason, PinType,
        PropertyChangedParams, TransformSupport,
        props::{PropertyInfo, PropertyValue},
    },
    editor::{BoardEditor, InWorldError, SelectedBoardItem},
    pool::get_pooled,
    state::sim::UpdateTaskPool,
    str::ArcStaticStr,
    tabs::{TabCreation, TabImpl},
    time::{self, Instant, TimeProvider},
    vector::Vec2usize,
};

const INWORLD_ERROR_DURATION: Duration = Duration::from_secs(5);
const VALUE_ERROR_DURATION: Duration = Duration::from_secs(5);
const VALUE_ERROR_FADE_TIME: Duration = Duration::from_millis(500);

pub struct ComponentProps {
    last_selection_counter: Option<usize>,
    last_editor: Option<Weak<RwLock<BoardEditor>>>,

    visible_property_list: Vec<ArcStaticStr>,
    visible_property_map: HashMap<ArcStaticStr, PropertyInfo>,

    value_errors: HashMap<ArcStaticStr, (String, Instant)>,

    new_component_property_list: Vec<ArcStaticStr>,
    new_component_property_map: HashMap<ArcStaticStr, PropertyInfo>,

    old_value_error_id_range: Option<Range<usize>>,

    blueprint_property_list: Vec<PropertyInfo>,
}

struct OldComponentGeometryData {
    /// None when size didn't change
    size: Option<Vec2usize>,
}

impl ComponentProps {
    fn try_applying_geometry_and_pin_changes(
        editor: &mut BoardEditor,
        component_locks: &mut BTreeMap<usize, RwLockWriteGuard<'_, ComponentImplData>>,
        reset: impl FnOnce(&mut BTreeMap<usize, RwLockWriteGuard<'_, ComponentImplData>>),
        in_world_errors: &mut Vec<InWorldError>,
    ) -> Result<(), String> {
        let mut changed_geometry_components = BTreeMap::new();
        let board = editor.board().clone();
        let components = board.components().read();

        let mut error = None::<String>;
        let mut multiple_errors = false;

        // Find components with changed deometry, remove them
        for (&id, component_imp) in component_locks.iter_mut() {
            let mut component_info = components.get(id).unwrap().info.write();

            let old_size = component_info.size;
            let new_size = component_imp.imp.size(component_info.transform);

            let new_size = component_info
                .transform
                .transform_size(new_size, Some(TransformSupport::Automatic));

            component_info.size = new_size;

            if old_size != new_size {
                editor
                    .tiles
                    .remove_component(id, component_info.pos, old_size);
                changed_geometry_components.insert(
                    id,
                    OldComponentGeometryData {
                        size: Some(old_size),
                    },
                );
                continue;
            }

            let valid = editor.tiles.validate_component_geometry(
                id,
                component_info.pos,
                component_info.size,
                component_info.transform,
                &component_imp.imp,
            );
            if !valid {
                editor
                    .tiles
                    .remove_component(id, component_info.pos, old_size);
                changed_geometry_components.insert(id, OldComponentGeometryData { size: None });
            }
        }

        let mut old_component_pins = BTreeMap::new();

        let mut fail = false;

        let mut disconnected_wires = BTreeSet::new();

        let mut tasks = get_pooled::<UpdateTaskPool>();

        // Update pins, place components with changed geometry
        for (&id, component_imp) in component_locks.iter_mut() {
            let component = components.get(id).unwrap();
            let component_info = component.info.read();
            let mut new_pins = component_imp.imp.describe_pins(component_info.transform);
            let mut component_pins = component.pins.write();

            let pins_eq = component_pins.len() == new_pins.len()
                && new_pins
                    .iter()
                    .zip(component_pins.iter())
                    .all(|(n, o)| n.functionally_equals(&o.desc));

            if !pins_eq {
                for p in component_pins.iter() {
                    let Some(wire) = p.pin.wire.write().take() else {
                        continue;
                    };

                    wire.remove_pin(id, p.pin.id);

                    editor.remove_needless_wire_point(
                        component_info.pos + p.desc.pos.convert(|v| v as isize),
                        &mut tasks,
                    );
                    tasks.clear(); // We do tasks manually in the correct order
                    disconnected_wires.insert(wire.id);
                }

                let orig_size = component_info
                    .transform
                    .transform_size(component_info.size, Some(TransformSupport::Automatic));

                component_info.transform.transform_pins(
                    orig_size,
                    &mut new_pins.iter_mut().map(|p| p.pos_dir_mut()),
                    Some(TransformSupport::Automatic),
                );

                let realized_pins = new_pins
                    .into_iter()
                    .enumerate()
                    .map(|(id, pin)| pin.into_realized(component.clone(), id))
                    .collect();

                let old_pins = std::mem::replace(component_pins.deref_mut(), realized_pins);
                old_component_pins.insert(id, old_pins);
            }

            let res = if changed_geometry_components.contains_key(&id) {
                let res = editor.tiles.place_component(
                    board.components().read().get(id).unwrap(),
                    component_info.pos,
                    component_info.size,
                    component_info.transform,
                    &component_imp.imp,
                    &component_pins,
                    false,
                );

                res.placement_error()
            } else if !pins_eq {
                editor
                    .tiles
                    .replace_pins(
                        id,
                        component_info.pos,
                        component_info.size,
                        component_pins.deref(),
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
                        component_info.pos.convert(|v| v as f32).into(),
                        component_info.size.convert(|v| v as f32).into(),
                    ),
                    INWORLD_ERROR_DURATION,
                    str,
                ));
            }
        }

        // Roll everything back
        if fail {
            for (&id, data) in changed_geometry_components.iter() {
                let mut component_info = components.get(id).unwrap().info.upgradable_read();

                editor
                    .tiles
                    .remove_component(id, component_info.pos, component_info.size);

                if let Some(old_size) = data.size {
                    component_info.with_upgraded(|i| {
                        i.size = old_size;
                    });
                }
            }

            for (&id, pins) in old_component_pins.iter_mut() {
                if !changed_geometry_components.contains_key(&id) {
                    let info = components.get(id).unwrap().info.read();
                    editor
                        .tiles
                        .replace_pins(id, info.pos, info.size, pins)
                        .ok();
                }

                *components.get(id).unwrap().pins.write() = std::mem::take(pins);
            }

            reset(component_locks);

            for &id in changed_geometry_components.keys() {
                let component = components.get(id).unwrap();
                let component_info = component.info.read();
                let component_pins = component.pins.read();
                let component_imp = component_locks.get(&id).unwrap();

                editor.tiles.place_component(
                    board.components().read().get(id).unwrap(),
                    component_info.pos,
                    component_info.size,
                    component_info.transform,
                    &component_imp.imp,
                    &component_pins,
                    true, // it worked before all of this so must be fine now!
                );
            }
        }

        // At this point everything is there but components with changed pins have them disconnected from the wires

        let mut connected_wires = BTreeSet::new();

        // Connect pins and place wires
        for &id in component_locks.keys() {
            if !old_component_pins.contains_key(&id) {
                continue;
            }

            let component = components.get(id).unwrap();
            let component_info = component.info.read();
            let component_pins = component.pins.read();

            for p in component_pins.iter() {
                let pos = component_info.pos + p.desc.pos.convert(|v| v as isize);
                if editor.tiles.should_pin_wire_point_exist(pos) {
                    let wire = editor.set_wire_point(pos, None, true, &mut tasks);
                    connected_wires.insert(wire.id);
                    tasks.clear(); // Manual tasks
                }
            }
        }

        // Add relevant state tasks

        // Update pin states
        for &id in component_locks.keys() {
            if !old_component_pins.contains_key(&id) {
                continue;
            }

            let component = components.get(id).unwrap();
            let component_pins = component.pins.read();

            for p in component_pins.iter() {
                let wire = p.pin.wire.read().clone();
                match p.pin.ty {
                    PinType::Inside => {
                        if let Some(wire) = wire {
                            // If wire won't be updated
                            if !disconnected_wires.contains(&wire.id) {
                                tasks.add_update_input_task(id, p.pin.id, false);
                            }
                        } else {
                            tasks.add_drop_component_task(id, Some(p.pin.id));
                        }
                    }
                    PinType::Outside => {
                        // Component will be updated later, for now just drop it
                        tasks.add_drop_component_task(id, Some(p.pin.id));
                    }
                    PinType::Multiwire => {}
                }
            }
        }

        // Update wires
        for &w in disconnected_wires.union(&connected_wires) {
            tasks.add_wire_task(w, false);
        }

        // Update components
        if !fail {
            for (id, lock) in &mut *component_locks {
                if !old_component_pins.contains_key(id) {
                    continue;
                }

                let component = components.get(*id).unwrap();

                tasks.add_component_task(*id, ComponentUpdateReason::NewPins);

                let lock = lock.deref_mut();

                lock.imp.pins_changed(component, &mut lock.instance);
            }
        }

        board.add_tasks(&tasks);

        for &id in component_locks.keys() {
            let component = components.get(id).unwrap();
            let mut component_info = component.info.write();

            component_info.render_size = component_info
                .transform
                .transform_size(component_info.size, Some(TransformSupport::Automatic));
        }

        if multiple_errors {
            Err("Multiple errors have happened".into())
        } else if let Some(err) = error {
            Err(err)
        } else {
            Ok(())
        }
    }

    fn in_world_components_ui(&mut self, app: &mut App, ui: &mut Ui) {
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
                    Select some components on the board using the Selection tool \
                    or pick a configurable component from the component list.\
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
        let board_components = board.components().read();

        if changed {
            self.visible_property_list.clear();
            self.visible_property_map.clear();
            self.value_errors.clear();

            let mut first = true;

            for &item in editor_data.selection.iter() {
                let SelectedBoardItem::Component { id, .. } = item else {
                    continue;
                };

                let Some(component) = board_components.get(id) else {
                    continue;
                };

                self.new_component_property_list.clear();
                self.new_component_property_map.clear();

                let imp = component.imp.read();
                imp.imp.enum_properties(&mut |p| {
                    if self.new_component_property_map.contains_key(&p.id) {
                        return;
                    };

                    self.new_component_property_list.push(p.id.clone());
                    self.new_component_property_map
                        .insert(p.id.clone(), p.clone());
                });

                if first {
                    self.visible_property_list
                        .clone_from(&self.new_component_property_list);
                    self.visible_property_map
                        .clone_from(&self.new_component_property_map);
                    first = false;
                } else {
                    self.visible_property_map.retain(|id, prop| {
                        let new = self.new_component_property_map.get(id);
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

            self.new_component_property_list.clear();
            self.new_component_property_map.clear();
        }

        if self.visible_property_list.is_empty() {
            ui.centered_and_justified(|ui| {
                Label::new("Selected components have no properties.")
                    .wrap_mode(TextWrapMode::Wrap)
                    .ui(ui);
            });
            return;
        }

        ComponentPropertiesUi::new(ui).show(|mut prop_ui| {
            let mut component_locks = BTreeMap::new();
            for &item in editor_data.selection.iter() {
                let SelectedBoardItem::Component { id, .. } = item else {
                    continue;
                };

                let Some(component) = board_components.get(id) else {
                    continue;
                };

                let imp_lock = component.imp.write();
                component_locks.insert(id, imp_lock);
            }

            for id in self.visible_property_list.iter() {
                let Some(prop) = self.visible_property_map.get(id) else {
                    continue;
                };

                let first_component = component_locks.values_mut().next().expect("any component");
                let prop_value = first_component.imp.get_property_value(id);

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
                        for (&component_id, component) in component_locks.iter_mut() {
                            let Some(prop) = component.imp.get_property_value(id) else {
                                continue;
                            };

                            let old = prop.clone_dyn();
                            new.clone_into_dyn(prop);

                            old_values.insert(component_id, old);
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
                            &mut component_locks,
                            |cl| {
                                for (component_id, old) in old_values {
                                    let component = cl.get_mut(&component_id).unwrap();
                                    let value = component.imp.get_property_value(id).unwrap();
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
                            self.value_errors.insert(
                                id.clone(),
                                (why, time::SYSTEM.now() + VALUE_ERROR_DURATION),
                            );
                        }
                        Ok(()) => {
                            self.value_errors.remove(id);
                            for (&component_id, component) in component_locks.iter_mut() {
                                let component = component.deref_mut();

                                let mut params = PropertyChangedParams::default();

                                component.imp.property_changed(
                                    Some((
                                        board_components.get(component_id).unwrap(),
                                        &mut component.instance,
                                    )),
                                    id,
                                    &mut params,
                                );

                                if params.trigger_update {
                                    let mut tasks = get_pooled::<UpdateTaskPool>();

                                    tasks.add_component_task(
                                        component_id,
                                        ComponentUpdateReason::PropertyChanged(id.clone()),
                                    );

                                    editor.board().add_tasks(&tasks);
                                }

                                if params.invalidate_multiwire_router {
                                    for state in editor.board().states().read().iter() {
                                        if let Some(state) = state.upgrade() {
                                            state.remove_multiwire_router(component_id);
                                        }
                                    }
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

    fn component_blueprint_ui(&mut self, ui: &mut Ui, blueprint: &mut ComponentBlueprint) {
        ComponentPropertiesUi::new(ui).show(|mut prop_ui| {
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

impl TabCreation for ComponentProps {
    fn new(_: &mut App) -> Self {
        Self {
            last_selection_counter: None,
            last_editor: None,

            visible_property_list: Default::default(),
            visible_property_map: Default::default(),

            value_errors: Default::default(),

            new_component_property_list: Default::default(),
            new_component_property_map: Default::default(),

            old_value_error_id_range: None,
            blueprint_property_list: Default::default(),
        }
    }
}

impl TabImpl for ComponentProps {
    fn update(&mut self, app: &mut App, ui: &mut Ui) {
        let Some(SelectedItem::Component(selected_component)) = &app.selected_item else {
            self.in_world_components_ui(app, ui);
            return;
        };

        self.component_blueprint_ui(ui, &mut selected_component.write());
    }
}

struct ComponentPropertiesUi<'a>(&'a mut Ui);
struct ComponentPropertiesUiInner<'a>(&'a mut Ui);

impl<'a> ComponentPropertiesUi<'a> {
    fn new(ui: &'a mut Ui) -> Self {
        Self(ui)
    }

    fn show(self, add_contents: impl FnOnce(ComponentPropertiesUiInner)) {
        Grid::new("properties").num_columns(2).show(self.0, |ui| {
            add_contents(ComponentPropertiesUiInner(ui));
        });
    }
}

impl ComponentPropertiesUiInner<'_> {
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
                            "Component enumerated this property (\"{}\") but returned no value",
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
