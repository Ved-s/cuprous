use std::{
    collections::HashMap,
    sync::{Arc, Weak},
};

use parking_lot::RwLock;
use smoldata::raw::RawValue;

use crate::{
    Direction4, Direction4HalfArray,
    components::{
        Component, ComponentBlueprint, ComponentImplData, ComponentInfo, ComponentPin, ComponentTransform,
        TransformSupport,
    },
    containers::FixedVec,
    io::savestate,
    simulation::{SimulationCtx, SimulationStateData},
    state::sim::UpdateTaskPool,
    str::ArcStaticStr,
    vector::Vec2isize,
};

pub struct Board {
    uid: u128,
    name: RwLock<String>,
    wires: RwLock<FixedVec<Arc<Wire>>>,
    components: RwLock<FixedVec<Arc<Component>>>,

    simulation: Weak<SimulationCtx>,
    states: RwLock<Vec<Weak<SimulationStateData>>>,
}

impl Board {
    pub fn uid(&self) -> u128 {
        self.uid
    }

    pub fn wires(&self) -> &RwLock<FixedVec<Arc<Wire>>> {
        &self.wires
    }

    pub fn components(&self) -> &RwLock<FixedVec<Arc<Component>>> {
        &self.components
    }

    pub fn simulation(&self) -> Arc<SimulationCtx> {
        self.simulation.upgrade().expect("simulation state dropped")
    }

    pub fn add_tasks(&self, tasks: &UpdateTaskPool) {
        let states = self.states.read();
        for state in states.iter() {
            let Some(state) = state.upgrade() else {
                continue;
            };

            state.add_tasks(&mut tasks.iter());
        }
    }

    pub fn states(&self) -> &RwLock<Vec<Weak<SimulationStateData>>> {
        &self.states
    }

    pub fn name(&self) -> &RwLock<String> {
        &self.name
    }

    pub fn save(&self) -> savestate::Board {
        savestate::Board {
            uid: self.uid,
            name: self.name.read().clone(),
            wires: self
                .wires
                .read()
                .inner
                .iter()
                .map(|o| o.as_ref().map(|w| w.save()))
                .collect(),
            components: self
                .components
                .read()
                .inner
                .iter()
                .map(|o| o.as_ref().map(|c| c.save()))
                .collect(),
            states: self
                .states
                .read()
                .iter()
                .filter_map(Weak::upgrade)
                .map(|s| s.uid())
                .collect(),
        }
    }

    pub fn preload(data: &mut savestate::Board, sim: &Arc<SimulationCtx>) -> Arc<Self> {
        Arc::new(Board {
            uid: data.uid,
            name: RwLock::new(std::mem::take(&mut data.name)),
            wires: RwLock::new(Vec::with_capacity(data.wires.len()).into()),
            components: RwLock::new(Vec::with_capacity(data.components.len()).into()),
            simulation: Arc::downgrade(sim),

            states: RwLock::new(Vec::with_capacity(data.states.len())),
        })
    }

    pub fn load_stage1_shallow(
        self: &Arc<Self>,
        data: &savestate::Board,
        blueprints: &HashMap<ArcStaticStr, Arc<RwLock<ComponentBlueprint>>>,
    ) {
        let mut wires = self.wires.write();

        for (i, wire_data) in data.wires.iter().enumerate() {
            let Some(wire_data) = wire_data else {
                continue;
            };

            let wire = Wire {
                id: i,
                points: Arc::new(RwLock::new(
                    wire_data
                        .points
                        .iter()
                        .map(|(pos, dirs)| {
                            (
                                *pos,
                                WirePoint {
                                    directions: Direction4HalfArray(*dirs),
                                },
                            )
                        })
                        .collect(),
                )),
                connected_pins: Default::default(),
            };

            wires.set(i, Arc::new(wire));
        }

        drop(wires);

        let mut components = self.components.write();

        for (i, component_data) in data.components.iter().enumerate() {
            let Some(component_data) = component_data else {
                continue;
            };
            let component = Component::preload(i, self, component_data, blueprints);
            components.set(i, Arc::new(component));
        }

        drop(components);
    }

    pub fn load_stage2_components(&self, data: &savestate::Board) {
        let components = self.components.read();

        for (i, component_data) in data.components.iter().enumerate() {
            let Some(component_data) = component_data else {
                continue;
            };

            let component = components.get(i).expect("shallow-loaded component");
            component.load_finish(component_data);
        }

        let wires = self.wires.read();
        for wire in wires.iter() {
            let Some(wire_data) = data.wires.get(wire.id).and_then(Option::as_ref) else {
                continue;
            };

            let mut connected_pins = wire.connected_pins.write();

            for pin_id in &wire_data.connected_pins {
                let pin = components.get(pin_id.component).and_then(|c| {
                    c.pins
                        .read()
                        .iter()
                        .find(|p| p.desc.id == pin_id.name)
                        .map(|p| p.pin.clone())
                });
                let Some(pin) = pin else {
                    // todo: error reporting
                    continue;
                };
                *pin.wire.write() = Some(wire.clone());
                connected_pins.push(pin);
            }
        }

        drop(components);
    }
}

impl Board {
    pub fn new(simulation: &Arc<SimulationCtx>, uid: Option<u128>, name: String) -> Self {
        let uid = uid.unwrap_or_else(|| {
            let mut uid_buf = [0u8; 16];
            if let Err(e) = getrandom::getrandom(&mut uid_buf) {
                panic!("Could not generate a new board uid: {e}")
            }
            u128::from_ne_bytes(uid_buf)
        });

        Self {
            uid,
            name: RwLock::new(name),
            wires: RwLock::new(vec![].into()),
            components: RwLock::new(vec![].into()),
            simulation: Arc::downgrade(simulation),
            states: RwLock::new(vec![]),
        }
    }

    pub fn create_wire(&self) -> Arc<Wire> {
        let mut wires = self.wires.write();
        let id = wires.first_free_pos();
        let wire = Wire {
            id,
            points: Default::default(),
            connected_pins: Default::default(),
        };
        let arc = Arc::new(wire);
        wires.set(id, arc.clone());
        arc
    }

    pub fn free_wire(&self, wire: &Arc<Wire>) {
        let mut wires = self.wires.write();
        let Some(ewire) = wires.inner.get(wire.id) else {
            return;
        };

        if ewire.as_ref().is_some_and(|w| Arc::ptr_eq(w, wire)) {
            wires.remove(wire.id);
        }
    }

    pub fn create_component(
        self: &Arc<Self>,
        pos: Vec2isize,
        blueprint: &ComponentBlueprint,
        overrides: ComponentCreationOverrides,
    ) -> Arc<Component> {
        let mut components = self.components.write();

        let id = components.first_free_pos();

        let component = Component {
            id,
            board: Arc::downgrade(self),
            info: RwLock::new(ComponentInfo {
                pos,
                render_size: blueprint.inner_size,
                size: blueprint.transformed_size,
                transform: blueprint.transform,
            }),
            imp: RwLock::new(ComponentImplData {
                imp: blueprint.imp.clone(),
                instance: Box::new(()),
            }),
            pins: Default::default(),
        };

        let component = Arc::new(component);

        let mut imp = component.imp.write();
        components.set(id, component.clone());

        let mut rebuild_info = false;

        if let Some(config) = overrides.config {
            // todo: error handling
            imp.imp.load_config(config).ok();

            rebuild_info = true;
        }

        if overrides.dir.is_some() || overrides.flip.is_some() {
            rebuild_info = true;
        }

        if rebuild_info {
            let mut info = component.info.write();

            info.transform = ComponentTransform {
                support: imp.imp.transform_support(),
                dir: overrides.dir.unwrap_or(info.transform.dir),
                flip: overrides.flip.unwrap_or(info.transform.flip),
            };

            info.render_size = imp.imp.size(info.transform);
            info.size = info
                .transform
                .transform_size(info.render_size, Some(TransformSupport::Automatic));
        }

        *component.pins.write() = blueprint
            .pins
            .iter()
            .enumerate()
            .map(|(id, pin)| pin.clone().into_realized(component.clone(), id))
            .collect();

        let loaded_instance = overrides.instance.and_then(|i| {
            // todo: error handling
            imp.imp.load_instance(&component, i).ok()
        });

        imp.instance = loaded_instance.unwrap_or_else(|| imp.imp.create_instance(&component));

        drop(imp);

        component
    }

    pub fn free_component(&self, component: &Arc<Component>) {
        let mut components = self.components.write();
        let Some(ecomponent) = components.inner.get(component.id) else {
            return;
        };

        if ecomponent.as_ref().is_some_and(|c| Arc::ptr_eq(c, component)) {
            components.remove(component.id);
        }
    }
}

#[derive(Default, Clone, Copy)]
pub struct ComponentCreationOverrides<'a> {
    pub dir: Option<Direction4>,
    pub flip: Option<bool>,
    pub config: Option<&'a RawValue>,
    pub instance: Option<&'a RawValue>,
}

impl<'a> ComponentCreationOverrides<'a> {
    pub const NONE: Self = Self {
        dir: None,
        flip: None,
        config: None,
        instance: None,
    };
}

pub struct Wire {
    pub id: usize,

    pub points: Arc<RwLock<HashMap<Vec2isize, WirePoint>>>,
    pub connected_pins: RwLock<Vec<Arc<ComponentPin>>>,
}

impl Wire {
    pub fn add_pin(&self, component: Arc<Component>, pin: Arc<ComponentPin>) {
        let mut pins = self.connected_pins.write();
        for p in pins.iter() {
            if p.component.id == component.id && p.id == pin.id {
                return;
            }
        }

        pins.push(pin);
    }

    pub fn remove_pin(&self, component_id: usize, pin_id: usize) {
        self.connected_pins
            .write()
            .retain(|p| !(p.component.id == component_id && p.id == pin_id));
    }

    pub fn save(&self) -> savestate::Wire {
        savestate::Wire {
            points: self
                .points
                .read()
                .iter()
                .map(|(pos, point)| (*pos, point.directions.0))
                .collect(),
            connected_pins: self
                .connected_pins
                .read()
                .iter()
                .map(|p| savestate::PinId {
                    component: p.component.id,
                    name: p.component.pins.read()[p.id].desc.id.clone(),
                })
                .collect(),
        }
    }
}

#[derive(Default)]
pub struct WirePoint {
    pub directions: Direction4HalfArray<bool>,
}
