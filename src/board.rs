use std::{
    collections::HashMap,
    sync::{Arc, Weak},
};

use parking_lot::{Mutex, RwLock};
use smoldata::raw::RawValue;

use crate::{
    circuits::{Circuit, CircuitBlueprint, CircuitImplData, CircuitInfo, CircuitPin, CircuitTransform, RealizedPin, TransformSupport}, containers::FixedVec, editor::BoardEditor, io::savestate, simulation::SimulationCtx, state::BoardStateCollection, vector::Vec2isize, Direction4, Direction4HalfArray
};

pub struct Board {
    uid: u128,
    wires: RwLock<FixedVec<Arc<Wire>>>,
    circuits: RwLock<FixedVec<Arc<Circuit>>>,

    simulation: Arc<SimulationCtx>,
    editor: Mutex<Option<Weak<RwLock<BoardEditor>>>>,
    states: RwLock<BoardStateCollection>,
}

impl Board {
    pub fn uid(&self) -> u128 {
        self.uid
    }

    pub fn wires(&self) -> &RwLock<FixedVec<Arc<Wire>>> {
        &self.wires
    }

    pub fn circuits(&self) -> &RwLock<FixedVec<Arc<Circuit>>> {
        &self.circuits
    }

    pub fn simulation(&self) -> &Arc<SimulationCtx> {
        &self.simulation
    }

    pub fn states(&self) -> &RwLock<BoardStateCollection> {
        &self.states
    }

    pub fn save(&self) -> savestate::Board {
        savestate::Board {
            uid: self.uid,
            wires: self
                .wires
                .read()
                .inner
                .iter()
                .map(|o| o.as_ref().map(|w| w.save()))
                .collect(),
            circuits: self
                .circuits
                .read()
                .inner
                .iter()
                .map(|o| o.as_ref().map(|c| c.save()))
                .collect(),
            states: self.states.read().save(),
        }
    }

    pub fn preload(data: &savestate::Board, sim: Arc<SimulationCtx>) -> Arc<Self> {
        let this = Arc::new(Board {
            uid: data.uid,
            wires: RwLock::new(Vec::with_capacity(data.wires.len()).into()),
            circuits: RwLock::new(Vec::with_capacity(data.circuits.len()).into()),
            simulation: sim,
            editor: Default::default(),
            states: RwLock::new(BoardStateCollection::uninitialized()),
        });

        this.states.write().preload(&data.states, this.clone());

        this
    }

    pub fn load_stage1_shallow(
        self: &Arc<Self>,
        data: &savestate::Board,
        blueprints: &[Arc<RwLock<CircuitBlueprint>>],
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

        let mut circuits = self.circuits.write();

        for (i, circuit_data) in data.circuits.iter().enumerate() {
            let Some(circuit_data) = circuit_data else {
                continue;
            };
            let circuit = Circuit::preload(i, self.clone(), circuit_data, blueprints);
            circuits.set(i, Arc::new(circuit));
        }

        drop(circuits);

        self.states.read().load_stage1_shallow(&data.states);
    }

    pub fn load_stage2_circuits(&self, data: &savestate::Board) {
        let circuits = self.circuits.read();

        for (i, circuit_data) in data.circuits.iter().enumerate() {
            let Some(circuit_data) = circuit_data else {
                continue;
            };

            let circuit = circuits.get(i).expect("shallow-loaded circuit");
            circuit.load_finish(circuit_data);
        }

        let wires = self.wires.read();
        for wire in wires.iter() {
            let Some(wire_data) = data.wires.get(wire.id).and_then(Option::as_ref) else {
                continue;
            };

            let mut connected_pins = wire.connected_pins.write();

            for pin_id in &wire_data.connected_pins {
                let pin = circuits.get(pin_id.circuit).and_then(|c| {
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

        drop(circuits);

        self.states.read().load_stage2_circuits(&data.states);
    }

    pub fn load_stage3_circuit_states(&self, data: &savestate::Board) {
        self.states.read().load_stage3_circuit_states(&data.states);
    }
}

// TODO: properly drop cyclic references on circuit/board removal!

impl Board {
    pub fn new(simulation: Arc<SimulationCtx>) -> Arc<Self> {
        let mut uid_buf = [0u8; 16];
        if let Err(e) = getrandom::getrandom(&mut uid_buf) {
            panic!("Could not generate a new board uid: {e}")
        }

        let this = Arc::new(Self {
            uid: u128::from_ne_bytes(uid_buf),
            wires: RwLock::new(vec![].into()),
            circuits: RwLock::new(vec![].into()),
            simulation,
            editor: Mutex::new(None),
            states: RwLock::new(BoardStateCollection::uninitialized()),
        });

        this.states.write().initialize(this.clone());

        this
    }

    pub fn make_editor(self: &Arc<Self>) -> Arc<RwLock<BoardEditor>> {
        let mut lock = self.editor.lock();
        let existing = lock.as_ref().and_then(|w| w.upgrade());
        if let Some(existing) = existing {
            return existing;
        }

        let editor = Arc::new(RwLock::new(BoardEditor::new(self.clone())));
        *lock = Some(Arc::downgrade(&editor));
        editor
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

    pub fn create_circuit(
        self: &Arc<Self>,
        pos: Vec2isize,
        blueprint: &CircuitBlueprint,
        overrides: CircuitCreationOverrides,
    ) -> Arc<Circuit> {
        let mut circuits = self.circuits.write();

        let id = circuits.first_free_pos();

        let circuit = Circuit {
            id,
            board: self.clone(),
            info: RwLock::new(CircuitInfo {
                pos,
                render_size: blueprint.inner_size,
                size: blueprint.transformed_size,
                transform: blueprint.transform,
            }),
            imp: RwLock::new(CircuitImplData {
                imp: blueprint.imp.clone(),
                instance: Box::new(()),
            }),
            pins: Default::default(),
        };

        let circuit = Arc::new(circuit);

        let mut imp = circuit.imp.write();
        circuits.set(id, circuit.clone());

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
            let mut info = circuit.info.write();

            info.transform = CircuitTransform {
                support: imp.imp.transform_support(),
                dir: overrides.dir.unwrap_or(info.transform.dir),
                flip: overrides.flip.unwrap_or(info.transform.flip),
            };

            info.render_size = imp.imp.size(info.transform);
            info.size = info
                .transform
                .transform_size(info.render_size, Some(TransformSupport::Automatic));
        }

        *circuit.pins.write() = blueprint
            .pins
            .iter()
            .enumerate()
            .map(|(id, pin)| RealizedPin {
                desc: pin.clone(),
                pin: Arc::new(CircuitPin {
                    id,
                    wire: RwLock::new(None),
                    ty: pin.ty,
                    circuit: circuit.clone(),
                }),
            })
            .collect();

        let loaded_instance = overrides.instance.and_then(|i| {
            // todo: error handling
            imp.imp.load_instance(&circuit, i).ok()
        });

        imp.instance = loaded_instance.unwrap_or_else(|| imp.imp.create_instance(&circuit));

        drop(imp);

        circuit
    }

    pub fn free_circuit(&self, circuit: &Arc<Circuit>) {
        let mut circuits = self.circuits.write();
        let Some(ecircuit) = circuits.inner.get(circuit.id) else {
            return;
        };

        if ecircuit.as_ref().is_some_and(|c| Arc::ptr_eq(c, circuit)) {
            circuits.remove(circuit.id);
        }
    }
}

#[derive(Default, Clone, Copy)]
pub struct CircuitCreationOverrides<'a> {
    pub dir: Option<Direction4>,
    pub flip: Option<bool>,
    pub config: Option<&'a RawValue>,
    pub instance: Option<&'a RawValue>,
}

impl<'a> CircuitCreationOverrides<'a> {
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
    pub connected_pins: RwLock<Vec<Arc<CircuitPin>>>,
}

impl Wire {
    pub fn add_pin(&self, circuit: Arc<Circuit>, pin: Arc<CircuitPin>) {
        let mut pins = self.connected_pins.write();
        for p in pins.iter() {
            if p.circuit.id == circuit.id && p.id == pin.id {
                return;
            }
        }

        pins.push(pin);
    }

    pub fn remove_pin(&self, circuit_id: usize, pin_id: usize) {
        self.connected_pins
            .write()
            .retain(|p| !(p.circuit.id == circuit_id && p.id == pin_id));
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
                    circuit: p.circuit.id,
                    name: p.circuit.pins.read()[p.id].desc.id.clone(),
                })
                .collect(),
        }
    }
}

#[derive(Default)]
pub struct WirePoint {
    pub directions: Direction4HalfArray<bool>,
}
