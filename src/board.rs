use std::{
    collections::HashMap,
    sync::{Arc, Weak},
};

use parking_lot::{Mutex, RwLock};

use crate::{
    circuits::{Circuit, CircuitBlueprint, CircuitImplData, CircuitInfo, CircuitPin, RealizedPin},
    containers::FixedVec,
    editor::BoardEditor,
    simulation::SimulationCtx,
    state::BoardStateCollection,
    vector::Vec2isize,
    Direction4HalfArray,
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
}

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
            states: RwLock::new(BoardStateCollection::new()),
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

        let mut imp = circuit.imp.write();
        circuits.set(id, circuit.clone());

        imp.instance = imp.imp.create_instance(&circuit);

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
}

#[derive(Default)]
pub struct WirePoint {
    pub directions: Direction4HalfArray<bool>,
}
