use std::{collections::HashMap, hash::{DefaultHasher, Hasher}, sync::Arc};

use eframe::egui::Color32;
use parking_lot::RwLock;

use crate::{
    circuits::{Circuit, CircuitBlueprint, CircuitInfo, CircuitPin, RealizedPin},
    containers::FixedVec,
    vector::Vec2isize,
    Direction4HalfArray,
};

pub struct Board {
    pub wires: RwLock<FixedVec<Arc<Wire>>>,
    pub circuits: RwLock<FixedVec<Arc<Circuit>>>,
}

impl Default for Board {
    fn default() -> Self {
        Self {
            wires: RwLock::new(vec![].into()),
            circuits: RwLock::new(vec![].into()),
        }
    }
}

impl Board {
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

    pub fn create_circuit(&self, pos: Vec2isize, blueprint: &CircuitBlueprint) -> Arc<Circuit> {
        let mut circuits = self.circuits.write();

        let id = circuits.first_free_pos();
        let circuit = Circuit {
            id,
            info: RwLock::new(CircuitInfo {
                pos,
                size: blueprint.size,
            }),
            imp: RwLock::new(blueprint.imp.clone()),
            pins: RwLock::new(
                blueprint
                    .pins
                    .iter()
                    .enumerate()
                    .map(|(id, pin)| RealizedPin {
                        desc: pin.clone(),
                        pin: Arc::new(CircuitPin {
                            id,
                            wire: RwLock::new(None),
                        }),
                    })
                    .collect(),
            ),
        };

        let arc = Arc::new(circuit);
        circuits.set(id, arc.clone());
        arc
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
    pub connected_pins: RwLock<Vec<(Arc<Circuit>, Arc<CircuitPin>)>>,
}

impl Wire {
    pub fn add_pin(&self, circuit: Arc<Circuit>, pin: Arc<CircuitPin>) {
        let mut pins = self.connected_pins.write();
        for (c, p) in pins.iter() {
            if c.id == circuit.id && p.id == pin.id {
                return;
            }
        }

        pins.push((circuit, pin));
    }

    pub fn remove_pin(&self, circuit_id: usize, pin_id: usize) {
        self.connected_pins
            .write()
            .retain(|(c, p)| !(c.id == circuit_id && p.id == pin_id));
    }

    pub fn color(&self) -> Color32 {

        // TODO: actual wire color
        let mut hasher = DefaultHasher::new();
        hasher.write_usize(178543947551947561);
        hasher.write_usize(self.id);
        let v = hasher.finish();
        let r = ((v >> 16) & 0xff) as u8;
        let g = ((v >> 8) & 0xff) as u8;
        let b = (v & 0xff) as u8;

        Color32::from_rgb(r, g, b)
    }
}

#[derive(Default)]
pub struct WirePoint {
    pub directions: Direction4HalfArray<bool>,
}
