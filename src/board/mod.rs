pub mod editor;

use std::{
    collections::HashMap,
    sync::Arc,
};

use parking_lot::RwLock;

use crate::{
    circuits::Circuit, containers::FixedVec, vector::Vec2isize, Direction4HalfArray
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
}

pub struct Wire {
    pub id: usize,

    pub points: Arc<RwLock<HashMap<Vec2isize, WirePoint>>>,
}

#[derive(Default)]
pub struct WirePoint {
    pub directions: Direction4HalfArray<bool>,
}