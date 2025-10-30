use std::any::Any;

use crate::{containers::FixedVec, state::wires::WireState};

#[derive(Default)]
pub struct CircuitState {
    pub pins: Vec<WireState>,
    pub internal: Option<Box<dyn Any + Send + Sync>>,
}

#[derive(Default)]
pub struct BoardCircuitsState {
    pub inner: FixedVec<CircuitState>,
}

impl BoardCircuitsState {
    pub fn get_pin(&self, circuit: usize, id: usize) -> WireState {
        self.inner
            .get(circuit)
            .and_then(|c| c.pins.get(id).cloned())
            .unwrap_or_default()
    }

    /// Returns true if value was changed
    pub fn set_pin(&mut self, circuit: usize, id: usize, state: WireState) -> bool {
        let circuit = self.inner.get_or_create_mut(circuit, Default::default);

        if circuit.pins.len() <= id {
            if state == WireState::default() {
                return false;
            }

            let add = id - circuit.pins.len() + 1;
            circuit.pins.reserve(add);
            for _ in 0..add {
                circuit.pins.push(WireState::default());
            }
        }

        if circuit.pins[id] == state {
            return false;
        }

        circuit.pins[id] = state;
        true
    }

    pub fn read_internal_circuit_state<S>(&self, id: usize) -> Option<&S>
    where
        S: 'static,
    {
        let circuit = self.inner.get(id)?;
        let internal = circuit.internal.as_ref()?.downcast_ref()?;
        Some(internal)
    }

    pub fn write_internal_circuit_state<S>(&mut self, id: usize) -> &mut S
    where
        S: Default + Send + Sync + 'static,
    {
        let existing = self.inner.get_or_create_mut(id, Default::default);

        let state = existing.internal.get_or_insert_with(|| Box::new(<S as Default>::default()));

        if state.downcast_ref::<S>().is_none() {
            *state = Box::new(<S as Default>::default());
        }

        state.downcast_mut().unwrap()
    }
    
    pub fn drop_circuit(&mut self, id: usize) {
        self.inner.remove(id);
    }
    
    pub fn reset(&mut self) {
        self.inner.clear();
    }
}
