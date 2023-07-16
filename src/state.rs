use std::{
    any::Any,
    sync::{Arc, Mutex, RwLock},
    time::{Duration, Instant},
};

use eframe::epaint::Color32;

use crate::{
    circuits::*,
    containers::{FixedVec, RandomQueue},
    wires::*,
};

pub enum UpdateTask {
    UpdateCircuitSignals { id: usize, pin: Option<usize> },
    UpdateWireState(usize),
}

#[derive(Default, Clone, Copy, Eq, PartialEq)]
pub enum WireState {
    #[default]
    None,
    True,
    False,
    Error,
}

impl WireState {
    pub fn combine(&self, state: WireState) -> WireState {
        match (*self, state) {
            (WireState::None, other) => other,
            (other, WireState::None) => other,
            (WireState::Error, _) => WireState::Error,
            (_, WireState::Error) => WireState::Error,
            (WireState::True, WireState::False) => WireState::Error,
            (WireState::False, WireState::True) => WireState::Error,

            (WireState::True, WireState::True) => WireState::True,
            (WireState::False, WireState::False) => WireState::True,
        }
    }

    pub fn color(&self) -> Color32 {
        let rgb = match self {
            Self::None => [0, 0, 200],
            Self::True => [0, 255, 0],
            Self::False => [0, 127, 0],
            Self::Error => [200, 0, 0],
        };
        Color32::from_rgb(rgb[0], rgb[1], rgb[2])
    }
}

impl From<bool> for WireState {
    fn from(value: bool) -> Self {
        match value {
            true => WireState::True,
            false => WireState::False,
        }
    }
}

pub trait InternalCircuitState: Any + Default {}

#[derive(Default)]
pub struct CircuitState {
    pub pins: FixedVec<WireState>,
    pub pin_dirs: FixedVec<PinDirection>,
    pub internal: Option<Box<dyn Any>>,
}

impl CircuitState {
    pub fn get_internal<T: InternalCircuitState>(&self) -> Option<&T> {
        self.internal.as_ref()?.downcast_ref()
    }

    pub fn get_internal_mut<T: InternalCircuitState>(&mut self) -> &mut T {
        let b = self.internal.get_or_insert_with(|| Box::<T>::default());
        if !b.is::<T>() {
            *b = Box::<T>::default();
        }

        b.downcast_mut().expect("unreachable")
    }
}

pub struct State {
    pub wires: Arc<RwLock<FixedVec<Arc<RwLock<WireState>>>>>,
    pub circuits: Arc<RwLock<FixedVec<Arc<RwLock<CircuitState>>>>>,

    pub queue: Arc<Mutex<RandomQueue<UpdateTask>>>,
    pub updates: Arc<Mutex<Vec<(usize, Instant)>>>,
}

impl Default for State {
    fn default() -> Self {
        Self {
            wires: Arc::new(RwLock::new(vec![].into())),
            circuits: Arc::new(RwLock::new(vec![].into())),
            queue: Arc::new(Mutex::new(RandomQueue::new())),
            updates: Arc::new(Mutex::new(vec![])),
        }
    }
}

impl State {
    pub fn read_wire(&self, id: usize) -> WireState {
        self.wires
            .read()
            .unwrap()
            .get(id)
            .map(|s| *s.read().unwrap())
            .unwrap_or_default()
    }

    pub fn get_wire(&self, id: usize) -> Arc<RwLock<WireState>> {
        self.wires
            .write()
            .unwrap()
            .get_or_create_mut(id, Default::default)
            .clone()
    }

    pub fn read_circuit(&self, id: usize) -> Option<Arc<RwLock<CircuitState>>> {
        self.circuits.read().unwrap().get(id).cloned()
    }

    pub fn get_circuit(&self, id: usize) -> Arc<RwLock<CircuitState>> {
        self.circuits
            .write()
            .unwrap()
            .get_or_create_mut(id, Default::default)
            .clone()
    }

    pub fn update_circuit_interval(&self, id: usize, dur: Duration) {
        let mut updates = self.updates.lock().unwrap();
        let index = updates.iter_mut().find(|v| v.0 == id);
        match index {
            Some(v) => v.1 = Instant::now() + dur,
            None => {
                let _i = updates.len();
                updates.push((id, Instant::now() + dur))
            }
        }
    }

    pub fn update(&self, wires: &Wires, circuits: &Circuits) {
        let mut limit = 10;
        while limit > 0 {
            limit -= 1;

            let queue_item = { self.queue.lock().unwrap().dequeue() };

            if let Some(task) = queue_item {
                match task {
                    UpdateTask::UpdateWireState(wire) => {
                        if let Some(wire) = wires.wires.get(wire) {
                            self.update_wire(wire);
                        }
                    }
                    UpdateTask::UpdateCircuitSignals { id, pin } => {
                        if let Some(circuit) = circuits.cirtuits.get(id) {
                            let state_ctx = CircuitStateContext::new(self, circuit);
                            self.update_circuit_signals(&state_ctx, pin);
                        }
                    }
                }
                continue;
            }

            let mut updates = self.updates.lock().unwrap();
            let now = Instant::now();
            let next = updates.iter_mut().enumerate().find(|(_, i)| i.1 <= now);
            if let Some((i, entry)) = next {
                let remove = if let Some(circ) = circuits.cirtuits.get(entry.0) {
                    let mut imp = circ.imp.write().unwrap();

                    let state = CircuitStateContext::new(self, circ);

                    imp.update(&state);
                    match imp.update_interval(&state) {
                        Some(d) => {
                            entry.1 = now + d;
                            None
                        }
                        None => Some(i),
                    }
                } else {
                    Some(i)
                };

                if let Some(rem) = remove {
                    updates.remove(rem);
                }
                continue;
            }

            
        }
    }

    pub fn update_wire(&self, wire: &Wire) {
        let mut state = WireState::None;
        for (_, point) in wire.nodes.iter() {
            if let Some(pin) = &point.pin {
                let pin = pin.read().unwrap();
                if let PinDirection::Outside = pin.direction(self) {
                    state = state.combine(pin.get_state(self))
                }
            }
        }

        let current = self.get_wire(wire.id);
        if *current.read().unwrap() == state {
            return;
        }

        *current.write().unwrap() = state;
        for (_, point) in wire.nodes.iter() {
            if let Some(pin) = &point.pin {
                let pin = pin.read().unwrap();
                if let PinDirection::Inside = pin.direction(self) {
                    pin.set_input(self, state)
                }
            }
        }
    }

    pub fn update_circuit_signals(&self, state_ctx: &CircuitStateContext, pin: Option<usize>) {
        state_ctx.circuit.imp.write().unwrap().update_signals(
            state_ctx,
            pin,
        )
    }
}
