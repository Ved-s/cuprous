use std::{
    any::Any,
    sync::Arc,
    time::{Duration, Instant},
};

use eframe::epaint::Color32;

use crate::{
    circuits::*,
    containers::{FixedVec, RandomQueue},
    wires::*, board::CircuitBoard, RwLock, Mutex,
};

#[derive(Debug)]
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

#[derive(Clone)]
pub struct StateCollection {
    states: Arc<RwLock<FixedVec<Arc<State>>>>,
}

impl StateCollection {

    pub fn new() -> Self {
        Self {
            states: Default::default(),
        }
    }

    pub fn create_state(&self) -> (usize, Arc<State>) {
        let mut vec = self.states.write().unwrap();
        let id = vec.first_free_pos();
        let state = Arc::<State>::default();
        vec.set(state.clone(), id);
        (id, state)
    }

    pub fn update_wire(&self, wire: usize) {
        for state in self.states.read().unwrap().iter() {
            state.update_wire(wire);
        }
    }

    pub fn update_circuit_signals(&self, circuit: usize, pin: Option<usize>) {
        for state in self.states.read().unwrap().iter() {
            state.update_circuit_signals(circuit, pin);
        }
    }

    pub fn update_circuit_interval(&self, id: usize, dur: Duration) {
        for state in self.states.read().unwrap().iter() {
            state.update_circuit_interval(id, dur);
        }
    }

    pub fn get(&self, state: usize) -> Option<Arc<State>> {
        self.states.read().unwrap().get(state).cloned()
    }
}

pub struct State {
    pub wires: Arc<RwLock<FixedVec<Arc<RwLock<WireState>>>>>,
    pub circuits: Arc<RwLock<FixedVec<Arc<RwLock<CircuitState>>>>>,

    queue: Arc<Mutex<RandomQueue<UpdateTask>>>,
    pub updates: Arc<Mutex<Vec<(usize, Instant)>>>,
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

    pub fn update(&self, board: &CircuitBoard) {
        let mut limit = 10;
        while limit > 0 {
            limit -= 1;

            let queue_item = { self.queue.lock().unwrap().dequeue() };

            if let Some(task) = queue_item {
                match task {
                    UpdateTask::UpdateWireState(wire) => {
                        if let Some(wire) = board.wires.get(wire) {
                            self.update_wire_now(wire);
                        }
                    }
                    UpdateTask::UpdateCircuitSignals { id, pin } => {
                        if let Some(circuit) = board.circuits.get(id) {
                            self.update_circuit_signals_now(circuit, pin);
                        }
                    }
                }
                continue;
            }

            let mut updates = self.updates.lock().unwrap();
            let now = Instant::now();
            let next = updates.iter_mut().enumerate().find(|(_, i)| i.1 <= now);
            if let Some((i, entry)) = next {
                let remove = if let Some(circ) = board.circuits.get(entry.0) {
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

    fn update_wire_now(&self, wire: &Wire) {
        let mut state = WireState::None;
        let mut writing_pins = 0;
        for (_, point) in wire.points.iter() {
            if let Some(pin) = &point.pin {
                let pin = pin.read().unwrap();
                if let PinDirection::Outside = pin.direction(self) {
                    state = state.combine(pin.get_state(self));
                    writing_pins += 1;
                }
            }
        }

        let current = self.get_wire(wire.id);
        if *current.read().unwrap() == state {
            println!("false wire {} update ({writing_pins} in pins)", wire.id);
            return;
        }

        *current.write().unwrap() = state;
        for (_, point) in wire.points.iter() {
            if let Some(pin) = &point.pin {
                let pin = pin.read().unwrap();
                if let PinDirection::Inside = pin.direction(self) {
                    pin.set_input(self, state)
                }
            }
        }
    }

    fn update_circuit_signals_now(&self, circuit: &Circuit, pin: Option<usize>) {
        circuit.imp.write().unwrap().update_signals(
            &CircuitStateContext::new(self, circuit),
            pin,
        )
    }

    pub fn update_wire(&self, wire: usize) {
        self.schedule_update(UpdateTask::UpdateWireState(wire));
    }

    pub fn update_circuit_signals(&self, circuit: usize, pin: Option<usize>) {
        self.schedule_update(UpdateTask::UpdateCircuitSignals { id: circuit, pin });
    }

    fn schedule_update(&self, task: UpdateTask) {
        // TODO: use condvar for future enqueues
        let mut queue = self.queue.lock().unwrap();
        queue.enqueue(task);
    }
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