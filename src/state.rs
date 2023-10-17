use std::{
    any::{Any, TypeId},
    sync::Arc,
    time::Duration,
};

#[cfg(not(feature = "single_thread"))]
use std::thread::{self, JoinHandle};

use crate::{containers::Queue, time::Instant, board::BoardStorage};
use eframe::epaint::Color32;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::{
    board::CircuitBoard, circuits::*, containers::FixedVec, unwrap_option_or_break,
    unwrap_option_or_return, wires::*, Mutex, RwLock,
};

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum UpdateTask {
    CircuitSignals { id: usize, pin: Option<usize> },
    WireState { id: usize, skip_state_ckeck: bool },
    PinInput { circuit: usize, id: usize },
}

#[derive(Default, Clone, Copy, Eq, PartialEq, Serialize, Deserialize, Debug)]
pub enum WireState {
    #[default]
    None,
    True,
    False,
    Error,
}

impl WireState {
    pub fn combine(self, state: WireState) -> WireState {
        match (self, state) {
            (WireState::None, other) | (other, WireState::None) => other,
            (WireState::Error, _) | (_, WireState::Error) => WireState::Error,

            (WireState::True, WireState::False) => WireState::Error,
            (WireState::False, WireState::True) => WireState::Error,

            (WireState::True, WireState::True) => WireState::True,
            (WireState::False, WireState::False) => WireState::False,
        }
    }

    pub const fn color(self) -> Color32 {
        let rgb = match self {
            Self::None => [0, 0, 200],
            Self::True => [0, 255, 0],
            Self::False => [0, 127, 0],
            Self::Error => [200, 0, 0],
        };
        Color32::from_rgb(rgb[0], rgb[1], rgb[2])
    }

    pub fn combine_boolean(self, state: Self, combiner: impl FnOnce(bool, bool) -> bool) -> Self {
        match (self, state) {
            (Self::None, other) | (other, Self::None) => other,
            (Self::Error, _) | (_, Self::Error) => Self::Error,

            (Self::True, Self::False) => combiner(true, false).into(),
            (Self::False, Self::True) => combiner(false, true).into(),

            (Self::True, Self::True) => combiner(true, true).into(),
            (Self::False, Self::False) => combiner(false, false).into(),
        }
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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]

/// Safe versoion of `WireState` that can be passed through `serde_intermediate`
pub struct SafeWireState(pub WireState);

impl<'de> Deserialize<'de> for SafeWireState {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(Self(
            match char::deserialize(deserializer)?.to_ascii_lowercase() {
                'f' => WireState::False,
                't' => WireState::True,
                'e' => WireState::Error,
                _ => WireState::None,
            },
        ))
    }
}

impl Serialize for SafeWireState {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self.0 {
            WireState::None => 'n',
            WireState::True => 't',
            WireState::False => 'f',
            WireState::Error => 'e',
        }
        .serialize(serializer)
    }
}

impl From<WireState> for SafeWireState {
    fn from(value: WireState) -> Self {
        Self(value)
    }
}

impl From<SafeWireState> for WireState {
    fn from(value: SafeWireState) -> Self {
        value.0
    }
}

pub trait InternalCircuitState: Any + Send + Sync {
    fn serialize(&self) -> serde_intermediate::Intermediate {
        ().into()
    }
}

#[derive(Default)]
pub struct CircuitState {
    pub pins: FixedVec<WireState>,
    pub pin_dirs: FixedVec<PinDirection>,
    pub internal: Option<Box<dyn InternalCircuitState>>,
}
impl CircuitState {
    pub fn get_internal<T: InternalCircuitState>(&self) -> Option<&T> {
        self.internal
            .as_ref()
            .map(|b| unsafe { &*(b.as_ref() as *const dyn InternalCircuitState as *const T) })
    }

    pub fn get_internal_mut<T: InternalCircuitState + Default>(&mut self) -> &mut T {
        let b = self.internal.get_or_insert_with(|| Box::<T>::default());
        if (**b).type_id() != TypeId::of::<T>() {
            *b = Box::<T>::default();
        }

        unsafe { &mut *(b.as_mut() as *mut dyn InternalCircuitState as *mut T) }
    }

    pub fn save(&self) -> crate::io::CircuitStateData {
        crate::io::CircuitStateData {
            pins: self.pins.inner().clone(),
            pin_dirs: self.pin_dirs.inner().clone(),
            internal: self
                .internal
                .as_ref()
                .map(|s| s.serialize())
                .unwrap_or_default(),
        }
    }

    fn load(data: &crate::io::CircuitStateData, id: usize, board: &CircuitBoard) -> Self {
        Self {
            pins: FixedVec::from_option_vec(data.pins.clone()),
            pin_dirs: FixedVec::from_option_vec(data.pin_dirs.clone()),
            internal: match &data.internal {
                serde_intermediate::Intermediate::Unit => None,
                data => board
                    .circuits
                    .get(id)
                    .and_then(|c| c.imp.read().load_internal(data)),
            },
        }
    }
}

#[derive(Clone)]
pub struct StateCollection {
    /// State 0 should be always allocated for main [`ActiveCircuitBoard`]
    states: Arc<RwLock<FixedVec<Arc<State>>>>,
}

impl Default for StateCollection {
    fn default() -> Self {
        Self::new()
    }
}

impl StateCollection {
    pub fn new() -> Self {
        Self {
            states: Arc::new(RwLock::new(vec![].into())),
        }
    }

    pub fn from_fixed_vec(vec: FixedVec<Arc<State>>) -> Self {
        Self {
            states: Arc::new(RwLock::new(vec)),
        }
    }

    pub fn create_state(&self, board: Arc<RwLock<CircuitBoard>>) -> Arc<State> {
        let mut vec = self.states.write();
        let id = vec.first_free_pos_filtered(|i| i > 0); // id 0 reserved
        let state = Arc::new(State::new(board, id));
        vec.set(state.clone(), id);
        state
    }

    pub fn update_pin_input(&self, circuit_id: usize, id: usize) {
        for state in self.states.read().iter() {
            state.update_pin_input(circuit_id, id);
        }
    }

    pub fn update_wire(&self, wire: usize, skip_state_ckeck: bool) {
        for state in self.states.read().iter() {
            state.update_wire(wire, skip_state_ckeck);
        }
    }

    pub fn update_circuit_signals(&self, circuit: usize, pin: Option<usize>) {
        for state in self.states.read().iter() {
            state.update_circuit_signals(circuit, pin);
        }
    }

    #[cfg(feature = "single_thread")]
    pub fn update(&self) {
        for state in self.states.read().iter() {
            state.update();
        }
    }

    pub fn reset_wire(&self, wire: usize) {
        for state in self.states.read().iter() {
            state.reset_wire(wire);
        }
    }

    pub fn reset_circuit(&self, circuit: usize) {
        for state in self.states.read().iter() {
            state.reset_circuit(circuit);
        }
    }

    pub fn get(&self, state: usize) -> Option<Arc<State>> {
        self.states.read().get(state).cloned()
    }

    /// Get or create state 0
    pub fn get_or_create_main(&self, board: Arc<RwLock<CircuitBoard>>) -> Arc<State> {
        self.get_or_create(board, 0)
    }

    pub fn get_or_create(&self, board: Arc<RwLock<CircuitBoard>>, id: usize) -> Arc<State> {
        let state = self.states.read().get(id).cloned();
        match state {
            Some(v) => v,
            None => self
                .states
                .write()
                .get_or_create_mut(id, || Arc::new(State::new(board, id)))
                .clone(),
        }
    }

    pub fn init_circuit(&self, circuit: &Circuit, boards: &BoardStorage) {
        for state in self.states.read().iter() {
            state.init_circuit(circuit, boards);
        }
    }

    #[cfg(single_thread)]
    pub fn update(&self) {}

    pub fn states(&self) -> &RwLock<FixedVec<Arc<State>>> {
        self.states.as_ref()
    }

    #[cfg(not(feature = "single_thread"))]
    pub fn activate(&self) {
        for state in self.states.read().iter() {
            state.poke_thread(false, false);
        }
    }

    pub fn set_ordered(&self, ordered: bool) {
        for state in self.states.read().iter() {
            state.set_ordered(ordered);
        }
    }
}

pub struct StateParent {
    pub parent: Arc<State>,
    pub circuit: usize 
}

#[derive(Clone)]
pub struct State {
    pub id: usize,
    pub parent: Arc<RwLock<Option<StateParent>>>,

    pub wires: Arc<RwLock<FixedVec<Arc<RwLock<WireState>>>>>,
    pub circuits: Arc<RwLock<FixedVec<Arc<RwLock<CircuitState>>>>>,

    queue: Arc<Mutex<Queue<UpdateTask>>>,

    #[cfg(not(feature = "single_thread"))]
    thread: Arc<RwLock<Option<StateThreadHandle>>>,

    pub board: Arc<RwLock<CircuitBoard>>,
    circuit_updates_removes: Arc<Mutex<Vec<usize>>>,

    pub updates: Arc<Mutex<Vec<(usize, Instant)>>>,
}

impl State {
    pub fn new(board: Arc<RwLock<CircuitBoard>>, id: usize) -> Self {
        let ordered = board.read().is_ordered_queue();
        Self {
            id,
            parent: Arc::new(RwLock::new(None)),
            wires: Default::default(),
            circuits: Default::default(),
            queue: Arc::new(Mutex::new(Queue::new(vec![], ordered))),
            #[cfg(not(feature = "single_thread"))]
            thread: Default::default(),
            board,
            circuit_updates_removes: Default::default(),
            updates: Default::default(),
        }
    }

    pub fn read_wire(&self, id: usize) -> WireState {
        self.wires
            .read()
            .get(id)
            .map(|s| *s.read())
            .unwrap_or_default()
    }

    pub fn get_wire(&self, id: usize) -> Arc<RwLock<WireState>> {
        self.wires
            .write()
            .get_or_create_mut(id, Default::default)
            .clone()
    }

    pub fn read_circuit(&self, id: usize) -> Option<Arc<RwLock<CircuitState>>> {
        self.circuits.read().get(id).cloned()
    }

    pub fn get_circuit(&self, id: usize) -> Arc<RwLock<CircuitState>> {
        self.circuits
            .write()
            .get_or_create_mut(id, Default::default)
            .clone()
    }

    pub fn set_circuit_update_interval(&self, id: usize, dur: Option<Duration>) {
        let mut updates = self.updates.lock();
        match dur {
            Some(dur) => {
                let index = updates.iter_mut().find(|v| v.0 == id);
                match index {
                    Some(v) => v.1 = Instant::now() + dur,
                    None => {
                        let _i = updates.len();
                        updates.push((id, Instant::now() + dur))
                    }
                }
            }
            None => {
                let index = updates
                    .iter()
                    .enumerate()
                    .find(|(_, t)| t.0 == id)
                    .map(|(i, _)| i);
                if let Some(index) = index {
                    updates.remove(index);
                }
            }
        }

        #[cfg(not(feature = "single_thread"))]
        self.poke_thread(true, false);
    }

    pub fn save(&self) -> crate::io::StateData {
        let now = Instant::now();
        crate::io::StateData {
            wires: self
                .wires
                .read()
                .inner()
                .iter()
                .map(|ws| ws.as_ref().map(|ws| *ws.read()))
                .collect(),
            circuits: self
                .circuits
                .read()
                .inner()
                .iter()
                .map(|cs| cs.as_ref().map(|cs| cs.read().save()))
                .collect(),
            queue: self.queue.lock().iter().copied().collect(),
            updates: self
                .updates
                .lock()
                .iter()
                .map(|(id, time)| (*id, time.checked_duration_since(now)))
                .collect(),
        }
    }

    pub fn load(data: &crate::io::StateData, board: Arc<RwLock<CircuitBoard>>, id: usize) -> State {
        let now = Instant::now();

        let wires = data
            .wires
            .iter()
            .map(|w| w.map(|w| Arc::new(RwLock::new(w))))
            .collect();

        let board_ref = board.read();
        let circuits = data
            .circuits
            .iter()
            .enumerate()
            .map(|(i, c)| {
                c.as_ref()
                    .map(|c| Arc::new(RwLock::new(CircuitState::load(c, i, &board_ref))))
            })
            .collect();
        drop(board_ref);

        let updates = data
            .updates
            .iter()
            .map(|(id, dur)| (*id, dur.map(|d| now + d).unwrap_or(now)))
            .collect();

        let ordered = board.read().is_ordered_queue();
        Self {
            id,
            parent: Arc::new(RwLock::new(None)),
            wires: Arc::new(RwLock::new(FixedVec::from_option_vec(wires))),
            circuits: Arc::new(RwLock::new(FixedVec::from_option_vec(circuits))),
            queue: Arc::new(Mutex::new(Queue::new(data.queue.clone(), ordered))),
            #[cfg(not(feature = "single_thread"))]
            thread: Arc::new(RwLock::new(None)),
            board,
            circuit_updates_removes: Default::default(),
            updates: Arc::new(Mutex::new(updates)),
        }
    }

    pub fn update_wire(&self, wire: usize, skip_state_ckeck: bool) {
        self.schedule_update(UpdateTask::WireState {
            id: wire,
            skip_state_ckeck,
        });
    }

    pub fn update_circuit_signals(&self, circuit: usize, pin: Option<usize>) {
        self.schedule_update(UpdateTask::CircuitSignals { id: circuit, pin });
    }

    pub fn update_pin_input(&self, circuit: usize, id: usize) {
        self.schedule_update(UpdateTask::PinInput { circuit, id });
    }

    pub fn reset_wire(&self, wire: usize) {
        self.wires.write().remove(wire);
    }

    pub fn reset_circuit(&self, circuit: usize) {
        self.circuits.write().remove(circuit);
        self.set_circuit_update_interval(circuit, None);
    }

    pub fn set_ordered(&self, ordered: bool) {
        self.queue.lock().set_ordered(ordered);
    }

    fn schedule_update(&self, task: UpdateTask) {
        let mut queue = self.queue.lock();
        queue.enqueue(task);

        #[cfg(not(feature = "single_thread"))]
        self.poke_thread(true, false);
    }

    #[cfg(feature = "single_thread")]
    pub fn update(&self) {
        self.update_once(5000);
    }

    fn update_once(&self, queue_limit: usize) -> Option<Instant> {
        // Lock shared simulation, so placing/deleting won't interrupt anything
        let sim_lock = { self.board.read().sim_lock.clone() };
        let sim_lock = sim_lock.read();
        let mut circuit_updates_removes = self.circuit_updates_removes.lock();

        let nearest_update = {
            let mut updates = self.updates.lock();
            let mut nearest_update = None;
            let now = Instant::now();
            circuit_updates_removes.clear();
            for (i, upd) in updates.iter_mut().enumerate() {
                if upd.1 <= now {
                    let board = self.board.read();
                    let circ = board.circuits.get(upd.0);
                    if let Some(circ) = circ {
                        let imp = circ.imp.read();

                        let state = CircuitStateContext::new(self, circ);
                        imp.update(&state);
                        match imp.update_interval(&state) {
                            Some(d) => {
                                upd.1 = now + d;
                            }
                            None => circuit_updates_removes.push(i),
                        }
                    } else {
                        circuit_updates_removes.push(i);
                    }
                }
                let closer = match nearest_update {
                    Some(nu) => upd.1 < nu,
                    None => true,
                };
                if closer {
                    nearest_update = Some(upd.1);
                }
            }
            // `circuit_updates_removes` is ordered, removing in reverse order is safe
            for i in circuit_updates_removes.drain(..).rev() {
                updates.remove(i);
            }
            nearest_update
        };

        let mut queue_counter = 0;

        while !nearest_update.is_some_and(|nu| nu <= Instant::now()) && queue_counter < queue_limit
        {
            let board = self.board.read();
            let deq = { self.queue.lock().dequeue() };
            let task = unwrap_option_or_break!(deq);

            match task {
                UpdateTask::WireState {
                    id,
                    skip_state_ckeck,
                } => {
                    if let Some(wire) = board.wires.get(id) {
                        self.update_wire_now(wire, skip_state_ckeck);
                    }
                }
                UpdateTask::CircuitSignals { id, pin } => {
                    if let Some(circuit) = board.circuits.get(id) {
                        self.update_circuit_signals_now(circuit, pin);
                    }
                }
                UpdateTask::PinInput { circuit, id } => {
                    if let Some(circuit) = board.circuits.get(circuit) {
                        self.update_pin_input_now(circuit, id);
                    }
                }
            }
            queue_counter += 1;
        }
        drop(sim_lock);
        match nearest_update {
            Some(t) => Some(t),
            None if queue_counter >= queue_limit => Some(Instant::now()),
            _ => None,
        }
    }

    fn init_circuit(&self, circuit: &Circuit, boards: &BoardStorage) {
        let state_ctx = CircuitStateContext::new(self, circuit);
        let mut imp = circuit.imp.write();
        imp.init_state(&state_ctx);
        imp.postload(&state_ctx, boards);
    }

    fn update_wire_now(&self, wire: &Wire, skip_state_ckeck: bool) {
        let mut state = WireState::None;
        let mut delayed_pins = vec![];
        for (_, point) in wire.points.iter() {
            if let Some(pin_arc) = &point.pin {
                let pin = pin_arc.read();

                match pin.direction(self) {
                    PinDirection::Inside => {}
                    PinDirection::Outside => state = state.combine(pin.get_state(self)),
                    PinDirection::Custom => delayed_pins.push(pin_arc),
                }
            }
        }

        for pin in delayed_pins {
            let pin = pin.read();
            // 2 locks, eugh
            if let PinDirection::Custom = pin.direction(self) {
                if let Some(circuit) = self.board.read().circuits.get(pin.id.circuit_id) {
                    let state_ctx = CircuitStateContext::new(self, circuit);
                    circuit
                        .imp
                        .read()
                        .custom_pin_mutate_state(&state_ctx, pin.id.id, &mut state);
                }
            }
        }

        let current = self.get_wire(wire.id);
        if !skip_state_ckeck && *current.read() == state {
            return;
        }

        *current.write() = state;
        for (_, point) in wire.points.iter() {
            if let Some(pin) = &point.pin {
                let pin = pin.read();

                match pin.direction(self) {
                    PinDirection::Inside => pin.set_input(self, state, true),
                    PinDirection::Outside => {}
                    PinDirection::Custom => pin.set_input(self, state, true),
                }
            }
        }
    }

    fn update_circuit_signals_now(&self, circuit: &Circuit, pin: Option<usize>) {
        circuit
            .imp
            .read()
            .update_signals(&CircuitStateContext::new(self, circuit), pin)
    }

    fn update_pin_input_now(&self, circuit: &Circuit, id: usize) {
        let info = circuit.info.read();
        let pin_info = unwrap_option_or_return!(info.pins.get(id));

        let ctx = CircuitStateContext::new(self, circuit);
        let old_state = pin_info.get_state(&ctx);

        let pin = pin_info.pin.clone();
        let pin = pin.write();

        match pin.direction(self) {
            PinDirection::Inside => {}
            PinDirection::Custom => {}
            PinDirection::Outside => return,
        }

        drop(info);

        let pin_wire = pin.connected_wire();

        let new_state = match pin_wire {
            None => WireState::default(),
            Some(id) => self.read_wire(id),
        };

        if old_state == new_state {
            return;
        }

        pin.set_input(self, new_state, false);
        drop(pin);

        self.update_circuit_signals_now(circuit, Some(id));
    }

    #[cfg(not(feature = "single_thread"))]
    fn poke_thread(&self, notify: bool, termination_req: bool) {
        let thread_sync = {
            let handle = self.thread.read();
            match &*handle {
                None => None,
                Some(handle) => {
                    if handle.handle.is_finished() {
                        None
                    } else {
                        Some(handle.sync.clone())
                    }
                }
            }
        };
        if let Some(sync) = thread_sync {
            if notify || termination_req {
                *sync.1.lock() = termination_req;
                sync.0.notify_one();
            }
            return;
        }

        if termination_req {
            return;
        }

        let clone = self.clone();
        let sync = Arc::new((parking_lot::Condvar::new(), parking_lot::Mutex::new(false)));
        let sync_clone = sync.clone();
        let handle = thread::spawn(move || {
            StateThread::new(clone, sync_clone).run();
        });
        let handle = StateThreadHandle { handle, sync };
        *self.thread.write() = Some(handle);
    }

    pub fn reset(&self) {
        // Important to lock everything, so thread won't do anything
        let mut queue = self.queue.lock();
        let mut circuits = self.circuits.write();
        let mut wires = self.wires.write();

        queue.clear();
        circuits.clear();
        wires.clear();
    }

    pub fn update_everything(&self) {
        let mut queue = self.queue.lock();

        let board = self.board.read();
        for circuit in board.circuits.iter() {
            queue.enqueue(UpdateTask::CircuitSignals {
                id: circuit.id,
                pin: None,
            });
        }
        for wire in board.wires.iter() {
            queue.enqueue(UpdateTask::WireState {
                id: wire.id,
                skip_state_ckeck: true,
            });
        }
        drop(queue);
        #[cfg(not(feature = "single_thread"))]
        self.poke_thread(true, false);
    }

    pub fn queue_len(&self) -> usize {
        self.queue.lock().len()
    }

    pub fn get_self_arc(&self) -> Arc<State> {
        self.board.read().states.get(self.id).expect("this state must exist")
    }
}

impl Drop for State {
    fn drop(&mut self) {
        #[cfg(not(feature = "single_thread"))]
        self.poke_thread(true, true);
    }
}

#[cfg(not(feature = "single_thread"))]
struct StateThreadHandle {
    handle: JoinHandle<()>,
    sync: Arc<(parking_lot::Condvar, parking_lot::Mutex<bool>)>,
}

#[cfg(not(feature = "single_thread"))]
struct StateThread {
    state: State,
    sync: Arc<(parking_lot::Condvar, parking_lot::Mutex<bool>)>,
}

#[cfg(not(feature = "single_thread"))]
impl StateThread {
    pub fn new(state: State, sync: Arc<(parking_lot::Condvar, parking_lot::Mutex<bool>)>) -> Self {
        Self { state, sync }
    }

    fn run(&mut self) {
        loop {
            {
                if *self.sync.1.lock() {
                    return;
                }
            }

            let wait = self.state.update_once(200);

            match wait {
                Some(nu) => {
                    // Lock might block for a bit, so sample duration after it blocked
                    let mut lock = self.sync.1.lock();
                    if let Some(duration) = nu.checked_duration_since(Instant::now()) {
                        let result = self.sync.0.wait_for(&mut lock, duration);
                        if result.timed_out() {
                            continue;
                        } else if *lock {
                            return;
                        }
                    }
                }
                None => {
                    let mut lock = self.sync.1.lock();
                    let result = self.sync.0.wait_for(&mut lock, Duration::from_secs(1));
                    if result.timed_out() || *lock {
                        return;
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {

    fn sync_send<T: Sync + Send>() {}

    #[test]
    fn sync_send_state() {
        sync_send::<super::State>();
    }
}
