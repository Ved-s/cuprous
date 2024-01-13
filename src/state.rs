use std::{
    any::{Any, TypeId},
    borrow::BorrowMut,
    collections::BTreeMap,
    iter::Peekable,
    ops::{Deref, IndexMut},
    str::Chars,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc,
    },
    time::Duration,
};

#[cfg(not(feature = "single_thread"))]
use std::thread::{self, JoinHandle};

use crate::{
    app::Style, containers::Queue, error::ErrorList, pool::PooledStateVec, time::Instant,
    unwrap_option_or_continue, wires::WireColors,
};
use eframe::epaint::Color32;
use object_pool::Pool;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::{
    board::CircuitBoard, circuits::*, containers::FixedVec, unwrap_option_or_break,
    unwrap_option_or_return, Mutex, RwLock,
};

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum UpdateTask {
    CircuitSignals { id: usize, pin: Option<usize> },
    WireState { id: usize, skip_state_ckeck: bool },
    PinInput { circuit: usize, id: usize },
}

#[derive(Default, Clone, Eq, PartialEq, Serialize, Deserialize, Debug)]
pub enum WireState {
    #[default]
    None,
    True,
    False,
    Error,

    Bundle(Arc<PooledStateVec>),
}

#[derive(Default, Clone, Copy, Eq, PartialEq, Debug)]
pub enum SingleWireState {
    #[default]
    None,
    True,
    False,
    Error,
}

impl From<bool> for SingleWireState {
    fn from(value: bool) -> Self {
        match value {
            true => Self::True,
            false => Self::False,
        }
    }
}

impl WireState {
    pub fn set(&mut self, index: usize, value: WireState) {
        match self {
            WireState::Bundle(bundle) => {
                let bundle = Arc::make_mut(bundle);
                if bundle.len() <= index {
                    bundle.push(WireState::None);
                }
                *bundle.index_mut(index) = value;
            }
            this => *this = value,
        }
    }

    pub fn get(&self, index: usize) -> WireState {
        match self {
            WireState::Bundle(bundle) => bundle.get(index).cloned().unwrap_or_default(),
            _ => self.clone(),
        }
    }

    pub fn bundle_len(&self) -> Option<usize> {
        match self {
            WireState::Bundle(bundle) => Some(bundle.len()),
            _ => None,
        }
    }

    pub fn as_single(&self) -> Result<SingleWireState, &Arc<PooledStateVec>> {
        match self {
            Self::None => Ok(SingleWireState::None),
            Self::True => Ok(SingleWireState::True),
            Self::False => Ok(SingleWireState::False),
            Self::Error => Ok(SingleWireState::Error),
            Self::Bundle(bundle) => Err(bundle),
        }
    }

    pub fn as_single_mut(&mut self) -> Result<SingleWireState, &mut Arc<PooledStateVec>> {
        match self {
            Self::None => Ok(SingleWireState::None),
            Self::True => Ok(SingleWireState::True),
            Self::False => Ok(SingleWireState::False),
            Self::Error => Ok(SingleWireState::Error),
            Self::Bundle(bundle) => Err(bundle),
        }
    }

    pub fn str_serialize(&self, str: &mut String) {
        match self {
            WireState::None => str.push('n'),
            WireState::True => str.push('t'),
            WireState::False => str.push('f'),
            WireState::Error => str.push('e'),
            WireState::Bundle(bundle) => {
                str.push('[');
                for state in bundle.iter() {
                    state.str_serialize(str);
                }
                str.push(']');
            }
        }
    }

    pub fn str_deserialize(chars: &mut Peekable<Chars>) -> Option<WireState> {
        let char = chars.next();
        let char = unwrap_option_or_return!(char, None);
        Some(match char {
            't' => WireState::True,
            'f' => WireState::False,
            'e' => WireState::Error,
            '[' => {
                let mut bundle = PooledStateVec::new();
                loop {
                    let next = chars.peek();
                    let next = unwrap_option_or_break!(next);
                    if *next == ']' {
                        break;
                    }
                    let wire = Self::str_deserialize(chars).unwrap_or_default();
                    bundle.push(wire);
                }
                WireState::Bundle(Arc::new(bundle))
            }
            ']' => return None,
            _ => WireState::None,
        })
    }

    pub fn merge(&mut self, state: &WireState) {
        self.combine(state, &|a, b| match (a, b) {
            (SingleWireState::None, other) => other,
            (other, SingleWireState::None) => other,

            (SingleWireState::Error, _) | (_, SingleWireState::Error) => SingleWireState::Error,

            (SingleWireState::True, SingleWireState::False) => SingleWireState::Error,
            (SingleWireState::False, SingleWireState::True) => SingleWireState::Error,

            (SingleWireState::True, SingleWireState::True) => SingleWireState::True,
            (SingleWireState::False, SingleWireState::False) => SingleWireState::False,
        })
    }

    pub fn combine(
        &mut self,
        state: &WireState,
        combiner: &impl Fn(SingleWireState, SingleWireState) -> SingleWireState,
    ) {
        let self_single = self.as_single_mut();
        let other_single = state.as_single();

        let out = match (self_single, other_single) {
            (Ok(a), Ok(b)) => Some(combiner(a, b).into()),
            (Ok(a), Err(b)) => {
                let mut bundle = PooledStateVec::new();
                bundle.extend(b.iter().map(|b| {
                    let mut ws: WireState = a.into();
                    ws.combine(b, combiner);
                    ws
                }));
                Some(WireState::Bundle(Arc::new(bundle)))
            }
            (Err(a), Ok(b)) => {
                let a = Arc::make_mut(a);
                for a in a.iter_mut() {
                    a.combine(&b.into(), combiner);
                }
                None
            }
            (Err(a), Err(b)) => {
                let a = Arc::make_mut(a);
                while a.len() < b.len() {
                    a.push(WireState::None);
                }
                for (a, b) in a.iter_mut().zip(b.iter()) {
                    a.combine(b, combiner);
                }
                None
            }
        };

        if let Some(out) = out {
            *self = out;
        }
    }

    pub fn color(&self, style: &Style, colors_override: Option<&WireColors>) -> Color32 {
        match self {
            Self::None => colors_override
                .and_then(|c| c.none)
                .unwrap_or_else(|| style.wire_colors.none_color()),
            Self::True => colors_override
                .and_then(|c| c.r#true)
                .unwrap_or_else(|| style.wire_colors.true_color()),
            Self::False => colors_override
                .and_then(|c| c.r#false)
                .unwrap_or_else(|| style.wire_colors.false_color()),
            Self::Error => colors_override
                .and_then(|c| c.error)
                .unwrap_or_else(|| style.wire_colors.error_color()),
            Self::Bundle(_) => colors_override
                .and_then(|c| c.bundle)
                .unwrap_or_else(|| style.wire_colors.bundle_color()),
        }
    }

    pub fn combine_boolean(&mut self, state: &WireState, combiner: &impl Fn(bool, bool) -> bool) {
        self.combine(state, &|a, b| match (a, b) {
            (SingleWireState::None, other) | (other, SingleWireState::None) => other,
            (SingleWireState::Error, _) | (_, SingleWireState::Error) => SingleWireState::Error,

            (SingleWireState::True, SingleWireState::False) => combiner(true, false).into(),
            (SingleWireState::False, SingleWireState::True) => combiner(false, true).into(),

            (SingleWireState::True, SingleWireState::True) => combiner(true, true).into(),
            (SingleWireState::False, SingleWireState::False) => combiner(false, false).into(),
        })
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

impl From<SingleWireState> for WireState {
    fn from(value: SingleWireState) -> Self {
        match value {
            SingleWireState::None => Self::None,
            SingleWireState::True => Self::True,
            SingleWireState::False => Self::False,
            SingleWireState::Error => Self::Error,
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]

/// Safe versoion of `WireState` that can be passed through `serde_intermediate`
pub struct SafeWireState(pub WireState);

impl<'de> Deserialize<'de> for SafeWireState {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let str = String::deserialize(deserializer)?;
        Ok(Self(
            WireState::str_deserialize(&mut str.chars().peekable()).unwrap_or_default(),
        ))
    }
}

impl Serialize for SafeWireState {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut str = String::new();
        self.0.str_serialize(&mut str);
        str.serialize(serializer)
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
    fn serialize(&self, copy: bool) -> serde_intermediate::Intermediate {
        let _ = copy;
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

    pub fn set_internal<T: InternalCircuitState>(&mut self, value: Option<T>) {
        self.internal = value.map(|v| Box::new(v) as Box<dyn InternalCircuitState>);
    }

    pub fn save(&self) -> crate::io::CircuitStateData {
        crate::io::CircuitStateData {
            pins: self.pins.inner.clone(),
            pin_dirs: self.pin_dirs.inner.clone(),
            internal: self
                .internal
                .as_ref()
                .map(|s| s.serialize(false))
                .unwrap_or_default(),
        }
    }

    fn load(
        data: &crate::io::CircuitStateData,
        circ: &CircuitStateContext,
        errors: &mut ErrorList,
    ) -> Self {
        Self {
            pins: FixedVec::from_option_vec(data.pins.clone()),
            pin_dirs: FixedVec::from_option_vec(data.pin_dirs.clone()),
            internal: match &data.internal {
                serde_intermediate::Intermediate::Unit => None,
                data => circ
                    .circuit
                    .imp
                    .read()
                    .load_internal(circ, data, false, errors),
            },
        }
    }
}

pub struct StateCollection {
    /// State 0 should be always allocated for main [`ActiveCircuitBoard`]
    pub states: RwLock<FixedVec<Arc<State>>>,
}

impl Default for StateCollection {
    fn default() -> Self {
        Self::new()
    }
}

impl StateCollection {
    pub fn new() -> Self {
        Self {
            states: RwLock::new(vec![].into()),
        }
    }

    pub fn from_fixed_vec(states: FixedVec<Arc<State>>) -> Self {
        Self {
            states: RwLock::new(states),
        }
    }

    pub fn create_state(&self, board: Arc<CircuitBoard>) -> Arc<State> {
        let mut states = self.states.write();
        let id = states.first_free_pos_filtered(|i| i > 0); // id 0 reserved
        let state = State::new(board, id);
        states.set(id, state.clone());
        state
    }

    pub fn update_pin_input(&self, circuit_id: usize, id: usize) {
        for state in self.states.read().iter() {
            state.update_pin_input(circuit_id, id, false, None);
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

    pub fn reset_circuit(&self, circuit: &Arc<Circuit>) {
        for state in self.states.read().iter() {
            state.remove_circuit_state(circuit);
        }
    }

    pub fn get(&self, state: usize) -> Option<Arc<State>> {
        self.states.read().get(state).cloned()
    }

    /// Get or create state 0
    pub fn get_or_create_main(&self, board: Arc<CircuitBoard>) -> Arc<State> {
        self.get_or_create(board, 0)
    }

    pub fn get_or_create(&self, board: Arc<CircuitBoard>, id: usize) -> Arc<State> {
        let state = self.states.read().get(id).cloned();
        match state {
            Some(v) => v,
            None => self
                .states
                .write()
                .get_or_create_mut(id, || State::new(board, id))
                .clone(),
        }
    }

    pub fn init_circuit_state(&self, circuit: &Arc<Circuit>, first_init: bool) {
        for state in self.states.read().iter() {
            state.init_circuit_state(circuit, first_init);
        }
    }

    #[cfg(single_thread)]
    pub fn update(&self) {}

    pub fn initialize(&self) {
        for state in self.states.read().iter() {
            state.init_circuit_states(false);
        }

        for state in self.states.read().iter() {
            state.set_frozen(false);
        }
    }

    pub fn set_ordered(&self, ordered: bool) {
        for state in self.states.read().iter() {
            state.set_ordered(ordered);
        }
    }
}

pub struct CircuitUpdateInfo {
    pub id: usize,
    pub time_override: bool,
    pub next_time: Instant,
    pub interval: Option<Duration>,
}

#[derive(Clone)]
pub struct StateParent {
    pub state: Arc<State>,
    pub circuit: Arc<Circuit>,
}

// TODO: remember freezing time to properly restore update timings
pub struct State {
    pub id: usize,

    pub wires: RwLock<FixedVec<RwLock<WireState>>>,
    pub circuits: RwLock<FixedVec<RwLock<CircuitState>>>,

    queue: Mutex<Queue<UpdateTask>>,

    #[cfg(not(feature = "single_thread"))]
    thread: RwLock<Option<StateThreadHandle>>,

    pub board: Arc<CircuitBoard>,
    circuit_updates_removes: Mutex<Vec<usize>>,

    pub updates: Mutex<Vec<CircuitUpdateInfo>>,
    parent: RwLock<Option<StateParent>>,
    children: AtomicUsize,
    frozen: AtomicBool,

    pin_vec_pool: Pool<Vec<Arc<RwLock<CircuitPin>>>>,
    vis_pool: Pool<VisitedList>,
}

impl State {
    pub fn new(board: Arc<CircuitBoard>, id: usize) -> Arc<Self> {
        let ordered = board.is_ordered_queue();
        let state = Arc::new(Self {
            id,
            parent: RwLock::new(None),
            wires: Default::default(),
            circuits: Default::default(),
            queue: Mutex::new(Queue::new(vec![], ordered)),
            #[cfg(not(feature = "single_thread"))]
            thread: Default::default(),
            board,
            circuit_updates_removes: Default::default(),
            updates: Default::default(),
            frozen: AtomicBool::new(false),
            children: AtomicUsize::new(0),
            pin_vec_pool: Pool::new(2, Vec::new),
            vis_pool: Pool::new(2, VisitedList::new),
        });
        state.init_circuit_states(true);
        state
    }

    pub fn get_wire(&self, id: usize) -> WireState {
        self.wires
            .read()
            .get(id)
            .map(|s| s.read().clone())
            .unwrap_or_default()
    }

    pub fn set_wire(&self, id: usize, state: WireState) {
        FixedVec::read_or_create_locked(
            &self.wires,
            id,
            || RwLock::new(WireState::None),
            |lock| *lock.write() = state,
        );
    }

    pub fn write_wire<R>(&self, id: usize, writer: impl FnOnce(&mut WireState) -> R) -> R {
        FixedVec::read_or_create_locked(
            &self.wires,
            id,
            || RwLock::new(WireState::None),
            |lock| writer(lock.write().borrow_mut()),
        )
    }

    pub fn get_wire_color(&self, id: usize, style: &Style) -> Color32 {
        let colors = self.board.wires.read().get(id).map(|w| w.colors);
        self.get_wire(id).color(style, colors.as_ref())
    }

    pub fn read_circuit<R>(&self, id: usize, reader: impl FnOnce(&CircuitState) -> R) -> Option<R> {
        self.circuits
            .read()
            .get(id)
            .map(|lock| reader(&lock.read()))
    }

    // TODO: make conditional read/write
    pub fn write_circuit<R>(&self, id: usize, writer: impl FnOnce(&mut CircuitState) -> R) -> R {
        FixedVec::read_or_create_locked(&self.circuits, id, Default::default, |lock| {
            writer(lock.write().borrow_mut())
        })
    }

    pub fn set_circuit_update_interval(self: &Arc<Self>, id: usize, dur: Duration) {
        let mut updates = self.updates.lock();

        let index = updates.iter_mut().find(|v| v.id == id);
        match index {
            Some(v) => {
                if !v.time_override {
                    v.next_time = Instant::now() + dur;
                }
                v.interval = Some(dur);
            }
            None => {
                updates.push(CircuitUpdateInfo {
                    id,
                    time_override: false,
                    next_time: Instant::now() + dur,
                    interval: Some(dur),
                })
            }
        }

        #[cfg(not(feature = "single_thread"))]
        self.wake_thread(true);
    }

    pub fn reset_circuit_update_interval(&self, id: usize) {
        let mut updates = self.updates.lock();

        let index = updates
            .iter()
            .enumerate()
            .find(|(_, t)| t.id == id)
            .map(|(i, _)| i);
        if let Some(index) = index {
            updates.remove(index);
        }
    }

    pub fn set_frozen(self: &Arc<Self>, frozen: bool) {
        self.frozen.store(frozen, Ordering::Relaxed);

        #[cfg(not(feature = "single_thread"))]
        self.wake_thread(true);
    }

    pub fn is_frozen(&self) -> bool {
        self.frozen.load(Ordering::Relaxed)
    }

    pub fn is_being_used(self: &Arc<Self>) -> bool {
        self.get_parent().is_some_and(|p| p.state.is_being_used())
            || self.id == 0
            || Arc::strong_count(self) - self.children.load(Ordering::Relaxed) > 1
            || Arc::weak_count(self) > 0
    }

    pub fn save(&self) -> crate::io::StateData {
        let now = Instant::now();
        let circuits = self.board.circuits.read();
        crate::io::StateData {
            wires: self
                .wires
                .read()
                .inner
                .iter()
                .map(|ws| ws.as_ref().map(|ws| ws.read().clone()))
                .collect(),
            circuits: self
                .circuits
                .read()
                .inner
                .iter()
                .enumerate()
                .map(|(i, cs)| {
                    cs.as_ref()
                        .filter(|_| circuits.exists(i))
                        .map(|cs| cs.read().save())
                })
                .collect(),
            queue: self.queue.lock().iter().copied().collect(),
            updates: self
                .updates
                .lock()
                .iter()
                .map(|info| (info.id, info.next_time.checked_duration_since(now)))
                .collect(),
        }
    }

    // State will be loaded frozen. activate it when ready
    pub fn load(
        data: &crate::io::StateData,
        board: Arc<CircuitBoard>,
        id: usize,
        errors: &mut ErrorList,
    ) -> Arc<State> {
        let mut errors = errors.enter_context(|| format!("loading state {}", id));
        let now = Instant::now();

        let wires = data
            .wires
            .iter()
            .map(|w| w.clone().map(RwLock::new))
            .collect();

        let updates = data
            .updates
            .iter()
            .map(|(id, dur)| CircuitUpdateInfo {
                id: *id,
                time_override: true,
                next_time: dur.map(|d| now + d).unwrap_or(now),
                interval: *dur,
            })
            .collect();

        let ordered = board.is_ordered_queue();
        let state = Arc::new(Self {
            id,
            parent: RwLock::new(None),
            wires: RwLock::new(FixedVec::from_option_vec(wires)),
            circuits: RwLock::new(vec![].into()),
            queue: Mutex::new(Queue::new(data.queue.clone(), ordered)),
            #[cfg(not(feature = "single_thread"))]
            thread: RwLock::new(None),
            board,
            circuit_updates_removes: Default::default(),
            updates: Mutex::new(updates),
            frozen: AtomicBool::new(true),
            children: AtomicUsize::new(0),
            pin_vec_pool: Pool::new(2, Vec::new),
            vis_pool: Pool::new(2, VisitedList::new),
        });

        let board_circuits = state.board.circuits.read();
        let mut state_circuits = state.circuits.write();
        for (i, c) in data.circuits.iter().enumerate() {
            let c = unwrap_option_or_continue!(c);
            let circuit = board_circuits.get(i);
            let circuit = errors.report_none(circuit, || format!("circuit {i} did not exist"));
            let circuit = unwrap_option_or_continue!(circuit);
            let mut errors = errors.enter_context(|| format!("loading state for circuit {i}"));
            let ctx = CircuitStateContext::new(state.clone(), circuit.clone());
            let loaded = RwLock::new(CircuitState::load(c, &ctx, &mut errors));
            state_circuits.set(i, loaded);
        }
        drop((board_circuits, state_circuits));

        state
    }

    pub fn update_wire(self: &Arc<Self>, wire: usize, skip_state_ckeck: bool) {
        self.schedule_update(UpdateTask::WireState {
            id: wire,
            skip_state_ckeck,
        });
    }

    pub fn update_circuit_signals(self: &Arc<Self>, circuit: usize, pin: Option<usize>) {
        self.schedule_update(UpdateTask::CircuitSignals { id: circuit, pin });
    }

    pub fn update_pin_input(
        self: &Arc<Self>,
        circuit: usize,
        id: usize,
        immediate: bool,
        visited_items: Option<&mut VisitedList>,
    ) {
        match immediate {
            true => {
                let circuit = self.board.circuits.read().get_clone(circuit);
                let circuit = unwrap_option_or_return!(circuit);
                self.update_pin_input_now(&circuit, id, visited_items);
            }
            false => self.schedule_update(UpdateTask::PinInput { circuit, id }),
        }
    }

    pub fn reset_wire(&self, wire: usize) {
        self.wires.write().remove(wire);
    }

    pub fn remove_circuit_states(self: &Arc<Self>) {
        let circuits = self.board.circuits.read();
        for circuit in circuits.iter() {
            self.remove_circuit_state(circuit);
        }
    }

    pub fn remove_circuit_state(self: &Arc<Self>, circuit: &Arc<Circuit>) {
        let state_ctx = CircuitStateContext::new(self.clone(), circuit.clone());
        let mut reset_state = true;
        circuit
            .imp
            .read()
            .state_remove(&state_ctx, &mut reset_state);

        if reset_state {
            self.circuits.write().remove(circuit.id);
        }
        self.reset_circuit_update_interval(circuit.id);
    }

    pub fn set_ordered(&self, ordered: bool) {
        self.queue.lock().set_ordered(ordered);
    }

    fn schedule_update(self: &Arc<Self>, task: UpdateTask) {
        let mut queue = self.queue.lock();

        if queue.len() > 10_000_000 {
            println!("warning! queue contains too many elements! draining...");
            for _ in 0..1_000_000 {
                queue.dequeue();
            }
        }

        queue.enqueue(task);

        #[cfg(not(feature = "single_thread"))]
        self.wake_thread(true);
    }

    #[cfg(feature = "single_thread")]
    pub fn update(self: &Arc<State>) {
        self.update_once(5000);
    }

    fn update_once(self: &Arc<State>, queue_limit: usize) -> Option<Instant> {
        if self.frozen.load(Ordering::Relaxed) {
            return None;
        }

        // Lock shared simulation, so placing/deleting won't interrupt anything
        let sim_lock = { self.board.sim_lock.clone() };
        let sim_lock = sim_lock.read();
        let mut circuit_updates_removes = self.circuit_updates_removes.lock();

        let nearest_update = {
            let mut updates = self.updates.lock();
            let mut nearest_update = None;
            let now = Instant::now();
            circuit_updates_removes.clear();
            for (i, upd) in updates.iter_mut().enumerate() {
                if upd.next_time <= now {
                    let circ = self.board.circuits.read().get(upd.id).cloned();
                    if let Some(circ) = circ {
                        let imp = circ.imp.read();

                        let state = CircuitStateContext::new(self.clone(), circ.clone());
                        imp.update(&state, &mut upd.interval);
                        upd.time_override = false;
                        match upd.interval {
                            Some(d) => {
                                upd.next_time = now + d;
                            }
                            None => circuit_updates_removes.push(i),
                        }
                    } else {
                        circuit_updates_removes.push(i);
                    }
                }
                let closer = match nearest_update {
                    Some(nu) => upd.next_time < nu,
                    None => true,
                };
                if closer {
                    nearest_update = Some(upd.next_time);
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
            let deq = { self.queue.lock().dequeue() };
            let task = unwrap_option_or_break!(deq);

            match task {
                UpdateTask::WireState {
                    id,
                    skip_state_ckeck,
                } => {
                    self.update_wire_now(id, skip_state_ckeck);
                }
                UpdateTask::CircuitSignals { id, pin } => {
                    if let Some(circuit) = self.board.circuits.read().get(id) {
                        self.update_circuit_signals_now(circuit, pin);
                    }
                }
                UpdateTask::PinInput { circuit, id } => {
                    if let Some(circuit) = self.board.circuits.read().get(circuit) {
                        self.update_pin_input_now(circuit, id, None);
                    }
                }
            }
            queue_counter += 1;
        }
        drop(sim_lock);
        match nearest_update {
            Some(t) => Some(t),
            None if queue_counter > 0 => Some(Instant::now()),
            _ => None,
        }
    }

    pub fn init_circuit_states(self: &Arc<Self>, first_init: bool) {
        let circuits = self.board.circuits.read();
        for circuit in circuits.iter() {
            self.init_circuit_state(circuit, first_init);
        }
    }

    pub fn init_circuit_state(self: &Arc<Self>, circuit: &Arc<Circuit>, first_init: bool) {
        let state_ctx = CircuitStateContext::new(self.clone(), circuit.clone());
        let imp = circuit.imp.read();
        imp.state_init(&state_ctx, first_init);
    }

    pub fn compute_wire_state(
        self: &Arc<Self>,
        wire: usize,
        state: &mut WireState,
        visited_items: &mut VisitedList,
    ) {
        let vis = VisitedItem::Wire(wire);
        if visited_items.contains(self.board.uid, vis) {
            return;
        }

        let wires = self.board.wires.read();
        let wire = wires.get(wire);
        let wire = unwrap_option_or_return!(wire);

        visited_items.push(self.board.uid, vis);

        let mut custom_pins = self.pin_vec_pool.pull(Vec::new);
        custom_pins.clear();

        for (_, point) in wire.points.iter() {
            if let Some(pin_arc) = &point.pin {
                let pin = pin_arc.read();

                match pin.direction(self) {
                    PinDirection::Inside => {}
                    PinDirection::Outside => {
                        state.merge(&pin.get_state(self));
                    }
                    PinDirection::Custom => {
                        // Ignore this pin if it triggered the update
                        if !visited_items.contains(self.board.uid, VisitedItem::Pin(pin.id)) {
                            custom_pins.push(pin_arc.clone());
                        }
                    }
                }
            }
        }

        for pin in custom_pins.drain(..) {
            let pin = pin.read();
            // 2 locks, eugh
            if let PinDirection::Custom = pin.direction(self) {
                if let Some(circuit) = self.board.circuits.read().get(pin.id.circuit_id).cloned() {
                    let state_ctx = CircuitStateContext::new(self.clone(), circuit.clone());
                    visited_items.push(self.board.uid, VisitedItem::Pin(pin.id));
                    circuit.imp.read().custom_pin_mutate_state(
                        &state_ctx,
                        pin.id.id,
                        state,
                        visited_items,
                    );
                    visited_items.pop(self.board.uid);
                }
            }
        }

        visited_items.pop(self.board.uid);
    }

    pub fn apply_wire_state(
        self: &Arc<Self>,
        wire: usize,
        state: &WireState,
        skip_state_ckeck: bool,
        visited_items: &mut VisitedList,
    ) {
        let vis = VisitedItem::Wire(wire);
        if visited_items.contains(self.board.uid, vis) {
            return;
        }

        let skip = self.write_wire(wire, |current| {
            if !skip_state_ckeck && *current == *state {
                true
            } else {
                *current = state.clone();
                false
            }
        });
        if skip {
            return;
        }

        let wires = self.board.wires.read();
        let wire = wires.get(wire);
        let wire = unwrap_option_or_return!(wire);

        visited_items.push(self.board.uid, vis);

        for (_, point) in wire.points.iter() {
            if let Some(pin) = &point.pin {
                let pin = pin.read();

                match pin.direction(self) {
                    PinDirection::Inside => pin.set_input(self, state, true, None),
                    PinDirection::Outside => {}
                    PinDirection::Custom => {
                        // Don't trigger this pin if it triggered the update
                        if !visited_items.contains(self.board.uid, VisitedItem::Pin(pin.id)) {
                            pin.set_input(self, state, true, Some(visited_items));
                        }
                    }
                }
            }
        }

        visited_items.pop(self.board.uid);
    }

    fn update_wire_now(self: &Arc<State>, wire: usize, skip_state_ckeck: bool) {
        let mut state = WireState::None;
        let mut visited_items = self.vis_pool.pull(VisitedList::new);
        visited_items.clear();
        self.compute_wire_state(wire, &mut state, &mut visited_items);
        visited_items.clear();
        self.apply_wire_state(wire, &state, skip_state_ckeck, &mut visited_items);
    }

    fn update_circuit_signals_now(self: &Arc<Self>, circuit: &Arc<Circuit>, pin: Option<usize>) {
        circuit.imp.read().update_signals(
            &CircuitStateContext::new(self.clone(), circuit.clone()),
            pin,
        )
    }

    fn update_pin_input_now(
        self: &Arc<Self>,
        circuit: &Arc<Circuit>,
        id: usize,
        visited_items: Option<&mut VisitedList>,
    ) {
        let info = circuit.info.read();
        let pin_info = unwrap_option_or_return!(info.pins.get(id));

        let ctx = CircuitStateContext::new(self.clone(), circuit.clone());

        let pin = pin_info.pin.clone();
        let pin = pin.read();

        let custom = match pin.direction(self) {
            PinDirection::Inside => false,
            PinDirection::Custom => true,
            PinDirection::Outside => return,
        };

        let pin_wire = pin.connected_wire();

        let new_state = pin_wire
            .and_then(|w| self.wires.read().get(w).map(|w| w.read().clone()))
            .unwrap_or_default();

        if !custom {
            let state = pin_info.get_state(&ctx);
            if state == new_state {
                return;
            }
        }

        pin.set_input(self, &new_state, false, None);
        drop(pin);

        let custom_visited_items = custom.then_some(visited_items).flatten();

        match custom_visited_items {
            None => self.update_circuit_signals_now(circuit, Some(id)),
            Some(visited_items) => {
                visited_items.push(
                    self.board.uid,
                    VisitedItem::Pin(CircuitPinId {
                        id,
                        circuit_id: circuit.id,
                    }),
                );
                circuit.imp.read().custom_pin_apply_state(
                    &CircuitStateContext::new(self.clone(), circuit.clone()),
                    id,
                    &new_state,
                    visited_items,
                );
                visited_items.pop(self.board.uid);
            }
        }
    }

    #[cfg(not(feature = "single_thread"))]
    fn wake_thread(self: &Arc<Self>, notify: bool) {
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
            if notify {
                *sync.1.lock() = false;
                sync.0.notify_one();
            }
            return;
        }
        if self.frozen.load(Ordering::Relaxed) {
            return;
        }

        let clone = self.clone();
        let sync = Arc::new((parking_lot::Condvar::new(), parking_lot::Mutex::new(false)));
        let sync_clone = sync.clone();
        let name = format!(
            "State runner {} ({})",
            self.id,
            self.board.name.read().get_arc()
        );
        let handle = thread::Builder::new()
            .name(name)
            .spawn(move || {
                StateThread::new(clone, sync_clone).run();
            })
            .expect("state thread spawn failed");
        let handle = StateThreadHandle { handle, sync };
        *self.thread.write() = Some(handle);
    }

    #[cfg(not(feature = "single_thread"))]
    fn terminate_thread(&self) {
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
            *sync.1.lock() = true;
            sync.0.notify_one();
        }
    }

    pub fn reset(self: &Arc<Self>) {
        // Important to lock everything, so thread won't do anything
        let mut queue = self.queue.lock();

        self.remove_circuit_states();

        let mut wires = self.wires.write();
        let mut circuits = self.circuits.write();

        queue.clear();
        circuits.clear();
        wires.clear();

        drop((queue, circuits, wires));

        self.init_circuit_states(true);
    }

    pub fn update_everything(self: &Arc<Self>) {
        let mut queue = self.queue.lock();

        for circuit in self.board.circuits.read().iter() {
            queue.enqueue(UpdateTask::CircuitSignals {
                id: circuit.id,
                pin: None,
            });
        }
        for wire in self.board.wires.read().iter() {
            queue.enqueue(UpdateTask::WireState {
                id: wire.id,
                skip_state_ckeck: true,
            });
        }
        drop(queue);
        #[cfg(not(feature = "single_thread"))]
        self.wake_thread(true);
    }

    pub fn queue_len(&self) -> usize {
        self.queue.lock().len()
    }

    pub fn get_self_arc(&self) -> Arc<State> {
        self.board
            .states
            .get(self.id)
            .expect("this state must exist")
    }

    pub fn get_parent(&self) -> Option<StateParent> {
        self.parent.read().clone()
    }

    pub fn set_parent(&self, new_parent: Option<StateParent>) {
        let mut parent = self.parent.write();
        if let Some(parent) = parent.deref() {
            parent.state.children.fetch_sub(1, Ordering::Relaxed);
        }

        if let Some(parent) = &new_parent {
            parent.state.children.fetch_add(1, Ordering::Relaxed);
        }
        *parent = new_parent;
    }
}

impl Drop for State {
    fn drop(&mut self) {
        #[cfg(not(feature = "single_thread"))]
        self.terminate_thread();
    }
}

#[cfg(not(feature = "single_thread"))]
struct StateThreadHandle {
    handle: JoinHandle<()>,
    sync: Arc<(parking_lot::Condvar, parking_lot::Mutex<bool>)>,
}

#[cfg(not(feature = "single_thread"))]
struct StateThread {
    state: Arc<State>,
    sync: Arc<(parking_lot::Condvar, parking_lot::Mutex<bool>)>,
}

#[cfg(not(feature = "single_thread"))]
impl StateThread {
    pub fn new(
        state: Arc<State>,
        sync: Arc<(parking_lot::Condvar, parking_lot::Mutex<bool>)>,
    ) -> Self {
        Self { state, sync }
    }

    fn run(&mut self) {
        loop {
            {
                if *self.sync.1.lock() {
                    return;
                }
            }

            if self.state.frozen.load(Ordering::Relaxed) {
                return;
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum VisitedItem {
    Wire(usize),
    Pin(CircuitPinId),
}

pub struct VisitedList {
    boards: BTreeMap<u128, Vec<VisitedItem>>,
    pool: Pool<Vec<VisitedItem>>,
}

impl VisitedList {
    pub fn new() -> Self {
        Self {
            boards: BTreeMap::new(),
            pool: Pool::new(2, Vec::new),
        }
    }

    pub fn clear(&mut self) {
        while let Some((_, mut vec)) = self.boards.pop_last() {
            vec.clear();
            self.pool.attach(vec);
        }
    }

    pub fn push(&mut self, board: u128, item: VisitedItem) {
        let vec = self
            .boards
            .entry(board)
            .or_insert_with(|| self.pool.pull(Vec::new).detach().1);
        vec.push(item);
    }

    pub fn contains(&self, board: u128, item: VisitedItem) -> bool {
        self.boards
            .get(&board)
            .map(|v| v.contains(&item))
            .unwrap_or(false)
    }

    pub fn pop(&mut self, board: u128) {
        let remove = match self.boards.get_mut(&board) {
            None => false,
            Some(vec) => {
                vec.pop();
                vec.is_empty()
            }
        };
        if remove {
            if let Some(mut vec) = self.boards.remove(&board) {
                vec.clear();
                self.pool.attach(vec);
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        self.boards.is_empty()
    }
}

impl Default for VisitedList {
    fn default() -> Self {
        Self::new()
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
