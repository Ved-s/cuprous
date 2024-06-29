use std::{
    any::Any,
    collections::{HashSet, VecDeque},
    hash::{DefaultHasher, Hasher},
    ops::{Deref, DerefMut},
    sync::Arc,
    time::Instant,
};

use parking_lot::RwLock;

use crate::{
    board::{Board, Wire},
    circuits::{CircuitPin, PinType, UntypedCircuitCtx},
    containers::FixedVec,
    pool::get_pooled,
    Style,
};

#[derive(Clone, Default, PartialEq, Eq)]
pub enum WireState {
    #[default]
    None,
    Bool(bool),
    Error,
}
impl WireState {
    fn combine(&mut self, other: &WireState) {
        let this = std::mem::take(self);
        *self = match (this, other) {
            (WireState::None, other) => other.clone(),
            (other, WireState::None) => other,

            (WireState::Error, _) | (_, WireState::Error) => WireState::Error,

            (WireState::Bool(a), WireState::Bool(b)) => {
                if a == *b {
                    WireState::Bool(a)
                } else {
                    WireState::Error
                }
            }
        };
    }

    fn type_eq(&self, other: &WireState) -> bool {
        match (self, other) {
            (WireState::None, WireState::None) => true,
            (WireState::Bool(a), WireState::Bool(b)) => a == b,
            (WireState::Error, WireState::Error) => true,

            (WireState::None, _) => false,
            (WireState::Bool(_), _) => false,
            (WireState::Error, _) => false,
        }
    }
}

pub struct BoardState {
    id: usize,
    wires: RwLock<Vec<WireState>>,
    circuits: RwLock<FixedVec<CircuitState>>,

    board: Arc<Board>,

    sim: RwLock<BoardStateSimulationCtx>,
}

impl BoardState {
    pub fn id(&self) -> usize {
        self.id
    }

    pub fn wires(&self) -> &RwLock<Vec<WireState>> {
        &self.wires
    }

    pub fn circuits(&self) -> &RwLock<FixedVec<CircuitState>> {
        &self.circuits
    }

    pub fn board(&self) -> &Arc<Board> {
        &self.board
    }
}

impl BoardState {
    pub(crate) fn new(board: Arc<Board>, id: usize) -> Arc<Self> {
        Arc::new(Self {
            id,
            wires: RwLock::new(vec![]),
            circuits: RwLock::new(vec![].into()),
            sim: Default::default(),
            board,
        })
    }

    pub fn wire_color(&self, wire: &Wire, style: &Style) -> eframe::egui::Color32 {
        let state = self.get_wire(wire.id);

        // TODO: wire color overrides
        style.wire_colors.get(&state)
    }

    pub fn pin_color(&self, pin: &CircuitPin, style: &Style) -> eframe::egui::Color32 {
        let connected_wire = pin.wire.read().clone();
        match connected_wire {
            None => style.wire_colors.get(&self.get_pin(pin.circuit.id, pin.id)),
            Some(wire) => {
                // TODO: do something when wire state and pin state don't match
                self.wire_color(&wire, style)
            }
        }
    }
}

impl BoardState {
    pub fn get_wire(&self, id: usize) -> WireState {
        self.wires.read().get(id).cloned().unwrap_or_default()
    }

    /// Returns true if value was changed
    pub fn set_wire(&self, id: usize, state: WireState) -> bool {
        let mut wires = self.wires.write();

        if wires.len() <= id {
            if state == WireState::default() {
                return false;
            }

            let add = id - wires.len() + 1;
            wires.reserve(add);
            for _ in 0..add {
                wires.push(WireState::default());
            }
        }

        if wires[id] == state {
            return false;
        }

        wires[id] = state;
        true
    }

    pub fn get_pin(&self, circuit: usize, id: usize) -> WireState {
        self.circuits
            .read()
            .get(circuit)
            .and_then(|c| c.pins.get(id).cloned())
            .unwrap_or_default()
    }

    /// Returns true if value was changed
    pub fn set_pin(&self, circuit: usize, id: usize, state: WireState) -> bool {
        let mut circuits = self.circuits.write();
        let circuit = circuits.get_or_create_mut(circuit, Default::default);

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

    pub fn read_internal_circuit_state<S, F, R>(&self, id: usize, reader: F) -> Option<R>
    where
        S: 'static,
        F: FnOnce(&S) -> R,
    {
        let circuits = self.circuits.read();
        let circuit = circuits.get(id)?;
        let internal = circuit.internal.clone();
        drop(circuits);

        let internal = internal.read();
        let internal = internal.deref().as_ref()?.downcast_ref()?;
        Some(reader(internal))
    }

    pub fn write_internal_circuit_state<S, F, R>(&self, id: usize, writer: F) -> R
    where
        S: Default + Send + Sync + 'static,
        F: FnOnce(&mut S) -> R,
    {
        let existing = self.circuits.read().get(id).map(|c| c.internal.clone());
        let internal = existing.unwrap_or_else(|| {
            let mut circuits = self.circuits.write();
            let circuit = circuits.get_or_create_mut(id, Default::default);
            circuit.internal.clone()
        });

        let mut lock = internal.write();
        match lock.deref_mut().as_mut().and_then(|i| i.downcast_mut()) {
            Some(internal) => writer(internal),
            None => {
                let mut internal = S::default();
                let res = writer(&mut internal);
                *lock = Some(Box::new(internal));
                res
            }
        }
    }
}

impl BoardState {
    pub fn add_tasks(&self, tasks: &mut dyn Iterator<Item = UpdateTask>, notify: bool) {
        let mut tasks = tasks.peekable();
        if tasks.peek().is_none() {
            return;
        }

        let mut circuits = get_pooled::<Vec<CircuitUpdateTask>>();
        let mut wires = get_pooled::<Vec<WireUpdateTask>>();
        let mut hasher = get_pooled::<DefaultHasher>();

        for task in tasks {
            match task {
                UpdateTask::Wire(w) => wires.push(w),
                UpdateTask::Circuit(c) => circuits.push(c),
            }
        }

        if !circuits.is_empty() {
            hasher.write_usize(circuits.len());
            for i in 0..circuits.len() {
                hasher.write_usize(i);
                let other = hasher.finish() as usize % circuits.len();
                if other == i {
                    continue;
                }

                circuits.swap(i, other);
            }
        }

        let mut sim = self.sim.write();

        sim.wires.extend(wires.drain(..));
        sim.circuits.extend(circuits.drain(..));

        drop(sim);

        if notify {
            self.board
                .simulation()
                .notify_state(self.board.uid(), self.id);
        }
    }

    pub fn has_jobs(&self) -> bool {
        let s = self.sim.read();
        !s.circuits.is_empty() || !s.wires.is_empty()
    }

    pub fn run(self: &Arc<Self>, task_limit: &mut usize) -> Option<Instant> {
        let mut tasks = get_pooled::<UpdateTaskPool>();

        'main_loop: while *task_limit > 0 {
            let mut sim = self.sim.write();

            'run_task: {
                // TODO: circuit updates

                if let Some(wire) = sim.wires.pop_front() {
                    drop(sim);
                    self.update_wire(wire, &mut tasks);
                    *task_limit -= 1;
                    break 'run_task;
                }

                if let Some(circuit) = sim.circuits.pop_front() {
                    drop(sim);
                    self.update_circuit(circuit, &mut tasks);
                    *task_limit -= 1;
                    break 'run_task;
                }

                break 'main_loop;
            }

            self.add_tasks(&mut tasks.drain(), false);
        }

        None
    }

    fn update_wire(&self, task: WireUpdateTask, tasks: &mut UpdateTaskPool) {
        fn update_wire_pins(
            this: &BoardState,
            wire: Arc<Wire>,
            force_pin_updates: bool,
            pins: &[Arc<CircuitPin>],
            tasks: &mut UpdateTaskPool,
        ) {
            let mut state = WireState::default();

            // Read
            for pin in pins {
                match pin.ty {
                    PinType::Inside => {}
                    PinType::Outside => {
                        state.combine(&this.get_pin(pin.circuit.id, pin.id));
                    }
                    PinType::Custom => todo!(),
                }
            }

            // Modify

            // TODO

            // Write
            let changed = this.set_wire(wire.id, state.clone());
            if !changed && !force_pin_updates {
                return;
            }

            for pin in pins {
                match pin.ty {
                    PinType::Inside => {
                        let changed = this.set_pin(pin.circuit.id, pin.id, state.clone());
                        if changed {
                            tasks.add(CircuitUpdateTask {
                                id: pin.circuit.id,
                                changed_pin: Some(pin.id),
                            });
                        }
                    }
                    PinType::Outside => {}
                    PinType::Custom => todo!(),
                }
            }
        }

        let Some(wire) = self.board.wires().read().get(task.id).cloned() else {
            return;
        };

        let pins = wire.connected_pins.read();

        let any_custom = pins.iter().any(|p| matches!(p.ty, PinType::Custom));

        if any_custom {
            todo!()
        } else {
            update_wire_pins(
                self,
                wire.clone(),
                task.force_pin_updates,
                pins.as_slice(),
                tasks,
            );
        }
    }

    fn update_circuit(self: &Arc<Self>, task: CircuitUpdateTask, tasks: &mut UpdateTaskPool) {
        let Some(circuit) = self.board.circuits().read().get(task.id).cloned() else {
            return;
        };

        let imp = circuit.imp.read();

        let ctx = UntypedCircuitCtx {
            state: self,
            circuit: &circuit,
            tasks,
            instance: imp.instance.deref(),
        };

        imp.imp.update_signals(ctx, task.changed_pin);
    }
}

type InternalCircuitStateLock = Arc<RwLock<Option<Box<dyn Any + Send + Sync>>>>;

#[derive(Default)]
pub struct CircuitState {
    pins: Vec<WireState>,
    internal: InternalCircuitStateLock,
}

#[derive(Default)]
pub struct UpdateTaskPool(HashSet<UpdateTask>);

impl UpdateTaskPool {
    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn is_empty(&mut self) -> bool {
        self.0.is_empty()
    }

    pub fn add_wire_task(&mut self, id: usize, force_pin_updates: bool) {
        self.add(WireUpdateTask {
            id,
            force_pin_updates,
        })
    }

    pub fn add_circuit_task(&mut self, id: usize, changed_pin: Option<usize>) {
        self.add(CircuitUpdateTask {
            id,
            changed_pin
        })
    }

    pub fn add(&mut self, task: impl Into<UpdateTask>) {
        self.add_internal(task.into());
    }

    fn add_internal(&mut self, task: UpdateTask) {
        match task {
            UpdateTask::Wire(WireUpdateTask {
                id,
                force_pin_updates,
            }) => match force_pin_updates {
                false => {
                    if self.0.contains(
                        &WireUpdateTask {
                            id,
                            force_pin_updates: true,
                        }
                        .into(),
                    ) {
                        return;
                    }
                }
                true => {
                    self.0.retain(|u| match u {
                        UpdateTask::Wire(WireUpdateTask { id: eid, .. }) => *eid != id,
                        _ => true,
                    });
                }
            },
            UpdateTask::Circuit(CircuitUpdateTask { id, changed_pin }) => match changed_pin {
                Some(_) => {
                    if self.0.contains(&CircuitUpdateTask { id, changed_pin: None }.into()) {
                        return;
                    }
                }
                None => {
                    self.0.retain(|u| match u {
                        UpdateTask::Circuit(CircuitUpdateTask { id: eid, .. }) => *eid != id,
                        _ => true,
                    });
                }
            },
        }
        self.0.insert(task);
    }

    pub fn iter(&self) -> impl Iterator<Item = UpdateTask> + '_ {
        self.0.iter().copied()
    }

    pub fn drain(&mut self) -> impl Iterator<Item = UpdateTask> + '_ {
        self.0.drain()
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct WireUpdateTask {
    pub id: usize,
    pub force_pin_updates: bool,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct CircuitUpdateTask {
    pub id: usize,
    pub changed_pin: Option<usize>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum UpdateTask {
    Wire(WireUpdateTask),
    Circuit(CircuitUpdateTask),
}

impl From<WireUpdateTask> for UpdateTask {
    fn from(value: WireUpdateTask) -> Self {
        Self::Wire(value)
    }
}

impl From<CircuitUpdateTask> for UpdateTask {
    fn from(value: CircuitUpdateTask) -> Self {
        Self::Circuit(value)
    }
}

#[derive(Default)]
pub struct BoardStateSimulationCtx {
    wires: VecDeque<WireUpdateTask>,
    circuits: VecDeque<CircuitUpdateTask>,
    // tmp_circuits: VecDeque<(usize, Option<usize>)>,
}

pub struct BoardStateCollection {
    main: Option<Arc<BoardState>>,
    states: FixedVec<Arc<BoardState>>,
}

impl BoardStateCollection {
    pub fn main_state(&self) -> &Arc<BoardState> {
        self.main.as_ref().expect("initialized state collection")
    }

    pub fn board(&self) -> &Arc<Board> {
        self.main_state().board()
    }

    pub(crate) fn new() -> Self {
        Self {
            main: None,
            states: vec![].into(),
        }
    }

    pub(crate) fn initialize(&mut self, board: Arc<Board>) {
        if self.main.is_some() {
            return;
        }

        self.main = Some(BoardState::new(board, 0));
    }

    pub fn iter(&self) -> impl Iterator<Item = &Arc<BoardState>> {
        let main = self.main.as_ref().expect("initialized state collection");
        std::iter::once(main).chain(self.states.iter())
    }

    pub fn get_or_create(&mut self, id: usize) -> &Arc<BoardState> {
        if id == 0 {
            return self.main_state();
        }

        self.states.get_or_create_mut(id - 1, || {
            let board = self
                .main
                .as_ref()
                .expect("initialized state collection")
                .board()
                .clone();
            BoardState::new(board, id)
        })
    }

    pub fn get(&self, id: usize) -> Option<&Arc<BoardState>> {
        if id == 0 {
            return Some(self.main_state());
        }

        self.states.get(id - 1)
    }

    pub fn add_tasks(&self, tasks: &UpdateTaskPool) {
        for state in self.iter() {
            state.add_tasks(&mut tasks.iter(), true)
        }
    }
}

generate_pool! {
    UpdateTaskPool,
    UPDATE_TASK_POOL,
    |p| p.clear()
}

generate_pool! {
    Vec<WireUpdateTask>,
    WIRE_UPDATE_TASK_VEC_POOL,
    |v| v.clear()
}

generate_pool! {
    Vec<CircuitUpdateTask>,
    CIRCUIT_UPDATE_TASK_VEC_POOL,
    |v| v.clear()
}

generate_pool! {
    DefaultHasher,
    DEFULT_HASHER_POOL,
    |_a| ()
}
