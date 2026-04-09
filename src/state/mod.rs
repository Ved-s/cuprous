use std::{
    ops::Deref,
    sync::{Arc, Weak},
};

use parking_lot::Mutex;

use crate::{
    Style,
    board::{Board, Wire},
    circuits::{CircuitPin, CircuitUpdateReason, PinType, UntypedCircuitCtx},
    io::savestate,
    pool::get_pooled,
    state::{
        circuits::{BoardCircuitsState, CircuitState},
        sim::{
            BoardSimulationState, CircuitUpdateTask, ExternalTaskPool, InputUpdateTask, UpdateTask,
            UpdateTaskPool, WireUpdateTask,
        },
        wires::{BoardWiresState, WireState},
    },
    time::Instant
};

pub mod circuits;
pub mod sim;
pub mod wires;

pub struct BoardState {
    id: u128,
    pub wires: BoardWiresState,
    pub circuits: BoardCircuitsState,
    sim: BoardSimulationState,

    board: Weak<Board>,
}

impl BoardState {
    pub(crate) fn new(board: &Arc<Board>, id: Option<u128>) -> Self {
        let id = id.unwrap_or_else(|| {
            let mut buf = [0u8; _];
            getrandom::getrandom(&mut buf).unwrap();
            u128::from_ne_bytes(buf)
        });
        Self {
            id,
            wires: Default::default(),
            circuits: Default::default(),
            sim: BoardSimulationState::new(),
            board: Arc::downgrade(board),
        }
    }

    pub fn uid(&self) -> u128 {
        self.id
    }

    pub fn board(&self) -> Arc<Board> {
        self.board
            .upgrade()
            .expect("state exists but its board is dropped!")
    }

    pub fn board_weak(&self) -> Weak<Board> {
        self.board.clone()
    }

    pub fn reset(&mut self) {
        self.wires.reset();
        self.circuits.reset();
        self.sim.reset();
    }

    pub fn save(&mut self) -> savestate::BoardState {
        let board = self
            .board
            .upgrade()
            .expect("tried to save state without attached board");

        let wires = board.wires().read();
        let circuits = board.circuits().read();

        savestate::BoardState {
            uid: self.id,
            wires: self
                .wires
                .wires
                .iter()
                .enumerate()
                .map(|(i, v)| match wires.get(i).is_some() {
                    true => v.clone(),
                    false => WireState::None,
                })
                .collect(),
            circuits: self
                .circuits
                .inner
                .inner
                .iter()
                .enumerate()
                .map(|(i, v)| {
                    let v = v.as_ref()?;
                    let circuit = circuits.get(i)?;
                    let pins = circuit.pins.read();

                    Some(savestate::CircuitState {
                        pins: v
                            .pins
                            .iter()
                            .enumerate()
                            .take(pins.len())
                            .map(|(i, v)| (pins[i].desc.id.clone(), v.clone()))
                            .collect(),
                        internal: {
                            v.internal.as_ref().and_then(|int| {
                                let imp = circuit.imp.read();
                                imp.imp.save_state(circuit, &imp.instance, int)
                            })
                        },
                    })
                })
                .collect(),
            sim: self.sim.save(),
        }
    }

    pub fn load_stage1_shallow(&mut self, data: &mut savestate::BoardState) {
        self.wires.wires.clone_from(&data.wires);

        self.sim.load(std::mem::take(&mut data.sim));
    }

    pub fn load_stage2_circuits(&mut self, data: &mut savestate::BoardState) {
        let board = self
            .board
            .upgrade()
            .expect("tried to load state without attached board");

        let board_circuits = board.circuits();
        let board_circuits = board_circuits.read();
        for (i, circuit_data) in data.circuits.iter().enumerate() {
            let Some(circuit_data) = circuit_data else {
                continue;
            };

            let board_circuit = board_circuits.get(i).expect("loaded circuits");

            let pins = board_circuit
                .pins
                .read()
                .iter()
                .map(|p| {
                    circuit_data
                        .pins
                        .get(&p.desc.id)
                        .cloned()
                        .unwrap_or(WireState::None)
                })
                .collect();

            let circuit = CircuitState {
                pins,
                internal: Default::default(),
            };

            self.circuits.inner.set(i, circuit);
        }
    }

    pub fn load_stage3_circuit_states(&mut self, data: &mut savestate::BoardState) {
        let board = self
            .board
            .upgrade()
            .expect("tried to load state without attached board");

        let board_circuits = board.circuits();
        let board_circuits = board_circuits.read();
        for (i, circuit_data) in data.circuits.iter().enumerate() {
            let Some(circuit_data) = circuit_data else {
                continue;
            };

            let Some(state_data) = &circuit_data.internal else {
                continue;
            };

            let board_circuit = board_circuits.get(i).expect("loaded circuits");
            let circuit = self.circuits.inner.get_mut(i).expect("loaded circuits");

            let imp = board_circuit.imp.read();

            // todo: errors
            let state = imp
                .imp
                .load_state(board_circuit, &imp.instance, state_data)
                .ok();
            circuit.internal = state;
        }
    }

    pub fn pin_color(&self, pin: &CircuitPin, style: &Style) -> eframe::egui::Color32 {
        let connected_wire = pin.wire.read().clone();
        match connected_wire {
            None => style
                .wire_colors
                .get(&self.circuits.get_pin(pin.circuit.id, pin.id)),
            Some(wire) => {
                // TODO: do something when wire state and pin state don't match
                self.wires.wire_color(&wire, style)
            }
        }
    }

    pub fn add_tasks(&mut self, tasks: &mut dyn Iterator<Item = UpdateTask>) {
        self.sim.add_tasks(tasks, false, None);
    }

    pub fn set_external_tasks(&mut self, external_tasks: Arc<Mutex<ExternalTaskPool>>) {
        self.sim.set_external_tasks(external_tasks);
    }

    pub fn run(&mut self, task_limit: &mut usize) -> Option<Instant> {
        let mut tasks = get_pooled::<UpdateTaskPool>();

        self.sim.flush_tasks();

        'main_loop: while *task_limit > 0 {
            let Some((task, meta)) = self.sim.next_task() else {
                break 'main_loop;
            };

            *task_limit -= 1;
            let mut queue_immediately = false;

            match task {
                UpdateTask::Wire(w) => {
                    self.update_wire(w, &mut tasks);
                }
                UpdateTask::Circuit(c) => {
                    self.update_circuit(c, &mut tasks);
                }
                UpdateTask::Input(i) => {
                    self.update_input(i, &mut tasks, &mut queue_immediately);
                }
                UpdateTask::DropCircuit(d) => {
                    self.circuits.drop_circuit(d.id, d.pin_only);
                }
            }

            self.sim
                .add_tasks(&mut tasks.drain(), queue_immediately, Some(&meta));
        }

        None
    }

    fn update_wire(&mut self, task: WireUpdateTask, tasks: &mut UpdateTaskPool) {
        fn update_wire_pins(
            this: &mut BoardState,
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
                        state.combine(&this.circuits.get_pin(pin.circuit.id, pin.id));
                    }
                }
            }

            // Modify

            // TODO

            // Write
            let changed = this.wires.set_wire(wire.id, state.clone());
            if !changed && !force_pin_updates {
                return;
            }

            for pin in pins {
                match pin.ty {
                    PinType::Inside => {
                        let changed = this.circuits.set_pin(pin.circuit.id, pin.id, state.clone());
                        if changed {
                            tasks.add_circuit_task(
                                pin.circuit.id,
                                CircuitUpdateReason::ChangedPin(pin.id),
                            );
                        }
                    }
                    PinType::Outside => {}
                }
            }
        }

        let Some(wire) = self.board().wires().read().get(task.id).cloned() else {
            return;
        };

        let pins = wire.connected_pins.read();

        update_wire_pins(
            self,
            wire.clone(),
            task.force_pin_updates,
            pins.as_slice(),
            tasks,
        );

        tasks.shuffle();
    }

    fn update_circuit(&mut self, task: CircuitUpdateTask, tasks: &mut UpdateTaskPool) {
        let Some(circuit) = self.board().circuits().read().get(task.id).cloned() else {
            return;
        };

        let imp = circuit.imp.read();

        let ctx = UntypedCircuitCtx {
            state: &mut self.circuits,
            circuit: &circuit,
            tasks,
            instance: imp.instance.deref(),
        };

        imp.imp.update(ctx, task.reason);
    }

    fn update_input(
        &mut self,
        task: InputUpdateTask,
        tasks: &mut UpdateTaskPool,
        queue_immediately: &mut bool,
    ) {
        let pin_input_wire = self
            .board()
            .circuits()
            .read()
            .get(task.circuit)
            .and_then(|c| {
                c.pins.read().get(task.pin).map(|p| match p.desc.ty {
                    PinType::Inside => Ok(p.pin.wire.read().clone()),
                    _ => Err(()),
                })
            })
            .ok_or(())
            .flatten();

        let state = match pin_input_wire {
            Err(()) => return,
            Ok(None) => WireState::None,
            Ok(Some(wire)) => self.wires.get_wire(wire.id),
        };

        let changed = self.circuits.set_pin(task.circuit, task.pin, state);

        if changed && task.update_circuit {
            tasks.add_circuit_task(task.circuit, CircuitUpdateReason::ChangedPin(task.pin));
            *queue_immediately = true;
        }
    }
}

/*
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

    pub(crate) fn uninitialized() -> Self {
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

    pub fn save(&self) -> savestate::BoardStates {
        savestate::BoardStates {
            main: self.main_state().save(),
            states: self
                .states
                .inner
                .iter()
                .map(|s| s.as_ref().map(|s| s.save()))
                .collect(),
        }
    }

    pub fn preload(&mut self, data: &savestate::BoardStates, board: Arc<Board>) {
        self.main = Some(BoardState::new(board.clone(), 0));
        self.states = FixedVec::from_option_vec(
            data.states
                .iter()
                .enumerate()
                .map(|(i, v)| v.as_ref().map(|_| BoardState::new(board.clone(), i + 1)))
                .collect::<Vec<Option<_>>>(),
        );
    }

    pub fn load_stage1_shallow(&self, data: &savestate::BoardStates) {
        self.main
            .as_ref()
            .expect("preloaded state collection")
            .load_stage1_shallow(&data.main);
        for (i, state_data) in data.iter().enumerate() {
            let Some(state_data) = state_data else {
                continue;
            };

            let state = self.get(i).expect("preloaded state collection");
            state.load_stage1_shallow(state_data);
        }
    }

    pub fn load_stage2_circuits(&self, data: &savestate::BoardStates) {
        for (i, state_data) in data.iter().enumerate() {
            let Some(state_data) = state_data else {
                continue;
            };

            let state = self.get(i).expect("preloaded state collection");
            state.load_stage2_circuits(state_data);
        }
    }

    pub fn load_stage3_circuit_states(&self, data: &savestate::BoardStates) {
        for (i, state_data) in data.iter().enumerate() {
            let Some(state_data) = state_data else {
                continue;
            };

            let state = self.get(i).expect("preloaded state collection");
            state.load_stage3_circuit_states(state_data);
        }
    }
}
*/
