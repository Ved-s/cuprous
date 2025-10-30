use std::{
    collections::{HashMap, VecDeque},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Weak,
    },
    time::{Duration, Instant},
};

use parking_lot::RwLock;

use crate::{
    board::Board,
    state::{sim::UpdateTask, BoardState},
};

pub struct SimulationStateData {
    uid: u128,
    board: Weak<Board>,
    state: Arc<RwLock<BoardState>>,
    update_pool_queue: RwLock<VecDeque<UpdateTask>>,
}

impl SimulationStateData {
    fn new(state: Arc<RwLock<BoardState>>) -> Self {
        let state_read = state.read();
        let uid = state_read.uid();
        let board = state_read.board_weak();
        drop(state_read);
        Self {
            uid,
            board,
            state,
            update_pool_queue: Default::default(),
        }
    }

    pub fn uid(&self) -> u128 {
        self.uid
    }

    pub fn state(&self) -> &Arc<RwLock<BoardState>> {
        &self.state
    }

    pub fn board(&self) -> Arc<Board> {
        self.board
            .upgrade()
            .expect("state exists but its board is dropped!")
    }

    pub fn add_tasks(&self, tasks: &mut dyn Iterator<Item = UpdateTask>) {
        let mut pq = self.update_pool_queue.write();
        if pq.is_empty() {
            if let Some(mut state) = self.state.try_write() {
                state.add_tasks(tasks);
            }
        } else {
            pq.extend(tasks);
        }
        drop(pq);

        self.board().simulation().queue_state(self.uid);
    }

    pub fn reset(&self) {
        let mut pq = self.update_pool_queue.write();
        pq.clear();

        let mut state = self.state.write();
        state.reset();

        drop(state);
        drop(pq);
    }
}

pub struct SimulationCtx {
    boards: RwLock<HashMap<u128, Arc<Board>>>,
    states: RwLock<HashMap<u128, Arc<SimulationStateData>>>,
    pub paused: AtomicBool,
}

impl SimulationCtx {
    pub fn new() -> Arc<Self> {
        let this = Arc::new(Self {
            boards: RwLock::new(HashMap::new()),
            states: RwLock::new(HashMap::new()),
            paused: AtomicBool::new(false),
        });

        let main = Arc::new(Board::new(&this));
        let main_state = Arc::new(RwLock::new(BoardState::new(&main, None)));
        let main_state_data = Arc::new(SimulationStateData::new(main_state));

        main.states().write().push(Arc::downgrade(&main_state_data));
        this.boards.write().insert(main.uid(), main);
        this.states()
            .write()
            .insert(main_state_data.uid(), main_state_data);
        this
    }

    pub fn boards(&self) -> &RwLock<HashMap<u128, Arc<Board>>> {
        &self.boards
    }

    pub fn states(&self) -> &RwLock<HashMap<u128, Arc<SimulationStateData>>> {
        &self.states
    }

    pub fn temp_run(&self) {
        if self.paused.load(Ordering::Relaxed) {
            return;
        }

        let start = Instant::now();
        let end = start + Duration::from_millis(10);

        let mut runs = 0;

        loop {
            let mut all_states_done = true;
            for state in self.states().read().values() {
                let mut state_sim = state.state.write();

                let mut pq = state.update_pool_queue.write();

                state_sim.add_tasks(&mut pq.drain(..));

                let mut task_limit = 100;

                state_sim.run(&mut task_limit);

                if task_limit == 0 {
                    all_states_done = false;
                }
            }

            if all_states_done {
                break;
            }

            runs += 1;
            let now = Instant::now();
            let elapsed = now - start;
            let average_ns = (elapsed.as_nanos() / runs as u128) as u64;
            let average = Duration::from_nanos(average_ns);
            if end <= now + average {
                break;
            }
        }
    }

    pub fn queue_state(&self, id: u128) {}

    // pub fn save(&self) -> savestate::Simulation {
    //     savestate::Simulation {
    //         boards: self.boards.read().values().map(|b| b.save()).collect()
    //     }
    // }

    // pub fn load(data: &savestate::Simulation, blueprints: &[Arc<RwLock<CircuitBlueprint>>]) -> Arc<Self> {
    //     let this = Arc::new(Self { boards: Default::default() });

    //     let mut boards = this.boards.write();

    //     for board_data in &data.boards {
    //         let board = Board::preload(board_data, this.clone());
    //         boards.insert(board.uid(), board);
    //     }

    //     drop(boards);

    //     let boards = this.boards.read();

    //     // load everything that doesn't reference other loaded stuff
    //     for board_data in &data.boards {
    //         boards[&board_data.uid].load_stage1_shallow(board_data, blueprints);
    //     }

    //     // calculate circuit size and pins, connect pins to wires, load pin states, load circuit instances
    //     for board_data in &data.boards {
    //         boards[&board_data.uid].load_stage2_circuits(board_data);
    //     }

    //     // load circuit states
    //     for board_data in &data.boards {
    //         boards[&board_data.uid].load_stage3_circuit_states(board_data);
    //     }

    //     drop(boards);

    //     this
    // }
}
