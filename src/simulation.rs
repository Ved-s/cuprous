use std::{
    collections::{HashMap, HashSet, hash_map::Entry},
    fmt::Write as _,
    io::ErrorKind,
    path::PathBuf,
    sync::{
        Arc, Weak,
        atomic::{AtomicBool, Ordering},
    },
    time::Duration,
};

use parking_lot::{Mutex, RwLock};

use crate::{
    app::ErrorStrings,
    board::Board,
    circuits::CircuitBlueprint,
    io::savestate,
    state::{
        BoardState,
        sim::{ExternalTaskPool, UpdateTask},
    },
    storage::{Filesystem, ItemType},
    str::ArcStaticStr,
    time::{self, TimeProvider},
};

const BOARDS_DIR: &str = "boards";
const STATES_DIR: &str = "states";

pub struct SimulationStateData {
    uid: u128,
    board: Weak<Board>,
    state: Arc<RwLock<BoardState>>,
    update_pool_queue: Arc<Mutex<ExternalTaskPool>>,
}

impl SimulationStateData {
    fn new(state: Arc<RwLock<BoardState>>) -> Self {
        let mut state_lock = state.write();
        let uid = state_lock.uid();
        let board = state_lock.board_weak();
        let tasks = Arc::<Mutex<ExternalTaskPool>>::default();
        state_lock.set_external_tasks(tasks.clone());
        drop(state_lock);
        Self {
            uid,
            board,
            state,
            update_pool_queue: tasks,
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
        let mut pq = self.update_pool_queue.lock();
        if pq.is_empty() {
            if let Some(mut state) = self.state.try_write() {
                state.add_tasks(tasks);
                return;
            }
        } else {
            pq.add_tasks(tasks);
        }
        drop(pq);

        self.board().simulation().queue_state(self.uid);
    }

    pub fn reset(&self) {
        let mut pq = self.update_pool_queue.lock();
        pq.clear();

        let mut state = self.state.write();
        state.reset();

        drop(state);
        drop(pq);
    }

    pub fn save(&self) -> crate::io::savestate::BoardState {
        self.state.write().save()
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

        let main = Arc::new(Board::new(&this, None, "main".into()));
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

        let start = time::SYSTEM.now();
        let end = start + Duration::from_millis(10);

        let mut runs = 0;

        loop {
            let mut all_states_done = true;
            for state in self.states().read().values() {
                let mut state_sim = state.state.write();

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
            let now = time::SYSTEM.now();
            let elapsed = now - start;
            let average_ns = (elapsed.as_nanos() / runs as u128) as u64;
            let average = Duration::from_nanos(average_ns);
            if end <= now + average {
                break;
            }
        }
    }

    pub fn queue_state(&self, id: u128) {}

    pub fn save(&self, fs: &mut dyn Filesystem, errors: &mut Vec<ErrorStrings>) {
        fs.rmdir(BOARDS_DIR.as_ref()).ok();
        if let Err(e) = fs.mkdir(BOARDS_DIR.as_ref()) {
            let e = eyre::Report::new(e).wrap_err("Creating directory for boards");
            errors.push(e.into());
            return;
        }

        fs.rmdir(STATES_DIR.as_ref()).ok();
        if let Err(e) = fs.mkdir(STATES_DIR.as_ref()) {
            let e = eyre::Report::new(e).wrap_err("Creating directory for states");
            errors.push(e.into());
            return;
        }

        // TODO: lock simulation

        let mut board_name_set = HashSet::new();

        let mut tmp_path = PathBuf::new();

        for board in self.boards.read().values() {
            let mut name = String::new();
            clean_name_for_filesystem(&board.name().read(), &mut name);

            let name_clean_len = name.len();
            let mut counter = 0usize;
            while board_name_set.contains(name.as_str()) {
                counter += 1;
                name.replace_range(name_clean_len.., "");
                write!(name, "_{counter}").ok();
            }

            tmp_path.clear();
            tmp_path.push(BOARDS_DIR);
            tmp_path.push(&name);

            let res = fs.writefile(&tmp_path, &mut |w| {
                let saved: crate::io::savestate::Board = board.save();
                smoldata::write_into(&saved, w)
            });

            if let Err(e) = res {
                let e = eyre::Report::new(e)
                    .wrap_err(format!("Saving board \"{}\"", board.name().read()));
                errors.push(e.into());
            }

            for (i, state) in board.states().read().iter().enumerate() {
                let Some(state) = state.upgrade() else {
                    continue;
                };

                name.replace_range(name_clean_len.., "");
                write!(name, "_{i}").ok();

                tmp_path.clear();
                tmp_path.push(STATES_DIR);
                tmp_path.push(&name);

                let res = fs.writefile(&tmp_path, &mut |w| {
                    let saved: crate::io::savestate::BoardState = state.save();
                    smoldata::write_into(&saved, w)
                });

                if let Err(e) = res {
                    let e = eyre::Report::new(e)
                        .wrap_err(format!("Saving board \"{}\"", board.name().read()));
                    errors.push(e.into());
                }
            }

            board_name_set.insert(name);
        }
    }

    pub fn load(
        fs: &mut dyn Filesystem,
        blueprints: &HashMap<ArcStaticStr, Arc<RwLock<CircuitBlueprint>>>,
        errors: &mut Vec<ErrorStrings>,
    ) -> Arc<Self> {
        let this = Arc::new(Self {
            boards: Default::default(),
            states: Default::default(),
            paused: AtomicBool::new(false),
        });

        let mut boards = this.boards.write();

        let mut board_file_names = vec![];

        let res = fs.readdir(BOARDS_DIR.as_ref(), &mut |name, ty| {
            if !matches!(ty, ItemType::File) {
                return Ok(());
            }

            board_file_names.push(name.to_owned());
            Ok(())
        });

        if let Err(e) = res {
            errors.push(ErrorStrings::from(
                eyre::Report::new(e).wrap_err("Error loading boards"),
            ));
            boards.clear();
            return Self::new();
        }

        let mut board_data = vec![];

        let mut tmpbuf = PathBuf::new();

        let mut state_board_map = HashMap::<u128, Arc<Board>>::new();

        for name in board_file_names {
            tmpbuf.clear();
            tmpbuf.push(BOARDS_DIR);
            tmpbuf.push(&name);
            let res = fs.readfile(&tmpbuf, &mut |reader| {
                let mut data: savestate::Board = match smoldata::read_from(reader) {
                    Ok(d) => d,
                    Err(e) => {
                        return Err(std::io::Error::new(ErrorKind::InvalidData, e));
                    }
                };

                if boards.contains_key(&data.uid) {
                    errors.push(ErrorStrings {
                        main: format!("Could not load board from {BOARDS_DIR}/{name}"),
                        sub: format!("Board with id {:032x} has already been loaded", data.uid),
                    });
                    return Ok(());
                }

                data.states.retain(|id| {
                    if let Some(board) = state_board_map.get(id) {
                        errors.push(ErrorStrings {
                            main: format!(
                                "Could not link state {id:032x} to board \"{}\" (id {:032x})",
                                board.name().read(),
                                board.uid()
                            ),
                            sub: "The state is already linked to another board".to_string(),
                        });
                        return false;
                    }
                    true
                });

                let board = Board::preload(&mut data, &this);

                for uid in data.states.iter().copied() {
                    state_board_map.insert(uid, board.clone());
                }
                boards.insert(data.uid, board);

                board_data.push(data);
                Ok(())
            });

            if let Err(e) = res {
                errors
                    .push(ErrorStrings::from(eyre::Report::new(e).wrap_err(format!(
                        "Could not load board from {BOARDS_DIR}/{name}"
                    ))));
            }
        }

        let mut states = this.states.write();

        let mut state_data = vec![];

        let mut state_file_names = vec![];

        let res = fs.readdir(STATES_DIR.as_ref(), &mut |name, ty| {
            if !matches!(ty, ItemType::File) {
                return Ok(());
            }

            state_file_names.push(name.to_owned());
            Ok(())
        });

        if let Err(e) = res {
            errors.push(ErrorStrings::from(
                eyre::Report::new(e).wrap_err("Error loading states"),
            ));
            boards.clear();
            return Self::new();
        }

        for name in state_file_names {
            tmpbuf.clear();
            tmpbuf.push(STATES_DIR);
            tmpbuf.push(&name);
            let res = fs.readfile(&tmpbuf, &mut |reader| {
                let data: savestate::BoardState = match smoldata::read_from(reader) {
                    Ok(d) => d,
                    Err(e) => {
                        return Err(std::io::Error::new(ErrorKind::InvalidData, e));
                    }
                };

                if states.contains_key(&data.uid) {
                    errors.push(ErrorStrings {
                        main: format!("Could not load board state from {BOARDS_DIR}/{name}"),
                        sub: format!("State with id {:032x} has already been loaded", data.uid),
                    });
                    return Ok(());
                }

                let Some(board) = state_board_map.get(&data.uid) else {
                    errors.push(ErrorStrings {
                        main: format!("Could not load board state from {BOARDS_DIR}/{name}"),
                        sub: "No board links to this state".to_string(),
                    });
                    return Ok(());
                };

                let state = Arc::new(RwLock::new(BoardState::new(board, Some(data.uid))));

                let state = Arc::new(SimulationStateData::new(state));

                states.insert(data.uid, state);

                state_data.push(data);
                Ok(())
            });

            if let Err(e) = res {
                errors
                    .push(ErrorStrings::from(eyre::Report::new(e).wrap_err(format!(
                        "Could not load board from {BOARDS_DIR}/{name}"
                    ))));
            }
        }

        for board_data in &mut board_data {
            let board = &boards[&board_data.uid];
            for &uid in &board_data.states {
                match states.entry(uid) {
                    Entry::Occupied(entry) => {
                        board.states().write().push(Arc::downgrade(entry.get()));
                    }
                    Entry::Vacant(entry) => {
                        errors.push(ErrorStrings {
                            main: format!(
                                "Could not link state {uid:032x} to board \"{}\" (id {:032x})",
                                board.name().read(),
                                board.uid()
                            ),
                            sub: "The state is missing from the project or failed to load"
                                .to_string(),
                        });

                        let state = Arc::new(RwLock::new(BoardState::new(board, Some(uid))));
                        let state = Arc::new(SimulationStateData::new(state));

                        board.states().write().push(Arc::downgrade(&state));

                        entry.insert(state);
                    }
                }
            }
        }

        drop(states);
        drop(boards);

        let boards = this.boards.read();
        let states = this.states.read();

        // load everything that doesn't reference other loaded stuff
        for board_data in &mut board_data {
            boards[&board_data.uid].load_stage1_shallow(board_data, blueprints);
        }
        for state_data in &mut state_data {
            states[&state_data.uid]
                .state()
                .write()
                .load_stage1_shallow(state_data);
        }

        // calculate circuit size and pins, connect pins to wires, load pin states, load circuit instances
        for board_data in &mut board_data {
            boards[&board_data.uid].load_stage2_circuits(board_data);
        }
        for state_data in &mut state_data {
            states[&state_data.uid]
                .state()
                .write()
                .load_stage2_circuits(state_data);
        }

        // load circuit states
        for state_data in &mut state_data {
            states[&state_data.uid]
                .state()
                .write()
                .load_stage3_circuit_states(state_data);
        }

        drop(states);
        drop(boards);

        this
    }
}

fn clean_name_for_filesystem(name: &str, output: &mut String) {
    for char in name.chars() {
        if char.is_ascii()
            && !char.is_control()
            && !char.is_whitespace()
            && !char.is_ascii_punctuation()
        {
            output.push(char);
        } else {
            output.push('_');
        }
    }

    if output.is_empty() {
        output.push('_');
    }
}
