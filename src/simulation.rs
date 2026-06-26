use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet, hash_map::Entry},
    fmt::Write as _,
    io::ErrorKind,
    path::PathBuf,
    sync::{
        Arc, Weak,
        atomic::{AtomicBool, Ordering},
    },
    time::Duration,
};

use parking_lot::{ArcRwLockReadGuard, Mutex, RawRwLock, RwLock, RwLockWriteGuard};

use crate::{
    app::ErrorStrings,
    board::{Board, MultiwireConnectionsMap},
    components::{Component, ComponentBlueprint},
    io::savestate,
    multiwire::{MultiwireRouter, MultiwireTargetState},
    pool::get_pooled,
    state::{
        BoardState, StateRunResult,
        sim::{ExternalTaskPool, UpdateTask, UpdateTaskPool},
        wires::WireStateHandler,
    },
    storage::{Filesystem, ItemType},
    str::ArcStaticStr,
    time::{self, Instant, TimeProvider},
};

const BOARDS_DIR: &str = "boards";
const STATES_DIR: &str = "states";

pub struct SimulationStateData {
    uid: u128,
    board: Weak<Board>,
    state: Arc<RwLock<BoardState>>,
    update_pool_queue: Arc<Mutex<ExternalTaskPool>>,

    cached_multiwire_routers: RwLock<BTreeMap<usize, Arc<dyn MultiwireRouter>>>,
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
            cached_multiwire_routers: Default::default(),
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

    pub fn get_multiwire_router(
        &self,
        component: usize,
        locked_state: Option<&mut BoardState>,
    ) -> Arc<dyn MultiwireRouter> {
        if let Some(cached) = self
            .cached_multiwire_routers
            .read()
            .get(&component)
            .cloned()
        {
            return cached;
        }

        let component = self
            .board()
            .components()
            .read()
            .get(component)
            .expect("requested a router from a nonexistent component")
            .clone();

        fn try_read_router(
            state: &BoardState,
            component: &Arc<Component>,
        ) -> Option<Box<dyn MultiwireRouter>> {
            match state.components.inner.get(component.id) {
                Some(state) => match &state.internal {
                    Some(int) => {
                        let imp = component.imp.read();
                        Some(
                            imp.imp
                                .create_multiwire_router(component, &imp.instance, int),
                        )
                    }
                    None => None,
                },
                None => None,
            }
        }

        fn create_router(
            state: &mut BoardState,
            component: &Arc<Component>,
        ) -> Box<dyn MultiwireRouter> {
            let state = state
                .components
                .inner
                .get_or_create_mut(component.id, Default::default);

            let imp = component.imp.read();

            let internal = state
                .internal
                .get_or_insert_with(|| imp.imp.create_default_state());

            imp.imp
                .create_multiwire_router(component, &imp.instance, internal)
        }

        let router = match locked_state {
            Some(s) => create_router(s, &component),
            None => {
                let state = self.state.read();
                let router = try_read_router(&state, &component);
                drop(state);
                match router {
                    Some(r) => r,
                    None => {
                        let mut state = self.state.write();
                        create_router(&mut state, &component)
                    }
                }
            }
        };

        let router: Arc<dyn MultiwireRouter> = router.into();

        self.cached_multiwire_routers
            .write()
            .insert(component.id, router.clone());

        router
    }

    /// Must be called when component is removed or router invalidation requested after property change
    pub fn remove_multiwire_router(&self, component: usize) {
        self.cached_multiwire_routers.write().remove(&component);
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

    pub fn save(&self, start: Instant) -> crate::io::savestate::BoardState {
        self.state.write().save(start)
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

        this.ensure_one_board_and_state();
        this
    }

    fn ensure_one_board_and_state(self: &Arc<Self>) {
        let mut boards = self.boards.write();

        if boards.is_empty() {
            let main = Arc::new(Board::new(self, None, "main".into()));
            boards.insert(main.uid(), main);
        }

        for board in boards.values() {
            let mut states = board.states().write();

            if states.is_empty() {
                let state = Arc::new(RwLock::new(BoardState::new(board, None)));
                let state_data = Arc::new(SimulationStateData::new(state));
                states.push(Arc::downgrade(&state_data));

                self.states.write().insert(state_data.uid, state_data);
            }
        }
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
                let mut task_limit = 100;

                while task_limit > 0 {
                    let mut state_sim = state.state.write();
                    match state_sim.run(&mut task_limit) {
                        StateRunResult::Done | StateRunResult::DoneUntil(_) => break,
                        StateRunResult::RunMultiwireUpdate {
                            wire,
                            force_pin_updates,
                        } => {
                            drop(state_sim);
                            self.run_multiwire_update(state.uid, wire, force_pin_updates);
                        }
                    }
                }

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

    pub fn queue_state(&self, _id: u128) {}

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

        // todo: correct time provider
        let start_time = time::SYSTEM.now();

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
                    let saved: crate::io::savestate::BoardState = state.save(start_time);
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
        blueprints: &HashMap<ArcStaticStr, Arc<RwLock<ComponentBlueprint>>>,
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

        let mut any_unloaded_comp = false;

        // load everything that doesn't reference other loaded stuff
        for board_data in &mut board_data {
            boards[&board_data.uid].load_stage1_shallow(
                board_data,
                blueprints,
                &mut any_unloaded_comp,
            );
        }
        for state_data in &mut state_data {
            states[&state_data.uid]
                .state()
                .write()
                .load_stage1_shallow(state_data);
        }

        if any_unloaded_comp {
            // scan for connected wires to unloaded components
            for board_data in &mut board_data {
                boards[&board_data.uid].load_stage1p5_unloaded_component_pins(board_data);
            }
        }

        // calculate component size and pins, connect pins to wires, load pin states, load component instances
        for board_data in &mut board_data {
            boards[&board_data.uid].load_stage2_components(board_data);
        }
        for state_data in &mut state_data {
            states[&state_data.uid]
                .state()
                .write()
                .load_stage2_components(state_data);
        }

        // load component states
        for state_data in &mut state_data {
            states[&state_data.uid]
                .state()
                .write()
                .load_stage3_component_states(state_data);
        }

        // todo: correct time provider
        let start_time = time::SYSTEM.now();

        // load simulations
        for state_data in &mut state_data {
            states[&state_data.uid]
                .state()
                .write()
                .load_stage4_simulation_data(state_data, start_time);
        }

        drop(states);
        drop(boards);

        this.ensure_one_board_and_state();

        this
    }

    pub fn run_multiwire_update(
        &self,
        start_state: u128,
        start_wire: usize,
        force_pin_updates: bool,
    ) {
        struct StateData<'a> {
            data: &'a SimulationStateData,
            state_lock: RwLockWriteGuard<'a, BoardState>,
            connections_lock: ArcRwLockReadGuard<RawRwLock, MultiwireConnectionsMap>,
            wires: BTreeSet<usize>,
        }

        let mut routes = vec![];

        let states = self.states.read();
        let mut all_states = HashMap::<u128, StateData>::new();

        let mut explorer_stack = vec![];
        explorer_stack.push((start_state, start_wire));

        while let Some((state, wire)) = explorer_stack.pop() {
            let data = match all_states.entry(state) {
                Entry::Occupied(e) => e.into_mut(),
                Entry::Vacant(e) => {
                    let Some(state) = states.get(&state) else {
                        continue;
                    };
                    e.insert(StateData {
                        data: state,
                        state_lock: state.state.write(),
                        connections_lock: state.board().multiwire_connections().read_arc(),
                        wires: Default::default(),
                    })
                }
            };

            let Some(wire_connections) = data.connections_lock.get(&wire) else {
                continue;
            };

            if !data.wires.insert(wire) {
                continue;
            }

            for (component, pin) in wire_connections {
                let router = data
                    .data
                    .get_multiwire_router(*component, Some(&mut data.state_lock));

                routes.clear();
                router.route(*pin, &mut routes);

                for route in routes.drain(..) {
                    let target_state = match route.target_state {
                        MultiwireTargetState::CurrentState => state,
                        MultiwireTargetState::Uid(s) => s,
                    };

                    explorer_stack.push((target_state, route.wire_id));
                }
            }
        }

        let mut wire_state = WireStateHandler::default();

        for state in all_states.values() {
            let board = state.data.board();
            let wires = board.wires().read();

            for wire in &state.wires {
                let Some(wire) = wires.get(*wire) else {
                    continue;
                };

                wire_state.read_pins(&state.state_lock, wire.connected_pins.read().as_slice())
            }
        }

        // todo: is it a good idea to drop locks as writes finish? or to drop them all at once

        for state in all_states.values_mut() {
            let board = state.data.board();
            let wires = board.wires().read();

            let mut tasks = get_pooled::<UpdateTaskPool>();

            for wire in &state.wires {
                let Some(wire) = wires.get(*wire) else {
                    continue;
                };

                let changed = state
                    .state_lock
                    .wires
                    .set_wire(wire.id, wire_state.state.clone());
                if !changed && !force_pin_updates {
                    continue;
                }

                wire_state.write_pins(
                    &mut state.state_lock,
                    wire.connected_pins.read().as_slice(),
                    &mut tasks,
                );
            }

            state.state_lock.flush_external_tasks();
            state.state_lock.add_tasks(&mut tasks.drain());
        }
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
