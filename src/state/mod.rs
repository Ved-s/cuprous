use std::{
    ops::Deref,
    sync::{Arc, Weak, atomic::Ordering},
    time::Duration,
};

use parking_lot::Mutex;

use crate::{
    Style, board::Board, components::{ComponentPin, ComponentUpdateReason, PinType, UntypedComponentCtx}, io::savestate, multiwire::MultiwireTargetState, pool::get_pooled, state::{
        components::{BoardComponentsState, ComponentState},
        sim::{
            BoardSimulationState, ComponentUpdateTask, ExternalTaskPool, InputUpdateTask,
            UpdateTask, UpdateTaskPool, WireUpdateTask,
        },
        wires::{BoardWiresState, WireState, WireStateHandler},
    }, time::{self, Instant, TimeProvider}
};

pub mod components;
pub mod sim;
pub mod wires;

pub enum StateRunResult {
    Done,
    DoneUntil(Instant),
    RunMultiwireUpdate {
        wire: usize,
        force_pin_updates: bool,
    },
}

struct WireRequiresMultiwireUpdate;

pub struct BoardState {
    id: u128,
    pub wires: BoardWiresState,
    pub components: BoardComponentsState,
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
            components: Default::default(),
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
        self.components.reset();
        self.sim.reset();
    }

    pub fn save(&mut self, start: Instant) -> savestate::BoardState {
        let board = self
            .board
            .upgrade()
            .expect("tried to save state without attached board");

        let wires = board.wires().read();
        let components = board.components().read();

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
            components: self
                .components
                .inner
                .inner
                .iter()
                .enumerate()
                .map(|(i, v)| {
                    let v = v.as_ref()?;
                    let component = components.get(i)?;
                    let pins = component.pins.read();

                    Some(savestate::ComponentState {
                        pins: v
                            .pins
                            .iter()
                            .enumerate()
                            .take(pins.len())
                            .map(|(i, v)| (pins[i].desc.id.clone(), v.clone()))
                            .collect(),
                        internal: {
                            v.internal.as_ref().and_then(|int| {
                                let imp = component.imp.read();
                                imp.imp.save_state(component, &imp.instance, int)
                            })
                        },
                    })
                })
                .collect(),
            sim: self.sim.save(start),
        }
    }

    pub fn load_stage1_shallow(&mut self, data: &mut savestate::BoardState) {
        self.wires.wires.clone_from(&data.wires);
    }

    pub fn load_stage2_components(&mut self, data: &mut savestate::BoardState) {
        let board = self
            .board
            .upgrade()
            .expect("tried to load state without attached board");

        let board_components = board.components();
        let board_components = board_components.read();
        for (i, component_data) in data.components.iter().enumerate() {
            let Some(component_data) = component_data else {
                continue;
            };

            let board_component = board_components.get(i).expect("loaded components");

            let pins = board_component
                .pins
                .read()
                .iter()
                .map(|p| {
                    component_data
                        .pins
                        .get(&p.desc.id)
                        .cloned()
                        .unwrap_or(WireState::None)
                })
                .collect();

            let component = ComponentState {
                pins,
                internal: Default::default(),
            };

            self.components.inner.set(i, component);
        }
    }

    pub fn load_stage3_component_states(&mut self, data: &mut savestate::BoardState) {
        let board = self
            .board
            .upgrade()
            .expect("tried to load state without attached board");

        let board_components = board.components();
        let board_components = board_components.read();
        for (i, component_data) in data.components.iter().enumerate() {
            let Some(component_data) = component_data else {
                continue;
            };

            let Some(state_data) = &component_data.internal else {
                continue;
            };

            let board_component = board_components.get(i).expect("loaded components");
            let component = self.components.inner.get_mut(i).expect("loaded components");

            let imp = board_component.imp.read();

            // todo: errors
            let state = imp
                .imp
                .load_state(board_component, &imp.instance, state_data)
                .ok();
            component.internal = state;
        }
    }

    pub fn load_stage4_simulation_data(
        &mut self,
        data: &mut savestate::BoardState,
        start: Instant,
    ) {
        self.sim.load(std::mem::take(&mut data.sim), start);
    }

    pub fn pin_color(&self, pin: &ComponentPin, style: &Style) -> eframe::egui::Color32 {
        let connected_wire = pin.wire.read().clone();
        match connected_wire {
            None => style
                .wire_colors
                .get(&self.components.get_pin(pin.component.id, pin.id)),
            Some(wire) => {
                // TODO: do something when wire state and pin state don't match
                self.wires.wire_color(&wire, style)
            }
        }
    }

    pub fn add_tasks(&mut self, tasks: &mut dyn Iterator<Item = UpdateTask>) {
        self.sim.add_tasks(tasks, false, None);
    }

    pub fn flush_external_tasks(&mut self) {
        self.sim.flush_tasks();
    }

    pub fn set_external_tasks(&mut self, external_tasks: Arc<Mutex<ExternalTaskPool>>) {
        self.sim.set_external_tasks(external_tasks);
    }

    pub fn run(&mut self, task_limit: &mut usize) -> StateRunResult {
        let mut tasks = get_pooled::<UpdateTaskPool>();

        self.sim.flush_tasks();

        // todo: correct time provider
        let time_provider = &time::SYSTEM;

        'main_loop: while *task_limit > 0 {
            let mut queue_immediately = false;
            let mut meta = None;

            if let Ok(id) = self.sim.next_update(time_provider.now()) {
                let task = ComponentUpdateTask {
                    id,
                    reason: ComponentUpdateReason::Timer,
                };
                self.update_component(task, &mut tasks);
            } else {
                let Some((task, m)) = self.sim.next_task() else {
                    break 'main_loop;
                };
                meta = Some(m);

                match task {
                    UpdateTask::Wire(w) => match self.update_wire(w, &mut tasks) {
                        Ok(()) => (),
                        Err(WireRequiresMultiwireUpdate) => {
                            return StateRunResult::RunMultiwireUpdate {
                                wire: w.id,
                                force_pin_updates: w.force_pin_updates,
                            };
                        }
                    },
                    UpdateTask::Component(c) => {
                        self.update_component(c, &mut tasks);
                    }
                    UpdateTask::Input(i) => {
                        self.update_input(i, &mut tasks, &mut queue_immediately);
                    }
                    UpdateTask::DropComponent(d) => {
                        self.components.drop_component(d.id, d.pin_only);
                    }
                }
            }

            *task_limit -= 1;
            self.sim
                .add_tasks(&mut tasks.drain(), queue_immediately, meta.as_ref());
        }

        match self.sim.next_update_time() {
            Some(time) => StateRunResult::DoneUntil(time),
            None => StateRunResult::Done,
        }
    }

    fn update_wire(
        &mut self,
        task: WireUpdateTask,
        tasks: &mut UpdateTaskPool,
    ) -> Result<(), WireRequiresMultiwireUpdate> {
        let Some(wire) = self.board().wires().read().get(task.id).cloned() else {
            return Ok(());
        };

        if wire.is_multiwire.load(Ordering::Relaxed) {
            return Err(WireRequiresMultiwireUpdate);
        }

        let pins = wire.connected_pins.read();

        let mut state = WireStateHandler::default();

        state.read_pins(self, pins.as_slice());

        // TODO: Modify?

        let changed = self.wires.set_wire(wire.id, state.state.clone());
        if !changed && !task.force_pin_updates {
            return Ok(());
        }

        state.write_pins(self, pins.as_slice(), tasks);

        // todo: figure out if this is still needed
        tasks.shuffle();

        Ok(())
    }

    fn update_component(&mut self, task: ComponentUpdateTask, tasks: &mut UpdateTaskPool) {
        let Some(component) = self.board().components().read().get(task.id).cloned() else {
            return;
        };

        let imp = component.imp.read();

        let ctx = UntypedComponentCtx {
            state: self,
            component: &component,
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
        let Some(pin) = self
            .board()
            .components()
            .read()
            .get(task.component)
            .and_then(|c| c.pins.read().get(task.pin).map(|p| p.pin.clone()))
        else {
            return;
        };

        match pin.ty {
            PinType::Inside => {
                let state = match pin.wire.read().deref() {
                    None => WireState::None,
                    Some(wire) => self.wires.get_wire(wire.id),
                };

                let changed = self.components.set_pin(task.component, task.pin, state);

                if changed && task.update_component {
                    tasks.add_component_task(
                        task.component,
                        ComponentUpdateReason::ChangedPin(task.pin),
                    );
                    *queue_immediately = true;
                }
            }
            PinType::Outside => {},
            PinType::Multiwire => {
                match pin.wire.read().deref() {
                    None => {
                        let simulation = self.board().simulation();
                        let Some(state_data) = simulation.states().read().get(&self.uid()).cloned() else {
                            return;
                        };

                        let router = state_data.get_multiwire_router(task.component, Some(self));

                        let mut routes = vec![];
                        router.route(task.pin, &mut routes);

                        for route in routes {
                            match route.target_state {
                                MultiwireTargetState::CurrentState => {
                                    tasks.add_wire_task(route.wire_id, false);
                                },
                                MultiwireTargetState::Uid(uid) => {
                                    let states = simulation.states().read();
                                    let Some(state) = states.get(&uid) else {
                                        continue;
                                    };

                                    state.add_tasks(&mut [WireUpdateTask {
                                        id: route.wire_id,
                                        force_pin_updates: false,
                                    }.into()].into_iter());
                                },
                            }
                        }
                        *queue_immediately = true;
                    },
                    Some(wire) => {
                        tasks.add_wire_task(wire.id, false);
                        *queue_immediately = true;
                    },
                };
            }
        };
    }

    pub fn get_timer(&self, component_id: usize) -> Option<(Instant, Option<Duration>)> {
        self.sim.find_update(component_id)
    }

    pub fn set_timer(&mut self, component_id: usize, at: Instant, interval: Option<Duration>) {
        self.sim.schedule_update(component_id, at, interval);
    }

    pub fn reset_timer(&mut self, component_id: usize) {
        self.sim.stop_update(component_id);
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

    pub fn load_stage2_components(&self, data: &savestate::BoardStates) {
        for (i, state_data) in data.iter().enumerate() {
            let Some(state_data) = state_data else {
                continue;
            };

            let state = self.get(i).expect("preloaded state collection");
            state.load_stage2_components(state_data);
        }
    }

    pub fn load_stage3_component_states(&self, data: &savestate::BoardStates) {
        for (i, state_data) in data.iter().enumerate() {
            let Some(state_data) = state_data else {
                continue;
            };

            let state = self.get(i).expect("preloaded state collection");
            state.load_stage3_component_states(state_data);
        }
    }
}
*/
