use std::{
    collections::{BTreeSet, BinaryHeap, VecDeque},
    hash::{BuildHasher, DefaultHasher, Hasher, RandomState},
    sync::Arc,
    time::Duration,
};

use parking_lot::Mutex;
use smoldata::SmolReadWrite;

use crate::{
    circuits::CircuitUpdateReason,
    pool::{Pooled, get_pooled}, time::Instant,
};

#[derive(Default)]
pub struct UpdateTaskPool(Vec<UpdateTask>);

impl UpdateTaskPool {
    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn add_wire_task(&mut self, id: usize, force_pin_updates: bool) {
        self.add(WireUpdateTask {
            id,
            force_pin_updates,
        })
    }

    pub fn add_circuit_task(&mut self, id: usize, reason: CircuitUpdateReason) {
        self.add(CircuitUpdateTask { id, reason })
    }

    pub fn add_update_input_task(&mut self, circuit: usize, pin: usize, update_circuit: bool) {
        self.add(InputUpdateTask {
            circuit,
            pin,
            update_circuit,
        });
    }

    pub fn add_drop_circuit_task(&mut self, id: usize, pin_only: Option<usize>) {
        self.add(DropCircuitTask { id, pin_only });
    }

    pub fn add(&mut self, task: impl Into<UpdateTask>) {
        self.0.push(task.into());
    }

    pub fn iter(&self) -> impl Iterator<Item = UpdateTask> + '_ {
        self.0.iter().cloned()
    }

    pub fn drain(&mut self) -> impl Iterator<Item = UpdateTask> + '_ {
        self.0.drain(..)
    }

    pub fn shuffle(&mut self) {
        if self.0.len() <= 1 {
            return;
        }

        let mut hasher = RandomState::new().build_hasher();
        hasher.write_usize(self.0.len());

        for i in 0..self.0.len() - 1 {
            hasher.write_usize(i);
            let swap = hasher.finish() as usize % self.0.len();
            if swap != i {
                self.0.swap(i, swap);
            }
        }
    }
}

enum UpdateTaskSeparated {
    Task(UpdateTask),
    Separator,
}

#[derive(Default)]
pub struct ExternalTaskPool(VecDeque<UpdateTaskSeparated>);

impl ExternalTaskPool {
    pub fn add_tasks(&mut self, tasks: &mut dyn Iterator<Item = UpdateTask>) {
        if !self.0.is_empty() {
            self.0.push_back(UpdateTaskSeparated::Separator);
        }
        self.0.extend(tasks.map(UpdateTaskSeparated::Task));
    }

    pub fn next_batch(&mut self) -> Option<ExternalTaskPoolBatch<'_>> {
        if self.0.is_empty() {
            None
        } else {
            Some(ExternalTaskPoolBatch(Some(&mut self.0)))
        }
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }
}

pub struct ExternalTaskPoolBatch<'a>(Option<&'a mut VecDeque<UpdateTaskSeparated>>);

impl Iterator for ExternalTaskPoolBatch<'_> {
    type Item = UpdateTask;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.0.as_deref_mut()?.pop_front();
        match item {
            Some(UpdateTaskSeparated::Separator) | None => {
                self.0 = None;
                None
            }
            Some(UpdateTaskSeparated::Task(task)) => Some(task),
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, SmolReadWrite)]
pub struct DropCircuitTask {
    pub id: usize,
    pub pin_only: Option<usize>,
}

#[derive(Clone, Hash, PartialEq, Eq, SmolReadWrite)]
pub struct InputUpdateTask {
    pub circuit: usize,
    pub pin: usize,
    pub update_circuit: bool,
}

#[derive(Clone, Hash, PartialEq, Eq, SmolReadWrite)]
pub struct WireUpdateTask {
    pub id: usize,
    pub force_pin_updates: bool,
}

#[derive(Clone, Hash, PartialEq, Eq, SmolReadWrite)]
pub struct CircuitUpdateTask {
    pub id: usize,
    pub reason: CircuitUpdateReason,
}

#[derive(Clone, Hash, PartialEq, Eq, SmolReadWrite)]
pub enum UpdateTask {
    Wire(WireUpdateTask),
    Circuit(CircuitUpdateTask),
    Input(InputUpdateTask),
    DropCircuit(DropCircuitTask),
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

impl From<InputUpdateTask> for UpdateTask {
    fn from(value: InputUpdateTask) -> Self {
        Self::Input(value)
    }
}

impl From<DropCircuitTask> for UpdateTask {
    fn from(value: DropCircuitTask) -> Self {
        Self::DropCircuit(value)
    }
}

pub struct UpdateTaskMetadata {
    epoch: usize,
    can_be_a_bit_late: bool,
}

#[derive(Clone, Copy)]
struct CircuitUpdateDeadline {
    at: Instant,
    id: usize,
    interval: Option<Duration>,
}

impl std::cmp::Eq for CircuitUpdateDeadline {}
impl std::cmp::PartialEq for CircuitUpdateDeadline {
    fn eq(&self, other: &Self) -> bool {
        self.at == other.at || self.id == other.id
    }
}

impl std::cmp::PartialOrd for CircuitUpdateDeadline {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl std::cmp::Ord for CircuitUpdateDeadline {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.at.cmp(&other.at).reverse()
    }
}

pub struct BoardSimulationState {
    current_tasks: VecDeque<UpdateTask>,

    next_tasks: VecDeque<Pooled<UpdateTaskPool>>,
    current_epoch: usize,
    hasher: DefaultHasher,

    external_tasks: Option<Arc<Mutex<ExternalTaskPool>>>,

    // (deadline, circuit id) => interval
    circuit_updates: BinaryHeap<CircuitUpdateDeadline>,
    active_circuit_updates: BTreeSet<usize>,
}

impl BoardSimulationState {
    pub fn new() -> Self {
        Self {
            current_tasks: Default::default(),
            next_tasks: Default::default(),
            current_epoch: 0,
            hasher: RandomState::new().build_hasher(),

            external_tasks: None,

            circuit_updates: Default::default(),
            active_circuit_updates: Default::default(),
        }
    }

    pub fn flush_tasks(&mut self) {
        let Some(ext) = self.external_tasks.clone() else {
            return;
        };

        let mut ext = ext.lock();
        while let Some(mut batch) = ext.next_batch() {
            self.add_tasks(&mut batch, false, None);
        }
    }

    pub fn next_task(&mut self) -> Option<(UpdateTask, UpdateTaskMetadata)> {
        loop {
            if let Some(task) = self.current_tasks.pop_front() {
                let meta = UpdateTaskMetadata {
                    epoch: self.current_epoch,
                    can_be_a_bit_late: matches!(task, UpdateTask::Circuit(_)),
                };
                break Some((task, meta));
            }

            self.flush_tasks();

            if let Some(mut next) = self.next_tasks.pop_front() {
                self.current_epoch = self.current_epoch.wrapping_add(1);

                self.current_tasks.extend(next.drain());
                continue;
            }

            break None;
        }
    }

    pub fn add_tasks(
        &mut self,
        tasks: &mut dyn Iterator<Item = UpdateTask>,
        queue_immediately: bool,
        meta: Option<&UpdateTaskMetadata>,
    ) {
        let mut start_epoch = if queue_immediately {
            self.current_epoch
        } else {
            meta.map(|m| {
                let offset = if m.can_be_a_bit_late {
                    self.hasher.write_usize(self.current_epoch);
                    if self.hasher.finish() & 1 == 0 { 2 } else { 1 }
                } else {
                    1
                };
                m.epoch.wrapping_add(offset)
            })
            .unwrap_or(self.current_epoch)
        };

        if start_epoch == self.current_epoch {
            if let Some(task) = tasks.next() {
                self.current_tasks.push_back(task);
            } else {
                return;
            }
            start_epoch = start_epoch.wrapping_add(1);
        }

        let mut next_tasks_index = start_epoch.wrapping_sub(self.current_epoch).wrapping_sub(1);
        for task in tasks {
            while self.next_tasks.len() <= next_tasks_index {
                self.next_tasks.push_back(get_pooled());
            }
            self.next_tasks[next_tasks_index].add(task);
            next_tasks_index += 1;
        }

        // let mut tasks = tasks.peekable();
        // if tasks.peek().is_none() {
        //     return;
        // }

        // let mut circuits = get_pooled::<Vec<CircuitUpdateTask>>();
        // let mut wires = get_pooled::<Vec<WireUpdateTask>>();
        // let mut hasher = get_pooled::<DefaultHasher>();

        // for task in tasks {
        //     match task {
        //         UpdateTask::Wire(w) => wires.push(w),
        //         UpdateTask::Circuit(c) => circuits.push(c),
        //         UpdateTask::Input(i) => todo!(),
        //                     }
        // }

        // if !circuits.is_empty() {
        //     hasher.write_usize(circuits.len());
        //     for i in 0..circuits.len() {
        //         hasher.write_usize(i);
        //         let other = hasher.finish() as usize % circuits.len();
        //         if other == i {
        //             continue;
        //         }

        //         circuits.swap(i, other);
        //     }
        // }

        // self.wires.extend(wires.drain(..));
        // self.circuits.extend(circuits.drain(..));
    }

    pub fn has_jobs(&self) -> bool {
        !self.current_tasks.is_empty() || self.next_tasks.iter().any(|n| !n.is_empty())
    }

    pub fn reset(&mut self) {
        self.current_tasks.clear();
        self.next_tasks.clear();
        self.current_epoch = 0;
    }

    pub fn set_external_tasks(&mut self, external_tasks: Arc<Mutex<ExternalTaskPool>>) {
        self.external_tasks = Some(external_tasks);
    }

    pub fn next_update(&mut self, now: Instant) -> Result<usize, Option<Instant>> {
        let Some(&first) = self.circuit_updates.peek() else {
            return Err(None);
        };

        if first.at > now {
            return Err(Some(first.at));
        }

        self.circuit_updates.pop();

        match first.interval {
            Some(interval) => {
                self.circuit_updates.push(CircuitUpdateDeadline {
                    at: first.at + interval,
                    ..first
                });
            }
            None => {
                self.active_circuit_updates.remove(&first.id);
            }
        }

        Ok(first.id)
    }

    pub fn schedule_update(&mut self, id: usize, at: Instant, interval: Option<Duration>) {
        let active = self.active_circuit_updates.contains(&id);
        if active {
            self.circuit_updates.retain(|d| d.id != id);
        } else {
            self.active_circuit_updates.insert(id);
        }

        self.circuit_updates
            .push(CircuitUpdateDeadline { at, id, interval });
    }

    pub fn find_update(&self, id: usize) -> Option<(Instant, Option<Duration>)> {
        if !self.active_circuit_updates.contains(&id) {
            return None;
        }

        self.circuit_updates.iter().find(|d| d.id == id).map(|d| (d.at, d.interval))
    }

    pub fn stop_update(&mut self, id: usize) {
        if !self.active_circuit_updates.contains(&id) {
            return;
        }

        self.circuit_updates.retain(|d| d.id != id);
    }

    pub fn next_update_time(&self) -> Option<Instant> {
        self.circuit_updates.peek().map(|d| d.at)
    }

    pub fn save(&mut self) -> crate::io::savestate::BoardStateSimulation {
        self.flush_tasks();

        let mut cap = self.current_tasks.len();
        for pool in &self.next_tasks {
            if cap > 0 {
                cap += 1;
            }
            cap += pool.len();
        }

        let mut vec = Vec::with_capacity(cap);
        vec.extend(self.current_tasks.iter().cloned().map(Some));

        for pool in &self.next_tasks {
            if !vec.is_empty() {
                vec.push(None);
            }
            vec.extend(pool.iter().map(Some));
        }

        crate::io::savestate::BoardStateSimulation { tasks: vec }
    }

    pub fn load(&mut self, sim: crate::io::savestate::BoardStateSimulation) {
        self.current_epoch = 0;
        self.current_tasks.clear();
        self.next_tasks.clear();

        let mut iter = sim.tasks.into_iter();

        while let Some(Some(task)) = iter.next() {
            self.current_tasks.push_back(task);
        }

        let mut iter = iter.peekable();
        loop {
            if iter.peek().is_none() {
                break;
            }

            let mut pool = get_pooled::<UpdateTaskPool>();

            while let Some(Some(task)) = iter.next() {
                pool.add(task);
            }
            self.next_tasks.push_back(pool);
        }
    }
}

impl Default for BoardSimulationState {
    fn default() -> Self {
        Self::new()
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
