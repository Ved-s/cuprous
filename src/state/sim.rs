use std::{
    collections::VecDeque,
    hash::{BuildHasher, DefaultHasher, Hasher, RandomState},
};

use smoldata::SmolReadWrite;

use crate::pool::{get_pooled, Pooled};

#[derive(Default)]
pub struct UpdateTaskPool(Vec<UpdateTask>);

impl UpdateTaskPool {
    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn add_wire_task(&mut self, id: usize, force_pin_updates: bool) {
        self.add(WireUpdateTask {
            id,
            force_pin_updates,
        })
    }

    pub fn add_circuit_task(&mut self, id: usize, changed_pin: Option<usize>) {
        self.add(CircuitUpdateTask { id, changed_pin })
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

    // fn add_internal(&mut self, task: UpdateTask) {
    //     todo!()
    // match task {
    //     UpdateTask::Wire(WireUpdateTask {
    //         id,
    //         force_pin_updates,
    //     }) => match force_pin_updates {
    //         false => {
    //             if self.0.contains(
    //                 &WireUpdateTask {
    //                     id,
    //                     force_pin_updates: true,
    //                 }
    //                 .into(),
    //             ) {
    //                 return;
    //             }
    //         }
    //         true => {
    //             self.0.retain(|u| match u {
    //                 UpdateTask::Wire(WireUpdateTask { id: eid, .. }) => *eid != id,
    //                 _ => true,
    //             });
    //         }
    //     },
    //     UpdateTask::Circuit(CircuitUpdateTask { id, changed_pin }) => match changed_pin {
    //         Some(_) => {
    //             if self.0.contains(
    //                 &CircuitUpdateTask {
    //                     id,
    //                     changed_pin: None,
    //                 }
    //                 .into(),
    //             ) {
    //                 return;
    //             }
    //         }
    //         None => {
    //             self.0.retain(|u| match u {
    //                 UpdateTask::Circuit(CircuitUpdateTask { id: eid, .. }) => *eid != id,
    //                 _ => true,
    //             });
    //         }
    //     },
    //     UpdateTask::Input(..) => {}
    // }
    // self.0.insert(task);
    // }

    pub fn iter(&self) -> impl Iterator<Item = UpdateTask> + '_ {
        self.0.iter().copied()
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

#[derive(Clone, Copy, Hash, PartialEq, Eq, SmolReadWrite)]
pub struct DropCircuitTask {
    pub id: usize,
    pub pin_only: Option<usize>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, SmolReadWrite)]
pub struct InputUpdateTask {
    pub circuit: usize,
    pub pin: usize,
    pub update_circuit: bool,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, SmolReadWrite)]
pub struct WireUpdateTask {
    pub id: usize,
    pub force_pin_updates: bool,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, SmolReadWrite)]
pub struct CircuitUpdateTask {
    pub id: usize,
    pub changed_pin: Option<usize>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
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

pub struct BoardSimulationState {
    current_tasks: VecDeque<UpdateTask>,

    next_tasks: VecDeque<Pooled<UpdateTaskPool>>,
    current_epoch: usize,
    hasher: DefaultHasher,
}

impl BoardSimulationState {
    pub fn new() -> Self {
        Self {
            current_tasks: Default::default(),
            next_tasks: Default::default(),
            current_epoch: 0,
            hasher: RandomState::new().build_hasher(),
        }
    }
    // fn save(&self) -> savestate::BoardStateSimulation {
    //     savestate::BoardStateSimulation {
    //         wires: self.wires.iter().cloned().collect(),
    //         circuits: self.circuits.iter().cloned().collect(),
    //     }
    // }

    // fn load(&mut self, data: &savestate::BoardStateSimulation) {
    //     self.wires = data.wires.iter().cloned().collect();
    //     self.circuits = data.circuits.iter().cloned().collect();
    // }

    pub fn next_task(&mut self) -> Option<(UpdateTask, UpdateTaskMetadata)> {
        loop {
            if let Some(task) = self.current_tasks.pop_front() {
                let meta = UpdateTaskMetadata {
                    epoch: self.current_epoch,
                    can_be_a_bit_late: matches!(task, UpdateTask::Circuit(_)),
                };
                break Some((task, meta));
            }

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
                    if self.hasher.finish() & 1 == 0 {
                        2
                    } else {
                        1
                    }
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
