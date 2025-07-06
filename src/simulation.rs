use std::{collections::HashMap, sync::Arc};

use parking_lot::RwLock;

use crate::{board::Board, circuits::CircuitBlueprint, io::savestate};

pub struct SimulationCtx {
    boards: RwLock<HashMap<u128, Arc<Board>>>
}

impl SimulationCtx {

    pub fn new() -> Arc<Self> {
        let this = Arc::new(Self {
            boards: RwLock::new(HashMap::new()),
        });

        let main = Board::new(this.clone());
        this.boards.write().insert(main.uid(), main);
        this
    }

    pub fn boards(&self) -> &RwLock<HashMap<u128, Arc<Board>>> {
        &self.boards
    }

    pub fn temp_run(&self) {
        for board in self.boards.read().values() {
            for state in board.states().read().iter() {
                state.run(&mut 100);
            }
        }
    }

    pub fn notify_state(&self, board: u128, id: usize) {
        
    }
    
    pub fn save(&self) -> savestate::Simulation {
        savestate::Simulation {
            boards: self.boards.read().values().map(|b| b.save()).collect()
        }
    }

    pub fn load(data: &savestate::Simulation, blueprints: &[Arc<RwLock<CircuitBlueprint>>]) -> Arc<Self> {
        let this = Arc::new(Self { boards: Default::default() });

        let mut boards = this.boards.write();

        for board_data in &data.boards {
            let board = Board::preload(board_data, this.clone());
            boards.insert(board.uid(), board);
        }

        drop(boards);

        let boards = this.boards.read();

        // load everything that doesn't reference other loaded stuff
        for board_data in &data.boards {
            boards[&board_data.uid].load_stage1_shallow(board_data, blueprints);
        }

        // calculate circuit size and pins, connect pins to wires, load pin states, load circuit instances
        for board_data in &data.boards {
            boards[&board_data.uid].load_stage2_circuits(board_data);
        }

        // load circuit states
        for board_data in &data.boards {
            boards[&board_data.uid].load_stage3_circuit_states(board_data);
        }

        drop(boards);

        this
    }
}