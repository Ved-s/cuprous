use std::{collections::HashMap, sync::Arc};

use parking_lot::RwLock;

use crate::board::Board;

pub struct SimulationCtx {
    boards: RwLock<HashMap<u128, Arc<Board>>>
}

impl SimulationCtx {
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

    pub fn notify_state(&self, board: u128, id: usize) {
        
    }
    
    
}