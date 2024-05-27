use std::{num::NonZeroU32, sync::Arc};

use eframe::egui::ahash::HashMap;

use crate::{containers::{Chunks2D, FixedVec}, vector::{Vec2isize, Vec2usize}, Direction4Half, Direction8};

pub struct Board {
    wires: FixedVec<Arc<Wire>>,
}

impl Default for Board {
    fn default() -> Self {
        Self { wires: vec![].into() }
    }
}

#[derive(Default)]
pub struct BoardEditor {
    pub wires: Chunks2D<16, WireNode>,

    pub board: Board,
}

impl BoardEditor {
    pub fn place_wire(&mut self, pos: Vec2isize, dir: Direction8, length: usize) {
        todo!()
    }
}

pub struct Wire {
    id: usize,

    points: HashMap<Vec2usize, WirePoint>,
}

pub struct WirePoint {
    directions: [bool; 4],
}

impl WirePoint {
    pub fn get_direction(&self, dir: Direction4Half) -> bool {
        self.directions[dir.into_index()]
    }

    pub fn get_direction_mut(&mut self, dir: Direction4Half) -> &mut bool {
        &mut self.directions[dir.into_index()]
    }
}

#[derive(Default)]
pub struct WireNode {
    pub wire: Option<Arc<Wire>>,
    directions: [Option<NonZeroU32>; 8],
}

impl WireNode {
    pub fn get_direction(&self, dir: Direction8) -> Option<NonZeroU32> {
        self.directions[dir.into_index()]
    }

    pub fn get_direction_mut(&mut self, dir: Direction8) -> &mut Option<NonZeroU32> {
        &mut self.directions[dir.into_index()]
    }
}