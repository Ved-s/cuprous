use std::{num::NonZeroU32, ops::Deref, sync::Arc};

use eframe::egui::ahash::{HashMap, HashMapExt};

use crate::{
    containers::{Chunks2D, FixedVec},
    vector::{Vec2isize, Vec2usize},
    Direction4Half, Direction8, Direction8Array,
};

pub struct Board {
    wires: FixedVec<Arc<Wire>>,
}

impl Default for Board {
    fn default() -> Self {
        Self {
            wires: vec![].into(),
        }
    }
}

impl Board {
    pub fn create_wire(&mut self) -> Arc<Wire> {
        let id = self.wires.first_free_pos();
        let wire = Wire {
            id,
            points: Default::default(),
        };
        let arc = Arc::new(wire);
        self.wires.set(id, arc.clone());
        arc
    }
}

#[derive(Default)]
pub struct BoardEditor {
    pub wires: Chunks2D<16, WireNode>,

    pub board: Board,
}

//
// TODO! reflect changes in backend wires
// 

impl BoardEditor {
    pub fn place_wire(&mut self, pos: Vec2isize, dir: Direction8, length: NonZeroU32) {
        let mut wire_map = HashMap::new();

        for pos in dir.iter_along(pos, length.get() as usize) {
            if let Some(wire) = self.wires.get(pos).and_then(|n| n.wire.clone()) {
                wire_map.insert(wire.id, wire);
            }
        }

        let wire = self.set_wire_point(pos, None);
        let other_wire = self.set_wire_point(
            pos + dir.into_dir_isize() * length.get() as isize,
            Some(wire.clone()),
        );

        wire_map.insert(wire.id, wire);
        wire_map.insert(other_wire.id, other_wire);

        self.set_distances(pos, dir, length.get() as usize, true, true);

        if wire_map.len() <= 1 {
            return;
        }

        self.merge_many_wires(wire_map.values().cloned());
    }

    fn set_distances(
        &mut self,
        pos: Vec2isize,
        dir: Direction8,
        length: usize,
        set: bool,
        bidirectional: bool,
    ) {
        if bidirectional {
            self.set_distances(pos, dir, length, set, false);
            self.set_distances(
                pos + dir.into_dir_isize() * length as isize,
                dir.inverted(),
                length,
                set,
                false,
            );
            return;
        }

        let mut distance = 0;

        for i in 0..length {
            let pos = pos + dir.into_dir_isize() * i as isize;
            let node = self.wires.get_or_create_mut(pos);

            if set {
                if distance > 0 {
                    *node.directions.get_mut(dir.inverted()) = NonZeroU32::new(distance);

                    distance += 1;
                }

                if node.wire.is_some() {
                    distance = 1;
                }
            } else {
                *node.directions.get_mut(dir.inverted()) = None;
            }
        }
    }

    fn set_wire_point(&mut self, pos: Vec2isize, new_wire: Option<Arc<Wire>>) -> Arc<Wire> {
        let node = self.wires.get(pos);

        if let Some(wire) = node.and_then(|n| n.wire.clone()) {
            return wire;
        }

        let directions = self.examine_directions(pos);

        let node = self.wires.get_or_create_mut(pos);

        let wire = match &directions {
            None => new_wire.unwrap_or_else(|| self.board.create_wire()),
            Some(array) => {
                let mut biggest_wire = None;
                let mut max_points = None;

                for (_, wire) in array.iter() {
                    let Some(wire) = wire else {
                        continue;
                    };
                    if !max_points.is_some_and(|mp| wire.1.points.len() <= mp) {
                        max_points = Some(wire.1.points.len());
                        biggest_wire = Some(wire.1.clone());
                    }
                }

                biggest_wire
                    .or(new_wire)
                    .unwrap_or_else(|| self.board.create_wire())
            }
        };

        node.wire = Some(wire.clone());

        self.fix_directions_at_intersection(pos);

        wire
    }

    fn remove_wire_point(&mut self, pos: Vec2isize) -> Option<Arc<Wire>> {
        let node = self.wires.get(pos);

        if let Some(node) = node {
            if node.wire.is_none() {
                return None;
            }
        }

        let node = self.wires.get_or_create_mut(pos);

        let ret = node.wire.take();

        self.fix_directions_at_intersection(pos);

        ret
    }

    fn fix_directions_at_intersection(&mut self, pos: Vec2isize) {
        let Some(directions) = self.examine_directions(pos) else {
            return;
        };

        for dir in Direction8::ALL {
            let forward_len = directions.get(dir).as_ref().map(|d| d.0.get()).unwrap_or(0);
            let backward_len = directions
                .get(dir.inverted())
                .as_ref()
                .map(|d| d.0.get())
                .unwrap_or(0);

            let total_len = forward_len + backward_len;
            if total_len == 0 {
                continue;
            }

            let start = pos + dir.inverted().into_dir_isize() * backward_len as isize;
            self.set_distances(start, dir, total_len as usize, true, false);
        }
    }

    fn examine_directions(
        &self,
        pos: Vec2isize,
    ) -> Option<Direction8Array<Option<(NonZeroU32, Arc<Wire>)>>> {
        let node = self.wires.get(pos)?;
        Some(Direction8Array::from_fn(|dir| {
            let dist = (*node.directions.get(dir))?;

            let pos = pos + dir.into_dir_isize() * dist.get() as isize;

            let node = self.wires.get(pos)?; // Return: invalid node!
            let wire = node.wire.clone()?;

            Some((dist, wire))
        }))
    }

    fn merge_many_wires<I>(&mut self, iter: I)
    where
        I: Clone + Iterator<Item = Arc<Wire>>,
    {
        let mut biggest_wire = None;
        let mut max_points = None;

        for wire in iter.clone() {
            if !max_points.is_some_and(|mp| wire.points.len() <= mp) {
                max_points = Some(wire.points.len());
                biggest_wire = Some(wire.clone());
            }
        }

        let Some(biggest_wire) = biggest_wire else {
            return;
        };

        for wire in iter {
            if wire.id == biggest_wire.id {
                continue;
            }

            self.merge_wires(wire, biggest_wire.clone());
        }
    }

    fn merge_wires(&mut self, from: Arc<Wire>, into: Arc<Wire>) {
        // unimplemented!
    }

    fn unmerge_wire(&mut self, wire: Arc<Wire>) {
        // unimplemented!
    }

    fn remove_unneeded_point(&mut self, pos: Vec2isize) {
        // unimplemented!
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

#[derive(Default, Clone)]
pub struct WireNode {
    pub wire: Option<Arc<Wire>>,
    pub directions: Direction8Array<Option<NonZeroU32>>,
}
