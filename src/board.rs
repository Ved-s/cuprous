use std::{
    collections::{HashMap, HashSet},
    num::NonZeroU32,
    sync::Arc,
};

use parking_lot::RwLock;

use crate::{
    containers::{Chunks2D, FixedVec},
    vector::Vec2isize,
    Direction4HalfArray, Direction8, Direction8Array, CHUNK_SIZE,
};

pub struct Board {
    pub wires: RwLock<FixedVec<Arc<Wire>>>,
}

impl Default for Board {
    fn default() -> Self {
        Self {
            wires: RwLock::new(vec![].into()),
        }
    }
}

impl Board {
    pub fn create_wire(&self) -> Arc<Wire> {
        let mut wires = self.wires.write();
        let id = wires.first_free_pos();
        let wire = Wire {
            id,
            points: Default::default(),
        };
        let arc = Arc::new(wire);
        wires.set(id, arc.clone());
        arc
    }

    pub fn free_wire(&self, wire: &Arc<Wire>) {
        let mut wires = self.wires.write();
        let Some(ewire) = wires.inner.get(wire.id) else {
            return;
        };

        if ewire.as_ref().is_some_and(|w| Arc::ptr_eq(w, wire)) {
            wires.remove(wire.id);
        }
    }
}

#[derive(Default)]
pub struct BoardEditor {
    pub wires: Chunks2D<CHUNK_SIZE, WireNode>,

    pub board: Arc<Board>,
}

impl BoardEditor {
    pub fn place_wire(&mut self, pos: Vec2isize, dir: Direction8, length: NonZeroU32) {
        let mut wire_map = HashMap::new();

        for pos in dir.iter_along(pos, length.get() as usize) {
            if let Some(wire) = self.wires.get(pos).and_then(|n| n.wire.clone()) {
                wire_map.insert(wire.id, wire);
            }
        }

        let other_pos = pos + dir.into_dir_isize() * length.get() as isize;

        for pos in [pos, other_pos] {
            let Some(dirs) = self.examine_directions(pos) else {
                continue;
            };

            for wire in dirs.values() {
                let Some((_, wire)) = wire else {
                    continue;
                };

                wire_map.insert(wire.id, wire.clone());
            }
        }

        let start = wire_map.values().next().cloned();

        let wire = self.set_wire_point(pos, start, false);
        let other_wire = self.set_wire_point(other_pos, Some(wire.clone()), false);

        wire_map.insert(wire.id, wire);
        wire_map.insert(other_wire.id, other_wire);

        self.set_distances(pos, dir, length.get() as usize, true, true);

        if wire_map.len() > 1 {
            self.merge_many_wires(wire_map.values().cloned());
        }

        for pos in dir.iter_along(pos, length.get() as usize) {
            self.remove_needless_point(pos);
        }
    }

    pub fn toggle_wire_point(&mut self, pos: Vec2isize) {
        let Some(node) = self.wires.get(pos) else {
            return;
        };

        if node.wire.is_some() {
            for (dir, dist) in node.directions.iter() {
                if dist.is_none() {
                    continue;
                }

                let other_dist = node.directions.get(dir.inverted()).is_some();
                if !other_dist {
                    return;
                }
            }

            self.remove_wire_point(pos, true);
        } else {
            if node.directions.values().all(|d| d.is_none()) {
                return;
            }

            self.set_wire_point(pos, None, true);
        }
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

        for i in 0..=length {
            let pos = pos + dir.into_dir_isize() * i as isize;
            let node = self.wires.get_or_create_mut(pos);

            if set {
                if distance > 0 {
                    *node.directions.get_mut(dir.inverted()) = NonZeroU32::new(distance);

                    distance += 1;
                }

                if let Some(wire) = &node.wire {
                    let (dir, rev) = dir.inverted().into_half();
                    if !rev {
                        *wire
                            .points
                            .write()
                            .entry(pos)
                            .or_default()
                            .directions
                            .get_mut(dir) = distance > 0;
                    }
                }

                if node.wire.is_some() {
                    distance = 1;
                }
            } else {
                *node.directions.get_mut(dir.inverted()) = None;
            }
        }
    }

    fn set_wire_point(
        &mut self,
        pos: Vec2isize,
        new_wire: Option<Arc<Wire>>,
        merge: bool,
    ) -> Arc<Wire> {
        let node = self.wires.get(pos);

        if let Some(wire) = node.and_then(|n| n.wire.clone()) {
            return wire;
        }

        let mut merge_wires = HashMap::new();

        let directions = self.examine_directions(pos);

        let wire = match &directions {
            None => new_wire.unwrap_or_else(|| self.board.create_wire()),
            Some(array) => {
                let mut biggest_wire = None;
                let mut max_points = None;

                for wire in array.values() {
                    let Some((_, wire)) = wire else {
                        continue;
                    };
                    let points = wire.points.read().len();
                    if !max_points.is_some_and(|mp| points <= mp) {
                        max_points = Some(points);
                        biggest_wire = Some(wire.clone());
                    }

                    if merge {
                        merge_wires.insert(wire.id, wire.clone());
                    }
                }

                biggest_wire
                    .or(new_wire)
                    .unwrap_or_else(|| self.board.create_wire())
            }
        };

        let node = self.wires.get_or_create_mut(pos);
        node.wire = Some(wire.clone());

        wire.points.write().insert(pos, WirePoint::default());

        self.fix_directions_at_intersection(pos);

        if merge_wires.len() > 1 {
            self.merge_many_wires(merge_wires.values().cloned());
        }

        wire
    }

    fn remove_wire_point(&mut self, pos: Vec2isize, unmerge: bool) -> Option<Arc<Wire>> {
        let node = self.wires.get(pos);

        if let Some(node) = node {
            #[allow(clippy::question_mark)] // i'd prefer this explicit return
            if node.wire.is_none() {
                return None;
            }
        }

        let node = self.wires.get_or_create_mut(pos);

        let wire = node.wire.take();

        if let Some(wire) = &wire {
            let mut points = wire.points.write();
            points.remove(&pos);

            if points.is_empty() {
                self.board.free_wire(wire);
            }
        }

        self.fix_directions_at_intersection(pos);

        if unmerge {
            if let Some(wire) = wire.clone() {
                self.unmerge_wire(wire);
            }
        }

        wire
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

    #[allow(clippy::type_complexity)]
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
            let points = wire.points.read().len();
            if !max_points.is_some_and(|mp| points <= mp) {
                max_points = Some(points);
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
        let mut points_from = from.points.write();
        let mut points_into = into.points.write();

        for (pos, point) in points_from.drain() {
            points_into.insert(pos, point);
            self.wires.get_or_create_mut(pos).wire = Some(into.clone());
        }

        self.board.free_wire(&from);
    }

    fn unmerge_wire(&mut self, wire: Arc<Wire>) {
        let start_wire_id = wire.id;
        let mut points = wire.points.write_arc();
        let mut positions: HashSet<_> = HashSet::from_iter(points.keys().copied());
        let mut trav_positions = HashSet::new();

        points.clear();

        let mut wire = Some((wire, points));

        while let Some(pos) = positions.iter().next().copied() {
            positions.remove(&pos);

            if !self.wires.get(pos).is_some_and(|n| n.wire.is_some()) {
                continue;
            };

            let mut wire = wire.take().unwrap_or_else(|| {
                let wire = self.board.create_wire();
                (wire.clone(), wire.points.write_arc())
            });

            trav_positions.clear();
            trav_positions.insert(pos);

            while let Some(pos) = trav_positions.iter().next().copied() {
                positions.remove(&pos);
                trav_positions.remove(&pos);

                if wire.1.contains_key(&pos) {
                    continue;
                }

                let Some(node) = self.wires.get_mut(pos).filter(|n| n.wire.is_some()) else {
                    continue;
                };

                node.wire = Some(wire.0.clone());
                let directions = node.directions;

                let mut point_directions = Direction4HalfArray::default();

                for (dir, dist) in directions.iter() {
                    let Some(dist) = *dist else {
                        continue;
                    };

                    let target_pos = pos + dir.into_dir_isize() * dist.get() as isize;
                    if !self
                        .wires
                        .get(target_pos)
                        .is_some_and(|n| n.wire.as_ref().is_some_and(|w| w.id == start_wire_id))
                    {
                        continue;
                    };

                    if let Some(dir) = dir.into_half_option() {
                        *point_directions.get_mut(dir) = true;
                    }

                    if !wire.1.contains_key(&target_pos) {
                        trav_positions.insert(target_pos);
                    }
                }

                wire.1.insert(
                    pos,
                    WirePoint {
                        directions: point_directions,
                    },
                );
            }

            if wire.1.is_empty() {
                self.board.free_wire(&wire.0);
            }
        }

        if let Some(wire) = wire {
            self.board.free_wire(&wire.0);
        }
    }

    fn remove_needless_point(&mut self, pos: Vec2isize) {
        let Some(node) = self.wires.get(pos) else {
            return;
        };

        if node.wire.is_none() {
            return;
        }

        let mut dirs = 0;

        for (dir, dist) in node.directions.iter() {
            if dist.is_none() {
                continue;
            }

            let other_dist = node.directions.get(dir.inverted()).is_some();
            if other_dist {
                dirs += 1;

                // Node is a crossing
                if dirs > 2 {
                    return;
                }
            } else {
                // Node has a wire not in line with others
                return;
            }
        }

        self.remove_wire_point(pos, false);
    }
}

pub struct Wire {
    pub id: usize,

    pub points: Arc<RwLock<HashMap<Vec2isize, WirePoint>>>,
}

#[derive(Default)]
pub struct WirePoint {
    pub directions: Direction4HalfArray<bool>,
}

#[derive(Default, Clone)]
pub struct WireNode {
    pub wire: Option<Arc<Wire>>,
    pub directions: Direction8Array<Option<NonZeroU32>>,
}
