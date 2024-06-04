use std::{
    collections::{HashMap, HashSet},
    num::NonZeroU32,
    sync::Arc,
};

use eframe::egui::{remap_clamp, vec2, Rect};

use crate::{
    circuits::Circuit,
    containers::Chunks2D,
    selection::SelectionImpl,
    vector::{Vec2f, Vec2isize, Vec2usize},
    Direction4Half, Direction4HalfArray, Direction8, Direction8Array, BIG_WIRE_POINT_WIDTH,
    CHUNK_SIZE, WIRE_POINT_WIDTH, WIRE_WIDTH,
};

use super::{Board, Wire, WirePoint};

#[derive(Default)]
pub struct BoardEditor {
    pub wires: Chunks2D<CHUNK_SIZE, WireNode>,
    pub circuits: Chunks2D<CHUNK_SIZE, CircuitNode>,

    pub board: Arc<Board>,
}

impl BoardEditor {
    pub fn place_wire(&mut self, pos: Vec2isize, dir: Direction8, length: NonZeroU32) {
        let mut wire_map = HashMap::new();

        for pos in dir.iter_along(pos, length.get() as usize + 1) {
            if let Some(wire) = self.wires.get(pos).and_then(|n| n.wire.clone()) {
                wire_map.insert(wire.id, wire);
            }
        }

        let other_pos = pos + dir.into_dir_isize() * length.get() as isize;

        for pos in [pos, other_pos] {
            let Some(dirs) = self.examine_wire_directions(pos) else {
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

        self.set_wire_distances(pos, dir, length.get() as usize, true, true);

        for pos in dir.iter_along(pos, length.get() as usize + 1) {
            self.remove_needless_wire_point(pos);
        }

        if wire_map.len() > 1 {
            self.merge_many_wires(wire_map.values().cloned());
        }
    }

    pub fn remove_wire(&mut self, pos: Vec2isize, dir: Direction8, length: NonZeroU32) {
        let other_pos = pos + dir.into_dir_isize() * length.get() as isize;

        // Place wire points if they don't exist and nodes have connections
        let start_wire = self
            .wires
            .get(pos)
            .map(|n| {
                (
                    n.wire.clone(),
                    n.wire.is_none() && n.directions.values().any(|d| d.is_some()),
                )
            })
            .and_then(|(wire, any_dir)| {
                wire.or_else(|| any_dir.then(|| self.set_wire_point(pos, None, true)))
            });

        let end_wire = self
            .wires
            .get(other_pos)
            .map(|n| {
                (
                    n.wire.clone(),
                    n.wire.is_none() && n.directions.values().any(|d| d.is_some()),
                )
            })
            .and_then(|(wire, any_dir)| {
                wire.or_else(|| any_dir.then(|| self.set_wire_point(other_pos, None, true)))
            });

        let mut wire_map = HashMap::new();
        let mut iter = dir.iter_along(pos, length.get() as usize - 1);
        iter.next();

        for pos in iter {
            if let Some(wire) = self.wires.get(pos).and_then(|n| n.wire.clone()) {
                wire_map.insert(wire.id, wire);
            }
        }

        self.set_wire_distances(pos, dir, length.get() as usize, false, true);

        if let Some(start) = start_wire {
            wire_map.insert(start.id, start);
        }
        if let Some(end) = end_wire {
            wire_map.insert(end.id, end);
        }

        for pos in dir.iter_along(pos, length.get() as usize + 1) {
            self.remove_needless_wire_point(pos);
        }

        for wire in wire_map.values() {
            self.unmerge_wire(wire.clone());
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

            self.remove_wire_point(pos, true, false);
        } else {
            if node.directions.values().all(|d| d.is_none()) {
                return;
            }

            self.set_wire_point(pos, None, true);
        }
    }

    pub fn remove_wire_point_with_parts(&mut self, pos: Vec2isize) {
        self.remove_wire_point(pos, true, true);
    }

    fn set_wire_distances(
        &mut self,
        pos: Vec2isize,
        dir: Direction8,
        length: usize,
        set: bool,
        bidirectional: bool,
    ) {
        if bidirectional {
            self.set_wire_distances(pos, dir, length, set, false);
            self.set_wire_distances(
                pos + dir.into_dir_isize() * length as isize,
                dir.inverted(),
                length,
                set,
                false,
            );
            return;
        }

        let mut distance = 0;

        let back_dir = dir.inverted();

        for i in 0..=length {
            let pos = pos + dir.into_dir_isize() * i as isize;
            let node = self.wires.get_or_create_mut(pos);

            if set {
                if distance > 0 {
                    *node.directions.get_mut(back_dir) = NonZeroU32::new(distance);

                    distance += 1;
                }

                if node.wire.is_some() {
                    distance = 1;
                }
            } else if i > 0 {
                *node.directions.get_mut(back_dir) = None;
            } else {
                continue;
            }

            if let Some(wire) = &node.wire {
                if let Some(dir) = back_dir.into_half_option() {
                    *wire
                        .points
                        .write()
                        .entry(pos)
                        .or_default()
                        .directions
                        .get_mut(dir) = node.directions.get(back_dir).is_some();
                }
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

        let directions = self.examine_wire_directions(pos);

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

        self.set_wire_distances_at_intersection(pos, true);

        if merge_wires.len() > 1 {
            self.merge_many_wires(merge_wires.values().cloned());
        }

        wire
    }

    fn remove_wire_point(&mut self, pos: Vec2isize, unmerge: bool, remove_connected_parts: bool) -> Option<Arc<Wire>> {
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

        let directions = node.directions;

        self.set_wire_distances_at_intersection(pos, !remove_connected_parts);

        if remove_connected_parts {
            for (dir, dist) in directions.iter() {
                let Some(dist) = dist else {
                    continue;
                };

                let target = pos + dir.into_dir_isize() * dist.get() as isize;
                self.remove_needless_wire_point(target);
            }
        }

        if unmerge {
            if let Some(wire) = wire.clone() {
                self.unmerge_wire(wire);
            }
        }

        wire
    }

    fn set_wire_distances_at_intersection(&mut self, pos: Vec2isize, set: bool) {
        let Some(node) = self.wires.get(pos) else {
            return;
        };
        let directions = node.directions;

        for dir in Direction8::ALL {
            let forward_len = directions.get(dir).as_ref().map(|d| d.get()).unwrap_or(0);
            let backward_len = directions
                .get(dir.inverted())
                .as_ref()
                .map(|d| d.get())
                .unwrap_or(0);

            let total_len = forward_len + backward_len;
            if total_len == 0 {
                continue;
            }

            let start = pos + dir.inverted().into_dir_isize() * backward_len as isize;
            self.set_wire_distances(start, dir, total_len as usize, set, false);
        }
    }

    #[allow(clippy::type_complexity)]
    fn examine_wire_directions(
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

                let Some(node) = self
                    .wires
                    .get_mut(pos)
                    .filter(|n| n.wire.as_ref().is_some_and(|w| w.id == start_wire_id))
                else {
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

    fn remove_needless_wire_point(&mut self, pos: Vec2isize) {
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

        self.remove_wire_point(pos, false, false);
    }
}

#[derive(Default, Clone)]
pub struct WireNode {
    pub wire: Option<Arc<Wire>>,
    pub directions: Direction8Array<Option<NonZeroU32>>,
}

#[derive(Default, Clone)]
pub struct CircuitNode {
    pub circuit: Option<Arc<Circuit>>,
    pub offset: Vec2usize,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum SelectedBoardItem {
    WirePart { pos: Vec2isize, dir: Direction4Half },
    WirePoint { pos: Vec2isize },
}

pub struct BoardSelectionImpl;

impl SelectionImpl for BoardSelectionImpl {
    type Item = SelectedBoardItem;
    type Pass = BoardEditor;

    fn include_area(pass: &Self::Pass, items: &mut HashSet<Self::Item>, area: eframe::egui::Rect) {
        let tl = Vec2f::from(area.left_top()).convert(|v| v.floor() as isize);
        let br = Vec2f::from(area.right_bottom()).convert(|v| v.floor() as isize);

        let size = (br - tl).convert(|v| v as usize) + 1;

        for (pos, lookaround, node) in pass.wires.iter_area_with_lookaround(tl, size) {
            let center_pos = pos.convert(|v| v as f32 + 0.5);
            let center_rect =
                Rect::from_center_size(center_pos.into(), vec2(WIRE_WIDTH, WIRE_WIDTH));

            let center_intersects = area.intersects(center_rect);

            for (dir, dist) in node.directions.iter() {
                let Some(dist) = dist else {
                    continue;
                };

                let cell_edge = center_pos + dir.into_dir_f32() * 0.5;
                let rect = Rect::from_two_pos(center_pos.into(), cell_edge.into())
                    .expand(WIRE_WIDTH / 2.0);
                if area.intersects(rect) || center_intersects {
                    let (half_dir, rev) = dir.into_half();
                    let target_rel = if !rev {
                        let di = dir.inverted();
                        let dist = node
                            .directions
                            .get(di)
                            .map(|d| d.get() as isize)
                            .unwrap_or(0);
                        di.into_dir_isize() * dist
                    } else {
                        dir.into_dir_isize() * dist.get() as isize
                    };

                    if lookaround
                        .get_relative(target_rel)
                        .is_some_and(|n| n.wire.is_some())
                    {
                        items.insert(SelectedBoardItem::WirePart {
                            pos: pos + target_rel,
                            dir: half_dir,
                        });
                    }
                }
            }

            if node.wire.is_some() {
                if center_intersects {
                    items.insert(SelectedBoardItem::WirePoint { pos });
                } else {
                    let dirs = node.directions.values().filter(|d| d.is_some()).count();
                    let point_size = remap_clamp(
                        dirs as f32,
                        4.0..=8.0,
                        WIRE_POINT_WIDTH..=BIG_WIRE_POINT_WIDTH,
                    );

                    let rect =
                        Rect::from_center_size(center_pos.into(), vec2(point_size, point_size));

                    if rect.intersects(area) {
                        items.insert(SelectedBoardItem::WirePoint { pos });
                    }
                }
            }
        }
    }
}
