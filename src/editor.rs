use std::{
    collections::{HashMap, HashSet},
    num::NonZeroU32,
    ops::DerefMut,
    sync::Arc,
};

use eframe::egui::{remap_clamp, vec2, Rect};

use crate::{
    board::{Board, Wire, WirePoint},
    circuits::{Circuit, CircuitBlueprint, CircuitPin, PinType, TransformSupport},
    containers::Chunks2D,
    pool::get_pooled,
    selection::SelectionImpl,
    state::UpdateTaskPool,
    vector::{Vec2f, Vec2isize, Vec2usize},
    Direction4Half, Direction4HalfArray, Direction8, Direction8Array, BIG_WIRE_POINT_WIDTH,
    CHUNK_SIZE, WIRE_POINT_WIDTH, WIRE_WIDTH,
};

pub struct BoardEditor {
    wires: Chunks2D<CHUNK_SIZE, WireNode>,
    circuits: Chunks2D<CHUNK_SIZE, CircuitNode>,

    board: Arc<Board>,
}

impl BoardEditor {
    pub fn board(&self) -> &Arc<Board> {
        &self.board
    }

    pub fn wires(&self) -> &Chunks2D<CHUNK_SIZE, WireNode> {
        &self.wires
    }

    pub fn circuits(&self) -> &Chunks2D<CHUNK_SIZE, CircuitNode> {
        &self.circuits
    }
}

impl BoardEditor {
    pub(crate) fn new(board: Arc<Board>) -> Self {

        // TODO: actual implementation

        Self {
            board,
            wires: Default::default(),
            circuits: Default::default(),
        }
    }

    pub fn place_wire(&mut self, pos: Vec2isize, dir: Direction8, length: NonZeroU32) {
        let mut tasks = get_pooled::<UpdateTaskPool>();
        let mut wire_map = HashMap::new();

        for pos in dir.iter_along(pos, length.get() as usize + 1) {
            if let Some(wire) = self.wires.get(pos).and_then(|n| n.wire.clone()) {
                wire_map.insert(wire.id, wire);
            } else if self.circuits.get(pos).is_some_and(|n| {
                n.quarters
                    .values()
                    .any(|q| q.as_ref().is_some_and(|q| q.pin.is_some()))
            }) {
                let start = wire_map.values().next().cloned();
                let wire = self.set_wire_point(pos, start, false, tasks.deref_mut());
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

        let wire = self.set_wire_point(pos, start, false, tasks.deref_mut());
        let other_wire =
            self.set_wire_point(other_pos, Some(wire.clone()), false, tasks.deref_mut());

        wire_map.insert(wire.id, wire);
        wire_map.insert(other_wire.id, other_wire);

        self.set_wire_distances(pos, dir, length.get() as usize, true, true);

        for pos in dir.iter_along(pos, length.get() as usize + 1) {
            self.remove_needless_wire_point(pos, tasks.deref_mut());
        }

        if wire_map.len() > 1 {
            self.merge_many_wires(wire_map.values().cloned(), None, tasks.deref_mut());
        }
        else if let Some(wire) = wire_map.values().next() {
            tasks.add_wire_task(wire.id, true);
        }

        self.board.states().read().add_tasks(&tasks);
    }

    pub fn remove_wire(&mut self, pos: Vec2isize, dir: Direction8, length: NonZeroU32) {
        let mut tasks = get_pooled::<UpdateTaskPool>();
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
                wire.or_else(|| {
                    any_dir.then(|| self.set_wire_point(pos, None, true, tasks.deref_mut()))
                })
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
                wire.or_else(|| {
                    any_dir.then(|| self.set_wire_point(other_pos, None, true, tasks.deref_mut()))
                })
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
            self.remove_needless_wire_point(pos, tasks.deref_mut());
        }

        for wire in wire_map.values() {
            self.unmerge_wire(wire.clone(), tasks.deref_mut());
        }

        self.board.states().read().add_tasks(&tasks);
    }

    pub fn toggle_wire_point(&mut self, pos: Vec2isize) {
        let Some(node) = self.wires.get(pos) else {
            return;
        };

        let mut tasks = get_pooled::<UpdateTaskPool>();

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

            self.remove_wire_point(pos, true, false, tasks.deref_mut());
        } else {
            if node.directions.values().all(|d| d.is_none()) {
                return;
            }

            self.set_wire_point(pos, None, true, tasks.deref_mut());
        }

        self.board.states().read().add_tasks(&tasks);
    }

    pub fn remove_wire_point_with_parts(&mut self, pos: Vec2isize) {
        if self.should_pin_wire_point_exist(pos) {
            return;
        }
        let mut tasks = get_pooled::<UpdateTaskPool>();
        self.remove_wire_point(pos, true, true, tasks.deref_mut());

        self.board.states().read().add_tasks(&tasks);
    }

    pub fn place_circuit(
        &mut self,
        pos: Vec2isize,
        blueprint: &CircuitBlueprint,
    ) -> Result<(), CircuitPlaceError> {
        let size = blueprint.inner_size;
        let transformed_size = blueprint.transformed_size;
        if transformed_size.x == 0 || transformed_size.y == 0 {
            return Err(CircuitPlaceError::ZeroSizeCircuit);
        }

        let transform = blueprint.transform;
        let circuit = self.board.create_circuit(pos, blueprint);

        let imp = circuit.imp.read();
        let pins = circuit.pins.read();

        let mut intersects = false;
        let mut occupies_any = false;
        let mut any_disconnected_pins = false;
        for y in 0..transformed_size.y {
            for x in 0..transformed_size.x {
                let xy = Vec2usize::new(x, y);
                let world_pos = pos + xy.convert(|v| v as isize);

                let node = self.circuits.get_or_create_mut(world_pos);

                let mut occupied_quarters = QuarterArray::default();

                let pin = pins.iter().find(|p| p.desc.pos == xy).map(|p| &p.pin);
                let mut occupies_any_quarter = false;

                for quarter in QuarterPos::ALL {
                    let pos = quarter.into_position() + [x * 2, y * 2];
                    let qpos = transform.backtransform_pos(
                        size * 2,
                        pos,
                        Some(TransformSupport::Automatic),
                    );
                    if !imp.imp.occupies_quarter(transform, qpos) {
                        continue;
                    }
                    occupies_any = true;
                    occupies_any_quarter = true;
                    let circuit_quarter = node.quarters.get_mut(quarter);

                    if circuit_quarter.is_some() {
                        *occupied_quarters.get_mut(quarter) = true;
                    } else {
                        *circuit_quarter = Some(CircuitNodeQuarter {
                            circuit: circuit.clone(),
                            offset: [x, y].into(),
                            pin: pin.cloned(),
                        });
                    }
                }

                if !occupies_any_quarter && pin.is_some() {
                    any_disconnected_pins = true;
                }

                if occupied_quarters.values().any(|v| *v) {
                    intersects = true;
                }
            }
        }

        drop(imp);

        let mut tasks = get_pooled::<UpdateTaskPool>();

        if intersects || !occupies_any || any_disconnected_pins {
            drop(pins);
            self.remove_circuit_internal(&circuit, tasks.deref_mut());

            if any_disconnected_pins && occupies_any {
                return Err(CircuitPlaceError::DisconnectedPins);
            } else if intersects {
                return Err(CircuitPlaceError::PlaceOccupied);
            } else {
                return Err(CircuitPlaceError::OccupiesNoTiles);
            }
        }

        for pin in pins.iter() {
            let world_pos = pos + pin.desc.pos.convert(|v| v as isize);
            if self.should_pin_wire_point_exist(world_pos) {
                // set_wire_point connects wire to pins
                self.set_wire_point(world_pos, None, true, tasks.deref_mut());
            }

            if let Some(wire) = pin.pin.wire.read().as_ref().map(|w| w.id) {
                match pin.pin.ty {
                    PinType::Inside => {
                        for state in self.board.states().read().iter() {
                            state.set_pin(circuit.id, pin.pin.id, state.get_wire(wire));
                        }
                    }
                    PinType::Custom => {
                        tasks.add_wire_task(wire, true);
                    }
                    PinType::Outside => {}
                }
            }
        }

        tasks.add_circuit_task(circuit.id, None);

        self.board.states().read().add_tasks(&tasks);

        Ok(())
    }

    pub fn remove_circuit(&mut self, circuit: &Arc<Circuit>) {
        let mut tasks = get_pooled::<UpdateTaskPool>();
        self.remove_circuit_internal(circuit, tasks.deref_mut());

        self.board.states().read().add_tasks(&tasks);
    }
    fn remove_circuit_internal(&mut self, circuit: &Arc<Circuit>, tasks: &mut UpdateTaskPool) {
        let info = circuit.info.read();
        let transformed_size = info.size;
        let pos = info.pos;
        drop(info);
        for y in 0..transformed_size.y {
            for x in 0..transformed_size.x {
                let world_pos = pos + [x as isize, y as isize];

                let node = self.circuits.get_or_create_mut(world_pos);

                for quarter in QuarterPos::ALL {
                    let quarter = node.quarters.get_mut(quarter);

                    let Some(q) = quarter.take() else {
                        continue;
                    };

                    if q.circuit.id != circuit.id {
                        *quarter = Some(q);
                        continue;
                    }
                    
                    if let Some(pin) = q.pin {
                        if let Some(wire) = pin.wire.read().clone() {
                            match pin.ty {
                                PinType::Custom => todo!(),
                                PinType::Inside => {},
                                PinType::Outside => {
                                    tasks.add_wire_task(wire.id, true);
                                },
                            }

                            wire.remove_pin(pin.circuit.id, pin.id);
                        }
                    }
                }
            }
        }

        for pin in circuit.pins.read().iter() {
            let world_pos = pos + pin.desc.pos.convert(|v| v as isize);
            self.remove_needless_wire_point(world_pos, tasks);
        }

        self.board.free_circuit(circuit);
    }

    fn should_pin_wire_point_exist(&mut self, pos: Vec2isize) -> bool {
        let circuit_node = self.circuits.get(pos);

        if let Some(circuit_node) = circuit_node {
            let mut one_circuit = None;
            for quarter in QuarterPos::ALL {
                let Some(quarter) = circuit_node.quarters.get(quarter) else {
                    continue;
                };

                if quarter.pin.is_none() {
                    continue;
                }

                match one_circuit {
                    Some(id) => {
                        // There are more than 1 qnique circuit quarters with pins
                        if id != quarter.circuit.id {
                            return true;
                        }
                    }
                    None => one_circuit = Some(quarter.circuit.id),
                }
            }

            if one_circuit.is_none() {
                return false;
            }
        } else {
            return false;
        }

        let wire_node = self.wires.get(pos);
        if let Some(wire_node) = wire_node {
            if wire_node.directions.values().any(|v| v.is_some()) {
                return true;
            }
        }

        false
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
        tasks: &mut UpdateTaskPool,
    ) -> Arc<Wire> {
        let node = self.wires.get(pos);
        let mut merge_wires = HashMap::new();

        let (wire, new_wire) = if let Some(wire) = node.and_then(|n| n.wire.clone()) {
            (wire, false)
        } else {
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

            (wire, true)
        };

        if let Some(circuit_node) = self.circuits.get(pos) {
            for quarter in QuarterPos::ALL {
                let Some(quarter) = circuit_node.quarters.get(quarter) else {
                    continue;
                };

                let Some(pin) = &quarter.pin else {
                    continue;
                };

                pin.connect(wire.clone(), tasks);
            }
        }

        if new_wire {
            self.set_wire_distances_at_intersection(pos, true);
        }

        if merge_wires.len() > 1 {
            self.merge_many_wires(merge_wires.values().cloned(), Some(wire.clone()), tasks);
        }

        wire
    }

    /// No update tasks are added if `unmerge: false`
    fn remove_wire_point(
        &mut self,
        pos: Vec2isize,
        unmerge: bool,
        remove_connected_parts: bool,
        tasks: &mut UpdateTaskPool,
    ) -> Option<Arc<Wire>> {
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

            if let Some(circuit_node) = self.circuits.get(pos) {
                for quarter in QuarterPos::ALL {
                    let Some(quarter) = circuit_node.quarters.get(quarter) else {
                        continue;
                    };

                    let Some(pin) = &quarter.pin else { continue };

                    pin.disconnect(tasks);
                }
            }

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
                self.remove_needless_wire_point(target, tasks);
            }
        }

        if unmerge {
            if let Some(wire) = wire.clone() {
                self.unmerge_wire(wire, tasks);
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

    fn merge_many_wires<I>(
        &mut self,
        iter: I,
        into: Option<Arc<Wire>>,
        tasks: &mut UpdateTaskPool,
    ) where
        I: Clone + Iterator<Item = Arc<Wire>>,
    {
        let merge_into = if into.is_some() {
            into
        } else {
            let mut merge_into = None;
            let mut max_points = None;

            for wire in iter.clone() {
                let points = wire.points.read().len();
                if !max_points.is_some_and(|mp| points <= mp) {
                    max_points = Some(points);
                    merge_into = Some(wire.clone());
                }
            }
            merge_into
        };

        let Some(merge_into) = merge_into else {
            return;
        };

        for wire in iter {
            if wire.id == merge_into.id {
                continue;
            }

            self.merge_wires(wire, merge_into.clone());
        }

        tasks.add_wire_task(merge_into.id, true);
    }

    fn merge_wires(&mut self, from: Arc<Wire>, into: Arc<Wire>) {
        let mut points_from = from.points.write();
        let mut points_into = into.points.write();

        for (pos, point) in points_from.drain() {
            points_into.insert(pos, point);
            self.wires.get_or_create_mut(pos).wire = Some(into.clone());
        }

        let mut pins_from = from.connected_pins.write();
        let mut pins_into = into.connected_pins.write();

        for pin in pins_from.drain(..) {
            let mut pin_wire = pin.wire.write();
            if !pin_wire.as_ref().is_some_and(|w| w.id == from.id) {

                // Weird pin connected to a different wire?
                continue;
            }

            *pin_wire = Some(into.clone());
            drop(pin_wire);
            pins_into.push(pin);
        }

        self.board.free_wire(&from);
    }

    fn unmerge_wire(&mut self, wire: Arc<Wire>, tasks: &mut UpdateTaskPool) {
        let start_wire_id = wire.id;
        let mut points = wire.points.write_arc();
        let mut positions: HashSet<_> = HashSet::from_iter(points.keys().copied());
        let mut trav_positions = HashSet::new();
        
        let mut old_pins = std::mem::take(wire.connected_pins.write().deref_mut());
        points.clear();

        let mut first_wire = Some((wire, points));

        while let Some(pos) = positions.iter().next().copied() {
            positions.remove(&pos);

            if !self.wires.get(pos).is_some_and(|n| n.wire.is_some()) {
                continue;
            };

            let (wire, mut wire_points) = first_wire.take().unwrap_or_else(|| {
                let wire = self.board.create_wire();
                (wire.clone(), wire.points.write_arc())
            });

            trav_positions.clear();
            trav_positions.insert(pos);

            while let Some(pos) = trav_positions.iter().next().copied() {
                positions.remove(&pos);
                trav_positions.remove(&pos);

                if wire_points.contains_key(&pos) {
                    continue;
                }

                let Some(node) = self
                    .wires
                    .get_mut(pos)
                    .filter(|n| n.wire.as_ref().is_some_and(|w| w.id == start_wire_id))
                else {
                    continue;
                };

                node.wire = Some(wire.clone());
                let directions = node.directions;

                if let Some(circuit) = self.circuits.get(pos) {
                    for quarter in circuit.quarters.values() {
                        let Some(quarter) = quarter else {
                            continue;
                        };

                        let Some(pin) = &quarter.pin else {
                            continue;
                        };

                        pin.connect(wire.clone(), tasks);

                        old_pins.retain(|p| !Arc::ptr_eq(p, pin));
                    }
                }

                let mut point_directions = Direction4HalfArray::default();

                for (dir, dist) in directions.iter() {
                    let Some(dist) = *dist else {
                        continue;
                    };

                    let target_pos = pos + dir.into_dir_isize() * dist.get() as isize;

                    if let Some(dir) = dir.into_half_option() {
                        *point_directions.get_mut(dir) = true;
                    }

                    if !wire_points.contains_key(&target_pos) {
                        trav_positions.insert(target_pos);
                    }
                }

                wire_points.insert(
                    pos,
                    WirePoint {
                        directions: point_directions,
                    },
                );
            }

            if wire_points.is_empty() && wire.connected_pins.read().is_empty() {
                self.board.free_wire(&wire);
                continue;
            }

            tasks.add_wire_task(wire.id, true);
        }

        if let Some(wire) = first_wire {
            self.board.free_wire(&wire.0);
        }

        for pin in old_pins {
            pin.disconnect(tasks);
        }
    }

    fn remove_needless_wire_point(&mut self, pos: Vec2isize, tasks: &mut UpdateTaskPool) {
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

        if self.should_pin_wire_point_exist(pos) {
            return;
        }

        self.remove_wire_point(pos, false, false, tasks);
    }
}

#[derive(Default, Clone)]
pub struct WireNode {
    pub wire: Option<Arc<Wire>>,
    pub directions: Direction8Array<Option<NonZeroU32>>,
}

#[derive(Clone, Copy)]
pub enum QuarterPos {
    TL,
    TR,
    BL,
    BR,
}

impl QuarterPos {
    pub const ALL: [Self; 4] = [Self::TL, Self::TR, Self::BL, Self::BR];

    pub fn into_index(self) -> usize {
        match self {
            Self::TL => 0,
            Self::TR => 1,
            Self::BL => 2,
            Self::BR => 3,
        }
    }

    pub fn from_index(i: usize) -> Self {
        match i % 4 {
            0 => Self::TL,
            1 => Self::TR,
            2 => Self::BL,
            3 => Self::BR,
            _ => unreachable!(),
        }
    }

    pub fn into_position(self) -> Vec2usize {
        match self {
            Self::TL => [0, 0],
            Self::TR => [1, 0],
            Self::BL => [0, 1],
            Self::BR => [1, 1],
        }
        .into()
    }

    pub fn into_quarter_position_f32(self) -> Vec2f {
        match self {
            Self::TL => [0.0, 0.0],
            Self::TR => [0.5, 0.0],
            Self::BL => [0.0, 0.5],
            Self::BR => [0.5, 0.5],
        }
        .into()
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct QuarterArray<T>([T; 4]);

impl<T> QuarterArray<T> {
    pub fn get(&self, quarter: QuarterPos) -> &T {
        &self.0[quarter.into_index()]
    }

    pub fn get_mut(&mut self, quarter: QuarterPos) -> &mut T {
        &mut self.0[quarter.into_index()]
    }

    pub fn from_fn(mut f: impl FnMut(QuarterPos) -> T) -> Self {
        Self(std::array::from_fn(|i| f(QuarterPos::from_index(i))))
    }

    pub fn iter(&self) -> impl Iterator<Item = (QuarterPos, &T)> {
        self.0
            .iter()
            .enumerate()
            .map(|(i, v)| (QuarterPos::from_index(i), v))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (QuarterPos, &mut T)> {
        self.0
            .iter_mut()
            .enumerate()
            .map(|(i, v)| (QuarterPos::from_index(i), v))
    }

    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.0.iter()
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.0.iter_mut()
    }
}

#[derive(Default, Clone)]
pub struct CircuitNode {
    pub quarters: QuarterArray<Option<CircuitNodeQuarter>>,
}

impl CircuitNode {
    pub fn is_empty(&self) -> bool {
        for quarter in self.quarters.values() {
            if quarter.is_some() {
                return false;
            }
        }

        true
    }
}

#[derive(Clone)]
pub struct CircuitNodeQuarter {
    pub circuit: Arc<Circuit>,
    pub offset: Vec2usize,
    pub pin: Option<Arc<CircuitPin>>,
}

#[derive(Debug, thiserror::Error)]
pub enum CircuitPlaceError {
    #[error("Circuit size is 0")]
    ZeroSizeCircuit,

    #[error("Circuit occupies no space")]
    OccupiesNoTiles,

    #[error("Circuit overlaps with existing circuits")]
    PlaceOccupied,

    #[error("Some circuit pins are disconnected from the circuit")]
    DisconnectedPins,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum SelectedBoardItem {
    WirePart { pos: Vec2isize, dir: Direction4Half },
    WirePoint { pos: Vec2isize },
    Circuit { id: usize, pos: Vec2isize },
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

        for (pos, node) in pass.circuits.iter_area(tl, size) {
            for qpos in QuarterPos::ALL {
                let Some(quarter) = node.quarters.get(qpos) else {
                    continue;
                };

                let world_pos = pos.convert(|v| v as f32) + qpos.into_quarter_position_f32();
                let rect = Rect::from_min_size(world_pos.into(), vec2(0.5, 0.5));
                if area.intersects(rect) {
                    items.insert(SelectedBoardItem::Circuit {
                        id: quarter.circuit.id,
                        pos: quarter.circuit.info.read().pos,
                    });
                }
            }
        }
    }
}
