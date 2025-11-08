use std::{
    collections::{HashMap, HashSet},
    num::NonZeroU32,
    ops::{Deref, DerefMut},
    sync::Arc,
};

use eframe::egui::{Rect, remap_clamp, vec2};

use crate::{
    BIG_WIRE_POINT_WIDTH, CHUNK_SIZE, Direction4Half, Direction4HalfArray, Direction8,
    Direction8Array, WIRE_POINT_WIDTH, WIRE_WIDTH,
    board::{Board, CircuitCreationOverrides, Wire, WirePoint},
    circuits::{
        Circuit, CircuitBlueprint, CircuitImplBox, CircuitPin, CircuitTransform, PinType,
        RealizedPin, TransformSupport,
    },
    containers::Chunks2D,
    pool::get_pooled,
    selection::SelectionImpl,
    state::sim::UpdateTaskPool,
    vector::{Vec2f, Vec2isize, Vec2usize},
};

#[derive(Default)]
pub struct BoardEditorTiles {
    wires: Chunks2D<CHUNK_SIZE, WireNode>,
    circuits: Chunks2D<CHUNK_SIZE, CircuitNode>,
}

pub struct BoardEditor {
    pub tiles: BoardEditorTiles,
    board: Arc<Board>,
}

pub struct PlaceCircuitResult {
    /// Whether the placed circuit has overlapping quarters with any other circuit
    any_overlapping_quarters: bool,

    placed_any_quarters: bool,

    /// Whether there were any pins that weren't attached to any quarters of this circuit
    any_disconnected_pins: bool,
}

impl PlaceCircuitResult {
    pub fn get_placement_error(&self) -> Option<CircuitPlaceError> {
        if self.any_overlapping_quarters || !self.placed_any_quarters || self.any_disconnected_pins
        {
            let err = if self.any_disconnected_pins && self.placed_any_quarters {
                CircuitPlaceError::DisconnectedPins
            } else if self.any_overlapping_quarters {
                CircuitPlaceError::PlaceOccupied
            } else {
                CircuitPlaceError::OccupiesNoTiles
            };
            Some(err)
        } else {
            None
        }
    }
}

impl BoardEditorTiles {
    pub fn wires(&self) -> &Chunks2D<CHUNK_SIZE, WireNode> {
        &self.wires
    }

    pub fn circuits(&self) -> &Chunks2D<CHUNK_SIZE, CircuitNode> {
        &self.circuits
    }
}

impl BoardEditor {
    pub(crate) fn new(board: Arc<Board>) -> Self {
        let mut tiles = BoardEditorTiles::default();

        let board_wires = board.wires().read();
        let board_circuits = board.circuits().read();

        for wire in board_wires.iter() {
            let points = wire.points.read();

            for (pos, point) in points.iter() {
                let pos = *pos;

                tiles.set_wire(pos, Some(wire.clone()));

                let abs_coord_diff_pos = pos.x.abs_diff(pos.y);
                let abs_coord_diff_neg = (-pos.x).abs_diff(pos.y);

                for dir in point.directions.iter() {
                    if !dir.1 {
                        continue;
                    }
                    let dir = dir.0;

                    let mut closest_target_dist = None::<usize>;

                    for (target_pos, _) in points.iter() {
                        let target_pos = *target_pos;
                        let dist = match dir {
                            Direction4Half::Left => {
                                if target_pos.y != pos.y || target_pos.x >= pos.x {
                                    continue;
                                }
                                (pos.x - target_pos.x) as usize
                            }
                            Direction4Half::Up => {
                                if target_pos.x != pos.x || target_pos.y >= pos.y {
                                    continue;
                                }
                                (pos.y - target_pos.y) as usize
                            }
                            Direction4Half::UpLeft => {
                                if target_pos.x >= pos.x || target_pos.y >= pos.y {
                                    continue;
                                }

                                let target_abs_coord_diff = target_pos.x.abs_diff(target_pos.y);
                                if target_abs_coord_diff != abs_coord_diff_pos {
                                    // Not on the same diagonal
                                    continue;
                                }

                                (pos.x - target_pos.x) as usize
                            }
                            Direction4Half::UpRight => {
                                if target_pos.x <= pos.x || target_pos.y >= pos.y {
                                    continue;
                                }

                                let target_abs_coord_diff = (-target_pos.x).abs_diff(target_pos.y);
                                if target_abs_coord_diff != abs_coord_diff_neg {
                                    // Not on the same diagonal
                                    continue;
                                }

                                (target_pos.x - pos.x) as usize
                            }
                        };

                        closest_target_dist = Some(match closest_target_dist {
                            None => dist,
                            Some(old_dist) => dist.min(old_dist),
                        });
                    }

                    let Some(target_dist) = closest_target_dist else {
                        // todo: an error or a warning
                        continue;
                    };

                    let Ok(target_dist) = u32::try_from(target_dist) else {
                        // todo: an error
                        continue;
                    };

                    tiles.set_wire_distances(
                        pos,
                        Direction8::from(dir),
                        target_dist,
                        true,
                        true,
                        false,
                    );
                }
            }
        }

        for circuit in board_circuits.iter() {
            let imp = circuit.imp.read();
            let info = circuit.info.read();
            let pins = circuit.pins.read();

            tiles.place_circuit(
                circuit,
                info.pos,
                info.size,
                info.transform,
                &imp.imp,
                &pins,
                true,
            );
        }

        drop((board_wires, board_circuits));

        Self { tiles, board }
    }

    pub fn board(&self) -> &Arc<Board> {
        &self.board
    }
}

impl BoardEditorTiles {
    /// Sets/removes distance pointers between wire points
    /// Assumes wire points set at both ends
    pub fn set_wire_distances(
        &mut self,
        pos: Vec2isize,
        dir: Direction8,
        length: u32,
        set: bool,
        bidirectional: bool,
        update_wire_points: bool,
    ) {
        if bidirectional {
            self.set_wire_distances(pos, dir, length, set, false, update_wire_points);
            self.set_wire_distances(
                pos + dir.into_dir_isize() * length as isize,
                dir.inverted(),
                length,
                set,
                false,
                update_wire_points,
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

            if update_wire_points
                && let Some(wire) = &node.wire
                && let Some(dir) = back_dir.into_half_option()
            {
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

    /// Get distances to closest wire points
    #[allow(clippy::type_complexity)]
    pub fn examine_wire_directions(
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

    /// Set wire point at the given position, returns the old one
    pub fn set_wire(&mut self, pos: Vec2isize, wire: Option<Arc<Wire>>) -> Option<Arc<Wire>> {
        std::mem::replace(&mut self.wires.get_or_create_mut(pos).wire, wire)
    }

    pub fn should_pin_wire_point_exist(&self, pos: Vec2isize) -> bool {
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
        if let Some(wire_node) = wire_node
            && wire_node.directions.values().any(|v| v.is_some())
        {
            return true;
        }

        false
    }

    #[allow(clippy::too_many_arguments)]
    pub fn place_circuit(
        &mut self,
        circuit: &Arc<Circuit>,
        pos: Vec2isize,
        size: Vec2usize,
        transform: CircuitTransform,
        imp: &CircuitImplBox,
        pins: &[RealizedPin],
        overwrite: bool,
    ) -> PlaceCircuitResult {
        let mut overlap = false;
        let mut any_quarters = false;
        let mut disconnected_pins = false;

        let orig_size = transform.transform_size(size, Some(TransformSupport::Automatic));

        for y in 0..size.y {
            for x in 0..size.x {
                let offset = Vec2usize::new(x, y);
                let cell = self
                    .circuits
                    .get_or_create_mut(pos + offset.convert(|v| v as isize));

                let pin = pins
                    .iter()
                    .find_map(|p| p.desc.pos.eq(&offset).then_some(&p.pin));

                let mut this_tile_any_quarters = false;

                for q in QuarterPos::ALL {
                    let qpos = transform.backtransform_pos(
                        orig_size * 2,
                        q.into_position() + offset * 2,
                        Some(TransformSupport::Automatic),
                    );

                    if !imp.occupies_quarter(transform, qpos) {
                        continue;
                    }

                    any_quarters = true;
                    this_tile_any_quarters = true;

                    let quarter = cell.quarters.get_mut(q);
                    if quarter.is_some() {
                        overlap = true;
                    }

                    if quarter.is_none() || overwrite {
                        *quarter = Some(CircuitNodeQuarter {
                            circuit: circuit.clone(),
                            offset,
                            pin: pin.cloned(),
                        });
                    }
                }

                if !this_tile_any_quarters && pin.is_some() {
                    disconnected_pins = true;
                }
            }
        }

        PlaceCircuitResult {
            any_overlapping_quarters: overlap,
            placed_any_quarters: any_quarters,
            any_disconnected_pins: disconnected_pins,
        }
    }

    pub fn remove_circuit(&mut self, id: usize, pos: Vec2isize, size: Vec2usize) {
        for y in 0..size.y {
            for x in 0..size.x {
                let world_pos = pos + [x as isize, y as isize];

                let node = self.circuits.get_mut(world_pos);
                let Some(node) = node else {
                    continue;
                };

                for quarter in QuarterPos::ALL {
                    let quarter = node.quarters.get_mut(quarter);

                    quarter.take_if(|q| q.circuit.id == id);
                }
            }
        }
    }

    /// Update/remove distances affected by this position
    pub fn set_wire_distances_at_intersection(
        &mut self,
        pos: Vec2isize,
        set: bool,
        update_wire_points: bool,
    ) {
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
            self.set_wire_distances(start, dir, total_len, set, false, update_wire_points);
        }
    }

    pub fn validate_circuit_geometry(
        &self,
        id: usize,
        pos: Vec2isize,
        size: Vec2usize,
        transform: CircuitTransform,
        imp: &CircuitImplBox,
    ) -> bool {
        let orig_size = transform.transform_size(size, Some(TransformSupport::Automatic));

        for y in 0..size.y {
            for x in 0..size.x {
                let offset = Vec2usize::new(x, y);
                let cell = self.circuits.get(pos + offset.convert(|v| v as isize));

                for q in QuarterPos::ALL {
                    let qpos = transform.backtransform_pos(
                        orig_size * 2,
                        q.into_position() + offset * 2,
                        Some(TransformSupport::Automatic),
                    );

                    let should_occupy = !imp.occupies_quarter(transform, qpos);
                    let actually_occupies = cell
                        .and_then(|c| c.quarters.get(q).as_ref())
                        .is_some_and(|q| q.circuit.id == id);

                    if should_occupy != actually_occupies {
                        return false;
                    }
                }
            }
        }

        true
    }

    pub fn replace_pins(
        &mut self,
        id: usize,
        pos: Vec2isize,
        size: Vec2usize,
        pins: &[RealizedPin],
    ) -> Result<(), DisconnectedPinsError> {
        for y in 0..size.y {
            for x in 0..size.x {
                let world_pos = pos + [x as isize, y as isize];

                let node = self.circuits.get_mut(world_pos);
                let Some(node) = node else {
                    continue;
                };

                for quarter in QuarterPos::ALL {
                    let quarter = node.quarters.get_mut(quarter);

                    let Some(quarter) = quarter else {
                        continue;
                    };

                    if quarter.circuit.id != id {
                        continue;
                    }

                    quarter.pin = None;
                }
            }
        }

        let mut disconnected_pins = false;

        for pin in pins {
            if pin.desc.pos.x >= size.x || pin.desc.pos.y >= size.y {
                continue;
            }

            let world_pos = pos + pin.desc.pos.convert(|v| v as isize);
            let Some(node) = self.circuits.get_mut(world_pos) else {
                disconnected_pins = true;
                continue;
            };

            let mut placed_any = false;
            for q in QuarterPos::ALL {
                let quarter = node.quarters.get_mut(q);
                let Some(quarter) = quarter else {
                    continue;
                };

                if quarter.circuit.id != id {
                    continue;
                }

                quarter.pin = Some(pin.pin.clone());
                placed_any = true;
            }

            if !placed_any {
                disconnected_pins = true;
            }
        }

        if disconnected_pins {
            Err(DisconnectedPinsError)
        } else {
            Ok(())
        }
    }
}

impl BoardEditor {
    pub fn place_wire(&mut self, pos: Vec2isize, dir: Direction8, length: NonZeroU32) {
        let mut tasks = get_pooled::<UpdateTaskPool>();
        self.place_wire_manual(pos, dir, length, tasks.deref_mut());
        self.board.add_tasks(&tasks);
    }

    pub fn place_wire_manual(
        &mut self,
        pos: Vec2isize,
        dir: Direction8,
        length: NonZeroU32,
        tasks: &mut UpdateTaskPool,
    ) {
        let mut wire_map = HashMap::new();

        for pos in dir.iter_along(pos, length.get() as usize + 1) {
            if let Some(wire) = self.tiles.wires().get(pos).and_then(|n| n.wire.clone()) {
                wire_map.insert(wire.id, wire);
            } else if self.tiles.circuits().get(pos).is_some_and(|n| {
                n.quarters
                    .values()
                    .any(|q| q.as_ref().is_some_and(|q| q.pin.is_some()))
            }) {
                let start = wire_map.values().next().cloned();
                let wire = self.set_wire_point(pos, start, false, tasks);
                wire_map.insert(wire.id, wire);
            }
        }

        let other_pos = pos + dir.into_dir_isize() * length.get() as isize;

        for pos in [pos, other_pos] {
            let Some(dirs) = self.tiles.examine_wire_directions(pos) else {
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

        let wire = self.set_wire_point(pos, start, false, tasks);
        let other_wire = self.set_wire_point(other_pos, Some(wire.clone()), false, tasks);

        wire_map.insert(wire.id, wire);
        wire_map.insert(other_wire.id, other_wire);

        self.tiles
            .set_wire_distances(pos, dir, length.get(), true, true, true);

        for pos in dir.iter_along(pos, length.get() as usize + 1) {
            self.remove_needless_wire_point(pos, tasks);
        }

        if wire_map.len() > 1 {
            self.merge_many_wires(wire_map.values().cloned(), None, tasks);
        } else if let Some(wire) = wire_map.values().next() {
            tasks.add_wire_task(wire.id, true);
        }
    }

    pub fn remove_wire(&mut self, pos: Vec2isize, dir: Direction8, length: NonZeroU32) {
        let mut tasks = get_pooled::<UpdateTaskPool>();
        let other_pos = pos + dir.into_dir_isize() * length.get() as isize;

        // Place wire points if they don't exist and nodes have connections
        let start_wire = self
            .tiles
            .wires()
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
            .tiles
            .wires()
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
            if let Some(wire) = self.tiles.wires().get(pos).and_then(|n| n.wire.clone()) {
                wire_map.insert(wire.id, wire);
            }
        }

        self.tiles
            .set_wire_distances(pos, dir, length.get(), false, true, true);

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

        self.board.add_tasks(&tasks);
    }

    pub fn toggle_wire_point(&mut self, pos: Vec2isize) {
        let mut tasks = get_pooled::<UpdateTaskPool>();
        self.toggle_wire_point_manual(pos, tasks.deref_mut());
        self.board.add_tasks(&tasks);
    }

    pub fn toggle_wire_point_manual(&mut self, pos: Vec2isize, tasks: &mut UpdateTaskPool) {
        let Some(node) = self.tiles.wires().get(pos) else {
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

            self.remove_wire_point(pos, true, false, tasks);
        } else {
            if node.directions.values().all(|d| d.is_none()) {
                return;
            }

            self.set_wire_point(pos, None, true, tasks);
        }
    }

    pub fn remove_wire_point_with_parts(&mut self, pos: Vec2isize) {
        if self.tiles.should_pin_wire_point_exist(pos) {
            return;
        }
        let mut tasks = get_pooled::<UpdateTaskPool>();
        self.remove_wire_point(pos, true, true, tasks.deref_mut());

        self.board.add_tasks(&tasks);
    }

    pub fn place_circuit(
        &mut self,
        pos: Vec2isize,
        blueprint: &CircuitBlueprint,
    ) -> Result<Arc<Circuit>, CircuitPlaceError> {
        let mut tasks = get_pooled::<UpdateTaskPool>();

        let res = self.place_circuit_manual(
            pos,
            blueprint,
            CircuitCreationOverrides::NONE,
            tasks.deref_mut(),
        )?;

        self.board.add_tasks(&tasks);

        Ok(res)
    }

    pub fn place_circuit_manual(
        &mut self,
        pos: Vec2isize,
        blueprint: &CircuitBlueprint,
        overrides: CircuitCreationOverrides,
        tasks: &mut UpdateTaskPool,
    ) -> Result<Arc<Circuit>, CircuitPlaceError> {
        let transformed_size = blueprint.transformed_size;
        if transformed_size.x == 0 || transformed_size.y == 0 {
            return Err(CircuitPlaceError::ZeroSizeCircuit);
        }

        let transform = blueprint.transform;
        let circuit = self.board.create_circuit(pos, blueprint, overrides);

        let imp = circuit.imp.read();
        let pins = circuit.pins.read();

        let res = self.tiles.place_circuit(
            &circuit,
            pos,
            transformed_size,
            transform,
            &imp.imp,
            &pins,
            false,
        );

        drop(imp);

        let err = res.get_placement_error();

        if let Some(err) = err {
            drop(pins);
            self.remove_circuit_internal(&circuit, tasks);
            return Err(err);
        }

        for pin in pins.iter() {
            let world_pos = pos + pin.desc.pos.convert(|v| v as isize);
            if self.tiles.should_pin_wire_point_exist(world_pos) {
                // set_wire_point connects wire to pins
                self.set_wire_point(world_pos, None, true, tasks);
            }

            if pin.pin.wire.read().is_some() {
                match pin.pin.ty {
                    PinType::Inside => {
                        tasks.add_update_input_task(circuit.id, pin.pin.id, false);
                    }
                    PinType::Outside => {}
                }
            }
        }

        drop(pins);

        tasks.add_circuit_task(circuit.id, None);

        Ok(circuit)
    }

    pub fn remove_circuit(&mut self, circuit: &Arc<Circuit>) {
        let mut tasks = get_pooled::<UpdateTaskPool>();
        self.remove_circuit_internal(circuit, tasks.deref_mut());

        self.board.add_tasks(&tasks);
    }

    fn remove_circuit_internal(&mut self, circuit: &Arc<Circuit>, tasks: &mut UpdateTaskPool) {
        let info = circuit.info.read();
        let transformed_size = info.size;
        let pos = info.pos;
        drop(info);

        self.tiles.remove_circuit(circuit.id, pos, transformed_size);

        for pin in circuit.pins.read().iter() {
            let mut wire = pin.pin.wire.write();
            if let Some(wire) = wire.deref() {
                match pin.pin.ty {
                    PinType::Inside => {}
                    PinType::Outside => {
                        tasks.add_wire_task(wire.id, true);
                    }
                }

                wire.remove_pin(circuit.id, pin.pin.id);
            }
            *wire = None;
            drop(wire);

            let world_pos = pos + pin.desc.pos.convert(|v| v as isize);
            self.remove_needless_wire_point(world_pos, tasks);
        }

        tasks.add_drop_circuit_task(circuit.id, None);

        self.board.free_circuit(circuit);
    }

    pub fn set_wire_point(
        &mut self,
        pos: Vec2isize,
        new_wire: Option<Arc<Wire>>,
        merge: bool,
        tasks: &mut UpdateTaskPool,
    ) -> Arc<Wire> {
        let node = self.tiles.wires.get(pos);
        let mut merge_wires = HashMap::new();

        let (wire, new_wire) = if let Some(wire) = node.and_then(|n| n.wire.clone()) {
            (wire, false)
        } else {
            let directions = self.tiles.examine_wire_directions(pos);

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
                        if max_points.is_none_or(|mp| points > mp) {
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

            self.tiles.set_wire(pos, Some(wire.clone()));

            wire.points.write().insert(pos, WirePoint::default());

            (wire, true)
        };

        if let Some(circuit_node) = self.tiles.circuits().get(pos) {
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
            self.tiles
                .set_wire_distances_at_intersection(pos, true, true);
        }

        if merge_wires.len() > 1 {
            self.merge_many_wires(merge_wires.values().cloned(), Some(wire.clone()), tasks);
        }

        wire
    }

    /// No update tasks are added if `unmerge: false`
    pub fn remove_wire_point(
        &mut self,
        pos: Vec2isize,
        unmerge: bool,
        remove_connected_parts: bool,
        tasks: &mut UpdateTaskPool,
    ) -> Option<Arc<Wire>> {
        let node = self.tiles.wires().get(pos);

        if let Some(node) = node {
            #[allow(clippy::question_mark)] // i'd prefer this explicit return
            if node.wire.is_none() {
                return None;
            }
        }

        let wire = self.tiles.set_wire(pos, None);

        if let Some(wire) = &wire {
            let mut points = wire.points.write();
            points.remove(&pos);

            if let Some(circuit_node) = self.tiles.circuits().get(pos) {
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

        self.tiles
            .set_wire_distances_at_intersection(pos, !remove_connected_parts, true);

        if remove_connected_parts {
            let node = self.tiles.wires().get(pos);
            if let Some(node) = node {
                let directions = node.directions;
                for (dir, dist) in directions.iter() {
                    let Some(dist) = dist else {
                        continue;
                    };

                    let target = pos + dir.into_dir_isize() * dist.get() as isize;
                    self.remove_needless_wire_point(target, tasks);
                }
            }
        }

        if unmerge && let Some(wire) = wire.clone() {
            self.unmerge_wire(wire, tasks);
        }

        wire
    }

    fn merge_many_wires<I>(&mut self, iter: I, into: Option<Arc<Wire>>, tasks: &mut UpdateTaskPool)
    where
        I: Clone + Iterator<Item = Arc<Wire>>,
    {
        let merge_into = if into.is_some() {
            into
        } else {
            let mut merge_into = None;
            let mut max_points = None;

            for wire in iter.clone() {
                let points = wire.points.read().len();
                if max_points.is_none_or(|mp| points > mp) {
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
            self.tiles.set_wire(pos, Some(into.clone()));
        }

        let mut pins_from = from.connected_pins.write();
        let mut pins_into = into.connected_pins.write();

        for pin in pins_from.drain(..) {
            let mut pin_wire = pin.wire.write();
            if pin_wire.as_ref().is_none_or(|w| w.id != from.id) {
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

            if self.tiles.wires().get(pos).is_none_or(|n| n.wire.is_none()) {
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

                let node = self.tiles.wires().get(pos);
                let Some(node) =
                    node.filter(|n| n.wire.as_ref().is_some_and(|w| w.id == start_wire_id))
                else {
                    continue;
                };

                let directions = node.directions;
                self.tiles.set_wire(pos, Some(wire.clone()));

                if let Some(circuit) = self.tiles.circuits().get(pos) {
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

    pub fn remove_needless_wire_point(&mut self, pos: Vec2isize, tasks: &mut UpdateTaskPool) {
        let Some(node) = self.tiles.wires().get(pos) else {
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

        if self.tiles.should_pin_wire_point_exist(pos) {
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

pub struct DisconnectedPinsError;

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

        for (pos, lookaround, node) in pass.tiles.wires().iter_area_with_lookaround(tl, size) {
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

        for (pos, node) in pass.tiles.circuits().iter_area(tl, size) {
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
