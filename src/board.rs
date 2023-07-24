use std::{
    collections::{hash_map::DefaultHasher, HashMap, HashSet},
    f32::consts::TAU,
    hash::Hasher,
    num::NonZeroU32,
    sync::Arc,
};

use eframe::{
    egui::{self, Sense, TextStyle},
    epaint::{Color32, Rounding, Stroke},
};
use emath::{vec2, Align2, Rect};

use crate::{
    circuits::{Circuit, CircuitNode, CircuitPin, CircuitPreview, CircuitStateContext},
    containers::{Chunks2D, ChunksLookaround, FixedVec},
    state::{State, StateCollection},
    vector::{IsZero, Vec2f, Vec2i, Vec2u},
    wires::{FoundWireNode, TileWires, Wire, WireNode, WirePart, WirePoint},
    Direction2, Direction4, PaintContext, RwLock, Screen,
};

pub struct CircuitBoard {
    pub wires: FixedVec<Wire>,
    pub circuits: FixedVec<Circuit>,
    pub states: StateCollection,
}

impl CircuitBoard {
    pub fn new() -> Self {
        Self {
            wires: vec![].into(),
            circuits: vec![].into(),
            states: StateCollection::new(),
        }
    }

    pub fn create_circuit(&mut self, pos: Vec2i, preview: &dyn CircuitPreview) -> usize {
        let id = self.circuits.first_free_pos();
        let circ = Circuit::create(id, pos, preview);
        self.circuits.set(circ, id);
        id
    }

    pub fn create_wire(&mut self) -> usize {
        let id = self.wires.first_free_pos();
        let wire = Wire {
            id,
            points: HashMap::default(),
        };
        self.wires.set(wire, id);
        id
    }

    pub fn merge_wires(&mut self, id: usize, with: usize, update_states: bool) {
        if !self.wires.exists(id) {
            return;
        }

        let with = match self.wires.remove(with) {
            Some(w) => w,
            None => return,
        };

        let Wire { id: _, points } = with;

        for point in points.values() {
            if let Some(pin) = &point.pin {
                pin.write().unwrap().set_wire(None, Some(id));
            }
        }

        let wire = self.wires.get_mut(id).unwrap();
        for point in points {
            wire.points.insert(point.0, point.1);
        }

        if update_states {
            self.states.update_wire(id);
        }
    }

    pub fn split_wire(
        &mut self,
        id: usize,
        points: &HashSet<Vec2i>,
        update_states: bool,
    ) -> Option<usize> {
        fn search_wire_point(
            nodes: impl Iterator<Item = Vec2i>,
            from: Vec2i,
            vertical: bool,
            inverse: bool,
        ) -> Option<Vec2i> {
            let f_eq_coord = if vertical { from.x() } else { from.y() };
            let f_diff_coord = if vertical { from.y() } else { from.x() };

            nodes
                .filter_map(|p| {
                    let p_eq_coord = if vertical { p.x() } else { p.y() };
                    if p_eq_coord != f_eq_coord {
                        None
                    } else {
                        let p_diff_coord = if vertical { p.y() } else { p.x() };

                        let dist = if inverse {
                            f_diff_coord - p_diff_coord
                        } else {
                            p_diff_coord - f_diff_coord
                        };

                        if dist <= 0 {
                            None
                        } else {
                            Some((p, dist))
                        }
                    }
                })
                .min_by(|a, b| a.1.cmp(&b.1))
                .map(|p| p.0)
        }

        let new_wire_id = self.wires.first_free_pos();
        let wire = match self.wires.get_mut(id) {
            None => return None,
            Some(w) => w,
        };

        let mut new_points = HashMap::new();
        for pos in points.iter() {
            let point = match wire.points.remove(pos) {
                None => continue,
                Some(p) => p,
            };

            let ref_right = search_wire_point(wire.points.keys().cloned(), *pos, false, true);
            let ref_down = search_wire_point(wire.points.keys().cloned(), *pos, true, true);
            if let Some(r) = ref_right {
                if let Some(p) = wire.points.get_mut(&r) {
                    p.left = false
                }
            }
            if let Some(r) = ref_down {
                if let Some(p) = wire.points.get_mut(&r) {
                    p.up = false
                }
            }

            let WirePoint { left, up, pin } = point;

            let left = left
                && search_wire_point(wire.points.keys().cloned(), *pos, false, false)
                    .is_some_and(|p| points.contains(&p));
            let up = up
                && search_wire_point(wire.points.keys().cloned(), *pos, true, false)
                    .is_some_and(|p| points.contains(&p));
            let pin = pin.map(|p| {
                p.write().unwrap().set_wire(None, Some(new_wire_id));
                p
            });

            new_points.insert(*pos, WirePoint { up, left, pin });
        }

        let new_wire = Wire {
            id: new_wire_id,
            points: new_points,
        };
        self.wires.set(new_wire, new_wire_id);

        if update_states {
            self.states.update_wire(id);
            self.states.update_wire(new_wire_id);
        }

        Some(new_wire_id)
    }
}

pub enum SelectedBoardItem<'a> {
    None,
    Wire,
    Circuit(&'a dyn CircuitPreview),
}

impl<'a> SelectedBoardItem<'a> {
    pub fn none(&self) -> bool {
        matches!(self, SelectedBoardItem::None)
    }

    pub fn wire(&self) -> bool {
        matches!(self, SelectedBoardItem::Wire)
    }

    pub fn circuit(&self) -> Option<&'a dyn CircuitPreview> {
        match self {
            SelectedBoardItem::Circuit(c) => Some(*c),
            _ => None,
        }
    }
}

#[derive(Hash, PartialEq, Eq)]
enum SelectedWorldObject {
    WirePart { pos: Vec2i, dir: Direction2 },
    Circuit { id: usize },
}

enum SelectionMode {
    Include,
    Exclude,
}

pub struct ActiveCircuitBoard {
    pub board: Arc<RwLock<CircuitBoard>>,
    pub state: Arc<State>,

    wire_nodes: Chunks2D<16, WireNode>,
    circuit_nodes: Chunks2D<16, CircuitNode>,

    wire_drag_pos: Option<Vec2i>,

    selection_start_pos: Option<Vec2f>,
    selection_rect: Option<Rect>,
    selection: HashSet<SelectedWorldObject>,
    selection_change: HashSet<SelectedWorldObject>,
    selection_mode: SelectionMode,
}

impl ActiveCircuitBoard {
    const WIRE_THICKNESS: f32 = 0.2;
    const WIRE_POINT_THICKNESS: f32 = 0.35;

    pub fn new(board: Arc<RwLock<CircuitBoard>>, state: usize) -> Option<Self> {
        let state = {
            let board = board.read().unwrap();
            board.states.get(state)?
        };

        Some(Self {
            board,
            wire_nodes: Default::default(), // TODO
            circuit_nodes: Default::default(),
            state,
            wire_drag_pos: None,

            selection_start_pos: None,
            selection_rect: None,
            selection: HashSet::new(),
            selection_change: HashSet::new(),
            selection_mode: SelectionMode::Include,
        })
    }

    pub fn update(&mut self, ctx: &PaintContext, selected: SelectedBoardItem) {
        self.pre_update_selection(ctx, selected.none());
        ctx.draw_chunks(
            &self.wire_nodes,
            &self,
            |node| !node.is_empty(),
            |node, pos, ctx, this, lookaround| {
                this.draw_wire_node(ctx, node, pos, lookaround);
            },
        );

        ctx.draw_chunks(
            &self.circuit_nodes,
            &*self,
            |n| n.circuit.is_some(),
            |node, pos, ctx, this, _| this.draw_circuit_node(node, pos, ctx),
        );

        self.update_wires(ctx, selected.wire());
        self.update_circuits(ctx, selected.circuit());
        self.update_selection(ctx, selected.none());
    }

    fn draw_wire_node(
        &self,
        ctx: &PaintContext<'_>,
        node: &WireNode,
        pos: Vec2i,
        lookaround: &ChunksLookaround<'_, 16, WireNode>,
    ) {
        struct WireDrawInfo {
            dist: Option<u32>,
            next_dist: Option<u32>,
            wire: Option<usize>,
            dir: Direction2,
            pos: Vec2i,
            color: Color32,
        }

        fn draw_wire(info: WireDrawInfo, this: &ActiveCircuitBoard, ctx: &PaintContext) {
            if info.dist.is_none() && info.wire.is_none() {
                return;
            }

            let edge = match info.dir {
                Direction2::Up => info.pos.y() == ctx.bounds.tiles_br.y(),
                Direction2::Left => info.pos.x() == ctx.bounds.tiles_br.x(),
            };

            if (info.wire.is_none() || info.dist.is_none()) && !edge {
                return;
            }

            let (length, pos) = match info.next_dist {
                None => match info.dist {
                    None => return,
                    Some(d) => (d, info.pos),
                },
                Some(next_dist) => {
                    let dist = match info.dist {
                        None => return,
                        Some(d) => d,
                    };

                    if next_dist == dist + 1 {
                        (next_dist, info.dir.move_vector(info.pos, 1, false))
                    } else {
                        (dist, info.pos)
                    }
                }
            };

            let part = WirePart {
                length: NonZeroU32::new(length).unwrap(),
                pos,
                dir: info.dir,
            };

            this.draw_wire_part(ctx, &part, info.color.linear_multiply(0.5));
        }
        if node.is_empty() {
            return;
        }
        let wires = self.wires_at_node(pos, node);

        for dir in Direction2::iter_all() {
            let wire_color = match wires.dir(dir.into()) {
                None => continue,
                Some(w) => self.state.read_wire(w).color(),
            };

            let next_node_rel_pos = dir.unit_vector(false).convert(|v| v as isize);
            let next_node = lookaround.get_relative(next_node_rel_pos);

            let draw = WireDrawInfo {
                dist: node.get_dir(dir.into()).get(),
                next_dist: next_node.map_or(None, |n| n.get_dir(dir.into()).get()),
                wire: node.wire.get(),
                dir,
                pos,
                color: wire_color,
            };

            draw_wire(draw, self, ctx);
        }

        if let Some(wire) = node.wire.get() {
            let possible_intersection = if ctx.egui_ctx.input(|input| input.modifiers.shift) {
                true
            } else {
                Direction4::iter_all().all(|dir| node.get_dir(dir).is_some())
            };

            if possible_intersection {
                if let Some(wire) = self.board.read().unwrap().wires.get(wire) {
                    Self::draw_wire_point(
                        ctx,
                        pos,
                        wire.color(&self.state),
                        wire.points.get(&pos).is_some_and(|p| p.pin.is_some()),
                        Some(wire.id),
                    )
                }
            }
        }

        let incorrect_font = TextStyle::Monospace.resolve(ctx.ui.style());

        for dir in Direction4::iter_all() {
            let correct = match node.get_dir(dir).get() {
                None => true,
                Some(dist) => 'm: {

                    // TODO: something wrong here

                    let back_dir = dir.inverted();
                    for i in 1..=dist {
                        let target = lookaround.get_relative(
                            dir.unit_vector().convert(|v| v as isize) * dist as isize,
                        );
                        let target = match target {
                            Some(t) => t,
                            None => break 'm false,
                        };
                        //let back_ptr = target.get_dir(back_dir);
                        //if back_ptr.get() != Some(i) {
                        //   break 'm false;
                        //}

                        if i == dist {
                            // Target node wire should be not None and match current node wire, if it exists
                            if target
                                .wire
                                .is_some_and(|w| node.wire.is_some_and(|node_wire| node_wire != w))
                            {
                                break 'm false;
                            }
                        } else if target.wire.is_some() && i != dist {
                            break 'm false;
                        }
                    }
                    true
                }
            };

            if let Some(dist) = node.get_dir(dir).get() {
                let world_pos = pos.convert(|v| v as f32 + 0.5);
                let world_pos = world_pos + dir.unit_vector().convert(|v| v as f32 * 0.25);
                let screen_pos = ctx.screen.world_to_screen(world_pos);
                ctx.paint.text(
                    screen_pos.into(),
                    Align2::CENTER_CENTER,
                    dist.to_string(),
                    incorrect_font.clone(),
                    if correct {
                        Color32::WHITE
                    } else {
                        Color32::RED
                    },
                );
            }
        }

        if let Some(wire) = node.wire.get() {
            let world_pos = pos.convert(|v| v as f32 + 0.5);
            let screen_pos = ctx.screen.world_to_screen(world_pos);
            ctx.paint.text(
                screen_pos.into(),
                Align2::CENTER_CENTER,
                wire.to_string(),
                incorrect_font,
                Color32::WHITE,
            );
        }

        //let correct_up = node.up == 0
        //    ||
        //let correct_left = node.left == 0
        //    || lookaround
        //        .get_relative(-(node.left as isize), 0)
        //        .is_some_and(|n| n.wire.is_some())
        //        && (1..node.left as isize).all(|p| {
        //            lookaround
        //                .get_relative(-p, 0)
        //                .is_some_and(|n| n.wire.is_none())
        //        });
        //if !correct_up || !correct_left {
        //    let pos = ctx.screen.world_to_screen_tile(pos);
        //    let rect = Rect::from_min_size(pos.into(), vec2(ctx.screen.scale, ctx.screen.scale));
        //    ctx.paint.rect_filled(
        //        rect,
        //        Rounding::none(),
        //        Color32::from_rgba_unmultiplied(255, 0, 0, 100),
        //    );
        //}
    }

    fn draw_circuit_node(&self, node: &CircuitNode, pos: Vec2i, ctx: &PaintContext) {
        if !node.origin_dist.is_zero()
            && pos.x() != ctx.bounds.tiles_tl.x()
            && pos.y() != ctx.bounds.tiles_tl.y()
        {
            return;
        }
        let circ_id = match node.circuit.get() {
            None => return,
            Some(c) => c,
        };

        let board = self.board.read().unwrap();
        let circuit = match board.circuits.get(circ_id) {
            None => return,
            Some(c) => c,
        };

        let circ_info = circuit.info.read().unwrap();

        let pos = pos - node.origin_dist.convert(|v| v as i32);
        let screen_pos = ctx.screen.world_to_screen_tile(pos);
        let screen_size = circ_info.size.convert(|v| v as f32) * ctx.screen.scale;
        let rect = Rect::from_min_size(screen_pos.into(), screen_size.into());
        let circ_ctx = ctx.with_rect(rect);

        let state_ctx = CircuitStateContext::new(&self.state, circuit);

        circuit.imp.read().unwrap().draw(&state_ctx, &circ_ctx);
    }

    fn update_wires(&mut self, ctx: &PaintContext, selected: bool) {
        if !selected {
            self.wire_drag_pos = None;
            return;
        }

        let mouse_tile_pos = ctx
            .egui_ctx
            .input(|input| input.pointer.interact_pos())
            .map(|p| ctx.screen.screen_to_world(Vec2f::from(p)));

        let mouse_tile_pos_i = mouse_tile_pos.map(|p| p.convert(|v| v.floor() as i32));

        let drawing_wire = Self::calc_wire_part(self.wire_drag_pos, mouse_tile_pos_i);
        if let Some(ref part) = drawing_wire {
            self.draw_wire_part(ctx, part, Color32::GRAY);
        }

        let interaction = ctx
            .ui
            .interact(ctx.rect, ctx.ui.id(), Sense::click_and_drag());

        if self.wire_drag_pos.is_none() && interaction.drag_started_by(egui::PointerButton::Primary)
        {
            self.wire_drag_pos = mouse_tile_pos_i;
        } else if self.wire_drag_pos.is_some()
            && interaction.drag_released_by(egui::PointerButton::Primary)
        {
            self.wire_drag_pos = None;

            if let Some(part) = drawing_wire {
                self.place_wire_part(part);
            }
        }

        if let Some(mouse_pos) = mouse_tile_pos_i {
            if interaction.clicked_by(egui::PointerButton::Primary) && self.wire_drag_pos.is_none()
            {
                self.try_toggle_node_intersection(mouse_pos);
            }

            for dir in Direction4::iter_all() {
                if let Some(found) = self.find_node(mouse_pos, dir) {
                    let world_pos = ctx.screen.world_to_screen_tile(found.pos);
                    let size = vec2(ctx.screen.scale, ctx.screen.scale);
                    let rect = Rect::from_min_size(world_pos.into(), size);

                    let r = if dir.is_vertical() { 255 } else { 0 };
                    let g = if dir.is_right_bottom() { 255 } else { 0 };
                    let color = Color32::from_rgba_unmultiplied(r, g, 0, 128);
                    ctx.paint.rect_filled(rect, Rounding::none(), color);
                }
            }
        }
    }

    fn update_circuits(&mut self, ctx: &PaintContext, selected: Option<&dyn CircuitPreview>) {
        if let Some(p) = selected {
            let mouse_tile_pos = ctx
                .egui_ctx
                .input(|input| input.pointer.interact_pos())
                .map(|p| ctx.screen.screen_to_world(Vec2f::from(p)));
            let mouse_tile_pos_i = match mouse_tile_pos {
                None => return,
                Some(v) => v.convert(|v| v.floor() as i32),
            };
            let size = p.size();
            if size.x() == 0 || size.y() == 0 {
                return;
            }
            let place_pos = mouse_tile_pos_i - size.convert(|v| v as i32) / 2;
            let rect = Rect::from_min_size(
                ctx.screen.world_to_screen_tile(place_pos).into(),
                (size.convert(|v| v as f32) * ctx.screen.scale).into(),
            );
            p.draw_preview(&ctx.with_rect(rect));
            let interaction = ctx.ui.interact(ctx.rect, ctx.ui.id(), Sense::click());

            if interaction.clicked_by(eframe::egui::PointerButton::Primary) {
                self.place_circuit(place_pos, p);
            }
        }
    }

    fn pre_update_selection(&mut self, ctx: &PaintContext, selected: bool) {
        fn draw_selection(
            this: &ActiveCircuitBoard,
            item: &SelectedWorldObject,
            ctx: &PaintContext,
            shift: bool,
        ) {
            match item {
                //SelectedWorldObject::WirePoint { id: _, pos } => {
                //    if !this.should_draw_wire_point(*pos, shift) {
                //        return;
                //    }
                //    let rect = ActiveCircuitBoard::calc_wire_point_rect(&ctx.screen, *pos);
                //    let rect = rect.expand(2.0);
                //    ctx.paint
                //        .rect_filled(rect, Rounding::none(), Color32::WHITE);
                //}
                SelectedWorldObject::WirePart { pos, dir } => {
                    if let Some(w) = this.find_node(*pos, (*dir).into()) {
                        let part = WirePart {
                            pos: w.pos,
                            dir: *dir,
                            length: w.distance,
                        };
                        let rect = ActiveCircuitBoard::calc_wire_part_rect(&ctx.screen, &part);
                        let rect = rect.expand(2.0);
                        ctx.paint
                            .rect_filled(rect, Rounding::none(), Color32::WHITE);
                    }
                }
                SelectedWorldObject::Circuit { id } => {
                    if let Some(circ) = this.board.read().unwrap().circuits.get(*id) {
                        let rect_pos = ctx.screen.world_to_screen_tile(circ.pos);
                        let rect_size =
                            circ.info.read().unwrap().size.convert(|v| v as f32) * ctx.screen.scale;
                        let rect = Rect::from_min_size(rect_pos.into(), rect_size.into());
                        let rect = rect.expand(2.0);
                        ctx.paint.rect(
                            rect,
                            Rounding::none(),
                            Color32::from_rgba_unmultiplied(200, 200, 255, 20),
                            Stroke::new(2.0, Color32::WHITE),
                        );
                    }
                }
            }
        }

        if !selected {
            self.selection_start_pos = None;
            self.selection_rect = None;
        } else {
            let mouse_tile_pos = ctx
                .egui_ctx
                .input(|input| input.pointer.interact_pos())
                .map(|p| ctx.screen.screen_to_world(Vec2f::from(p)));

            if let Some(selection_start_pos) = self.selection_start_pos {
                if let Some(mouse_tile_pos) = mouse_tile_pos {
                    let start = selection_start_pos;
                    let end = mouse_tile_pos;

                    let min = Vec2f::from([start.x().min(end.x()), start.y().min(end.y())]);
                    let max = Vec2f::from([start.x().max(end.x()), start.y().max(end.y())]);

                    let min_tile = min.convert(|v| v.floor() as i32);
                    let max_tile = max.convert(|v| v.ceil() as i32);

                    let rect_min = ctx.screen.world_to_screen(min);
                    let rect_max = ctx.screen.world_to_screen(max);
                    let rect = Rect::from_min_max(rect_min.into(), rect_max.into());
                    self.selection_rect = Some(rect);

                    ctx.paint.rect_filled(
                        rect,
                        Rounding::none(),
                        Color32::from_rgba_unmultiplied(200, 200, 255, 10),
                    );

                    self.selection_update_changes(
                        min_tile,
                        (max_tile - min_tile).convert(|v| v as u32),
                    );
                } else {
                    self.selection_rect = None;
                }
            } else {
                self.selection_rect = None;
            }

            let interaction = ctx
                .ui
                .interact(ctx.rect, ctx.ui.id(), Sense::click_and_drag());

            if self.selection_start_pos.is_none()
                && interaction.drag_started_by(egui::PointerButton::Primary)
            {
                self.selection_start_pos = mouse_tile_pos;
                self.selection_change.clear();

                let (shift, ctrl) = ctx
                    .egui_ctx
                    .input(|input| (input.modifiers.shift, input.modifiers.ctrl));
                if !shift && !ctrl {
                    self.selection.clear();
                }

                if ctrl {
                    self.selection_mode = SelectionMode::Exclude;
                } else {
                    self.selection_mode = SelectionMode::Include;
                }
            } else if self.selection_start_pos.is_some()
                && interaction.drag_released_by(egui::PointerButton::Primary)
            {
                self.selection_start_pos = None;

                match self.selection_mode {
                    SelectionMode::Include => self.selection.extend(self.selection_change.drain()),
                    SelectionMode::Exclude => {
                        for i in self.selection_change.drain() {
                            self.selection.remove(&i);
                        }
                    }
                }
                // TODO
                self.selection_change.clear();
            }
        }

        let shift = ctx.egui_ctx.input(|input| input.modifiers.shift);
        for item in self.selection.iter() {
            if matches!(self.selection_mode, SelectionMode::Exclude)
                && self.selection_change.contains(item)
            {
                continue;
            }
            draw_selection(self, item, ctx, shift);
        }
        if matches!(self.selection_mode, SelectionMode::Include) {
            for item in self.selection_change.iter() {
                if self.selection.contains(item) {
                    continue;
                }
                draw_selection(self, item, ctx, shift);
            }
        }
    }

    fn update_selection(&mut self, ctx: &PaintContext, selected: bool) {
        if let Some(rect) = self.selection_rect {
            ctx.paint
                .rect_stroke(rect, Rounding::none(), Stroke::new(1.0, Color32::WHITE));
        }
    }

    fn should_draw_wire_point(&self, pos: Vec2i, shift: bool) -> bool {
        let node = match self.wire_nodes.get(pos.convert(|v| v as isize)) {
            Some(n) => n,
            None => return false,
        };

        if node.wire.is_none() {
            return false;
        }

        if shift {
            return true;
        }

        Direction4::iter_all().all(|dir| node.get_dir(dir).is_some())
    }

    fn selection_update_changes(&mut self, pos: Vec2i, size: Vec2u) {
        self.selection_change.clear();

        for (pos, node) in self
            .wire_nodes
            .iter_area(pos.convert(|v| v as isize), size.convert(|v| v as usize))
        {
            let pos = pos.convert(|v| v as i32);

            //if let Some(wire) = node.wire.get() {
            //    self.selection_change
            //        .insert(SelectedWorldObject::WirePoint { id: wire, pos });
            //}
            if node.wire.is_some() {
                for dir in Direction4::iter_all() {
                    let node = self.find_node_from_node(node, pos, dir);
                    let node = match node {
                        None => continue,
                        Some(node) => node,
                    };
                    let (dir, forward) = dir.into_dir2();

                    self.selection_change.insert(SelectedWorldObject::WirePart {
                        pos: if forward { pos } else { node.pos },
                        dir,
                    });
                }
            } else {
                for dir in [Direction4::Right, Direction4::Down] {
                    let node = self.find_node_from_node(node, pos, dir);
                    let node = match node {
                        None => continue,
                        Some(node) => node,
                    };
                    self.selection_change.insert(SelectedWorldObject::WirePart {
                        pos: node.pos,
                        dir: dir.into_dir2().0,
                    });
                }
            }
        }
        for (_, node) in self
            .circuit_nodes
            .iter_area(pos.convert(|v| v as isize), size.convert(|v| v as usize))
        {
            let circuit = match node.circuit.get() {
                Some(c) => c,
                None => continue,
            };

            self.selection_change
                .insert(SelectedWorldObject::Circuit { id: circuit });
        }
    }

    fn place_circuit(&mut self, place_pos: Vec2i, preview: &dyn CircuitPreview) {
        let size = preview.size();

        for j in 0..size.y() as i32 {
            for i in 0..size.x() as i32 {
                let pos = place_pos + [i, j];
                if self
                    .circuit_nodes
                    .get(pos.convert(|v| v as isize))
                    .is_some_and(|n| n.circuit.is_some())
                {
                    return;
                }
            }
        }

        let cid = {
            self.board
                .write()
                .unwrap()
                .create_circuit(place_pos, preview)
        };

        for j in 0..size.y() {
            for i in 0..size.x() {
                let pos = place_pos + [i as i32, j as i32];
                let node = self
                    .circuit_nodes
                    .get_or_create_mut(pos.convert(|v| v as isize));

                node.circuit.set(Some(cid));
                node.origin_dist = [i, j].into();
            }
        }
        let circ_info = {
            self.board
                .read()
                .unwrap()
                .circuits
                .get(cid)
                .unwrap()
                .info
                .clone()
        };
        for pin in circ_info.read().unwrap().pins.iter() {
            let pos = place_pos + pin.pos.convert(|v| v as i32);
            if let Some(wire) = self.create_wire_intersection(pos, None) {
                let wire = if let Some(wire) = self.board.write().unwrap().wires.get_mut(wire) {
                    if let Some(p) = wire.points.get_mut(&pos) {
                        p.pin = Some(pin.pin.clone());
                    }
                    Some(wire.id)
                } else {
                    None
                };

                if let Some(wire) = wire {
                    pin.pin
                        .write()
                        .unwrap()
                        .set_wire(Some(&self.board.read().unwrap()), Some(wire));
                }
            }
        }
        let board = self.board.read().unwrap();
        let circ = board.circuits.get(cid).unwrap();
        board.states.init_circuit(circ);
        board.states.update_circuit_signals(cid, None);
    }

    fn pin_at(&self, pos: Vec2i) -> Option<Arc<RwLock<CircuitPin>>> {
        let circ = self.circuit_nodes.get(pos.convert(|v| v as isize))?;
        let pos = circ.origin_dist;
        let circ = circ.circuit.get()?;

        let board = self.board.read().unwrap();
        let circ_info = board.circuits.get(circ)?.info.clone();
        let circ_info = circ_info.read().unwrap();

        circ_info
            .pins
            .iter()
            .find(|p| p.pos == pos)
            .map(|p| p.pin.clone())
    }

    fn try_toggle_node_intersection(&mut self, pos: Vec2i) {
        let node = self.wire_nodes.get(pos.convert(|v| v as isize));

        let node = match node {
            None => return,
            Some(n) if n.is_empty() => return,
            Some(v) => v,
        };

        let center = node.wire.is_some();
        let left = node.left.is_some();
        let up = node.up.is_some();
        let right = node.right.is_some();
        let down = node.down.is_some();

        if up != down || left != right {
            return;
        }

        if center {
            self.remove_intersection_at_node(pos, *node, true);
        } else {
            self.create_wire_intersection_at_node(pos, *node, None);
        }
    }

    fn calc_wire_part_rect(screen: &Screen, part: &WirePart) -> Rect {
        let thickness = screen.scale * Self::WIRE_THICKNESS;

        let topleft = part
            .dir
            .move_vector(part.pos, part.length.get() as i32, true);

        let pos = screen.world_to_screen_tile(topleft) + ((screen.scale - thickness) * 0.5);
        let length = screen.scale * part.length.get() as f32 + thickness;

        let rect_size = match part.dir {
            Direction2::Up => [thickness, length],
            Direction2::Left => [length, thickness],
        };
        Rect::from_min_size(pos.into(), rect_size.into())
    }

    fn draw_wire_part(
        &self,
        ctx: &PaintContext,
        part: &WirePart,
        color: Color32
    ) {
        let screen = &ctx.screen;
        let rect = Self::calc_wire_part_rect(screen, part);
        ctx.paint.rect_filled(rect, Rounding::none(), color);
    }

    fn calc_wire_point_rect(screen: &Screen, pos: Vec2i) -> Rect {
        let thickness = screen.scale * Self::WIRE_POINT_THICKNESS;

        let rect_pos = screen.world_to_screen_tile(pos) + ((screen.scale - thickness) * 0.5);

        Rect::from_min_size(rect_pos.into(), vec2(thickness, thickness))
    }

    fn calc_thin_wire_point_rect(screen: &Screen, pos: Vec2i) -> Rect {
        let thickness = screen.scale * Self::WIRE_THICKNESS;

        let rect_pos = screen.world_to_screen_tile(pos) + ((screen.scale - thickness) * 0.5);

        Rect::from_min_size(rect_pos.into(), vec2(thickness, thickness))
    }

    fn draw_wire_point(
        ctx: &PaintContext,
        pos: Vec2i,
        color: Color32,
        pin: bool,
        wire_id: Option<usize>,
    ) {
        let screen = &ctx.screen;

        let rect = Self::calc_wire_point_rect(screen, pos);
        ctx.paint.rect_filled(rect, Rounding::none(), color);

        // DEBUG: visuals
        if pin {
            ctx.paint
                .rect_stroke(rect, Rounding::none(), Stroke::new(1.0, Color32::RED));
        }

        if let Some(wire) = wire_id {
            let thickness = screen.scale * 0.15;

            let pos = screen.world_to_screen_tile(pos) + ((screen.scale - thickness) * 0.5);
            let rect = Rect::from_min_size(pos.into(), vec2(thickness, thickness));

            let mut hasher = DefaultHasher::new();
            hasher.write_usize(wire);
            hasher.write_usize(1999573);
            let [r, _, g, _, b, _, _, _] = hasher.finish().to_le_bytes();

            ctx.paint
                .rect_filled(rect, Rounding::none(), Color32::from_rgb(r, g, b));
        }
    }

    fn calc_wire_part(from: Option<Vec2i>, to: Option<Vec2i>) -> Option<WirePart> {
        if let Some(from) = from {
            if let Some(to) = to {
                if from != to {
                    let angle = (to - from).convert(|v| v as f32).angle_to_x();
                    let axis = (angle / (TAU / 4.0)).round() as i32 % 4;

                    let origin = match axis {
                        0 => [to.x(), from.y()].into(),
                        1 => from,
                        2 => from,
                        3 => [from.x(), to.y()].into(),
                        _ => unreachable!(),
                    };
                    let length = match axis {
                        0 => to.x() - from.x(),
                        1 => from.y() - to.y(),
                        2 => from.x() - to.x(),
                        3 => to.y() - from.y(),
                        _ => unreachable!(),
                    } as u32;

                    let dir = match axis {
                        0 => Direction2::Left,
                        1 => Direction2::Up,
                        2 => Direction2::Left,
                        3 => Direction2::Up,
                        _ => unreachable!(),
                    };

                    let part = WirePart {
                        pos: origin,
                        length: NonZeroU32::new(length).unwrap(),
                        dir,
                    };

                    Some(part)
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn create_wire_intersection(&mut self, pos: Vec2i, wire: Option<usize>) -> Option<usize> {
        let node = self.wire_nodes.get(pos.convert(|v| v as isize));
        let node = match node {
            Some(v) => *v,
            None => return None,
        };
        self.create_wire_intersection_at_node(pos, node, wire)
    }

    // wire param: if Some(w) will use wire w, if None, will try to figure ot which wire to use (with merging wires)
    pub fn create_wire_intersection_at_node(
        &mut self,
        pos: Vec2i,
        node: WireNode,
        wire: Option<usize>,
    ) -> Option<usize> {
        let wires = self.wires_at_node(pos, &node);

        match wires {
            TileWires::None => None,
            TileWires::Point {
                wire,
                left: _,
                up: _,
                right: _,
                down: _,
            } => Some(wire),
            TileWires::One { wire, vertical: _ } => {
                self.set_wire_point(pos, Some(wire), true);
                Some(wire)
            }
            TileWires::Two {
                horizontal,
                vertical,
            } => {
                let wire = if horizontal != vertical {
                    let (h_nodes, v_nodes) = {
                        let board = self.board.read().unwrap();
                        (
                            board.wires.get(horizontal).map(|w| w.points.len()),
                            board.wires.get(vertical).map(|w| w.points.len()),
                        )
                    };

                    match (h_nodes, v_nodes) {
                        (Some(hn), Some(vn)) => {
                            if hn > vn {
                                self.merge_wires(horizontal, vertical, false);
                                Some(horizontal)
                            } else {
                                self.merge_wires(vertical, horizontal, false);
                                Some(vertical)
                            }
                        }
                        _ => None,
                    }
                } else {
                    Some(horizontal)
                };

                wire.and_then(|w| {
                    self.set_wire_point(pos, Some(w), true);
                    Some(w)
                })
            }
        }
    }

    #[allow(unused)]
    fn remove_intersection(&mut self, pos: Vec2i, split: bool) {
        if let Some(node) = self.wire_nodes.get(pos.convert(|v| v as isize)) {
            self.remove_intersection_at_node(pos, *node, split)
        }
    }

    // split param: if intersection was of 2 wires, split them
    fn remove_intersection_at_node(&mut self, pos: Vec2i, node: WireNode, split: bool) {
        let (wire, int4) = match self.wires_at_node(pos, &node) {
            TileWires::None => return,
            TileWires::One { wire, vertical } => return,
            TileWires::Two {
                horizontal,
                vertical,
            } => return,
            TileWires::Point {
                wire,
                left,
                up,
                right,
                down,
            } => {
                // Removins point is allowed only on straight lines, 4-intersections and if it has only one connection
                if (left != right || up != down)
                    && [left, up, right, down].into_iter().filter(|v| *v).count() != 1
                {
                    return;
                }
                (wire, left && up && right && down)
            }
        };

        if self
            .board
            .read()
            .unwrap()
            .wires
            .get(wire)
            .is_some_and(|w| w.points.get(&pos).is_some_and(|p| p.pin.is_some()))
        {
            return;
        }

        let split = split && int4;

        let prev = self.set_wire_point(pos, None, !split);

        if split {
            if let Some(wire) = prev {
                self.split_wires(wire, true);
            }
        }
    }

    fn place_wire_part(&mut self, part: WirePart) {
        let part = match self.optimize_wire_part(part) {
            Some(p) => p,
            None => return,
        };

        let wires_crossed = {
            let mut wires_crossed = HashSet::new();
            for (i, pos) in part.iter_pos(true).enumerate() {
                let node = match self.wire_nodes.get(pos.convert(|v| v as isize)) {
                    None => continue,
                    Some(v) => v,
                };

                if i == 0 || i as u32 == part.length.get() {
                    match self.wires_at(pos) {
                        TileWires::None => {}
                        TileWires::One { wire, vertical } => {
                            wires_crossed.insert(wire);
                        }
                        TileWires::Two {
                            horizontal,
                            vertical,
                        } => {
                            wires_crossed.insert(horizontal);
                            wires_crossed.insert(vertical);
                        }
                        TileWires::Point {
                            wire,
                            left,
                            up,
                            right,
                            down,
                        } => {
                            wires_crossed.insert(wire);
                        }
                    }
                } else if let Some(wire) = node.wire.get() {
                    wires_crossed.insert(wire);
                }
            }
            wires_crossed
        };

        let new_wire = match wires_crossed.len() {
            0 => self.board.write().unwrap().create_wire(),
            1 => *wires_crossed.iter().next().unwrap(),
            _ => {
                let main_wire = {
                    self.board
                        .read()
                        .unwrap()
                        .wires
                        .iter()
                        .filter(|v| wires_crossed.contains(&v.id))
                        .max_by(|x, y| x.points.len().cmp(&y.points.len()))
                        .expect("Some matching wires")
                        .id
                };

                for wire in wires_crossed.iter() {
                    if *wire != main_wire {
                        self.merge_wires(main_wire, *wire, false)
                    }
                }
                main_wire
            }
        };

        self.set_wire_point(part.pos, Some(new_wire), false);
        self.set_wire_point(
            part.dir
                .move_vector(part.pos, part.length.get() as i32, true),
            Some(new_wire),
            false,
        );

        self.place_wire_multipart(&part, new_wire);

        let board = self.board.read().unwrap();
        board.states.update_wire(new_wire);
    }

    fn place_wire_multipart(&mut self, part: &WirePart, wire: usize) {
        fn fix_pointers(this: &mut ActiveCircuitBoard, pos: Vec2i, dist: u32, dir: Direction4) {
            for (i, pos) in dir.iter_pos_along(pos, dist as i32, false).enumerate() {
                let node = this
                    .wire_nodes
                    .get_or_create_mut(pos.convert(|v| v as isize));
                node.get_dir_mut(dir.inverted()).set(Some(i as u32 + 1))
            }
        }

        let dir_rev = Direction4::from(part.dir).inverted();

        let mut dist = 0;
        for (i, pos) in part.iter_pos(true).enumerate() {
            let node = self
                .wire_nodes
                .get_or_create_mut(pos.convert(|v| v as isize));

            let point = node.wire.is_some() || i == 0 || i as u32 == part.length.get();

            if i > 0 {
                node.get_dir_mut(dir_rev).set(Some(dist))
            }

            if point {
                if i > 0 {
                    node.wire.set(Some(wire));
                    let node = *node;
                    fix_pointers(self, pos, dist, dir_rev);

                    if let Some(wire) = self.board.write().unwrap().wires.get_mut(wire) {
                        wire.add_point(
                            pos,
                            None,
                            WirePoint {
                                left: node.left.is_some(),
                                up: node.up.is_some(),
                                pin: self.pin_at(pos),
                            },
                        )
                    }
                }
                dist = 1;
            } else {
                dist += 1;
            }
        }
    }

    fn optimize_wire_part(&mut self, part: WirePart) -> Option<WirePart> {
        let mut part_pos = part.pos;
        let mut part_len = part.length.get() as i32;

        let new_start = self
            .wire_nodes
            .get(part_pos.convert(|v| v as isize))
            .and_then(|n| {
                if n.wire.is_some() {
                    None
                } else {
                    self.find_node_from_node(n, part_pos, part.dir.into())
                }
            });

        if let Some(found) = new_start {
            part_len -= found.distance.get() as i32;
            part_pos = found.pos;
        }

        let end_pos = part.dir.move_vector(part_pos, part_len, true);

        let new_end_dist = self
            .wire_nodes
            .get(end_pos.convert(|v| v as isize))
            .and_then(|n| {
                self.find_node_from_node(n, end_pos, Direction4::from(part.dir).inverted())
                    .map(|v| v.distance)
            });

        match new_end_dist {
            None => {}
            Some(dist) => {
                part_len -= dist.get() as i32;
            }
        }

        if part_len <= 0 {
            return None;
        }
        let part = WirePart {
            pos: part_pos,
            length: NonZeroU32::new(part_len as u32).unwrap(),
            dir: part.dir,
        };
        Some(part)
    }

    fn wires_at(&self, pos: Vec2i) -> TileWires {
        match self.wire_nodes.get(pos.convert(|v| v as isize)) {
            None => TileWires::None,
            Some(node) => self.wires_at_node(pos, node),
        }
    }

    fn wires_at_node(&self, pos: Vec2i, node: &WireNode) -> TileWires {
        if let Some(wire) = node.wire.get() {
            return TileWires::Point {
                wire,
                left: node.left.is_some(),
                up: node.up.is_some(),
                right: node.right.is_some(),
                down: node.down.is_some(),
            };
        }

        let up = match node.up.get() {
            None => None,
            Some(up) => self
                .wire_nodes
                .get([pos.x() as isize, (pos.y() - up as i32) as isize])
                .and_then(|n| n.wire.get()),
        };
        let left = match node.left.get() {
            None => None,
            Some(left) => self
                .wire_nodes
                .get([(pos.x() - left as i32) as isize, pos.y() as isize])
                .and_then(|n| n.wire.get()),
        };

        match (left, up) {
            (None, None) => TileWires::None,
            (None, Some(u)) => TileWires::One {
                wire: u,
                vertical: true,
            },
            (Some(l), None) => TileWires::One {
                wire: l,
                vertical: false,
            },
            (Some(left), Some(up)) => TileWires::Two {
                horizontal: left,
                vertical: up,
            },
        }
    }

    fn merge_wires(&mut self, wire: usize, with: usize, update_state: bool) {
        {
            let board = self.board.read().unwrap();

            let with_wire = match board.wires.get(with) {
                Some(w) => w,
                None => return,
            };

            if !board.wires.exists(wire) || wire == with {
                return;
            }

            let points: Vec<_> = with_wire.points.keys().cloned().collect();
            drop(board);

            self.set_node_wires(points.iter(), wire);
        }
        self.board
            .write()
            .unwrap()
            .merge_wires(wire, with, update_state);
    }

    fn find_node(&self, pos: Vec2i, dir: Direction4) -> Option<FoundWireNode> {
        let node = self.wire_nodes.get(pos.convert(|v| v as isize))?;
        self.find_node_from_node(node, pos, dir)
    }

    fn find_node_from_node(
        &self,
        node: &WireNode,
        pos: Vec2i,
        dir: Direction4,
    ) -> Option<FoundWireNode> {
        let dist = node.get_dir(dir).get()?;
        let target_pos = dir.move_vector(pos, dist as i32);
        let target = self.wire_nodes.get(target_pos.convert(|v| v as isize))?;
        let wire = target.wire.get()?;
        Some(FoundWireNode {
            node: *target,
            wire,
            pos: target_pos,
            distance: NonZeroU32::new(dist).unwrap(),
        })
    }

    fn split_wires(&mut self, id: usize, update_states: bool) {
        let mut groups = vec![];

        let mut remaining_nodes: HashSet<_> = {
            let board = self.board.read().unwrap();
            let wire = match board.wires.get(id) {
                Some(w) => w,
                None => return,
            };
            wire.points.keys().copied().collect()
        };
        let mut queue = vec![];

        while !remaining_nodes.is_empty() {
            let mut group = HashSet::new();
            let start = *remaining_nodes.iter().next().unwrap();
            queue.push(start);

            while let Some(pos) = queue.pop() {
                if !remaining_nodes.remove(&pos) {
                    continue;
                }

                group.insert(pos);

                let (ints, intc) = self.node_neighboring_intersections(pos, Some(id));
                (0..intc).for_each(|inti| {
                    let int = ints[inti];
                    if remaining_nodes.contains(&int) {
                        queue.push(int);
                    }
                });
            }
            groups.push(group);
        }

        if groups.len() <= 1 {
            return;
        }

        let biggest_wire = groups
            .iter()
            .enumerate()
            .max_by(|a, b| a.1.len().cmp(&b.1.len()))
            .unwrap()
            .0;

        let mut wires = vec![];

        for (groupid, group) in groups.drain(..).enumerate() {
            if groupid != biggest_wire {
                let new_wire = self.board.write().unwrap().split_wire(id, &group, false);
                if let Some(wire) = new_wire {
                    self.set_node_wires(group.iter(), wire);
                    wires.push(wire);
                }
            } else {
                wires.push(id);
            }
        }

        if update_states {
            let states = self.board.read().unwrap().states.clone();
            for wire in wires {
                states.update_wire(wire);
            }
        }
    }

    fn node_neighboring_intersections(
        &self,
        pos: Vec2i,
        wire: Option<usize>,
    ) -> ([Vec2i; 4], usize) {
        let mut arr = [Vec2i::default(); 4];
        let mut arrpos = 0;

        let node = match self.wire_nodes.get(pos.convert(|v| v as isize)) {
            None => return (arr, 0),
            Some(v) => v,
        };

        for dir in Direction4::iter_all() {
            if let Some(f) = self.find_node_from_node(node, pos, dir) {
                if !wire.is_some_and(|v| v != f.wire) {
                    arr[arrpos] = f.pos;
                    arrpos += 1;
                }
            }
        }
        (arr, arrpos)
    }

    fn set_node_wires<'a>(&mut self, positions: impl Iterator<Item = &'a Vec2i>, wire: usize) {
        for npos in positions {
            match self.wire_nodes.get_mut(npos.convert(|v| v as isize)) {
                None => {}
                Some(n) => n.wire.set(Some(wire)),
            }
        }
    }

    /// returns previous wire id
    fn set_wire_point(
        &mut self,
        pos: Vec2i,
        wire: Option<usize>,
        update_state: bool,
    ) -> Option<usize> {
        fn fix_pointers(
            wires: &mut Chunks2D<16, WireNode>,
            pos: Vec2i,
            dir: Direction4,
            start_dist: u32,
            dist: u32,
            possibly_remove: bool,
        ) {
            if dist == 0 {
                return;
            }

            let remove = possibly_remove && start_dist == 0;

            let back_dir = dir.inverted();

            for (i, pos) in dir.iter_pos_along(pos, dist as i32, true).enumerate() {
                let forward_ptr = if remove { None } else { Some(dist - i as u32) };
                let back_ptr = if remove {
                    None
                } else {
                    Some(i as u32 + start_dist)
                };

                let node = wires.get_or_create_mut(pos.convert(|v| v as isize));
                if i > 0 {
                    node.get_dir_mut(back_dir).set(back_ptr);
                }
                if (i as u32) < dist {
                    node.get_dir_mut(dir).set(forward_ptr);
                }
            }
        }

        let node = self
            .wire_nodes
            .get_or_create_mut(pos.convert(|v| v as isize));
        let prev_wire = node.wire.get();
        if prev_wire == wire {
            return prev_wire;
        }

        node.wire.set(wire);

        let node = *node;

        for dir in Direction4::iter_all() {
            let dist_forward = match node.get_dir(dir).get() {
                Some(d) => d,
                None => continue,
            };

            if wire.is_some() {
                fix_pointers(&mut self.wire_nodes, pos, dir, 0, dist_forward, false);
            } else {
                let dist_back = node.get_dir(dir.inverted()).get().unwrap_or(0);
                fix_pointers(
                    &mut self.wire_nodes,
                    pos,
                    dir,
                    dist_back,
                    dist_forward,
                    true,
                );
            }
        }

        {
            let pin = self.pin_at(pos);
            if let Some(pin) = &pin {
                pin.write().unwrap().set_wire(None, wire);
            }

            let mut board = self.board.write().unwrap();

            if let Some(wire) = prev_wire.and_then(|w| board.wires.get_mut(w)) {
                wire.remove_point(pos, None);
            }

            if let Some(wire) = wire.and_then(|w| board.wires.get_mut(w)) {
                wire.add_point(
                    pos,
                    None,
                    WirePoint {
                        left: node.left.is_some(),
                        up: node.up.is_some(),
                        pin,
                    },
                );
            }
        }

        if update_state {
            let states = self.board.read().unwrap().states.clone();

            if let Some(wire) = prev_wire {
                states.update_wire(wire);
            }

            if let Some(wire) = wire {
                states.update_wire(wire)
            }
        }

        prev_wire
    }
}
