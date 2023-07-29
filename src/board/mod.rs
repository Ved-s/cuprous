use std::{
    cell::RefCell,
    collections::{hash_map::DefaultHasher, HashMap, HashSet},
    f32::consts::TAU,
    hash::Hasher,
    num::NonZeroU32,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
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
    unwrap_option_or_continue, unwrap_option_or_return,
    vector::{IsZero, Vec2f, Vec2i},
    wires::{FoundWireNode, TileWires, Wire, WireNode, WirePart, WirePoint},
    Direction2, Direction4, PaintContext, RwLock, Screen,
};

use self::selection::{SelectedWorldObject, Selection};

mod selection;

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

        self.states.reset_wire(with);
        let with = unwrap_option_or_return!(self.wires.remove(with));

        let Wire { id: _, points } = with;

        for point in points.values() {
            if let Some(pin) = &point.pin {
                pin.write()
                    .unwrap()
                    .set_wire(&self.states, Some(id), false, true);
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
        let wire = unwrap_option_or_return!(self.wires.get_mut(id), None);

        let mut new_points = HashMap::new();
        for pos in points.iter() {
            let point = unwrap_option_or_continue!(wire.points.remove(pos));

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
                p.write()
                    .unwrap()
                    .set_wire(&self.states, Some(new_wire_id), false, true);
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

pub struct ActiveCircuitBoard {
    pub board: Arc<RwLock<CircuitBoard>>,
    pub state: Arc<State>,

    pub wire_nodes: Chunks2D<16, WireNode>,
    pub circuit_nodes: Chunks2D<16, CircuitNode>,

    wire_drag_pos: Option<Vec2i>,
    selection: RefCell<Selection>,

    pub wires_drawn: AtomicUsize,
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
            selection: RefCell::new(Selection::new()),

            wires_drawn: AtomicUsize::new(0),
        })
    }

    pub fn update(&mut self, ctx: &PaintContext, selected: SelectedBoardItem, debug: bool) {
        self.wires_drawn.store(0, Ordering::Relaxed);
        self.selection
            .borrow_mut()
            .pre_update_selection(self, ctx, selected.none());

        if ctx.egui_ctx.input(|input| {
            input.key_pressed(egui::Key::Delete) || input.key_pressed(egui::Key::Backspace)
        }) {
            let mut affected_wires = HashSet::new();
            let drain = {
                let mut selection = self.selection.borrow_mut();
                selection.selection.drain().collect::<Vec<_>>()
            };
            for obj in drain {
                match obj {
                    SelectedWorldObject::Circuit { id } => {
                        self.remove_circuit(id, &mut affected_wires);
                    }
                    SelectedWorldObject::WirePart { pos, dir } => {
                        if let Some(wire) = self.remove_wire_part(pos, dir.into(), true, false) {
                            affected_wires.insert(wire);
                        }
                    }
                }
            }

            let states = self.board.read().unwrap().states.clone();
            for wire in affected_wires {
                states.update_wire(wire);
                self.split_wires(wire, true)
            }
        }

        ctx.draw_chunks(
            &self.wire_nodes,
            &self,
            |node| !node.is_empty(),
            |node, pos, ctx, this, lookaround| {
                this.draw_wire_node(ctx, node, pos, lookaround);
            },
        );

        if debug {
            ctx.draw_chunks(
                &self.wire_nodes,
                &self,
                |node| !node.is_empty(),
                |node, pos, ctx, this, lookaround| {
                    this.draw_wire_node_debug(ctx, node, pos, lookaround);
                },
            );
        }

        ctx.draw_chunks(
            &self.circuit_nodes,
            &*self,
            |n| n.circuit.is_some(),
            |node, pos, ctx, this, _| this.draw_circuit_node(node, pos, ctx),
        );

        self.update_wires(ctx, selected.wire());
        self.update_circuits(ctx, selected.circuit());
        self.selection.borrow_mut().update_selection(ctx);
    }

    /* #region Drawing nodes */

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
                        Some(d) => d,
                        None if info.wire.is_some() => 0,
                        None => return,
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

            this.draw_wire_part(ctx, &part, info.color);
        }
        if node.is_empty() {
            return;
        }
        let wires = self.wires_at_node(pos, node);

        let center_color = node.wire.get().map(|w| self.state.read_wire(w).color());

        for dir in Direction2::iter_all() {
            let wire_color = center_color.or_else(|| {
                wires
                    .dir(dir.into())
                    .map(|w| self.state.read_wire(w).color())
            });

            let wire_color = unwrap_option_or_continue!(wire_color);

            let next_node_rel_pos = dir.unit_vector(false).convert(|v| v as isize);
            let next_node = lookaround.get_relative(next_node_rel_pos);

            let draw = WireDrawInfo {
                dist: node.get_dir(dir.into()).get(),
                next_dist: next_node.and_then(|n| n.get_dir(dir.into()).get()),
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
    }

    fn draw_wire_node_debug(
        &self,
        ctx: &PaintContext<'_>,
        node: &WireNode,
        pos: Vec2i,
        lookaround: &ChunksLookaround<'_, 16, WireNode>,
    ) {
        if node.is_empty() {
            return;
        }
        let font = TextStyle::Monospace.resolve(ctx.ui.style());

        if let Some(wire_id) = node.wire.get() {
            let board = self.board.read().unwrap();
            let wire = board.wires.get(wire_id);

            if let Some(wire) = wire {
                match wire.points.get(&pos) {
                    None => {
                        let pos = ctx.screen.world_to_screen(pos.convert(|v| v as f32));
                        let size = [ctx.screen.scale, ctx.screen.scale];
                        let rect = Rect::from_min_size(pos.into(), size.into());
                        ctx.paint.rect_stroke(
                            rect,
                            Rounding::none(),
                            Stroke::new(2.0, Color32::RED),
                        );
                    }
                    Some(point) => {
                        for dir in Direction2::iter_all() {
                            let point_dir = match dir {
                                Direction2::Up => point.up,
                                Direction2::Left => point.left,
                            };
                            let nodes_dir = node.get_dir(dir.into()).is_some();

                            if point_dir != nodes_dir {
                                let world_pos_center = pos.convert(|v| v as f32 + 0.5);
                                let world_pos = world_pos_center
                                    + dir.unit_vector(true).convert(|v| v as f32 * 0.5);
                                let screen_pos = ctx.screen.world_to_screen(world_pos);
                                ctx.paint.circle_filled(
                                    screen_pos.into(),
                                    ctx.screen.scale * 0.1,
                                    Color32::RED,
                                );
                            }
                        }
                    }
                }
            }

            let world_pos = pos.convert(|v| v as f32 + 0.5);
            let screen_pos = ctx.screen.world_to_screen(world_pos);
            ctx.paint.text(
                screen_pos.into(),
                Align2::CENTER_CENTER,
                wire_id.to_string(),
                font.clone(),
                if wire.is_some() {
                    Color32::WHITE
                } else {
                    Color32::RED
                },
            );
        }

        for dir in Direction4::iter_all() {
            let dist = node.get_dir(dir).get();
            let dist = unwrap_option_or_continue!(dist);

            let correct = 'm: {
                let back_dir = dir.inverted();
                let backptr_start = if node.wire.is_some() {
                    0
                } else {
                    node.get_dir(back_dir).get().unwrap_or(0)
                };
                for i in 1..=dist {
                    let rel_pos = dir.unit_vector().convert(|v| v as isize) * i as isize;

                    let target = lookaround.get_relative(rel_pos);
                    let target = match target {
                        Some(t) => t,
                        None => break 'm false,
                    };
                    let back_ptr = target.get_dir(back_dir);
                    if back_ptr.get() != Some(backptr_start + i) {
                        break 'm false;
                    }

                    if i == dist {
                        // Target node wire should be not None and match current node wire, if it exists

                        match target.wire.get() {
                            None => break 'm false,
                            Some(wire) => {
                                if node.wire.is_some_and(|nw| nw != wire) {
                                    break 'm false;
                                }
                            }
                        }
                    } else if target.wire.is_some() && i != dist {
                        break 'm false;
                    }
                }
                true
            };

            let world_pos = pos.convert(|v| v as f32 + 0.5);
            let world_pos = world_pos + dir.unit_vector().convert(|v| v as f32 * 0.25);
            let screen_pos = ctx.screen.world_to_screen(world_pos);
            ctx.paint.text(
                screen_pos.into(),
                Align2::CENTER_CENTER,
                dist.to_string(),
                font.clone(),
                if correct {
                    Color32::WHITE
                } else {
                    Color32::RED
                },
            );
        }
    }

    fn draw_circuit_node(&self, node: &CircuitNode, pos: Vec2i, ctx: &PaintContext) {
        if !node.origin_dist.is_zero()
            && pos.x() != ctx.bounds.tiles_tl.x()
            && pos.y() != ctx.bounds.tiles_tl.y()
        {
            return;
        }
        let circ_id = unwrap_option_or_return!(node.circuit.get());

        let board = self.board.read().unwrap();
        let circuit = unwrap_option_or_return!(board.circuits.get(circ_id));

        let circ_info = circuit.info.read().unwrap();

        let pos = pos - node.origin_dist.convert(|v| v as i32);
        let screen_pos = ctx.screen.world_to_screen_tile(pos);
        let screen_size = circ_info.size.convert(|v| v as f32) * ctx.screen.scale;
        let rect = Rect::from_min_size(screen_pos.into(), screen_size.into());
        let circ_ctx = ctx.with_rect(rect);

        let imp = circuit.imp.read().unwrap();
        let state_ctx = CircuitStateContext::new(&self.state, circuit);

        if imp.draw_pin_points() {
            for pin in circ_info.pins.iter() {
                let pos = circuit.pos + pin.pos.convert(|v| v as i32);
                let pin = pin.pin.read().unwrap();
                if pin.connected_wire().is_some() {
                    continue;
                }
                let color = pin.get_state(state_ctx.global_state).color();
                let pos = circ_ctx.screen.world_to_screen_tile(pos) + circ_ctx.screen.scale / 2.0;
                circ_ctx
                    .paint
                    .circle_filled(pos.into(), circ_ctx.screen.scale * 0.1, color);
            }
        }

        imp.draw(&state_ctx, &circ_ctx);
    }

    /* #endregion */

    /* #region Updating */

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
                if let Some(found) = self.find_wire_node(mouse_pos, dir) {
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

    /* #endregion */

    /* #region Wire drawing helpers */

    fn draw_wire_part(&self, ctx: &PaintContext, part: &WirePart, color: Color32) {
        let screen = &ctx.screen;
        let rect = Self::calc_wire_part_rect(screen, part);
        ctx.paint.rect_filled(rect, Rounding::none(), color);

        self.wires_drawn.fetch_add(1, Ordering::Relaxed);
    }

    fn calc_wire_point_rect(screen: &Screen, pos: Vec2i) -> Rect {
        let thickness = screen.scale * Self::WIRE_POINT_THICKNESS;

        let rect_pos = screen.world_to_screen_tile(pos) + ((screen.scale - thickness) * 0.5);

        Rect::from_min_size(rect_pos.into(), vec2(thickness, thickness))
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

    /* #endregion */

    /* #region Wire manipulations */

    fn place_wire_part(&mut self, part: WirePart) {
        let part = unwrap_option_or_return!(self.optimize_wire_part(part));

        let wires_crossed = {
            let mut wires_crossed = HashSet::new();
            for (i, pos) in part.iter_pos(true).enumerate() {
                let node =
                    unwrap_option_or_continue!(self.wire_nodes.get(pos.convert(|v| v as isize)));

                if i == 0 || i as u32 == part.length.get() {
                    match self.wires_at(pos) {
                        TileWires::None => {}
                        TileWires::One { wire, vertical: _ } => {
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
                            left: _,
                            up: _,
                            right: _,
                            down: _,
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

        self.remove_useless_intersection(part.pos, false);
        self.remove_useless_intersection(
            part.dir
                .move_vector(part.pos, part.length.get() as i32, true),
            false,
        );

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

        let mut dist = 1;
        for (i, pos) in part.iter_pos(true).enumerate() {
            let node = self
                .wire_nodes
                .get_or_create_mut(pos.convert(|v| v as isize));

            let point = node.wire.is_some() || i == 0 || i as u32 == part.length.get();

            if i > 0 {
                node.get_dir_mut(dir_rev).set(Some(dist))
            }

            if point {
                node.wire.set(Some(wire));
                if i > 0 {
                    fix_pointers(self, pos, dist, dir_rev);
                }
                dist = 1;
            }
            else {
                dist += 1;
            }
        }

        for (i, pos) in part.iter_pos(true).enumerate() {
            let node = self.wire_nodes.get(pos.convert(|v| v as isize));
            let node = unwrap_option_or_continue!(node);

            let point = node.wire.is_some() || i == 0 || i as u32 == part.length.get();
            if point {
                let pin = self.pin_at(pos);

                let mut board = self.board.write().unwrap();
                let states = board.states.clone();
                if let Some(wire) = board.wires.get_mut(wire) {
                    wire.set_point(
                        pos,
                        &states,
                        Some(WirePoint {
                            left: node.left.is_some(),
                            up: node.up.is_some(),
                            pin,
                        }),
                        false,
                    )
                }
            }
        }
    }

    /// parameter `ptr_start`: None to remove pointers, Some(0) indicates that node at `pos` is an intersection, other values don't
    fn set_wire_pointers(
        &mut self,
        pos: Vec2i,
        dir: Direction4,

        ptr_start: Option<u32>,
        dist: u32,
    ) {
        if dist == 0 {
            return;
        }

        let start_dist = ptr_start.unwrap_or(0);
        let remove = ptr_start.is_none();

        let back_dir = dir.inverted();

        for (i, pos) in dir.iter_pos_along(pos, dist as i32, true).enumerate() {
            let forward_ptr = if remove { None } else { Some(dist - i as u32) };
            let back_ptr = if remove {
                None
            } else {
                Some(i as u32 + start_dist)
            };

            let node = self
                .wire_nodes
                .get_or_create_mut(pos.convert(|v| v as isize));
            if i > 0 {
                node.get_dir_mut(back_dir).set(back_ptr);
            }
            if (i as u32) < dist {
                node.get_dir_mut(dir).set(forward_ptr);
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
            let dist_forward = unwrap_option_or_continue!(node.get_dir(dir).get());

            if wire.is_some() {
                self.set_wire_pointers(pos, dir, Some(0), dist_forward);
            } else {
                let dist_back = node.get_dir(dir.inverted()).get();
                self.set_wire_pointers(pos, dir, dist_back, dist_forward);
            }
        }

        {
            let pin = self.pin_at(pos);
            if let Some(pin) = &pin {
                pin.write().unwrap().set_wire(
                    &self.board.read().unwrap().states.clone(),
                    wire,
                    false,
                    true,
                );
            }

            let mut board = self.board.write().unwrap();
            let states = board.states.clone();

            if let Some(wire) = prev_wire.and_then(|w| board.wires.get_mut(w)) {
                wire.set_point(pos, &states, None, false);
            }

            if let Some(wire) = wire.and_then(|w| board.wires.get_mut(w)) {
                wire.set_point(
                    pos,
                    &states,
                    Some(WirePoint {
                        left: node.left.is_some(),
                        up: node.up.is_some(),
                        pin,
                    }),
                    false,
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
            self.remove_intersection_at_node(pos, *node, true, true);
        } else {
            self.create_wire_intersection_at_node(pos, *node);
        }
    }

    pub fn create_wire_intersection(&mut self, pos: Vec2i) -> Option<usize> {
        let node = self.wire_nodes.get(pos.convert(|v| v as isize));
        let node = match node {
            Some(v) => *v,
            None => return None,
        };
        self.create_wire_intersection_at_node(pos, node)
    }

    // wire param: if Some(w) will use wire w, if None, will try to figure ot which wire to use (with merging wires)
    pub fn create_wire_intersection_at_node(
        &mut self,
        pos: Vec2i,
        node: WireNode,
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

                wire.map(|w| {
                    self.set_wire_point(pos, Some(w), true);
                    w
                })
            }
        }
    }

    #[allow(unused)]
    fn remove_intersection(&mut self, pos: Vec2i, split: bool, update_states: bool) {
        if let Some(node) = self.wire_nodes.get(pos.convert(|v| v as isize)) {
            self.remove_intersection_at_node(pos, *node, split, update_states)
        }
    }

    // split param: if intersection was of 2 wires, split them
    fn remove_intersection_at_node(
        &mut self,
        pos: Vec2i,
        node: WireNode,
        split: bool,
        update_states: bool,
    ) {
        let (wire, int4, int_none) = match self.wires_at_node(pos, &node) {
            TileWires::None => return,
            TileWires::One {
                wire: _,
                vertical: _,
            } => return,
            TileWires::Two {
                horizontal: _,
                vertical: _,
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
                (
                    wire,
                    left && up && right && down,
                    !(left || up || right || down),
                )
            }
        };

        if !int_none
            && self
                .board
                .read()
                .unwrap()
                .wires
                .get(wire)
                .is_some_and(|w| w.points.get(&pos).is_some_and(|p| p.pin.is_some()))
        {
            return;
        }

        if !split && int4 {
            return;
        }

        let split = split && int4;

        let prev = self.set_wire_point(pos, None, !split && update_states);

        if split {
            if let Some(wire) = prev {
                self.split_wires(wire, update_states);
            }
        }
    }

    fn remove_useless_intersection(&mut self, pos: Vec2i, update_states: bool) {
        let node = self.wire_nodes.get(pos.convert(|v| v as isize));
        let node = *unwrap_option_or_return!(node);

        if node.wire.is_none() {
            return;
        }

        let up = node.up.is_some();
        let left = node.left.is_some();
        let right = node.right.is_some();
        let down = node.down.is_some();

        // 4-way intersections aren't useless
        if up && left && right && down {
            return;
        }

        if up == down && right == left {
            self.remove_intersection_at_node(pos, node, false, update_states);
        }
    }

    fn remove_wire_part(
        &mut self,
        pos: Vec2i,
        dir: Direction4,
        split: bool,
        update_states: bool,
    ) -> Option<usize> {
        let node = *self.wire_nodes.get(pos.convert(|v| v as isize))?;
        let wire = node.wire.get()?;

        let target = self.find_wire_node_from_node(&node, pos, dir)?;

        let (wp_dir, wp_dir_forward) = dir.into_dir2();
        let wp_pos = match wp_dir_forward {
            true => pos,
            false => target.pos,
        };

        {
            let mut board = self.board.write().unwrap();
            let wire = board.wires.get_mut(wire)?;
            let point = wire.points.get_mut(&wp_pos)?;

            match wp_dir {
                Direction2::Up => point.up = false,
                Direction2::Left => point.left = false,
            }
        }

        self.set_wire_pointers(pos, dir, None, target.distance.get());

        self.remove_useless_intersection(pos, false);
        self.remove_useless_intersection(target.pos, false);

        if split {
            self.split_wires(wire, update_states);
        } else if update_states {
            let states = self.board.read().unwrap().states.clone();
            states.update_wire(wire);
        }

        Some(wire)
    }

    fn merge_wires(&mut self, wire: usize, with: usize, update_state: bool) {
        {
            let board = self.board.read().unwrap();

            let with_wire = unwrap_option_or_return!(board.wires.get(with));

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

    fn split_wires(&mut self, id: usize, update_states: bool) {
        let mut groups = vec![];

        let mut remaining_nodes: HashSet<_> = {
            let board = self.board.read().unwrap();
            let wire = unwrap_option_or_return!(board.wires.get(id));
            wire.points.keys().copied().collect()
        };

        // empty wire
        if remaining_nodes.is_empty() {
            let mut board = self.board.write().unwrap();
            board.wires.remove(id);
            board.states.reset_wire(id);
        }

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

    fn set_node_wires<'a>(&mut self, positions: impl Iterator<Item = &'a Vec2i>, wire: usize) {
        for npos in positions {
            match self.wire_nodes.get_mut(npos.convert(|v| v as isize)) {
                None => {}
                Some(n) => n.wire.set(Some(wire)),
            }
        }
    }

    /* #endregion */

    /* #region Wire node querying */

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

    fn find_wire_node(&self, pos: Vec2i, dir: Direction4) -> Option<FoundWireNode> {
        let node = self.wire_nodes.get(pos.convert(|v| v as isize))?;
        self.find_wire_node_from_node(node, pos, dir)
    }

    fn find_wire_node_from_node(
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
            if let Some(f) = self.find_wire_node_from_node(node, pos, dir) {
                if !wire.is_some_and(|v| v != f.wire) {
                    arr[arrpos] = f.pos;
                    arrpos += 1;
                }
            }
        }
        (arr, arrpos)
    }

    /* #endregion */

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

    fn should_draw_wire_point(&self, pos: Vec2i, shift: bool) -> bool {
        let node =
            unwrap_option_or_return!(self.wire_nodes.get(pos.convert(|v| v as isize)), false);

        if node.wire.is_none() {
            return false;
        }

        if shift {
            return true;
        }

        Direction4::iter_all().all(|dir| node.get_dir(dir).is_some())
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
            if let Some(wire) = self.create_wire_intersection(pos) {
                let wire = if let Some(wire) = self.board.write().unwrap().wires.get_mut(wire) {
                    if let Some(p) = wire.points.get_mut(&pos) {
                        p.pin = Some(pin.pin.clone());
                    }
                    Some(wire.id)
                } else {
                    None
                };

                if let Some(wire) = wire {
                    pin.pin.write().unwrap().set_wire(
                        &self.board.read().unwrap().states.clone(),
                        Some(wire),
                        true,
                        false,
                    );
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

    fn optimize_wire_part(&self, part: WirePart) -> Option<WirePart> {
        let mut part_pos = part.pos;
        let mut part_len = part.length.get() as i32;

        let new_start = self
            .wire_nodes
            .get(part_pos.convert(|v| v as isize))
            .and_then(|n| {
                if n.wire.is_some() {
                    None
                } else {
                    self.find_wire_node_from_node(n, part_pos, part.dir.into())
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
                self.find_wire_node_from_node(n, end_pos, Direction4::from(part.dir).inverted())
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

    fn remove_circuit(&mut self, id: usize, affected_wires: &mut HashSet<usize>) {
        let board = self.board.read().unwrap();
        let circuit = unwrap_option_or_return!(board.circuits.get(id));

        let pos = circuit.pos;
        let circuit_info = circuit.info.read().unwrap();
        let size = circuit_info.size;
        let pins: Vec<_> = circuit
            .info
            .read()
            .unwrap()
            .pins
            .iter()
            .map(|p| (p.pos.convert(|v| v as i32) + pos, p.pin.clone()))
            .collect();

        drop(circuit_info);
        drop(board);

        for y in pos.y()..(pos.y() + size.y() as i32) {
            for x in pos.x()..(pos.x() + size.x() as i32) {
                let node = self.circuit_nodes.get_mut([x as isize, y as isize]);
                let node = unwrap_option_or_continue!(node);

                if node.circuit.get() != Some(id) {
                    continue;
                }

                node.circuit.set(None);
                node.origin_dist = Default::default();
            }
        }

        let mut board = self.board.write().unwrap();

        for (pos, pin) in pins {
            let wire_id = pin.read().unwrap().connected_wire();
            let wire_id = unwrap_option_or_continue!(wire_id);
            let wire = unwrap_option_or_continue!(board.wires.get_mut(wire_id));
            let point = unwrap_option_or_continue!(wire.points.get_mut(&pos));
            point.pin = None;

            affected_wires.insert(wire_id);
        }

        board.circuits.remove(id);
        board.states.reset_circuit(id);
    }
}
