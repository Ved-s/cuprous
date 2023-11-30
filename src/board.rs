use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    f32::consts::TAU,
    num::NonZeroU32,
    ops::{Deref, Not},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};

use bimap::BiMap;
use eframe::{
    egui::{self, FontSelection, Id, Painter, Sense, TextStyle, WidgetText},
    epaint::{Color32, FontId, RectShape, Rounding, Shape, Stroke, TextShape},
};
use emath::{pos2, vec2, Align2, Pos2, Rect, Vec2};
use serde::{Deserialize, Serialize};
use serde_intermediate::Intermediate;

use crate::{
    app::SimulationContext,
    circuits::{
        props::{CircuitPropertyImpl, CircuitPropertyStore},
        Circuit, CircuitNode, CircuitPin, CircuitPinId, CircuitPreview, CircuitStateContext,
        InternalPinDirection,
    },
    containers::{Chunks2D, ChunksLookaround, FixedVec},
    random_u128,
    state::{State, StateCollection, WireState},
    ui::{
        designer::DesignProvider,
        editor::TileDrawBounds,
        selection::{
            selection_border_color, selection_fill_color, Selection, SelectionImpl, SelectionMode,
        },
        RectVisuals,
    },
    unwrap_option_or_continue, unwrap_option_or_return,
    vector::{IsZero, Vec2f, Vec2i, Vec2u},
    wires::{FoundWireNode, TileWires, Wire, WireNode, WirePart, WirePoint},
    ArcString, Direction2, Direction4, DynStaticStr, PaintContext, PastePreview, RwLock, Screen,
};

pub struct StoredCircuitBoard {
    pub board: Arc<CircuitBoard>,
    pub preview: Option<Arc<CircuitPreview>>,
}

impl StoredCircuitBoard {
    pub fn new(board: Arc<CircuitBoard>) -> Self {
        Self {
            board,
            preview: None,
        }
    }
}

pub struct CircuitBoard {
    pub name: RwLock<ArcString>,
    pub uid: u128,

    pub wires: RwLock<FixedVec<Wire>>,
    pub circuits: RwLock<FixedVec<Arc<Circuit>>>,
    pub states: Arc<StateCollection>,

    pub designs: Arc<RwLock<CircuitDesignStorage>>,
    pub pins: RwLock<BiMap<Arc<str>, usize>>,
    pub controls: RwLock<HashSet<usize>>,
    pub ctx: Arc<SimulationContext>,

    // RwLock for blocking simulation while modifying board
    pub sim_lock: Arc<RwLock<()>>,
    ordered_queue: AtomicBool,
    pub single_outer_control: AtomicBool,
}

impl CircuitBoard {
    pub fn new(ctx: Arc<SimulationContext>, name: &str) -> Self {
        Self {
            uid: random_u128(),
            name: RwLock::new(name.into()),
            wires: RwLock::new(vec![].into()),
            circuits: RwLock::new(vec![].into()),
            states: Arc::new(StateCollection::new()),
            sim_lock: Default::default(),
            ordered_queue: AtomicBool::new(false),
            designs: Arc::new(RwLock::new(CircuitDesignStorage::new(
                CircuitDesign::default_board_design(),
            ))),
            pins: Default::default(),
            controls: Default::default(),
            single_outer_control: AtomicBool::new(false),
            ctx,
        }
    }

    pub fn create_circuit(
        self: &Arc<Self>,
        pos: Vec2i,
        preview: &CircuitPreview,
        props_override: Option<CircuitPropertyStore>,
        imp_data: Option<&Intermediate>,
        process_formatting: bool,
    ) -> usize {
        let circuits = self.circuits.read();
        let id = circuits.first_free_pos();
        let circ = Circuit::create(
            id,
            pos,
            preview,
            self.clone(),
            props_override,
            imp_data,
            process_formatting,
        );

        drop(circuits);
        self.circuits.write().set(circ, id);
        id
    }

    pub fn create_wire(&self) -> usize {
        let mut wires = self.wires.write();
        let id = wires.first_free_pos();
        let wire = Wire {
            id,
            points: HashMap::default(),
        };
        wires.set(wire, id);
        id
    }

    pub fn merge_wires(&self, id: usize, with: usize, update_states: bool) {
        let mut wires = self.wires.write();

        if !wires.exists(id) {
            return;
        }

        self.states.reset_wire(with);
        let with = unwrap_option_or_return!(wires.remove(with));

        let Wire { id: _, points } = with;

        for point in points.values() {
            if let Some(pin) = &point.pin {
                pin.write().set_wire(&self.states, Some(id), false, true);
            }
        }

        let wire = wires.get_mut(id).unwrap();
        for point in points {
            wire.points.insert(point.0, point.1);
        }

        if update_states {
            self.states.update_wire(id, true);
        }
    }

    pub fn split_wire(
        &self,
        id: usize,
        points: &HashSet<Vec2i>,
        update_states: bool,
    ) -> Option<usize> {
        let mut wires = self.wires.write();

        let new_wire_id = wires.first_free_pos();
        let wire = unwrap_option_or_return!(wires.get_mut(id), None);

        let point_positions: Vec<_> = wire.points.keys().cloned().collect();

        for pos in point_positions {
            // sometimes I hate rust borrow checker
            if let Some(point_dirs) = wire.points.get(&pos).map(|p| [p.up, p.left]) {
                let iter = [Direction2::Up, Direction2::Left]
                    .into_iter()
                    .enumerate()
                    .filter_map(|(i, v)| point_dirs[i].then_some(v));
                for dir in iter {
                    if let Some(found) = wire.search_wire_point(pos, dir.into()) {
                        if points.contains(&found.pos) != points.contains(&pos) {
                            *wire
                                .points
                                .get_mut(&pos)
                                .expect("unreachable")
                                .get_dir_mut(dir) = false;
                        }
                    }
                }
            }
        }

        let mut new_points = HashMap::new();
        for pos in points.iter() {
            let mut point = unwrap_option_or_continue!(wire.points.remove(pos));

            point.pin = point.pin.map(|p| {
                p.write()
                    .set_wire(&self.states, Some(new_wire_id), false, true);
                p
            });

            new_points.insert(*pos, point);
        }

        let new_wire = Wire {
            id: new_wire_id,
            points: new_points,
        };
        wires.set(new_wire, new_wire_id);

        if update_states {
            self.states.update_wire(id, true);
            self.states.update_wire(new_wire_id, true);
        }

        Some(new_wire_id)
    }

    pub fn save(&self, sim_lock: bool) -> crate::io::CircuitBoardData {
        let sim_lock = sim_lock.then(|| self.sim_lock.write());
        let data = crate::io::CircuitBoardData {
            name: self.name.read().get_str().deref().into(),
            uid: self.uid,
            wires: self
                .wires
                .read()
                .inner()
                .iter()
                .map(|w| w.as_ref().map(|w| w.save()))
                .collect(),
            circuits: self
                .circuits
                .read()
                .inner()
                .iter()
                .map(|c| c.as_ref().map(|c| c.save()))
                .collect(),
            states: self
                .states
                .states
                .read()
                .inner()
                .iter()
                .map(|s| s.as_ref().and_then(|s| s.is_being_used().then(|| s.save())))
                .collect(),
            ordered: self.ordered_queue.load(Ordering::Relaxed),
            designs: self.designs.read().save(),
            single_outer_control: self.single_outer_control.load(Ordering::Relaxed),
        };
        drop(sim_lock);
        data
    }

    pub fn load(data: &crate::io::CircuitBoardData, ctx: &Arc<SimulationContext>) -> Arc<Self> {
        let designs = Arc::new(RwLock::new(CircuitDesignStorage::load(&data.designs)));

        let board = Arc::new(CircuitBoard {
            name: RwLock::new(data.name.as_str().into()),
            uid: data.uid,
            wires: RwLock::new(vec![].into()),
            circuits: RwLock::new(vec![].into()),
            states: Arc::new(StateCollection::new()),
            sim_lock: Default::default(),
            ordered_queue: AtomicBool::new(data.ordered),
            designs,
            pins: Default::default(),
            controls: Default::default(),
            single_outer_control: AtomicBool::new(data.single_outer_control),
            ctx: ctx.clone(),
        });

        let mut circuits = board.circuits.write();

        for (i, c) in data.circuits.iter().enumerate() {
            let c = unwrap_option_or_continue!(c);

            let preview = ctx.previews.get(&c.ty);
            let preview = unwrap_option_or_continue!(preview); // TODO: Errors
            let props = preview.imp.default_props();
            props.load(&c.props);

            let data = matches!(c.imp, serde_intermediate::Intermediate::Unit)
                .not()
                .then_some(&c.imp);

            let circ = Circuit::create(i, c.pos, preview, board.clone(), Some(props), data, false);
            {
                let mut info = circ.info.write();
                for (pin_name, wire) in c.pin_wires.iter() {
                    if let Some(info) = info.pins.iter_mut().find(|i| i.name == *pin_name) {
                        info.pin.write().wire = Some(*wire);
                    }
                }
            }
            circuits.set(circ, i);
        }

        let mut wires = board.wires.write();

        for (i, w) in data.wires.iter().enumerate() {
            let w = unwrap_option_or_continue!(w);
            let wire = Wire::load(w, i, &circuits);
            wires.set(wire, i);
        }

        drop((wires, circuits));

        let mut states = board.states.states.write();
        for (i, s) in data.states.iter().enumerate() {
            let s = unwrap_option_or_continue!(s);
            states.set(State::load(s, board.clone(), i), i);
        }
        drop(states);

        board
    }

    pub fn is_ordered_queue(&self) -> bool {
        self.ordered_queue.load(Ordering::Relaxed)
    }

    pub fn set_ordered_queue(&self, ordered: bool, lock_sim: bool) {
        let sim_lock = lock_sim.then(|| self.sim_lock.write());
        self.states.set_ordered(ordered);
        self.ordered_queue.store(ordered, Ordering::Relaxed);
        drop(sim_lock);
    }

    // Run board simulation after loading. Not required on newly created or empty boards
    pub fn activate(&self) {
        let circuits = self.circuits.read();
        for circuit in circuits.iter() {
            circuit.initialize(false);
        }
        drop(circuits);

        self.states.initialize();
    }
}

#[derive(Clone)]
pub enum SelectedItem {
    Selection,
    Wire,
    Circuit(Arc<CircuitPreview>),
    Paste(Arc<PastePreview>),
}

// TODO: Move all drawing to the editor
pub struct ActiveCircuitBoard {
    pub board: Arc<CircuitBoard>,
    pub state: Arc<State>,

    pub wire_nodes: Chunks2D<16, WireNode>,
    pub circuit_nodes: Chunks2D<16, CircuitNode>,

    wire_drag_pos: Option<Vec2i>,
    pub selection: RefCell<Selection<BoardObjectSelectionImpl>>,
}

impl ActiveCircuitBoard {
    pub const WIRE_THICKNESS: f32 = 0.2;
    pub const WIRE_POINT_THICKNESS: f32 = 0.35;

    pub fn new_main(board: Arc<CircuitBoard>) -> Self {
        Self::new(board.clone().states.get_or_create_main(board))
    }

    pub fn new(state: Arc<State>) -> Self {
        let board = state.board.clone();
        let (wires, circuits) = {
            let mut wires = Chunks2D::<16, WireNode>::default();
            let mut circuits = Chunks2D::<16, CircuitNode>::default();

            for wire in board.wires.read().iter() {
                let id = wire.id;
                for (pos, point) in wire.points.iter() {
                    wires
                        .get_or_create_mut(pos.convert(|v| v as isize))
                        .wire
                        .set(Some(id));
                    for dir in Direction2::iter_all() {
                        if let Some(point) = point
                            .get_dir(dir)
                            .then(|| wire.search_wire_point(*pos, dir.into()))
                            .flatten()
                        {
                            for (i, pos) in dir
                                .iter_pos_along(*pos, point.dist.get() as i32, true)
                                .enumerate()
                            {
                                let node = wires.get_or_create_mut(pos.convert(|v| v as isize));

                                let start = i == 0;
                                let end = i == point.dist.get() as usize;

                                if !start {
                                    node.get_dir_mut(Direction4::from(dir).inverted())
                                        .set(Some(i as u32));
                                }
                                if !end {
                                    node.get_dir_mut(Direction4::from(dir))
                                        .set(Some(point.dist.get() - i as u32));
                                }
                            }
                        }
                    }
                }
            }

            for circuit in board.circuits.read().iter() {
                let id = circuit.id;
                let info = circuit.info.read();

                for y in 0..info.size.y() {
                    for x in 0..info.size.x() {
                        let dist = Vec2u::from([x, y]);
                        let pos = circuit.pos + dist.convert(|v| v as i32);
                        let node = circuits.get_or_create_mut(pos.convert(|v| v as isize));
                        node.circuit.set(Some(id));
                        node.origin_dist = dist;
                    }
                }
            }

            (wires, circuits)
        };

        Self {
            board,
            wire_nodes: wires,
            circuit_nodes: circuits,
            state,
            wire_drag_pos: None,
            selection: RefCell::new(Selection::default()),
        }
    }

    pub fn update(
        &mut self,
        ctx: &PaintContext,
        bounds: TileDrawBounds,
        selected: Option<SelectedItem>,
        debug: bool,
    ) {
        self.selection.borrow_mut().pre_update_selection(
            self,
            ctx,
            matches!(&selected, Some(SelectedItem::Selection)),
        );

        let selected_something = !self.selection.borrow().selection.is_empty();

        if selected_something && !ctx.ui.ctx().wants_keyboard_input() {
            cfg_if::cfg_if! {
                if #[cfg(all(not(web_sys_unstable_apis), feature = "wasm"))] {
                    let copy_request = ctx.ui.input(|input| {
                        input.modifiers.ctrl
                            && (input.key_pressed(egui::Key::C) || input.key_pressed(egui::Key::X))
                    });
                } else {
                    let copy_request = ctx.ui.input(|input| {
                        input
                            .events
                            .iter()
                            .any(|e| matches!(e, egui::Event::Copy | egui::Event::Cut))
                    });
                }
            }

            if copy_request {
                let selection = self.selection.borrow();

                let min_pos = {
                    let mut min_pos = None;
                    for obj in selection.selection.iter() {
                        let pos = match obj {
                            SelectedBoardObject::WirePart { pos, dir } => {
                                let node = self.find_wire_node(*pos, (*dir).into());
                                let node = unwrap_option_or_continue!(node);
                                node.pos
                            }
                            SelectedBoardObject::Circuit { id } => {
                                let pos = self.board.circuits.read().get(*id).map(|c| c.pos);
                                unwrap_option_or_continue!(pos)
                            }
                        };
                        min_pos = match min_pos {
                            None => Some(pos),
                            Some(mp) => Some([mp.x().min(pos.x()), mp.y().min(pos.y())].into()),
                        }
                    }
                    min_pos
                };
                if let Some(min_pos) = min_pos {
                    let mut copy = crate::io::CopyPasteData::default();
                    for obj in selection.selection.iter() {
                        match obj {
                            SelectedBoardObject::WirePart { pos, dir } => {
                                if let Some(w) = self.find_wire_node(*pos, (*dir).into()) {
                                    copy.wires.push(crate::io::WirePartCopyData {
                                        pos: (*pos - min_pos).convert(|v| v as u32),
                                        length: w.distance.get(),
                                        dir: *dir,
                                    })
                                }
                            }
                            SelectedBoardObject::Circuit { id } => {
                                let circuits = self.board.circuits.read();
                                if let Some(circuit) = circuits.get(*id) {
                                    let pos = (circuit.pos - min_pos).convert(|v| v as u32);
                                    copy.circuits.push(circuit.copy(pos, self.state.as_ref()))
                                }
                            }
                        }
                    }
                    cfg_if::cfg_if! {
                        if #[cfg(all(not(web_sys_unstable_apis), feature = "wasm"))] {
                            *crate::io::GLOBAL_CLIPBOARD.lock() = Some(copy);
                        } else {
                            let copy_text = ron::to_string(&copy).unwrap();
                            ctx.ui.output_mut(|output| output.copied_text = copy_text);
                        }
                    }
                }
            }

            cfg_if::cfg_if! {
                if #[cfg(all(not(web_sys_unstable_apis), feature = "wasm"))] {
                    let delete_request = ctx
                        .ui
                        .input(|input| input.modifiers.ctrl && input.key_pressed(egui::Key::X)) ;
                } else {
                    let delete_request = ctx
                        .ui
                        .input(|input| input.events.iter().any(|e| matches!(e, egui::Event::Cut)));
                }
            }

            if delete_request || ctx.ui.input(|input| input.key_pressed(egui::Key::Delete)) {
                let mut affected_wires = HashSet::new();
                let drain = {
                    let mut selection = self.selection.borrow_mut();
                    selection.selection.drain().collect::<Vec<_>>()
                };
                let sim_lock = { self.board.sim_lock.clone() };
                let sim_lock = sim_lock.write();
                for obj in drain {
                    match obj {
                        SelectedBoardObject::Circuit { id } => {
                            self.remove_circuit(id, &mut affected_wires);
                        }
                        SelectedBoardObject::WirePart { pos, dir } => {
                            if let Some(wire) = self.remove_wire_part(pos, dir.into(), true, false)
                            {
                                affected_wires.insert(wire);
                            }
                        }
                    }
                }

                for wire in affected_wires {
                    self.board.states.update_wire(wire, true);
                }
                drop(sim_lock)
            }
        }

        ctx.draw_chunks(
            bounds,
            &self.wire_nodes,
            &self,
            |node| !node.is_empty(),
            |node, pos, ctx, this, lookaround| {
                this.draw_wire_node(bounds, ctx, node, pos, lookaround, debug);
            },
        );

        if debug {
            ctx.draw_chunks(
                bounds,
                &self.wire_nodes,
                &self,
                |node| !node.is_empty(),
                |node, pos, ctx, this, lookaround| {
                    this.draw_wire_node_debug(ctx, node, pos, lookaround);
                },
            );
        }

        ctx.draw_chunks(
            bounds,
            &self.circuit_nodes,
            &*self,
            |n| n.circuit.is_some(),
            |node, pos, ctx, this, _| this.draw_circuit_node(bounds, node, pos, ctx),
        );

        self.update_wires(ctx, matches!(&selected, Some(SelectedItem::Wire)));

        self.draw_hovered_circuit_pin_names(ctx);

        self.update_previews(ctx, selected);
        self.selection.borrow_mut().update_selection(self, ctx);
    }

    fn draw_hovered_circuit_pin_names(&self, ctx: &PaintContext) {
        let mouse_tile_pos = ctx
            .ui
            .input(|input| input.pointer.interact_pos())
            .map(|p| ctx.screen.screen_to_world(Vec2f::from(p)));

        let mouse_tile_pos = unwrap_option_or_return!(mouse_tile_pos);
        let mouse_tile_pos = mouse_tile_pos.convert(|v| v.floor() as isize);
        let node = self.circuit_nodes.get(mouse_tile_pos);
        let node = unwrap_option_or_return!(node);
        let circuit = unwrap_option_or_return!(node.circuit.get());

        let pos = mouse_tile_pos - node.origin_dist.convert(|v| v as isize);
        let info = self
            .board
            .circuits
            .read()
            .get(circuit)
            .map(|c| c.info.clone());
        let info = unwrap_option_or_return!(info);

        let info = info.read();
        crate::ui::drawing::draw_pin_names(
            pos,
            info.pins
                .iter()
                .map(|pin| (pin.pos, pin.display_name.deref(), pin.display_dir)),
            0.5,
            0.5,
            ctx,
        );
    }

    /* #region Drawing nodes */

    fn draw_wire_node(
        &self,
        bounds: TileDrawBounds,
        ctx: &PaintContext<'_>,
        node: &WireNode,
        pos: Vec2i,
        lookaround: &ChunksLookaround<'_, 16, WireNode>,
        debug: bool,
    ) {
        struct WireDrawInfo {
            dist: Option<u32>,
            next_dist: Option<u32>,
            wire: Option<usize>,
            dir: Direction2,
            pos: Vec2i,
            color: Color32,
        }

        fn draw_wire(
            bounds: TileDrawBounds,
            info: WireDrawInfo,
            this: &ActiveCircuitBoard,
            ctx: &PaintContext,
        ) {
            if info.dist.is_none() && info.wire.is_none() {
                return;
            }

            let edge = match info.dir {
                Direction2::Up => info.pos.y() == bounds.tiles_br.y(),
                Direction2::Left => info.pos.x() == bounds.tiles_br.x(),
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

        let center_color = node.wire.get().map(|w| self.state.get_wire(w).color());

        for dir in Direction2::iter_all() {
            let wire_color = center_color.or_else(|| {
                wires
                    .dir(dir.into())
                    .map(|w| self.state.get_wire(w).color())
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

            draw_wire(bounds, draw, self, ctx);
        }

        if let Some(wire) = node.wire.get() {
            let possible_intersection = if ctx.ui.input(|input| input.modifiers.shift) {
                true
            } else {
                Direction4::iter_all().all(|dir| node.get_dir(dir).is_some())
            };

            if possible_intersection {
                let wires = self.board.wires.read();
                if let Some(wire) = wires.get(wire) {
                    Self::draw_wire_point(
                        ctx,
                        pos,
                        wire.color(&self.state),
                        debug && wire.points.get(&pos).is_some_and(|p| p.pin.is_some()),
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
            let wires = self.board.wires.read();
            let wire = wires.get(wire_id);

            if let Some(wire) = wire {
                match wire.points.get(&pos) {
                    None => {
                        let pos = ctx.screen.world_to_screen(pos.convert(|v| v as f32));
                        let size = [ctx.screen.scale, ctx.screen.scale];
                        let rect = Rect::from_min_size(pos.into(), size.into());
                        ctx.paint
                            .rect_stroke(rect, Rounding::ZERO, Stroke::new(2.0, Color32::RED));
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

    fn draw_circuit_node(
        &self,
        bounds: TileDrawBounds,
        node: &CircuitNode,
        pos: Vec2i,
        ctx: &PaintContext,
    ) {
        if !(node.origin_dist.is_zero()
            || pos.x() == bounds.tiles_tl.x() && pos.y() == bounds.tiles_tl.y()
            || node.origin_dist.y() == 0 && pos.x() == bounds.tiles_tl.x()
            || node.origin_dist.x() == 0 && pos.y() == bounds.tiles_tl.y())
        {
            return;
        }
        let circ_id = unwrap_option_or_return!(node.circuit.get());

        let circuits = self.board.circuits.read();
        let circuit = unwrap_option_or_return!(circuits.get(circ_id));

        let circ_info = circuit.info.read();

        let pos = pos - node.origin_dist.convert(|v| v as i32);
        let screen_pos = ctx.screen.world_to_screen_tile(pos);
        let screen_size = circ_info.size.convert(|v| v as f32) * ctx.screen.scale;
        let rect = Rect::from_min_size(screen_pos.into(), screen_size.into());
        let circ_ctx = ctx.with_rect(rect);

        let imp = circuit.imp.read();
        let state_ctx = CircuitStateContext::new(self.state.clone(), circuit.clone());

        if imp.draw_pin_points() {
            for pin in circ_info.pins.iter() {
                let pos = circuit.pos + pin.pos.convert(|v| v as i32);
                let pin = pin.pin.read();
                if pin.connected_wire().is_some() {
                    continue;
                }
                let color = pin.get_state(&state_ctx.global_state).color();
                let pos = circ_ctx.screen.world_to_screen_tile(pos) + circ_ctx.screen.scale / 2.0;
                circ_ctx.paint.circle_filled(
                    pos.into(),
                    circ_ctx.screen.scale * Self::WIRE_THICKNESS * 0.5,
                    color,
                );
            }
        }

        imp.draw(&state_ctx, &circ_ctx);

        if let Some(controls) = imp.control_count(circuit) {
            let posf = Vec2::from(circuit.pos.convert(|v| v as f32));
            for i in 0..controls {
                let info = imp.control_info(circuit, i);
                let info = unwrap_option_or_continue!(info);
                let rect = Rect {
                    min: info.rect.min + posf,
                    max: info.rect.max + posf,
                };

                let ctx = ctx.with_rect(ctx.screen.world_to_screen_rect(rect));
                imp.update_control(
                    i,
                    circuit,
                    Some(&state_ctx),
                    &ctx,
                    true,
                    Id::new((self.board.uid, circuit.pos, i)),
                );
            }
        }

        let name = circuit.props.read("name", |s: &ArcString| s.get_arc());
        let label_dir = circuit.props.read_clone::<Direction4>("label_dir");

        if let (Some(name), Some(label_dir)) = (name, label_dir) {
            if !name.is_empty() {
                let offset = 0.5 * ctx.screen.scale;
                let galley = WidgetText::from(name.deref()).into_galley(
                    ctx.ui,
                    Some(false),
                    f32::INFINITY,
                    FontSelection::FontId(FontId::monospace(ctx.screen.scale * 0.7)),
                );

                let textsize = galley.size();
                let pos = match label_dir {
                    Direction4::Up => {
                        rect.center_top() + vec2(-textsize.x * 0.5, -offset - textsize.y)
                    }
                    Direction4::Left => {
                        rect.left_center() + vec2(-textsize.x - offset, -textsize.y * 0.5)
                    }
                    Direction4::Down => rect.center_bottom() + vec2(-textsize.x * 0.5, offset),
                    Direction4::Right => rect.right_center() + vec2(offset, -textsize.y * 0.5),
                };

                let rect = Rect::from_min_size(pos, textsize).expand(ctx.screen.scale * 0.15);
                let visual = &ctx.ui.style().visuals;
                ctx.paint.rect(
                    rect,
                    Rounding::same(ctx.screen.scale * 0.15),
                    visual.window_fill.linear_multiply(1.1),
                    visual.window_stroke,
                );
                ctx.paint.add(TextShape {
                    pos,
                    galley: galley.galley,
                    underline: Stroke::NONE,
                    override_text_color: Some(visual.text_color()),
                    angle: 0.0,
                });
            }
        }
    }

    /* #endregion */

    /* #region Updating */

    fn update_wires(&mut self, ctx: &PaintContext, selected: bool) {
        if !selected {
            self.wire_drag_pos = None;
            return;
        }

        let mouse_tile_pos = ctx
            .ui
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
                self.place_wire_part(part, true);
            }
        }

        if let Some(mouse_pos) = mouse_tile_pos_i {
            if interaction.clicked_by(egui::PointerButton::Primary) && self.wire_drag_pos.is_none()
            {
                self.try_toggle_node_intersection(mouse_pos);
            }
        }
    }

    fn update_previews(&mut self, ctx: &PaintContext, selected: Option<SelectedItem>) {
        let selected = match selected {
            Some(selected) => selected,
            None => return,
        };

        match selected {
            SelectedItem::Circuit(_) => (),
            SelectedItem::Paste(_) => (),
            _ => return,
        };

        let mouse_tile_pos = ctx
            .ui
            .input(|input| input.pointer.interact_pos())
            .map(|p| ctx.screen.screen_to_world(Vec2f::from(p)));
        let mouse_tile_pos_i = match mouse_tile_pos {
            None => return,
            Some(v) => v.convert(|v| v.floor() as i32),
        };

        if let SelectedItem::Circuit(p) = selected {
            let description = p.describe();
            let size = description.size;
            if size.x() == 0 || size.y() == 0 {
                return;
            }
            let place_pos = mouse_tile_pos_i - size.convert(|v| v as i32) / 2;
            let rect = Rect::from_min_size(
                ctx.screen.world_to_screen_tile(place_pos).into(),
                (size.convert(|v| v as f32) * ctx.screen.scale).into(),
            );
            p.draw(&ctx.with_rect(rect), true);

            for pin in description.pins.iter() {
                if !pin.active {
                    continue;
                }
                let pos = ctx.screen.world_to_screen(
                    place_pos.convert(|v| v as f32) + pin.pos.convert(|v| v as f32) + 0.5,
                );
                ctx.paint.circle_filled(
                    pos.into(),
                    ActiveCircuitBoard::WIRE_THICKNESS * 0.5 * ctx.screen.scale,
                    WireState::False.color(),
                );
            }

            crate::ui::drawing::draw_pin_names(
                place_pos.convert(|v| v as isize),
                description
                    .pins
                    .iter()
                    .filter(|p| p.active)
                    .map(|i| (i.pos, i.display_name.deref(), i.display_dir)),
                0.5,
                0.5,
                ctx,
            );

            let interaction = ctx.ui.interact(ctx.rect, ctx.ui.id(), Sense::click());

            if interaction.clicked_by(eframe::egui::PointerButton::Primary) {
                fn empty_handler(_: &mut ActiveCircuitBoard, _: usize) {}
                self.place_circuit(place_pos, true, p.as_ref(), None, None, &empty_handler);
            }
        } else if let SelectedItem::Paste(p) = selected {
            let size = p.size;
            if size.x() == 0 || size.y() == 0 {
                return;
            }
            let place_pos = mouse_tile_pos_i - size.convert(|v| v as i32) / 2;
            let rect = Rect::from_min_size(
                ctx.screen.world_to_screen_tile(place_pos).into(),
                (size.convert(|v| v as f32) * ctx.screen.scale).into(),
            );
            p.draw(self, place_pos, &ctx.with_rect(rect));
            let interaction = ctx.ui.interact(ctx.rect, ctx.ui.id(), Sense::click());
            if interaction.clicked_by(eframe::egui::PointerButton::Primary) {
                p.place(self, place_pos);
            }
        }
    }

    /* #endregion */

    /* #region Wire drawing helpers */

    pub fn draw_wire_part(&self, ctx: &PaintContext, part: &WirePart, color: Color32) {
        let screen = &ctx.screen;
        let rect = Self::calc_wire_part_rect(screen, part);
        ctx.paint.rect_filled(rect, Rounding::ZERO, color);
    }

    pub fn calc_wire_point_rect(screen: &Screen, pos: Vec2i) -> Rect {
        let thickness = screen.scale * Self::WIRE_POINT_THICKNESS;

        let rect_pos = screen.world_to_screen_tile(pos) + ((screen.scale - thickness) * 0.5);

        Rect::from_min_size(rect_pos.into(), vec2(thickness, thickness))
    }

    pub fn calc_wire_part_rect(screen: &Screen, part: &WirePart) -> Rect {
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

    fn draw_wire_point(ctx: &PaintContext, pos: Vec2i, color: Color32, pin_debug: bool) {
        let screen = &ctx.screen;

        let rect = Self::calc_wire_point_rect(screen, pos);
        ctx.paint.rect_filled(rect, Rounding::ZERO, color);

        // DEBUG: visuals
        if pin_debug {
            ctx.paint
                .rect_stroke(rect, Rounding::ZERO, Stroke::new(1.0, Color32::RED));
        }
    }

    /* #endregion */

    /* #region Wire manipulations */

    /// Returns placed wire id
    pub fn place_wire_part(&mut self, part: WirePart, lock_sim: bool) -> Option<usize> {
        let part = unwrap_option_or_return!(self.optimize_wire_part(part), None);

        let sim_lock = { self.board.sim_lock.clone() };
        let sim_lock = { lock_sim.then(|| sim_lock.write()) };
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
            0 => self.board.create_wire(),
            1 => *wires_crossed.iter().next().unwrap(),
            _ => {
                let main_wire = {
                    self.board
                        .wires
                        .read()
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

        self.board.states.update_wire(new_wire, true);
        drop(sim_lock);
        Some(new_wire)
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
            let pin = self.pin_at(pos);
            let node = self
                .wire_nodes
                .get_or_create_mut(pos.convert(|v| v as isize));

            let point =
                pin.is_some() || node.wire.is_some() || i == 0 || i as u32 == part.length.get();

            if i > 0 {
                node.get_dir_mut(dir_rev).set(Some(dist))
            }

            if point {
                node.wire.set(Some(wire));
                if i > 0 {
                    fix_pointers(self, pos, dist, dir_rev);
                }
                dist = 1;
            } else {
                dist += 1;
            }
        }
        let mut wires = self.board.wires.write();
        for pos in part.iter_pos(true) {
            let node = self.wire_nodes.get(pos.convert(|v| v as isize));
            let node = unwrap_option_or_continue!(node);

            let point = node.wire.is_some();
            if point {
                let pin = self.pin_at(pos);
                {
                    if let Some(wire) = wires.get_mut(wire) {
                        wire.set_point(
                            pos,
                            &self.board.states,
                            Some(WirePoint {
                                left: node.left.is_some(),
                                up: node.up.is_some(),
                                pin: pin.clone(),
                            }),
                            false,
                        )
                    }
                }
                if let Some(pin) = &pin {
                    pin.write()
                        .set_wire(&self.board.states, Some(wire), false, true);
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
                pin.write().set_wire(&self.board.states, wire, false, true);
            }

            let mut wires = self.board.wires.write();

            if let Some(wire) = prev_wire.and_then(|w| wires.get_mut(w)) {
                wire.set_point(pos, &self.board.states, None, false);
            }

            if let Some(wire) = wire.and_then(|w| wires.get_mut(w)) {
                wire.set_point(
                    pos,
                    &self.board.states,
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
            if let Some(wire) = prev_wire {
                self.board.states.update_wire(wire, true);
            }

            if let Some(wire) = wire {
                self.board.states.update_wire(wire, true)
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
                        let wires = self.board.wires.read();
                        (
                            wires.get(horizontal).map(|w| w.points.len()),
                            wires.get(vertical).map(|w| w.points.len()),
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
                .wires
                .read()
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
            let mut wires = self.board.wires.write();
            let wire = wires.get_mut(wire)?;
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
            self.split_wires(wire, true);
        }
        if update_states {
            self.board.states.update_wire(wire, true);
        }

        Some(wire)
    }

    fn merge_wires(&mut self, wire: usize, with: usize, update_state: bool) {
        {
            let wires = self.board.wires.read();

            let with_wire = unwrap_option_or_return!(wires.get(with));

            if !wires.exists(wire) || wire == with {
                return;
            }

            let points: Vec<_> = with_wire.points.keys().cloned().collect();
            drop(wires);

            self.set_node_wires(points.iter(), wire);
        }
        self.board.merge_wires(wire, with, update_state);
    }

    fn split_wires(&mut self, id: usize, update_states: bool) {
        let mut groups = vec![];

        let mut remaining_nodes: HashSet<_> = {
            let wires = self.board.wires.read();
            let wire = unwrap_option_or_return!(wires.get(id));
            wire.points.keys().copied().collect()
        };

        // empty wire
        if remaining_nodes.is_empty() {
            self.board.wires.write().remove(id);
            self.board.states.reset_wire(id);
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
                let new_wire = self.board.split_wire(id, &group, false);
                if let Some(wire) = new_wire {
                    self.set_node_wires(group.iter(), wire);
                    wires.push(wire);
                }
            } else {
                wires.push(id);
            }
        }

        if update_states {
            for wire in wires {
                self.board.states.update_wire(wire, true);
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

    pub fn find_wire_node(&self, pos: Vec2i, dir: Direction4) -> Option<FoundWireNode> {
        let node = self.wire_nodes.get(pos.convert(|v| v as isize))?;
        self.find_wire_node_from_node(node, pos, dir)
    }

    pub fn find_wire_node_from_node(
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

    pub fn should_draw_wire_point(&self, pos: Vec2i, shift: bool) -> bool {
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

    /// Handler is called after circuit is crated and placed but before it's connected to wires and updated
    pub fn place_circuit(
        &mut self,
        place_pos: Vec2i,
        lock_sim: bool,
        preview: &CircuitPreview,
        props_override: Option<CircuitPropertyStore>,
        imp_data: Option<&Intermediate>,
        handler: &dyn Fn(&mut ActiveCircuitBoard, usize),
    ) -> Option<usize> {
        let size = preview.describe().size;
        if !self.can_place_circuit_at(size, place_pos, None) {
            return None;
        }

        let sim_lock = { self.board.sim_lock.clone() };
        let sim_lock = { lock_sim.then(|| sim_lock.write()) };

        let cid = {
            self.board
                .create_circuit(place_pos, preview, props_override, imp_data, true)
        };

        self.set_circuit_nodes(size, place_pos, Some(cid));
        handler(self, cid);
        self.connect_circuit_to_wires(cid);
        let circ = self.board.circuits.read().get(cid).unwrap().clone();

        circ.initialize(true);
        self.board.states.init_circuit_state(&circ, true);
        self.board.states.update_circuit_signals(cid, None);
        drop(sim_lock);
        Some(cid)
    }

    fn connect_circuit_to_wires(&mut self, circuit: usize) {
        let circuits = self.board.circuits.read();
        let circuit = circuits.get(circuit);
        let circuit = unwrap_option_or_return!(circuit);
        let info = circuit.info.clone();
        let pos = circuit.pos;

        drop(circuits);

        for pin in info.read().pins.iter() {
            let pos = pos + pin.pos.convert(|v| v as i32);
            if let Some(wire) = self.create_wire_intersection(pos) {
                let mut wires = self.board.wires.write();
                let wire = if let Some(wire) = wires.get_mut(wire) {
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
                        .set_wire(&self.board.states, Some(wire), true, false);
                }
            } else {
                pin.pin
                    .write()
                    .set_wire(&self.board.states, None, true, false);
            }
        }
    }

    fn set_circuit_nodes(&mut self, size: Vec2u, pos: Vec2i, id: Option<usize>) {
        for j in 0..size.y() {
            for i in 0..size.x() {
                let pos = pos + [i as i32, j as i32];
                let node = self
                    .circuit_nodes
                    .get_or_create_mut(pos.convert(|v| v as isize));

                node.circuit.set(id);
                node.origin_dist = if id.is_some() {
                    [i, j].into()
                } else {
                    Vec2u::single_value(0)
                };
            }
        }
    }

    pub fn can_place_circuit_at(
        &self,
        size: Vec2u,
        pos: Vec2i,
        ignore_circuit: Option<usize>,
    ) -> bool {
        for j in 0..size.y() as i32 {
            for i in 0..size.x() as i32 {
                let pos = pos + [i, j];
                if self
                    .circuit_nodes
                    .get(pos.convert(|v| v as isize))
                    .is_some_and(|n| {
                        n.circuit
                            .is_some_and(|id| !ignore_circuit.is_some_and(|ign| ign == id))
                    })
                {
                    return false;
                }
            }
        }
        true
    }

    fn pin_at(&self, pos: Vec2i) -> Option<Arc<RwLock<CircuitPin>>> {
        let circ = self.circuit_nodes.get(pos.convert(|v| v as isize))?;
        let pos = circ.origin_dist;
        let circ = circ.circuit.get()?;

        let circuits = self.board.circuits.read();
        let circ_info = circuits.get(circ)?.info.read();

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
        let circuit = self.board.circuits.read().get(id).cloned();
        let circuit = unwrap_option_or_return!(circuit);

        let size = circuit.info.read().size;

        self.set_circuit_nodes(size, circuit.pos, None);

        let mut wires = self.board.wires.write();

        for pin in circuit.info.read().pins.iter() {
            let wire_id = pin.pin.read().connected_wire();
            let wire_id = unwrap_option_or_continue!(wire_id);
            pin.pin.write().wire = None;

            let wire = unwrap_option_or_continue!(wires.get_mut(wire_id));
            let pos = circuit.pos + pin.pos.convert(|v| v as i32);
            let point = unwrap_option_or_continue!(wire.points.get_mut(&pos));
            point.pin = None;

            affected_wires.insert(wire_id);
        }

        self.board.circuits.write().remove(id);
        self.board.states.reset_circuit(&circuit);

        circuit.remove();
    }

    // Todo: result
    fn try_updating_circuit_property(&mut self, circuit_id: usize, property: &str) -> bool {
        let sim_lock = { self.board.sim_lock.clone() };
        let sim_lock = sim_lock.write();

        let circuits = self.board.circuits.read();
        let circuit = circuits.get(circuit_id);
        let circuit = unwrap_option_or_return!(circuit, false);

        let mut resize = false;
        let mut recreate_pins = false;
        circuit
            .imp
            .read()
            .prop_changed(property, &mut resize, &mut recreate_pins);

        if resize {
            let new_size = circuit.imp.read().size(circuit);
            let size_changed = new_size != circuit.info.read().size;
            drop(circuits);

            if size_changed && !self.try_change_circuit_size(circuit_id, new_size) {
                return false;
            }
        } else {
            drop(circuits);
        }

        if recreate_pins {
            let mut wires = self.board.wires.write();
            let circuits = self.board.circuits.read();

            let circuit = circuits.get(circuit_id).expect("unexpected");

            let pins: Vec<_> = circuit
                .info
                .read()
                .pins
                .iter()
                .map(|p| (p.pos.convert(|v| v as i32) + circuit.pos, p.pin.clone()))
                .collect();

            // disconnect all older pins
            for (pos, pin) in pins {
                let pin_wire = pin.read().connected_wire();
                if let Some(wire) = pin_wire {
                    if let Some(wire) = wires.get_mut(wire) {
                        if let Some(point) = wire.points.get_mut(&pos) {
                            point.pin = None;
                            self.board.states.update_wire(wire.id, true);
                        }
                    }
                    pin.write().set_wire(&self.board.states, None, false, false);
                }
            }

            let circuit = circuits.get(circuit_id).expect("unexpected");

            let new_pins = circuit.imp.write().create_pins(circuit);
            for pin in new_pins.iter().enumerate() {
                pin.1.pin.write().id = CircuitPinId::new(pin.0, circuit_id);
            }
            circuit.info.write().pins = new_pins;
            drop((circuits, wires));

            self.connect_circuit_to_wires(circuit_id);
        }

        let circuit = self
            .board
            .circuits
            .read()
            .get(circuit_id)
            .expect("unexpected")
            .clone();

        circuit.imp.write().apply_props(&circuit, Some(property));

        self.board.states.update_circuit_signals(circuit_id, None);

        drop(sim_lock);
        true
    }

    pub fn circuit_property_changed(
        &mut self,
        circuit: usize,
        property: &str,
        old_value: &dyn CircuitPropertyImpl,
    ) {
        if !self.try_updating_circuit_property(circuit, property) {
            let circuits = self.board.circuits.read();
            if let Some(circuit) = circuits.get(circuit) {
                circuit
                    .props
                    .write_dyn(property, |p| old_value.copy_into(p.imp_mut()));
            }
        }
    }

    // todo: result
    fn try_change_circuit_size(&mut self, circuit_id: usize, new_size: Vec2u) -> bool {
        let circuit = self.board.circuits.read().get(circuit_id).cloned();
        let circuit = unwrap_option_or_return!(circuit, false);
        let circuit_pos = circuit.pos;
        let old_size = circuit.info.read().size;

        if !self.can_place_circuit_at(new_size, circuit_pos, Some(circuit_id)) {
            return false;
        }
        self.set_circuit_nodes(old_size, circuit_pos, None);
        self.set_circuit_nodes(new_size, circuit_pos, Some(circuit_id));

        circuit.info.write().size = new_size;
        true
    }
}

pub struct CircuitDesignStorage {
    current: usize,
    designs: FixedVec<Arc<CircuitDesign>>,
}

impl CircuitDesignStorage {
    pub fn new(current: CircuitDesign) -> Self {
        let id = current.id;
        let design = Arc::new(current);
        let mut designs = FixedVec::new();
        designs.set(design, id);

        Self {
            current: id,
            designs,
        }
    }

    pub fn get(&self, id: usize) -> Option<Arc<CircuitDesign>> {
        self.designs.get(id).cloned()
    }

    pub fn current_id(&self) -> usize {
        self.current
    }

    pub fn current(&self) -> Arc<CircuitDesign> {
        self.designs
            .get(self.current)
            .expect("current design must always exist")
            .clone()
    }

    pub fn current_mut(&mut self) -> &mut CircuitDesign {
        let current_unique = Arc::get_mut(
            self.designs
                .get_mut(self.current)
                .expect("current design must always exist"),
        )
        .is_some();

        if !current_unique {
            let clone = self
                .designs
                .get_mut(self.current)
                .expect("current design must always exist")
                .deref()
                .deref()
                .clone();
            let id = self.designs.first_free_pos();
            self.designs.set(Arc::new(clone.with_id(id)), id);
            self.current = id;
        }

        Arc::get_mut(
            self.designs
                .get_mut(self.current)
                .expect("current design must always exist"),
        )
        .expect("this should've been validated before")
    }

    pub fn save(&self) -> crate::io::CircuitDesignStoreData {
        crate::io::CircuitDesignStoreData {
            current: self.current,
            designs: self
                .designs
                .inner()
                .iter()
                .map(|d| {
                    d.as_ref().and_then(|d| {
                        (Arc::strong_count(d) > 1 || Arc::weak_count(d) > 0 || d.id == self.current)
                            .then(|| crate::io::CircuitDesignData {
                                pins: d.pins.clone(),
                                size: d.size,
                                decorations: d.decorations.clone(),
                                controls: d.controls.clone(),
                            })
                    })
                })
                .collect(),
        }
    }

    pub fn load(data: &crate::io::CircuitDesignStoreData) -> Self {
        Self {
            current: data.current,
            designs: FixedVec::from_option_vec(
                data.designs
                    .iter()
                    .enumerate()
                    .map(|(i, d)| {
                        d.as_ref().map(|d| {
                            Arc::new(CircuitDesign {
                                id: i,
                                size: d.size,
                                pins: d.pins.clone(),
                                decorations: d.decorations.clone(),
                                controls: d.controls.clone(),
                            })
                        })
                    })
                    .collect(),
            ),
        }
    }
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize)]
pub enum Decoration {
    Rect { rect: Rect, visuals: RectVisuals }, // TODO: MOAR!
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DecorationType {
    Rect,
}

impl Decoration {
    pub fn draw(&self, painter: &Painter, base_pos: Pos2, scale: f32, opacity: f32) {
        match self {
            Decoration::Rect { rect, visuals } => {
                let rect = Rect::from_min_size(
                    (rect.left_top().to_vec2() * scale + base_pos.to_vec2()).to_pos2(),
                    rect.size() * scale,
                );
                let scaled_stroke = Stroke::new(
                    visuals.stroke.width * scale,
                    visuals.stroke.color.linear_multiply(opacity),
                );
                painter.rect(
                    rect,
                    visuals.rounding,
                    visuals.fill.linear_multiply(opacity),
                    scaled_stroke,
                );
            }
        }
    }

    pub fn ty(self) -> DecorationType {
        match self {
            Decoration::Rect {
                rect: _,
                visuals: _,
            } => DecorationType::Rect,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CircuitDesignPin {
    pub id: DynStaticStr,
    pub pos: Vec2u,
    pub dir: InternalPinDirection,
    pub display_dir: Option<Direction4>,
    pub display_name: DynStaticStr,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CircuitDesignControl {
    pub rect: Rect,
    pub display_name: ArcString,
}

#[derive(Debug, Clone, Default)]
pub struct CircuitDesign {
    pub id: usize,

    pub size: Vec2u,
    pub pins: Vec<CircuitDesignPin>,
    pub decorations: Vec<Decoration>,
    pub controls: HashMap<(usize, usize), CircuitDesignControl>,
}

impl CircuitDesign {
    pub fn id(&self) -> usize {
        self.id
    }

    pub fn with_id(self, id: usize) -> Self {
        Self { id, ..self }
    }

    pub fn default_board_design() -> Self {
        Self {
            id: 0,
            size: 2.into(),
            pins: vec![],
            decorations: vec![Decoration::Rect {
                rect: Rect::from_min_size(pos2(0.0, 0.0), vec2(2.0, 2.0)),
                visuals: RectVisuals {
                    rounding: Rounding::ZERO,
                    fill: Color32::from_gray(100),
                    stroke: Stroke::new(0.1, Color32::BLACK),
                },
            }],
            controls: HashMap::new(),
        }
    }

    pub fn draw_decorations(&self, painter: &Painter, rect: Rect, opacity: f32) {
        let base_pos = rect.left_top();
        let scale = rect.size() / Vec2::from(self.size.convert(|v| v as f32));
        for decoration in self.decorations.iter() {
            decoration.draw(painter, base_pos, scale.x.min(scale.y), opacity);
        }
    }
}

#[derive(Hash, PartialEq, Eq)]
pub enum SelectedBoardObject {
    WirePart { pos: Vec2i, dir: Direction2 },
    Circuit { id: usize },
}

#[derive(Default)]
pub struct BoardObjectSelectionImpl {
    possible_points: HashSet<Vec2i>,
}

impl SelectionImpl for BoardObjectSelectionImpl {
    type Pass = ActiveCircuitBoard;
    type Object = SelectedBoardObject;

    fn draw_object_selection(
        &mut self,
        pass: &ActiveCircuitBoard,
        object: &SelectedBoardObject,
        ctx: &PaintContext,
        shapes: &mut Vec<Shape>,
    ) {
        match object {
            SelectedBoardObject::WirePart { pos, dir } => {
                if let Some(w) = pass.find_wire_node(*pos, (*dir).into()) {
                    let part = WirePart {
                        pos: *pos,
                        dir: *dir,
                        length: w.distance,
                    };
                    let rect = ActiveCircuitBoard::calc_wire_part_rect(&ctx.screen, &part);
                    let rect = rect.expand(2.0);
                    shapes.push(Shape::rect_filled(rect, Rounding::ZERO, Color32::WHITE));

                    self.possible_points.insert(*pos);
                    self.possible_points.insert(w.pos);
                }
            }
            SelectedBoardObject::Circuit { id } => {
                let circuits = pass.board.circuits.read();
                if let Some(circ) = circuits.get(*id) {
                    let rect_pos = ctx.screen.world_to_screen_tile(circ.pos);
                    let rect_size = circ.info.read().size.convert(|v| v as f32) * ctx.screen.scale;
                    let rect = Rect::from_min_size(rect_pos.into(), rect_size.into());
                    let rect = rect.expand(2.0);
                    shapes.push(Shape::Rect(RectShape::new(
                        rect,
                        Rounding::ZERO,
                        selection_fill_color(),
                        Stroke::new(2.0, selection_border_color()),
                    )));
                }
            }
        }
    }

    fn collect_changes(
        &mut self,
        pass: &ActiveCircuitBoard,
        changes: &mut HashSet<SelectedBoardObject>,
        rect: Rect,
    ) {
        let min_tile = Vec2f::from(rect.min).convert(|v| v.floor() as i32);
        let max_tile = Vec2f::from(rect.max).convert(|v| v.ceil() as i32);

        let pos = min_tile;
        let size = (max_tile - min_tile).convert(|v| v as u32);

        for (pos, node) in pass
            .wire_nodes
            .iter_area(pos.convert(|v| v as isize), size.convert(|v| v as usize))
        {
            let pos = pos.convert(|v| v as i32);

            if node.wire.is_some() {
                for dir in Direction4::iter_all() {
                    let node = pass.find_wire_node_from_node(node, pos, dir);
                    let node = unwrap_option_or_continue!(node);
                    let (dir, forward) = dir.into_dir2();

                    changes.insert(SelectedBoardObject::WirePart {
                        pos: if forward { pos } else { node.pos },
                        dir,
                    });
                }
            } else {
                for dir in [Direction4::Right, Direction4::Down] {
                    let node = pass.find_wire_node_from_node(node, pos, dir);
                    let node = unwrap_option_or_continue!(node);
                    changes.insert(SelectedBoardObject::WirePart {
                        pos: node.pos,
                        dir: dir.into_dir2().0,
                    });
                }
            }
        }
        for (_, node) in pass
            .circuit_nodes
            .iter_area(pos.convert(|v| v as isize), size.convert(|v| v as usize))
        {
            let circuit = unwrap_option_or_continue!(node.circuit.get());

            changes.insert(SelectedBoardObject::Circuit { id: circuit });
        }
    }

    fn post_draw_selection(
        &mut self,
        pass: &ActiveCircuitBoard,
        ctx: &PaintContext,
        mode: SelectionMode,
        selected: &HashSet<SelectedBoardObject>,
        change: &HashSet<SelectedBoardObject>,
        shapes: &mut Vec<Shape>,
    ) {
        let shift = ctx.ui.input(|input| input.modifiers.shift);
        for point in self.possible_points.iter().copied() {
            if pass.should_draw_wire_point(point, shift) {
                let node =
                    unwrap_option_or_continue!(pass.wire_nodes.get(point.convert(|v| v as isize)));
                let all_connections_selected = Direction4::iter_all().all(|dir| {
                    node.get_dir(dir).is_none_or(|_| {
                        let (part_dir, forward) = dir.into_dir2();

                        let part_pos = if forward {
                            Some(point)
                        } else {
                            pass.find_wire_node_from_node(node, point, dir)
                                .map(|f| f.pos)
                        };

                        match part_pos {
                            None => false,
                            Some(part_pos) => {
                                let part = SelectedBoardObject::WirePart {
                                    pos: part_pos,
                                    dir: part_dir,
                                };

                                match mode {
                                    SelectionMode::Include => {
                                        selected.contains(&part) || change.contains(&part)
                                    }
                                    SelectionMode::Exclude => {
                                        selected.contains(&part) && !change.contains(&part)
                                    }
                                }
                            }
                        }
                    })
                });

                if all_connections_selected {
                    let rect = ActiveCircuitBoard::calc_wire_point_rect(&ctx.screen, point);
                    let rect = rect.expand(2.0);
                    shapes.push(Shape::rect_filled(rect, Rounding::ZERO, Color32::WHITE));
                }
            }
        }
        self.possible_points.clear();
    }
}

pub struct BoardDesignProvider {
    board: Arc<CircuitBoard>,
}

impl BoardDesignProvider {
    pub fn new(board: Arc<CircuitBoard>) -> Self {
        Self { board }
    }
}

impl DesignProvider for BoardDesignProvider {
    fn get_storage(&self) -> Arc<RwLock<CircuitDesignStorage>> {
        self.board.designs.clone()
    }

    fn get_pin_ids(&self) -> Vec<DynStaticStr> {
        self.board
            .pins
            .read()
            .left_values()
            .map(|a| DynStaticStr::Dynamic(a.clone()))
            .collect()
    }

    fn get_pin(&self, id: &DynStaticStr) -> Option<crate::ui::designer::CircuitDesignPinInfo> {
        let pin_id = *self.board.pins.read().get_by_left(id.deref())?;

        let circuits = self.board.circuits.read();
        let circuit = circuits.get(pin_id)?;
        circuit.read_imp(|p: &crate::circuits::pin::Pin| p.get_designer_info(&circuit.props))
    }

    fn get_control_provider_data(&self) -> Vec<crate::ui::designer::ControlProvider> {
        let circuits = self.board.circuits.read();
        self.board
            .controls
            .read()
            .iter()
            .filter_map(|id| circuits.get(*id))
            .map(|circuit| crate::ui::designer::ControlProvider {
                id: circuit.id,
                count: circuit.imp.read().control_count(circuit).unwrap_or(0),
            })
            .filter(|p| p.count > 0)
            .collect()
    }

    fn get_control_info(
        &self,
        provider: usize,
        id: usize,
    ) -> Option<crate::ui::designer::CircuitDesignControlInfo> {
        let circuits = self.board.circuits.read();
        let circuit = circuits.get(provider)?;
        let info = circuit.imp.read().control_info(circuit, id)?;
        Some(crate::ui::designer::CircuitDesignControlInfo {
            rect: info.rect,
            display_name: info.display_name,
        })
    }

    fn paint_control(&self, provider: usize, id: usize, ctx: &PaintContext) {
        let circuits = self.board.circuits.read();
        let circuit = unwrap_option_or_return!(circuits.get(provider));
        circuit.imp.read().update_control(
            id,
            circuit,
            None,
            ctx,
            false,
            Id::new((self.board.uid, circuit.pos, id)),
        );
    }

    fn has_custom_config(&self) -> bool {
        true
    }

    fn custom_config_ui(&self, ui: &mut egui::Ui) {
        let mut single_outer_control = self.board.single_outer_control.load(Ordering::Relaxed);

        if ui
            .checkbox(&mut single_outer_control, "Single outer control")
            .on_hover_text("Whether or not all controls on this design\nshould be grouped into one")
            .changed()
        {
            self.board
                .single_outer_control
                .store(single_outer_control, Ordering::Relaxed);
        }
    }
}
