use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    f32::consts::TAU,
    num::NonZeroU32,
    ops::Deref,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

use bimap::BiMap;
use eframe::{
    egui::{self, FontSelection, Sense, TextStyle, WidgetText},
    epaint::{Color32, FontId, Rounding, Stroke, TextShape},
};
use emath::{pos2, vec2, Align2, Pos2, Rect};
use serde::{Deserialize, Serialize};

use crate::{
    circuits::{
        props::{CircuitPropertyImpl, CircuitPropertyStore},
        Circuit, CircuitNode, CircuitPin, CircuitPinId, CircuitPreview, CircuitStateContext, InternalPinDirection,
    },
    containers::{Chunks2D, ChunksLookaround, FixedVec},
    state::{State, StateCollection, WireState},
    unwrap_option_or_continue, unwrap_option_or_return,
    vector::{IsZero, Vec2f, Vec2i, Vec2isize, Vec2u},
    wires::{FoundWireNode, TileWires, Wire, WireNode, WirePart, WirePoint},
    ArcString, Direction2, Direction4, PaintContext, PastePreview, RwLock, Screen, DynStaticStr,
};

use self::selection::{SelectedWorldObject, Selection};

pub mod selection;

pub type BoardStorage = HashMap<u128, Arc<RwLock<CircuitBoard>>>;

pub struct CircuitBoard {
    pub name: String,
    pub uid: u128,

    pub wires: FixedVec<Wire>,
    pub circuits: FixedVec<Circuit>,
    pub states: StateCollection,

    pub designs: CircuitDesignStorage,
    pub pins: BiMap<Arc<str>, usize>,

    // RwLock for blocking simulation while modifying board
    pub sim_lock: Arc<RwLock<()>>,
    ordered_queue: bool,
}

impl CircuitBoard {
    pub fn new() -> Self {
        Self {
            uid: rand::random(),
            name: "".into(),
            wires: vec![].into(),
            circuits: vec![].into(),
            states: StateCollection::new(),
            sim_lock: Default::default(),
            ordered_queue: false,
            designs: CircuitDesignStorage::new(CircuitDesign::default_board_design()),
            pins: Default::default(),
        }
    }

    pub fn create_circuit(
        &mut self,
        pos: Vec2i,
        preview: &CircuitPreview,
        props_override: Option<CircuitPropertyStore>,
    ) -> usize {
        let id = self.circuits.first_free_pos();
        let circ = Circuit::create(id, pos, preview, props_override);
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
                pin.write().set_wire(&self.states, Some(id), false, true);
            }
        }

        let wire = self.wires.get_mut(id).unwrap();
        for point in points {
            wire.points.insert(point.0, point.1);
        }

        if update_states {
            self.states.update_wire(id, true);
        }
    }

    pub fn split_wire(
        &mut self,
        id: usize,
        points: &HashSet<Vec2i>,
        update_states: bool,
    ) -> Option<usize> {
        let new_wire_id = self.wires.first_free_pos();
        let wire = unwrap_option_or_return!(self.wires.get_mut(id), None);

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
        self.wires.set(new_wire, new_wire_id);

        if update_states {
            self.states.update_wire(id, true);
            self.states.update_wire(new_wire_id, true);
        }

        Some(new_wire_id)
    }

    pub fn save(&self, sim_lock: bool) -> crate::io::CircuitBoardData {
        let sim_lock = sim_lock.then(|| self.sim_lock.write());
        let data = crate::io::CircuitBoardData {
            name: self.name.clone(),
            uid: self.uid,
            wires: self
                .wires
                .inner()
                .iter()
                .map(|w| w.as_ref().map(|w| w.save()))
                .collect(),
            circuits: self
                .circuits
                .inner()
                .iter()
                .map(|c| c.as_ref().map(|c| c.save()))
                .collect(),
            states: self
                .states
                .states()
                .read()
                .inner()
                .iter()
                .map(|s| s.as_ref().map(|s| s.save()))
                .collect(),
            ordered: self.ordered_queue,
            designs: self.designs.save(),
        };
        drop(sim_lock);
        data
    }

    pub fn load(
        data: &crate::io::CircuitBoardData,
        ctx: &impl crate::io::LoadingContext,
    ) -> Arc<RwLock<Self>> {
        let circuits = FixedVec::from_option_vec(
            data.circuits
                .iter()
                .enumerate()
                .map(|(i, c)| {
                    c.as_ref().and_then(|c| {
                        let preview = ctx.get_circuit_preview(&c.ty)?;
                        let props = preview.imp.default_props();
                        props.load(&c.props);
                        let circ = Circuit::create(i, c.pos, preview, Some(props));
                        if !matches!(c.imp, serde_intermediate::Intermediate::Unit) {
                            circ.imp.write().load(&c.imp);
                        }
                        {
                            let mut info = circ.info.write();
                            for (pin_name, wire) in c.pin_wires.iter() {
                                if let Some(info) =
                                    info.pins.iter_mut().find(|i| i.name == *pin_name)
                                {
                                    info.pin.write().wire = Some(*wire);
                                }
                            }
                        }
                        Some(circ)
                    })
                })
                .collect(),
        );

        let wires = FixedVec::from_option_vec(
            data.wires
                .iter()
                .enumerate()
                .map(|(i, w)| w.as_ref().map(|w| Wire::load(w, i, &circuits)))
                .collect(),
        );

        let designs = CircuitDesignStorage::load(&data.designs);

        let board = CircuitBoard {
            name: data.name.clone(),
            uid: data.uid,
            wires,
            circuits,
            states: StateCollection::new(),
            sim_lock: Default::default(),
            ordered_queue: data.ordered,
            designs,
            pins: Default::default()
        };
        let board = Arc::new(RwLock::new(board));

        let states = StateCollection::from_fixed_vec(FixedVec::from_option_vec(
            data.states
                .iter()
                .enumerate()
                .map(|(i, v)| {
                    v.as_ref()
                        .map(|s| Arc::new(State::load(s, board.clone(), i)))
                })
                .collect(),
        ));

        board.write().states = states;

        board
    }

    pub fn is_ordered_queue(&self) -> bool {
        self.ordered_queue
    }

    pub fn set_ordered_queue(&mut self, ordered: bool, lock_sim: bool) {
        let sim_lock = lock_sim.then(|| self.sim_lock.write());
        self.states.set_ordered(ordered);
        self.ordered_queue = ordered;
        drop(sim_lock);
    }

    // Run board simulation after loading. Not required on newly created or empty boards
    #[cfg(not(feature = "single_thread"))]
    pub fn activate(&self) {
        self.states.activate();
    }
}

impl Default for CircuitBoard {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone)]
pub enum SelectedItem {
    None,
    Selection,
    Wire,
    Circuit(Arc<CircuitPreview>),
    Paste(Arc<PastePreview>),
}

impl SelectedItem {
    pub fn none(&self) -> bool {
        matches!(self, SelectedItem::None)
    }

    pub fn selection(&self) -> bool {
        matches!(self, SelectedItem::Selection)
    }

    pub fn wire(&self) -> bool {
        matches!(self, SelectedItem::Wire)
    }

    pub fn circuit(&self) -> Option<&CircuitPreview> {
        match self {
            SelectedItem::Circuit(c) => Some(c.as_ref()),
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
    pub selection: RefCell<Selection>,

    pub wires_drawn: AtomicUsize,
}

impl ActiveCircuitBoard {
    pub const WIRE_THICKNESS: f32 = 0.2;
    pub const WIRE_POINT_THICKNESS: f32 = 0.35;

    pub fn new_main(board: Arc<RwLock<CircuitBoard>>) -> Self {
        Self::new(
            board.clone(),
            board.clone().read().states.get_or_create_main(board),
        )
    }

    pub fn new(board: Arc<RwLock<CircuitBoard>>, state: Arc<State>) -> Self {
        let (wires, circuits) = {
            let board = board.read();
            let mut wires = Chunks2D::<16, WireNode>::default();
            let mut circuits = Chunks2D::<16, CircuitNode>::default();

            for wire in board.wires.iter() {
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

            for circuit in board.circuits.iter() {
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
            selection: RefCell::new(Selection::new()),

            wires_drawn: AtomicUsize::new(0),
        }
    }

    pub fn update(
        &mut self,
        ctx: &PaintContext,
        selected: SelectedItem,
        debug: bool,
        boards: &BoardStorage,
    ) {
        self.wires_drawn.store(0, Ordering::Relaxed);
        self.selection
            .borrow_mut()
            .pre_update_selection(self, ctx, selected.selection());

        let selected_something = !self.selection.borrow().selection.is_empty();

        if selected_something && !ctx.egui_ctx.wants_keyboard_input() {
            cfg_if::cfg_if! {
                if #[cfg(all(not(web_sys_unstable_apis), feature = "wasm"))] {
                    let copy_request = ctx.egui_ctx.input(|input| {
                        input.modifiers.ctrl
                            && (input.key_pressed(egui::Key::C) || input.key_pressed(egui::Key::X))
                    });
                } else {
                    let copy_request = ctx.egui_ctx.input(|input| {
                        input
                            .events
                            .iter()
                            .any(|e| matches!(e, egui::Event::Copy | egui::Event::Cut))
                    });
                }
            }

            if copy_request {
                let selection = self.selection.borrow();
                let board = self.board.read();

                let min_pos = {
                    let mut min_pos = None;
                    for obj in selection.selection.iter() {
                        let pos = match obj {
                            SelectedWorldObject::WirePart { pos, dir } => {
                                let node = self.find_wire_node(*pos, (*dir).into());
                                let node = unwrap_option_or_continue!(node);
                                node.pos
                            }
                            SelectedWorldObject::Circuit { id } => {
                                let circuit = board.circuits.get(*id);
                                let circuit = unwrap_option_or_continue!(circuit);
                                circuit.pos
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
                            SelectedWorldObject::WirePart { pos, dir } => {
                                if let Some(w) = self.find_wire_node(*pos, (*dir).into()) {
                                    copy.wires.push(crate::io::WirePartCopyData {
                                        pos: (*pos - min_pos).convert(|v| v as u32),
                                        length: w.distance.get(),
                                        dir: *dir,
                                    })
                                }
                            }
                            SelectedWorldObject::Circuit { id } => {
                                if let Some(circuit) = board.circuits.get(*id) {
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
                        .egui_ctx
                        .input(|input| input.modifiers.ctrl && input.key_pressed(egui::Key::X)) ;
                } else {
                    let delete_request = ctx
                        .egui_ctx
                        .input(|input| input.events.iter().any(|e| matches!(e, egui::Event::Cut)));
                }
            }

            if delete_request
                || ctx
                    .egui_ctx
                    .input(|input| input.key_pressed(egui::Key::Delete))
            {
                let mut affected_wires = HashSet::new();
                let drain = {
                    let mut selection = self.selection.borrow_mut();
                    selection.selection.drain().collect::<Vec<_>>()
                };
                let sim_lock = { self.board.read().sim_lock.clone() };
                let sim_lock = sim_lock.write();
                for obj in drain {
                    match obj {
                        SelectedWorldObject::Circuit { id } => {
                            self.remove_circuit(id, &mut affected_wires);
                        }
                        SelectedWorldObject::WirePart { pos, dir } => {
                            if let Some(wire) = self.remove_wire_part(pos, dir.into(), true, false)
                            {
                                affected_wires.insert(wire);
                            }
                        }
                    }
                }

                let states = self.board.read().states.clone();
                for wire in affected_wires {
                    states.update_wire(wire, true);
                }
                drop(sim_lock)
            }
        }

        ctx.draw_chunks(
            &self.wire_nodes,
            &self,
            |node| !node.is_empty(),
            |node, pos, ctx, this, lookaround| {
                this.draw_wire_node(ctx, node, pos, lookaround, debug);
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

        self.draw_hovered_circuit_pin_names(ctx);

        self.update_previews(ctx, selected, boards);
        self.selection.borrow_mut().update_selection(ctx);
    }

    fn draw_hovered_circuit_pin_names(&self, ctx: &PaintContext) {
        let mouse_tile_pos = ctx
            .egui_ctx
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
            .read()
            .circuits
            .get(circuit)
            .map(|c| c.info.clone());
        let info = unwrap_option_or_return!(info);

        let info = info.read();
        ActiveCircuitBoard::draw_pin_names(
            pos,
            info.pins
                .iter()
                .map(|pin| (pin.pos, pin.display_name.deref(), pin.display_dir)),
            ctx,
        );
    }

    fn draw_pin_names<'a>(
        pos: Vec2isize,
        pins: impl Iterator<Item = (Vec2u, &'a str, Option<Direction4>)>,
        ctx: &PaintContext,
    ) {
        fn draw_pin_name(
            name: &str,
            dir: Option<Direction4>,
            circ_pos: Vec2isize,
            pin_pos: Vec2u,
            ctx: &PaintContext,
        ) {
            if name.is_empty() {
                return;
            }
            let textoffset = ctx.screen.scale * 0.5;

            let galley = WidgetText::from(name).into_galley(
                ctx.ui,
                Some(false),
                f32::INFINITY,
                FontSelection::FontId(FontId::monospace(ctx.screen.scale * 0.5)),
            );

            //         n|
            //         i|
            //         P|
            //      +--*--+
            //      |  *  * Pin
            //  Pin * Pin |
            //      +--*--+
            //        |P
            //        |i          | marks text bottom
            //        |n

            let textsize = galley.size();
            let (dtx, dty, angle) = match dir {
                Some(Direction4::Up) => (-textsize.y * 0.5, -textoffset, TAU * 0.75),
                Some(Direction4::Left) => (-textsize.x - textoffset, -textsize.y * 0.5, 0.0),
                Some(Direction4::Down) => (textsize.y * 0.5, textoffset, TAU * 0.25),
                Some(Direction4::Right) => (textoffset, -textsize.y * 0.5, 0.0),

                None => (-textsize.x * 0.5, textoffset, 0.0),
            };

            let (drx, dry, vertical) = match dir {
                Some(Direction4::Up) => (-textsize.y * 0.5, -textoffset - textsize.x, true),
                Some(Direction4::Left) => (-textsize.x - textoffset, -textsize.y * 0.5, false),
                Some(Direction4::Down) => (-textsize.y * 0.5, textoffset, true),
                Some(Direction4::Right) => (textoffset, -textsize.y * 0.5, false),

                None => (-textsize.x * 0.5, textoffset, false),
            };

            let centerpos = Pos2::from(ctx.screen.world_to_screen(
                circ_pos.convert(|v| v as f32) + pin_pos.convert(|v| v as f32) + 0.5,
            ));

            let textpos = centerpos + vec2(dtx, dty);
            let rectpos = centerpos + vec2(drx, dry);
            let rectsize = match vertical {
                true => vec2(textsize.y, textsize.x),
                false => textsize,
            };
            let rect = Rect::from_min_size(rectpos, rectsize).expand(ctx.screen.scale * 0.1);

            let visual = &ctx.ui.style().visuals;
            ctx.paint.rect(
                rect,
                Rounding::same(ctx.screen.scale * 0.15),
                visual.window_fill.linear_multiply(0.6),
                visual.window_stroke,
            );
            ctx.paint.add(TextShape {
                pos: textpos,
                galley: galley.galley,
                underline: Stroke::NONE,
                override_text_color: Some(visual.text_color()),
                angle,
            });
        }

        for (pin_pos, pin_name, pin_dir) in pins {
            //     let u = pin_pos.y() == 0;
            //     let l = pin_pos.x() == 0;
            //     let d = pin_pos.y() + 1 >= size.y();
            //     let r = pin_pos.x() + 1 >= size.x();

            //     let dir = match (u, l, d, r) {
            //         (true, true, true, true) => None,
            //         (false, false, false, false) => None,

            //         (true, false, _, false) => Some(Direction4::Up),
            //         (false, true, false, _) => Some(Direction4::Left),
            //         (false, false, true, false) => Some(Direction4::Down),
            //         (false, false, false, true) => Some(Direction4::Right),

            //         (true, true, false, true) => Some(Direction4::Up),
            //         (true, true, true, false) => Some(Direction4::Left),
            //         (false, true, true, true) => Some(Direction4::Down),
            //         (true, false, true, true) => Some(Direction4::Right),

            //         (true, true, _, _) => {
            //             let dist_t = info
            //                 .pins
            //                 .iter()
            //                 .map(|i| i.pos)
            //                 .filter(|p| p.y() == 0)
            //                 .map(|p| p.x())
            //                 .filter(|d| *d > 0)
            //                 .min()
            //                 .unwrap_or(u32::MAX);
            //             let dist_l = info
            //                 .pins
            //                 .iter()
            //                 .map(|i| i.pos)
            //                 .filter(|p| p.x() == 0)
            //                 .map(|p| p.y())
            //                 .filter(|d| *d > 0)
            //                 .min()
            //                 .unwrap_or(u32::MAX);

            //             if dist_t < dist_l {
            //                 Some(Direction4::Up)
            //             } else {
            //                 Some(Direction4::Left)
            //             }
            //         }

            //         _ => None,
            //     };
            draw_pin_name(pin_name, pin_dir, pos, pin_pos, ctx);
        }
    }

    /* #region Drawing nodes */

    fn draw_wire_node(
        &self,
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
                if let Some(wire) = self.board.read().wires.get(wire) {
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
            let board = self.board.read();
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

        let board = self.board.read();
        let circuit = unwrap_option_or_return!(board.circuits.get(circ_id));

        let circ_info = circuit.info.read();

        let pos = pos - node.origin_dist.convert(|v| v as i32);
        let screen_pos = ctx.screen.world_to_screen_tile(pos);
        let screen_size = circ_info.size.convert(|v| v as f32) * ctx.screen.scale;
        let rect = Rect::from_min_size(screen_pos.into(), screen_size.into());
        let circ_ctx = ctx.with_rect(rect);

        let imp = circuit.imp.read();
        let state_ctx = CircuitStateContext::new(&self.state, circuit);

        if imp.draw_pin_points() {
            for pin in circ_info.pins.iter() {
                let pos = circuit.pos + pin.pos.convert(|v| v as i32);
                let pin = pin.pin.read();
                if pin.connected_wire().is_some() {
                    continue;
                }
                let color = pin.get_state(state_ctx.global_state).color();
                let pos = circ_ctx.screen.world_to_screen_tile(pos) + circ_ctx.screen.scale / 2.0;
                circ_ctx.paint.circle_filled(
                    pos.into(),
                    circ_ctx.screen.scale * Self::WIRE_THICKNESS * 0.5,
                    color,
                );
            }
        }

        imp.draw(&state_ctx, &circ_ctx);

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

    fn update_previews(
        &mut self,
        ctx: &PaintContext,
        selected: SelectedItem,
        boards: &BoardStorage,
    ) {
        match selected {
            SelectedItem::None => return,
            SelectedItem::Selection => return,
            SelectedItem::Wire => return,
            SelectedItem::Circuit(_) => (),
            SelectedItem::Paste(_) => (),
        };

        let mouse_tile_pos = ctx
            .egui_ctx
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

            Self::draw_pin_names(
                place_pos.convert(|v| v as isize),
                description
                    .pins
                    .iter()
                    .filter(|p| p.active)
                    .map(|i| (i.pos, i.display_name.deref(), i.display_dir)),
                ctx,
            );

            let interaction = ctx.ui.interact(ctx.rect, ctx.ui.id(), Sense::click());

            if interaction.clicked_by(eframe::egui::PointerButton::Primary) {
                fn empty_handler(_: &mut ActiveCircuitBoard, _: usize) {}
                self.place_circuit(place_pos, true, p.as_ref(), None, &empty_handler, boards);
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
                p.place(self, place_pos, boards);
            }
        }
    }

    /* #endregion */

    /* #region Wire drawing helpers */

    pub fn draw_wire_part(&self, ctx: &PaintContext, part: &WirePart, color: Color32) {
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

    fn draw_wire_point(ctx: &PaintContext, pos: Vec2i, color: Color32, pin_debug: bool) {
        let screen = &ctx.screen;

        let rect = Self::calc_wire_point_rect(screen, pos);
        ctx.paint.rect_filled(rect, Rounding::none(), color);

        // DEBUG: visuals
        if pin_debug {
            ctx.paint
                .rect_stroke(rect, Rounding::none(), Stroke::new(1.0, Color32::RED));
        }
    }

    /* #endregion */

    /* #region Wire manipulations */

    /// Returns placed wire id
    pub fn place_wire_part(&mut self, part: WirePart, lock_sim: bool) -> Option<usize> {
        let part = unwrap_option_or_return!(self.optimize_wire_part(part), None);

        let sim_lock = { self.board.read().sim_lock.clone() };
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
            0 => self.board.write().create_wire(),
            1 => *wires_crossed.iter().next().unwrap(),
            _ => {
                let main_wire = {
                    self.board
                        .read()
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

        let board = self.board.read();
        board.states.update_wire(new_wire, true);
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

        let states = self.board.read().states.clone();

        for pos in part.iter_pos(true) {
            let node = self.wire_nodes.get(pos.convert(|v| v as isize));
            let node = unwrap_option_or_continue!(node);

            let point = node.wire.is_some();
            if point {
                let pin = self.pin_at(pos);

                {
                    let mut board = self.board.write();
                    if let Some(wire) = board.wires.get_mut(wire) {
                        wire.set_point(
                            pos,
                            &states,
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
                    pin.write().set_wire(&states, Some(wire), false, true);
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
                pin.write()
                    .set_wire(&self.board.read().states.clone(), wire, false, true);
            }

            let mut board = self.board.write();
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
            let states = self.board.read().states.clone();

            if let Some(wire) = prev_wire {
                states.update_wire(wire, true);
            }

            if let Some(wire) = wire {
                states.update_wire(wire, true)
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
                        let board = self.board.read();
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
            let mut board = self.board.write();
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
            self.split_wires(wire, true);
        }
        if update_states {
            let states = self.board.read().states.clone();
            states.update_wire(wire, true);
        }

        Some(wire)
    }

    fn merge_wires(&mut self, wire: usize, with: usize, update_state: bool) {
        {
            let board = self.board.read();

            let with_wire = unwrap_option_or_return!(board.wires.get(with));

            if !board.wires.exists(wire) || wire == with {
                return;
            }

            let points: Vec<_> = with_wire.points.keys().cloned().collect();
            drop(board);

            self.set_node_wires(points.iter(), wire);
        }
        self.board.write().merge_wires(wire, with, update_state);
    }

    fn split_wires(&mut self, id: usize, update_states: bool) {
        let mut groups = vec![];

        let mut remaining_nodes: HashSet<_> = {
            let board = self.board.read();
            let wire = unwrap_option_or_return!(board.wires.get(id));
            wire.points.keys().copied().collect()
        };

        // empty wire
        if remaining_nodes.is_empty() {
            let mut board = self.board.write();
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
                let new_wire = self.board.write().split_wire(id, &group, false);
                if let Some(wire) = new_wire {
                    self.set_node_wires(group.iter(), wire);
                    wires.push(wire);
                }
            } else {
                wires.push(id);
            }
        }

        if update_states {
            let states = self.board.read().states.clone();
            for wire in wires {
                states.update_wire(wire, true);
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

    /// Handler is called after circuit is crated and placed but before it's connected to wires and updated
    pub fn place_circuit(
        &mut self,
        place_pos: Vec2i,
        lock_sim: bool,
        preview: &CircuitPreview,
        props_override: Option<CircuitPropertyStore>,
        handler: &dyn Fn(&mut ActiveCircuitBoard, usize),
        boards: &BoardStorage,
    ) -> Option<usize> {
        let size = preview.describe().size;
        if !self.can_place_circuit_at(size, place_pos, None) {
            return None;
        }

        let sim_lock = { self.board.read().sim_lock.clone() };
        let sim_lock = { lock_sim.then(|| sim_lock.write()) };

        let cid = {
            self.board
                .write()
                .create_circuit(place_pos, preview, props_override)
        };

        self.set_circuit_nodes(size, place_pos, Some(cid));
        handler(self, cid);
        self.connect_circuit_to_wires(cid);
        let board = self.board.read();
        let circ = board.circuits.get(cid).unwrap();
        board.states.init_circuit(circ, boards);
        board.states.update_circuit_signals(cid, None);
        drop(sim_lock);
        Some(cid)
    }

    fn connect_circuit_to_wires(&mut self, circuit: usize) {
        let board = self.board.read();
        let circuit = board.circuits.get(circuit);
        let circuit = unwrap_option_or_return!(circuit);
        let info = circuit.info.clone();
        let pos = circuit.pos;

        let states = board.states.clone();
        drop(board);

        for pin in info.read().pins.iter() {
            let pos = pos + pin.pos.convert(|v| v as i32);
            if let Some(wire) = self.create_wire_intersection(pos) {
                let mut board = self.board.write();
                let wire = if let Some(wire) = board.wires.get_mut(wire) {
                    if let Some(p) = wire.points.get_mut(&pos) {
                        p.pin = Some(pin.pin.clone());
                    }
                    Some(wire.id)
                } else {
                    None
                };

                if let Some(wire) = wire {
                    pin.pin.write().set_wire(&states, Some(wire), true, false);
                }
            } else {
                pin.pin.write().set_wire(&states, None, true, false);
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
        &mut self,
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

        let board = self.board.read();
        let circ_info = board.circuits.get(circ)?.info.clone();
        let circ_info = circ_info.read();

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
        let board = self.board.read();
        let circuit = unwrap_option_or_return!(board.circuits.get(id));

        let pos = circuit.pos;
        let info = circuit.info.clone();
        let size = info.read().size;

        drop(board);
        self.set_circuit_nodes(size, pos, None);

        let mut board = self.board.write();

        for pin in info.read().pins.iter() {
            let wire_id = pin.pin.read().connected_wire();
            let wire_id = unwrap_option_or_continue!(wire_id);
            let wire = unwrap_option_or_continue!(board.wires.get_mut(wire_id));
            let pos = pos + pin.pos.convert(|v| v as i32);
            let point = unwrap_option_or_continue!(wire.points.get_mut(&pos));
            point.pin = None;

            affected_wires.insert(wire_id);
        }

        board.circuits.remove(id);
        board.states.reset_circuit(id);
    }

    // Todo: result
    fn try_updating_circuit_property(&mut self, circuit_id: usize, property: &str) -> bool {
        let sim_lock = { self.board.read().sim_lock.clone() };
        let sim_lock = sim_lock.write();

        let board = self.board.read();
        let circuit = board.circuits.get(circuit_id);
        let circuit = unwrap_option_or_return!(circuit, false);

        let mut resize = false;
        let mut recreate_pins = false;
        circuit
            .imp
            .read()
            .prop_changed(property, &mut resize, &mut recreate_pins);

        if resize {
            let new_size = circuit.imp.read().size(&circuit.props);
            let size_changed = new_size != circuit.info.read().size;
            drop(board);

            if size_changed && !self.try_change_circuit_size(circuit_id, new_size) {
                return false;
            }
        } else {
            drop(board);
        }

        if recreate_pins {
            let mut board = self.board.write();
            let circuit = board.circuits.get(circuit_id).expect("unexpected");

            let pins: Vec<_> = circuit
                .info
                .read()
                .pins
                .iter()
                .map(|p| (p.pos.convert(|v| v as i32) + circuit.pos, p.pin.clone()))
                .collect();

            let states = board.states.clone();
            // disconnect all older pins
            for (pos, pin) in pins {
                let pin_wire = pin.read().connected_wire();
                if let Some(wire) = pin_wire {
                    if let Some(wire) = board.wires.get_mut(wire) {
                        if let Some(point) = wire.points.get_mut(&pos) {
                            point.pin = None;
                            states.update_wire(wire.id, true);
                        }
                    }
                    pin.write().set_wire(&states, None, false, false);
                }
            }

            let circuit = board.circuits.get(circuit_id).expect("unexpected");

            let new_pins = circuit.imp.write().create_pins(&circuit.props);
            for pin in new_pins.iter().enumerate() {
                pin.1.pin.write().id = CircuitPinId::new(pin.0, circuit_id);
            }
            circuit.info.write().pins = new_pins;
            drop(board);

            self.connect_circuit_to_wires(circuit_id);
        }

        let board = self.board.read();
        let circuit = board.circuits.get(circuit_id).expect("unexpected");
        circuit
            .imp
            .write()
            .apply_props(&circuit.props, Some(property));

        board.states.update_circuit_signals(circuit_id, None);

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
            let board = self.board.write();
            if let Some(circuit) = board.circuits.get(circuit) {
                circuit
                    .props
                    .write_dyn(property, |p| old_value.copy_into(p.imp_mut()));
            }
        }
    }

    // todo: result
    fn try_change_circuit_size(&mut self, circuit_id: usize, new_size: Vec2u) -> bool {
        let board = self.board.read();
        let circuit = board.circuits.get(circuit_id);
        let circuit = unwrap_option_or_return!(circuit, false);
        let circuit_pos = circuit.pos;
        let old_size = circuit.info.read().size;
        let info = circuit.info.clone();
        drop(board);

        if !self.can_place_circuit_at(new_size, circuit_pos, Some(circuit_id)) {
            return false;
        }
        self.set_circuit_nodes(old_size, circuit_pos, None);
        self.set_circuit_nodes(new_size, circuit_pos, Some(circuit_id));

        info.write().size = new_size;
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

    // pub fn current_mut(&mut self) -> &mut CustomCircuitDesign {

    //     // I don't care... I'm tired of endless borrowing errors
    //     let ptr1 = &mut self.designs as *mut FixedVec<Arc<CustomCircuitDesign>>;

    //     let current = self
    //         .designs
    //         .get_mut(self.current)
    //         .expect("current design must always exist");

    //     let ptr2 = Arc::as_ptr(current);

    //     if let Some(unique) = Arc::get_mut(current) {
    //         return unique;
    //     }

    //     unsafe {

    //         let mut new = (*ptr2).clone();
    //         let id = (*ptr1).first_free_pos();
    //         new.id = id;
    //         let arc = (*ptr1).set(Arc::new(new), id).value_ref;
    //         Arc::make_mut(arc)
    //     }
    // }

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

    fn save(&self) -> crate::io::CircuitDesignStoreData {
        crate::io::CircuitDesignStoreData {
            current: self.current,
            designs: self
                .designs
                .inner()
                .iter()
                .map(|d| {
                    d.as_ref().map(|d| crate::io::CircuitDesignData {
                        pins: d.pins.clone(),
                        size: d.size,
                        decorations: d.decorations.clone(),
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
    Rect {
        rect: Rect,
        rounding: Rounding,
        fill: Color32,
        stroke: Stroke,
    }, // TODO: MOAR!
}

enum CircuitDesignIdType {
    Unresolved(usize),
    Resolved(Option<Arc<CircuitDesign>>),
}

pub struct CircuitDesignId(RwLock<CircuitDesignIdType>);

impl CircuitDesignId {

    pub fn new_resolved(design: Option<Arc<CircuitDesign>>) -> Self {
        Self(RwLock::new(CircuitDesignIdType::Resolved(design)))
    }

    pub fn new_unresolved(id: usize) -> Self {
        Self(RwLock::new(CircuitDesignIdType::Unresolved(id)))
    }

    pub fn resolve<D>(&self, designs: impl FnOnce() -> D) -> Option<Arc<CircuitDesign>>
    where
        D: Deref<Target = CircuitDesignStorage>,
    {
        if let CircuitDesignIdType::Resolved(arc) = self.0.read().deref() {
            return arc.clone();
        }

        let mut ty = self.0.write();
        let id = match ty.deref() {
            CircuitDesignIdType::Unresolved(id) => *id,
            CircuitDesignIdType::Resolved(arc) => return arc.clone(),
        };

        let store = designs();
        let arc = store.get(id);

        *ty = CircuitDesignIdType::Resolved(arc.clone());
        arc
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CircuitDesignPin {
    pub id: DynStaticStr,
    pub pos: Vec2u,
    pub dir: InternalPinDirection,
    pub display_dir: Option<Direction4>,
    pub display_name: DynStaticStr
}

#[derive(Clone, Default)]
pub struct CircuitDesign {
    pub id: usize,

    pub size: Vec2u,
    pub pins: Vec<CircuitDesignPin>,
    pub decorations: Vec<Decoration>,
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
                rounding: Rounding::none(),
                fill: Color32::from_gray(100),
                stroke: Stroke::new(0.1, Color32::BLACK),
            }],
        }
    }
}
