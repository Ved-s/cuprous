use std::{collections::{HashSet, BTreeSet}, f32::consts::PI, num::NonZeroU32, ops::Deref, sync::Arc};

use eframe::{
    egui::{
        self, CollapsingHeader, Event, FontSelection, Frame, Id, Key, Margin, PointerButton,
        ScrollArea, Sense, TextEdit, TextStyle, Ui, WidgetText,
    },
    epaint::{Color32, FontId, PathShape, Rounding, Shape, Stroke, TextShape},
};
use emath::{pos2, vec2, Align2, Pos2, Rangef, Rect, Vec2};
use num_traits::Zero;

use crate::{
    app::{SimulationContext, Style},
    board::{
        BoardDesignProvider, BoardObjectSelectionImpl, CircuitBoard, EditableCircuitBoard,
        SelectedBoardObject, SelectedItem, StoredCircuitBoard,
    },
    circuits::{
        props::{CircuitPropertyImpl, CircuitPropertyStore},
        CircuitNode, CircuitPreview, CircuitStateContext,
    },
    containers::ChunksLookaround,
    error::{ErrorList, ResultReport},
    ext::IteratorEqExt,
    string::StringFormatterState,
    vector::{Vec2f, Vec2i},
    wires::{WireNode, WirePart, WireColors},
    ArcString, Direction2, Direction4, DynStaticStr, PaintContext, PanAndZoom, PastePreview,
    Screen,
};

use super::{
    designer::Designer,
    drawing,
    selection::{Selection, SelectionInventoryItem},
    side_panel::{PanelSide, SidePanel},
    DoubleSelectableLabel, Inventory, InventoryItem, InventoryItemGroup, PropertyEditor,
};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum SelectedItemId {
    Wires,
    Selection,
    Paste,
    Circuit(DynStaticStr),
    Board(u128),
}

#[allow(unused)]
#[derive(Debug, Clone, Copy)]
pub struct TileDrawBounds {
    pub screen_tl: Vec2f,
    pub screen_br: Vec2f,

    pub tiles_tl: Vec2i,
    pub tiles_br: Vec2i,

    pub chunks_tl: Vec2i,
    pub chunks_br: Vec2i,
}

impl TileDrawBounds {
    #[allow(unused)]
    pub const EVERYTHING: TileDrawBounds = TileDrawBounds {
        screen_tl: Vec2f::single_value(f32::NEG_INFINITY),
        screen_br: Vec2f::single_value(f32::INFINITY),
        tiles_tl: Vec2i::single_value(i32::MIN),
        tiles_br: Vec2i::single_value(i32::MAX),
        chunks_tl: Vec2i::single_value(i32::MIN),
        chunks_br: Vec2i::single_value(i32::MAX),
    };
}

pub struct EditorResponse {
    pub designer_request: Option<Designer>,
}

#[derive(Default)]
struct ComponentsUiResponse {
    designer_request: Option<Designer>,
}

#[derive(PartialEq, Eq)]
enum SelectedObjectId {
    Circuit,
    Wire,
}

struct InventoryItemDrawData {
    false_color_override: Option<Color32>
}

pub struct CircuitBoardEditor {
    pan_zoom: PanAndZoom,
    pub board: EditableCircuitBoard,

    pub debug: bool,
    errors: ErrorList,

    paste: Option<Arc<PastePreview>>,
    inventory: Arc<[InventoryItemGroup<SelectedItemId, InventoryItemDrawData>]>,
    selected_id: Option<SelectedItemId>,

    props_ui: PropertyEditor,
    sim: Arc<SimulationContext>,

    wire_drag_pos: Option<Vec2i>,
    wire_colors: WireColors,
    pub selection: Selection<BoardObjectSelectionImpl>,
}

static INVENTORY_CIRCUIT_ORDER: &[&str] = &["or", "nor", "and", "nand", "xor", "xnor", "not"];

static COMPONENT_BUILTIN_ORDER: &[&str] = &[
    "button",
    "led",
    "or",
    "nor",
    "and",
    "nand",
    "xor",
    "xnor",
    "not",
    "transistor",
    "relay",
    "pin",
    "bundler",
    "clock",
    "pullup",
    "freq_meter",
];

struct WireInventoryItem {}
impl InventoryItem<SelectedItemId, InventoryItemDrawData> for WireInventoryItem {
    fn id(&self) -> SelectedItemId {
        SelectedItemId::Wires
    }

    fn draw(&self, pass: &InventoryItemDrawData, ctx: &PaintContext) {
        let color = pass.false_color_override.unwrap_or_else(|| ctx.style.wire_colors.false_color());

        let rect1 = Rect::from_center_size(
            ctx.rect.lerp_inside([0.2, 0.2].into()),
            ctx.rect.size() * 0.2,
        );
        let rect2 = Rect::from_center_size(
            ctx.rect.lerp_inside([0.8, 0.8].into()),
            ctx.rect.size() * 0.2,
        );

        ctx.paint
            .line_segment([rect1.center(), rect2.center()], Stroke::new(2.5, color));

        ctx.paint.add(Shape::Path(PathShape {
            points: rotated_rect_shape(rect1, PI * 0.25, rect1.center()),
            closed: true,
            fill: color,
            stroke: Stroke::NONE,
        }));

        ctx.paint.add(Shape::Path(PathShape {
            points: rotated_rect_shape(rect2, PI * 0.25, rect2.center()),
            closed: true,
            fill: color,
            stroke: Stroke::NONE,
        }));
    }
}

struct CircuitInventoryItem {
    preview: Arc<CircuitPreview>,
    id: DynStaticStr,
}
impl<P> InventoryItem<SelectedItemId, P> for CircuitInventoryItem {
    fn id(&self) -> SelectedItemId {
        SelectedItemId::Circuit(self.id.clone())
    }

    fn draw(&self, _pass: &P, ctx: &PaintContext) {
        let size = self.preview.describe().size.convert(|v| v as f32);
        let scale = Vec2f::from(ctx.rect.size()) / size;
        let scale = scale.x.min(scale.y);
        let size = size * scale;
        let rect = Rect::from_center_size(ctx.rect.center(), size.into());

        let circ_ctx = PaintContext {
            screen: Screen {
                scale,
                ..ctx.screen
            },
            rect,
            ..*ctx
        };
        self.preview.draw(&circ_ctx, false);
    }
}

fn rotated_rect_shape(rect: Rect, angle: f32, origin: Pos2) -> Vec<Pos2> {
    let mut points = vec![
        rect.left_top(),
        rect.right_top(),
        rect.right_bottom(),
        rect.left_bottom(),
    ];

    let cos = angle.cos();
    let sin = angle.sin();

    for p in points.iter_mut() {
        let pl = *p - origin;

        let x = cos * pl.x - sin * pl.y;
        let y = sin * pl.x + cos * pl.y;
        *p = pos2(x, y) + origin.to_vec2();
    }

    points
}

impl CircuitBoardEditor {
    pub const WIRE_THICKNESS: f32 = 0.2;
    pub const WIRE_POINT_THICKNESS: f32 = 0.35;

    pub fn new(board: EditableCircuitBoard, ctx: &Arc<SimulationContext>) -> Self {
        let inventory_group: Vec<_> = INVENTORY_CIRCUIT_ORDER
            .iter()
            .filter_map(|id| ctx.previews.get(*id))
            .map(|preview| {
                Box::new(CircuitInventoryItem {
                    preview: preview.clone(),
                    id: preview.imp.type_name(),
                }) as Box<dyn InventoryItem<SelectedItemId, InventoryItemDrawData>>
            })
            .collect();

        Self {
            pan_zoom: PanAndZoom::default(),
            board,
            debug: false,
            errors: ErrorList::new(),
            paste: None,
            inventory: vec![
                InventoryItemGroup::SingleItem(Box::new(SelectionInventoryItem::new(
                    SelectedItemId::Selection,
                ))),
                InventoryItemGroup::SingleItem(Box::new(WireInventoryItem {})),
                InventoryItemGroup::Group(inventory_group),
            ]
            .into(),
            selected_id: None,
            props_ui: PropertyEditor::new(),
            sim: ctx.clone(),
            wire_drag_pos: None,
            wire_colors: Default::default(),
            selection: Selection::default(),
        }
    }

    pub fn background_update(&mut self, style: &Style, ui: &mut Ui) {
        let rect = ui.max_rect();
        self.pan_zoom.update(ui, rect, self.selected_id.is_none());

        if !ui.ctx().wants_keyboard_input() {
            cfg_if::cfg_if! {
                if #[cfg(all(not(web_sys_unstable_apis), feature = "wasm"))] {
                    let paste = ui
                        .input(|input| input.modifiers.ctrl && input.key_pressed(egui::Key::V))
                        .then(|| crate::io::GLOBAL_CLIPBOARD.lock().clone())
                        .flatten();
                } else {
                    let paste = ui.input(|input| {
                        input
                            .events
                            .iter()
                            .find_map(|e| match e {
                                egui::Event::Paste(s) => Some(s),
                                _ => None,
                            })
                            .and_then(|p| ron::from_str::<crate::io::CopyPasteData>(p).report_error(&mut self.errors.enter_context(|| "loading paste")))
                    });
                }
            }

            if let Some(paste) = paste {
                self.paste = Some(Arc::new(PastePreview::new(
                    paste,
                    &self.sim,
                    &mut self.errors,
                )));
                self.selected_id = Some(SelectedItemId::Paste);
            }

            if !self.selection.selection.is_empty() {
                cfg_if::cfg_if! {
                    if #[cfg(all(not(web_sys_unstable_apis), feature = "wasm"))] {
                        let (copy, cut) = ui.input(|input|
                                (
                                    input.modifiers.ctrl && input.key_pressed(egui::Key::C),
                                    input.modifiers.ctrl && input.key_pressed(egui::Key::X),
                                )
                            );
                    } else {
                        let (copy, cut) = ui.input(|input| {
                            input
                                .events
                                .iter()
                                .fold((false, false), |(copy, cut), e| (
                                    copy || matches!(e, egui::Event::Copy),
                                    cut || matches!(e, egui::Event::Cut)
                                ))
                        });
                    }
                }
                if copy || cut {
                    let min_pos = {
                        let mut min_pos = None;
                        for obj in self.selection.selection.iter() {
                            let pos = match obj {
                                SelectedBoardObject::WirePart { pos, dir } => {
                                    let node = self.board.find_wire_node(*pos, (*dir).into());
                                    let node = unwrap_option_or_continue!(node);
                                    node.pos
                                }
                                SelectedBoardObject::Circuit { id } => {
                                    let pos =
                                        self.board.board.circuits.read().get(*id).map(|c| c.pos);
                                    unwrap_option_or_continue!(pos)
                                }
                            };
                            min_pos = match min_pos {
                                None => Some(pos),
                                Some(mp) => Some([mp.x.min(pos.x), mp.y.min(pos.y)].into()),
                            }
                        }
                        min_pos
                    };
                    if let Some(min_pos) = min_pos {
                        let mut copy = crate::io::CopyPasteData::default();
                        for obj in self.selection.selection.iter() {
                            match obj {
                                SelectedBoardObject::WirePart { pos, dir } => {
                                    if let Some(w) = self.board.find_wire_node(*pos, (*dir).into())
                                    {
                                        let colors = self.board.board.wires.read().get(w.wire).map(|w| w.colors).unwrap_or_default();
                                        copy.wires.push(crate::io::WirePartCopyData {
                                            pos: (*pos - min_pos).convert(|v| v as u32),
                                            length: w.distance.get(),
                                            colors,
                                            dir: *dir,
                                        })
                                    }
                                }
                                SelectedBoardObject::Circuit { id } => {
                                    let circuits = self.board.board.circuits.read();
                                    if let Some(circuit) = circuits.get(*id) {
                                        let pos = (circuit.pos - min_pos).convert(|v| v as u32);
                                        copy.circuits
                                            .push(circuit.copy(pos, self.board.state.as_ref()))
                                    }
                                }
                            }
                        }
                        cfg_if::cfg_if! {
                            if #[cfg(all(not(web_sys_unstable_apis), feature = "wasm"))] {
                                *crate::io::GLOBAL_CLIPBOARD.lock() = Some(copy);
                            } else {
                                let copy_text = ron::to_string(&copy).unwrap();
                                ui.output_mut(|output| output.copied_text = copy_text);
                            }
                        }
                    }
                }

                if cut || ui.input(|input| input.key_pressed(egui::Key::Delete)) {
                    let mut affected_wires = HashSet::new();
                    let drain = self.selection.selection.drain().collect::<Vec<_>>();

                    let sim_lock = { self.board.board.sim_lock.clone() };
                    let sim_lock = sim_lock.write();
                    for obj in drain {
                        match obj {
                            SelectedBoardObject::Circuit { id } => {
                                self.board.remove_circuit(id, &mut affected_wires);
                            }
                            SelectedBoardObject::WirePart { pos, dir } => {
                                if let Some(wire) =
                                    self.board.remove_wire_part(pos, dir.into(), true, false)
                                {
                                    affected_wires.insert(wire);
                                }
                            }
                        }
                    }

                    for wire in affected_wires {
                        self.board.board.states.update_wire(wire, true);
                    }
                    drop(sim_lock)
                }
            }

            if ui.input(|input| input.key_pressed(Key::F9)) {
                self.debug = !self.debug;
            } else if ui.input(|input| input.key_pressed(Key::F8)) {
                let state = self.board.state.clone();
                self.board = EditableCircuitBoard::new(state);
            } else if ui.input(|input| input.key_pressed(Key::F2)) {
                if ui.input(|input| input.modifiers.shift) {
                    for board in self.board.board.ctx.boards.read().values() {
                        for state in board.board.states.states.read().iter() {
                            state.reset();
                        }
                    }
                    for board in self.board.board.ctx.boards.read().values() {
                        for state in board.board.states.states.read().iter() {
                            state.update_everything();
                        }
                    }
                } else if ui.input(|input| input.modifiers.command) {
                    for state in self.board.board.states.states.read().iter() {
                        state.reset();
                        state.update_everything();
                    }
                } else {
                    let state = &self.board.state;
                    state.reset();
                    state.update_everything();
                }
            } else if ui.input(|input| input.key_pressed(Key::Q)) {
                let sim_lock = self.board.board.sim_lock.clone();
                let sim_lock = sim_lock.write();

                let ordered = self.board.board.is_ordered_queue();
                self.board.board.set_ordered_queue(!ordered, false);
                drop(sim_lock);
            } else if ui.input(|input| input.key_pressed(Key::P)) {
                self.board.state.set_frozen(!self.board.state.is_frozen());
            }

            let sequence = ui
                .data(|data| data.get_temp(Id::from("sequence")))
                .unwrap_or(0);
            let seq_keys = [Key::Num2, Key::Num4, Key::Num9, Key::Num7];

            let key = ui.input(|input| {
                input
                    .events
                    .iter()
                    .filter_map(|e| match e {
                        Event::Key {
                            key, pressed: true, ..
                        } => Some(*key),
                        _ => None,
                    })
                    .next()
            });

            if let Some(key) = key {
                let seq_key = seq_keys.get(sequence).copied();
                match seq_key {
                    Some(seq_key) if seq_key == key => {
                        ui.data_mut(|data| data.insert_temp(Id::from("sequence"), sequence + 1));
                        if sequence == seq_keys.len() - 1 {
                            self.selected_id = Some(SelectedItemId::Circuit("gate2497".into()))
                        }
                    }
                    _ => ui.data_mut(|data| data.insert_temp(Id::from("sequence"), 0)),
                }
            }
        }
        let selected_item = self.selected_item();

        if !ui.ctx().wants_keyboard_input() {
            if let Some(selected) = &selected_item {
                if ui.input(|input| input.key_pressed(Key::R)) {
                    self.change_selected_props(selected, "dir", |d: &mut Direction4| {
                        *d = d.rotate_clockwise()
                    });
                    self.change_selected_props(selected, "label_dir", |d: &mut Direction4| {
                        *d = d.rotate_clockwise()
                    });
                    self.swap_selected_size(selected);
                }

                if ui.input(|input| input.key_pressed(Key::F)) {
                    self.change_selected_props(selected, "flip", |f: &mut bool| *f = !*f);
                }
            }
        }

        let screen = self.pan_zoom.to_screen(rect);
        let paint = ui.painter_at(rect);
        drawing::draw_dynamic_grid(&screen, style, 16.0, 16.into(), &paint);
        drawing::draw_cross(&screen, style, rect, &paint);

        let ctx = PaintContext {
            screen,
            style,
            paint: &paint,
            rect,
            ui,
        };

        let tile_bounds = self.calc_draw_bounds(&screen);

        self.selection.pre_update_selection(
            &self.board,
            &ctx,
            matches!(&selected_item, Some(SelectedItem::Selection)),
        );

        ctx.draw_chunks(
            tile_bounds,
            &self.board.wire_nodes,
            &self,
            |node| !node.is_empty(),
            |node, pos, ctx, this, lookaround| {
                this.draw_wire_node(tile_bounds, ctx, node, pos, lookaround);
            },
        );

        if self.debug {
            ctx.draw_chunks(
                tile_bounds,
                &self.board.wire_nodes,
                &self,
                |node| !node.is_empty(),
                |node, pos, ctx, this, lookaround| {
                    this.draw_wire_node_debug(ctx, node, pos, lookaround);
                },
            );
        }

        ctx.draw_chunks(
            tile_bounds,
            &self.board.circuit_nodes,
            &*self,
            |n| n.circuit.is_some(),
            |node, pos, ctx, this, _| this.draw_circuit_node(tile_bounds, node, pos, ctx),
        );

        self.update_wires(&ctx, matches!(&selected_item, Some(SelectedItem::Wire)));

        self.draw_hovered_circuit_pin_names(&ctx);

        self.update_previews(&ctx);
        self.selection.update_selection(&self.board, &ctx);

        if let Some(SelectedItemId::Board(board_id)) = self.selected_id {
            if let Some(board) = self.sim.boards.write().get_mut(&board_id) {
                if board.preview.is_none() {
                    let preview =
                        crate::circuits::board::BoardPreview::new_from_board(board.board.clone());
                    let preview =
                        CircuitPreview::new(Box::new(preview), CircuitPropertyStore::default());
                    board.preview = Some(Arc::new(preview));
                }
            }
        }
    }

    pub fn ui_update(&mut self, style: &Style, ui: &mut Ui) -> EditorResponse {
        let components_response = self.components_ui(style, ui);
        self.properties_ui(style, ui);

        {
            let rect = crate::ui::side_panel::remaining_rect(ui).shrink(10.0);
            let mut ui = ui.child_ui(rect, *ui.layout());

            if ui.input(|input| input.key_pressed(Key::Escape))
                || ui.input(|input| {
                    input.pointer.button_released(PointerButton::Secondary)
                        && !input.pointer.is_decidedly_dragging()
                })
            {
                self.selected_id = None;
                self.selection.clear();
            }

            let draw_data = InventoryItemDrawData {
                false_color_override: self.wire_colors.r#false,
            };

            Inventory::new(&mut self.selected_id, &self.inventory).ui(
                &mut ui,
                &draw_data,
                style,
                |id| match id {
                    SelectedItemId::Wires => Some("Wires".into()),
                    SelectedItemId::Selection => Some("Selection".into()),
                    SelectedItemId::Paste => Some("Paste".into()),
                    SelectedItemId::Circuit(cid) => {
                        self.sim.previews.get(cid).map(|p| p.imp.display_name())
                    }
                    SelectedItemId::Board(id) => self
                        .sim
                        .boards
                        .read()
                        .get(id)
                        .map(|b| b.board.name.read().get_arc().into()),
                },
            );

            match (
                self.selected_id == Some(SelectedItemId::Paste),
                self.paste.is_some(),
            ) {
                (true, true) => (),
                (true, false) => self.selected_id = None,
                (_, true) => self.paste = None,
                _ => (),
            }

            let queue_len = self.board.state.queue_len();
            let states = self.board.board.states.states.read().iter().count();
            let frozen_states = self
                .board
                .board
                .states
                .states
                .read()
                .iter()
                .filter(|s| s.is_frozen())
                .count();
            let unused_states = self
                .board
                .board
                .states
                .states
                .read()
                .iter()
                .filter(|s| !s.is_being_used())
                .count();

            let text = format!("This board has {states} state(s), {frozen_states} are frozen, {unused_states} will be removed\nCurrent queue length: {queue_len}");

            ui.monospace(text);
        }

        if !self.errors.is_empty() {
            let mut open = true;
            egui::Window::new("Errors")
                .anchor(Align2::CENTER_CENTER, vec2(0.0, 0.0))
                .fixed_pos(ui.max_rect().center())
                .resizable(false)
                .open(&mut open)
                .vscroll(true)
                .collapsible(false)
                .show(ui.ctx(), |ui| {
                    self.errors.show_ui(ui);
                });
            if !open {
                self.errors.clear();
            }
        }

        EditorResponse {
            designer_request: components_response.designer_request,
        }
    }

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

        let drawing_wire =
            EditableCircuitBoard::calc_wire_part(self.wire_drag_pos, mouse_tile_pos_i);
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
                self.board.place_wire_part(part, true, self.wire_colors);
            }
        }

        if let Some(mouse_pos) = mouse_tile_pos_i {
            if interaction.clicked_by(egui::PointerButton::Primary) && self.wire_drag_pos.is_none()
            {
                self.board.try_toggle_node_intersection(mouse_pos);
            }
        }
    }

    fn update_previews(&mut self, ctx: &PaintContext) {
        let selected = self.selected_item();

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
            if size.x == 0 || size.y == 0 {
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
                    Self::WIRE_THICKNESS * 0.5 * ctx.screen.scale,
                    ctx.style.wire_colors.false_color(),
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
                fn empty_handler(_: &mut EditableCircuitBoard, _: usize) {}
                self.board.place_circuit(
                    place_pos,
                    true,
                    p.as_ref(),
                    None,
                    false,
                    None,
                    &mut empty_handler,
                );
            }
        } else if let SelectedItem::Paste(p) = selected {
            let size = p.size;
            if size.x == 0 || size.y == 0 {
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
                p.place(&mut self.board, place_pos, &mut self.errors);
            }
        }
    }

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

    fn draw_hovered_circuit_pin_names(&self, ctx: &PaintContext) {
        let mouse_tile_pos = ctx
            .ui
            .input(|input| input.pointer.interact_pos())
            .map(|p| ctx.screen.screen_to_world(Vec2f::from(p)));

        let mouse_tile_pos = unwrap_option_or_return!(mouse_tile_pos);
        let mouse_tile_pos = mouse_tile_pos.convert(|v| v.floor() as isize);
        let node = self.board.circuit_nodes.get(mouse_tile_pos);
        let node = unwrap_option_or_return!(node);
        let circuit = unwrap_option_or_return!(node.circuit.get());

        let pos = mouse_tile_pos - node.origin_dist.convert(|v| v as isize);
        let info = self
            .board
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

    fn draw_wire_node(
        &self,
        bounds: TileDrawBounds,
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

        fn draw_wire(
            bounds: TileDrawBounds,
            info: WireDrawInfo,
            this: &CircuitBoardEditor,
            ctx: &PaintContext,
        ) {
            if info.dist.is_none() && info.wire.is_none() {
                return;
            }

            let edge = match info.dir {
                Direction2::Up => info.pos.y == bounds.tiles_br.y,
                Direction2::Left => info.pos.x == bounds.tiles_br.x,
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
        let wires = self.board.wires_at_node(pos, node);

        let center_color = node
            .wire
            .get()
            .map(|w| self.board.state.get_wire_color(w, ctx.style));

        for dir in Direction2::iter_all() {
            let wire_color = center_color.or_else(|| {
                wires
                    .dir(dir.into())
                    .map(|w| self.board.state.get_wire_color(w, ctx.style))
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
                let wires = self.board.board.wires.read();
                if let Some(wire) = wires.get(wire) {
                    Self::draw_wire_point(
                        ctx,
                        pos,
                        wire.color(&self.board.state, ctx.style),
                        self.debug && wire.points.get(&pos).is_some_and(|p| p.pin.is_some()),
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
            let wires = self.board.board.wires.read();
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
            || pos.x == bounds.tiles_tl.x && pos.y == bounds.tiles_tl.y
            || node.origin_dist.y == 0 && pos.x == bounds.tiles_tl.x
            || node.origin_dist.x == 0 && pos.y == bounds.tiles_tl.y)
        {
            return;
        }
        let circ_id = unwrap_option_or_return!(node.circuit.get());

        let circuits = self.board.board.circuits.read();
        let circuit = unwrap_option_or_return!(circuits.get(circ_id));

        let circ_info = circuit.info.read();

        let pos = pos - node.origin_dist.convert(|v| v as i32);
        let screen_pos = ctx.screen.world_to_screen_tile(pos);
        let screen_size = circ_info.size.convert(|v| v as f32) * ctx.screen.scale;
        let rect = Rect::from_min_size(screen_pos.into(), screen_size.into());
        let circ_ctx = ctx.with_rect(rect);

        let imp = circuit.imp.read();
        let state_ctx = CircuitStateContext::new(self.board.state.clone(), circuit.clone());

        if imp.draw_pin_points() {
            for pin in circ_info.pins.iter() {
                let pos = circuit.pos + pin.pos.convert(|v| v as i32);
                let pin = pin.pin.read();
                if pin.connected_wire().is_some() {
                    continue;
                }
                let color = pin
                    .get_state(&state_ctx.global_state)
                    .color(ctx.style, None);
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
                    Id::new((self.board.board.uid, circuit.pos, i)),
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
                let color = visual.text_color();
                ctx.paint.add(TextShape {
                    pos,
                    galley,
                    underline: Stroke::NONE,
                    override_text_color: Some(color),
                    fallback_color: color,
                    angle: 0.0,
                });
            }
        }
    }

    fn calc_draw_bounds(&self, screen: &Screen) -> TileDrawBounds {
        let chunk_size: Vec2f = (screen.scale * 16.0).into();

        let screen_tl = screen.wld_pos * screen.scale;
        let screen_br = screen_tl + screen.scr_rect.size();

        TileDrawBounds {
            screen_tl,
            screen_br,

            tiles_tl: (screen_tl / screen.scale).convert(|v| v.floor() as i32),
            tiles_br: (screen_br / screen.scale).convert(|v| v.floor() as i32),

            chunks_tl: (screen_tl / chunk_size).convert(|v| v.floor() as i32),
            chunks_br: (screen_br / chunk_size).convert(|v| v.floor() as i32),
        }
    }

    fn change_selected_props<T: CircuitPropertyImpl>(
        &mut self,
        selected_item: &SelectedItem,
        id: &str,
        f: impl Fn(&mut T),
    ) {
        if let SelectedItem::Circuit(pre) = selected_item {
            pre.props.write(id, f);
            pre.redescribe();
        } else {
            let selected_circuits: Vec<_> = self
                .selection
                .selection
                .iter()
                .filter_map(|o| match o {
                    SelectedBoardObject::Circuit { id } => Some(*id),
                    _ => None,
                })
                .collect();

            let mut vec = vec![];
            let circuits = self.board.board.circuits.read();
            for circuit_id in selected_circuits {
                let circuit = circuits.get(circuit_id);
                let circuit = unwrap_option_or_continue!(circuit);
                let old = circuit.props.write(id, |p: &mut T| {
                    let old = p.clone();
                    f(p);
                    old
                });
                let old = unwrap_option_or_continue!(old);
                vec.push((circuit_id, old))
            }
            drop(circuits);
            for (circuit, old) in vec {
                self.board
                    .circuit_property_changed(circuit, id, old.as_ref());
            }
        }
    }

    fn swap_selected_size(&mut self, selected_item: &SelectedItem) {
        fn swap_props(props: &CircuitPropertyStore) -> Option<[Box<dyn CircuitPropertyImpl>; 2]> {
            let width = props.read_dyn("width", |p| p.imp().clone());
            let width = unwrap_option_or_return!(width, None);
            let height = props.read_dyn("height", |p| p.imp().clone());
            let height = unwrap_option_or_return!(height, None);
            if width.type_id() != height.type_id() {
                return None;
            }
            props.write_dyn("width", |p| height.copy_into(p.imp_mut()));
            props.write_dyn("height", |p| width.copy_into(p.imp_mut()));
            Some([width, height])
        }

        if let SelectedItem::Circuit(pre) = selected_item {
            if swap_props(&pre.props).is_some() {
                pre.redescribe();
            }
        } else {
            let selected_circuits: Vec<_> = self
                .selection
                .selection
                .iter()
                .filter_map(|o| match o {
                    SelectedBoardObject::Circuit { id } => Some(*id),
                    _ => None,
                })
                .collect();

            let mut vec = vec![];
            let circuits = self.board.board.circuits.read();
            for circuit_id in selected_circuits {
                let circuit = circuits.get(circuit_id);
                let circuit = unwrap_option_or_continue!(circuit);
                let old = swap_props(&circuit.props);
                let old = unwrap_option_or_continue!(old);
                vec.push((circuit.clone(), old))
            }
            drop(circuits);
            for (circuit, [oldw, oldh]) in vec {
                let width = self
                    .board
                    .circuit_property_changed(circuit.id, "width", oldw.as_ref());
                if !width {
                    circuit
                        .props
                        .write_dyn("height", |p| oldh.copy_into(p.imp_mut()));
                    continue;
                }

                let height =
                    self.board
                        .circuit_property_changed(circuit.id, "height", oldh.as_ref());
                if !height {
                    circuit
                        .props
                        .write_dyn("width", |p| oldw.copy_into(p.imp_mut()));
                    self.board
                        .circuit_property_changed(circuit.id, "width", oldh.as_ref()); // width was successfully set to height before
                    continue;
                }
            }
        }
    }

    fn selected_item(&self) -> Option<SelectedItem> {
        self.selected_id.as_ref().and_then(|sel| match sel {
            SelectedItemId::Paste => self.paste.as_ref().map(|p| SelectedItem::Paste(p.clone())),
            SelectedItemId::Selection => Some(SelectedItem::Selection),
            SelectedItemId::Wires => Some(SelectedItem::Wire),
            SelectedItemId::Circuit(circ) => self
                .sim
                .previews
                .get(circ)
                .map(|p| SelectedItem::Circuit(p.clone())),
            SelectedItemId::Board(id) => self
                .sim
                .boards
                .read()
                .get(id)
                .and_then(|b| b.preview.clone())
                .map(SelectedItem::Circuit),
        })
    }

    fn components_ui(&mut self, style: &Style, ui: &mut Ui) -> ComponentsUiResponse {
        fn builtin_ui(this: &mut CircuitBoardEditor, style: &Style, ui: &mut Ui) {
            let font = TextStyle::Monospace.resolve(ui.style());
            for name in COMPONENT_BUILTIN_ORDER {
                if let Some(preview) = this.sim.previews.get(&DynStaticStr::Static(name)) {
                    ui.horizontal(|ui| {
                        let resp = ui.allocate_response(vec2(font.size, font.size), Sense::hover());
                        let (rect, scale) = drawing::align_rect_scaled(
                            resp.rect.min,
                            vec2(font.size, font.size),
                            preview.describe().size.convert(|v| v as f32).into(),
                        );

                        let paint_ctx = PaintContext::new_on_ui(ui, style, rect, scale);
                        preview.draw(&paint_ctx, false);

                        let selected = match &this.selected_id {
                            Some(SelectedItemId::Circuit(id)) => *id == preview.imp.type_name(),
                            _ => false,
                        };

                        let resp =
                            ui.selectable_label(selected, preview.imp.display_name().deref());

                        if resp.clicked() {
                            this.selected_id = match selected {
                                true => None,
                                false => Some(SelectedItemId::Circuit(preview.imp.type_name())),
                            };
                        }

                        let style = ui.ctx().style();
                        ui.ctx().style_mut(|style| {
                            style.visuals.window_fill =
                                style.visuals.window_fill.linear_multiply(0.9);
                            style.visuals.window_stroke.color =
                                style.visuals.window_stroke.color.linear_multiply(0.9);
                        });

                        resp.on_hover_ui(|ui| {
                            ui.label(preview.imp.description().deref());
                        });

                        ui.ctx().set_style(style);
                    });
                }
            }
        }

        fn boards_ui(this: &mut CircuitBoardEditor, ui: &mut Ui) -> Option<Designer> {
            let renamer_memory_id = ui.id().with("__renamer_memory");
            let renamer_id = ui.id().with("__renamer_input");
            let rename = ui
                .memory(|mem| mem.data.get_temp::<Option<u128>>(renamer_memory_id))
                .flatten();

            let mut queued_deletion = None;
            let mut drawn_renamer = false;
            let mut designer_request = None;
            let no_delete = this.sim.boards.read().len() <= 1;
            for board in this.sim.boards.read().values() {
                if Some(board.board.uid) == rename && !drawn_renamer {
                    let mut name = board.board.name.write();

                    let res = TextEdit::singleline(name.get_mut()).id(renamer_id).show(ui);
                    drawn_renamer = true;

                    if res.response.lost_focus() {
                        ui.memory_mut(|mem| {
                            mem.data.insert_temp(renamer_memory_id, None::<u128>);
                        });
                    }
                } else {
                    let selected = this.selected_id == Some(SelectedItemId::Board(board.board.uid));
                    let active = board.board.uid == this.board.board.uid;

                    let resp = ui.add(DoubleSelectableLabel::new(
                        selected,
                        active,
                        board.board.name.read().get_str().deref(),
                        Color32::WHITE.gamma_multiply(0.3),
                        None,
                        Stroke::new(1.0, Color32::LIGHT_GREEN),
                    ));

                    if resp.clicked_by(egui::PointerButton::Primary) && !selected && !active {
                        this.selected_id = Some(SelectedItemId::Board(board.board.uid));

                        if let Some(preview) = board.preview.as_ref() {
                            preview.redescribe();
                        }
                    }

                    if resp.double_clicked_by(egui::PointerButton::Primary) && !active {
                        this.board = EditableCircuitBoard::new_main(board.board.clone());
                        if selected {
                            this.selected_id = None;
                        }
                    }

                    resp.context_menu(|ui| {
                        if ui.button("Edit").clicked() {
                            this.board = EditableCircuitBoard::new_main(board.board.clone());
                            if selected {
                                this.selected_id = None;
                            }
                            ui.close_menu();
                        }

                        if ui.button("Design").clicked() {
                            let design_provider = BoardDesignProvider::new(board.board.clone());
                            let designer = Designer::new(Box::new(design_provider));
                            designer_request = Some(designer);
                            ui.close_menu();
                        }

                        if ui.button("Rename").clicked() {
                            // same hack as below
                            if !drawn_renamer {
                                TextEdit::singleline(&mut "").id(renamer_id).show(ui);
                            }

                            ui.memory_mut(|mem| {
                                mem.data
                                    .insert_temp(renamer_memory_id, Some(board.board.uid));
                                mem.request_focus(renamer_id);
                            });
                            ui.close_menu();
                        }

                        if !no_delete {
                            if ui.input(|input| input.modifiers.shift) {
                                if ui.button("Delete").clicked() {
                                    queued_deletion = Some(board.board.uid);
                                    ui.close_menu();
                                }
                            } else {
                                ui.menu_button("Delete", |ui| {
                                    if ui.button("Confirm").clicked() {
                                        queued_deletion = Some(board.board.uid);
                                        ui.close_menu();
                                    }
                                });
                            }
                        }
                    });
                }
            }

            if ui.button("Add board").clicked() {
                let board = CircuitBoard::new(this.sim.clone(), "New board");
                let uid = board.uid;
                let board = Arc::new(board);
                this.sim
                    .boards
                    .write()
                    .insert(uid, StoredCircuitBoard::new(board.clone()));
                this.board = EditableCircuitBoard::new_main(board);

                // HACK: widget must exist before `request_focus` can be called on its id, panics otherwise
                if !drawn_renamer {
                    TextEdit::singleline(&mut "").id(renamer_id).show(ui);
                }

                ui.memory_mut(|mem| {
                    mem.data.insert_temp(renamer_memory_id, Some(uid));
                    mem.request_focus(renamer_id);
                });
            }

            if let Some(uid) = queued_deletion {
                let mut boards = this.sim.boards.write();
                if let Some(board) = boards.remove(&uid) {
                    board.board.destroy();
                };
                if this.board.board.uid == uid {
                    let board = boards.values().next().expect("Boards must exist!");
                    this.board = EditableCircuitBoard::new_main(board.board.clone());
                }
            }

            designer_request
        }

        fn inner_ui(
            this: &mut CircuitBoardEditor,
            style: &Style,
            ui: &mut Ui,
        ) -> ComponentsUiResponse {
            ScrollArea::vertical()
                .show(ui, |ui| {
                    CollapsingHeader::new("Built-in")
                        .default_open(true)
                        .show(ui, |ui| builtin_ui(this, style, ui));

                    let boards_resp = CollapsingHeader::new("Circuit boards")
                        .default_open(true)
                        .show(ui, |ui| boards_ui(this, ui));

                    ComponentsUiResponse {
                        designer_request: boards_resp.body_returned.flatten(),
                    }
                })
                .inner
        }

        let egui_style = ui.style().clone();
        SidePanel::new(PanelSide::Left, "components-ui")
            .frame(
                Frame::side_top_panel(&egui_style)
                    .rounding(Rounding {
                        ne: 5.0,
                        nw: 0.0,
                        se: 5.0,
                        sw: 0.0,
                    })
                    .outer_margin(Margin::symmetric(0.0, 8.0))
                    .inner_margin(Margin::symmetric(5.0, 5.0))
                    .stroke(egui_style.visuals.window_stroke),
            )
            .show_separator_line(false)
            .default_tab(Some(0))
            .show(
                ui,
                1,
                |_| "Components".into(),
                |_, ui| inner_ui(self, style, ui),
            )
            .inner
            .unwrap_or_default()
    }

    fn properties_ui(&mut self, style: &Style, ui: &mut Ui) {
        let selected_item = self.selected_item();
        let selected_item = if selected_item
            .as_ref()
            .is_some_and(|s| matches!(s, SelectedItem::Circuit(_) | SelectedItem::Wire))
        {
            selected_item
        } else {
            None
        };

        let selected_object = selected_item
            .is_none()
            .then(|| {
                self.selection
                    .iter()
                    .map(|s| match s {
                        SelectedBoardObject::WirePart { .. } => SelectedObjectId::Wire,
                        SelectedBoardObject::Circuit { .. } => SelectedObjectId::Circuit,
                    })
                    .same()
            })
            .flatten();

        let tabs = if selected_item.is_none() && selected_object.is_none() {
            0
        } else {
            1
        };
        let egui_style = ui.style().clone();
        SidePanel::new(PanelSide::Right, "prop-ui")
            .frame(
                Frame::side_top_panel(&egui_style)
                    .rounding(Rounding {
                        nw: 5.0,
                        ne: 0.0,
                        sw: 5.0,
                        se: 0.0,
                    })
                    .outer_margin(Margin::symmetric(0.0, 8.0))
                    .inner_margin(Margin::symmetric(5.0, 5.0))
                    .stroke(egui_style.visuals.window_stroke),
            )
            .show_separator_line(false)
            .default_tab(Some(0))
            .size_range(Rangef::new(120.0, f32::MAX))
            .show(
                ui,
                tabs,
                |_| "Properties editor".into(),
                |_, ui| {
                    if let Some(item) = selected_item {
                        match item {
                            SelectedItem::Selection | SelectedItem::Paste(_) => unreachable!(),
                            SelectedItem::Circuit(p) => {
                                let props = [((), &p.props).into()];
                                let changed = !self.props_ui.ui(ui, props).changes.is_empty();
                                if changed {
                                    p.redescribe();
                                }
                            }
                            SelectedItem::Wire => Self::wire_color_properties(style, ui, [&mut self.wire_colors]),
                        }
                    } else if let Some(obj) = selected_object {
                        match obj {
                            SelectedObjectId::Circuit => self.selected_circuit_properties(ui),
                            SelectedObjectId::Wire => {
                                let mut wire_ids = BTreeSet::new();
                                for object in self.selection.selection.iter() {
                                    let (pos, dir) = match object {
                                        SelectedBoardObject::WirePart { pos, dir } => (pos, dir),
                                         _ => continue,
                                    };
                                    let wire = self.board.wires_at(*pos).dir(Direction4::from(*dir));
                                    let wire = unwrap_option_or_continue!(wire);
                                    wire_ids.insert(wire);
                                }

                                let wires = &mut self.board.board.wires.write();
                                let iter = wires.iter_mut().filter(|w| wire_ids.contains(&w.id)).map(|w| &mut w.colors);
                                Self::wire_color_properties(style, ui, iter);
                            },
                        }
                    }
                },
            );
    }

    fn wire_color_properties<'a>(style: &Style, ui: &mut Ui, props: impl IntoIterator<Item = &'a mut WireColors>) {
        let mut props = props.into_iter();
        let first = props.next();
        let first = unwrap_option_or_return!(first);

        let changes = first.ui(Some(style.wire_colors), ui);

        if !changes.is_empty() {
            let value = *first;
            for prop in props {
                if changes.none {
                    prop.none = value.none;
                }
                if changes.r#false {
                    prop.r#false = value.r#false;
                }
                if changes.r#true {
                    prop.r#true = value.r#true;
                }
                if changes.error {
                    prop.error = value.error;
                }
                if changes.bundle {
                    prop.bundle = value.bundle;
                }
            }
        }
    }

    fn selected_circuit_properties(&mut self, ui: &mut Ui) {
        let selected_circuit_props = self.selection.selection.iter().filter_map(|o| match o {
            SelectedBoardObject::Circuit { id } => Some(*id),
            _ => None,
        });
        let board = self.board.board.clone();
        let circuits = board.circuits.read();
        let stores: Vec<_> = selected_circuit_props
            .filter_map(|id| circuits.get(id).map(|c| (id, &c.props).into()))
            .collect();

        let response = self.props_ui.ui(ui, stores);

        for property in response.changes {
            let str_arc = property
                .new
                .downcast_ref::<ArcString>()
                .map(|s| s.get_arc());
            let mut formatter = str_arc.as_ref().map(|str| StringFormatterState::new(str));
            if let Some(formatter) = &mut formatter {
                let affected_circuits: HashSet<_> =
                    HashSet::from_iter(property.affected_values.iter().map(|(id, _)| *id));

                for circuit in circuits.iter() {
                    if affected_circuits.contains(&circuit.id) {
                        continue;
                    }
                    circuit.props.read(&property.id, |s: &ArcString| {
                        formatter.add_evironment_string(&s.get_str());
                    });
                }

                let mut affected_circuits: Vec<_> = property
                    .affected_values
                    .iter()
                    .filter_map(|(id, _)| circuits.get(*id).cloned())
                    .collect();
                affected_circuits.sort_by_key(|c| c.id);

                for circuit in affected_circuits {
                    circuit.props.write(&property.id, |s: &mut ArcString| {
                        formatter.process_string(s.get_mut());
                    });
                }
            }
            for (circuit, old) in property.affected_values {
                self.board
                    .circuit_property_changed(circuit, &property.id, old.as_ref());
            }
        }
    }
}
