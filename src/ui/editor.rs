use std::{collections::HashSet, f32::consts::PI, ops::Deref, sync::Arc};

use eframe::{
    egui::{
        self, CollapsingHeader, Event, Frame, Id, Key, Margin, PointerButton, Sense, TextEdit,
        TextStyle, Ui,
    },
    epaint::{Color32, PathShape, Rounding, Shape, Stroke},
};
use emath::{pos2, vec2, Align2, Pos2, Rangef, Rect};

use crate::{
    app::SimulationContext,
    board::{
        ActiveCircuitBoard, BoardDesignProvider, CircuitBoard, SelectedBoardObject, SelectedItem,
        StoredCircuitBoard,
    },
    circuits::{
        props::{CircuitPropertyImpl, CircuitPropertyStore},
        CircuitPreview,
    },
    error::{ErrorList, ResultReport},
    state::WireState,
    string::StringFormatterState,
    vector::{Vec2f, Vec2i},
    ArcString, Direction4, DynStaticStr, PaintContext, PanAndZoom, PastePreview, Screen,
};

use super::{
    designer::Designer,
    drawing,
    selection::SelectionInventoryItem,
    side_panel::{PanelSide, SidePanel},
    DoubleSelectableLabel, Inventory, InventoryItem, InventoryItemGroup, PropertyEditor,
    PropertyStoreItem,
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

struct ComponentsUiResponse {
    designer_request: Option<Designer>,
}

pub struct CircuitBoardEditor {
    pan_zoom: PanAndZoom,
    pub board: ActiveCircuitBoard,

    pub debug: bool,
    errors: ErrorList,

    paste: Option<Arc<PastePreview>>,
    inventory: Arc<[InventoryItemGroup<SelectedItemId>]>,
    selected_id: Option<SelectedItemId>,

    props_ui: PropertyEditor,
    sim: Arc<SimulationContext>,
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
    "pin",
    "pullup",
    "freq_meter",
];

struct WireInventoryItem {}
impl InventoryItem<SelectedItemId> for WireInventoryItem {
    fn id(&self) -> SelectedItemId {
        SelectedItemId::Wires
    }

    fn draw(&self, ctx: &PaintContext) {
        let color = WireState::False.color();

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
impl InventoryItem<SelectedItemId> for CircuitInventoryItem {
    fn id(&self) -> SelectedItemId {
        SelectedItemId::Circuit(self.id.clone())
    }

    fn draw(&self, ctx: &PaintContext) {
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
    pub fn new(board: ActiveCircuitBoard, ctx: &Arc<SimulationContext>) -> Self {
        let inventory_group: Vec<_> = INVENTORY_CIRCUIT_ORDER
            .iter()
            .filter_map(|id| ctx.previews.get(*id))
            .map(|preview| {
                Box::new(CircuitInventoryItem {
                    preview: preview.clone(),
                    id: preview.imp.type_name(),
                }) as Box<dyn InventoryItem<SelectedItemId>>
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
        }
    }

    pub fn background_update(&mut self, ui: &mut Ui) {
        let rect = ui.max_rect();
        self.pan_zoom.update(ui, rect, self.selected_id.is_none());

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

        let selected_item = self.selected_item();

        if !ui.ctx().wants_keyboard_input() {
            if ui.input(|input| input.key_pressed(Key::F9)) {
                self.debug = !self.debug;
            } else if ui.input(|input| input.key_pressed(Key::F8)) {
                let state = self.board.state.clone();
                self.board = ActiveCircuitBoard::new(state);
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
            }

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

            if ui.input(|input| input.key_pressed(Key::Q)) {
                let sim_lock = self.board.board.sim_lock.clone();
                let sim_lock = sim_lock.write();

                let ordered = self.board.board.is_ordered_queue();
                self.board.board.set_ordered_queue(!ordered, false);
                drop(sim_lock);
            }

            if ui.input(|input| input.key_pressed(Key::P)) {
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

        let screen = self.pan_zoom.to_screen(rect);
        let paint = ui.painter_at(rect);
        drawing::draw_dynamic_grid(&screen, 16.0, 16.into(), &paint);
        drawing::draw_cross(&screen, rect, &paint);

        let ctx = PaintContext {
            screen,
            paint: &paint,
            rect,
            ui,
        };

        let tile_bounds = self.calc_draw_bounds(&screen);

        self.board.update(
            &ctx,
            tile_bounds,
            selected_item,
            self.debug,
            &mut self.errors,
        );

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

    pub fn ui_update(&mut self, ui: &mut Ui) -> EditorResponse {
        let components_response = self.components_ui(ui);

        if let Some(SelectedItem::Circuit(p)) = self.selected_item() {
            let props = [((), &p.props).into()];
            let changed = Self::properties_ui(&mut self.props_ui, ui, Some(props))
                .is_some_and(|v| !v.is_empty());
            if changed {
                p.redescribe();
            }
        } else {
            let selection = self.board.selection.borrow();
            if !selection.selection.is_empty() {
                let selected_circuit_props = selection.selection.iter().filter_map(|o| match o {
                    SelectedBoardObject::Circuit { id } => Some(*id),
                    _ => None,
                });
                let board = self.board.board.clone();
                let circuits = board.circuits.read();
                let stores: Vec<_> = selected_circuit_props
                    .filter_map(|id| circuits.get(id).map(|c| (id, &c.props).into()))
                    .collect();

                let response = Self::properties_ui(&mut self.props_ui, ui, Some(stores));
                drop(selection);

                if let Some(changes) = response {
                    for property in changes {
                        let str_arc = property
                            .new
                            .downcast_ref::<ArcString>()
                            .map(|s| s.get_arc());
                        let mut formatter =
                            str_arc.as_ref().map(|str| StringFormatterState::new(str));
                        if let Some(formatter) = &mut formatter {
                            let affected_circuits: HashSet<_> = HashSet::from_iter(
                                property.affected_values.iter().map(|(id, _)| *id),
                            );

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
                            self.board.circuit_property_changed(
                                circuit,
                                &property.id,
                                old.as_ref(),
                            );
                        }
                    }
                }
            } else {
                Self::properties_ui(
                    &mut self.props_ui,
                    ui,
                    None::<[PropertyStoreItem<'_, ()>; 1]>,
                );
            }
        }
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
                self.board.selection.borrow_mut().clear();
            }

            Inventory::new(&mut self.selected_id, &self.inventory).ui(&mut ui, |id| match id {
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
            });

            match (
                self.selected_id == Some(SelectedItemId::Paste),
                self.paste.is_some(),
            ) {
                (true, true) => (),
                (true, false) => self.selected_id = None,
                (_, true) => self.paste = None,
                _ => (),
            }

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

            let text = format!("This board has {states} state(s), {frozen_states} are frozen, {unused_states} will be removed");

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
                .board
                .selection
                .borrow()
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
                .board
                .selection
                .borrow()
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

    fn properties_ui<'a, T: Clone + 'static>(
        editor: &'a mut PropertyEditor,
        ui: &mut Ui,
        props: Option<impl IntoIterator<Item = PropertyStoreItem<'a, T>>>,
    ) -> Option<Vec<crate::ui::ChangedProperty<T>>> {
        let style = ui.style().clone();
        let tabs = if props.is_some() { 1 } else { 0 };
        SidePanel::new(PanelSide::Right, "prop-ui")
            .frame(
                Frame::side_top_panel(&style)
                    .rounding(Rounding {
                        nw: 5.0,
                        ne: 0.0,
                        sw: 5.0,
                        se: 0.0,
                    })
                    .outer_margin(Margin::symmetric(0.0, 8.0))
                    .inner_margin(Margin::symmetric(5.0, 5.0))
                    .stroke(style.visuals.window_stroke),
            )
            .show_separator_line(false)
            .default_tab(Some(0))
            .size_range(Rangef::new(120.0, f32::MAX))
            .show(
                ui,
                tabs,
                |_| "Properties editor".into(),
                |_, ui| props.map(|props| editor.ui(ui, props).changes),
            )
            .inner
            .flatten()
    }

    fn components_ui(&mut self, ui: &mut Ui) -> ComponentsUiResponse {
        let style = ui.style().clone();
        let resp = SidePanel::new(PanelSide::Left, "components-ui")
            .frame(
                Frame::side_top_panel(&style)
                    .rounding(Rounding {
                        ne: 5.0,
                        nw: 0.0,
                        se: 5.0,
                        sw: 0.0,
                    })
                    .outer_margin(Margin::symmetric(0.0, 8.0))
                    .inner_margin(Margin::symmetric(5.0, 5.0))
                    .stroke(style.visuals.window_stroke),
            )
            .show_separator_line(false)
            .default_tab(Some(0))
            .show(
                ui,
                1,
                |_| "Components".into(),
                |_, ui| {
                    let font = TextStyle::Monospace.resolve(ui.style());

                    CollapsingHeader::new("Built-in")
                        .default_open(true)
                        .show(ui, |ui| {
                            for name in COMPONENT_BUILTIN_ORDER {
                                if let Some(preview) =
                                    self.sim.previews.get(&DynStaticStr::Static(name))
                                {
                                    ui.horizontal(|ui| {
                                        let resp = ui.allocate_response(
                                            vec2(font.size, font.size),
                                            Sense::hover(),
                                        );
                                        let (rect, scale) = drawing::align_rect_scaled(
                                            resp.rect.min,
                                            vec2(font.size, font.size),
                                            preview.describe().size.convert(|v| v as f32).into(),
                                        );

                                        let paint_ctx = PaintContext::new_on_ui(ui, rect, scale);
                                        preview.draw(&paint_ctx, false);

                                        let selected = match &self.selected_id {
                                            Some(SelectedItemId::Circuit(id)) => {
                                                *id == preview.imp.type_name()
                                            }
                                            _ => false,
                                        };

                                        let resp = ui.selectable_label(
                                            selected,
                                            preview.imp.display_name().deref(),
                                        );

                                        if resp.clicked() {
                                            self.selected_id = match selected {
                                                true => None,
                                                false => Some(SelectedItemId::Circuit(
                                                    preview.imp.type_name(),
                                                )),
                                            };
                                        }

                                        let style = ui.ctx().style();
                                        ui.ctx().style_mut(|style| {
                                            style.visuals.window_fill =
                                                style.visuals.window_fill.linear_multiply(0.9);
                                            style.visuals.window_stroke.color = style
                                                .visuals
                                                .window_stroke
                                                .color
                                                .linear_multiply(0.9);
                                        });

                                        resp.on_hover_ui(|ui| {
                                            ui.label(preview.imp.description().deref());
                                        });

                                        ui.ctx().set_style(style);
                                    });
                                }
                            }
                        });

                    CollapsingHeader::new("Circuit boards")
                        .default_open(true)
                        .show(ui, |ui| {
                            let renamer_memory_id = ui.id().with("__renamer_memory");
                            let renamer_id = ui.id().with("__renamer_input");
                            let rename = ui
                                .memory(|mem| mem.data.get_temp::<Option<u128>>(renamer_memory_id))
                                .flatten();

                            let mut queued_deletion = None;
                            let mut drawn_renamer = false;
                            let mut designer_request = None;
                            let no_delete = self.sim.boards.read().len() <= 1;
                            for board in self.sim.boards.read().values() {
                                if Some(board.board.uid) == rename && !drawn_renamer {
                                    let mut name = board.board.name.write();

                                    let res = TextEdit::singleline(name.get_mut())
                                        .id(renamer_id)
                                        .show(ui);
                                    drawn_renamer = true;

                                    if res.response.lost_focus() {
                                        ui.memory_mut(|mem| {
                                            mem.data.insert_temp(renamer_memory_id, None::<u128>);
                                        });
                                    }
                                } else {
                                    let selected = self.selected_id
                                        == Some(SelectedItemId::Board(board.board.uid));
                                    let active = board.board.uid == self.board.board.uid;

                                    let resp = ui.add(DoubleSelectableLabel::new(
                                        selected,
                                        active,
                                        board.board.name.read().get_str().deref(),
                                        Color32::WHITE.gamma_multiply(0.3),
                                        None,
                                        Stroke::new(1.0, Color32::LIGHT_GREEN),
                                    ));

                                    if resp.clicked_by(egui::PointerButton::Primary)
                                        && !selected
                                        && !active
                                    {
                                        self.selected_id =
                                            Some(SelectedItemId::Board(board.board.uid));

                                        if let Some(preview) = board.preview.as_ref() {
                                            preview.redescribe();
                                        }
                                    }

                                    if resp.double_clicked_by(egui::PointerButton::Primary)
                                        && !active
                                    {
                                        self.board =
                                            ActiveCircuitBoard::new_main(board.board.clone());
                                        if selected {
                                            self.selected_id = None;
                                        }
                                    }

                                    resp.context_menu(|ui| {
                                        if ui.button("Edit").clicked() {
                                            self.board =
                                                ActiveCircuitBoard::new_main(board.board.clone());
                                            if selected {
                                                self.selected_id = None;
                                            }
                                            ui.close_menu();
                                        }

                                        if ui.button("Design").clicked() {
                                            let design_provider =
                                                BoardDesignProvider::new(board.board.clone());
                                            let designer = Designer::new(Box::new(design_provider));
                                            designer_request = Some(designer);
                                            ui.close_menu();
                                        }

                                        if ui.button("Rename").clicked() {
                                            // same hack as below
                                            if !drawn_renamer {
                                                TextEdit::singleline(&mut "")
                                                    .id(renamer_id)
                                                    .show(ui);
                                            }

                                            ui.memory_mut(|mem| {
                                                mem.data.insert_temp(
                                                    renamer_memory_id,
                                                    Some(board.board.uid),
                                                );
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
                                let board = CircuitBoard::new(self.sim.clone(), "New board");
                                let uid = board.uid;
                                let board = Arc::new(board);
                                self.sim
                                    .boards
                                    .write()
                                    .insert(uid, StoredCircuitBoard::new(board.clone()));
                                self.board = ActiveCircuitBoard::new_main(board);

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
                                let mut boards = self.sim.boards.write();
                                if let Some(board) = boards.remove(&uid) {
                                    board.board.destroy();
                                };
                                if self.board.board.uid == uid {
                                    let board = boards.values().next().expect("Boards must exist!");
                                    self.board = ActiveCircuitBoard::new_main(board.board.clone());
                                }
                            }

                            designer_request
                        })
                        .body_returned
                        .flatten()
                },
            );

        ComponentsUiResponse {
            designer_request: resp.inner.flatten(),
        }
    }
}
