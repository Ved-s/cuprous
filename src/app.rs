use std::{collections::HashMap, fmt::Write, ops::Deref, sync::Arc};

use eframe::{
    egui::{
        self, CollapsingHeader, Context, FontSelection, Frame, Key, Margin, Sense, SidePanel,
        TextStyle, Ui, WidgetText,
    },
    epaint::{Color32, Rounding, Stroke, TextShape},
    CreationContext,
};
use emath::{pos2, vec2, Pos2, Rect, Vec2};

use crate::{
    board::{ActiveCircuitBoard, CircuitBoard, SelectedItem},
    circuits::{self, props::CircuitPropertyImpl, CircuitPreview, CircuitPreviewImpl},
    time::Instant,
    ui::{
        CollapsibleSidePanel, Inventory, InventoryItem, InventoryItemGroup, PropertyEditor,
        PropertyStoreItem,
    },
    vector::{Vec2f, Vector},
    BasicLoadingContext, Direction4, DynStaticStr, PaintContext, PanAndZoom, PastePreview, RwLock,
    TileDrawBounds,
};

pub struct App {
    #[cfg(not(feature = "wasm"))]
    last_win_pos: Option<Pos2>,
    #[cfg(not(feature = "wasm"))]
    last_win_size: Vec2,

    pub pan_zoom: PanAndZoom,
    pub board: ActiveCircuitBoard,

    pub debug: bool,

    paste: Option<Arc<PastePreview>>,
    inventory_items: Vec<InventoryItemGroup>,
    selected_id: Option<DynStaticStr>,
    circuit_previews: HashMap<DynStaticStr, Arc<CircuitPreview>>,

    props_ui: crate::ui::PropertyEditor,
}

// TODO: fix coi sometimes not working by re-registering it and reloading
impl eframe::App for App {
    fn update(&mut self, ctx: &eframe::egui::Context, frame: &mut eframe::Frame) {
        #[cfg(not(feature = "wasm"))]
        {
            let int_info = frame.info();
            if let Some(win_pos) = int_info.window_info.position {
                if let Some(last_win_pos) = self.last_win_pos {
                    let win_size = int_info.window_info.size;
                    if win_size != self.last_win_size {
                        let diff: Vec2f = (win_pos - last_win_pos).into();
                        self.pan_zoom.pos += diff / self.pan_zoom.scale;
                    }
                }
            }
            self.last_win_pos = int_info.window_info.position;
            self.last_win_size = int_info.window_info.size;
        }
        ctx.request_repaint();

        #[cfg(feature = "single_thread")]
        let sim_time = {
            let start_time = Instant::now();
            self.board.board.read().states.update();
            Instant::now() - start_time
        };

        cfg_if::cfg_if! {
            if #[cfg(all(not(web_sys_unstable_apis), feature = "wasm"))] {
                let paste = ctx
                    .input(|input| input.modifiers.ctrl && input.key_pressed(egui::Key::V))
                    .then(|| crate::io::GLOBAL_CLIPBOARD.lock().clone())
                    .flatten();
            } else {
                let paste = ctx.input(|input| {
                    input
                        .events
                        .iter()
                        .find_map(|e| match e {
                            egui::Event::Paste(s) => Some(s),
                            _ => None,
                        })
                        .and_then(|p| ron::from_str::<crate::io::CopyPasteData>(p).ok())
                });
            }
        }

        let start_time = Instant::now();

        if let Some(paste) = paste {
            self.paste = Some(Arc::new(PastePreview::new(
                paste,
                &BasicLoadingContext {
                    previews: &self.circuit_previews,
                },
            )));
            self.selected_id = Some("paste".into());
        }

        if !ctx.wants_keyboard_input() {
            if ctx.input(|input| input.key_pressed(Key::F9)) {
                self.debug = !self.debug;
            } else if ctx.input(|input| input.key_pressed(Key::F8)) {
                let board = self.board.board.clone();
                self.board = ActiveCircuitBoard::new(board, 0).unwrap();
            } else if ctx.input(|input| input.key_pressed(Key::F4)) {
                let state = &self.board.state;
                state.reset();
                state.update_everything();
            }
        }

        egui::CentralPanel::default()
            .frame(egui::Frame::central_panel(ctx.style().as_ref()).inner_margin(Margin::same(0.0)))
            .show(ctx, |ui| {
                self.main_update(ui, ctx);

                let left_panel_rect = self.components_ui(ui);

                if let SelectedItem::Circuit(p) = self.selected_item() {
                    let props = [((), &p.props).into()];
                    let changed = App::properties_ui(&mut self.props_ui, ui, Some(props))
                        .is_some_and(|v| !v.is_empty());
                    if changed {}
                } else {
                    let selection = self.board.selection.borrow();
                    if !selection.selection.is_empty() {
                        let selected_circuit_props =
                            selection.selection.iter().filter_map(|o| match o {
                                crate::board::selection::SelectedWorldObject::Circuit { id } => {
                                    Some(*id)
                                }
                                _ => None,
                            });
                        let board = self.board.board.read();
                        let stores: Vec<_> = selected_circuit_props
                            .filter_map(|id| board.circuits.get(id).map(|c| (id, &c.props).into()))
                            .collect();

                        let response = App::properties_ui(&mut self.props_ui, ui, Some(stores));
                        drop(selection);
                        drop(board);

                        if let Some(changes) = response {
                            for property in changes {
                                for circuit in property.affected_values {
                                    self.board.circuit_property_changed(
                                        circuit,
                                        &property.id,
                                        property.old_value.as_ref(),
                                    );
                                }
                            }
                        }
                    } else {
                        App::properties_ui(
                            &mut self.props_ui,
                            ui,
                            None::<[PropertyStoreItem<'_, ()>; 1]>,
                        );
                    }
                }
                {
                    let mut rect = ui.clip_rect();
                    rect.min.x += left_panel_rect.width();
                    rect = rect.shrink(10.0);
                    let mut ui = ui.child_ui(rect, *ui.layout());

                    let mut selected = self.selected_id.take();
                    if ui.input(|input| input.key_pressed(Key::Escape)) {
                        selected = None;
                    }

                    let inv_resp = ui.add(Inventory {
                        selected: &mut selected,
                        groups: &self.inventory_items,
                        item_size: [28.0, 28.0].into(),
                        item_margin: Margin::same(6.0),
                        margin: Margin::default(),
                    });

                    match (selected.as_deref(), &self.paste) {
                        (Some("paste"), Some(_)) => (),
                        (Some("paste"), None) => selected = None,
                        (_, Some(_)) => self.paste = None,
                        _ => (),
                    }
                    self.selected_id = selected;

                    let selected_name = match self.selected_item() {
                        SelectedItem::None => None,
                        SelectedItem::Selection => Some("Selection".into()),
                        SelectedItem::Wire => Some("Wire".into()),
                        SelectedItem::Paste(_) => Some("Pasted objects".into()),
                        SelectedItem::Circuit(c) => Some(c.imp.display_name()),
                    };

                    if let Some(selected_name) = selected_name {
                        let galley = WidgetText::from(selected_name.deref())
                            .fallback_text_style(TextStyle::Monospace)
                            .into_galley(
                                &ui,
                                Some(true),
                                inv_resp.rect.width(),
                                FontSelection::Style(TextStyle::Monospace),
                            )
                            .galley;

                        let size = galley.rect.size() + vec2(12.0, 6.0);
                        let offset = vec2(
                            20.0f32.min(inv_resp.rect.width() - 5.0 - size.x).max(0.0),
                            -2.5,
                        );

                        let resp = ui.allocate_response(size + offset, Sense::hover());
                        let rect = Rect::from_min_size(resp.rect.min + offset, size);
                        let paint = ui.painter();
                        paint.rect(
                            rect,
                            Rounding {
                                nw: 0.0,
                                ne: 0.0,
                                sw: 3.0,
                                se: 3.0,
                            },
                            ui.style().visuals.panel_fill,
                            ui.style().visuals.window_stroke,
                        );

                        paint.add(TextShape {
                            pos: rect.min + vec2(6.0, 3.0),
                            galley,
                            underline: Stroke::NONE,
                            override_text_color: Some(ui.style().visuals.text_color()),
                            angle: 0.0,
                        });
                    }

                    let mut text = String::new();

                    #[cfg(feature = "single_thread")]
                    {
                        let sim_time = sim_time.as_secs_f32() * 1000.0;
                        text.write_fmt(format_args!("Simulation time: {sim_time:.02}ms\n"))
                            .unwrap();
                    }

                    let paint_time = (Instant::now() - start_time).as_secs_f32() * 1000.0;
                    let debug = self.debug;
                    let ordered_queue = self.board.board.read().is_ordered_queue();

                    text.write_fmt(format_args!(
                        "Paint time: {paint_time:.02}ms\n\
                         [F9] Debug: {debug}\n\
                         [F8] Board reload\n\
                         [F4] State reset\n\
                         [R]  Rotate\n\
                         [F]  Flip\n\
                         [Q]  Ordered queue: {ordered_queue}\n\
                        "
                    ))
                    .unwrap();

                    ui.monospace(text);
                }
            });
    }

    fn save(&mut self, _storage: &mut dyn eframe::Storage) {
        let board = self.board.board.read();
        let data = board.save();
        _storage.set_string("board", ron::to_string(&data).unwrap());

        let previews = crate::io::CircuitPreviewCollectionData(HashMap::from_iter(
            self.circuit_previews
                .iter()
                .filter_map(|(ty, p)| p.save().map(|d| (ty.clone(), d))),
        ));
        _storage.set_string("previews", ron::to_string(&previews).unwrap());
    }
}

static INVENTORY_CIRCUIT_ORDER: &[&str] = &["or", "nor", "and", "nand", "xor", "xnor", "not"];

static COMPONENT_BUILTIN_ORDER: &[&str] = &[
    "button",
    "or",
    "nor",
    "and",
    "nand",
    "xor",
    "xnor",
    "not",
    "transistor",
    "pullup",
    "freq_meter",
];

impl App {
    pub fn create(cc: &CreationContext) -> Self {
        let previews = [
            Box::new(circuits::button::Preview {}) as Box<dyn CircuitPreviewImpl>,
            Box::new(circuits::gates::gate::Preview {
                template: circuits::gates::or::TEMPLATE,
            }),
            Box::new(circuits::gates::gate::Preview {
                template: circuits::gates::nor::TEMPLATE,
            }),
            Box::new(circuits::gates::gate::Preview {
                template: circuits::gates::xor::TEMPLATE,
            }),
            Box::new(circuits::gates::gate::Preview {
                template: circuits::gates::xnor::TEMPLATE,
            }),
            Box::new(circuits::gates::gate::Preview {
                template: circuits::gates::and::TEMPLATE,
            }),
            Box::new(circuits::gates::gate::Preview {
                template: circuits::gates::nand::TEMPLATE,
            }),
            Box::new(circuits::gates::not::Preview {}),
            Box::new(circuits::pullup::Preview {}),
            Box::new(circuits::transistor::Preview {}),
            Box::new(circuits::freq_meter::Preview {}),
        ];
        let preview_data = cc
            .storage
            .and_then(|s| s.get_string("previews"))
            .and_then(|s| ron::from_str::<crate::io::CircuitPreviewCollectionData>(&s).ok());
        let previews = HashMap::from_iter(previews.into_iter().map(|p| {
            let data = preview_data
                .as_ref()
                .and_then(|d| d.0.get(p.type_name().deref()));
            let p = match data {
                Some(d) => CircuitPreview::load_with_data(p, d),
                None => CircuitPreview::from_impl(p),
            };
            (p.imp.type_name(), Arc::new(p))
        }));

        let ctx = BasicLoadingContext {
            previews: &previews,
        };
        let shift = cc.egui_ctx.input(|input| input.modifiers.shift);
        let board = (!shift)
            .then_some(cc.storage)
            .flatten()
            .and_then(|s| s.get_string("board"))
            .and_then(|s| ron::from_str::<crate::io::CircuitBoardData>(&s).ok())
            .map(|d| CircuitBoard::load(&d, &ctx));

        Self::new(board, previews)
    }

    pub fn new(
        board: Option<Arc<RwLock<CircuitBoard>>>,
        previews: HashMap<DynStaticStr, Arc<CircuitPreview>>,
    ) -> Self {
        let board = board.unwrap_or_else(|| Arc::new(RwLock::new(CircuitBoard::new())));

        #[cfg(not(feature = "single_thread"))]
        board.read().activate();

        let state_id = {
            let circuit_board = board.read();
            let states = circuit_board.states.states().read();
            let first_id = states
                .inner()
                .iter()
                .enumerate()
                .find(|(_, v)| v.is_some())
                .map(|(i, _)| i);
            drop(states);
            first_id.unwrap_or_else(|| circuit_board.states.create_state(board.clone()).0)
        };
        let inventory_group: Vec<_> = INVENTORY_CIRCUIT_ORDER
            .iter()
            .filter_map(|id| previews.get(*id))
            .map(|preview| {
                Box::new(crate::CircuitInventoryItem {
                    preview: preview.clone(),
                    id: preview.imp.type_name(),
                }) as Box<dyn InventoryItem>
            })
            .collect();

        Self {
            pan_zoom: PanAndZoom::new(0.0.into(), 16.0),

            #[cfg(not(feature = "wasm"))]
            last_win_pos: None,
            #[cfg(not(feature = "wasm"))]
            last_win_size: Default::default(),
            board: ActiveCircuitBoard::new(board, state_id).unwrap(),
            debug: false,

            selected_id: None,
            inventory_items: vec![
                InventoryItemGroup::SingleItem(Box::new(crate::SelectionInventoryItem {})),
                InventoryItemGroup::SingleItem(Box::new(crate::WireInventoryItem {})),
                InventoryItemGroup::Group(inventory_group),
            ],
            circuit_previews: previews,
            paste: None,
            props_ui: Default::default(),
        }
    }

    fn draw_grid(
        pos: Vec2f,
        cell_size: Vec2f,
        mid_lines: Vector<2, u32>,
        rect: emath::Rect,
        paint: &egui::Painter,
    ) {
        let pos = pos * cell_size;
        let visible_cells = (Vec2f::from(rect.size()) / cell_size).convert(|v| v as i32 + 2);
        let start = (pos / cell_size).convert(|v| v as i32);
        let off = pos % cell_size;

        let dim_stroke = Stroke::new(1.0, Color32::from_gray(64));
        let mid_stroke = Stroke::new(1.5, Color32::from_gray(96));

        for i in 0..visible_cells.x() {
            let x = i + start.x();
            if mid_lines.x() > 0 && x % mid_lines.x() as i32 == 0 {
                continue;
            }

            let pos = rect.left() + cell_size.x() * i as f32 - off.x();
            paint.line_segment(
                [pos2(pos, rect.top()), pos2(pos, rect.bottom())],
                dim_stroke,
            );
        }

        for i in 0..visible_cells.y() {
            let y = i + start.y();
            if mid_lines.y() > 0 && y % mid_lines.y() as i32 == 0 {
                continue;
            }

            let pos = rect.top() + cell_size.y() * i as f32 - off.y();
            paint.line_segment(
                [pos2(rect.left(), pos), pos2(rect.right(), pos)],
                dim_stroke,
            );
        }

        let mid_cells =
            visible_cells.combine_with(mid_lines, |v, m| if m == 0 { 0 } else { v / m as i32 + 2 });
        let mid_off = pos % (cell_size * mid_lines.convert(|v| v as f32));

        for i in 0..mid_cells.x() {
            let pos = rect.left() + cell_size.x() * i as f32 * mid_lines.x() as f32 - mid_off.x();
            paint.line_segment(
                [pos2(pos, rect.top()), pos2(pos, rect.bottom())],
                mid_stroke,
            );
        }

        for i in 0..mid_cells.y() {
            let pos = rect.top() + cell_size.y() * i as f32 * mid_lines.y() as f32 - mid_off.y();
            paint.line_segment(
                [pos2(rect.left(), pos), pos2(rect.right(), pos)],
                mid_stroke,
            );
        }

        if start.x() <= 0 && visible_cells.x() + start.x() >= 0 {
            let pos = rect.left() + cell_size.x() * -start.x() as f32 - off.x();
            paint.line_segment(
                [pos2(pos, rect.top()), pos2(pos, rect.bottom())],
                Stroke::new(1.0, Color32::GREEN),
            );
        }

        if start.y() <= 0 && visible_cells.y() + start.y() >= 0 {
            let pos = rect.top() + cell_size.y() * -start.y() as f32 - off.y();
            paint.line_segment(
                [pos2(rect.left(), pos), pos2(rect.right(), pos)],
                Stroke::new(1.0, Color32::RED),
            );
        }
    }

    fn draw_cross(&mut self, bounds: Rect, paint: &egui::Painter) {
        let mut cross_pos = self
            .pan_zoom
            .to_screen(bounds.left_top().into())
            .world_to_screen(0.0.into());

        *cross_pos.x_mut() = cross_pos.x().clamp(bounds.left(), bounds.right());
        *cross_pos.y_mut() = cross_pos.y().clamp(bounds.top(), bounds.bottom());

        let unit = Vec2f::single_value(self.pan_zoom.scale);

        let cross_stroke = Stroke::new(2.0, Color32::WHITE);

        paint.line_segment(
            [
                pos2(cross_pos.x() - unit.x(), cross_pos.y()),
                pos2(cross_pos.x() + unit.x(), cross_pos.y()),
            ],
            cross_stroke,
        );
        paint.line_segment(
            [
                pos2(cross_pos.x(), cross_pos.y() - unit.y()),
                pos2(cross_pos.x(), cross_pos.y() + unit.y()),
            ],
            cross_stroke,
        );
    }

    fn calc_draw_bounds(&self, rect: Rect) -> TileDrawBounds {
        let screen = &self.pan_zoom;
        let chunk_size: Vec2f = (screen.scale * 16.0).into();

        let screen_tl = screen.pos * screen.scale;
        let screen_br = screen_tl + rect.size();

        TileDrawBounds {
            screen_tl,
            screen_br,

            tiles_tl: (screen_tl / screen.scale).convert(|v| v.floor() as i32),
            tiles_br: (screen_br / screen.scale).convert(|v| v.floor() as i32),

            chunks_tl: (screen_tl / chunk_size).convert(|v| v.floor() as i32),
            chunks_br: (screen_br / chunk_size).convert(|v| v.floor() as i32),
        }
    }

    fn main_update(&mut self, ui: &mut Ui, ctx: &Context) {
        let rect = ui.max_rect();
        self.pan_zoom.update(ui, rect, self.selected_id.is_none());
        let paint = ui.painter_at(rect);
        // let font_id = TextStyle::Monospace.resolve(ui.style());
        let mut grid_ds_cell_size = self.pan_zoom.scale;
        while grid_ds_cell_size < 6.0 {
            grid_ds_cell_size *= 16.0;
        }
        App::draw_grid(
            self.pan_zoom.pos * self.pan_zoom.scale / grid_ds_cell_size,
            grid_ds_cell_size.into(),
            16.into(),
            rect,
            &paint,
        );
        self.draw_cross(rect, &paint);
        let bounds = self.calc_draw_bounds(rect);
        let ctx = PaintContext {
            screen: self.pan_zoom.to_screen(rect.left_top().into()),
            paint: &paint,
            rect,
            bounds,
            ui,
            egui_ctx: ctx,
        };

        let selected_item = self.selected_item();

        if !ctx.egui_ctx.wants_keyboard_input() {
            if ctx.egui_ctx.input(|input| input.key_pressed(Key::R)) {
                self.change_selected_props(&selected_item, "dir", |d: &mut Direction4| {
                    *d = d.rotate_clockwise()
                });
            }

            if ctx.egui_ctx.input(|input| input.key_pressed(Key::F)) {
                self.change_selected_props(&selected_item, "flip", |f: &mut bool| *f = !*f);
            }

            if ctx.egui_ctx.input(|input| input.key_pressed(Key::Q)) {
                let sim_lock = self.board.board.read().sim_lock.clone();
                let sim_lock = sim_lock.write();

                let mut board = self.board.board.write();
                let ordered = board.is_ordered_queue();
                board.set_ordered_queue(!ordered, false);
                drop(sim_lock);
            }
        }

        self.board.update(&ctx, selected_item, self.debug);
    }

    fn change_selected_props<T: CircuitPropertyImpl>(
        &mut self,
        selected_item: &SelectedItem,
        id: &str,
        f: impl Fn(&mut T),
    ) {
        if let SelectedItem::Circuit(pre) = selected_item {
            pre.props.write(id, f);
            pre.prop_changed();
        } else {
            let selected_circuits: Vec<_> = self
                .board
                .selection
                .borrow()
                .selection
                .iter()
                .filter_map(|o| match o {
                    crate::board::selection::SelectedWorldObject::Circuit { id } => Some(*id),
                    _ => None,
                })
                .collect();

            let mut vec = vec![];
            let board = self.board.board.read();
            for circuit_id in selected_circuits {
                let circuit = board.circuits.get(circuit_id);
                let circuit = unwrap_option_or_continue!(circuit);
                let old = circuit.props.write(id, |p: &mut T| {
                    let old = p.clone();
                    f(p);
                    old
                });
                let old = unwrap_option_or_continue!(old);
                vec.push((circuit_id, old))
            }
            drop(board);
            for (circuit, old) in vec {
                self.board
                    .circuit_property_changed(circuit, id, old.as_ref());
            }
        }
    }

    fn selected_item(&self) -> SelectedItem {
        match self.selected_id.as_deref() {
            None => SelectedItem::None,
            Some("paste") => match &self.paste {
                Some(p) => SelectedItem::Paste(p.clone()),
                None => SelectedItem::None,
            },
            Some("selection") => SelectedItem::Selection,
            Some("wire") => SelectedItem::Wire,
            Some(circ) => match self.circuit_previews.get(circ) {
                Some(p) => SelectedItem::Circuit(p.clone()),
                None => SelectedItem::None,
            },
        }
    }

    fn properties_ui<'a, T: Clone>(
        editor: &'a mut PropertyEditor,
        ui: &mut Ui,
        props: Option<impl IntoIterator<Item = PropertyStoreItem<'a, T>>>,
    ) -> Option<Vec<crate::ui::ChangedProperty<T>>> {
        let style = ui.style().clone();
        CollapsibleSidePanel::new("prop-ui", "Properties editor")
            .active(props.is_some())
            .header_offset(20.0)
            .side(egui::panel::Side::Right)
            .panel_transformer(Some(Box::new(move |panel: SidePanel| {
                panel
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
            })))
            .show(ui, |ui| props.map(|props| editor.ui(ui, props).changes))
            .panel?
            .inner
    }

    fn components_ui(&mut self, ui: &mut Ui) -> Rect {
        let style = ui.style().clone();
        CollapsibleSidePanel::new("components-ui", "Components")
            .header_offset(20.0)
            .side(egui::panel::Side::Left)
            .panel_transformer(Some(Box::new(move |panel: SidePanel| {
                panel
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
            })))
            .show(ui, |ui| {
                let font = TextStyle::Monospace.resolve(ui.style());

                CollapsingHeader::new("Built-in")
                    .default_open(true)
                    .show(ui, |ui| {
                        for name in COMPONENT_BUILTIN_ORDER {
                            if let Some(preview) =
                                self.circuit_previews.get(&DynStaticStr::Static(name))
                            {
                                ui.horizontal(|ui| {
                                    let resp = ui.allocate_response(
                                        vec2(font.size, font.size),
                                        Sense::hover(),
                                    );
                                    let (rect, scale) = align_rect_scaled(
                                        resp.rect.min,
                                        vec2(font.size, font.size),
                                        preview.describe().size.convert(|v| v as f32).into(),
                                    );

                                    let paint_ctx = PaintContext::new_on_ui(ui, rect, scale);
                                    preview.draw(&paint_ctx, false);

                                    let selected = self
                                        .selected_id
                                        .as_ref()
                                        .is_some_and(|s| *s == preview.imp.type_name());

                                    if ui
                                        .selectable_label(
                                            selected,
                                            preview.imp.display_name().deref(),
                                        )
                                        .clicked()
                                    {
                                        self.selected_id = match selected {
                                            true => None,
                                            false => Some(preview.imp.type_name()),
                                        };
                                    }
                                });
                            }
                        }
                    })
            })
            .full_rect
    }
}

fn align_rect_scaled(pos: Pos2, size: Vec2, rect_size: Vec2) -> (Rect, f32) {
    let scale = (size.x / rect_size.x).min(size.y / rect_size.y);
    let new_size = rect_size * scale;
    let offset = vec2((size.x - new_size.x) * 0.5, (size.y - new_size.y) * 0.5);
    (Rect::from_min_size(pos + offset, new_size), scale)
}
