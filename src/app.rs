use std::{collections::HashMap, sync::Arc, ops::Deref};

use eframe::{
    egui::{self, Key, Margin, Context, Ui, TextStyle},
    CreationContext, epaint::{Stroke, Color32},
};
use emath::{Pos2, Vec2, pos2, Rect, vec2, Align2};

use crate::{
    board::{ActiveCircuitBoard, CircuitBoard, SelectedBoardItem},
    circuits::{self, CircuitPreview},
    ui::{Inventory, InventoryItemGroup, InventoryItem},
    vector::{Vec2f, Vector},
    BasicLoadingContext, PanAndZoom, RwLock, DynStaticStr, TileDrawBounds, time::Instant, PaintContext,
};

pub struct App {
    #[cfg(not(target_arch = "wasm32"))]
    last_win_pos: Option<Pos2>,
    #[cfg(not(target_arch = "wasm32"))]
    last_win_size: Vec2,

    pub pan_zoom: PanAndZoom,
    pub board: ActiveCircuitBoard,

    pub debug: bool,

    inventory_items: Vec<InventoryItemGroup>,
    selected_id: Option<String>,
    circuit_previews: HashMap<String, Arc<Box<dyn CircuitPreview>>>,
}

impl eframe::App for App {
    fn update(&mut self, ctx: &eframe::egui::Context, frame: &mut eframe::Frame) {
        #[cfg(not(target_arch = "wasm32"))]
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
        self.board.board.read().unwrap().states.update();

        if ctx.input(|input| input.key_pressed(Key::F9)) {
            self.debug = !self.debug;
        }

        if ctx.input(|input| input.key_pressed(Key::F8)) {
            let board = self.board.board.clone();
            self.board = ActiveCircuitBoard::new(board, 0).unwrap();
        }

        egui::CentralPanel::default()
            .frame(egui::Frame::central_panel(ctx.style().as_ref()).inner_margin(Margin::same(0.0)))
            .show(ctx, |ui| {
                self.main_update(ui, ctx);

                let mut selected = self.selected_id.take();

                if ui.input(|input| input.key_pressed(Key::Escape)) {
                    selected = None;
                }

                ui.add(Inventory {
                    selected: &mut selected,
                    groups: &self.inventory_items,
                    item_size: [28.0, 28.0].into(),
                    item_margin: Margin::same(6.0),
                    margin: Margin::same(10.0),
                });

                self.selected_id = selected;
            });
    }

    fn save(&mut self, _storage: &mut dyn eframe::Storage) {
        let board = self.board.board.read().unwrap();
        let data = board.save();
        _storage.set_string("board", ron::to_string(&data).unwrap());
    }
}

impl App {
    pub fn create(cc: &CreationContext) -> Self {
        let previews = [
            Box::new(circuits::test::Preview {}) as Box<dyn CircuitPreview>,
            Box::new(circuits::button::Preview {}),
            Box::new(circuits::gates::gate::Preview {
                template: circuits::gates::or::TEMPLATE,
            }),
            Box::new(circuits::gates::gate::Preview {
                template: circuits::gates::nor::TEMPLATE,
            }),
            Box::new(circuits::gates::gate::Preview {
                template: circuits::gates::and::TEMPLATE,
            }),
            Box::new(circuits::gates::gate::Preview {
                template: circuits::gates::nand::TEMPLATE,
            }),
            Box::new(circuits::pullup::Preview {}),
        ];
        let previews =
            HashMap::from_iter(previews.into_iter().map(|p| (p.type_name(), Arc::new(p))));

        let ctx = BasicLoadingContext {
            previews: &previews,
        };
        let shift = cc.egui_ctx.input(|input| input.modifiers.shift);
        let board = (!shift)
            .then_some(cc.storage)
            .flatten()
            .and_then(|s| s.get_string("board"))
            .map(|s| ron::from_str::<crate::io::CircuitBoardData>(&s).unwrap())
            .map(|d| CircuitBoard::load(&d, &ctx));

        Self::new(board, previews)
    }

    pub fn new(
        board: Option<Arc<RwLock<CircuitBoard>>>,
        previews: HashMap<DynStaticStr, Arc<Box<dyn CircuitPreview>>>,
    ) -> Self {
        let board = board.unwrap_or_else(|| Arc::new(RwLock::new(CircuitBoard::new())));

        #[cfg(not(feature = "single_thread"))]
        board.read().unwrap().activate();

        let state_id = {
            let circuit_board = board.read().unwrap();
            let states = circuit_board.states.states().read().unwrap();
            let first_id = states
                .inner()
                .iter()
                .enumerate()
                .find(|(_, v)| v.is_some())
                .map(|(i, _)| i);
            drop(states);
            first_id.unwrap_or_else(|| circuit_board.states.create_state(board.clone()).0)
        };
        Self {
            pan_zoom: PanAndZoom::new(0.0.into(), 16.0),

            #[cfg(not(target_arch = "wasm32"))]
            last_win_pos: None,
            #[cfg(not(target_arch = "wasm32"))]
            last_win_size: Default::default(),
            board: ActiveCircuitBoard::new(board, state_id).unwrap(),
            debug: false,

            selected_id: None,
            inventory_items: vec![
                InventoryItemGroup::SingleItem(Box::new(crate::SelectionInventoryItem {})),
                InventoryItemGroup::SingleItem(Box::new(crate::WireInventoryItem {})),
                InventoryItemGroup::Group(
                    previews
                        .iter()
                        .map(|e| {
                            Box::new(crate::CircuitInventoryItem {
                                preview: e.1.clone(),
                                id: e.0.deref().to_owned(),
                            }) as Box<dyn InventoryItem>
                        })
                        .collect(),
                ),
            ],
            circuit_previews: previews
                .into_iter()
                .map(|(id, arc)| (id.deref().to_owned(), arc))
                .collect(),
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
        let start_time = Instant::now();

        let rect = ui.max_rect();
        self.pan_zoom.update(ui, rect, self.selected_id.is_none());
        let paint = ui.painter_at(rect);
        let font_id = TextStyle::Monospace.resolve(ui.style());
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

        let selected_item = match self.selected_id.as_deref() {
            None => SelectedBoardItem::None,
            Some("selection") => SelectedBoardItem::Selection,
            Some("wire") => SelectedBoardItem::Wire,
            Some(circ) => match self.circuit_previews.get(circ) {
                Some(p) => SelectedBoardItem::Circuit(&***p),
                None => SelectedBoardItem::None,
            },
        };

        self.board.update(&ctx, selected_item, self.debug);
        let update_time = Instant::now() - start_time;

        paint.text(
            rect.left_top() + vec2(10.0, 80.0),
            Align2::LEFT_TOP,
            format!(
                r#"Pos: {}
Tile draw bounds: {} - {}
Chunk draw bounds: {} - {}
Time: {:.2} ms
Selected: {:?}

[F9] Debug: {}

Wire parts drawn: {}
Pressed keys: {:?}
"#,
                self.pan_zoom.pos,
                bounds.tiles_tl,
                bounds.tiles_br,
                bounds.chunks_tl,
                bounds.chunks_br,
                update_time.as_secs_f64() * 1000.0,
                self.selected_id,
                self.debug,
                self.board
                    .wires_drawn
                    .load(std::sync::atomic::Ordering::Relaxed),
                ui.input(|input| input.keys_down.iter().cloned().collect::<Vec<_>>())
            ),
            font_id,
            Color32::WHITE,
        );
    }
}
