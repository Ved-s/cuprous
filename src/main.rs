#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
#![feature(int_roundings)]
#![feature(lazy_cell)]
#![feature(thread_id_value)]

use std::{f32::consts::PI, mem::size_of, ops::Range, sync::Arc, time::Instant};

use board::{selection::Selection, ActiveCircuitBoard, CircuitBoard, SelectedBoardItem};
use eframe::{
    egui::{self, Context, Frame, Key, Margin, Sense, TextStyle, Ui},
    epaint::{ahash::HashMap, Color32, PathShape, Rounding, Shape, Stroke},
};
use emath::{pos2, vec2, Align2, Pos2, Rect, Vec2};

mod r#const;

mod vector;

use ui::{Inventory, InventoryItem, InventoryItemGroup};
use vector::{Vec2f, Vec2i, Vector};

mod containers;
use crate::containers::*;

mod circuits;
use circuits::CircuitPreview;

mod wires;

mod state;
use state::{State, WireState};

mod board;

#[macro_use]
mod macros;

#[cfg(debug_assertions)]
mod debug;

mod ui;
mod io;
mod cache;

#[cfg(debug_assertions)]
type RwLock<T> = debug::DebugRwLock<T>;

#[cfg(not(debug_assertions))]
type RwLock<T> = std::sync::RwLock<T>;
type Mutex<T> = std::sync::Mutex<T>;

fn main() { 
    eframe::run_native(
        "rls",
        eframe::NativeOptions::default(),
        Box::new(|_| Box::new(App::new())),
    )
    .unwrap();
}

#[allow(unused)]
#[derive(Debug, Clone, Copy)]
struct TileDrawBounds {
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

#[allow(clippy::redundant_allocation)]
pub struct PaintContext<'a> {
    screen: Screen,
    paint: &'a egui::Painter,
    rect: Rect,
    bounds: TileDrawBounds,
    ui: &'a Ui,
    egui_ctx: &'a Context,
}

impl<'a> PaintContext<'a> {
    pub fn new_on_ui(ui: &'a Ui, rect: Rect) -> Self {
        Self {
            screen: Screen {
                offset: rect.left_top().into(),
                pos: 0.0.into(),
                scale: 1.0,
            },
            paint: ui.painter(),
            rect,
            bounds: TileDrawBounds::EVERYTHING,
            ui,
            egui_ctx: ui.ctx(),
        }
    }

    pub fn with_rect(&self, rect: Rect) -> PaintContext<'a> {
        Self {
            rect,
            ui: self.ui,
            ..*self
        }
    }

    fn draw_chunks<const CHUNK_SIZE: usize, T: Default, P>(
        &self,
        chunks: &Chunks2D<CHUNK_SIZE, T>,
        pass: &P,
        draw_tester: impl Fn(&T) -> bool,
        drawer: impl Fn(&T, Vec2i, &Self, &P, &ChunksLookaround<CHUNK_SIZE, T>),
    ) {
        let TileDrawBounds {
            screen_tl: _,
            screen_br: _,
            tiles_tl,
            tiles_br,
            chunks_tl,
            chunks_br,
        } = self.bounds;

        let screen = &self.screen;

        for cy in chunks_tl.y()..=chunks_br.y() {
            let rowrange = chunks.get_chunk_row_range(cy as isize);
            let rowrange = Range {
                start: rowrange.start as i32,
                end: rowrange.end as i32,
            };

            for cx in (chunks_tl.x()..chunks_br.x() + 1).intersect(&rowrange) {
                let chunk_coord: Vec2i = [cx, cy].into();
                let chunk_tl = chunk_coord * 16;
                let chunk = unwrap_option_or_continue!(
                    chunks.get_chunk(chunk_coord.convert(|v| v as isize))
                );
                let chunk_viewport_tl = tiles_tl - chunk_tl;
                let chunk_viewport_br = tiles_br - chunk_tl;

                for j in 0..16 {
                    if j < chunk_viewport_tl.y() {
                        continue;
                    } else if j > chunk_viewport_br.y() {
                        break;
                    }

                    for i in 0..16 {
                        if i < chunk_viewport_tl.x() {
                            continue;
                        } else if i > chunk_viewport_br.x() {
                            break;
                        }

                        let tile = &chunk[i as usize][j as usize];
                        if !draw_tester(tile) {
                            continue;
                        }

                        let pos: Vec2i = chunk_tl + [i, j];
                        let draw_pos = Vec2f::from(self.rect.left_top())
                            + screen.world_to_screen(pos.convert(|v| v as f32));
                        let rect =
                            Rect::from_min_size(draw_pos.into(), vec2(screen.scale, screen.scale));
                        let lookaround = ChunksLookaround::new(
                            chunks,
                            chunk,
                            pos.convert(|v| v as isize),
                            [i as usize, j as usize].into(),
                        );

                        let drawer_ctx = self.with_rect(rect);

                        drawer(tile, pos, &drawer_ctx, pass, &lookaround)
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct PanAndZoom {
    pos: Vec2f,
    scale: f32,
}

impl PanAndZoom {
    fn update(&mut self, ui: &egui::Ui, rect: Rect) {
        let zoom = ui.input(|input| {
            input
                .multi_touch()
                .map(|mt| mt.zoom_delta)
                .unwrap_or_else(|| {
                    let v = input.scroll_delta.y / 240.0;
                    if v < 0.0 {
                        1.0 / (-v + 1.0)
                    } else if v > 0.0 {
                        v + 1.0
                    } else {
                        1.0
                    }
                })
        });

        let interaction = ui.interact(rect, ui.id(), Sense::drag());

        if interaction.dragged_by(egui::PointerButton::Secondary) {
            self.pos -= interaction.drag_delta() / self.scale;
        }

        if zoom != 1.0 {
            let pointer_screen = Vec2f::from(
                ui.input(|i| i.pointer.hover_pos())
                    .unwrap_or_else(|| rect.center())
                    - rect.left_top(),
            );
            let world_before = self.pos + pointer_screen / self.scale;
            self.scale *= zoom;
            let world_after = self.pos + pointer_screen / self.scale;
            self.pos -= world_after - world_before;
        }
    }
}

impl Default for PanAndZoom {
    fn default() -> Self {
        Self {
            scale: 1.0,
            pos: Default::default(),
        }
    }
}

impl PanAndZoom {
    pub fn new(pos: Vec2f, scale: f32) -> Self {
        Self { pos, scale }
    }

    pub fn to_screen(self, offset: Vec2f) -> Screen {
        Screen {
            offset,
            pos: self.pos,
            scale: self.scale,
        }
    }
}

#[derive(Clone, Copy)]
struct Screen {
    offset: Vec2f,
    pos: Vec2f,
    scale: f32,
}

#[allow(unused)]
impl Screen {
    pub fn screen_to_world(&self, v: Vec2f) -> Vec2f {
        self.pos + (v - self.offset) / self.scale
    }

    pub fn world_to_screen(&self, v: Vec2f) -> Vec2f {
        (v - self.pos) * self.scale + self.offset
    }

    pub fn screen_to_world_tile(&self, v: Vec2f) -> Vec2i {
        self.screen_to_world(v).convert(|v| v.floor() as i32)
    }

    pub fn world_to_screen_tile(&self, v: Vec2i) -> Vec2f {
        self.world_to_screen(v.convert(|v| v as f32))
    }
}

struct App {
    last_win_pos: Option<Pos2>,
    last_win_size: Vec2,

    pub pan_zoom: PanAndZoom,
    pub board: ActiveCircuitBoard,

    pub debug: bool,

    inventory_items: Vec<InventoryItemGroup>,
    selected_id: Option<String>,
    circuit_previews: HashMap<String, Box<dyn CircuitPreview>>,
}

struct SelectionInventoryItem {}
impl InventoryItem for SelectionInventoryItem {
    fn id(&self) -> &str {
        "selection"
    }

    fn draw(&self, ctx: &PaintContext) {
        let rect = ctx.rect.shrink2(ctx.rect.size() / 5.0);
        ctx.paint
            .rect_filled(rect, Rounding::none(), Selection::fill_color());
        let rect_corners = [
            rect.left_top(),
            rect.right_top(),
            rect.right_bottom(),
            rect.left_bottom(),
            rect.left_top(),
        ];

        let mut shapes = vec![];
        Shape::dashed_line_many(
            &rect_corners,
            Stroke::new(1.0, Selection::border_color()),
            3.0,
            2.0,
            &mut shapes,
        );

        shapes.into_iter().for_each(|s| {
            ctx.paint.add(s);
        });
    }
}

struct WireInventoryItem {}
impl InventoryItem for WireInventoryItem {
    fn id(&self) -> &str {
        "wire"
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
    preview: Box<dyn CircuitPreview>,
    id: String,
}
impl InventoryItem for CircuitInventoryItem {
    fn id(&self) -> &str {
        &self.id
    }

    fn draw(&self, ctx: &PaintContext) {
        let size = self.preview.size().convert(|v| v as f32);
        let scale = Vec2f::from(ctx.rect.size()) / size;
        let scale = scale.x().min(scale.y());
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
        self.preview.draw_preview(&circ_ctx, false);
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

impl eframe::App for App {
    fn update(&mut self, ctx: &eframe::egui::Context, frame: &mut eframe::Frame) {
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
        ctx.request_repaint();
        self.last_win_pos = int_info.window_info.position;
        self.last_win_size = int_info.window_info.size;

        //self.board.state.update(&self.board.board.read().unwrap());

        if ctx.input(|input| input.key_pressed(Key::F9)) {
            self.debug = !self.debug;
        }
        
        if ctx.input(|input| input.key_pressed(Key::F8)) {
            let board = self.board.board.clone();
            self.board = ActiveCircuitBoard::new(board, 0).unwrap();
        }

        egui::CentralPanel::default()
            .frame(Frame::central_panel(ctx.style().as_ref()).inner_margin(Margin::same(0.0)))
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
    fn new() -> Self {
        let board = CircuitBoard::new();
        let states = board.states.clone();
        let board = Arc::new(RwLock::new(board));
        let state_id = states.create_state(board.clone()).0;

        Self {
            pan_zoom: PanAndZoom::new(0.0.into(), 16.0),
            last_win_pos: None,
            last_win_size: Default::default(),
            board: ActiveCircuitBoard::new(board, state_id).unwrap(),
            debug: true,

            selected_id: None,
            inventory_items: vec![
                InventoryItemGroup::SingleItem(Box::new(SelectionInventoryItem {})),
                InventoryItemGroup::SingleItem(Box::new(WireInventoryItem {})),
                InventoryItemGroup::Group(vec![
                    Box::new(CircuitInventoryItem {
                        preview: Box::new(circuits::test::Preview {}),
                        id: "test".to_owned(),
                    }),
                    Box::new(CircuitInventoryItem {
                        preview: Box::new(circuits::button::Preview {}),
                        id: "button".to_owned(),
                    }),
                    Box::new(CircuitInventoryItem {
                        preview: Box::new(circuits::gates::gate::Preview {
                            template: circuits::gates::or::TEMPLATE,
                        }),
                        id: "or".to_owned(),
                    }),
                    Box::new(CircuitInventoryItem {
                        preview: Box::new(circuits::gates::gate::Preview {
                            template: circuits::gates::and::TEMPLATE,
                        }),
                        id: "and".to_owned(),
                    }),
                    Box::new(CircuitInventoryItem {
                        preview: Box::new(circuits::pullup::Preview {}),
                        id: "pullup".to_owned(),
                    }),
                ]),
            ],
            circuit_previews: HashMap::from_iter(
                [
                    (
                        "test".to_owned(),
                        Box::new(circuits::test::Preview {}) as Box<dyn CircuitPreview>,
                    ),
                    ("button".to_owned(), Box::new(circuits::button::Preview {})),
                    (
                        "or".to_owned(),
                        Box::new(circuits::gates::gate::Preview {
                            template: circuits::gates::or::TEMPLATE,
                        }),
                    ),
                    (
                        "and".to_owned(),
                        Box::new(circuits::gates::gate::Preview {
                            template: circuits::gates::and::TEMPLATE,
                        }),
                    ),
                    ("pullup".to_owned(), Box::new(circuits::pullup::Preview {})),
                ]
                .into_iter(),
            ),
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
        self.pan_zoom.update(ui, rect);
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
                Some(p) => SelectedBoardItem::Circuit(&**p),
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
Time: {:.4} ms
Selected: {:?}

[F9] Debug: {}

Wire parts drawn: {}
State thread id: {:?}
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
                self.board.state.thread().map(|id| id.as_u64().get()),
                ui.input(|input| input.keys_down.iter().cloned().collect::<Vec<_>>())
            ),
            font_id,
            Color32::WHITE,
        );
    }
}
trait SizeCalc
where
    Self: Sized,
{
    fn calc_size_outer(&self) -> usize {
        self.calc_size_inner() + size_of::<Self>()
    }
    fn calc_size_inner(&self) -> usize;
}

macro_rules! impl_empty_inner_size {
    ($($t:ty),+) => {
        $(impl SizeCalc for $t {
            fn calc_size_inner(&self) -> usize {
                0
            }
        })+
    };
}

impl_empty_inner_size!(u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, usize, isize, f32, f64);

impl<T: SizeCalc> SizeCalc for &[T] {
    fn calc_size_inner(&self) -> usize {
        self.iter().map(|i| i.calc_size_outer()).sum()
    }
}

impl<T: SizeCalc> SizeCalc for Vec<T> {
    fn calc_size_inner(&self) -> usize {
        self.iter().map(|i| i.calc_size_outer()).sum()
    }
}

impl<const SIZE: usize, T: SizeCalc> SizeCalc for [T; SIZE] {
    fn calc_size_inner(&self) -> usize {
        self.iter().map(|i| i.calc_size_inner()).sum()
    }
}

impl<T: SizeCalc> SizeCalc for Option<T> {
    fn calc_size_inner(&self) -> usize {
        match self {
            None => 0,
            Some(v) => v.calc_size_inner(),
        }
    }
}

impl<T: SizeCalc> SizeCalc for Box<T> {
    fn calc_size_inner(&self) -> usize {
        let t = self.as_ref();
        t.calc_size_outer()
    }
}

fn format_size(size: usize) -> String {
    const SIZE_LABELS: &[&str] = &["B", "kB", "MB", "GB", "TB"];

    let mut div = 1usize;
    let mut label = 0;
    for _ in 0..SIZE_LABELS.len() {
        let (next_div, overflow) = div.overflowing_shl(10);
        if overflow {
            break;
        }
        if size >= next_div {
            label += 1;
            div = next_div;
        } else {
            break;
        }
    }

    let valf = size as f64 / div as f64;
    let ff = format!("{valf:.2}");
    let ff = ff.trim_end_matches('0').trim_end_matches('.');
    format!("{ff} {}", SIZE_LABELS[label])
}

trait Intersect {
    fn intersect(&self, other: &Self) -> Self;
}

impl<T: Ord + Copy> Intersect for Range<T> {
    fn intersect(&self, other: &Self) -> Self {
        Self {
            start: self.start.max(other.start),
            end: self.end.min(other.end),
        }
    }
}

pub trait Integer: Eq + Copy {
    const SIGNED: bool;
    const ZERO: Self;
    const MAX: Self;
    const MIN: Self;
}

macro_rules! impl_integer_trait {
    (signed $($t:ty),+) => {
        $(impl crate::Integer for $t {
            const SIGNED: bool = true;
            const ZERO: $t = 0;
            const MIN: $t = <$t>::MIN;
            const MAX: $t = <$t>::MAX;
        })+
    };
    (unsigned $($t:ty),+) => {
        $(impl crate::Integer for $t {
            const SIGNED: bool = false;
            const ZERO: $t = 0;
            const MIN: $t = <$t>::MIN;
            const MAX: $t = <$t>::MAX;
        })+
    };
}

impl_integer_trait!(signed i8, i16, i32, i64, i128, isize);
impl_integer_trait!(unsigned u8, u16, u32, u64, u128, usize);

macro_rules! impl_optional_int {
    ($name:ident, $none:expr) => {
        #[derive(Clone, Copy, Debug)]
        pub struct $name<T: Integer>(T);

        impl<T: Integer> Default for $name<T> {
            fn default() -> Self {
                Self(Self::NONE_VALUE)
            }
        }

        #[allow(unused)]
        impl<T: Integer> $name<T> {
            const NONE_VALUE: T = $none;

            pub fn new(value: T) -> Self {
                Self(value)
            }

            pub fn none() -> Self {
                Self(Self::NONE_VALUE)
            }

            pub fn is_none(&self) -> bool {
                self.0 == Self::NONE_VALUE
            }

            pub fn is_some(&self) -> bool {
                self.0 != Self::NONE_VALUE
            }

            pub fn is_some_and(&self, f: impl FnOnce(T) -> bool) -> bool {
                self.0 != Self::NONE_VALUE && f(self.0)
            }

            pub fn is_none_or(&self, f: impl FnOnce(T) -> bool) -> bool {
                self.0 == Self::NONE_VALUE || f(self.0)
            }

            pub fn get(&self) -> Option<T> {
                if self.0 == Self::NONE_VALUE {
                    None
                } else {
                    Some(self.0)
                }
            }

            pub fn set(&mut self, value: Option<T>) {
                self.0 = match value {
                    None => Self::NONE_VALUE,
                    Some(v) => v,
                }
            }
        }
    };
}

impl_optional_int!(OptionalInt, (if T::SIGNED { T::MIN } else { T::MAX }));
impl_optional_int!(OptionalNonzeroInt, (T::ZERO));

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Direction4 {
    Up,
    Left,
    Down,
    Right,
}

impl Direction4 {
    pub fn iter_all() -> impl Iterator<Item = Self> {
        [Self::Left, Self::Up, Self::Right, Self::Down].into_iter()
    }

    pub fn unit_vector(self) -> Vec2i {
        match self {
            Self::Up => [0, -1],
            Self::Left => [-1, 0],
            Self::Down => [0, 1],
            Self::Right => [1, 0],
        }
        .into()
    }

    pub fn move_vector(self, vec: Vec2i, distance: i32) -> Vec2i {
        vec + self.unit_vector() * distance
    }

    pub fn is_vertical(self) -> bool {
        match self {
            Self::Left | Self::Right => false,
            Self::Up | Self::Down => true,
        }
    }

    pub fn is_horizontal(self) -> bool {
        match self {
            Self::Left | Self::Right => true,
            Self::Up | Self::Down => false,
        }
    }

    pub fn is_left_up(self) -> bool {
        match self {
            Self::Left | Self::Up => true,
            Self::Right | Self::Down => false,
        }
    }

    pub fn is_right_bottom(self) -> bool {
        match self {
            Self::Right | Self::Down => true,
            Self::Left | Self::Up => false,
        }
    }

    pub fn inverted(self) -> Self {
        match self {
            Self::Up => Self::Down,
            Self::Left => Self::Right,
            Self::Down => Self::Up,
            Self::Right => Self::Left,
        }
    }

    /// Returns: (direction, forward)
    pub fn into_dir2(self) -> (Direction2, bool) {
        match self {
            Direction4::Up => (Direction2::Up, true),
            Direction4::Left => (Direction2::Left, true),
            Direction4::Down => (Direction2::Up, false),
            Direction4::Right => (Direction2::Left, false),
        }
    }

    /// if include_start { returns dist values } else { returns start pos + dist values }
    pub fn iter_pos_along(
        self,
        pos: Vec2i,
        dist: i32,
        include_start: bool,
    ) -> DirectionPosItreator {
        let dir = self.unit_vector() * if dist >= 0 { 1 } else { -1 };
        let dist = dist.unsigned_abs();

        let (pos, dist) = if include_start {
            (pos, dist + 1)
        } else {
            (pos + dir, dist)
        };
        DirectionPosItreator {
            pos,
            remaining: dist,
            dir,
        }
    }
}

impl From<Direction2> for Direction4 {
    fn from(value: Direction2) -> Self {
        match value {
            Direction2::Up => Direction4::Up,
            Direction2::Left => Direction4::Left,
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Direction2 {
    Up,
    Left,
}

impl Direction2 {
    pub fn iter_all() -> impl Iterator<Item = Direction2> {
        [Direction2::Left, Direction2::Up].into_iter()
    }

    pub fn unit_vector(self, forward: bool) -> Vec2i {
        match (self, forward) {
            (Direction2::Up, true) => [0, -1],
            (Direction2::Left, true) => [-1, 0],
            (Direction2::Up, false) => [0, 1],
            (Direction2::Left, false) => [1, 0],
        }
        .into()
    }

    pub fn move_vector(self, vec: Vec2i, distance: i32, forward: bool) -> Vec2i {
        vec + self.unit_vector(forward) * distance
    }

    pub fn choose_axis_component<T>(self, x: T, y: T) -> T {
        match self {
            Direction2::Up => y,
            Direction2::Left => x,
        }
    }

    /// if include_start { returns dist values } else { returns start pos + dist values }
    pub fn iter_pos_along(
        self,
        pos: Vec2i,
        dist: i32,
        include_start: bool,
    ) -> DirectionPosItreator {
        let dir = self.unit_vector(dist >= 0);
        let dist = dist.unsigned_abs();

        let (pos, dist) = if include_start {
            (pos, dist + 1)
        } else {
            (pos + dir, dist)
        };
        DirectionPosItreator {
            pos,
            remaining: dist,
            dir,
        }
    }
}

pub struct DirectionPosItreator {
    pos: Vec2i,
    remaining: u32,
    dir: Vec2i,
}

impl Iterator for DirectionPosItreator {
    type Item = Vec2i;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            return None;
        }

        let p = self.pos;
        self.pos += self.dir;
        self.remaining -= 1;
        Some(p)
    }
}