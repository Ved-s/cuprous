#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
#![feature(int_roundings)]

use std::{
    mem::size_of,
    time::Instant,
};

use eframe::{
    egui::{self, Context, Frame, Margin, Sense, TextStyle, Ui},
    epaint::{Color32, Rounding, Stroke},
};
use emath::{pos2, vec2, Align2, Pos2, Rect};
use vector::{Vec2i, Vector};

use crate::vector::Vec2f;
use containers::*;
use wires::Wires;

mod r#const;
mod containers;
mod vector;
mod wires;

fn main() {
    eframe::run_native(
        "rls",
        eframe::NativeOptions::default(),
        Box::new(|_| Box::new(App::new())),
    )
    .unwrap();
}

#[derive(Default)]
struct App {
    last_win_pos: Option<Pos2>,

    pub screen: PanAndZoom,
    pub wires: Wires,
}

#[derive(Debug, Clone, Copy)]
struct TileDrawBounds {
    pub tile_size: Vec2f,
    pub chunk_size: Vec2f,

    pub screen_tl: Vec2f,
    pub screen_br: Vec2f,

    pub tiles_tl: Vec2i,
    pub tiles_br: Vec2i,

    pub chunks_tl: Vec2i,
    pub chunks_br: Vec2i,
}

pub struct PaintContext<'a> {
    screen: PanAndZoom,
    paint: &'a egui::Painter,
    rect: Rect,
    bounds: TileDrawBounds,
    ui: &'a mut Ui,
    egui_ctx: &'a Context,
}

impl PaintContext<'_> {
    fn draw_chunks<const CHUNK_SIZE: usize, T: Default, P>(
        &self,
        chunks: &Chunks2D<CHUNK_SIZE, T>,
        rect: Rect,
        paint: &egui::Painter,
        bounds: TileDrawBounds,
        pass: &P,
        draw_tester: impl Fn(&T) -> bool,
        drawer: impl Fn(
            &T,
            Vec2i,
            Rect,
            &P,
            &egui::Painter,
            &TileDrawBounds,
            &ChunksLookaround<CHUNK_SIZE, T>,
        ),
    ) {
        let TileDrawBounds {
            tile_size: _,
            chunk_size,
            screen_tl: _,
            screen_br: _,
            tiles_tl,
            tiles_br,
            chunks_tl,
            chunks_br,
        } = bounds;

        let screen = &self.screen;

        for cy in chunks_tl.y()..=chunks_br.y() {
            for cx in chunks_tl.x()..=chunks_br.x() {
                let chunk_coord: Vec2i = [cx, cy].into();
                let chunk_tl = chunk_coord * 16;
                let chunk =
                    match chunks.get_chunk(chunk_coord.x() as isize, chunk_coord.y() as isize) {
                        Some(v) => v,
                        None => continue,
                    };

                let pos = Vec2f::from(rect.left_top())
                    + screen.world_to_screen(chunk_tl.convert_values(|v| v as f32));
                let chunk_rect = Rect::from_min_size(pos.into(), chunk_size.into());
                paint.rect_filled(
                    chunk_rect,
                    Rounding::none(),
                    Color32::from_rgba_unmultiplied(100, 0, 100, 40),
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
                        let draw_pos = Vec2f::from(rect.left_top())
                            + screen.world_to_screen(pos.convert_values(|v| v as f32));
                        let rect =
                            Rect::from_min_size(draw_pos.into(), vec2(screen.scale, screen.scale));
                        let lookaround = ChunksLookaround::new(
                            chunks,
                            chunk,
                            pos.convert_values(|v| v as isize),
                            [i as usize, j as usize].into(),
                        );
                        drawer(tile, pos, rect, pass, paint, &bounds, &lookaround)
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
            let pointer_screen = (ui
                .input(|i| i.pointer.hover_pos())
                .unwrap_or_else(|| rect.center())
                - rect.left_top())
            .into();
            let world_before = self.screen_to_world(pointer_screen);
            self.scale *= zoom;
            let world_after = self.screen_to_world(pointer_screen);
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
    fn new(pos: Vec2f, scale: f32) -> Self {
        Self { pos, scale }
    }

    fn screen_to_world(&self, v: Vec2f) -> Vec2f {
        self.pos + v / self.scale
    }

    fn world_to_screen(&self, v: Vec2f) -> Vec2f {
        (v - self.pos) * self.scale
    }

    fn screen_to_world_tile(&self, v: Vec2f) -> Vec2i {
        (self.pos + v / self.scale).convert_values(|v| v.floor() as i32)
    }

    fn world_to_screen_tile(&self, v: Vec2i) -> Vec2f {
        (v.convert_values(|v| v as f32) - self.pos) * self.scale
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &eframe::egui::Context, frame: &mut eframe::Frame) {
        let start_time = Instant::now();
        let int_info = frame.info();
        if let Some(win_pos) = int_info.window_info.position {
            if let Some(last_win_pos) = self.last_win_pos {
                if win_pos != last_win_pos {
                    let diff: Vec2f = (win_pos - last_win_pos).into();
                    self.screen.pos += diff / self.screen.scale;
                }
            }
        }
        self.last_win_pos = int_info.window_info.position;

        egui::CentralPanel::default()
            .frame(Frame::central_panel(ctx.style().as_ref()).inner_margin(Margin::same(0.0)))
            .show(ctx, |ui| {
                let rect = ui.max_rect();

                self.screen.update(ui, rect);

                let paint = ui.painter_at(rect);

                let font_id = TextStyle::Monospace.resolve(ui.style());

                let mut grid_ds_cell_size = self.screen.scale;

                while grid_ds_cell_size < 6.0 {
                    grid_ds_cell_size *= 16.0;
                }

                App::draw_grid(
                    self.screen.pos * self.screen.scale / grid_ds_cell_size,
                    grid_ds_cell_size.into(),
                    16.into(),
                    rect,
                    &paint,
                );

                self.draw_cross(rect, &paint);

                let bounds = self.calc_draw_bounds(rect);
                let ctx = PaintContext {
                    screen: self.screen,
                    paint: &paint,
                    rect,
                    bounds,
                    ui,
                    egui_ctx: ctx,
                };

                self.wires.update(&ctx);

                let update_time = Instant::now() - start_time;

                paint.text(
                    rect.left_top() + vec2(10.0, 10.0),
                    Align2::LEFT_TOP,
                    format!(
                        r#"Pos: {}
Tile draw bounds: {} - {}
Chunk draw bounds: {} - {}
Wires drawn: {}
time: {:.4} ms
"#,
                        self.screen.pos,
                        bounds.tiles_tl,
                        bounds.tiles_br,
                        bounds.chunks_tl,
                        bounds.chunks_br,
                        *self.wires.wire_parts_drawn.read().unwrap(),
                        update_time.as_secs_f64() * 1000.0
                    ),
                    font_id,
                    Color32::WHITE,
                );
            });
    }
}

impl App {
    fn new() -> Self {
        Self {
            screen: PanAndZoom::new(0.0.into(), 16.0),
            wires: Wires::new(),
            ..Default::default()
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
        let visible_cells = (Vec2f::from(rect.size()) / cell_size).convert_values(|v| v as i32 + 2);
        let start = (pos / cell_size).convert_values(|v| v as i32);
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
        let mid_off = pos % (cell_size * mid_lines.convert_values(|v| v as f32));

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
        let mut cross_pos = self.screen.world_to_screen(0.0.into());

        *cross_pos.x_mut() = cross_pos.x().clamp(0.0, bounds.width());
        *cross_pos.y_mut() = cross_pos.y().clamp(0.0, bounds.height());

        cross_pos += Vec2f::from(bounds.left_top());

        let unit = Vec2f::single_value(self.screen.scale);

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
        let screen = &self.screen;
        let tile_size: Vec2f = screen.scale.into();
        let chunk_size: Vec2f = (screen.scale * 16.0).into();

        let screen_tl = screen.pos * screen.scale;
        let screen_br = screen_tl + rect.size();

        TileDrawBounds {
            tile_size,
            chunk_size,

            screen_tl,
            screen_br,

            tiles_tl: (screen_tl / screen.scale).convert_values(|v| v.floor() as i32),
            tiles_br: (screen_br / screen.scale).convert_values(|v| v.floor() as i32),

            chunks_tl: (screen_tl / chunk_size).convert_values(|v| v.floor() as i32),
            chunks_br: (screen_br / chunk_size).convert_values(|v| v.floor() as i32),
        }
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

impl<T: SizeCalc> SizeCalc for &[T] {
    fn calc_size_inner(&self) -> usize {
        self.iter()
            .map(|i| i.calc_size_outer())
            .fold(0, |a, b| a + b)
    }
}

impl<T: SizeCalc> SizeCalc for Vec<T> {
    fn calc_size_inner(&self) -> usize {
        self.iter()
            .map(|i| i.calc_size_outer())
            .fold(0, |a, b| a + b)
    }
}

impl<const SIZE: usize, T: SizeCalc> SizeCalc for [T; SIZE] {
    fn calc_size_inner(&self) -> usize {
        self.iter()
            .map(|i| i.calc_size_inner())
            .fold(0, |a, b| a + b)
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
