#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
#![feature(int_roundings)]

use std::{
    collections::{hash_map::DefaultHasher, HashSet},
    f32::consts::TAU,
    hash::Hasher,
    mem::{size_of, MaybeUninit},
    num::{NonZeroU32, NonZeroUsize},
    sync::RwLock,
    time::Instant,
};

use eframe::{
    egui::{self, Frame, Margin, Response, Sense, TextStyle},
    epaint::{Color32, Rounding, Stroke},
};
use emath::{pos2, vec2, Align2, Pos2, Rect};
use vector::{Vec2i, Vector};

use crate::vector::Vec2f;

mod r#const;
mod vector;

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
    screen_pos: Vec2f,
    scale: f32,
    last_win_pos: Option<Pos2>,

    wire_drag_pos: Option<Vec2i>,
    wire_nodes: Chunks2D<16, WireNode>,
    wires: FixedVec<Wire>,

    wire_parts_drawn: RwLock<u32>,
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

#[derive(Debug, Default)]
struct Wire {
    id: usize,
    color: Color32,
    nodes: Vec<Vec2i>,
}

impl Wire {
    fn dummy() -> Self {
        Self {
            id: 0,
            color: Color32::from_rgb(255, 0, 255),
            nodes: vec![],
        }
    }
}

struct WirePart {
    pub pos: Vec2i,
    pub length: NonZeroU32,
    pub vertical: bool,
}

#[derive(Default, Clone, Copy)]
struct WireNode {
    up: u32,
    left: u32,
    wire: usize,
}

impl eframe::App for App {
    fn update(&mut self, ctx: &eframe::egui::Context, frame: &mut eframe::Frame) {
        let start_time = Instant::now();
        let int_info = frame.info();
        if let Some(win_pos) = int_info.window_info.position {
            if let Some(last_win_pos) = self.last_win_pos {
                if win_pos != last_win_pos {
                    let diff: Vec2f = (win_pos - last_win_pos).into();
                    self.screen_pos += diff / self.scale;
                }
            }
        }
        self.last_win_pos = int_info.window_info.position;

        *self.wire_parts_drawn.write().unwrap() = 0;

        egui::CentralPanel::default()
            .frame(Frame::central_panel(ctx.style().as_ref()).inner_margin(Margin::same(0.0)))
            .show(ctx, |ui| {
                let rect = ui.max_rect();

                let interaction = ui.interact(rect, ui.id(), Sense::drag());

                self.handle_pan_zoom(ui, rect, &interaction);

                let paint = ui.painter_at(rect);

                let font_id = TextStyle::Monospace.resolve(ui.style());

                let mut grid_ds_cell_size = self.scale;

                while grid_ds_cell_size < 6.0 {
                    grid_ds_cell_size *= 16.0;
                }

                App::draw_grid(
                    self.screen_pos * self.scale / grid_ds_cell_size,
                    grid_ds_cell_size.into(),
                    16.into(),
                    rect,
                    &paint,
                );

                self.draw_cross(rect, &paint);

                let bounds = self.calc_draw_bounds(rect);

                self.draw_chunks(
                    &self.wire_nodes,
                    rect,
                    &paint,
                    bounds,
                    &self,
                    |node, pos, _, this, paint, bounds, lookaround| {
                        fn draw_wire(
                            dist: u32,
                            next_dist: u32,
                            wire: usize,
                            vertical: bool,
                            app: &App,
                            pos: Vec2i,
                            paint: &egui::Painter,
                            bounds: &TileDrawBounds,
                            color: Color32,
                        ) {
                            if dist == 0 && wire == 0 {
                                return;
                            }

                            let edge = match vertical {
                                true => pos.y() == bounds.tiles_br.y(),
                                false => pos.x() == bounds.tiles_br.x(),
                            };

                            if (wire == 0 || dist == 0) && !edge {
                                return;
                            }

                            let length = match next_dist {
                                0 => {
                                    if dist == 0 {
                                        return;
                                    } else {
                                        dist
                                    }
                                }
                                node_dist => {
                                    if node_dist == dist + 1 {
                                        node_dist
                                    } else {
                                        dist
                                    }
                                }
                            };

                            let part = WirePart {
                                length: NonZeroU32::new(length).unwrap(),
                                pos: pos
                                    - if vertical {
                                        [0, dist as i32]
                                    } else {
                                        [dist as i32, 0]
                                    },
                                vertical,
                            };

                            app.draw_wire_part(paint, &part, color)
                        }

                        if node.wire == 0 && node.up == 0 && node.left == 0 {
                            return;
                        }

                        let wires = this.wire_at_node(pos, node);

                        let wire_color_v = wires
                            .up()
                            .map_or(Color32::from_rgb(255, 100, 255), |w| w.color);

                        let wire_color_h = wires
                            .left()
                            .map_or(Color32::from_rgb(255, 100, 255), |w| w.color);

                        let next_node_v = lookaround.get_relative(0, 1);
                        let next_node_h = lookaround.get_relative(1, 0);

                        draw_wire(
                            node.left,
                            next_node_h.map_or(0, |n| n.left),
                            node.wire,
                            false,
                            this,
                            pos,
                            paint,
                            bounds,
                            wire_color_h,
                        );
                        draw_wire(
                            node.up,
                            next_node_v.map_or(0, |n| n.up),
                            node.wire,
                            true,
                            this,
                            pos,
                            paint,
                            bounds,
                            wire_color_v,
                        );

                        if node.wire > 0 {
                            //let count = [
                            //    node.left > 0,
                            //    node.up > 0,
                            //    next_node_h.is_some_and(|n| n.left == 1),
                            //    next_node_v.is_some_and(|n| n.up == 1),
                            //]
                            //.iter()
                            //.filter(|v| **v)
                            //.count();
                            //if count > 2 {
                            //    this.draw_wire_intersection(paint, pos, wire.color)
                            //}

                            if node.left > 0
                                && node.up > 0
                                && next_node_h.is_some_and(|n| n.left == 1)
                                && next_node_v.is_some_and(|n| n.up == 1)
                            {
                                if let Some(wire) = self.wires.get(node.wire) {
                                    this.draw_wire_intersection(paint, pos, wire.color)
                                }
                            }
                        }

                        let correct_up = node.up == 0
                            || lookaround
                                .get_relative(0, -(node.up as isize))
                                .is_some_and(|n| n.wire > 0)
                                && (1..node.up as isize).all(|p| {
                                    lookaround.get_relative(0, -p).is_some_and(|n| n.wire == 0)
                                });

                        let correct_left = node.left == 0
                            || lookaround
                                .get_relative(-(node.left as isize), 0)
                                .is_some_and(|n| n.wire > 0)
                                && (1..node.left as isize).all(|p| {
                                    lookaround.get_relative(-p, 0).is_some_and(|n| n.wire == 0)
                                });

                        if !correct_up || !correct_left {
                            let pos = this.world_to_screen_tile(pos);

                            let rect =
                                Rect::from_min_size(pos.into(), vec2(this.scale, this.scale));
                            paint.rect_filled(
                                rect,
                                Rounding::none(),
                                Color32::from_rgba_unmultiplied(255, 0, 0, 100),
                            );
                        }
                    },
                );

                let mouse_tile_pos = ctx
                    .input(|input| input.pointer.interact_pos())
                    .map(|p| self.screen_to_world(Vec2f::from(p) - rect.left_top()));

                let mouse_tile_pos_i =
                    mouse_tile_pos.map(|p| p.convert_values(|v| v.floor() as i32));

                let drawing_wire = Self::calc_wire_part(self.wire_drag_pos, mouse_tile_pos_i);
                if let Some(ref part) = drawing_wire {
                    self.draw_wire_part(&paint, part, Color32::GRAY);
                }

                if self.wire_drag_pos.is_none()
                    && interaction.drag_started_by(egui::PointerButton::Primary)
                {
                    self.wire_drag_pos = mouse_tile_pos_i;
                } else if self.wire_drag_pos.is_some()
                    && interaction.drag_released_by(egui::PointerButton::Primary)
                {
                    self.wire_drag_pos = None;

                    // TODO: travel wires and equalize wire IDs
                    if let Some(part) = drawing_wire {
                        self.place_wire_part(part);
                    }
                }

                let update_time = Instant::now() - start_time;

                paint.text(
                    rect.left_top() + vec2(10.0, 10.0),
                    Align2::LEFT_TOP,
                    format!(
                        r#"Pos: {}
Tile draw bounds: {} - {}
Chunk draw bounds: {} - {}
Wires drawn: {}
size of wires: {}
size of chunk 0: {}
size of tile 0: {}
time: {:.4} ms
"#,
                        self.screen_pos,
                        bounds.tiles_tl,
                        bounds.tiles_br,
                        bounds.chunks_tl,
                        bounds.chunks_br,
                        *self.wire_parts_drawn.read().unwrap(),
                        format_size(self.wire_nodes.calc_size_outer()),
                        format_size(
                            self.wire_nodes
                                .get_chunk(0, 0)
                                .map(|c| c.calc_size_outer())
                                .unwrap_or_default()
                        ),
                        format_size(
                            self.wire_nodes
                                .get(0, 0)
                                .map(|c| c.calc_size_outer())
                                .unwrap_or_default()
                        ),
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
            scale: 16.0,
            wires: vec![Wire::dummy()].into(),
            ..Default::default()
        }
    }

    fn screen_to_world(&self, v: Vec2f) -> Vec2f {
        self.screen_pos + v / self.scale
    }

    fn world_to_screen(&self, v: Vec2f) -> Vec2f {
        (v - self.screen_pos) * self.scale
    }

    fn screen_to_world_tile(&self, v: Vec2f) -> Vec2i {
        (self.screen_pos + v / self.scale).convert_values(|v| v.floor() as i32)
    }

    fn world_to_screen_tile(&self, v: Vec2i) -> Vec2f {
        (v.convert_values(|v| v as f32) - self.screen_pos) * self.scale
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

    fn handle_pan_zoom(&mut self, ui: &egui::Ui, rect: Rect, interaction: &Response) {
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

        if interaction.dragged_by(egui::PointerButton::Secondary) {
            self.screen_pos -= interaction.drag_delta() / self.scale;
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
            self.screen_pos -= world_after - world_before;
        }
    }

    fn draw_cross(&mut self, bounds: Rect, paint: &egui::Painter) {
        let mut cross_pos = self.world_to_screen(0.0.into());

        *cross_pos.x_mut() = cross_pos.x().clamp(0.0, bounds.width());
        *cross_pos.y_mut() = cross_pos.y().clamp(0.0, bounds.height());

        cross_pos += Vec2f::from(bounds.left_top());

        let unit = Vec2f::single_value(self.scale);

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

    fn draw_chunks<const CHUNK_SIZE: usize, T: Default, P>(
        &self,
        chunks: &Chunks2D<CHUNK_SIZE, T>,
        rect: Rect,
        paint: &egui::Painter,
        bounds: TileDrawBounds,
        pass: &P,
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
                    + self.world_to_screen(chunk_tl.convert_values(|v| v as f32));
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

                        let pos: Vec2i = chunk_tl + [i, j];

                        let tile = &chunk[i as usize][j as usize];

                        let draw_pos = Vec2f::from(rect.left_top())
                            + self.world_to_screen(pos.convert_values(|v| v as f32));
                        let rect =
                            Rect::from_min_size(draw_pos.into(), vec2(self.scale, self.scale));
                        let lookaround = ChunksLookaround {
                            chunks,
                            chunk,
                            pos: pos.convert_values(|v| v as isize),
                            in_chunk_pos: [i as usize, j as usize].into(),
                        };
                        drawer(tile, pos, rect, pass, paint, &bounds, &lookaround)
                    }
                }
            }
        }
    }

    fn calc_draw_bounds(&self, rect: Rect) -> TileDrawBounds {
        let tile_size: Vec2f = self.scale.into();
        let chunk_size: Vec2f = (self.scale * 16.0).into();

        let screen_tl = self.screen_pos * self.scale;
        let screen_br = screen_tl + rect.size();

        TileDrawBounds {
            tile_size,
            chunk_size,

            screen_tl,
            screen_br,

            tiles_tl: (screen_tl / self.scale).convert_values(|v| v.floor() as i32),
            tiles_br: (screen_br / self.scale).convert_values(|v| v.floor() as i32),

            chunks_tl: (screen_tl / chunk_size).convert_values(|v| v.floor() as i32),
            chunks_br: (screen_br / chunk_size).convert_values(|v| v.floor() as i32),
        }
    }

    fn draw_wire_part(&self, paint: &egui::Painter, part: &WirePart, color: Color32) {
        let thickness = self.scale * 0.25;

        let pos = self.world_to_screen_tile(part.pos) + ((self.scale - thickness) * 0.5);
        let length = self.scale * part.length.get() as f32 + thickness;

        let rect_size = match part.vertical {
            true => vec2(thickness, length),
            false => vec2(length, thickness),
        };
        let rect = Rect::from_min_size(pos.into(), rect_size);
        paint.rect_filled(rect, Rounding::none(), color);

        *self.wire_parts_drawn.write().unwrap() += 1;
    }

    fn draw_wire_intersection(&self, paint: &egui::Painter, pos: Vec2i, color: Color32) {
        let thickness = self.scale * 0.4;

        let pos = self.world_to_screen_tile(pos) + ((self.scale - thickness) * 0.5);

        let rect = Rect::from_min_size(pos.into(), vec2(thickness, thickness));
        paint.rect_filled(rect, Rounding::none(), color);
    }

    fn calc_wire_part(from: Option<Vec2i>, to: Option<Vec2i>) -> Option<WirePart> {
        if let Some(from) = from {
            if let Some(to) = to {
                if from != to {
                    let angle = (to - from).convert_values(|v| v as f32).angle_to_x();
                    let axis = (angle / (TAU / 4.0)).round() as i32 % 4;

                    let origin = match axis {
                        1 => [from.x(), to.y()].into(),
                        2 => [to.x(), from.y()].into(),
                        _ => from,
                    };
                    let length = match axis {
                        0 => to.x() - from.x(),
                        1 => from.y() - to.y(),
                        2 => from.x() - to.x(),
                        3 => to.y() - from.y(),
                        _ => unreachable!(),
                    } as u32;

                    let vertical = match axis {
                        1 | 3 => true,
                        _ => false,
                    };

                    let part = WirePart {
                        pos: origin,
                        length: NonZeroU32::new(length).unwrap(),
                        vertical,
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

    fn place_wire_part(&mut self, part: WirePart) {
        fn split_wire_at(this: &mut App, at: Vec2i) {
            let node = this.wire_nodes.get(at.x() as isize, at.y() as isize);
            let node = match node {
                Some(v) => *v,
                None => return,
            };

            fn split_wire_dir(
                wires: &mut Chunks2D<16, WireNode>,
                at: Vec2i,
                vertical: bool,
                dist: u32,
            ) {
                for split_dist in 1.. {
                    let node =
                        match vertical {
                            true => wires
                                .get_mut(at.x() as isize, at.y() as isize + split_dist as isize),
                            false => wires
                                .get_mut(at.x() as isize + split_dist as isize, at.y() as isize),
                        };
                    let node = match node {
                        Some(v) => v,
                        None => break,
                    };
                    let node_dir = match vertical {
                        true => &mut node.up,
                        false => &mut node.left,
                    };
                    if *node_dir != split_dist + dist {
                        // went off the rails!
                        break;
                    }
                    *node_dir = split_dist;

                    if node.wire > 0 {
                        break;
                    }
                }
            }

            if node.wire > 0 {
                return;
            }

            let (left, up) = match this.wire_at_node(at, &node) {
                TileWires::None => return,
                TileWires::One { wire, dir } => match dir {
                    WireDirection::None => return,
                    WireDirection::Up => (0, wire.id),
                    WireDirection::Left => (wire.id, 0),
                },
                TileWires::Two { left, up } => (left.id, up.id),
            };

            if up == 0 && left == 0 {
                return;
            }

            if up > 0 {
                split_wire_dir(&mut this.wire_nodes, at, true, node.up);
            }
            if left > 0 {
                split_wire_dir(&mut this.wire_nodes, at, false, node.left);
            }
            let node = this
                .wire_nodes
                .get_or_create_mut(at.x() as isize, at.y() as isize);

            let first = if up > 0 { up } else { left };

            if up > 0 && left > 0 && up != left {
                todo!("Merge wires")
            }
            else {
                node.wire = first;
                let wire = this.wires.get_mut(first).unwrap();
                wire.nodes.push(at);
            }
        }

        let wires_crossed = {
            let mut wires_crossed = HashSet::new();
            for i in 0..=part.length.get() {
                let pos = part.pos
                    + match part.vertical {
                        true => [0, i as i32],
                        false => [i as i32, 0],
                    };

                let node = match self.wire_nodes.get(pos.x() as isize, pos.y() as isize) {
                    None => continue,
                    Some(v) => v,
                };

                if i == 0 || i == part.length.get() {
                    match self.wires_at(pos) {
                        TileWires::None => (),
                        TileWires::One { wire, dir: _ } => {
                            wires_crossed.insert(wire.id);
                        }
                        TileWires::Two { left, up } => {
                            wires_crossed.insert(left.id);
                            wires_crossed.insert(up.id);
                        }
                    }
                } else if node.wire > 0 {
                    wires_crossed.insert(node.wire);
                }
            }
            wires_crossed
        };

        let new_wire = match wires_crossed.len() {
            0 => self.create_wire().id,
            1 => *wires_crossed.iter().next().unwrap(),
            _ => {
                let main_wire = self
                    .wires
                    .iter()
                    .filter(|v| wires_crossed.contains(&v.id))
                    .max_by(|x, y| x.nodes.len().cmp(&y.nodes.len()))
                    .expect("Some matching wires")
                    .id;

                for wire in wires_crossed {
                    if wire != main_wire {
                        self.merge_wires(main_wire, wire)
                    }
                }
                main_wire
            }
        };

        let mut dist = 0;
        for i in 0..=part.length.get() {
            let pos = part.pos
                + match part.vertical {
                    true => [0, i as i32],
                    false => [i as i32, 0],
                };

            if i == 0 || i == part.length.get() {
                split_wire_at(self, pos, );
            }

            let node = self
                .wire_nodes
                .get_or_create_mut(pos.x() as isize, pos.y() as isize);

            if i > 0 {
                match part.vertical {
                    true => node.up = dist,
                    false => node.left = dist,
                }
            }
            if node.wire > 0 {
                dist = 1
            } else {
                dist += 1;
            }
            if (i == 0 || i == part.length.get()) && node.wire != new_wire {
                node.wire = new_wire;
                self.wires.get_mut(new_wire).unwrap().nodes.push(pos);
            }
        }
    }

    fn wires_at(&self, pos: Vec2i) -> TileWires<'_> {
        match self.wire_nodes.get(pos.x() as isize, pos.y() as isize) {
            None => TileWires::None,
            Some(node) => self.wire_at_node(pos, node),
        }
    }

    fn wire_at_node(&self, pos: Vec2i, node: &WireNode) -> TileWires {
        if node.wire > 0 {
            return match self.wires.get(node.wire) {
                None => TileWires::None,
                Some(wire) => TileWires::One {
                    wire,
                    dir: WireDirection::None,
                },
            };
        }

        let up = match node.up {
            0 => None,
            up => self
                .wire_nodes
                .get(pos.x() as isize, (pos.y() - up as i32) as isize)
                .and_then(|n| match n.wire {
                    0 => None,
                    wire => self.wires.get(wire),
                }),
        };
        let left = match node.left {
            0 => None,
            left => self
                .wire_nodes
                .get((pos.x() - left as i32) as isize, pos.y() as isize)
                .and_then(|n| match n.wire {
                    0 => None,
                    wire => self.wires.get(wire),
                }),
        };

        match (left, up) {
            (None, None) => TileWires::None,
            (None, Some(u)) => TileWires::One {
                wire: u,
                dir: WireDirection::Up,
            },
            (Some(l), None) => TileWires::One {
                wire: l,
                dir: WireDirection::Left,
            },
            (Some(left), Some(up)) => TileWires::Two { left, up },
        }
    }

    fn create_wire(&mut self) -> &mut Wire {
        let id = self.wires.first_free_pos();

        let mut hash = DefaultHasher::new();
        hash.write_usize(id);
        hash.write_u16(61942);
        let color = hash.finish().to_le_bytes();

        let wire = Wire {
            id,
            color: Color32::from_rgb_additive(color[0], color[1], color[2]),
            ..Default::default()
        };

        self.wires.set(wire, id).value_ref
    }

    fn merge_wires(&mut self, wire: usize, with: usize) {
        if wire == 0
            || !self.wires.exists(wire)
            || with == 0
            || !self.wires.exists(with)
            || wire == with
        {
            return;
        }

        let Wire {
            id: _,
            color: _,
            nodes,
        } = self.wires.remove(with).unwrap();

        for npos in nodes.iter() {
            match self
                .wire_nodes
                .get_mut(npos.x() as isize, npos.y() as isize)
            {
                None => {}
                Some(n) => n.wire = wire,
            }
        }

        let wire = &mut self.wires.get_mut(wire).unwrap();
        wire.nodes.extend(nodes);
    }
}

enum WireDirection {
    None,
    Up,
    Left,
}

enum TileWires<'a> {
    None,
    One { wire: &'a Wire, dir: WireDirection },
    Two { left: &'a Wire, up: &'a Wire },
}

impl TileWires<'_> {
    fn up(&self) -> Option<&Wire> {
        match self {
            TileWires::One { wire, dir }
                if matches!(dir, WireDirection::Up) || matches!(dir, WireDirection::None) =>
            {
                Some(wire)
            }
            TileWires::Two { left: _, up } => Some(up),
            _ => None,
        }
    }

    fn left(&self) -> Option<&Wire> {
        match self {
            TileWires::One { wire, dir }
                if matches!(dir, WireDirection::Left) || matches!(dir, WireDirection::None) =>
            {
                Some(wire)
            }
            TileWires::Two { left, up: _ } => Some(left),
            _ => None,
        }
    }
}

type Chunk<const CHUNK_SIZE: usize, T> = [[T; CHUNK_SIZE]; CHUNK_SIZE];
type ChunksQuarter<const CHUNK_SIZE: usize, T> =
    Vec<Option<Vec<Option<Box<Chunk<CHUNK_SIZE, T>>>>>>;

#[derive(Default)]
struct Chunks2D<const CHUNK_SIZE: usize, T: Default> {
    quarters: [ChunksQuarter<CHUNK_SIZE, T>; 4],
}

struct ChunksLookaround<'a, const CHUNK_SIZE: usize, T: Default> {
    chunks: &'a Chunks2D<CHUNK_SIZE, T>,
    chunk: &'a Chunk<CHUNK_SIZE, T>,
    pos: Vector<2, isize>,
    in_chunk_pos: Vector<2, usize>,
}

impl<'a, const CHUNK_SIZE: usize, T: Default> ChunksLookaround<'a, CHUNK_SIZE, T> {
    pub fn get_relative(&self, x: isize, y: isize) -> Option<&T> {
        let target_x = x + self.in_chunk_pos.x() as isize;
        let target_y = y + self.in_chunk_pos.y() as isize;

        if target_x < 0
            || target_x >= CHUNK_SIZE as isize
            || target_y < 0
            || target_y >= CHUNK_SIZE as isize
        {
            self.chunks.get(self.pos.x() + x, self.pos.y() + y)
        } else {
            Some(&self.chunk[target_x as usize][target_y as usize])
        }
    }
}

#[allow(dead_code)]
impl<const CHUNK_SIZE: usize, T: Default> Chunks2D<CHUNK_SIZE, T> {
    const QUARTER_TL: usize = 0;
    const QUARTER_TR: usize = 1;
    const QUARTER_BL: usize = 2;
    const QUARTER_BR: usize = 3;

    const QUARTERS_BOTTOM: usize = 0x2;
    const QUARTERS_RIGHT: usize = 0x1;

    fn to_chunk_pos(v: isize) -> (isize, usize) {
        let chunk = v.div_floor(CHUNK_SIZE as isize);
        let pos = (v - (chunk * CHUNK_SIZE as isize)) as usize;
        (chunk, pos)
    }

    fn to_quarter_id_pos(chunk_x: isize, chunk_y: isize) -> ((usize, usize), usize) {
        let (x, right) = if chunk_x >= 0 {
            (chunk_x as usize, true)
        } else {
            ((-chunk_x - 1) as usize, false)
        };

        let (y, bottom) = if chunk_y >= 0 {
            (chunk_y as usize, true)
        } else {
            ((-chunk_y - 1) as usize, false)
        };

        let quarter = match (right, bottom) {
            (false, false) => Self::QUARTER_TL,
            (false, true) => Self::QUARTER_BL,
            (true, false) => Self::QUARTER_TR,
            (true, true) => Self::QUARTER_BR,
        };

        ((x, y), quarter)
    }

    pub fn get_chunk<'a>(&'a self, x: isize, y: isize) -> Option<&'a Chunk<CHUNK_SIZE, T>> {
        let ((qx, qy), qid) = Self::to_quarter_id_pos(x, y);
        let quarter = &self.quarters[qid];
        let col = quarter.get(qx)?;
        col.as_ref()
            .and_then(|col| col.get(qy)?.as_ref().map(|b| b.as_ref()))
    }

    pub fn get_chunk_mut<'a>(
        &'a mut self,
        x: isize,
        y: isize,
    ) -> Option<&'a mut Chunk<CHUNK_SIZE, T>> {
        let ((qx, qy), qid) = Self::to_quarter_id_pos(x, y);
        let quarter = &mut self.quarters[qid];
        let col = quarter.get_mut(qx)?;
        col.as_mut()
            .and_then(|col| col.get_mut(qy)?.as_mut().map(|b| b.as_mut()))
    }

    pub fn get_or_create_chunk_mut(&mut self, x: isize, y: isize) -> &mut Chunk<CHUNK_SIZE, T> {
        let ((qx, qy), qid) = Self::to_quarter_id_pos(x, y);
        let quarter = &mut self.quarters[qid];

        if quarter.capacity() <= qx {
            quarter.reserve_exact(qx + 1 - quarter.len());
        }

        while quarter.len() <= qx {
            quarter.push(None);
        }
        let col = &mut quarter[qx];

        match col.as_mut() {
            None => {
                *col = Some(Vec::with_capacity(qy + 1));
            }
            Some(v) if v.capacity() <= qy => v.reserve_exact(qy + 1 - v.len()),
            _ => {}
        }

        let col = match col.as_mut() {
            Some(v) => v,
            None => unreachable!(),
        };

        while col.len() <= qy {
            col.push(None);
        }
        let chunk = &mut col[qy];

        if chunk.is_none() {
            let mut arr: Chunk<CHUNK_SIZE, T> = unsafe { MaybeUninit::uninit().assume_init() };
            for i in 0..CHUNK_SIZE {
                let col = &mut arr[i];
                for j in 0..CHUNK_SIZE {
                    col[j] = T::default();
                }
            }
            *chunk = Some(Box::new(arr));
        }

        match chunk.as_mut() {
            Some(v) => v,
            None => unreachable!(),
        }
    }

    pub fn get(&self, x: isize, y: isize) -> Option<&T> {
        let (chunk_x, x) = Self::to_chunk_pos(x);
        let (chunk_y, y) = Self::to_chunk_pos(y);

        let chunk = self.get_chunk(chunk_x, chunk_y)?;
        Some(&chunk[x][y])
    }

    pub fn get_mut(&mut self, x: isize, y: isize) -> Option<&mut T> {
        let (chunk_x, x) = Self::to_chunk_pos(x);
        let (chunk_y, y) = Self::to_chunk_pos(y);

        let chunk = self.get_chunk_mut(chunk_x, chunk_y)?;
        Some(&mut chunk[x][y])
    }

    pub fn get_or_create_mut(&mut self, x: isize, y: isize) -> &mut T {
        let (chunk_x, x) = Self::to_chunk_pos(x);
        let (chunk_y, y) = Self::to_chunk_pos(y);

        let chunk = self.get_or_create_chunk_mut(chunk_x, chunk_y);
        &mut chunk[x][y]
    }

    pub fn chunk_exists_at(&self, x: isize, y: isize) -> bool {
        let (chunk_x, _) = Self::to_chunk_pos(x);
        let (chunk_y, _) = Self::to_chunk_pos(y);

        self.get_chunk(chunk_x, chunk_y).is_some()
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

impl<const CHUNK_SIZE: usize, T: SizeCalc + Default> SizeCalc for Chunks2D<CHUNK_SIZE, T> {
    fn calc_size_inner(&self) -> usize {
        self.quarters.calc_size_inner()
    }
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

impl SizeCalc for WireNode {
    fn calc_size_inner(&self) -> usize {
        0
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

#[derive(Default, Debug)]
struct FixedVec<T> {
    vec: Vec<Option<T>>,
    first_free: Option<usize>,
}

struct VecSetResult<'a, T> {
    value_ref: &'a mut T,
    prev: Option<T>,
}

impl<T> FixedVec<T> {
    fn new(mut vec: Vec<T>) -> Self {
        Self {
            vec: vec.drain(..).map(|v| Some(v)).collect(),
            first_free: None,
        }
    }

    fn get(&self, pos: usize) -> Option<&T> {
        self.vec.get(pos)?.as_ref()
    }

    fn get_mut(&mut self, pos: usize) -> Option<&mut T> {
        self.vec.get_mut(pos)?.as_mut()
    }

    fn remove(&mut self, pos: usize) -> Option<T> {
        if pos >= self.vec.len() {
            return None;
        }

        let item = &mut self.vec[pos];

        if item.is_some() {
            match &mut self.first_free {
                Some(v) if *v < pos => {}
                fe => *fe = Some(pos),
            }
        }

        item.take()
    }

    fn exists(&self, pos: usize) -> bool {
        self.vec.get(pos).is_some_and(|v| v.is_some())
    }

    fn set<'a>(&'a mut self, value: T, into: usize) -> VecSetResult<'a, T> {
        if into >= self.vec.len() {
            if self.vec.len() != into && self.first_free.is_none() {
                self.first_free = Some(self.vec.len())
            }
            self.vec.reserve(into + 1 - self.vec.len());
            while self.vec.len() < into {
                self.vec.push(None)
            }
            self.vec.push(Some(value));
            return VecSetResult {
                value_ref: self.vec[into].as_mut().unwrap(),
                prev: None,
            };
        };

        if let Some(ff) = self.first_free {
            if ff == into {
                self.first_free = (ff + 1..self.vec.len()).find(|i| matches!(self.vec[*i], None));
            }
        }

        let prev = self.vec.get_mut(into).unwrap().replace(value);

        return VecSetResult {
            value_ref: self.vec[into].as_mut().unwrap(),
            prev: prev,
        };
    }

    fn first_free_pos(&self) -> usize {
        match self.first_free {
            Some(v) => v,
            None => self.vec.len(),
        }
    }

    fn iter<'a>(&'a self) -> FixedVecIterator<'a, T> {
        FixedVecIterator {
            vec: &self.vec,
            pos: 0,
        }
    }
}

impl<T> From<Vec<T>> for FixedVec<T> {
    fn from(value: Vec<T>) -> Self {
        Self::new(value)
    }
}

struct FixedVecIterator<'a, T> {
    vec: &'a Vec<Option<T>>,
    pos: usize,
}

impl<'a, T> Iterator for FixedVecIterator<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        while self.pos < self.vec.len() {
            self.pos += 1;
            if let Some(v) = &self.vec[self.pos - 1] {
                return Some(v);
            }
        }
        None
    }
}
