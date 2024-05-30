use std::{
    f32::consts::{PI, SQRT_2, TAU},
    hash::{DefaultHasher, Hasher},
    num::NonZeroU32,
    ops::{Deref, Not},
    sync::Arc,
};

use eframe::{
    egui::{
        remap_clamp, Align, Color32, Key, PointerButton, Rect, Response, Rounding, Sense, Stroke,
        TextStyle, Ui, WidgetText,
    },
    epaint::TextShape,
};
use parking_lot::{Mutex, RwLock};

use crate::{
    app::App,
    board::BoardEditor,
    vector::{Vec2f, Vec2isize, Vector2},
    vertex_renderer::{LineBuffer, TriangleBuffer, VertexRenderer},
    Direction4Half, Direction8, Direction8Array, PaintContext, Screen, VertexPaintContext,
    CHUNK_SIZE, WIRE_WIDTH,
};

use super::{TabCreation, TabImpl};

pub struct BoardView {
    pan_zoom: PanAndZoom,

    vertexes: Option<Arc<VertexRenderer>>,

    wire_draw_start: Option<Vec2isize>,

    editor: Arc<RwLock<BoardEditor>>,
    fixed_screen_pos: Option<(Rect, PanAndZoom)>,
    wire_debug: bool,

    grid_buffer: Arc<Mutex<LineBuffer>>,
    wire_part_buffer: Arc<Mutex<TriangleBuffer>>,
}

impl TabCreation for BoardView {
    fn new() -> Self {
        Self {
            pan_zoom: Default::default(),
            vertexes: None,
            wire_draw_start: None,

            editor: Default::default(),
            fixed_screen_pos: None,
            wire_debug: false,

            grid_buffer: Arc::new(Mutex::new(LineBuffer::new(1.0))),
            wire_part_buffer: Default::default(),
        }
    }
}

impl TabImpl for BoardView {
    fn update(&mut self, app: &mut App, ui: &mut Ui) {
        let screen_rect = ui.max_rect();
        let interaction = ui.interact(
            screen_rect,
            ui.id().with("board-interaction"),
            Sense::click_and_drag(),
        );
        if self.fixed_screen_pos.is_none() || ui.input(|input| input.modifiers.alt) {
            self.pan_zoom.update(
                ui,
                screen_rect,
                interaction.dragged_by(PointerButton::Secondary),
                &interaction,
            );
        }

        let global_screen = self.pan_zoom.to_screen(screen_rect);

        if ui.input(|input| input.key_pressed(Key::F5)) {
            match self.fixed_screen_pos.is_none() {
                true => {
                    let rect = Rect::from_min_max(
                        global_screen.screen_to_world(0.0).into(),
                        global_screen.screen_to_world(screen_rect.size()).into(),
                    );
                    self.fixed_screen_pos =
                        Some((rect, PanAndZoom::new(rect.center().into(), 1.0)));
                    let mul = (screen_rect.width() - 40.0) / screen_rect.width();
                    self.pan_zoom.scale *= mul;
                }
                false => {
                    self.fixed_screen_pos = None;
                }
            }
        }

        let screen = match &mut self.fixed_screen_pos {
            None => global_screen,
            Some((rect, pan_zoom)) => {
                pan_zoom.scale *= self.pan_zoom.scale;
                let screen_rect = global_screen.world_to_screen_rect(*rect);
                if !ui.input(|input| input.modifiers.alt) {
                    pan_zoom.update(
                        ui,
                        screen_rect,
                        interaction.dragged_by(PointerButton::Secondary),
                        &interaction,
                    );
                }
                let screen = pan_zoom.to_screen(screen_rect);
                pan_zoom.scale /= self.pan_zoom.scale;
                screen
            }
        };

        if self.fixed_screen_pos.is_some() {
            ui.painter().rect_stroke(
                screen.screen_rect,
                Rounding::ZERO,
                Stroke::new(1.0, Color32::RED),
            );
        }

        if ui.input(|input| input.key_pressed(Key::F9)) {
            self.wire_debug = !self.wire_debug;
        }

        let ctx = PaintContext::new(ui, screen);

        let vertexes = self
            .vertexes
            .get_or_insert_with(|| {
                Arc::new(VertexRenderer::new(&app.gl).expect("vertex renderer init"))
            })
            .clone();

        self.draw_grid(&ctx, vertexes.clone());
        self.prepare_wire_draw(&ctx);
        self.draw_wires(&ctx, vertexes.clone());

        if self.wire_debug {
            self.draw_wire_debug(&ctx);
        }
        self.handle_wire_interactions(&ctx, &interaction);
    }
}

impl BoardView {
    fn draw_grid_layer(ctx: &VertexPaintContext, buffer: &mut LineBuffer, chunks: bool) {
        let (scale_mul, fade_min, fade_max) = if chunks {
            (
                CHUNK_SIZE as f32,
                CHUNK_SIZE as f32 / 8.0,
                CHUNK_SIZE as f32 / 2.0,
            )
        } else {
            (1.0, CHUNK_SIZE as f32 / 4.0, CHUNK_SIZE as f32)
        };

        let scale = ctx.screen.scale * scale_mul;

        if ctx.screen.scale > fade_min {
            let alpha = remap_clamp(ctx.screen.scale, fade_min..=fade_max, 0.0..=1.0);

            let offx = (1.0 - ((ctx.screen.world_pos.x / scale_mul).fract() + 1.0).fract()) * scale;
            let offy = (1.0 - ((ctx.screen.world_pos.y / scale_mul).fract() + 1.0).fract()) * scale;

            let countx = (ctx.screen.screen_rect.width() / scale).ceil() as usize;
            let county = (ctx.screen.screen_rect.height() / scale).ceil() as usize;

            let white = if chunks { 0.2 } else { 0.15 };

            for i in 0..countx {
                let x = offx + i as f32 * scale + ctx.screen.screen_rect.left();
                let wx = i as isize + (ctx.screen.world_pos.x / scale_mul).floor() as isize + 1;

                if (wx % CHUNK_SIZE as isize) == 0 && !chunks || wx == 0 {
                    continue;
                }

                buffer.add_singlecolor_line(
                    [x, ctx.screen.screen_rect.top()],
                    [x, ctx.screen.screen_rect.bottom()],
                    [white * alpha, white * alpha, white * alpha, alpha],
                );
            }

            for i in 0..county {
                let y = offy + i as f32 * scale + ctx.screen.screen_rect.top();
                let wy = i as isize + (ctx.screen.world_pos.y / scale_mul).floor() as isize + 1;

                if (wy % CHUNK_SIZE as isize) == 0 && !chunks || wy == 0 {
                    continue;
                }

                buffer.add_singlecolor_line(
                    [ctx.screen.screen_rect.left(), y],
                    [ctx.screen.screen_rect.right(), y],
                    [white * alpha, white * alpha, white * alpha, alpha],
                );
            }
        }

        let zero = ctx.screen.world_to_screen(0.0);

        if ctx.screen.screen_rect.left() <= zero.x && zero.x < ctx.screen.screen_rect.right() {
            buffer.add_singlecolor_line(
                [zero.x, ctx.screen.screen_rect.top()],
                [zero.x, ctx.screen.screen_rect.bottom()],
                [0.2, 1.0, 0.2, 1.0],
            );
        }

        if ctx.screen.screen_rect.top() <= zero.y && zero.y < ctx.screen.screen_rect.bottom() {
            buffer.add_singlecolor_line(
                [ctx.screen.screen_rect.left(), zero.y],
                [ctx.screen.screen_rect.right(), zero.y],
                [1.0, 0.2, 0.2, 1.0],
            );
        }
    }

    fn draw_grid_cross(ctx: &VertexPaintContext, buffer: &mut LineBuffer) {
        let cross_pos = ctx.screen.world_to_screen(0.0);
        let cross_pos = ctx.screen.screen_rect.clamp(cross_pos.into());

        buffer.add_singlecolor_line(
            [cross_pos.x - ctx.screen.scale, cross_pos.y],
            [cross_pos.x + ctx.screen.scale, cross_pos.y],
            [1.0; 4],
        );

        buffer.add_singlecolor_line(
            [cross_pos.x, cross_pos.y - ctx.screen.scale],
            [cross_pos.x, cross_pos.y + ctx.screen.scale],
            [1.0; 4],
        );
    }

    fn draw_grid(&self, ctx: &PaintContext, vertexes: Arc<VertexRenderer>) {
        let buffer = self.grid_buffer.clone();

        ctx.draw_vertexes(vertexes, move |ctx| {
            let mut buffer = buffer.lock();
            buffer.clear();

            Self::draw_grid_layer(&ctx, &mut buffer, false);
            Self::draw_grid_layer(&ctx, &mut buffer, true);
            Self::draw_grid_cross(&ctx, &mut buffer);

            ctx.vertexes
                .draw(ctx.gl, ctx.full_screen_size.into(), buffer.deref());
        });
    }

    fn prepare_wire_draw(&mut self, ctx: &PaintContext) {
        fn node_has_135_deg_turns(dirs: &Direction8Array<Option<NonZeroU32>>) -> bool {
            for (dir, dist) in dirs.iter() {
                if dist.is_none() {
                    continue;
                }

                if dirs
                    .get(dir.rotated_clockwise_by(Direction8::DownRight))
                    .is_some()
                    || dirs
                        .get(dir.rotated_counterclockwise_by(Direction8::DownRight))
                        .is_some()
                {
                    return true;
                }
            }
            false
        }

        fn node_is_a_45_deg_turn(dirs: &Direction8Array<Option<NonZeroU32>>) -> bool {
            let mut iter = dirs.iter();

            for (dir, dist) in &mut iter {
                if dist.is_none() {
                    continue;
                }

                if dirs
                    .get(dir.rotated_clockwise_by(Direction8::UpRight))
                    .is_some()
                    || dirs
                        .get(dir.rotated_counterclockwise_by(Direction8::UpRight))
                        .is_some()
                {
                    break;
                } else {
                    return false;
                }
            }

            let mut one = false;

            for (_, d) in iter {
                if d.is_none() {
                    continue;
                }

                match one {
                    false => one = true,
                    true => return false,
                }
            }

            true
        }

        fn part_add_len(dirs: &Direction8Array<Option<NonZeroU32>>, dir: Direction8) -> f32 {
            // Arrow shape with `dir` being the middle
            if dirs
                .get(dir.rotated_clockwise_by(Direction8::UpRight))
                .is_some()
                && dirs
                    .get(dir.rotated_counterclockwise_by(Direction8::UpRight))
                    .is_some()
            {
                0.0
            } else if node_has_135_deg_turns(dirs) {
                WIRE_WIDTH / 2.0 * (22.5f32).to_radians().tan()
            } else if node_is_a_45_deg_turn(dirs) {
                if dir.is_diagonal() {
                    WIRE_WIDTH / 2.0 * -(22.5f32).to_radians().tan()
                } else {
                    WIRE_WIDTH / 2.0 * (22.5f32).to_radians().tan()
                }
            } else {
                WIRE_WIDTH / 2.0
            }
        }

        let mut part_buffer = self.wire_part_buffer.lock();
        let editor = self.editor.read();

        part_buffer.clear();

        let tl = [ctx.tile_bounds_tl.x - 1, ctx.tile_bounds_tl.y].into();
        let size = [ctx.tile_bounds_size.x + 2, ctx.tile_bounds_size.y + 1].into();

        for (pos, lookaround, node) in editor.wires.iter_area_with_lookaround(tl, size) {
            for dir in Direction4Half::ALL {
                let dist = node.directions.get(dir.into());
                let Some(dist) = dist else {
                    continue;
                };

                let (wire, target_node) = match &node.wire {
                    Some(wire) => {
                        let rel = Direction8::from_half(dir, false).into_dir_isize()
                            * dist.get() as isize;
                        let target = lookaround.get_relative(rel);
                        (wire, target)
                    }
                    None => {
                        match dir {
                            Direction4Half::Left if pos.x > ctx.tile_bounds_br.x => (),
                            Direction4Half::UpLeft
                                if pos.y > ctx.tile_bounds_br.y || pos.x > ctx.tile_bounds_br.x => {
                            }
                            Direction4Half::Up if pos.y > ctx.tile_bounds_br.y => {}
                            Direction4Half::UpRight
                                if pos.y > ctx.tile_bounds_br.y || pos.x < ctx.tile_bounds_tl.x => {
                            }
                            _ => continue,
                        }

                        let dir = Direction8::from_half(dir, true);
                        let dist = node.directions.get(dir);
                        let Some(dist) = dist else {
                            continue;
                        };
                        let rel = dir.into_dir_isize() * dist.get() as isize;
                        let target = lookaround.get_relative(rel);
                        let wire = target.and_then(|n| n.wire.as_ref());
                        let Some(wire) = wire else {
                            continue;
                        };
                        (wire, target)
                    }
                };

                let len = dist.get() as f32;

                let full_dir = Direction8::from_half(dir, false);

                let angle = full_dir.into_angle_xp_cw();

                let this_add_len = part_add_len(&node.directions, full_dir);
                let that_add_len = target_node
                    .map(|n| part_add_len(&n.directions, full_dir.inverted()))
                    .unwrap_or_default();

                let len = if dir.is_diagonal() { len * SQRT_2 } else { len };
                let len = len + that_add_len;
                let posf = ctx.screen.world_to_screen(pos.convert(|v| v as f32 + 0.5));

                let pos1 =
                    posf + Vec2f::from_angle_length(angle + PI, this_add_len * ctx.screen.scale);

                let pos2 = posf + Vec2f::from_angle_length(angle, len * ctx.screen.scale);

                let mut hasher = DefaultHasher::new();
                hasher.write_usize(178543947551947561);
                hasher.write_usize(wire.id);
                let v = hasher.finish();
                let r = ((v >> 16) & 0xff) as u8;
                let g = ((v >> 8) & 0xff) as u8;
                let b = (v & 0xff) as u8;

                let color = Color32::from_rgb(r, g, b);

                part_buffer.add_quad_line(
                    pos1,
                    pos2,
                    (WIRE_WIDTH * ctx.screen.scale).max(1.0),
                    color.to_normalized_gamma_f32(),
                );
            }
        }
    }

    fn draw_wires(&self, ctx: &PaintContext, vertexes: Arc<VertexRenderer>) {
        let part_buffer = self.wire_part_buffer.clone();
        ctx.draw_vertexes(vertexes, move |ctx| {
            ctx.vertexes.draw(
                ctx.gl,
                ctx.full_screen_size.into(),
                part_buffer.lock().deref(),
            );
        });
    }

    fn draw_wire_debug(&self, ctx: &PaintContext) {
        let editor = self.editor.read();
        for (pos, lookaround, node) in editor
            .wires
            .iter_area_with_lookaround(ctx.tile_bounds_tl, ctx.tile_bounds_size)
        {
            let screen_pos = ctx.screen.world_to_screen_tile(pos);

            let wire_points = node.wire.as_ref().map(|w| w.points.read());
            let backend_point = wire_points.as_ref().and_then(|p| p.get(&pos));

            for dir in Direction8::ALL {
                let dist = *node.directions.get(dir);

                let half = dir.into_half();
                let half = half.1.not().then_some(half.0);
                let backend_point_connected = half
                    .zip(backend_point)
                    .map(|(dir, point)| *point.directions.get(dir));

                let text = match dist {
                    Some(d) => format!("{d}"),
                    None if backend_point_connected.is_some_and(|p| p) => "?".into(),
                    _ => continue,
                };

                let text = WidgetText::from(text);
                let galley =
                    text.into_galley(ctx.ui, Some(false), f32::INFINITY, TextStyle::Monospace);

                let align = Vector2::<Align>::from(dir.into_align2().0);
                let align = align.convert(|v| v.to_factor());

                let pos = screen_pos + (Vec2f::from(ctx.screen.scale) - galley.size()) * align;

                let mut rgb = [60u8; 3];

                if let Some(dist) = dist {
                    rgb[0] = 255;
                    let target_node =
                        lookaround.get_relative(dir.into_dir_isize() * dist.get() as isize);
                    let correct_target = target_node.is_some_and(|n| n.wire.is_some());

                    if correct_target {
                        rgb[1] = 255;
                    }
                }
                if !backend_point_connected.is_some_and(|p| !p) {
                    rgb[2] = 255;
                }

                let color = Color32::from_rgb(rgb[0], rgb[1], rgb[2]);

                ctx.painter.rect_filled(
                    Rect::from_min_size(pos.into(), galley.size()).expand(1.0),
                    Rounding::ZERO,
                    Color32::BLACK.gamma_multiply(0.4),
                );

                ctx.painter.add(TextShape {
                    pos: pos.into(),
                    galley,
                    underline: Stroke::NONE,
                    fallback_color: color,
                    override_text_color: None,
                    opacity_factor: 0.9,
                    angle: 0.0,
                });
            }

            if node.wire.is_some() || backend_point.is_some() != node.wire.is_some() {
                let text = match &node.wire {
                    Some(w) => format!("{}", w.id),
                    None => "?".into(),
                };
                let text = WidgetText::from(text);
                let galley =
                    text.into_galley(ctx.ui, Some(false), f32::INFINITY, TextStyle::Monospace);

                let pos = screen_pos + (ctx.screen.scale / 2.0) - (galley.size() / 2.0);

                let mut rgb = [60u8; 3];

                if let Some(wire) = &node.wire {
                    rgb[0] = 255;
                    let correct_wire = editor
                        .board
                        .wires
                        .read()
                        .get(wire.id)
                        .is_some_and(|board_wire| Arc::ptr_eq(wire, board_wire));
                    if correct_wire {
                        rgb[1] = 255;
                    }
                }
                if backend_point.is_some() {
                    rgb[2] = 255;
                }

                let color = Color32::from_rgb(rgb[0], rgb[1], rgb[2]);

                ctx.painter.rect_filled(
                    Rect::from_min_size(pos.into(), galley.size()).expand(1.0),
                    Rounding::ZERO,
                    Color32::BLACK.gamma_multiply(0.4),
                );

                ctx.painter.add(TextShape {
                    pos: pos.into(),
                    galley,
                    underline: Stroke::NONE,
                    fallback_color: color,
                    override_text_color: None,
                    opacity_factor: 0.9,
                    angle: 0.0,
                });
            }
        }
    }

    fn handle_wire_interactions(&mut self, ctx: &PaintContext, interaction: &Response) {
        let world_mouse = ctx
            .ui
            .input(|input| input.pointer.latest_pos())
            .map(|p| ctx.screen.screen_to_world(p));

        let world_mouse_tile = world_mouse.map(|v| v.convert(|v| v.floor() as isize));

        if interaction.hovered()
            && ctx
                .ui
                .input(|input| input.pointer.button_pressed(PointerButton::Primary))
        {
            self.wire_draw_start = world_mouse_tile;
        }

        if let Some((start, end)) = self.wire_draw_start.zip(world_mouse_tile) {
            let startf = start.convert(|v| v as f32 + 0.5);
            let endf = end.convert(|v| v as f32 + 0.5);
            let diff = endf - startf;

            // Clockwise from +Y
            let angle = diff.angle_to_xp() + (TAU / 4.0);

            let segment = (angle / (TAU / 8.0)).round();
            let segment_index = ((segment + 8.0) % 8.0) as usize;
            let direction = Direction8::from_index(segment_index);

            let length = diff.length();

            let length = if direction.is_diagonal() {
                length / std::f32::consts::SQRT_2
            } else {
                length
            };
            let length = length.round() as u32;

            let end = start + direction.into_dir_isize() * length as isize;
            let endf = end.convert(|v| v as f32 + 0.5);

            let startf = ctx.screen.world_to_screen(startf);
            let endf = ctx.screen.world_to_screen(endf);

            let place = !ctx.ui.input(|input| input.modifiers.shift);
            let color = if place {
                Color32::from_gray(100)
            } else {
                Color32::from_rgb(255, 100, 100)
            };

            let add_len = if direction.is_diagonal() {
                WIRE_WIDTH / 2.0 * (22.5f32).to_radians().tan()
            } else {
                WIRE_WIDTH / 2.0
            };

            let vec = direction.into_dir_f32() * add_len * ctx.screen.scale;

            let startf = startf - vec;
            let endf = endf + vec;

            ctx.painter.line_segment(
                [startf.into(), endf.into()],
                Stroke::new(WIRE_WIDTH * ctx.screen.scale, color),
            );

            if !ctx
                .ui
                .input(|input| input.pointer.button_down(PointerButton::Primary))
            {
                if let Some(nonzero_len) = NonZeroU32::new(length) {
                    let mut editor = self.editor.write();
                    match place {
                        true => editor.place_wire(start, direction, nonzero_len),
                        false => editor.remove_wire(start, direction, nonzero_len),
                    }
                }
                self.wire_draw_start = None;
            }
        }

        if self.wire_draw_start.is_some()
            && !ctx
                .ui
                .input(|input| input.pointer.button_down(PointerButton::Primary))
        {
            self.wire_draw_start = None;
        }

        if let Some(world_mouse_tile) = world_mouse_tile {
            if interaction.clicked_by(PointerButton::Primary) {
                self.editor.write().toggle_wire_point(world_mouse_tile);
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PanAndZoom {
    center_pos: Vec2f,
    scale: f32,
}

impl PanAndZoom {
    fn update(&mut self, ui: &Ui, rect: Rect, drag: bool, interaction: &Response) {
        let zoom = ui.input(|input| {
            input
                .multi_touch()
                .map(|mt| mt.zoom_delta)
                .unwrap_or_else(|| {
                    if interaction.hovered() {
                        let v = input.smooth_scroll_delta.y / 240.0;
                        if v < 0.0 {
                            1.0 / (-v + 1.0)
                        } else if v > 0.0 {
                            v + 1.0
                        } else {
                            1.0
                        }
                    } else {
                        1.0
                    }
                })
        });

        if drag {
            self.center_pos -= interaction.drag_delta() / self.scale;
        }

        if zoom != 1.0 {
            let pointer_screen = Vec2f::from(
                ui.input(|i| i.pointer.hover_pos())
                    .unwrap_or_else(|| rect.center())
                    - rect.left_top(),
            );

            let pointer_center = pointer_screen - (rect.size() / 2.0);

            let world_before = self.center_pos + pointer_center / self.scale;
            self.scale *= zoom;
            let world_after = self.center_pos + pointer_center / self.scale;
            self.center_pos -= world_after - world_before;
        }
    }
}

impl Default for PanAndZoom {
    fn default() -> Self {
        Self {
            scale: 16.0,
            center_pos: Default::default(),
        }
    }
}

impl PanAndZoom {
    pub fn new(center_pos: Vec2f, scale: f32) -> Self {
        Self { center_pos, scale }
    }

    pub fn to_screen(self, screen_rect: Rect) -> Screen {
        let tl_pos = self.center_pos - (screen_rect.size() / 2.0 / self.scale);
        Screen::new(screen_rect, tl_pos, self.scale)
    }
}
