use std::{f32::consts::TAU, num::NonZeroU32, sync::Arc};

use eframe::{
    egui::{
        remap_clamp, Align, Color32, Key, PaintCallback, PaintCallbackInfo,
        PointerButton, Rect, Response, Rounding, Sense, Stroke, TextStyle, Ui, WidgetText,
    },
    egui_glow,
    epaint::TextShape,
};
use parking_lot::Mutex;

use crate::{
    app::App,
    board::BoardEditor,
    vector::{Vec2f, Vec2isize, Vector2},
    vertex_renderer::VertexRenderer,
    Direction8, PaintContext, Screen, CHUNK_SIZE,
};

use super::{TabCreation, TabImpl};

pub struct BoardView {
    pan_zoom: PanAndZoom,
    vertexes: Arc<Mutex<Option<VertexRenderer>>>,

    wire_draw_start: Option<Vec2isize>,

    editor: BoardEditor,
    fixed_screen_pos: Option<(Rect, PanAndZoom)>,
}

impl TabCreation for BoardView {
    fn new() -> Self {
        Self {
            pan_zoom: Default::default(),
            vertexes: Arc::new(Mutex::new(None)),
            wire_draw_start: None,

            editor: Default::default(),
            fixed_screen_pos: None,
        }
    }
}

impl TabImpl for BoardView {
    fn update(&mut self, _app: &mut App, ui: &mut Ui) {
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

        let ctx = PaintContext::new(ui, screen);

        self.draw_grid(&ctx);
        self.draw_wire_debug(&ctx);
        self.handle_wire_drawing(&ctx, &interaction);
    }
}

impl BoardView {
    fn draw_grid_layer(vertexes: &mut VertexRenderer, screen: Screen, chunks: bool) {
        let (scale_mul, fade_min, fade_max) = if chunks {
            (
                CHUNK_SIZE as f32,
                CHUNK_SIZE as f32 / 8.0,
                CHUNK_SIZE as f32 / 2.0,
            )
        } else {
            (1.0, CHUNK_SIZE as f32 / 4.0, CHUNK_SIZE as f32)
        };

        let scale = screen.scale * scale_mul;

        if screen.scale > fade_min {
            let alpha = remap_clamp(screen.scale, fade_min..=fade_max, 0.0..=1.0);

            let offx = (1.0 - ((screen.world_pos.x / scale_mul).fract() + 1.0).fract()) * scale;
            let offy = (1.0 - ((screen.world_pos.y / scale_mul).fract() + 1.0).fract()) * scale;

            let countx = (screen.screen_rect.width() / scale).ceil() as usize;
            let county = (screen.screen_rect.height() / scale).ceil() as usize;

            let white = if chunks { 0.2 } else { 0.15 };

            for i in 0..countx {
                let x = offx + i as f32 * scale + screen.screen_rect.left();
                let wx = i as isize + (screen.world_pos.x / scale_mul).floor() as isize + 1;

                if (wx % CHUNK_SIZE as isize) == 0 && !chunks || wx == 0 {
                    continue;
                }

                vertexes.add_singlecolor_line(
                    [x, screen.screen_rect.top()],
                    [x, screen.screen_rect.bottom()],
                    [white * alpha, white * alpha, white * alpha, alpha],
                );
            }

            for i in 0..county {
                let y = offy + i as f32 * scale + screen.screen_rect.top();
                let wy = i as isize + (screen.world_pos.y / scale_mul).floor() as isize + 1;

                if (wy % CHUNK_SIZE as isize) == 0 && !chunks || wy == 0 {
                    continue;
                }

                vertexes.add_singlecolor_line(
                    [screen.screen_rect.left(), y],
                    [screen.screen_rect.right(), y],
                    [white * alpha, white * alpha, white * alpha, alpha],
                );
            }
        }

        let zero = screen.world_to_screen(0.0);

        if screen.screen_rect.left() <= zero.x && zero.x < screen.screen_rect.right() {
            vertexes.add_singlecolor_line(
                [zero.x, screen.screen_rect.top()],
                [zero.x, screen.screen_rect.bottom()],
                [0.2, 1.0, 0.2, 1.0],
            );
        }

        if screen.screen_rect.top() <= zero.y && zero.y < screen.screen_rect.bottom() {
            vertexes.add_singlecolor_line(
                [screen.screen_rect.left(), zero.y],
                [screen.screen_rect.right(), zero.y],
                [1.0, 0.2, 0.2, 1.0],
            );
        }
    }

    fn draw_grid_cross(vertexes: &mut VertexRenderer, screen: Screen) {
        let cross_pos = screen.world_to_screen(0.0);
        let cross_pos = screen.screen_rect.clamp(cross_pos.into());

        vertexes.add_singlecolor_line(
            [cross_pos.x - screen.scale, cross_pos.y],
            [cross_pos.x + screen.scale, cross_pos.y],
            [1.0; 4],
        );

        vertexes.add_singlecolor_line(
            [cross_pos.x, cross_pos.y - screen.scale],
            [cross_pos.x, cross_pos.y + screen.scale],
            [1.0; 4],
        );
    }

    fn draw_grid(&mut self, ctx: &PaintContext) {
        let vertexes = self.vertexes.clone();
        let screen = ctx.screen;
        let callback = move |pi: PaintCallbackInfo, p: &egui_glow::Painter| {
            use glow::HasContext;

            unsafe {
                p.gl().viewport(
                    0,
                    0,
                    pi.screen_size_px[0] as i32,
                    pi.screen_size_px[1] as i32,
                );
            };

            let mut vertexes = vertexes.lock();
            let vertexes = vertexes.get_or_insert_with(|| {
                VertexRenderer::new(p.gl()).expect("running in OpenGL context")
            });

            vertexes.clear();
            Self::draw_grid_layer(vertexes, screen, false);
            Self::draw_grid_layer(vertexes, screen, true);
            Self::draw_grid_cross(vertexes, screen);
            let screen_size = [pi.screen_size_px[0] as f32, pi.screen_size_px[1] as f32];
            vertexes.draw_lines(p.gl(), screen_size, 1.0);
        };
        ctx.painter.add(PaintCallback {
            rect: screen.screen_rect,
            callback: Arc::new(egui_glow::CallbackFn::new(callback)),
        });
    }

    fn draw_wire_debug(&mut self, ctx: &PaintContext) {
        for (pos, lookaround, node) in self
            .editor
            .wires
            .iter_area_with_lookaround(ctx.tile_bounds_tl, ctx.tile_bounds_size)
        {
            let screen_pos = ctx.screen.world_to_screen_tile(pos);
            for dir in Direction8::ALL {
                let dist = *node.directions.get(dir);
                let Some(dist) = dist else {
                    continue;
                };

                let text = format!("{}", dist.get());
                let text = WidgetText::from(text);
                let galley =
                    text.into_galley(ctx.ui, Some(false), f32::INFINITY, TextStyle::Monospace);

                let align = Vector2::<Align>::from(dir.into_align2().0);
                let align = align.convert(|v| v.to_factor());

                let pos = screen_pos + (Vec2f::from(ctx.screen.scale) - galley.size()) * align;

                let target_node =
                    lookaround.get_relative(dir.into_dir_isize() * dist.get() as isize);
                let correct_target = target_node.is_some_and(|n| n.wire.is_some());

                let color = if correct_target {
                    Color32::WHITE
                } else {
                    Color32::from_rgb(255, 60, 60)
                };

                ctx.painter.add(TextShape {
                    pos: pos.into(),
                    galley,
                    underline: Stroke::NONE,
                    fallback_color: color,
                    override_text_color: None,
                    opacity_factor: 1.0,
                    angle: 0.0,
                });
            }

            if let Some(wire) = &node.wire {
                let text = format!("{}", wire.id);
                let text = WidgetText::from(text);
                let galley =
                    text.into_galley(ctx.ui, Some(false), f32::INFINITY, TextStyle::Monospace);

                let pos = screen_pos + (ctx.screen.scale / 2.0) - (galley.size() / 2.0);

                let correct_wire = self
                    .editor
                    .board
                    .wires
                    .get(wire.id)
                    .is_some_and(|board_wire| Arc::ptr_eq(wire, board_wire));

                let color = if correct_wire {
                    Color32::from_rgb(60, 255, 60)
                } else {
                    Color32::from_rgb(255, 60, 60)
                };

                ctx.painter.add(TextShape {
                    pos: pos.into(),
                    galley,
                    underline: Stroke::NONE,
                    fallback_color: color,
                    override_text_color: None,
                    opacity_factor: 1.0,
                    angle: 0.0,
                });
            }
        }
    }

    fn handle_wire_drawing(&mut self, ctx: &PaintContext, interaction: &Response) {
        let world_mouse = ctx
            .ui
            .input(|input| input.pointer.latest_pos())
            .map(|p| ctx.screen.screen_to_world(p));

        let world_mouse_tile = world_mouse.map(|v| v.convert(|v| v.floor() as isize));

        if let Some(world_mouse_tile) = world_mouse_tile {
            let pos = ctx.screen.world_to_screen_tile(world_mouse_tile);
            let size = Vec2f::from(ctx.screen.scale);
            let rect = Rect::from_min_size(pos.into(), size.into());

            ctx.painter
                .rect_filled(rect, Rounding::ZERO, Color32::YELLOW.gamma_multiply(0.1));
        }

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
            let angle = (TAU - diff.angle_to_x()) + (TAU / 4.0);

            let segment = (angle / (TAU / 8.0)).round();
            let segment_index = ((segment + 8.0) % 8.0) as usize;
            let direction = Direction8::from_index(segment_index);

            let length = diff.length();

            let length = if segment_index % 2 == 1 {
                length / std::f32::consts::SQRT_2
            } else {
                length
            };
            let length = length.round() as u32;

            let end = start + direction.into_dir_isize() * length as isize;
            let endf = end.convert(|v| v as f32 + 0.5);

            let startf = ctx.screen.world_to_screen(startf);
            let endf = ctx.screen.world_to_screen(endf);

            ctx.painter.line_segment(
                [startf.into(), endf.into()],
                Stroke::new(0.2 * ctx.screen.scale, Color32::from_gray(100)),
            );

            if !ctx
                .ui
                .input(|input| input.pointer.button_down(PointerButton::Primary))
            {
                if let Some(nonzero_len) = NonZeroU32::new(length) {
                    self.editor.place_wire(start, direction, nonzero_len);
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
