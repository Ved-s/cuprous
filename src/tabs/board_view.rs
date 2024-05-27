use std::{f32::consts::TAU, sync::Arc};

use eframe::{
    egui::{
        pos2, remap_clamp, Color32, PaintCallback, PaintCallbackInfo, PointerButton, Rect, Sense,
        Stroke, Ui,
    },
    egui_glow,
};
use parking_lot::Mutex;

use crate::{
    app::App,
    board::BoardEditor,
    vector::{Vec2f, Vec2isize},
    vertex_renderer::VertexRenderer,
    Direction8, Screen,
};

use super::{TabCreation, TabImpl};

pub struct BoardView {
    pan_zoom: PanAndZoom,
    vertexes: Arc<Mutex<Option<VertexRenderer>>>,

    wire_draw_start: Option<Vec2isize>,

    editor: BoardEditor,
}

impl TabCreation for BoardView {
    fn new() -> Self {
        Self {
            pan_zoom: Default::default(),
            vertexes: Arc::new(Mutex::new(None)),
            wire_draw_start: None,

            editor: Default::default(),
        }
    }
}

impl TabImpl for BoardView {
    fn update(&mut self, _app: &mut App, ui: &mut Ui) {
        let interaction = ui.interact(
            ui.max_rect(),
            ui.id().with("board-interaction"),
            Sense::click_and_drag(),
        );
        self.pan_zoom.update(
            ui,
            ui.max_rect(),
            interaction.dragged_by(PointerButton::Secondary),
        );

        let gl_screen = self
            .pan_zoom
            .to_screen(Rect::from_min_size(pos2(0.0, 0.0), ui.max_rect().size()));

        let vertexes = self.vertexes.clone();
        let screen_size: [f32; 2] = ui.max_rect().size().into();
        let callback = move |_pi: PaintCallbackInfo, p: &egui_glow::Painter| {
            let mut vertexes = vertexes.lock();
            let vertexes = vertexes.get_or_insert_with(|| {
                VertexRenderer::new(p.gl()).expect("running in OpenGL context")
            });

            vertexes.clear();
            Self::draw_grid(vertexes, gl_screen, false);
            Self::draw_grid(vertexes, gl_screen, true);
            Self::draw_grid_cross(vertexes, gl_screen);
            vertexes.draw_lines(p.gl(), screen_size, 1.0);
        };
        ui.painter().add(PaintCallback {
            rect: ui.max_rect(),
            callback: Arc::new(egui_glow::CallbackFn::new(callback)),
        });

        let screen = self.pan_zoom.to_screen(ui.max_rect());

        let world_mouse = ui
            .input(|input| input.pointer.latest_pos())
            .map(|p| screen.screen_to_world(p));

        let world_mouse_tile = world_mouse.map(|v| v.convert(|v| v.floor() as isize));

        if ui.input(|input| input.pointer.button_pressed(PointerButton::Primary)) {
            self.wire_draw_start = world_mouse_tile;
        }
        if self.wire_draw_start.is_some()
            && ui.input(|input| !input.pointer.button_down(PointerButton::Primary))
        {
            self.wire_draw_start = None;
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

            let end = start + direction.into_dir_isize() * length.round() as isize;
            let endf = end.convert(|v| v as f32 + 0.5);

            let startf = screen.world_to_screen(startf);
            let endf = screen.world_to_screen(endf);

            ui.painter().line_segment(
                [startf.into(), endf.into()],
                Stroke::new(0.2 * screen.scale, Color32::from_gray(100)),
            );
        }
    }
}

impl BoardView {
    fn draw_grid(vertexes: &mut VertexRenderer, screen: Screen, chunks: bool) {
        let (scale_mul, fade_min, fade_max) = if chunks {
            (16.0, 2.0, 8.0)
        } else {
            (1.0, 4.0, 16.0)
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
                let x = offx + i as f32 * scale;
                let wx = i as isize + (screen.world_pos.x / scale_mul).floor() as isize + 1;

                if (wx % 16) == 0 && !chunks || wx == 0 {
                    continue;
                }

                vertexes.add_singlecolor_line(
                    [x, screen.screen_rect.top()],
                    [x, screen.screen_rect.bottom()],
                    [white * alpha, white * alpha, white * alpha, alpha],
                );
            }

            for i in 0..county {
                let y = offy + i as f32 * scale;
                let wy = i as isize + (screen.world_pos.y / scale_mul).floor() as isize + 1;

                if (wy % 16) == 0 && !chunks || wy == 0 {
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
}

#[derive(Debug, Clone, Copy)]
pub struct PanAndZoom {
    center_pos: Vec2f,
    scale: f32,
}

impl PanAndZoom {
    fn update(&mut self, ui: &Ui, rect: Rect, drag: bool) {
        let interaction = ui.interact(rect, ui.id(), Sense::click_and_drag());

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
