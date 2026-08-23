use eframe::{
    egui::{
        self, Color32, Event, Mesh, Pos2, Rect, TextStyle,
        WidgetText,
    },
    epaint::{TextShape, Vertex},
};

use crate::{
    Screen,
    vector::{Vec2f, Vec2isize},
};

// todo: configurable
const CURSOR_HEIGHT: usize = 20;

const CURSOR_WIDTH: usize = {
    const_int_sqrt_slow(
        (CURSOR_HEIGHT * CURSOR_HEIGHT) / 2,
        CURSOR_HEIGHT / 2,
        CURSOR_HEIGHT,
    )
};

const CURSOR_VERTEX_OFFSETS: &[(f32, f32)] = &[
    (0.0, CURSOR_HEIGHT as f32),
    (0.0, 0.0),
    ((CURSOR_WIDTH / 2) as f32, CURSOR_WIDTH as f32),
    (CURSOR_WIDTH as f32, CURSOR_WIDTH as f32),
];

const _: () = assert!(CURSOR_VERTEX_OFFSETS.len() > 2);

#[derive(Default, Clone, Copy)]
pub struct Multicursor {
    extra_cursors: usize,
    offset: Vec2isize,

    editing_cursor_pos: Option<Vec2f>,
    pub updated: bool,
}

impl Multicursor {
    pub fn has_extra_cursors(&self) -> bool {
        self.extra_cursors > 0 && (self.offset.x != 0 || self.offset.y != 0)
    }

    pub fn editing(&self) -> bool {
        self.editing_cursor_pos.is_some()
    }

    pub fn cursors_screen(&self, main_pos: Vec2f, scale: f32) -> impl Iterator<Item = Vec2f> {
        let cursors = if self.has_extra_cursors() {
            self.extra_cursors
        } else {
            0
        } + 1;
        let scaled_offset = self.offset.convert(|v| v as f32) * scale;
        std::iter::successors(Some(main_pos), move |&p| Some(p + scaled_offset)).take(cursors)
    }

    pub fn cursors_world(&self, main_pos: Vec2isize) -> impl Iterator<Item = Vec2isize> {
        let cursors = if self.has_extra_cursors() {
            self.extra_cursors
        } else {
            0
        } + 1;
        let offset = self.offset;
        std::iter::successors(Some(main_pos), move |&p| Some(p + offset)).take(cursors)
    }

    pub fn update(&mut self, ctx: &egui::Context, editing: bool, screen: Screen) {
        self.updated = true;

        let Some(pointer) = ctx.pointer_hover_pos() else {
            return;
        };

        if editing && self.editing_cursor_pos.is_none() {
            self.editing_cursor_pos = Some(
                (Vec2f::from(pointer) - self.offset.convert(|v| v as f32) * screen.scale).round(),
            );
        }

        if !editing && self.editing_cursor_pos.is_some() {
            self.editing_cursor_pos = None;
            if self.extra_cursors == 0 {
                self.offset = Default::default();
            }
        }

        if let Some(editing_anchor) = self.editing_cursor_pos {
            self.offset = ((Vec2f::from(pointer) - editing_anchor) / screen.scale)
                .convert(|v| v.round() as isize);

            ctx.input(|input| {
                for e in &input.events {
                    let Event::MouseWheel { delta, .. } = e else {
                        continue;
                    };

                    self.extra_cursors =
                        (self.extra_cursors as isize + delta.y as isize).max(0) as usize;
                }
            });
        }
    }

    pub fn ui(&self, ui: &mut egui::Ui, screen: Screen) {
        if !self.has_extra_cursors() && self.editing_cursor_pos.is_none() {
            return;
        }

        let Some(pointer) = ui.pointer_hover_pos() else {
            return;
        };

        let mut mesh = Mesh::default();

        // TODO: configurable
        let cursor_color_idle = Color32::LIGHT_GRAY.gamma_multiply(0.6);
        let cursor_color_anchor = Color32::DARK_GRAY.gamma_multiply(0.6);

        let pointer_chain_start = self.editing_cursor_pos.unwrap_or_else(|| pointer.into());

        for (i, c) in self
            .cursors_screen(pointer_chain_start, screen.scale)
            .enumerate()
        {
            let color = match (i, self.editing_cursor_pos.is_some()) {
                (0, false) => continue,
                (0, true) => cursor_color_anchor,
                (1, true) => continue,
                _ => cursor_color_idle,
            };

            let c = c.round();

            for (i, pos) in CURSOR_VERTEX_OFFSETS.iter().enumerate() {
                let pos = c + Vec2f::from(*pos);
                mesh.vertices.push(Vertex {
                    pos: pos.into(),
                    uv: Pos2::ZERO,
                    color,
                });
                if i >= 2 {
                    let index = mesh.vertices.len() as u32;
                    mesh.indices.push(index - 1);
                    mesh.indices.push(index - 2);
                    mesh.indices.push(index - 3);
                }
            }
        }

        ui.painter().add(mesh);

        let text = format!("x{}", self.extra_cursors + 1);
        let galley = WidgetText::from(text).into_galley(
            ui,
            None,
            f32::INFINITY,
            TextStyle::Monospace.resolve(ui.style()),
        );

        let text_pos = Vec2f::from(pointer) + Vec2f::new(CURSOR_WIDTH as f32, CURSOR_HEIGHT as f32);
        let text_rect = Rect::from_min_size(text_pos.into(), galley.size());

        ui.painter()
            .rect_filled(text_rect, 0.0, Color32::BLACK.gamma_multiply(0.6));
        ui.painter()
            .add(TextShape::new(text_pos.into(), galley, Color32::WHITE));
    }

    pub fn update_inactive(&mut self) {
        if self.editing_cursor_pos.is_some() && self.extra_cursors == 0 {
            self.offset = Default::default();
        }
        self.editing_cursor_pos = None;
    }
}

const fn const_int_sqrt_slow(val: usize, min_bound: usize, max_bound: usize) -> usize {
    let mut i = min_bound;

    while i <= max_bound {
        if i * i > val {
            break;
        }
        i += 1;
    }

    if i > max_bound {
        return max_bound;
    }

    if i == 0 {
        return 0;
    }

    let prev = i - 1;

    let prev_pow = prev * prev;
    let now_pow = i * i;

    if prev_pow > val {
        return min_bound;
    }

    let prev_diff = val - prev_pow;
    let now_diff = now_pow - val;

    if prev_diff < now_diff { prev } else { i }
}
