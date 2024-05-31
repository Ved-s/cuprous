use std::{
    collections::{self, HashSet},
    hash::Hash,
    marker::PhantomData,
};

use eframe::egui::{Rect, Response, Rounding, Stroke, Ui};
use geo_nd::Vector;
use glow::{Context, HasContext};

use crate::{
    vector::Vec2f,
    vertex_renderer::{
        FullscreenQuadVertexBuffer, Shaders, SimpleVertex, TriangleBuffer, UvVertex, VertexRenderer,
    },
    CustomPaintContext, PaintContext, Screen,
};

pub struct Selection<I: SelectionImpl> {
    selection: HashSet<I::Item>,
    change: HashSet<I::Item>,
    exclude: bool,

    selection_start: Option<Vec2f>,

    _phantom: PhantomData<I>,
}

impl<I: SelectionImpl> Selection<I> {
    pub fn new() -> Self {
        Self {
            selection: HashSet::new(),
            change: HashSet::new(),
            exclude: false,
            selection_start: None,
            _phantom: PhantomData,
        }
    }

    pub fn update(
        &mut self,
        pass: &I::Pass,
        interaction: &Response,
        ui: &Ui,
        screen: Screen,
        active: bool,
    ) {
        if active
            && self.selection_start.is_none()
            && interaction.hovered()
            && ui.input(|input| input.pointer.primary_pressed())
        {
            if let Some(hover) = interaction.hover_pos() {
                self.selection_start = Some(screen.screen_to_world(hover));
                self.exclude = false;
                self.change.clear();

                if !ui.input(|input| input.modifiers.ctrl || input.modifiers.shift) {
                    self.selection.clear();
                }
            }
        }

        let Some(start) = self.selection_start else {
            return;
        };

        self.exclude = ui.input(|input| input.modifiers.ctrl);

        let interaction = active
            .then(|| {
                ui.input(|input| {
                    input
                        .pointer
                        .primary_down()
                        .then(|| input.pointer.latest_pos())
                        .flatten()
                })
            })
            .flatten();

        let Some(interaction) = interaction else {
            self.selection_start = None;

            match self.exclude {
                true => {
                    self.selection.retain(|i| !self.change.contains(i));
                    self.change.clear();
                }
                false => {
                    self.selection.extend(self.change.drain());
                }
            };

            return;
        };

        let changed = ui.input(|input| {
            input.pointer.primary_pressed() || input.pointer.delta().length_sq() > 0.01
        });
        if !changed {
            return;
        }

        let b = screen.screen_to_world(interaction);
        let rect = Rect::from_two_pos(start.into(), b.into());

        self.change.clear();
        I::include_area(pass, &mut self.change, rect);
    }

    pub fn contains(&self, item: &I::Item) -> bool {
        match (self.selection.contains(item), self.exclude) {
            (true, true) => !self.change.contains(item),
            (true, false) => true,
            (false, true) => false,
            (false, false) => self.change.contains(item),
        }
    }

    pub fn iter(&self) -> SelectionIterator<'_, I::Item> {
        SelectionIterator::new(&self.selection, &self.change, self.exclude)
    }

    pub fn draw_overlay(&self, ctx: &PaintContext) {
        let Some(a) = self.selection_start else {
            return;
        };

        let Some(b) = ctx.ui.input(|input| input.pointer.latest_pos()) else {
            return;
        };

        let a = ctx.screen.world_to_screen(a).round();
        let b = b.round();
        let rect = Rect::from_two_pos(a.into(), b);

        ctx.painter.rect(
            rect,
            Rounding::ZERO,
            ctx.style.selection_fill,
            Stroke::new(2.0, ctx.style.selection_border),
        );
    }
}

impl<I: SelectionImpl> Default for Selection<I> {
    fn default() -> Self {
        Self::new()
    }
}

pub trait SelectionImpl {
    type Item: Hash + Eq;
    type Pass;

    fn include_area(pass: &Self::Pass, items: &mut HashSet<Self::Item>, area: Rect);
}

pub struct SelectionIterator<'a, T: Hash + Eq> {
    change: &'a HashSet<T>,
    exclude: bool,

    current_iter: collections::hash_set::Iter<'a, T>,
    first_iter: bool,
}

impl<'a, T: Hash + Eq> SelectionIterator<'a, T> {
    pub fn new(selection: &'a HashSet<T>, change: &'a HashSet<T>, exclude: bool) -> Self {
        Self {
            change,
            exclude,
            current_iter: selection.iter(),
            first_iter: true,
        }
    }
}

impl<'a, T: Hash + Eq> Iterator for SelectionIterator<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.first_iter {
            return self.current_iter.next();
        }

        if self.exclude {
            loop {
                let next = self.current_iter.next()?;
                if self.change.contains(next) {
                    continue;
                }

                break Some(next);
            }
        } else {
            match self.current_iter.next() {
                Some(i) => Some(i),
                None => {
                    if self.first_iter {
                        self.first_iter = false;
                        self.current_iter = self.change.iter();
                        self.current_iter.next()
                    } else {
                        None
                    }
                }
            }
        }
    }
}

pub struct SelectionRenderer {
    border_buffer: TriangleBuffer<SimpleVertex>,
    fill_buffer: TriangleBuffer<SimpleVertex>,

    renderer_borderfill: VertexRenderer<SimpleVertex, SelectionBorderFillShader>,
    renderer_fullscreen: VertexRenderer<UvVertex, SelectionFullscreenShader>,

    framebuffer: <Context as HasContext>::Framebuffer,
    texture: <Context as HasContext>::Texture,

    last_screen_size: Option<[u32; 2]>,
}

impl SelectionRenderer {
    pub fn new(gl: &glow::Context) -> Self {
        let (framebuffer, texture);

        unsafe {
            framebuffer = gl.create_framebuffer().expect("framebuffer");
            texture = gl.create_texture().expect("texture");
        };

        Self {
            border_buffer: TriangleBuffer::new(),
            fill_buffer: TriangleBuffer::new(),
            renderer_borderfill: VertexRenderer::new(gl).expect("valid renderer"),
            renderer_fullscreen: VertexRenderer::new_with_data(gl, &FullscreenQuadVertexBuffer)
                .expect("valid renderer"),

            framebuffer,
            texture,
            last_screen_size: None,
        }
    }

    pub fn clear_draw(&mut self) {
        self.border_buffer.clear();
        self.fill_buffer.clear();
    }

    pub fn add_selection_line(&mut self, a: Vec2f, b: Vec2f, width: f32, scale: f32) {
        self.fill_buffer.add_quad_line(a, b, width, ());

        let dir = (b - a).normalize();
        let a = a - (dir * scale * 0.05);
        let b = b + (dir * scale * 0.05);
        let width = width + (scale * 0.1);

        self.border_buffer.add_quad_line(a, b, width, ());
    }

    pub fn draw(&mut self, ctx: &CustomPaintContext) {
        unsafe {
            let gl = ctx.painter.gl();

            if !self
                .last_screen_size
                .is_some_and(|s| s == ctx.paint_info.screen_size_px)
            {
                gl.bind_texture(glow::TEXTURE_2D, Some(self.texture));
                gl.tex_image_2d(
                    glow::TEXTURE_2D,
                    0,
                    glow::RG as i32,
                    ctx.paint_info.screen_size_px[0] as i32,
                    ctx.paint_info.screen_size_px[1] as i32,
                    0,
                    glow::RG,
                    glow::UNSIGNED_BYTE,
                    None,
                );
                gl.tex_parameter_i32(
                    glow::TEXTURE_2D,
                    glow::TEXTURE_MAG_FILTER,
                    glow::LINEAR as i32,
                );
                gl.tex_parameter_i32(
                    glow::TEXTURE_2D,
                    glow::TEXTURE_MIN_FILTER,
                    glow::LINEAR as i32,
                );
                gl.tex_parameter_i32(
                    glow::TEXTURE_2D,
                    glow::TEXTURE_WRAP_R,
                    glow::CLAMP_TO_BORDER as i32,
                );
                gl.tex_parameter_i32(
                    glow::TEXTURE_2D,
                    glow::TEXTURE_WRAP_S,
                    glow::CLAMP_TO_BORDER as i32,
                );
                gl.tex_parameter_i32(
                    glow::TEXTURE_2D,
                    glow::TEXTURE_WRAP_T,
                    glow::CLAMP_TO_BORDER as i32,
                );

                gl.bind_texture(glow::TEXTURE_2D, None);
            }

            gl.bind_framebuffer(glow::FRAMEBUFFER, Some(self.framebuffer));

            gl.framebuffer_texture(
                glow::FRAMEBUFFER,
                glow::COLOR_ATTACHMENT0,
                Some(self.texture),
                0,
            );

            gl.clear_color(0.0, 0.0, 0.0, 1.0);
            gl.clear(glow::COLOR_BUFFER_BIT);

            let draw_color_location =
                gl.get_uniform_location(self.renderer_borderfill.gl_program(), "drawColor");

            gl.use_program(Some(self.renderer_borderfill.gl_program()));
            gl.uniform_4_f32(draw_color_location.as_ref(), 1.0, 0.0, 0.0, 1.0);
            self.renderer_borderfill
                .draw(gl, ctx.paint_info.screen_size_px, &self.border_buffer);

            gl.use_program(Some(self.renderer_borderfill.gl_program()));
            gl.uniform_4_f32(draw_color_location.as_ref(), 0.0, 1.0, 0.0, 1.0);
            self.renderer_borderfill
                .draw(gl, ctx.paint_info.screen_size_px, &self.fill_buffer);


            gl.bind_framebuffer(glow::FRAMEBUFFER, None);

            gl.bind_texture(glow::TEXTURE_2D, Some(self.texture));

            gl.use_program(Some(self.renderer_fullscreen.gl_program()));

            let fill_color_location =
                gl.get_uniform_location(self.renderer_fullscreen.gl_program(), "fillColor");
            let border_color_location =
                gl.get_uniform_location(self.renderer_fullscreen.gl_program(), "borderColor");

            let fill = ctx.style.selection_fill.to_normalized_gamma_f32();
            let border = ctx.style.selection_border.to_normalized_gamma_f32();

            gl.uniform_4_f32(
                fill_color_location.as_ref(),
                fill[0],
                fill[1],
                fill[2],
                fill[3],
            );
            gl.uniform_4_f32(
                border_color_location.as_ref(),
                border[0],
                border[1],
                border[2],
                border[3],
            );

            // gl.viewport(
            //     0,
            //     0,
            //     ctx.paint_info.screen_size_px[0] as i32,
            //     ctx.paint_info.screen_size_px[1] as i32,
            // );

            self.renderer_fullscreen.draw_no_copy(
                gl,
                ctx.paint_info.screen_size_px,
                &FullscreenQuadVertexBuffer,
            );

            gl.bind_texture(glow::TEXTURE_2D, None);
        }
    }
}

pub struct SelectionBorderFillShader;

impl Shaders<SimpleVertex> for SelectionBorderFillShader {
    const VERT_SHADER: &'static str = r#"#version 330 core
        layout (location = 0) in vec2 pos;

        uniform vec2 screenSize;

        out vec4 color;
        void main() {

            vec2 norm = pos / screenSize;
            vec2 clip = norm * 2 - 1;

            gl_Position = vec4(clip.x, -clip.y, 0.0, 1.0);
        }
    "#;

    const FRAG_SHADER: &'static str = r#"#version 330 core
        uniform vec4 drawColor;

        void main() {
            gl_FragColor = drawColor;
        }
    "#;
}

pub struct SelectionFullscreenShader;

impl Shaders<UvVertex> for SelectionFullscreenShader {
    const VERT_SHADER: &'static str = r#"#version 330 core
        layout (location = 0) in vec2 pos;
        layout (location = 1) in vec2 uv_in;

        out vec2 uv;
        void main() {
            gl_Position = vec4(pos.x, pos.y, 0.0, 1.0);
            uv = uv_in;
        }
    "#;

    const FRAG_SHADER: &'static str = r#"#version 330 core
        uniform sampler2D sampler;
        uniform vec4 fillColor;
        uniform vec4 borderColor;

        in vec2 uv;

        void main() {

            vec2 rg = texture(sampler, uv).rg;
            vec4 col = rg.r * borderColor + rg.g * fillColor;
            gl_FragColor = col;

            // vec2 fw = fwidth(uv);
            // float mod = texture(sampler, uv).r;
            // // float sum = 
            // //     texture(sampler, uv + vec2(0, fw.y)).r +
            // //     texture(sampler, uv + vec2(fw.x, fw.y)).r +
            // //     texture(sampler, uv + vec2(fw.x, 0)).r +
            // //     texture(sampler, uv + vec2(fw.x, -fw.y)).r +
            // //     texture(sampler, uv + vec2(0, -fw.y)).r +
            // //     texture(sampler, uv + vec2(-fw.x, -fw.y)).r +
            // //     texture(sampler, uv + vec2(-fw.x, 0)).r +
            // //     texture(sampler, uv + vec2(-fw.x, fw.y)).r +
            // //     texture(sampler, uv + 2 * vec2(0, fw.y)).r +
            // //     texture(sampler, uv + 2 * vec2(fw.x, fw.y)).r +
            // //     texture(sampler, uv + 2 * vec2(fw.x, 0)).r +
            // //     texture(sampler, uv + 2 * vec2(fw.x, -fw.y)).r +
            // //     texture(sampler, uv + 2 * vec2(0, -fw.y)).r +
            // //     texture(sampler, uv + 2 * vec2(-fw.x, -fw.y)).r +
            // //     texture(sampler, uv + 2 * vec2(-fw.x, 0)).r +
            // //     texture(sampler, uv + 2 * vec2(-fw.x, fw.y)).r;

            // float val = 1;
            // for (int x = -border; x <= border; x++) {
            //     for (int y = -border; y <= border; y++) {
            //         float r = texture(sampler, uv + vec2(fw.x * x, fw.y * y)).r;
            //         val = min(val, r);
            //     }
            // }

            // gl_FragColor = mix(borderColor, fillColor, val) * mod;
        }
    "#;
}
