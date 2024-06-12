use std::{
    f32::consts::TAU,
    marker::PhantomData,
    sync::OnceLock,
};

use glow::{Buffer, Context, HasContext, Program, UniformLocation, VertexArray};
use parking_lot::{Mutex, MutexGuard};

use crate::vector::Vec2f;

#[derive(Debug, thiserror::Error)]
pub enum RendererInitError {
    #[error("glGenBuffers: {0}")]
    BufferCreation(String),

    #[error("glGenVertexArrays: {0}")]
    VertexArrayCreation(String),

    #[error("glCreateShader: {0}")]
    ShaderCreation(String),

    #[error("glCreateProgram: {0}")]
    ProgramCreation(String),

    #[error("Vertex shader compilation: {0}")]
    VertexShaderCompilation(String),

    #[error("Fragment shader compilation: {0}")]
    FragmentShaderCompilation(String),

    #[error("Shader program linking: {0}")]
    ShaderProgramLinking(String),
}

pub type ColoredVertexRenderer = VertexRenderer<ColoredVertex, ColoredVertexDefaultShader>;
pub type ColoredLineBuffer = LineBuffer<ColoredVertex>;
pub type ColoredTriangleBuffer = TriangleBuffer<ColoredVertex>;

pub struct VertexRenderer<V: Vertex, S: Shaders<V>> {
    buffer: Buffer,
    vertex_array: VertexArray,
    program: Program,
    screen_size_location: Option<UniformLocation>,
    _phantom: PhantomData<(V, S)>,
}

#[allow(dead_code)]
impl<V: Vertex, S: Shaders<V>> VertexRenderer<V, S> {
    pub fn new(gl: &Context) -> Result<Self, RendererInitError> {
        Self::new_inner(gl, None)
    }
    /// For drawing static vertexes with `Self::draw_no_copy`
    pub fn new_with_data<B: VertexBuffer<V>>(
        gl: &Context,
        buffer: &B,
    ) -> Result<Self, RendererInitError> {
        Self::new_inner(gl, Some(vertex_slice_to_bytes(buffer.get_vertex_slice())))
    }
    fn new_inner(gl: &Context, data: Option<&[u8]>) -> Result<Self, RendererInitError> {
        unsafe {
            let vert_shader = gl
                .create_shader(glow::VERTEX_SHADER)
                .map_err(RendererInitError::ShaderCreation)?;

            gl.shader_source(vert_shader, S::VERT_SHADER);
            gl.compile_shader(vert_shader);

            if !gl.get_shader_compile_status(vert_shader) {
                let log = gl.get_shader_info_log(vert_shader);
                return Err(RendererInitError::VertexShaderCompilation(log));
            }

            let frag_shader = gl
                .create_shader(glow::FRAGMENT_SHADER)
                .map_err(RendererInitError::ShaderCreation)?;

            gl.shader_source(frag_shader, S::FRAG_SHADER);
            gl.compile_shader(frag_shader);

            if !gl.get_shader_compile_status(frag_shader) {
                let log = gl.get_shader_info_log(frag_shader);
                return Err(RendererInitError::FragmentShaderCompilation(log));
            }

            let program = gl
                .create_program()
                .map_err(RendererInitError::ProgramCreation)?;

            gl.attach_shader(program, vert_shader);
            gl.attach_shader(program, frag_shader);
            gl.link_program(program);

            if !gl.get_program_link_status(program) {
                let log = gl.get_program_info_log(program);
                return Err(RendererInitError::ShaderProgramLinking(log));
            }

            let screen_size_location = gl.get_uniform_location(program, "screenSize");

            gl.delete_shader(vert_shader);
            gl.delete_shader(frag_shader);

            let buffer = gl
                .create_buffer()
                .map_err(RendererInitError::BufferCreation)?;

            let vertex_array = gl
                .create_vertex_array()
                .map_err(RendererInitError::VertexArrayCreation)?;

            gl.bind_buffer(glow::ARRAY_BUFFER, Some(buffer));

            if let Some(data) = data {
                gl.buffer_data_u8_slice(glow::ARRAY_BUFFER, data, glow::STATIC_DRAW);
            }

            gl.bind_vertex_array(Some(vertex_array));

            V::init_vertex_buffer(gl);

            gl.bind_buffer(glow::ARRAY_BUFFER, None);
            gl.bind_vertex_array(None);

            Ok(Self {
                buffer,
                vertex_array,
                program,
                screen_size_location,

                _phantom: PhantomData,
            })
        }
    }

    pub fn draw<B: VertexBuffer<V>>(&mut self, gl: &Context, screen_size: [u32; 2], buffer: &B) {
        let vertexes = buffer.get_vertex_slice();
        if vertexes.is_empty() {
            return;
        }

        buffer.before_draw(gl);
        self.draw_inner(
            gl,
            screen_size,
            Some(vertex_slice_to_bytes(vertexes)),
            B::DRAW_MODE,
            buffer.vertex_count() as i32,
        );
    }

    /// Drawing with existing vertex data, without copying it from the `buffer`<br>
    /// Data is initialized with `Self::new_with_data`
    pub fn draw_no_copy<B: VertexBuffer<V>>(
        &mut self,
        gl: &Context,
        screen_size: [u32; 2],
        buffer: &B,
    ) {
        buffer.before_draw(gl);
        self.draw_inner(
            gl,
            screen_size,
            None,
            B::DRAW_MODE,
            buffer.vertex_count() as i32,
        );
    }

    fn draw_inner(
        &mut self,
        gl: &Context,
        screen_size: [u32; 2],
        data: Option<&[u8]>,
        mode: u32,
        count: i32,
    ) {
        unsafe {
            gl.bind_buffer(glow::ARRAY_BUFFER, Some(self.buffer));

            if let Some(data) = data {
                gl.buffer_data_u8_slice(glow::ARRAY_BUFFER, data, glow::STREAM_DRAW);
            }

            gl.bind_vertex_array(Some(self.vertex_array));
            gl.use_program(Some(self.program));
            gl.uniform_2_f32(
                self.screen_size_location.as_ref(),
                screen_size[0] as f32,
                screen_size[1] as f32,
            );

            gl.draw_arrays(mode, 0, count);

            gl.use_program(None);
            gl.bind_buffer(glow::ARRAY_BUFFER, None);
            gl.bind_vertex_array(None);
        }
    }

    pub fn gl_buffer(&mut self) -> <Context as HasContext>::Buffer {
        self.buffer
    }

    pub fn gl_vertex_array(&mut self) -> <Context as HasContext>::VertexArray {
        self.vertex_array
    }

    pub fn gl_program(&mut self) -> <Context as HasContext>::Program {
        self.program
    }
}

static GLOBAL_COLORED_VERTEX_RENDERER: OnceLock<Mutex<ColoredVertexRenderer>> = OnceLock::new();

impl ColoredVertexRenderer {
    pub fn global(gl: &glow::Context) -> MutexGuard<'static, ColoredVertexRenderer> {
        let mutex = GLOBAL_COLORED_VERTEX_RENDERER.get_or_init(|| {
            Mutex::new(ColoredVertexRenderer::new(gl).expect("ColoredVertexRenderer init"))
        });
        match mutex.try_lock() {
            Some(g) => g,
            None => panic!("requested global ColoredVertexRenderer multiple times"),
        }
    }
}

pub trait VertexBuffer<V: Vertex> {
    const DRAW_MODE: u32;

    fn get_vertex_slice(&self) -> &[V];
    fn vertex_count(&self) -> usize;

    fn before_draw(&self, gl: &glow::Context) {
        let _ = gl;
    }
}

pub trait Vertex: Sized + Copy + Default {
    fn init_vertex_buffer(gl: &glow::Context);
}

pub trait PositionedVertex: Vertex {
    type ExtraData: Copy;

    fn new(pos: Vec2f, extra: Self::ExtraData) -> Self;
    fn into_parts(self) -> (Vec2f, Self::ExtraData);
}

pub trait Shaders<V: Vertex> {
    const VERT_SHADER: &'static str;
    const FRAG_SHADER: &'static str;
}

#[derive(Default)]
pub struct TriangleBuffer<V: Vertex>(Vec<V>);

impl<V: Vertex> TriangleBuffer<V> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn push_quad(&mut self, quad: Quad<V>) {
        let [a, b] = quad.into();
        self.push_triangle(a);
        self.push_triangle(b);
    }

    pub fn push_triangle(&mut self, tri: Triangle<V>) {
        self.0.extend(tri.0);
    }

    pub fn add_new_triangle(
        &mut self,
        a: impl Into<Vec2f>,
        b: impl Into<Vec2f>,
        c: impl Into<Vec2f>,
        extra: impl Into<V::ExtraData>,
    ) where
        V: PositionedVertex,
    {
        self.push_triangle(Triangle::new(a.into(), b.into(), c.into(), extra.into()));
    }

    pub fn add_new_quad(
        &mut self,
        tl: impl Into<Vec2f>,
        tr: impl Into<Vec2f>,
        bl: impl Into<Vec2f>,
        br: impl Into<Vec2f>,
        extra: impl Into<V::ExtraData>,
    ) where
        V: PositionedVertex,
    {
        self.push_quad(Quad::new(
            tl.into(),
            tr.into(),
            bl.into(),
            br.into(),
            extra.into(),
        ));
    }

    pub fn add_new_rect(
        &mut self,
        tl: impl Into<Vec2f>,
        size: impl Into<Vec2f>,
        extra: impl Into<V::ExtraData>,
    ) where
        V: PositionedVertex,
    {
        self.push_quad(Quad::new_rect(tl.into(), size.into(), extra.into()));
    }

    pub fn add_quad_line(
        &mut self,
        a: impl Into<Vec2f>,
        b: impl Into<Vec2f>,
        width: f32,
        extra: impl Into<V::ExtraData>,
    ) where
        V: PositionedVertex,
    {
        self.push_quad(Quad::new_line(a.into(), b.into(), width, extra.into()));
    }

    pub fn add_centered_rect(
        &mut self,
        pos: impl Into<Vec2f>,
        size: impl Into<Vec2f>,
        extra: impl Into<V::ExtraData>,
    ) where
        V: PositionedVertex,
    {
        let size = size.into();
        self.add_new_rect(pos.into() - size / 2.0, size, extra.into());
    }

    pub fn add_centered_rotated_rect(
        &mut self,
        pos: impl Into<Vec2f>,
        size: impl Into<Vec2f>,
        angle: f32,
        extra: impl Into<V::ExtraData>,
    ) where
        V: PositionedVertex,
    {
        let pos = pos.into();
        let size = size.into();
        let quad = Quad::new_rect(pos - size / 2.0, size, extra.into());
        self.push_quad(quad.rotated(angle, pos))
    }

    pub fn add_circle(
        &mut self,
        center: impl Into<Vec2f>,
        radius: f32,
        extra: impl Into<V::ExtraData>,
    ) where
        V: PositionedVertex,
    {
        // Mostly copied from epaint tesselator
        use crate::precomputed::circles::*;

        let points = if radius <= 2.0 {
            &CIRCLE_8[..]
        } else if radius <= 5.0 {
            &CIRCLE_16[..]
        } else if radius < 18.0 {
            &CIRCLE_32[..]
        } else if radius < 50.0 {
            &CIRCLE_64[..]
        } else {
            &CIRCLE_128[..]
        };

        let extra = extra.into();
        let center = center.into();
        self.add_filled_path(points.iter().map(|p| V::new(center + *p * radius, extra)))
    }

    pub fn add_filled_path(&mut self, mut verts: impl Iterator<Item = V>) {
        let Some(first) = verts.next() else {
            return;
        };

        let Some(mut prev) = verts.next() else {
            return;
        };

        for vertex in verts {
            let tri = Triangle([first, prev, vertex]);
            prev = vertex;

            self.push_triangle(tri);
        }
    }
}

impl<V: Vertex> VertexBuffer<V> for TriangleBuffer<V> {
    const DRAW_MODE: u32 = glow::TRIANGLES;

    fn get_vertex_slice(&self) -> &[V] {
        &self.0
    }

    fn vertex_count(&self) -> usize {
        self.0.len()
    }
}

#[derive(Default)]
pub struct LineBuffer<V: Vertex>(Vec<V>, f32);

impl<V: Vertex> LineBuffer<V> {
    pub fn new(line_width: f32) -> Self {
        Self(vec![], line_width)
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn push_line(&mut self, line: Line<V>) {
        self.0.extend(line.0);
    }

    pub fn set_line_width(&mut self, width: f32) {
        self.1 = width;
    }

    pub fn add_singlecolor_line(
        &mut self,
        a: impl Into<Vec2f>,
        b: impl Into<Vec2f>,
        extra: impl Into<V::ExtraData>,
    ) where
        V: PositionedVertex,
    {
        let extra = extra.into();
        self.push_line(Line([V::new(a.into(), extra), V::new(b.into(), extra)]));
    }
}

impl<V: Vertex> VertexBuffer<V> for LineBuffer<V> {
    const DRAW_MODE: u32 = glow::LINES;

    fn get_vertex_slice(&self) -> &[V] {
        &self.0
    }

    fn before_draw(&self, gl: &glow::Context) {
        unsafe { gl.line_width(self.1) };
    }

    fn vertex_count(&self) -> usize {
        self.0.len()
    }
}

#[derive(Clone, Copy)]
pub struct Quad<V: Vertex> {
    pub tl: V,
    pub tr: V,
    pub bl: V,
    pub br: V,
}

impl<V: PositionedVertex> Quad<V> {
    pub fn new(
        tl: impl Into<Vec2f>,
        tr: impl Into<Vec2f>,
        bl: impl Into<Vec2f>,
        br: impl Into<Vec2f>,
        extra: impl Into<V::ExtraData>,
    ) -> Self {
        let extra = extra.into();
        Self {
            tl: V::new(tl.into(), extra),
            tr: V::new(tr.into(), extra),
            bl: V::new(bl.into(), extra),
            br: V::new(br.into(), extra),
        }
    }

    pub fn new_line(a: Vec2f, b: Vec2f, width: f32, extra: impl Into<V::ExtraData>) -> Self {
        let diff = b - a;
        let angle = diff.angle_to_xp();
        let up = Vec2f::from_angle_length(angle - TAU / 4.0, width / 2.0);
        let down = Vec2f::from_angle_length(angle + TAU / 4.0, width / 2.0);

        Self::new(a + up, b + up, a + down, b + down, extra.into())
    }

    pub fn new_rect(
        tl: impl Into<Vec2f>,
        size: impl Into<Vec2f>,
        extra: impl Into<V::ExtraData>,
    ) -> Self {
        let extra = extra.into();
        let tl = tl.into();
        let size = size.into();
        Self {
            tl: V::new([tl.x, tl.y].into(), extra),
            tr: V::new([tl.x + size.x, tl.y].into(), extra),
            bl: V::new([tl.x, tl.y + size.y].into(), extra),
            br: V::new([tl.x + size.x, tl.y + size.y].into(), extra),
        }
    }

    fn rotated(&self, angle: f32, origin: Vec2f) -> Self {
        let tl = self.tl.into_parts();
        let tr = self.tr.into_parts();
        let bl = self.bl.into_parts();
        let br = self.br.into_parts();

        Self {
            tl: V::new(tl.0.rotated(angle, origin), tl.1),
            tr: V::new(tr.0.rotated(angle, origin), tr.1),
            bl: V::new(bl.0.rotated(angle, origin), bl.1),
            br: V::new(br.0.rotated(angle, origin), br.1),
        }
    }
}

impl<V: Vertex> From<Quad<V>> for [Triangle<V>; 2] {
    fn from(val: Quad<V>) -> Self {
        [
            Triangle([val.tl, val.tr, val.bl]),
            Triangle([val.tr, val.bl, val.br]),
        ]
    }
}

#[repr(transparent)]
pub struct Triangle<V: Vertex>(pub [V; 3]);

impl<V: PositionedVertex> Triangle<V> {
    pub fn new(
        a: impl Into<Vec2f>,
        b: impl Into<Vec2f>,
        c: impl Into<Vec2f>,
        extra: impl Into<V::ExtraData>,
    ) -> Self {
        let extra = extra.into();
        Self([
            V::new(a.into(), extra),
            V::new(b.into(), extra),
            V::new(c.into(), extra),
        ])
    }
}

#[repr(transparent)]
pub struct Line<V: Vertex>(pub [V; 2]);

impl<V: PositionedVertex> Line<V> {
    pub fn new(a: impl Into<Vec2f>, b: impl Into<Vec2f>, extra: impl Into<V::ExtraData>) -> Self {
        let extra = extra.into();
        Self([V::new(a.into(), extra), V::new(b.into(), extra)])
    }
}

pub type Rgba = [f32; 4];
pub type Uv = [f32; 2];

// [ x, y, r, g, b, a ]
#[repr(transparent)]
#[derive(Clone, Copy, Default)]
pub struct ColoredVertex([f32; 6]);

impl ColoredVertex {
    pub fn new(xy: impl Into<Vec2f>, rgba: impl Into<[f32; 4]>) -> Self {
        let mut slice = [0f32; 6];
        slice[0..2].copy_from_slice(&<[f32; 2]>::from(xy.into()));
        slice[2..6].copy_from_slice(&rgba.into());
        Self(slice)
    }
}

impl Vertex for ColoredVertex {
    fn init_vertex_buffer(gl: &glow::Context) {
        unsafe {
            let stride = std::mem::size_of::<Self>() as i32;

            gl.vertex_attrib_pointer_f32(0, 2, glow::FLOAT, false, stride, 0);
            gl.vertex_attrib_pointer_f32(1, 4, glow::FLOAT, false, stride, 8);
            gl.enable_vertex_attrib_array(0);
            gl.enable_vertex_attrib_array(1);
        }
    }
}
impl PositionedVertex for ColoredVertex {
    type ExtraData = Rgba;

    fn new(pos: Vec2f, extra: Self::ExtraData) -> Self {
        Self::new(pos, extra)
    }

    fn into_parts(self) -> (Vec2f, Self::ExtraData) {
        let mut xy = [0f32; 2];
        let mut rgba = [0f32; 4];

        xy.copy_from_slice(&self.0[0..2]);
        rgba.copy_from_slice(&self.0[2..6]);

        (xy.into(), rgba)
    }
}

pub struct ColoredVertexDefaultShader;

impl Shaders<ColoredVertex> for ColoredVertexDefaultShader {
    const VERT_SHADER: &'static str = r#"#version 330 core
        layout (location = 0) in vec2 pos;
        layout (location = 1) in vec4 color_in;

        uniform vec2 screenSize;

        out vec4 color;
        void main() {

            vec2 norm = pos / screenSize;
            vec2 clip = norm * 2 - 1;

            gl_Position = vec4(clip.x, -clip.y, 0.0, 1.0);
            color = color_in;
        }
    "#;

    const FRAG_SHADER: &'static str = r#"#version 330 core
        in vec4 color;

        void main() {
            gl_FragColor = color;
        }
    "#;
}

// [ x, y ]
#[repr(transparent)]
#[derive(Clone, Copy, Default)]
pub struct SimpleVertex([f32; 2]);

impl SimpleVertex {
    pub fn new(xy: impl Into<[f32; 2]>) -> Self {
        Self(xy.into())
    }
}

impl Vertex for SimpleVertex {
    fn init_vertex_buffer(gl: &glow::Context) {
        unsafe {
            let stride = std::mem::size_of::<Self>() as i32;

            gl.vertex_attrib_pointer_f32(0, 2, glow::FLOAT, false, stride, 0);
            gl.enable_vertex_attrib_array(0);
        }
    }
}

impl PositionedVertex for SimpleVertex {
    type ExtraData = ();

    fn new(pos: Vec2f, _extra: Self::ExtraData) -> Self {
        Self::new(pos)
    }

    fn into_parts(self) -> (Vec2f, Self::ExtraData) {
        (self.0.into(), ())
    }
}

// [ x, y, u, v ]
#[repr(transparent)]
#[derive(Clone, Copy, Default)]
pub struct UvVertex([f32; 4]);

impl UvVertex {
    pub fn new(xy: impl Into<Vec2f>, uv: impl Into<Vec2f>) -> Self {
        let mut slice = [0f32; 4];
        slice[0..2].copy_from_slice(&<[f32; 2]>::from(xy.into()));
        slice[2..4].copy_from_slice(&<[f32; 2]>::from(uv.into()));
        Self(slice)
    }

    pub fn new_const(xy: Vec2f, uv: Vec2f) -> Self {
        let mut slice = [0f32; 4];
        slice[0] = xy[0];
        slice[1] = xy[1];
        slice[2] = uv[0];
        slice[3] = uv[1];
        Self(slice)
    }
}

impl Vertex for UvVertex {
    fn init_vertex_buffer(gl: &glow::Context) {
        unsafe {
            let stride = std::mem::size_of::<Self>() as i32;

            gl.vertex_attrib_pointer_f32(0, 2, glow::FLOAT, false, stride, 0);
            gl.vertex_attrib_pointer_f32(1, 2, glow::FLOAT, false, stride, 8);
            gl.enable_vertex_attrib_array(0);
            gl.enable_vertex_attrib_array(1);
        }
    }
}

impl PositionedVertex for UvVertex {
    type ExtraData = Uv;

    fn new(pos: Vec2f, extra: Self::ExtraData) -> Self {
        Self::new(pos, extra)
    }

    fn into_parts(self) -> (Vec2f, Self::ExtraData) {
        let mut xy = [0f32; 2];
        let mut uv = [0f32; 2];

        xy.copy_from_slice(&self.0[0..2]);
        uv.copy_from_slice(&self.0[2..4]);

        (xy.into(), uv)
    }
}

pub struct FullscreenQuadVertexBuffer;

impl VertexBuffer<UvVertex> for FullscreenQuadVertexBuffer {
    const DRAW_MODE: u32 = glow::TRIANGLE_STRIP;

    fn get_vertex_slice(&self) -> &[UvVertex] {
        &[
            UvVertex([-1.0, 1.0, 0.0, 1.0]),
            UvVertex([1.0, 1.0, 1.0, 1.0]),
            UvVertex([-1.0, -1.0, 0.0, 0.0]),
            UvVertex([1.0, -1.0, 1.0, 0.0]),
        ]
    }

    fn vertex_count(&self) -> usize {
        4
    }
}

pub fn vertex_slice_to_bytes<V: Vertex>(s: &[V]) -> &[u8] {
    let ptr = s.as_ptr().cast();
    let len = std::mem::size_of_val(s);
    unsafe { std::slice::from_raw_parts(ptr, len) }
}
