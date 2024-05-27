use glow::{Buffer, Context, HasContext, Program, UniformLocation, VertexArray};

const VERT_SHADER: &str = r#"#version 330 core
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

const FRAG_SHADER: &str = r#"#version 330 core
  in vec4 color;

  void main() {
    gl_FragColor = color;
  }
"#;

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

    #[error("Could not find shader uniform \"screenSize\"")]
    UniformError,
}

pub struct VertexRenderer {
    buffer: Buffer,
    vertex_array: VertexArray,
    program: Program,
    screen_size_location: UniformLocation,

    tris: Vec<Triangle>,
    lines: Vec<Line>,
}

#[allow(dead_code)]
impl VertexRenderer {
    pub fn new(gl: &Context) -> Result<Self, RendererInitError> {
        unsafe {
            let vert_shader = gl
                .create_shader(glow::VERTEX_SHADER)
                .map_err(RendererInitError::ShaderCreation)?;

            gl.shader_source(vert_shader, VERT_SHADER);
            gl.compile_shader(vert_shader);

            if !gl.get_shader_compile_status(vert_shader) {
                let log = gl.get_shader_info_log(vert_shader);
                return Err(RendererInitError::VertexShaderCompilation(log));
            }

            let frag_shader = gl
                .create_shader(glow::FRAGMENT_SHADER)
                .map_err(RendererInitError::ShaderCreation)?;

            gl.shader_source(frag_shader, FRAG_SHADER);
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

            let screen_size_location = gl
                .get_uniform_location(program, "screenSize")
                .ok_or(RendererInitError::UniformError)?;

            gl.delete_shader(vert_shader);
            gl.delete_shader(frag_shader);

            let buffer = gl
                .create_buffer()
                .map_err(RendererInitError::BufferCreation)?;

            let vertex_array = gl
                .create_vertex_array()
                .map_err(RendererInitError::VertexArrayCreation)?;

            gl.bind_buffer(glow::ARRAY_BUFFER, Some(buffer));
            gl.bind_vertex_array(Some(vertex_array));

            let stride = std::mem::size_of::<Vertex>() as i32;

            gl.vertex_attrib_pointer_f32(0, 2, glow::FLOAT, false, stride, 0);
            gl.vertex_attrib_pointer_f32(1, 4, glow::FLOAT, false, stride, 8);
            gl.enable_vertex_attrib_array(0);
            gl.enable_vertex_attrib_array(1);

            gl.bind_buffer(glow::ARRAY_BUFFER, None);
            gl.bind_vertex_array(None);

            Ok(Self {
                buffer,
                vertex_array,
                program,
                screen_size_location,

                tris: vec![],
                lines: vec![],
            })
        }
    }

    fn draw(&self, gl: &Context, screen_size: [f32; 2], data: &[u8], mode: u32, count: i32) {
        if data.is_empty() {
            return;
        }

        unsafe {
            gl.bind_buffer(glow::ARRAY_BUFFER, Some(self.buffer));

            gl.buffer_data_u8_slice(glow::ARRAY_BUFFER, data, glow::STREAM_DRAW);

            gl.bind_vertex_array(Some(self.vertex_array));
            gl.use_program(Some(self.program));
            gl.uniform_2_f32(
                Some(&self.screen_size_location),
                screen_size[0],
                screen_size[1],
            );

            gl.draw_arrays(mode, 0, count);

            gl.use_program(None);
            gl.bind_buffer(glow::ARRAY_BUFFER, None);
            gl.bind_vertex_array(None);
        }
    }

    pub fn draw_tris(&self, gl: &Context, screen_size: [f32; 2]) {
        self.draw(
            gl,
            screen_size,
            Triangle::slice_to_bytes(&self.tris),
            glow::TRIANGLES,
            self.tris.len() as i32 * 3,
        );
    }

    pub fn draw_lines(&self, gl: &Context, screen_size: [f32; 2], line_width: f32) {
        unsafe { gl.line_width(line_width) };
        self.draw(
            gl,
            screen_size,
            Line::slice_to_bytes(&self.lines),
            glow::LINES,
            self.lines.len() as i32 * 2,
        );
    }

    pub fn clear(&mut self) {
        self.tris.clear();
        self.lines.clear();
    }

    pub fn push_line(&mut self, line: Line) {
        self.lines.push(line);
    }

    pub fn push_quad(&mut self, quad: Quad) {
        let tris: [Triangle; 2] = quad.into();
        self.tris.extend(tris);
    }

    pub fn push_triangle(&mut self, tri: Triangle) {
        self.tris.push(tri);
    }

    pub fn add_singlecolor_line(&mut self, a: impl Into<[f32; 2]>, b: impl Into<[f32; 2]>, rgba: impl Into<[f32; 4]>) {
        let rgba = rgba.into();
        self.push_line(Line([
            Vertex::new(a.into(), rgba),
            Vertex::new(b.into(), rgba),
        ]));
    }

    pub fn add_singlecolor_triangle(&mut self, points: [[f32; 2]; 3], rgba: impl Into<[f32; 4]>) {
        let rgba = rgba.into();
        self.push_triangle(Triangle([
            Vertex::new(points[0], rgba),
            Vertex::new(points[1], rgba),
            Vertex::new(points[2], rgba),
        ]));
    }

    pub fn add_singlecolor_quad(&mut self, points: [[f32; 2]; 4], rgba: impl Into<[f32; 4]>) {
        let rgba = rgba.into();
        self.push_quad(Quad {
            tl: Vertex::new(points[0], rgba),
            tr: Vertex::new(points[1], rgba),
            bl: Vertex::new(points[2], rgba),
            br: Vertex::new(points[3], rgba),
        });
    }

    pub fn add_singlecolor_rect(
        &mut self,
        tl: impl Into<[f32; 2]>,
        size: impl Into<[f32; 2]>,
        rgba: impl Into<[f32; 4]>,
    ) {
        let rgba = rgba.into();
        let tl = tl.into();
        let size = size.into();
        self.push_quad(Quad {
            tl: Vertex::new([tl[0], tl[1]], rgba),
            tr: Vertex::new([tl[0] + size[0], tl[1]], rgba),
            bl: Vertex::new([tl[0], tl[1] + size[1]], rgba),
            br: Vertex::new([tl[0] + size[0], tl[1] + size[1]], rgba),
        });
    }
}

#[derive(Clone, Copy)]
pub struct Quad {
    tl: Vertex,
    tr: Vertex,
    bl: Vertex,
    br: Vertex,
}

impl From<Quad> for [Triangle; 2] {
    fn from(val: Quad) -> Self {
        [
            Triangle([val.tl, val.tr, val.bl]),
            Triangle([val.tr, val.bl, val.br]),
        ]
    }
}

#[repr(transparent)]
pub struct Triangle(pub [Vertex; 3]);

impl Triangle {
    pub fn slice_to_bytes(s: &[Triangle]) -> &[u8] {
        let ptr = s.as_ptr().cast();
        let len = std::mem::size_of_val(s);
        unsafe { std::slice::from_raw_parts(ptr, len) }
    }
}

#[repr(transparent)]
pub struct Line(pub [Vertex; 2]);

impl Line {
    pub fn slice_to_bytes(s: &[Line]) -> &[u8] {
        let ptr = s.as_ptr().cast();
        let len = std::mem::size_of_val(s);
        unsafe { std::slice::from_raw_parts(ptr, len) }
    }
}

// [ x, y, r, g, b, a ]
#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct Vertex([f32; 6]);

impl Vertex {
    pub fn new(xy: impl Into<[f32; 2]>, rgba: impl Into<[f32; 4]>) -> Self {
        let mut slice = [0f32; 6];
        slice[0..2].copy_from_slice(&xy.into());
        slice[2..6].copy_from_slice(&rgba.into());
        Self(slice)
    }

    pub fn slice_to_bytes(s: &[Vertex]) -> &[u8] {
        let ptr = s.as_ptr().cast();
        let len = std::mem::size_of_val(s);
        unsafe { std::slice::from_raw_parts(ptr, len) }
    }
}
