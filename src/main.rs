use std::sync::Arc;

use app::DockedApp;
use eframe::{
    egui::{Align2, Color32, PaintCallback, PaintCallbackInfo, Painter, Rect, Ui},
    egui_glow,
};
use vector::{Vec2f, Vec2isize, Vec2usize};

pub mod app;
pub mod board;
pub mod circuits;
pub mod containers;
pub mod drawing;
pub mod editor;
pub mod ext;
pub mod macros;
pub mod precomputed;
pub mod selection;
pub mod str;
pub mod tabs;
pub mod vector;
pub mod vertex_renderer;
pub mod pool;

pub const CHUNK_SIZE: usize = 16;
pub const WIRE_WIDTH: f32 = 0.2;
pub const WIRE_POINT_WIDTH: f32 = 0.35;
pub const BIG_WIRE_POINT_WIDTH: f32 = 0.65;

fn main() -> Result<(), eframe::Error> {
    let options = eframe::NativeOptions {
        viewport: eframe::egui::ViewportBuilder::default().with_title("cuprous"),
        ..Default::default()
    };

    eframe::run_native(
        "cuprous-dev",
        options,
        Box::new(|cc| Box::new(DockedApp::create(cc))),
    )
}

#[derive(Debug, Clone, Copy)]
pub enum PinStyle {
    Circle,
    NGon {
        n: usize,
        angle: f32,
        directional: bool,
    },
}

pub struct Style {
    selection_fill: Color32,
    selection_border: Color32,
    pins: PinStyle,
}

impl Default for Style {
    fn default() -> Self {
        Self {
            selection_fill: Color32::from_rgba_unmultiplied(150, 150, 210, 1),
            selection_border: Color32::from_rgb(180, 180, 220),
            pins: PinStyle::NGon {
                n: 6,
                angle: 0.0,
                directional: true,
            },
        }
    }
}

#[derive(Clone, Copy)]
pub struct Screen {
    pub screen_rect: Rect,
    pub world_pos: Vec2f,
    pub scale: f32,
}

impl Screen {
    pub fn new(scr_rect: Rect, wld_pos: Vec2f, scale: f32) -> Self {
        Self {
            screen_rect: scr_rect,
            world_pos: wld_pos,
            scale,
        }
    }

    pub fn screen_to_world(&self, v: impl Into<Vec2f>) -> Vec2f {
        self.world_pos + (v.into() - self.screen_rect.left_top()) / self.scale
    }

    pub fn world_to_screen(&self, v: impl Into<Vec2f>) -> Vec2f {
        (v.into() - self.world_pos) * self.scale + self.screen_rect.left_top()
    }

    pub fn screen_to_world_tile(&self, v: impl Into<Vec2f>) -> Vec2isize {
        self.screen_to_world(v).convert(|v| v.floor() as isize)
    }

    pub fn world_to_screen_tile(&self, v: impl Into<Vec2isize>) -> Vec2f {
        self.world_to_screen(v.into().convert(|v| v as f32))
    }

    pub fn screen_to_world_rect(&self, r: Rect) -> Rect {
        Rect::from_min_size(
            self.screen_to_world(r.left_top()).into(),
            r.size() / self.scale,
        )
    }

    pub fn world_to_screen_rect(&self, r: Rect) -> Rect {
        Rect::from_min_size(
            self.world_to_screen(r.left_top()).into(),
            r.size() * self.scale,
        )
    }
}

#[derive(Clone)]
pub struct PaintContext<'a> {
    pub ui: &'a Ui,
    pub painter: &'a Painter,

    pub tile_bounds_tl: Vec2isize,
    pub tile_bounds_br: Vec2isize,
    pub tile_bounds_size: Vec2usize,

    pub chunk_bounds_tl: Vec2isize,
    pub chunk_bounds_br: Vec2isize,
    pub chunk_bounds_size: Vec2usize,

    pub gl: Arc<glow::Context>,
    pub screen: Screen,
    pub style: Arc<Style>,
}

pub struct CustomPaintContext<'a> {
    pub tile_bounds_tl: Vec2isize,
    pub tile_bounds_br: Vec2isize,
    pub tile_bounds_size: Vec2usize,

    pub chunk_bounds_tl: Vec2isize,
    pub chunk_bounds_br: Vec2isize,
    pub chunk_bounds_size: Vec2usize,

    pub screen: Screen,
    pub style: Arc<Style>,

    pub paint_info: PaintCallbackInfo,
    pub painter: &'a egui_glow::Painter,
}

impl<'a> PaintContext<'a> {
    pub fn new(ui: &'a Ui, screen: Screen, gl: Arc<glow::Context>, style: Arc<Style>) -> Self {
        let tl = screen.screen_to_world(screen.screen_rect.left_top());
        let br = screen.screen_to_world(screen.screen_rect.right_bottom());

        let tl = tl.convert(|v| v.floor() as isize);
        let br = br.convert(|v| v.floor() as isize);
        let size = (br - tl).convert(|v| v as usize) + 1;

        let chunks_tl = tl.convert(|v| div_floor_isize(v, CHUNK_SIZE as isize));
        let chunks_br = br.convert(|v| div_floor_isize(v, CHUNK_SIZE as isize));
        let chunks_size = (chunks_br - chunks_tl).convert(|v| v as usize) + 1;

        let painter = ui.painter();

        Self {
            ui,
            painter,
            tile_bounds_tl: tl,
            tile_bounds_br: br,
            tile_bounds_size: size,

            chunk_bounds_tl: chunks_tl,
            chunk_bounds_br: chunks_br,
            chunk_bounds_size: chunks_size,

            gl,
            screen,
            style,
        }
    }

    pub fn custom_draw(&self, draw: impl Fn(CustomPaintContext) + Sync + Send + 'static) {
        let tile_bounds_tl = self.tile_bounds_tl;
        let tile_bounds_br = self.tile_bounds_br;
        let tile_bounds_size = self.tile_bounds_size;
        let chunk_bounds_tl = self.chunk_bounds_tl;
        let chunk_bounds_br = self.chunk_bounds_br;
        let chunk_bounds_size = self.chunk_bounds_size;
        let screen = self.screen;
        let style = self.style.clone();

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

            let ctx = CustomPaintContext {
                tile_bounds_tl,
                tile_bounds_br,
                tile_bounds_size,
                chunk_bounds_tl,
                chunk_bounds_br,
                chunk_bounds_size,
                screen,
                style: style.clone(),

                painter: p,
                paint_info: pi,
            };

            draw(ctx);
        };
        self.painter.add(PaintCallback {
            rect: self.screen.screen_rect,
            callback: Arc::new(egui_glow::CallbackFn::new(callback)),
        });
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction8 {
    Up,
    UpRight,
    Right,
    DownRight,
    Down,
    DownLeft,
    Left,
    UpLeft,
}

#[allow(dead_code)]
impl Direction8 {
    pub const ALL: [Self; 8] = [
        Self::Up,
        Self::UpRight,
        Self::Right,
        Self::DownRight,
        Self::Down,
        Self::DownLeft,
        Self::Left,
        Self::UpLeft,
    ];

    pub const fn into_dir_isize(self) -> Vec2isize {
        let [x, y] = match self {
            Self::Up => [0, -1],
            Self::UpRight => [1, -1],
            Self::Right => [1, 0],
            Self::DownRight => [1, 1],
            Self::Down => [0, 1],
            Self::DownLeft => [-1, 1],
            Self::Left => [-1, 0],
            Self::UpLeft => [-1, -1],
        };
        Vec2isize::new(x, y)
    }

    pub const fn into_dir_f32(self) -> Vec2f {
        let [x, y] = match self {
            Self::Up => [0.0, -1.0],
            Self::UpRight => [1.0, -1.0],
            Self::Right => [1.0, 0.0],
            Self::DownRight => [1.0, 1.0],
            Self::Down => [0.0, 1.0],
            Self::DownLeft => [-1.0, 1.0],
            Self::Left => [-1.0, 0.0],
            Self::UpLeft => [-1.0, -1.0],
        };
        Vec2f::new(x, y)
    }

    pub const fn into_index(self) -> usize {
        match self {
            Self::Up => 0,
            Self::UpRight => 1,
            Self::Right => 2,
            Self::DownRight => 3,
            Self::Down => 4,
            Self::DownLeft => 5,
            Self::Left => 6,
            Self::UpLeft => 7,
        }
    }

    pub const fn from_index(i: usize) -> Self {
        match i % 8 {
            0 => Self::Up,
            1 => Self::UpRight,
            2 => Self::Right,
            3 => Self::DownRight,
            4 => Self::Down,
            5 => Self::DownLeft,
            6 => Self::Left,
            7 => Self::UpLeft,
            _ => unreachable!(),
        }
    }

    pub const fn rotated_clockwise_by(self, other: Self) -> Self {
        Self::from_index(self.into_index() + other.into_index())
    }

    pub const fn rotated_counterclockwise_by(self, other: Self) -> Self {
        Self::from_index(self.into_index() + 8 - other.into_index())
    }

    pub const fn inverted(self) -> Self {
        Self::from_index(self.into_index() + 4)
    }

    // UpRight (1), Up (0) / Down (4) -> UpLeft (7)
    // Up (0), Left (6) / Right (2) -> Down(4)
    pub const fn flip_by(self, other: Self) -> Self {
        let angle = self.rotated_counterclockwise_by(other);
        other.rotated_counterclockwise_by(angle)
    }

    pub const fn into_half_option(self) -> Option<Direction4Half> {
        match self {
            Self::Up => Some(Direction4Half::Up),
            Self::UpRight => Some(Direction4Half::UpRight),
            Self::Right => None,
            Self::DownRight => None,
            Self::Down => None,
            Self::DownLeft => None,
            Self::Left => Some(Direction4Half::Left),
            Self::UpLeft => Some(Direction4Half::UpLeft),
        }
    }

    pub const fn into_half(self) -> (Direction4Half, bool) {
        match self {
            Self::Up => (Direction4Half::Up, false),
            Self::UpRight => (Direction4Half::UpRight, false),
            Self::Right => (Direction4Half::Left, true),
            Self::DownRight => (Direction4Half::UpLeft, true),
            Self::Down => (Direction4Half::Up, true),
            Self::DownLeft => (Direction4Half::UpRight, true),
            Self::Left => (Direction4Half::Left, false),
            Self::UpLeft => (Direction4Half::UpLeft, false),
        }
    }

    pub const fn from_half(dir: Direction4Half, reverse: bool) -> Direction8 {
        match (dir, reverse) {
            (Direction4Half::Up, false) => Self::Up,
            (Direction4Half::UpRight, false) => Self::UpRight,
            (Direction4Half::Left, true) => Self::Right,
            (Direction4Half::UpLeft, true) => Self::DownRight,
            (Direction4Half::Up, true) => Self::Down,
            (Direction4Half::UpRight, true) => Self::DownLeft,
            (Direction4Half::Left, false) => Self::Left,
            (Direction4Half::UpLeft, false) => Self::UpLeft,
        }
    }

    pub fn iter_along(self, start: Vec2isize, length: usize) -> impl Iterator<Item = Vec2isize> {
        (0..length).map(move |i| start + self.into_dir_isize() * i as isize)
    }

    pub const fn into_align2(self) -> Align2 {
        match self {
            Self::Up => Align2::CENTER_TOP,
            Self::UpRight => Align2::RIGHT_TOP,
            Self::Right => Align2::RIGHT_CENTER,
            Self::DownRight => Align2::RIGHT_BOTTOM,
            Self::Down => Align2::CENTER_BOTTOM,
            Self::DownLeft => Align2::LEFT_BOTTOM,
            Self::Left => Align2::LEFT_CENTER,
            Self::UpLeft => Align2::LEFT_TOP,
        }
    }

    pub const fn is_diagonal(self) -> bool {
        match self {
            Self::Up => false,
            Self::UpRight => true,
            Self::Right => false,
            Self::DownRight => true,
            Self::Down => false,
            Self::DownLeft => true,
            Self::Left => false,
            Self::UpLeft => true,
        }
    }

    /// Returns angle in radians, 0 being at +X (Right), clockwise
    pub const fn into_angle_xp_cw(self) -> f32 {
        use std::f32::consts::*;
        const TAU_SEGMENTS: [f32; 8] = [
            0.0, FRAC_PI_4, FRAC_PI_2, 2.3561945, PI, 3.9269908, 4.712389, 5.497787,
        ];
        let i = match self {
            Direction8::Up => 6,
            Direction8::UpRight => 7,
            Direction8::Right => 0,
            Direction8::DownRight => 1,
            Direction8::Down => 2,
            Direction8::DownLeft => 3,
            Direction8::Left => 4,
            Direction8::UpLeft => 5,
        };
        TAU_SEGMENTS[i]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Direction4Half {
    Left,
    UpLeft,
    Up,
    UpRight,
}

impl Direction4Half {
    pub const ALL: [Self; 4] = [Self::Left, Self::UpLeft, Self::Up, Self::UpRight];

    fn is_diagonal(self) -> bool {
        match self {
            Self::Up => false,
            Self::UpRight => true,
            Self::Left => false,
            Self::UpLeft => true,
        }
    }

    pub fn into_index(self) -> usize {
        match self {
            Self::Left => 0,
            Self::UpLeft => 1,
            Self::Up => 2,
            Self::UpRight => 3,
        }
    }

    pub fn from_index(i: usize) -> Self {
        match i % 4 {
            0 => Self::Left,
            1 => Self::UpLeft,
            2 => Self::Up,
            3 => Self::UpRight,
            _ => unreachable!(),
        }
    }
}

impl From<Direction4Half> for Direction8 {
    fn from(value: Direction4Half) -> Self {
        Self::from_half(value, false)
    }
}

#[derive(Default, Clone, Copy)]
pub struct Direction8Array<T>([T; 8]);

impl<T> Direction8Array<T> {
    pub fn get(&self, dir: Direction8) -> &T {
        &self.0[dir.into_index()]
    }

    pub fn get_mut(&mut self, dir: Direction8) -> &mut T {
        &mut self.0[dir.into_index()]
    }

    pub fn from_fn(mut f: impl FnMut(Direction8) -> T) -> Self {
        Self(std::array::from_fn(|i| f(Direction8::from_index(i))))
    }

    pub fn iter(&self) -> impl Iterator<Item = (Direction8, &T)> {
        self.0
            .iter()
            .enumerate()
            .map(|(i, v)| (Direction8::from_index(i), v))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (Direction8, &mut T)> {
        self.0
            .iter_mut()
            .enumerate()
            .map(|(i, v)| (Direction8::from_index(i), v))
    }

    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.0.iter()
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.0.iter_mut()
    }
}

#[derive(Default)]
pub struct Direction4HalfArray<T>([T; 4]);

impl<T> Direction4HalfArray<T> {
    pub fn get(&self, dir: Direction4Half) -> &T {
        &self.0[dir.into_index()]
    }

    pub fn get_mut(&mut self, dir: Direction4Half) -> &mut T {
        &mut self.0[dir.into_index()]
    }
}

// literally just copied from generated std code
pub const fn div_floor_isize(lhs: isize, rhs: isize) -> isize {
    let d = lhs / rhs;
    let r = lhs % rhs;
    if (r > 0 && rhs < 0) || (r < 0 && rhs > 0) {
        d - 1
    } else {
        d
    }
}

pub const fn div_ceil_isize(lhs: isize, rhs: isize) -> isize {
    let d = lhs / rhs;
    let r = lhs % rhs;
    if (r > 0 && rhs > 0) || (r < 0 && rhs < 0) {
        d + 1
    } else {
        d
    }
}
