use app::DockedApp;
use eframe::egui::{Align2, Painter, Rect, Ui};
use vector::{Vec2f, Vec2isize, Vec2usize};

pub mod app;
pub mod board;
pub mod containers;
pub mod ext;
pub mod macros;
pub mod str;
pub mod tabs;
pub mod vector;
pub mod vertex_renderer;

pub const CHUNK_SIZE: usize = 16;

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

    pub screen: Screen
}

impl<'a> PaintContext<'a> {
    pub fn new(ui: &'a Ui, screen: Screen) -> Self {
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

            screen,
        }
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

    pub fn into_dir_isize(self) -> Vec2isize {
        match self {
            Self::Up => [0, -1],
            Self::UpRight => [1, -1],
            Self::Right => [1, 0],
            Self::DownRight => [1, 1],
            Self::Down => [0, 1],
            Self::DownLeft => [-1, 1],
            Self::Left => [-1, 0],
            Self::UpLeft => [-1, -1],
        }
        .into()
    }

    pub fn into_index(self) -> usize {
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

    pub fn from_index(i: usize) -> Self {
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

    pub fn rotated_clockwise_by(self, other: Self) -> Self {
        Self::from_index(self.into_index() + other.into_index())
    }

    pub fn rotated_couterclockwise_by(self, other: Self) -> Self {
        Self::from_index(self.into_index() + 8 - other.into_index())
    }

    pub fn inverted(self) -> Self {
        Self::from_index(self.into_index() + 4)
    }

    // UpRight (1), Up (0) / Down (4) -> UpLeft (7)
    // Up (0), Left (6) / Right (2) -> Down(4)
    pub fn flip_by(self, other: Self) -> Self {
        let angle = self.rotated_couterclockwise_by(other);
        other.rotated_couterclockwise_by(angle)
    }

    pub fn into_half(self) -> (Direction4Half, bool) {
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

    pub fn from_half(dir: Direction4Half, reverse: bool) -> Direction8 {
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

    pub fn into_align2(self) -> Align2 {
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction4Half {
    Left,
    UpLeft,
    Up,
    UpRight,
}

impl Direction4Half {
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
        self.0.iter().enumerate().map(|(i, v)| (Direction8::from_index(i), v))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (Direction8, &mut T)> {
        self.0.iter_mut().enumerate().map(|(i, v)| (Direction8::from_index(i), v))
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