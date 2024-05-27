use app::DockedApp;
use eframe::egui::Rect;
use serde::{Deserialize, Serialize};
use vector::{Vec2f, Vec2isize};

pub mod app;
pub mod board;
pub mod containers;
pub mod ext;
pub mod macros;
pub mod str;
pub mod tabs;
pub mod vector;
pub mod vertex_renderer;

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