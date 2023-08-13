#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
#![feature(int_roundings)]
#![feature(lazy_cell)]
#![feature(thread_id_value)]

use std::{
    collections::HashMap,
    f32::consts::PI,
    ops::{Deref, Range},
    sync::Arc,
};

use board::selection::Selection;
use cache::GLOBAL_STR_CACHE;
use eframe::{
    egui::{self, Context, Sense, Ui},
    epaint::{PathShape, Rounding, Shape, Stroke},
};
use emath::{pos2, vec2, Pos2, Rect};

use serde::{Deserialize, Serialize};
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::{prelude::*, JsValue};

mod r#const;

mod vector;

use ui::InventoryItem;
use vector::{Vec2f, Vec2i};

mod containers;
use crate::containers::*;

mod circuits;
use circuits::CircuitPreview;

mod wires;

mod state;
use state::{State, WireState};

mod board;

#[macro_use]
mod macros;

#[cfg(debug_assertions)]
mod debug;

mod app;
mod cache;
mod io;
mod time;
mod ui;

#[cfg(debug_assertions)]
type RwLock<T> = debug::DebugRwLock<T>;

#[cfg(not(debug_assertions))]
type RwLock<T> = std::sync::RwLock<T>;
type Mutex<T> = std::sync::Mutex<T>;

struct BasicLoadingContext<'a> {
    previews: &'a HashMap<DynStaticStr, Arc<Box<dyn CircuitPreview>>>,
}

impl io::LoadingContext for BasicLoadingContext<'_> {
    fn get_circuit_preview<'a>(&'a self, ty: &DynStaticStr) -> Option<&'a dyn CircuitPreview> {
        self.previews.get(ty).map(|b| b.deref().deref())
    }
}

fn main() {
    #[cfg(not(target_arch = "wasm32"))]
    eframe::run_native(
        "rls",
        eframe::NativeOptions::default(),
        Box::new(|cc| Box::new(app::App::create(cc))),
    )
    .unwrap();
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
#[cfg(target_arch = "wasm32")]
pub async fn web_main(canvas_id: &str) -> Result<(), JsValue> {
    use eframe::WebOptions;

    eframe::WebRunner::new()
        .start(
            canvas_id,
            WebOptions::default(),
            Box::new(|cc| Box::new(app::App::create(cc))),
        )
        .await
}

#[allow(unused)]
#[derive(Debug, Clone, Copy)]
struct TileDrawBounds {
    pub screen_tl: Vec2f,
    pub screen_br: Vec2f,

    pub tiles_tl: Vec2i,
    pub tiles_br: Vec2i,

    pub chunks_tl: Vec2i,
    pub chunks_br: Vec2i,
}

impl TileDrawBounds {
    pub const EVERYTHING: TileDrawBounds = TileDrawBounds {
        screen_tl: Vec2f::single_value(f32::NEG_INFINITY),
        screen_br: Vec2f::single_value(f32::INFINITY),
        tiles_tl: Vec2i::single_value(i32::MIN),
        tiles_br: Vec2i::single_value(i32::MAX),
        chunks_tl: Vec2i::single_value(i32::MIN),
        chunks_br: Vec2i::single_value(i32::MAX),
    };
}

#[allow(clippy::redundant_allocation)]
pub struct PaintContext<'a> {
    screen: Screen,
    paint: &'a egui::Painter,
    rect: Rect,
    bounds: TileDrawBounds,
    ui: &'a Ui,
    egui_ctx: &'a Context,
}

impl<'a> PaintContext<'a> {
    pub fn new_on_ui(ui: &'a Ui, rect: Rect) -> Self {
        Self {
            screen: Screen {
                offset: rect.left_top().into(),
                pos: 0.0.into(),
                scale: 1.0,
            },
            paint: ui.painter(),
            rect,
            bounds: TileDrawBounds::EVERYTHING,
            ui,
            egui_ctx: ui.ctx(),
        }
    }

    pub fn with_rect(&self, rect: Rect) -> PaintContext<'a> {
        Self {
            rect,
            ui: self.ui,
            ..*self
        }
    }

    fn draw_chunks<const CHUNK_SIZE: usize, T: Default, P>(
        &self,
        chunks: &Chunks2D<CHUNK_SIZE, T>,
        pass: &P,
        draw_tester: impl Fn(&T) -> bool,
        drawer: impl Fn(&T, Vec2i, &Self, &P, &ChunksLookaround<CHUNK_SIZE, T>),
    ) {
        let TileDrawBounds {
            screen_tl: _,
            screen_br: _,
            tiles_tl,
            tiles_br,
            chunks_tl,
            chunks_br,
        } = self.bounds;

        let screen = &self.screen;

        for cy in chunks_tl.y()..=chunks_br.y() {
            let rowrange = chunks.get_chunk_row_range(cy as isize);
            let rowrange = Range {
                start: rowrange.start as i32,
                end: rowrange.end as i32,
            };

            for cx in (chunks_tl.x()..chunks_br.x() + 1).intersect(&rowrange) {
                let chunk_coord: Vec2i = [cx, cy].into();
                let chunk_tl = chunk_coord * 16;
                let chunk = unwrap_option_or_continue!(
                    chunks.get_chunk(chunk_coord.convert(|v| v as isize))
                );
                let chunk_viewport_tl = tiles_tl - chunk_tl;
                let chunk_viewport_br = tiles_br - chunk_tl;

                for j in 0..16 {
                    if j < chunk_viewport_tl.y() {
                        continue;
                    } else if j > chunk_viewport_br.y() {
                        break;
                    }

                    for i in 0..16 {
                        if i < chunk_viewport_tl.x() {
                            continue;
                        } else if i > chunk_viewport_br.x() {
                            break;
                        }

                        let tile = &chunk[i as usize][j as usize];
                        if !draw_tester(tile) {
                            continue;
                        }

                        let pos: Vec2i = chunk_tl + [i, j];
                        let draw_pos = Vec2f::from(self.rect.left_top())
                            + screen.world_to_screen(pos.convert(|v| v as f32));
                        let rect =
                            Rect::from_min_size(draw_pos.into(), vec2(screen.scale, screen.scale));
                        let lookaround = ChunksLookaround::new(
                            chunks,
                            chunk,
                            pos.convert(|v| v as isize),
                            [i as usize, j as usize].into(),
                        );

                        let drawer_ctx = self.with_rect(rect);

                        drawer(tile, pos, &drawer_ctx, pass, &lookaround)
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PanAndZoom {
    pos: Vec2f,
    scale: f32,
}

impl PanAndZoom {
    fn update(&mut self, ui: &egui::Ui, rect: Rect, allow_primary_button_drag: bool) {
        let zoom = ui.input(|input| {
            input
                .multi_touch()
                .map(|mt| mt.zoom_delta)
                .unwrap_or_else(|| {
                    let v = input.scroll_delta.y / 240.0;
                    if v < 0.0 {
                        1.0 / (-v + 1.0)
                    } else if v > 0.0 {
                        v + 1.0
                    } else {
                        1.0
                    }
                })
        });

        let interaction = ui.interact(rect, ui.id(), Sense::drag());

        if interaction.dragged_by(egui::PointerButton::Secondary)
            || (allow_primary_button_drag && interaction.dragged_by(egui::PointerButton::Primary))
        {
            self.pos -= interaction.drag_delta() / self.scale;
        }

        if zoom != 1.0 {
            let pointer_screen = Vec2f::from(
                ui.input(|i| i.pointer.hover_pos())
                    .unwrap_or_else(|| rect.center())
                    - rect.left_top(),
            );
            let world_before = self.pos + pointer_screen / self.scale;
            self.scale *= zoom;
            let world_after = self.pos + pointer_screen / self.scale;
            self.pos -= world_after - world_before;
        }
    }
}

impl Default for PanAndZoom {
    fn default() -> Self {
        Self {
            scale: 1.0,
            pos: Default::default(),
        }
    }
}

impl PanAndZoom {
    pub fn new(pos: Vec2f, scale: f32) -> Self {
        Self { pos, scale }
    }

    pub fn to_screen(self, offset: Vec2f) -> Screen {
        Screen {
            offset,
            pos: self.pos,
            scale: self.scale,
        }
    }
}

#[derive(Clone, Copy)]
pub struct Screen {
    offset: Vec2f,
    pos: Vec2f,
    scale: f32,
}

#[allow(unused)]
impl Screen {
    pub fn screen_to_world(&self, v: Vec2f) -> Vec2f {
        self.pos + (v - self.offset) / self.scale
    }

    pub fn world_to_screen(&self, v: Vec2f) -> Vec2f {
        (v - self.pos) * self.scale + self.offset
    }

    pub fn screen_to_world_tile(&self, v: Vec2f) -> Vec2i {
        self.screen_to_world(v).convert(|v| v.floor() as i32)
    }

    pub fn world_to_screen_tile(&self, v: Vec2i) -> Vec2f {
        self.world_to_screen(v.convert(|v| v as f32))
    }
}

struct SelectionInventoryItem {}
impl InventoryItem for SelectionInventoryItem {
    fn id(&self) -> &str {
        "selection"
    }

    fn draw(&self, ctx: &PaintContext) {
        let rect = ctx.rect.shrink2(ctx.rect.size() / 5.0);
        ctx.paint
            .rect_filled(rect, Rounding::none(), Selection::fill_color());
        let rect_corners = [
            rect.left_top(),
            rect.right_top(),
            rect.right_bottom(),
            rect.left_bottom(),
            rect.left_top(),
        ];

        let mut shapes = vec![];
        Shape::dashed_line_many(
            &rect_corners,
            Stroke::new(1.0, Selection::border_color()),
            3.0,
            2.0,
            &mut shapes,
        );

        shapes.into_iter().for_each(|s| {
            ctx.paint.add(s);
        });
    }
}

struct WireInventoryItem {}
impl InventoryItem for WireInventoryItem {
    fn id(&self) -> &str {
        "wire"
    }

    fn draw(&self, ctx: &PaintContext) {
        let color = WireState::False.color();

        let rect1 = Rect::from_center_size(
            ctx.rect.lerp_inside([0.2, 0.2].into()),
            ctx.rect.size() * 0.2,
        );
        let rect2 = Rect::from_center_size(
            ctx.rect.lerp_inside([0.8, 0.8].into()),
            ctx.rect.size() * 0.2,
        );

        ctx.paint
            .line_segment([rect1.center(), rect2.center()], Stroke::new(2.5, color));

        ctx.paint.add(Shape::Path(PathShape {
            points: rotated_rect_shape(rect1, PI * 0.25, rect1.center()),
            closed: true,
            fill: color,
            stroke: Stroke::NONE,
        }));

        ctx.paint.add(Shape::Path(PathShape {
            points: rotated_rect_shape(rect2, PI * 0.25, rect2.center()),
            closed: true,
            fill: color,
            stroke: Stroke::NONE,
        }));
    }
}

struct CircuitInventoryItem {
    preview: Arc<Box<dyn CircuitPreview>>,
    id: String,
}
impl InventoryItem for CircuitInventoryItem {
    fn id(&self) -> &str {
        &self.id
    }

    fn draw(&self, ctx: &PaintContext) {
        let size = self.preview.size().convert(|v| v as f32);
        let scale = Vec2f::from(ctx.rect.size()) / size;
        let scale = scale.x().min(scale.y());
        let size = size * scale;
        let rect = Rect::from_center_size(ctx.rect.center(), size.into());

        let circ_ctx = PaintContext {
            screen: Screen {
                scale,
                ..ctx.screen
            },
            rect,
            ..*ctx
        };
        self.preview.draw_preview(&circ_ctx, false);
    }
}

fn rotated_rect_shape(rect: Rect, angle: f32, origin: Pos2) -> Vec<Pos2> {
    let mut points = vec![
        rect.left_top(),
        rect.right_top(),
        rect.right_bottom(),
        rect.left_bottom(),
    ];

    let cos = angle.cos();
    let sin = angle.sin();

    for p in points.iter_mut() {
        let pl = *p - origin;

        let x = cos * pl.x - sin * pl.y;
        let y = sin * pl.x + cos * pl.y;
        *p = pos2(x, y) + origin.to_vec2();
    }

    points
}

trait Intersect {
    fn intersect(&self, other: &Self) -> Self;
}

impl<T: Ord + Copy> Intersect for Range<T> {
    fn intersect(&self, other: &Self) -> Self {
        Self {
            start: self.start.max(other.start),
            end: self.end.min(other.end),
        }
    }
}

pub trait Integer: Eq + Copy {
    const SIGNED: bool;
    const ZERO: Self;
    const MAX: Self;
    const MIN: Self;
}

macro_rules! impl_integer_trait {
    (signed $($t:ty),+) => {
        $(impl crate::Integer for $t {
            const SIGNED: bool = true;
            const ZERO: $t = 0;
            const MIN: $t = <$t>::MIN;
            const MAX: $t = <$t>::MAX;
        })+
    };
    (unsigned $($t:ty),+) => {
        $(impl crate::Integer for $t {
            const SIGNED: bool = false;
            const ZERO: $t = 0;
            const MIN: $t = <$t>::MIN;
            const MAX: $t = <$t>::MAX;
        })+
    };
}

impl_integer_trait!(signed i8, i16, i32, i64, i128, isize);
impl_integer_trait!(unsigned u8, u16, u32, u64, u128, usize);

macro_rules! impl_optional_int {
    ($name:ident, $none:expr) => {
        #[derive(Clone, Copy, Debug)]
        pub struct $name<T: Integer>(T);

        impl<T: Integer> Default for $name<T> {
            fn default() -> Self {
                Self(Self::NONE_VALUE)
            }
        }

        #[allow(unused)]
        impl<T: Integer> $name<T> {
            const NONE_VALUE: T = $none;

            pub fn new(value: T) -> Self {
                Self(value)
            }

            pub fn none() -> Self {
                Self(Self::NONE_VALUE)
            }

            pub fn is_none(&self) -> bool {
                self.0 == Self::NONE_VALUE
            }

            pub fn is_some(&self) -> bool {
                self.0 != Self::NONE_VALUE
            }

            pub fn is_some_and(&self, f: impl FnOnce(T) -> bool) -> bool {
                self.0 != Self::NONE_VALUE && f(self.0)
            }

            pub fn is_none_or(&self, f: impl FnOnce(T) -> bool) -> bool {
                self.0 == Self::NONE_VALUE || f(self.0)
            }

            pub fn get(&self) -> Option<T> {
                if self.0 == Self::NONE_VALUE {
                    None
                } else {
                    Some(self.0)
                }
            }

            pub fn set(&mut self, value: Option<T>) {
                self.0 = match value {
                    None => Self::NONE_VALUE,
                    Some(v) => v,
                }
            }
        }
    };
}

impl_optional_int!(OptionalInt, (if T::SIGNED { T::MIN } else { T::MAX }));
impl_optional_int!(OptionalNonzeroInt, (T::ZERO));

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Direction4 {
    Up,
    Left,
    Down,
    Right,
}

impl Direction4 {
    pub fn iter_all() -> impl Iterator<Item = Self> {
        [Self::Left, Self::Up, Self::Right, Self::Down].into_iter()
    }

    pub fn unit_vector(self) -> Vec2i {
        match self {
            Self::Up => [0, -1],
            Self::Left => [-1, 0],
            Self::Down => [0, 1],
            Self::Right => [1, 0],
        }
        .into()
    }

    pub fn move_vector(self, vec: Vec2i, distance: i32) -> Vec2i {
        vec + self.unit_vector() * distance
    }

    pub fn is_vertical(self) -> bool {
        match self {
            Self::Left | Self::Right => false,
            Self::Up | Self::Down => true,
        }
    }

    pub fn is_horizontal(self) -> bool {
        match self {
            Self::Left | Self::Right => true,
            Self::Up | Self::Down => false,
        }
    }

    pub fn is_left_up(self) -> bool {
        match self {
            Self::Left | Self::Up => true,
            Self::Right | Self::Down => false,
        }
    }

    pub fn is_right_bottom(self) -> bool {
        match self {
            Self::Right | Self::Down => true,
            Self::Left | Self::Up => false,
        }
    }

    pub fn inverted(self) -> Self {
        match self {
            Self::Up => Self::Down,
            Self::Left => Self::Right,
            Self::Down => Self::Up,
            Self::Right => Self::Left,
        }
    }

    /// Returns: (direction, forward)
    pub fn into_dir2(self) -> (Direction2, bool) {
        match self {
            Direction4::Up => (Direction2::Up, true),
            Direction4::Left => (Direction2::Left, true),
            Direction4::Down => (Direction2::Up, false),
            Direction4::Right => (Direction2::Left, false),
        }
    }

    /// if include_start { returns dist values } else { returns start pos + dist values }
    pub fn iter_pos_along(
        self,
        pos: Vec2i,
        dist: i32,
        include_start: bool,
    ) -> DirectionPosItreator {
        let dir = self.unit_vector() * if dist >= 0 { 1 } else { -1 };
        let dist = dist.unsigned_abs();

        let (pos, dist) = if include_start {
            (pos, dist + 1)
        } else {
            (pos + dir, dist)
        };
        DirectionPosItreator {
            pos,
            remaining: dist,
            dir,
        }
    }
}

impl From<Direction2> for Direction4 {
    fn from(value: Direction2) -> Self {
        match value {
            Direction2::Up => Direction4::Up,
            Direction2::Left => Direction4::Left,
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Direction2 {
    Up,
    Left,
}

impl Direction2 {
    pub fn iter_all() -> impl Iterator<Item = Direction2> {
        [Direction2::Left, Direction2::Up].into_iter()
    }

    pub fn unit_vector(self, forward: bool) -> Vec2i {
        match (self, forward) {
            (Direction2::Up, true) => [0, -1],
            (Direction2::Left, true) => [-1, 0],
            (Direction2::Up, false) => [0, 1],
            (Direction2::Left, false) => [1, 0],
        }
        .into()
    }

    pub fn move_vector(self, vec: Vec2i, distance: i32, forward: bool) -> Vec2i {
        vec + self.unit_vector(forward) * distance
    }

    pub fn choose_axis_component<T>(self, x: T, y: T) -> T {
        match self {
            Direction2::Up => y,
            Direction2::Left => x,
        }
    }

    /// if include_start { returns dist values } else { returns start pos + dist values }
    pub fn iter_pos_along(
        self,
        pos: Vec2i,
        dist: i32,
        include_start: bool,
    ) -> DirectionPosItreator {
        let dir = self.unit_vector(dist >= 0);
        let dist = dist.unsigned_abs();

        let (pos, dist) = if include_start {
            (pos, dist + 1)
        } else {
            (pos + dir, dist)
        };
        DirectionPosItreator {
            pos,
            remaining: dist,
            dir,
        }
    }
}

pub struct DirectionPosItreator {
    pos: Vec2i,
    remaining: u32,
    dir: Vec2i,
}

impl Iterator for DirectionPosItreator {
    type Item = Vec2i;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            return None;
        }

        let p = self.pos;
        self.pos += self.dir;
        self.remaining -= 1;
        Some(p)
    }
}

#[derive(Clone, Debug)]
pub enum DynStaticStr {
    Static(&'static str),
    Dynamic(Arc<str>),
}

impl Serialize for DynStaticStr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.deref())
    }
}

impl<'de> Deserialize<'de> for DynStaticStr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let str = <_>::deserialize(deserializer)?;
        Ok(Self::Dynamic(GLOBAL_STR_CACHE.cache(str)))
    }
}

impl std::hash::Hash for DynStaticStr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.deref().hash(state)
    }
}

impl Eq for DynStaticStr {}

impl PartialEq<str> for DynStaticStr {
    fn eq(&self, other: &str) -> bool {
        self.deref() == other
    }
}

impl<T: PartialEq<str>> PartialEq<T> for DynStaticStr {
    fn eq(&self, other: &T) -> bool {
        other.eq(self.deref())
    }
}

impl Deref for DynStaticStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            DynStaticStr::Static(str) => str,
            DynStaticStr::Dynamic(arc) => arc.deref(),
        }
    }
}

impl From<&'static str> for DynStaticStr {
    fn from(value: &'static str) -> Self {
        Self::Static(value)
    }
}

impl From<Arc<str>> for DynStaticStr {
    fn from(value: Arc<str>) -> Self {
        Self::Dynamic(value)
    }
}
