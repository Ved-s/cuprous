use std::{
    borrow::Borrow, collections::HashSet, f32::consts::TAU, fmt::Debug, hash::Hash, num::NonZeroU32, ops::{Deref, Not, Range}, sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    }
};

use app::{SimulationContext, Style};
use board::{EditableCircuitBoard, CircuitDesignControl};
use cache::GLOBAL_STR_CACHE;
use eframe::{
    egui::{self, Sense, Ui, ViewportBuilder},
    epaint::{Color32, Rounding},
    Theme,
};
use emath::{vec2, Rect};

use error::{ErrorList, OptionReport};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_intermediate::Intermediate;
#[cfg(feature = "wasm")]
use wasm_bindgen::{prelude::*, JsValue};

mod pool;
mod vector;

use ui::editor::{TileDrawBounds, CircuitBoardEditor};
use vector::{Vec2f, Vec2i, Vec2u};
use wires::WirePart;

mod containers;
use crate::containers::*;

#[macro_use]
mod circuits;
use circuits::{CircuitPreview, CircuitStateContext};

mod error;
mod wires;

mod state;
use state::State;
mod string;

mod board;

#[macro_use]
mod macros;

#[cfg(all(feature = "deadlock_detection", not(feature = "wasm")))]
mod debug;

mod app;
mod cache;
mod ext;
mod graphics;
mod io;
mod path;
mod time;

#[cfg(feature = "wasm")]
mod web;

#[macro_use]
mod ui;

#[cfg(all(feature = "deadlock_detection", not(feature = "wasm")))]
type RwLock<T> = debug::DebugRwLock<T>;
#[cfg(all(feature = "deadlock_detection", not(feature = "wasm")))]
type Mutex<T> = debug::DebugMutex<T>;

#[cfg(any(not(feature = "deadlock_detection"), feature = "wasm"))]
type RwLock<T> = parking_lot::RwLock<T>;
#[cfg(any(not(feature = "deadlock_detection"), feature = "wasm"))]
type Mutex<T> = parking_lot::Mutex<T>;

fn main() {
    #[cfg(all(feature = "deadlock_detection", not(feature = "wasm")))]
    debug::set_this_thread_debug_name("egui main thread");

    #[cfg(not(feature = "wasm"))]
    {
        let options = eframe::NativeOptions {
            follow_system_theme: false,
            default_theme: Theme::Dark,
            viewport: ViewportBuilder::default()
                .with_app_id("cuprous")
                .with_drag_and_drop(true),

            ..Default::default()
        };

        eframe::run_native(
            "Cuprous Logic Simulator",
            options,
            Box::new(|cc| Box::new(app::App::create(cc))),
        )
        .unwrap();
    }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
#[cfg(feature = "wasm")]
pub async fn web_main(canvas_id: &str) -> Result<(), JsValue> {
    let options = eframe::WebOptions {
        follow_system_theme: false,
        default_theme: Theme::Dark,

        ..Default::default()
    };

    eframe::WebRunner::new()
        .start(
            canvas_id,
            options,
            Box::new(|cc| Box::new(app::App::create(cc))),
        )
        .await
}

#[allow(clippy::redundant_allocation)]
pub struct PaintContext<'a> {
    screen: Screen,
    style: &'a Style,
    paint: &'a egui::Painter,
    rect: Rect,
    ui: &'a Ui,
}

impl<'a> PaintContext<'a> {
    pub fn new_on_ui(ui: &'a Ui, style: &'a Style, rect: Rect, scale: f32) -> Self {
        Self {
            screen: Screen {
                scr_rect: rect,
                wld_pos: 0.0.into(),
                scale,
            },
            style,
            paint: ui.painter(),
            rect,
            ui,
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
        bounds: TileDrawBounds,
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
        } = bounds;

        let screen = &self.screen;

        for cy in chunks_tl.y..=chunks_br.y {
            let rowrange = chunks.get_chunk_row_range(cy as isize);
            let rowrange = Range {
                start: rowrange.start as i32,
                end: rowrange.end as i32,
            };

            for cx in (chunks_tl.x..chunks_br.x + 1).intersect(&rowrange) {
                let chunk_coord: Vec2i = [cx, cy].into();
                let chunk_tl = chunk_coord * 16;
                let chunk = unwrap_option_or_continue!(
                    chunks.get_chunk(chunk_coord.convert(|v| v as isize))
                );
                let chunk_viewport_tl = tiles_tl - chunk_tl;
                let chunk_viewport_br = tiles_br - chunk_tl;

                for j in 0..16 {
                    if j < chunk_viewport_tl.y {
                        continue;
                    } else if j > chunk_viewport_br.y {
                        break;
                    }

                    for i in 0..16 {
                        if i < chunk_viewport_tl.x {
                            continue;
                        } else if i > chunk_viewport_br.x {
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
    center_pos: Vec2f,
    scale: f32,
}

impl PanAndZoom {
    fn update(&mut self, ui: &egui::Ui, rect: Rect, allow_primary_button_drag: bool) {
        let interaction = ui.interact(rect, ui.id(), Sense::click_and_drag());

        let zoom = ui.input(|input| {
            input
                .multi_touch()
                .map(|mt| mt.zoom_delta)
                .unwrap_or_else(|| {
                    if interaction.hovered() {
                        let v = input.scroll_delta.y / 240.0;
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

        if interaction.dragged_by(egui::PointerButton::Secondary)
            || (allow_primary_button_drag && interaction.dragged_by(egui::PointerButton::Primary))
        {
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
        Screen {
            scr_rect: screen_rect,
            wld_pos: tl_pos,
            scale: self.scale,
        }
    }
}

#[derive(Clone, Copy)]
pub struct Screen {
    scr_rect: Rect,
    wld_pos: Vec2f,
    scale: f32,
}

impl Screen {
    pub fn screen_to_world(&self, v: Vec2f) -> Vec2f {
        self.wld_pos + (v - self.scr_rect.left_top()) / self.scale
    }

    pub fn world_to_screen(&self, v: Vec2f) -> Vec2f {
        (v - self.wld_pos) * self.scale + self.scr_rect.left_top()
    }

    pub fn screen_to_world_tile(&self, v: Vec2f) -> Vec2i {
        self.screen_to_world(v).convert(|v| v.floor() as i32)
    }

    pub fn world_to_screen_tile(&self, v: Vec2i) -> Vec2f {
        self.world_to_screen(v.convert(|v| v as f32))
    }

    pub fn screen_to_world_rect(&self, r: Rect) -> Rect {
        Rect::from_min_size(
            self.screen_to_world(r.left_top().into()).into(),
            r.size() / self.scale,
        )
    }

    pub fn world_to_screen_rect(&self, r: Rect) -> Rect {
        Rect::from_min_size(
            self.world_to_screen(r.left_top().into()).into(),
            r.size() * self.scale,
        )
    }
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

        impl<T: Serialize + Integer> Serialize for $name<T> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: Serializer,
            {
                self.0.serialize(serializer)
            }
        }

        impl<'de, T: Deserialize<'de> + Integer> Deserialize<'de> for $name<T> {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                Ok(Self(T::deserialize(deserializer)?))
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

impl<'de> Deserialize<'de> for Direction4 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Direction4::from_char(char::deserialize(deserializer)?).unwrap_or(Direction4::Up))
    }
}

impl Serialize for Direction4 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.into_char().serialize(serializer)
    }
}

#[allow(unused)]
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

    pub const fn is_vertical(self) -> bool {
        match self {
            Self::Left | Self::Right => false,
            Self::Up | Self::Down => true,
        }
    }

    pub const fn is_horizontal(self) -> bool {
        match self {
            Self::Left | Self::Right => true,
            Self::Up | Self::Down => false,
        }
    }

    pub const fn is_left_up(self) -> bool {
        match self {
            Self::Left | Self::Up => true,
            Self::Right | Self::Down => false,
        }
    }

    pub const fn is_right_bottom(self) -> bool {
        match self {
            Self::Right | Self::Down => true,
            Self::Left | Self::Up => false,
        }
    }

    pub const fn inverted(self) -> Self {
        match self {
            Self::Up => Self::Down,
            Self::Left => Self::Right,
            Self::Down => Self::Up,
            Self::Right => Self::Left,
        }
    }

    pub const fn inverted_ud(self) -> Self {
        match self {
            Self::Up => Self::Down,
            Self::Left => Self::Left,
            Self::Down => Self::Up,
            Self::Right => Self::Right,
        }
    }

    pub const fn inverted_lr(self) -> Self {
        match self {
            Self::Up => Self::Up,
            Self::Left => Self::Right,
            Self::Down => Self::Down,
            Self::Right => Self::Left,
        }
    }

    /// Returns: (direction, forward)
    pub const fn into_dir2(self) -> (Direction2, bool) {
        match self {
            Self::Up => (Direction2::Up, true),
            Self::Left => (Direction2::Left, true),
            Self::Down => (Direction2::Up, false),
            Self::Right => (Direction2::Left, false),
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

    pub const fn rotate_clockwise(self) -> Self {
        match self {
            Self::Up => Self::Right,
            Self::Left => Self::Up,
            Self::Down => Self::Left,
            Self::Right => Self::Down,
        }
    }

    pub const fn rotate_counterclockwise(self) -> Self {
        match self {
            Self::Up => Self::Left,
            Self::Left => Self::Down,
            Self::Down => Self::Right,
            Self::Right => Self::Up,
        }
    }

    pub const fn into_char(self) -> char {
        match self {
            Direction4::Up => 'u',
            Direction4::Left => 'l',
            Direction4::Down => 'd',
            Direction4::Right => 'r',
        }
    }

    pub const fn from_char(char: char) -> Option<Self> {
        match char {
            'u' => Some(Direction4::Up),
            'l' => Some(Direction4::Left),
            'd' => Some(Direction4::Down),
            'r' => Some(Direction4::Right),
            _ => None,
        }
    }

    pub const fn name(self) -> &'static str {
        match self {
            Direction4::Up => "Up",
            Direction4::Left => "Left",
            Direction4::Down => "Down",
            Direction4::Right => "Right",
        }
    }

    pub fn angle_to_right(self) -> f32 {
        match self {
            Direction4::Right => TAU * 0.0,
            Direction4::Up => TAU * 0.25,
            Direction4::Left => TAU * 0.5,
            Direction4::Down => TAU * 0.75,
        }
    }

    pub fn angle_to_left(self) -> f32 {
        match self {
            Direction4::Left => TAU * 0.0,
            Direction4::Down => TAU * 0.25,
            Direction4::Right => TAU * 0.5,
            Direction4::Up => TAU * 0.75,
        }
    }

    // Up - no rotation, Right - one, Down - two, etc
    pub const fn rotate_clockwise_by(self, other: Direction4) -> Self {
        match other {
            Direction4::Up => self,
            Direction4::Right => self.rotate_clockwise(),
            Direction4::Down => self.inverted(),
            Direction4::Left => self.rotate_counterclockwise(),
        }
    }

    // Up - no rotation, Left - one, Down - two, etc
    pub const fn rotate_counterclockwise_by(self, other: Direction4) -> Self {
        match other {
            Direction4::Up => self,
            Direction4::Left => self.rotate_clockwise(),
            Direction4::Down => self.inverted(),
            Direction4::Right => self.rotate_counterclockwise(),
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

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Serialize, Deserialize)]
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

#[derive(Clone)]
pub enum DynStaticStr {
    Static(&'static str),
    Dynamic(Arc<str>),
}

impl std::fmt::Debug for DynStaticStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Static(s) => f.write_fmt(format_args!("static \"{s}\"")),
            Self::Dynamic(s) => f.write_fmt(format_args!("dynamic \"{}\"", s.deref())),
        }
    }
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

impl Hash for DynStaticStr {
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

impl Borrow<str> for DynStaticStr {
    fn borrow(&self) -> &str {
        self.deref()
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

impl From<String> for DynStaticStr {
    fn from(val: String) -> Self {
        DynStaticStr::Dynamic(val.into())
    }
}

pub struct PastePreview {
    wires: Vec<crate::io::WirePartCopyData>,
    circuits: Vec<(crate::io::CircuitCopyData, CircuitPreview)>,
    size: Vec2u,
}

impl PastePreview {
    pub fn new(
        data: crate::io::CopyPasteData,
        ctx: &Arc<SimulationContext>,
        errors: &mut ErrorList,
    ) -> Self {
        let mut errors = errors.enter_context(|| "loading paste data");
        let wires = data.wires;
        let circuits: Vec<_> = data
            .circuits
            .into_iter()
            .filter_map(|d| {
                ctx.previews
                    .get(&d.ty)
                    .report_none(&mut errors, || {
                        format!("circuit {} does not exist", d.ty.deref())
                    })
                    .and_then(|p| p.load_copy(&d, ctx, &mut errors).map(|b| (d, b)))
            })
            .collect();

        let size = {
            let mut size = Vec2u::default();
            for wire in wires.iter() {
                size = [size.x.max(wire.pos.x + 1), size.y.max(wire.pos.y + 1)].into()
            }
            for (circuit, preview) in circuits.iter() {
                let br = circuit.pos + preview.describe().size;
                size = [size.x.max(br.x), size.y.max(br.y)].into()
            }
            size
        };

        Self {
            wires,
            circuits,
            size,
        }
    }

    pub fn draw(&self, editor: &CircuitBoardEditor, pos: Vec2i, ctx: &PaintContext) {
        let rect = Rect::from_min_size(
            ctx.screen.world_to_screen_tile(pos).into(),
            (self.size.convert(|v| v as f32) * ctx.screen.scale).into(),
        );
        ctx.paint.rect_filled(
            rect,
            Rounding::ZERO,
            Color32::from_rgba_unmultiplied(0, 120, 120, 120),
        );

        for wire in self.wires.iter() {
            if let Some(length) = NonZeroU32::new(wire.length) {
                let part = WirePart {
                    pos: pos + wire.pos.convert(|v| v as i32),
                    length,
                    dir: wire.dir,
                };
                editor.draw_wire_part(ctx, &part, wire.colors.none.or(wire.colors.r#false).unwrap_or(Color32::from_gray(128)))
            }
        }

        for (circuit, preview) in self.circuits.iter() {
            let size = preview.describe().size;
            if size.x == 0 || size.y == 0 {
                return;
            }
            let pos = pos + circuit.pos.convert(|v| v as i32);
            let rect = Rect::from_min_size(
                ctx.screen.world_to_screen_tile(pos).into(),
                (size.convert(|v| v as f32) * ctx.screen.scale).into(),
            );
            preview.draw(&ctx.with_rect(rect), true);
        }
    }

    fn place(&self, board: &mut EditableCircuitBoard, pos: Vec2i, errors: &mut ErrorList) {
        if self.circuits.iter().any(|(c, p)| {
            !board.can_place_circuit_at(p.describe().size, pos + c.pos.convert(|v| v as i32), None)
        }) {
            return;
        }

        let sim_lock = { board.board.sim_lock.clone() };
        let sim_lock = sim_lock.write();

        let mut wire_ids: HashSet<usize> = HashSet::new();
        for wire in self.wires.iter() {
            if let Some(length) = NonZeroU32::new(wire.length) {
                let part = WirePart {
                    pos: pos + wire.pos.convert(|v| v as i32),
                    length,
                    dir: wire.dir,
                };
                if let Some(id) = board.place_wire_part(part, false, wire.colors) {
                    wire_ids.insert(id);
                }
            }
        }
        for (circuit_data, preview) in self.circuits.iter() {
            let data = matches!(circuit_data.imp, Intermediate::Unit)
                .not()
                .then_some(&circuit_data.imp);
            let id = board.place_circuit(
                pos + circuit_data.pos.convert(|v| v as i32),
                false,
                preview,
                None,
                true,
                data,
                &mut |board, id| {
                    if let Some(circuit) = board.board.circuits.read().get(id).cloned() {
                        if !matches!(
                            circuit_data.internal,
                            serde_intermediate::Intermediate::Unit
                        ) {
                            for state in board.board.states.states.read().iter() {
                                let ctx = CircuitStateContext::new(state.clone(), circuit.clone());
                                let mut errors = errors.enter_context(|| {
                                    format!(
                                        "loading state for {} {} at {},{}",
                                        circuit.ty.deref(),
                                        circuit.id,
                                        circuit.pos.x,
                                        circuit.pos.y
                                    )
                                });
                                state.write_circuit(id, |state| {
                                    state.internal = circuit.imp.write().load_internal(
                                        &ctx,
                                        &circuit_data.internal,
                                        true,
                                        &mut errors,
                                    );
                                });
                            }
                        }
                    }
                },
            );
            if let Some(id) = id {
                if !circuit_data.design_controls.is_empty() {
                    let mut designs = board.board.designs.write();
                    let design = designs.current_mut();

                    for (i, control) in circuit_data.design_controls.iter().enumerate() {
                        let control = unwrap_option_or_continue!(control);
                        design.controls.insert(
                            (id, i),
                            CircuitDesignControl {
                                rect: control.rect,
                                display_name: control.display_name.as_str().into(),
                            },
                        );
                    }
                }

                if let Some(dur) = circuit_data.update {
                    for state in board.board.states.states.read().iter() {
                        state.set_circuit_update_interval(id, dur)
                    }
                }
            }
        }

        for wire in wire_ids {
            board.board.states.update_wire(wire, true);
        }
        drop(sim_lock)
    }
}

#[derive(Default)]
pub struct ArcString {
    string: Option<String>,
    arc: RwLock<Option<Arc<str>>>,
    check_str: AtomicBool,
}

impl Clone for ArcString {
    fn clone(&self) -> Self {
        if self.string.is_none() && self.arc.read().is_none() {
            return Default::default();
        }

        Self {
            string: None,
            arc: RwLock::new(Some(self.get_arc())),
            check_str: AtomicBool::new(self.check_str.load(Ordering::Relaxed)),
        }
    }
}

impl Debug for ArcString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.get_str().deref().fmt(f)
    }
}

impl Serialize for ArcString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.get_str().deref().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for ArcString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        <&str>::deserialize(deserializer).map(|str| str.into())
    }
}

impl ArcString {
    fn check_string(&self, s: &str) -> bool {
        let check_str = self.check_str.load(Ordering::Relaxed);

        if !check_str {
            return true;
        }
        let str = match &self.string {
            Some(s) => s.as_str(),
            None => "",
        };
        if str == s {
            self.check_str.store(false, Ordering::Relaxed);
            return true;
        }
        false
    }

    pub fn get_arc(&self) -> Arc<str> {
        let arc = self.arc.read().clone();
        if let Some(arc) = arc {
            if self.check_string(&arc) {
                return arc;
            }
        }

        let mut arc = self.arc.write();
        if let Some(arc) = arc.clone() {
            if self.check_string(&arc) {
                return arc;
            }
        }

        self.check_str.store(false, Ordering::Relaxed);
        let str = self.string.as_deref().unwrap_or_default();
        let new_arc = Arc::<str>::from(str);

        *arc = Some(new_arc.clone());
        new_arc
    }

    pub fn get_mut(&mut self) -> &mut String {
        self.check_str.store(true, Ordering::Relaxed);
        self.string.get_or_insert_with(|| {
            self.arc
                .read()
                .as_ref()
                .map(|a| a.deref().into())
                .unwrap_or_default()
        })
    }

    pub fn get_str(&self) -> ArcBorrowStr<'_> {
        if let Some(string) = &self.string {
            ArcBorrowStr::Borrow(string)
        } else if let Some(arc) = self.arc.read().as_ref() {
            ArcBorrowStr::Arc(arc.clone())
        } else {
            ArcBorrowStr::Borrow("")
        }
    }

    pub fn is_empty(&self) -> bool {
        !self.string.as_ref().is_some_and(|s| !s.is_empty())
            && !self.arc.read().as_ref().is_some_and(|a| !a.is_empty())
    }

    #[allow(unused)]
    fn len(&self) -> usize {
        self.string
            .as_ref()
            .map(|s| s.len())
            .or_else(|| self.arc.read().as_ref().map(|arc| arc.len()))
            .unwrap_or(0)
    }
}

impl From<&str> for ArcString {
    fn from(value: &str) -> Self {
        Self {
            string: Some(value.into()),
            arc: RwLock::new(None),
            check_str: AtomicBool::new(false),
        }
    }
}

impl From<Arc<str>> for ArcString {
    fn from(value: Arc<str>) -> Self {
        ArcString {
            string: None,
            arc: RwLock::new(Some(value)),
            check_str: AtomicBool::new(false),
        }
    }
}

impl From<DynStaticStr> for ArcString {
    fn from(value: DynStaticStr) -> Self {
        match value {
            DynStaticStr::Static(str) => str.into(),
            DynStaticStr::Dynamic(arc) => arc.into(),
        }
    }
}

pub enum ArcBorrowStr<'a> {
    Arc(Arc<str>),
    Borrow(&'a str),
}

impl<'a> Deref for ArcBorrowStr<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            ArcBorrowStr::Arc(a) => a.deref(),
            ArcBorrowStr::Borrow(b) => b,
        }
    }
}

enum MaybeResolvedType<I, T> {
    Unresolved(I),
    Resolved(T),
}

pub struct MaybeResolved<I, T: Clone>(RwLock<MaybeResolvedType<I, T>>);

impl<I, T: Clone> MaybeResolved<I, T> {
    pub fn new_unresolved(id: I) -> Self {
        Self(RwLock::new(MaybeResolvedType::Unresolved(id)))
    }

    pub fn new_resolved(value: T) -> Self {
        Self(RwLock::new(MaybeResolvedType::Resolved(value)))
    }

    pub fn resolve(&self, resolver: impl FnOnce(&I) -> T) -> T {
        if let MaybeResolvedType::Resolved(value) = self.0.read().deref() {
            return value.clone();
        }

        let mut ty = self.0.write();
        let id = match ty.deref() {
            MaybeResolvedType::Unresolved(id) => id,
            MaybeResolvedType::Resolved(value) => return value.clone(),
        };

        let value = resolver(id);
        *ty = MaybeResolvedType::Resolved(value.clone());
        value
    }
}

pub fn random_u128() -> u128 {
    let mut buf = [0u8; 16];
    getrandom::getrandom(&mut buf).unwrap();
    u128::from_le_bytes(buf)
}
