#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
#![feature(int_roundings)]

use std::{
    mem::size_of,
    ops::Range,
    rc::Rc,
    sync::{Arc, Mutex, RwLock},
    time::Instant,
};

use eframe::{
    egui::{self, Context, Frame, Key, Margin, Sense, TextStyle, Ui},
    epaint::{Color32, Stroke},
};
use emath::{pos2, vec2, Align2, Pos2, Rect};

mod r#const;

mod vector;
use vector::{Vec2f, Vec2i, Vector};
use wires::Wire;

mod containers;
use crate::containers::*;

mod circuits;
use circuits::{CircuitPreview, Circuits, PinDirection, Circuit};

mod wires;
use crate::wires::Wires;

fn main() {
    eframe::run_native(
        "rls",
        eframe::NativeOptions::default(),
        Box::new(|_| Box::new(App::new())),
    )
    .unwrap();
}

#[allow(unused)]
#[derive(Debug, Clone, Copy)]
struct TileDrawBounds {
    pub tile_size: Vec2f,
    pub chunk_size: Vec2f,

    pub screen_tl: Vec2f,
    pub screen_br: Vec2f,

    pub tiles_tl: Vec2i,
    pub tiles_br: Vec2i,

    pub chunks_tl: Vec2i,
    pub chunks_br: Vec2i,
}

pub struct PaintContext<'a> {
    screen: Screen,
    paint: &'a egui::Painter,
    rect: Rect,
    bounds: TileDrawBounds,
    ui: Rc<&'a mut Ui>,
    egui_ctx: &'a Context,
}

impl PaintContext<'_> {
    fn with_rect<'a>(&'a self, rect: Rect) -> PaintContext<'a> {
        Self {
            rect,
            ui: self.ui.clone(),
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
            tile_size: _,
            chunk_size: _,
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
                let chunk =
                    match chunks.get_chunk(chunk_coord.x() as isize, chunk_coord.y() as isize) {
                        Some(v) => v,
                        None => continue,
                    };

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
                            + screen.world_to_screen(pos.convert_values(|v| v as f32));
                        let rect =
                            Rect::from_min_size(draw_pos.into(), vec2(screen.scale, screen.scale));
                        let lookaround = ChunksLookaround::new(
                            chunks,
                            chunk,
                            pos.convert_values(|v| v as isize),
                            [i as usize, j as usize].into(),
                        );

                        let drawer_ctx = Self {
                            rect,
                            ui: self.ui.clone(),
                            ..*self
                        };

                        drawer(tile, pos, &drawer_ctx, pass, &lookaround)
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct PanAndZoom {
    pos: Vec2f,
    scale: f32,
}

impl PanAndZoom {
    fn update(&mut self, ui: &egui::Ui, rect: Rect) {
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

        if interaction.dragged_by(egui::PointerButton::Secondary) {
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

    pub fn to_screen(&self, offset: Vec2f) -> Screen {
        Screen {
            offset,
            pos: self.pos,
            scale: self.scale,
        }
    }
}

#[derive(Clone, Copy)]
struct Screen {
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
        self.screen_to_world(v).convert_values(|v| v.floor() as i32)
    }

    pub fn world_to_screen_tile(&self, v: Vec2i) -> Vec2f {
        self.world_to_screen(v.convert_values(|v| v as f32))
    }
}

#[derive(Debug)]
enum SelectedItem {
    Wires,
    Circuit(Box<dyn CircuitPreview>),
}

struct App {
    last_win_pos: Option<Pos2>,

    pub pan_zoom: PanAndZoom,
    pub wires: Wires,
    pub circuits: Circuits,
    pub selected: SelectedItem,

    pub state: Arc<State>,
}

impl eframe::App for App {
    fn update(&mut self, ctx: &eframe::egui::Context, frame: &mut eframe::Frame) {
        let start_time = Instant::now();
        let int_info = frame.info();
        if let Some(win_pos) = int_info.window_info.position {
            if let Some(last_win_pos) = self.last_win_pos {
                if win_pos != last_win_pos {
                    let diff: Vec2f = (win_pos - last_win_pos).into();
                    self.pan_zoom.pos += diff / self.pan_zoom.scale;
                }
            }
        }
        self.last_win_pos = int_info.window_info.position;

        self.state.update(&self.wires, &self.circuits);

        if ctx.input(|input| input.key_pressed(Key::F1)) {
            self.selected = match self.selected {
                SelectedItem::Wires => {
                    SelectedItem::Circuit(Box::new(circuits::TestCircuitPreview { a: 10, b: 20 }))
                }
                SelectedItem::Circuit(_) => SelectedItem::Wires,
            }
        }

        egui::CentralPanel::default()
            .frame(Frame::central_panel(ctx.style().as_ref()).inner_margin(Margin::same(0.0)))
            .show(ctx, |ui| {
                let rect = ui.max_rect();

                self.pan_zoom.update(ui, rect);

                let paint = ui.painter_at(rect);

                let font_id = TextStyle::Monospace.resolve(ui.style());

                let mut grid_ds_cell_size = self.pan_zoom.scale;

                while grid_ds_cell_size < 6.0 {
                    grid_ds_cell_size *= 16.0;
                }

                App::draw_grid(
                    self.pan_zoom.pos * self.pan_zoom.scale / grid_ds_cell_size,
                    grid_ds_cell_size.into(),
                    16.into(),
                    rect,
                    &paint,
                );

                self.draw_cross(rect, &paint);

                let bounds = self.calc_draw_bounds(rect);
                let ctx = PaintContext {
                    screen: self.pan_zoom.to_screen(rect.left_top().into()),
                    paint: &paint,
                    rect,
                    bounds,
                    ui: Rc::new(ui),
                    egui_ctx: ctx,
                };

                self.wires.update(
                    &self.state,
                    &mut self.circuits,
                    &ctx,
                    matches!(self.selected, SelectedItem::Wires),
                );
                self.circuits.update(
                    &self.state,
                    &mut self.wires,
                    &ctx,
                    match &self.selected {
                        SelectedItem::Wires => None,
                        SelectedItem::Circuit(p) => Some(&p),
                    },
                );

                let update_time = Instant::now() - start_time;

                paint.text(
                    rect.left_top() + vec2(10.0, 10.0),
                    Align2::LEFT_TOP,
                    format!(
                        r#"Pos: {}
Tile draw bounds: {} - {}
Chunk draw bounds: {} - {}
Wires drawn: {}
Time: {:.4} ms
Sizes: 
  wires.nodes: {},
Selected: {:?}
"#,
                        self.pan_zoom.pos,
                        bounds.tiles_tl,
                        bounds.tiles_br,
                        bounds.chunks_tl,
                        bounds.chunks_br,
                        *self.wires.parts_drawn.read().unwrap(),
                        update_time.as_secs_f64() * 1000.0,
                        format_size(self.wires.nodes.calc_size_outer()),
                        self.selected
                    ),
                    font_id,
                    Color32::WHITE,
                );
            });
    }
}

impl App {
    fn new() -> Self {
        Self {
            pan_zoom: PanAndZoom::new(0.0.into(), 16.0),
            wires: Wires::new(),
            circuits: Circuits::new(),
            last_win_pos: None,
            selected: SelectedItem::Wires,
            state: Default::default(),
        }
    }

    fn draw_grid(
        pos: Vec2f,
        cell_size: Vec2f,
        mid_lines: Vector<2, u32>,
        rect: emath::Rect,
        paint: &egui::Painter,
    ) {
        let pos = pos * cell_size;
        let visible_cells = (Vec2f::from(rect.size()) / cell_size).convert_values(|v| v as i32 + 2);
        let start = (pos / cell_size).convert_values(|v| v as i32);
        let off = pos % cell_size;

        let dim_stroke = Stroke::new(1.0, Color32::from_gray(64));
        let mid_stroke = Stroke::new(1.5, Color32::from_gray(96));

        for i in 0..visible_cells.x() {
            let x = i + start.x();
            if mid_lines.x() > 0 && x % mid_lines.x() as i32 == 0 {
                continue;
            }

            let pos = rect.left() + cell_size.x() * i as f32 - off.x();
            paint.line_segment(
                [pos2(pos, rect.top()), pos2(pos, rect.bottom())],
                dim_stroke,
            );
        }

        for i in 0..visible_cells.y() {
            let y = i + start.y();
            if mid_lines.y() > 0 && y % mid_lines.y() as i32 == 0 {
                continue;
            }

            let pos = rect.top() + cell_size.y() * i as f32 - off.y();
            paint.line_segment(
                [pos2(rect.left(), pos), pos2(rect.right(), pos)],
                dim_stroke,
            );
        }

        let mid_cells =
            visible_cells.combine_with(mid_lines, |v, m| if m == 0 { 0 } else { v / m as i32 + 2 });
        let mid_off = pos % (cell_size * mid_lines.convert_values(|v| v as f32));

        for i in 0..mid_cells.x() {
            let pos = rect.left() + cell_size.x() * i as f32 * mid_lines.x() as f32 - mid_off.x();
            paint.line_segment(
                [pos2(pos, rect.top()), pos2(pos, rect.bottom())],
                mid_stroke,
            );
        }

        for i in 0..mid_cells.y() {
            let pos = rect.top() + cell_size.y() * i as f32 * mid_lines.y() as f32 - mid_off.y();
            paint.line_segment(
                [pos2(rect.left(), pos), pos2(rect.right(), pos)],
                mid_stroke,
            );
        }

        if start.x() <= 0 && visible_cells.x() + start.x() >= 0 {
            let pos = rect.left() + cell_size.x() * -start.x() as f32 - off.x();
            paint.line_segment(
                [pos2(pos, rect.top()), pos2(pos, rect.bottom())],
                Stroke::new(1.0, Color32::GREEN),
            );
        }

        if start.y() <= 0 && visible_cells.y() + start.y() >= 0 {
            let pos = rect.top() + cell_size.y() * -start.y() as f32 - off.y();
            paint.line_segment(
                [pos2(rect.left(), pos), pos2(rect.right(), pos)],
                Stroke::new(1.0, Color32::RED),
            );
        }
    }

    fn draw_cross(&mut self, bounds: Rect, paint: &egui::Painter) {
        let mut cross_pos = self
            .pan_zoom
            .to_screen(bounds.left_top().into())
            .world_to_screen(0.0.into());

        *cross_pos.x_mut() = cross_pos.x().clamp(bounds.left(), bounds.right());
        *cross_pos.y_mut() = cross_pos.y().clamp(bounds.top(), bounds.bottom());

        let unit = Vec2f::single_value(self.pan_zoom.scale);

        let cross_stroke = Stroke::new(2.0, Color32::WHITE);

        paint.line_segment(
            [
                pos2(cross_pos.x() - unit.x(), cross_pos.y()),
                pos2(cross_pos.x() + unit.x(), cross_pos.y()),
            ],
            cross_stroke,
        );
        paint.line_segment(
            [
                pos2(cross_pos.x(), cross_pos.y() - unit.y()),
                pos2(cross_pos.x(), cross_pos.y() + unit.y()),
            ],
            cross_stroke,
        );
    }

    fn calc_draw_bounds(&self, rect: Rect) -> TileDrawBounds {
        let screen = &self.pan_zoom;
        let tile_size: Vec2f = screen.scale.into();
        let chunk_size: Vec2f = (screen.scale * 16.0).into();

        let screen_tl = screen.pos * screen.scale;
        let screen_br = screen_tl + rect.size();

        TileDrawBounds {
            tile_size,
            chunk_size,

            screen_tl,
            screen_br,

            tiles_tl: (screen_tl / screen.scale).convert_values(|v| v.floor() as i32),
            tiles_br: (screen_br / screen.scale).convert_values(|v| v.floor() as i32),

            chunks_tl: (screen_tl / chunk_size).convert_values(|v| v.floor() as i32),
            chunks_br: (screen_br / chunk_size).convert_values(|v| v.floor() as i32),
        }
    }
}
trait SizeCalc
where
    Self: Sized,
{
    fn calc_size_outer(&self) -> usize {
        self.calc_size_inner() + size_of::<Self>()
    }
    fn calc_size_inner(&self) -> usize;
}

macro_rules! impl_empty_inner_size {
    ($($t:ty),+) => {
        $(impl SizeCalc for $t {
            fn calc_size_inner(&self) -> usize {
                0
            }
        })+
    };
}

impl_empty_inner_size!(u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, usize, isize, f32, f64);

impl<T: SizeCalc> SizeCalc for &[T] {
    fn calc_size_inner(&self) -> usize {
        self.iter()
            .map(|i| i.calc_size_outer())
            .fold(0, |a, b| a + b)
    }
}

impl<T: SizeCalc> SizeCalc for Vec<T> {
    fn calc_size_inner(&self) -> usize {
        self.iter()
            .map(|i| i.calc_size_outer())
            .fold(0, |a, b| a + b)
    }
}

impl<const SIZE: usize, T: SizeCalc> SizeCalc for [T; SIZE] {
    fn calc_size_inner(&self) -> usize {
        self.iter()
            .map(|i| i.calc_size_inner())
            .fold(0, |a, b| a + b)
    }
}

impl<T: SizeCalc> SizeCalc for Option<T> {
    fn calc_size_inner(&self) -> usize {
        match self {
            None => 0,
            Some(v) => v.calc_size_inner(),
        }
    }
}

impl<T: SizeCalc> SizeCalc for Box<T> {
    fn calc_size_inner(&self) -> usize {
        let t = self.as_ref();
        t.calc_size_outer()
    }
}

fn format_size(size: usize) -> String {
    const SIZE_LABELS: &[&str] = &["B", "kB", "MB", "GB", "TB"];

    let mut div = 1usize;
    let mut label = 0;
    for _ in 0..SIZE_LABELS.len() {
        let (next_div, overflow) = div.overflowing_shl(10);
        if overflow {
            break;
        }
        if size >= next_div {
            label += 1;
            div = next_div;
        } else {
            break;
        }
    }

    let valf = size as f64 / div as f64;
    let ff = format!("{valf:.2}");
    let ff = ff.trim_end_matches('0').trim_end_matches('.');
    format!("{ff} {}", SIZE_LABELS[label])
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

#[derive(Clone, Copy, Debug)]
struct OptionalInt<T: Integer>(T);

impl<T: Integer> Default for OptionalInt<T> {
    fn default() -> Self {
        Self(Self::NONE_VALUE)
    }
}

#[allow(unused)]
impl<T: Integer> OptionalInt<T> {
    const NONE_VALUE: T = if T::SIGNED { T::MIN } else { T::MAX };

    fn is_none(&self) -> bool {
        self.0 == Self::NONE_VALUE
    }

    fn is_some(&self) -> bool {
        self.0 != Self::NONE_VALUE
    }

    fn is_some_and(&self, f: impl FnOnce(T) -> bool) -> bool {
        self.0 != Self::NONE_VALUE && f(self.0)
    }

    fn is_none_or(&self, f: impl FnOnce(T) -> bool) -> bool {
        self.0 == Self::NONE_VALUE || f(self.0)
    }

    fn get(&self) -> Option<T> {
        if self.0 == Self::NONE_VALUE {
            None
        } else {
            Some(self.0)
        }
    }

    fn set(&mut self, value: Option<T>) {
        self.0 = match value {
            None => Self::NONE_VALUE,
            Some(v) => v,
        }
    }
}

pub trait Integer: Eq + Copy {
    const SIGNED: bool;
    const MAX: Self;
    const MIN: Self;
}

macro_rules! impl_integer_trait {
    (signed $($t:ty),+) => {
        $(impl crate::Integer for $t {
            const SIGNED: bool = true;
            const MIN: $t = <$t>::MIN;
            const MAX: $t = <$t>::MAX;
        })+
    };
    (unsigned $($t:ty),+) => {
        $(impl crate::Integer for $t {
            const SIGNED: bool = false;
            const MIN: $t = <$t>::MIN;
            const MAX: $t = <$t>::MAX;
        })+
    };
}

impl_integer_trait!(signed i8, i16, i32, i64, i128, isize);
impl_integer_trait!(unsigned u8, u16, u32, u64, u128, usize);

pub enum UpdateTask {
    UpdateCircuit { id: usize, pin: Option<usize> },
    UpdateWireState(usize),
}

#[derive(Default, Clone, Copy, Eq, PartialEq)]
pub enum WireState {
    #[default]
    None,
    True,
    False,
    Error,
}

impl WireState {
    pub fn combine(&self, state: WireState) -> WireState {
        match (*self, state) {
            (WireState::None, other) => other,
            (other, WireState::None) => other,
            (WireState::Error, _) => WireState::Error,
            (_, WireState::Error) => WireState::Error,
            (WireState::True, WireState::False) => WireState::Error,
            (WireState::False, WireState::True) => WireState::Error,

            (WireState::True, WireState::True) => WireState::True,
            (WireState::False, WireState::False) => WireState::True,
        }
    }

    pub fn color(&self) -> Color32 {
        let rgb = match self {
            Self::None => [0, 0, 200],
            Self::True => [0, 255, 0],
            Self::False => [0, 127, 200],
            Self::Error => [200, 0, 0],
        };
        Color32::from_rgb(rgb[0], rgb[1], rgb[2])
    }
}

#[derive(Default)]
pub struct CircuitState {
    pub pins: FixedVec<WireState>,
}

pub struct State {
    pub wires: Arc<RwLock<FixedVec<Arc<RwLock<WireState>>>>>,
    pub circuits: Arc<RwLock<FixedVec<Arc<RwLock<CircuitState>>>>>,

    pub queue: Arc<Mutex<RandomQueue<UpdateTask>>>,
}

impl Default for State {
    fn default() -> Self {
        Self {
            wires: Arc::new(RwLock::new(vec![].into())),
            circuits: Arc::new(RwLock::new(vec![].into())),
            queue: Arc::new(Mutex::new(RandomQueue::new())),
        }
    }
}

impl State {
    fn read_wire(&self, id: usize) -> WireState {
        self.wires
            .read()
            .unwrap()
            .get(id)
            .map(|s| *s.read().unwrap())
            .unwrap_or_default()
    }

    fn get_wire(&self, id: usize) -> Arc<RwLock<WireState>> {
        self.wires
            .write()
            .unwrap()
            .get_or_create_mut(id, || Default::default())
            .clone()
    }

    fn read_circuit(&self, id: usize) -> Option<Arc<RwLock<CircuitState>>> {
        self.circuits
            .read()
            .unwrap()
            .get(id)
            .map(|s| s.clone())
    }

    fn get_circuit(&self, id: usize) -> Arc<RwLock<CircuitState>> {
        self.circuits
            .write()
            .unwrap()
            .get_or_create_mut(id, || Default::default())
            .clone()
    }

    fn update(&self, wires: &Wires, circuits: &Circuits) {
        let mut limit = 10;
        while limit > 0 {
            limit -= 1;
            let task = match self.queue.lock().unwrap().dequeue() {
                None => break,
                Some(t) => t,
            };

            match task {
                UpdateTask::UpdateWireState(wire) => {
                    if let Some(wire) = wires.wires.get(wire) {
                        self.update_wire(wire);
                    }
                },
                UpdateTask::UpdateCircuit { id, pin } => {
                    if let Some(circuit) = circuits.cirtuits.get(id) {
                        self.update_circuit(circuit, pin);
                    }
                },
            }
        }
    }

    fn update_wire(&self, wire: &Wire) {
        
        let mut state = WireState::None;
        for (_, point) in wire.nodes.iter() {
            if let Some(pin) = &point.pin {
                let pin = pin.read().unwrap();
                if let PinDirection::Outside = pin.dir {
                    state = state.combine(pin.get_state(self))
                }
            }
        }

        let current = self.get_wire(wire.id);
        if *current.read().unwrap() == state {
            return;
        }

        *current.write().unwrap() = state;
        for (_, point) in wire.nodes.iter() {
            if let Some(pin) = &point.pin {
                let pin = pin.read().unwrap();
                if let PinDirection::Inside = pin.dir {
                    self.queue.lock().unwrap().enqueue(UpdateTask::UpdateCircuit { id: pin.id.circuit_id, pin: Some(pin.id.id) })
                }
            }
        }
    }

    fn update_circuit(&self, circuit: &Circuit, pin: Option<usize>) {
        circuit.imp.write().unwrap().update_signals(self, &mut *self.get_circuit(circuit.id).write().unwrap(), pin)
    }
}
