use std::sync::Weak;

use eframe::epaint::{Color32, Stroke};
use emath::lerp;
use parking_lot::Mutex;

use crate::circuits::props::CircuitProperty;
use crate::containers::FixedVec;
use crate::pool::{PooledColor32Vec, PooledStateVec};
use crate::ui::editor::CircuitBoardEditor;
use crate::vector::Vec2f;
use crate::wires::WireColors;
use crate::{circuits::*, create_safe_prop_enums};

create_safe_prop_enums! {
    #[default(Input)]
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    enum BundlerDirecion {
        Input('i'),
        Output('o'),
    }

    #[default(TopLeft)]
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    enum FirstPinSide {
        TopLeft('t', "Top / Left"),
        BottomRight('b', "Bottom / Right"),
    }

    #[default(TopLeft)]
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    enum BundlePinSide {
        TopLeft('t', "Top / Left"),
        BottomRight('b', "Bottom / Right"),
        Center('c'),
    }
}

thread_local! {
    static INPUT_DISPLAY_NAMES: Mutex<FixedVec<Weak<str>>> = Mutex::new(vec![].into());
    static OUTPUT_DISPLAY_NAMES: Mutex<FixedVec<Weak<str>>> = Mutex::new(vec![].into());
    static IO_NAMES: Mutex<FixedVec<Weak<str>>> = Mutex::new(vec![].into());
}

#[derive(Clone, Copy)]
enum CacheNameType {
    InputDisplay,
    OutputDisplay,
    Internal,
}

struct BundlerProperties {
    dir: Direction4,
    bundle_dir: BundlerDirecion,
    first_side: FirstPinSide,
    bundle_side: BundlePinSide,
    bundle_pos: f32,
    io_offset: u32,
    io_count: u32,
    remaining_pin: bool,
    preceding_pin: bool,
}

struct BundlerVisuals<'a> {
    dir: Direction4,
    first_side: FirstPinSide,
    bundle_side: BundlePinSide,
    bundle_pos: f32,
    io: &'a [Color32],
    bundle: Color32,
    remaining_pin: Option<Color32>,
    preceding_pin: Option<Color32>,
}

struct Bundler {
    dir: Direction4,
    bundle_dir: BundlerDirecion,
    first_side: FirstPinSide,
    bundle_side: BundlePinSide,
    bundle_pos: f32,
    io_offset: u32,

    io: Box<[CircuitPinInfo]>,

    bundle: CircuitPinInfo,
    remaining: Option<CircuitPinInfo>,
    preceding: Option<CircuitPinInfo>,
}

impl Bundler {
    const SLEEVE_THICKNESS: f32 =
        (CircuitBoardEditor::WIRE_POINT_THICKNESS + CircuitBoardEditor::WIRE_THICKNESS) / 2.0;

    const BUNDLER_COLOR: Color32 = Color32::from_gray(16);

    fn new() -> Self {
        let props = Self::default_props();
        let description = Self::describe(&props);

        let bundle = description.pins[0].to_info();
        let pins = &description.pins[1..];
        let (preceding, pins) = match props.preceding_pin {
            true => (Some(pins[0].to_info()), &pins[1..]),
            false => (None, pins),
        };
        let (remaining, pins) = match props.remaining_pin {
            true => (Some(pins[0].to_info()), &pins[1..]),
            false => (None, pins),
        };

        let io = pins.iter().map(|d| d.to_info()).collect();

        Self {
            dir: props.dir,
            bundle_dir: props.bundle_dir,
            first_side: props.first_side,
            bundle_side: props.bundle_side,
            io_offset: props.io_offset,
            bundle_pos: props.bundle_pos,
            io,
            bundle,
            remaining,
            preceding,
        }
    }

    fn draw(vis: BundlerVisuals, ctx: &PaintContext) {
        let size = Self::size(
            vis.preceding_pin.is_some(),
            vis.remaining_pin.is_some(),
            vis.bundle_side,
            vis.io.len() as u32,
        );
        let transformer = |v: Vec2f| {
            let v = v / size.convert(|v| v as f32);
            let v = if vis.dir.is_horizontal() {
                v
            } else {
                [v.y, v.x].into()
            };
            ctx.rect.lerp_inside(v.into())
        };

        let io_x = if vis.dir.is_right_bottom() { 0 } else { 1 };
        let bundle_x = if vis.dir.is_right_bottom() { 1 } else { 0 };
        let io_sleeve_long_x = if vis.dir.is_right_bottom() {
            0.65
        } else {
            1.35
        };
        let io_sleeve_short_x = if vis.dir.is_right_bottom() {
            0.70
        } else {
            1.30
        };
        let body_x = 1.0;

        let io_y = match (vis.first_side, vis.bundle_side) {
            (FirstPinSide::TopLeft, BundlePinSide::TopLeft) => 1,
            (FirstPinSide::TopLeft, _) => 0,
            (FirstPinSide::BottomRight, BundlePinSide::BottomRight) => size.y - 2,
            (FirstPinSide::BottomRight, _) => size.y - 1,
        };
        let io_pos = Vec2u::new(io_x, io_y);

        let preceding_io = if vis.preceding_pin.is_some() { 1 } else { 0 };
        if let Some(preceding_pin) = vis.preceding_pin {
            ctx.paint.line_segment(
                [
                    transformer(io_pos.convert(|v| v as f32 + 0.5)),
                    transformer([body_x, io_y as f32 + 0.5].into()),
                ],
                Stroke::new(
                    CircuitBoardEditor::WIRE_THICKNESS * ctx.screen.scale,
                    preceding_pin,
                ),
            );
            ctx.paint.line_segment(
                [
                    transformer([body_x, io_y as f32 + 0.5].into()),
                    transformer([io_sleeve_long_x, io_y as f32 + 0.5].into()),
                ],
                Stroke::new(
                    Self::SLEEVE_THICKNESS * ctx.screen.scale,
                    Self::BUNDLER_COLOR,
                ),
            );
        }
        if let Some(remaining_pin) = vis.remaining_pin {
            let off = preceding_io + vis.io.len() as u32;
            ctx.paint.line_segment(
                [
                    transformer(Vec2u::new(io_x, io_y + off).convert(|v| v as f32 + 0.5)),
                    transformer([body_x, (io_y + off) as f32 + 0.5].into()),
                ],
                Stroke::new(
                    CircuitBoardEditor::WIRE_THICKNESS * ctx.screen.scale,
                    remaining_pin,
                ),
            );
            ctx.paint.line_segment(
                [
                    transformer([body_x, (io_y + off) as f32 + 0.5].into()),
                    transformer([io_sleeve_long_x, (io_y + off) as f32 + 0.5].into()),
                ],
                Stroke::new(
                    Self::SLEEVE_THICKNESS * ctx.screen.scale,
                    Self::BUNDLER_COLOR,
                ),
            );
        }
        for (i, io) in vis.io.iter().copied().enumerate() {
            let off = preceding_io + i as u32;
            ctx.paint.line_segment(
                [
                    transformer(Vec2u::new(io_x, io_y + off).convert(|v| v as f32 + 0.5)),
                    transformer([body_x, (io_y + off) as f32 + 0.5].into()),
                ],
                Stroke::new(CircuitBoardEditor::WIRE_THICKNESS * ctx.screen.scale, io),
            );
            ctx.paint.line_segment(
                [
                    transformer([body_x, (io_y + off) as f32 + 0.5].into()),
                    transformer([io_sleeve_short_x, (io_y + off) as f32 + 0.5].into()),
                ],
                Stroke::new(
                    Self::SLEEVE_THICKNESS * ctx.screen.scale,
                    Self::BUNDLER_COLOR,
                ),
            );
        }

        let bundle_connect_y = match vis.bundle_side {
            BundlePinSide::TopLeft => 1.0,
            BundlePinSide::BottomRight => size.y as f32 - 1.0,
            BundlePinSide::Center => ((size.y - 1) as f32 * vis.bundle_pos).round() + 0.5,
        };

        let bundle_y = match vis.bundle_side {
            BundlePinSide::TopLeft => 0.5,
            BundlePinSide::BottomRight => size.y as f32 - 0.5,
            BundlePinSide::Center => ((size.y - 1) as f32 * vis.bundle_pos).round() + 0.5,
        };

        let bundle_sleeve_x = lerp(body_x..=bundle_x as f32 + 0.5, 0.65);
        let bundle_sleeve_y = lerp(bundle_connect_y..=bundle_y, 0.65);

        let sleeve_off = 0.077;
        let sleeve_off_x = match vis.dir.is_right_bottom() {
            true => -sleeve_off,
            false => sleeve_off,
        };
        let sleeve_off_y = match vis.bundle_side {
            BundlePinSide::TopLeft => sleeve_off,
            BundlePinSide::BottomRight => -sleeve_off,
            BundlePinSide::Center => 0.0,
        };

        ctx.paint.line_segment(
            [
                transformer([bundle_x as f32 + 0.5, bundle_y].into()),
                transformer([body_x, bundle_connect_y].into()),
            ],
            Stroke::new(
                CircuitBoardEditor::WIRE_THICKNESS * ctx.screen.scale,
                vis.bundle,
            ),
        );
        ctx.paint.line_segment(
            [
                transformer([bundle_sleeve_x, bundle_sleeve_y].into()),
                transformer([body_x + sleeve_off_x, bundle_connect_y + sleeve_off_y].into()),
            ],
            Stroke::new(
                Self::SLEEVE_THICKNESS * ctx.screen.scale,
                Self::BUNDLER_COLOR,
            ),
        );

        let bundle_start_y = match vis.bundle_side {
            BundlePinSide::TopLeft => 0.980,
            _ => 0.5 - CircuitBoardEditor::WIRE_POINT_THICKNESS * 0.5,
        };

        let bundle_end_y = match vis.bundle_side {
            BundlePinSide::BottomRight => size.y as f32 - 1.0,
            _ => size.y as f32 - 0.5 + CircuitBoardEditor::WIRE_POINT_THICKNESS * 0.5,
        };

        ctx.paint.line_segment(
            [
                transformer([body_x, bundle_start_y].into()),
                transformer([body_x, bundle_end_y].into()),
            ],
            Stroke::new(
                CircuitBoardEditor::WIRE_POINT_THICKNESS * ctx.screen.scale,
                Self::BUNDLER_COLOR,
            ),
        );
    }

    fn cached_name(index: usize, ty: CacheNameType) -> Arc<str> {
        let pool = match ty {
            CacheNameType::InputDisplay => &INPUT_DISPLAY_NAMES,
            CacheNameType::OutputDisplay => &OUTPUT_DISPLAY_NAMES,
            CacheNameType::Internal => &IO_NAMES,
        };

        pool.with(|pool| {
            let mut pool = pool.lock();
            if let Some(weak) = pool.get(index) {
                if let Some(arc) = weak.upgrade() {
                    return arc;
                }
            }
            let value = match ty {
                CacheNameType::InputDisplay => format!("In {index}"),
                CacheNameType::OutputDisplay => format!("Out {index}"),
                CacheNameType::Internal => format!("io_{index}"),
            };
            let arc = value.into();
            pool.set(index, Arc::downgrade(&arc));
            arc
        })
    }

    fn size_props(props: &BundlerProperties) -> Vec2u {
        Self::size(
            props.preceding_pin,
            props.remaining_pin,
            props.bundle_side,
            props.io_count,
        )
    }

    fn size(
        preceding_pin: bool,
        remaining_pin: bool,
        bundle_side: BundlePinSide,
        io_count: u32,
    ) -> Vec2u {
        let width = 2;

        let preceding_size = if preceding_pin { 1 } else { 0 };
        let remaining_size = if remaining_pin { 1 } else { 0 };
        let bundle_pin_size = match bundle_side {
            BundlePinSide::TopLeft => 1,
            BundlePinSide::BottomRight => 1,
            BundlePinSide::Center => 0,
        };
        let height = preceding_size + remaining_size + io_count + bundle_pin_size;
        let height = height.max(1);
        [width, height].into()
    }

    fn size_directional(size: Vec2u, dir: Direction4) -> Vec2u {
        if dir.is_horizontal() {
            size
        } else {
            [size.y, size.x].into()
        }
    }

    fn default_props() -> BundlerProperties {
        BundlerProperties {
            dir: Direction4::Right,
            bundle_dir: BundlerDirecion::Input,
            first_side: FirstPinSide::TopLeft,
            bundle_side: BundlePinSide::TopLeft,
            bundle_pos: 0.5,
            io_offset: 0,
            io_count: 3,
            remaining_pin: false,
            preceding_pin: false,
        }
    }

    fn read_props(props: &CircuitPropertyStore) -> BundlerProperties {
        let io_offset = props.read_clone("io_offset").unwrap_or(0);
        BundlerProperties {
            dir: props.read_clone("dir").unwrap_or(Direction4::Right),
            bundle_dir: props
                .read_clone("bundle_dir")
                .unwrap_or(BundlerDirecion::Input),
            first_side: props
                .read_clone("first_side")
                .unwrap_or(FirstPinSide::TopLeft),
            bundle_side: props
                .read_clone("bundle_side")
                .unwrap_or(BundlePinSide::TopLeft),
            bundle_pos: props
                .read_clone("bundle_pos")
                .unwrap_or(props::Slider {
                    value: 0.5f32,
                    range: 0.0..=1.0,
                })
                .value,
            io_offset,
            io_count: props.read_clone("io_count").unwrap_or(3),
            remaining_pin: props.read_clone("remaining_pin").unwrap_or(false),
            preceding_pin: io_offset
                .gt(&0)
                .then(|| props.read_clone("preceding_pin"))
                .flatten()
                .unwrap_or(false),
        }
    }

    fn describe(props: &BundlerProperties) -> DynCircuitDescription {
        let size = Self::size_props(props);

        let io_x = if props.dir.is_right_bottom() { 0 } else { 1 };
        let io_y = match (props.first_side, props.bundle_side) {
            (FirstPinSide::TopLeft, BundlePinSide::TopLeft) => 1,
            (FirstPinSide::TopLeft, _) => 0,
            (FirstPinSide::BottomRight, BundlePinSide::BottomRight) => size.y - 2,
            (FirstPinSide::BottomRight, _) => size.y - 1,
        };
        let io_pos: Vec2u = if props.dir.is_horizontal() {
            [io_x, io_y].into()
        } else {
            [io_y, io_x].into()
        };

        let io_dir = match (props.first_side, props.dir.is_horizontal()) {
            (FirstPinSide::TopLeft, true) => Direction4::Down,
            (FirstPinSide::TopLeft, false) => Direction4::Right,
            (FirstPinSide::BottomRight, true) => Direction4::Up,
            (FirstPinSide::BottomRight, false) => Direction4::Left,
        };

        let preceding_io = if props.preceding_pin { 1 } else { 0 };
        let io_display_pin_dir = props.dir.inverted();
        let io_pin_dir = match props.bundle_dir {
            BundlerDirecion::Input => InternalPinDirection::Inside,
            BundlerDirecion::Output => InternalPinDirection::Outside,
        };

        let io_display_name_key = match props.bundle_dir {
            BundlerDirecion::Input => CacheNameType::InputDisplay,
            BundlerDirecion::Output => CacheNameType::OutputDisplay,
        };

        let bundle_x = if props.dir.is_right_bottom() { 1 } else { 0 };
        let bundle_y = match props.bundle_side {
            BundlePinSide::TopLeft => 0,
            BundlePinSide::BottomRight => size.y - 1,
            BundlePinSide::Center => (props.bundle_pos * (size.y - 1) as f32).round() as u32,
        };

        let bundle_display_dir = match (props.bundle_side, props.dir.is_horizontal()) {
            (BundlePinSide::TopLeft, false) => Direction4::Left,
            (BundlePinSide::TopLeft, true) => Direction4::Up,
            (BundlePinSide::BottomRight, false) => Direction4::Right,
            (BundlePinSide::BottomRight, true) => Direction4::Down,
            (BundlePinSide::Center, _) => props.dir,
        };

        let bundle_pin_dir = match props.bundle_dir {
            BundlerDirecion::Input => InternalPinDirection::Outside,
            BundlerDirecion::Output => InternalPinDirection::Inside,
        };

        let bundle_name = match props.bundle_dir {
            BundlerDirecion::Input => "Bundle Out",
            BundlerDirecion::Output => "Bundle In",
        };

        let bundle_pos = if props.dir.is_horizontal() {
            [bundle_x, bundle_y].into()
        } else {
            [bundle_y, bundle_x].into()
        };

        // out [pre_pin] [rem_pin] io...
        let mut pins = vec![];

        pins.push(CircuitPinDescription {
            active: true,
            display_name: bundle_name.into(),
            display_dir: Some(bundle_display_dir),
            dir: bundle_pin_dir,
            name: "bundle".into(),
            pos: bundle_pos,
        });

        if props.preceding_pin {
            pins.push(CircuitPinDescription {
                active: true,
                display_name: "Preceding".into(),
                display_dir: Some(io_display_pin_dir),
                dir: io_pin_dir,
                name: "pre".into(),
                pos: io_pos,
            });
        }

        if props.remaining_pin {
            let pos = io_dir
                .move_vector(
                    io_pos.convert(|v| v as i32),
                    (props.io_count + preceding_io) as i32,
                )
                .convert(|v| v as u32);
            pins.push(CircuitPinDescription {
                active: true,
                display_name: "Remaining".into(),
                display_dir: Some(io_display_pin_dir),
                dir: io_pin_dir,
                name: "rem".into(),
                pos,
            });
        }

        for i in 0..props.io_count {
            let pos = io_dir
                .move_vector(io_pos.convert(|v| v as i32), (i + preceding_io) as i32)
                .convert(|v| v as u32);
            pins.push(CircuitPinDescription {
                active: true,
                display_name: Self::cached_name(
                    (i + props.io_offset) as usize,
                    io_display_name_key,
                )
                .into(),
                display_dir: Some(io_display_pin_dir),
                dir: io_pin_dir,
                name: Self::cached_name((i + props.io_offset) as usize, CacheNameType::Internal)
                    .into(),
                pos,
            });
        }

        let size_rotated = if props.dir.is_horizontal() {
            size
        } else {
            [size.y, size.x].into()
        };

        DynCircuitDescription {
            size: size_rotated,
            pins: pins.into(),
        }
    }
}

impl CircuitImpl for Bundler {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext) {
        let mut io = PooledColor32Vec::new();
        for pin in self.io.iter() {
            io.push(pin.wire_or_self_color(state_ctx, paint_ctx.style));
        }
        let bundle = self.bundle.wire_or_self_color(state_ctx, paint_ctx.style);
        let remaining_pin = self.remaining.as_ref().map(|p| {
            p.wire_or_self_color(state_ctx, paint_ctx.style)
        });
        let preceding_pin = self.preceding.as_ref().map(|p| {
            p.wire_or_self_color(state_ctx, paint_ctx.style)
        });

        let vis = BundlerVisuals {
            dir: self.dir,
            first_side: self.first_side,
            bundle_side: self.bundle_side,
            bundle_pos: self.bundle_pos,
            io: &io,
            bundle,
            remaining_pin,
            preceding_pin,
        };

        Bundler::draw(vis, paint_ctx);
    }

    fn create_pins(&mut self, circ: &Arc<Circuit>) -> Box<[CircuitPinInfo]> {
        let props = Self::read_props(&circ.props);
        let description = Self::describe(&props);

        let bundle = description.pins[0].to_info();
        let pins = &description.pins[1..];
        let (preceding, pins) = match props.preceding_pin {
            true => (Some(pins[0].to_info()), &pins[1..]),
            false => (None, pins),
        };
        let (remaining, pins) = match props.remaining_pin {
            true => (Some(pins[0].to_info()), &pins[1..]),
            false => (None, pins),
        };

        let io = pins.iter().map(|d| d.to_info()).collect();

        self.preceding = preceding;
        self.remaining = remaining;
        self.io = io;
        self.bundle = bundle;

        let mut vec = vec![self.bundle.clone()];
        if let Some(preceding) = self.preceding.clone() {
            vec.push(preceding);
        }
        if let Some(remaining) = self.remaining.clone() {
            vec.push(remaining);
        }
        vec.extend(self.io.iter().cloned());
        vec.into()
    }

    fn update_signals(&self, state_ctx: &CircuitStateContext, _changed_pin: Option<usize>) {
        match self.bundle_dir {
            BundlerDirecion::Input => {
                let mut out = PooledStateVec::new();
                if let Some(preceding) = &self.preceding {
                    let pre = preceding.get_state(state_ctx);
                    for i in 0..self.io_offset as usize {
                        out.push(pre.get(i));
                    }
                }
                for io in self.io.iter() {
                    out.push(io.get_state(state_ctx));
                }
                if let Some(remaining) = &self.remaining {
                    let rem = remaining.get_state(state_ctx);
                    match rem {
                        WireState::Bundle(bundle) => out.extend_from_slice(&bundle),
                        rem => out.push(rem),
                    }
                }

                self.bundle
                    .set_state(state_ctx, WireState::Bundle(Arc::new(out)));
            }
            BundlerDirecion::Output => {
                let bundle = self.bundle.get_state(state_ctx);
                if let Some(pre) = &self.preceding {
                    let out = match &bundle {
                        WireState::Bundle(bundle) => {
                            let mut out = PooledStateVec::new();
                            for i in 0..self.io_offset as usize {
                                out.push(bundle.get(i).cloned().unwrap_or_default());
                            }
                            WireState::Bundle(Arc::new(out))
                        }
                        bundle => bundle.clone(),
                    };

                    pre.set_state(state_ctx, out);
                }
                if let Some(rem) = &self.remaining {
                    let out = match &bundle {
                        WireState::Bundle(bundle) => {
                            let start = self.io_offset as usize + self.io.len();
                            let mut out = PooledStateVec::new();
                            if bundle.len() > start {
                                out.extend_from_slice(&bundle[start..]);
                            }
                            WireState::Bundle(Arc::new(out))
                        }
                        bundle => bundle.clone(),
                    };

                    rem.set_state(state_ctx, out);
                }

                for (i, io) in self.io.iter().enumerate() {
                    io.set_state(state_ctx, bundle.get(i + self.io_offset as usize));
                }
            }
        }
    }

    fn size(&self, circ: &Arc<Circuit>) -> Vec2u {
        let props = Self::read_props(&circ.props);
        Self::size_directional(Self::size_props(&props), props.dir)
    }

    fn prop_changed(&self, prop_id: &str, resize: &mut bool, recreate_pins: &mut bool) {
        match prop_id {
            "dir" => {
                *resize = true;
                *recreate_pins = true;
            }
            "bundle_dir" => {
                *resize = true;
                *recreate_pins = true;
            }
            "first_side" => {
                *recreate_pins = true;
            }
            "bundle_side" => {
                *resize = true;
                *recreate_pins = true;
            }
            "bundle_pos" => {
                *recreate_pins = true;
            }
            "io_offset" => {
                *recreate_pins = true;
            }
            "io_count" => {
                *resize = true;
                *recreate_pins = true;
            }
            "remaining_pin" => {
                *resize = true;
                *recreate_pins = true;
            }
            "preceding_pin" => {
                *resize = true;
                *recreate_pins = true;
            }
            _ => {}
        }
    }

    fn apply_props(&mut self, circ: &Arc<Circuit>, _: Option<&str>) {
        let props = Self::read_props(&circ.props);

        self.dir = props.dir;
        self.bundle_dir = props.bundle_dir;
        self.first_side = props.first_side;
        self.bundle_side = props.bundle_side;
        self.bundle_pos = props.bundle_pos;
        self.io_offset = props.io_offset;
    }
}

pub struct Preview {}

impl CircuitPreviewImpl for Preview {
    fn type_name(&self) -> DynStaticStr {
        "bundler".into()
    }

    fn display_name(&self) -> DynStaticStr {
        "Bundler".into()
    }

    fn description(&self) -> DynStaticStr {
        "Splitter/merger for wire bundles".into()
    }

    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, _in_world: bool) {
        let props = Bundler::read_props(props);
        let mut io = PooledColor32Vec::new();
        let false_color = ctx.style.wire_colors.false_color();
        io.extend((0..props.io_count).map(|_| false_color));
        let vis = BundlerVisuals {
            dir: props.dir,
            first_side: props.first_side,
            bundle_side: props.bundle_side,
            bundle_pos: props.bundle_pos,
            io: &io,
            bundle: WireColors::BUNDLE,
            remaining_pin: props.remaining_pin.then_some(WireColors::BUNDLE),
            preceding_pin: props.preceding_pin.then_some(WireColors::BUNDLE),
        };
        Bundler::draw(vis, ctx);
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Bundler::new())
    }

    fn load_copy_data(
        &self,
        _imp: &serde_intermediate::Intermediate,
        _internal: &serde_intermediate::Intermediate,
        _ctx: &Arc<SimulationContext>,
        _errors: &mut ErrorList,
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        Some(Box::new(Preview {}))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        CircuitPropertyStore::new([
            CircuitProperty::new("dir", "Direction", Direction4::Right),
            CircuitProperty::new("bundle_dir", "Bundle direction", BundlerDirecion::Input),
            CircuitProperty::new("first_side", "First pin side", FirstPinSide::TopLeft),
            CircuitProperty::new("bundle_side", "Bundle side", BundlePinSide::TopLeft),
            CircuitProperty::new(
                "bundle_pos",
                "Bundle position",
                props::Slider {
                    value: 0.5f32,
                    range: 0.0..=1.0,
                },
            ),
            CircuitProperty::new("io_offset", "IO offset", 0u32),
            CircuitProperty::new("io_count", "IO count", 3u32),
            CircuitProperty::new("remaining_pin", "Remaining pin", false),
            CircuitProperty::new("preceding_pin", "Preceding pin", false),
        ])
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        Bundler::describe(&Bundler::read_props(props))
    }
}
