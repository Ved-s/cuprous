use eframe::{
    egui::{CursorIcon, Sense},
    epaint::{Color32, Rounding, Stroke},
};
use emath::{pos2, vec2};

use crate::{error::ResultReport, unwrap_option_or_return, Direction4};

use super::{
    props::{CircuitProperty, RangedValue},
    *,
};

struct LedVisuals {
    stroke: Stroke,
    off_color: Color32,
    on_color: Color32,
    rounding: Rounding,
}

struct Led {
    in_pin: CircuitPinInfo,
    out_pin: Option<CircuitPinInfo>,
    size: Vec2u,
    visuals: LedVisuals,
}

impl Led {
    fn new() -> Self {
        let description = Self::describe(Direction4::Left, 3, 3, false);
        Self {
            in_pin: description.pins[0].to_info(),
            out_pin: description.pins[1].to_active_info(),
            size: 3.into(),
            visuals: LedVisuals {
                on_color: Color32::WHITE,
                off_color: Color32::GRAY.linear_multiply(0.3),
                rounding: Rounding::same(0.75),
                stroke: Stroke::NONE,
            },
        }
    }

    fn draw_base(ctx: &PaintContext, semi_transparent: bool) {
        let color_mul = if semi_transparent { 0.5 } else { 1.0 };
        ctx.paint.rect_filled(
            ctx.rect.expand(ctx.screen.scale * -0.5),
            Rounding::same(ctx.screen.scale * 0.25),
            Color32::from_gray(100).linear_multiply(color_mul),
        );
    }

    fn draw_led(visuals: &LedVisuals, ctx: &PaintContext, on: bool, semi_transparent: bool) {
        let color_mul = if semi_transparent { 0.5 } else { 1.0 };
        let color = if on {
            visuals.on_color
        } else {
            visuals.off_color
        }
        .linear_multiply(color_mul);

        let rounding = Rounding {
            nw: visuals.rounding.nw * ctx.screen.scale,
            ne: visuals.rounding.ne * ctx.screen.scale,
            sw: visuals.rounding.sw * ctx.screen.scale,
            se: visuals.rounding.se * ctx.screen.scale,
        };

        let stroke = Stroke {
            width: visuals.stroke.width * ctx.screen.scale,
            color: visuals.stroke.color,
        };

        ctx.paint.rect(ctx.rect, rounding, color, stroke);
    }

    fn describe_props(props: &CircuitPropertyStore) -> CircuitDescription<2> {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Right);
        let out = props.read_clone("out").unwrap_or(false);
        let width = props
            .read("width", |v: &RangedValue<u32>| v.get())
            .unwrap_or(3);
        let height = props
            .read("height", |v: &RangedValue<u32>| v.get())
            .unwrap_or(3);
        Self::describe(dir, width, height, out)
    }

    fn describe(dir: Direction4, width: u32, height: u32, out: bool) -> CircuitDescription<2> {
        let in_pin_pos = match dir {
            Direction4::Up => [width / 2, 0],
            Direction4::Left => [0, height / 2],
            Direction4::Down => [width / 2, height - 1],
            Direction4::Right => [width - 1, height / 2],
        };
        let out_pin_pos = [width - 1 - in_pin_pos[0], height - 1 - in_pin_pos[1]];

        crate::circuits::CircuitDescription {
            size: [width, height].into(),
            pins: [
                crate::circuits::CircuitPinDescription {
                    active: true,
                    name: "in".into(),
                    dir: InternalPinDirection::Inside,
                    display_name: "In".into(),
                    display_dir: Some(dir),
                    pos: in_pin_pos.into(),
                },
                crate::circuits::CircuitPinDescription {
                    active: out,
                    name: "out".into(),
                    dir: InternalPinDirection::Outside,
                    display_name: "Out".into(),
                    display_dir: Some(dir.inverted()),
                    pos: out_pin_pos.into(),
                },
            ],
        }
    }
}

impl CircuitImpl for Led {
    fn draw(&self, _: &CircuitStateContext, paint_ctx: &PaintContext) {
        Self::draw_base(paint_ctx, false);
    }

    fn control_count(&self, _: &Arc<Circuit>) -> Option<usize> {
        Some(1)
    }

    fn control_info(&self, circuit: &Arc<Circuit>, id: usize) -> Option<CircuitControlInfo> {
        let width = circuit
            .props
            .read("width", |v: &RangedValue<u32>| v.get())
            .unwrap_or(3);
        let height = circuit
            .props
            .read("height", |v: &RangedValue<u32>| v.get())
            .unwrap_or(3);

        match id {
            0 => Some(CircuitControlInfo {
                rect: Rect::from_min_size(
                    pos2(0.75, 0.75),
                    vec2(width as f32 - 1.5, height as f32 - 1.5),
                ),
                display_name: circuit
                    .name()
                    .map(|arc| arc.into())
                    .unwrap_or_else(|| "Led".into()),
            }),
            _ => None,
        }
    }

    fn update_control(
        &self,
        id: usize,
        _: &Arc<Circuit>,
        state: Option<&CircuitStateContext>,
        ctx: &PaintContext,
        interactive: bool,
        uid: Id,
    ) {
        if id != 0 {
            return;
        }

        let led_state = state
            .and_then(|s| s.read_circuit_internal_state(|s: &LedState| s.clone()))
            .unwrap_or_default();

        Self::draw_led(&self.visuals, ctx, led_state.led, false);
        if !interactive {
            return;
        }

        let out_pin = unwrap_option_or_return!(&self.out_pin);
        let state = unwrap_option_or_return!(state);

        let id = ctx.ui.auto_id_with(uid);

        let interaction = ctx.ui.interact(ctx.rect, id, Sense::click());
        if interaction.hovered() {
            ctx.ui.ctx().set_cursor_icon(CursorIcon::PointingHand);
        }

        let new_state = ctx.ui.input(|input| {
            input
                .pointer
                .latest_pos()
                .is_some_and(|p| ctx.rect.contains(p))
                && input.pointer.any_down()
        });

        if led_state.click != new_state {
            state.write_circuit_internal_state(|s: &mut LedState| s.click = new_state);
            out_pin.set_state(state, new_state.into());
        }
    }

    fn prop_changed(&self, prop_id: &str, resize: &mut bool, recreate_pins: &mut bool) {
        if prop_id == "dir" || prop_id == "out" {
            *recreate_pins = true;
        }
        if prop_id == "width" || prop_id == "height" {
            *recreate_pins = true;
            *resize = true;
        }
    }

    fn apply_props(&mut self, circ: &Arc<Circuit>, changed: Option<&str>) {
        if matches!(changed, None | Some("stroke")) {
            if let Some(stroke) = circ.props.read_clone("stroke") {
                self.visuals.stroke = stroke;
            }
        }
        if matches!(changed, None | Some("on_color")) {
            if let Some(on_color) = circ.props.read_clone("on_color") {
                self.visuals.on_color = on_color;
            }
        }
        if matches!(changed, None | Some("off_color")) {
            if let Some(off_color) = circ.props.read_clone("off_color") {
                self.visuals.off_color = off_color;
            }
        }
        if matches!(changed, None | Some("width")) {
            if let Some(value) = circ.props.read("width", |v: &RangedValue<u32>| v.get()) {
                self.size.x = value;
            }
        }
        if matches!(changed, None | Some("height")) {
            if let Some(value) = circ.props.read("height", |v: &RangedValue<u32>| v.get()) {
                self.size.y = value;
            }
        }
        if matches!(changed, None | Some("rounding")) {
            if let Some(value) = circ.props.read_clone("rounding") {
                self.visuals.rounding = value;
            }
        }
    }

    fn create_pins(&mut self, circ: &Arc<Circuit>) -> Box<[CircuitPinInfo]> {
        let description = Self::describe_props(&circ.props);
        self.in_pin = description.pins[0].to_info();
        self.out_pin = description.pins[1].to_active_info();

        let mut vec = vec![self.in_pin.clone()];
        if let Some(out_pin) = self.out_pin.clone() {
            vec.push(out_pin);
        }
        vec.into_boxed_slice()
    }

    fn update_signals(&self, state_ctx: &CircuitStateContext, changed_pin: Option<usize>) {
        if matches!(changed_pin, None | Some(0)) {
            let state = self.in_pin.get_state(state_ctx) == WireState::True;
            state_ctx.write_circuit_internal_state(|s: &mut LedState| s.led = state);
        }
        if matches!(changed_pin, None | Some(1)) {
            if let Some(pin) = self.out_pin.as_ref() {
                let state = state_ctx
                    .read_circuit_internal_state(|s: &LedState| s.click)
                    .unwrap_or(false);
                pin.set_state(state_ctx, state.into())
            }
        }
    }

    fn load_internal(
        &self,
        _ctx: &CircuitStateContext,
        data: &serde_intermediate::Intermediate,
        _paste: bool,
        errors: &mut ErrorList,
    ) -> Option<Box<dyn InternalCircuitState>> {
        serde_intermediate::de::intermediate::deserialize::<LedState>(data)
            .report_error(errors)
            .map(|s| Box::new(s) as Box<dyn InternalCircuitState>)
    }

    fn size(&self, circ: &Arc<Circuit>) -> Vec2u {
        Self::describe_props(&circ.props).size
    }
}

#[derive(Default, Clone, Serialize, Deserialize)]
struct LedState {
    led: bool,
    click: bool,
}

impl InternalCircuitState for LedState {
    fn serialize(&self, _: bool) -> serde_intermediate::Intermediate {
        serde_intermediate::to_intermediate(self).unwrap_or_default()
    }
}

#[derive(Debug)]
pub struct LedPreview {}

impl CircuitPreviewImpl for LedPreview {
    fn type_name(&self) -> DynStaticStr {
        "led".into()
    }

    fn display_name(&self) -> DynStaticStr {
        "LED".into()
    }

    fn description(&self) -> DynStaticStr {
        "A LED that turns on and off depending on the input.\n\
         Can be clickable.\n\
         Size and appearance can be changed.\n\
         \n\
         Can be exposed in a circuit design.\
        "
        .into()
    }

    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, in_world: bool) {
        Led::draw_base(ctx, in_world);
        let led_ctx = ctx.with_rect(ctx.rect.shrink(ctx.screen.scale * 0.75));
        let on_color = props.read_clone("on_color").unwrap_or(Color32::WHITE);

        let rounding = props.read_clone("rounding").unwrap_or(Rounding::same(0.75));
        let stroke = props.read_clone("stroke").unwrap_or(Stroke::NONE);

        let visuals = LedVisuals {
            on_color,
            off_color: Color32::TRANSPARENT,
            stroke,
            rounding,
        };

        Led::draw_led(&visuals, &led_ctx, true, in_world);
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Led::new())
    }

    fn load_copy_data(
        &self,
        _imp: &serde_intermediate::Intermediate,
        _internal: &serde_intermediate::Intermediate,
        _ctx: &Arc<SimulationContext>,
        _errors: &mut ErrorList,
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        Some(Box::new(LedPreview {}))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        CircuitPropertyStore::new([
            CircuitProperty::new("dir", "Direction", Direction4::Left),
            CircuitProperty::new("on_color", "ON color", Color32::WHITE),
            CircuitProperty::new("off_color", "OFF color", Color32::GRAY.linear_multiply(0.3)),
            CircuitProperty::new("out", "Clickable", false),
            CircuitProperty::new("stroke", "Stroke", Stroke::NONE),
            CircuitProperty::new("width", "Width", RangedValue::new(3..=u32::MAX, 1, 3)),
            CircuitProperty::new("height", "Height", RangedValue::new(3..=u32::MAX, 1, 3)),
            CircuitProperty::new("rounding", "Rounding", Rounding::same(0.75)),
        ])
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        Led::describe_props(props).to_dyn()
    }
}
