use eframe::{
    egui::{CursorIcon, PointerButton, Sense},
    epaint::{Color32, FontId, Rounding, Stroke},
};
use emath::{pos2, vec2, Align2};

use crate::{error::ResultReport, unwrap_option_or_return, Direction4};

use super::{
    props::{CircuitProperty, RangedValue},
    *,
};

struct ButtonVisuals {
    color: Color32,
    stroke: Stroke,
    font_color: Color32,
    font_scale: f32,
    rounding: Rounding,
}

struct Button {
    out_pin: CircuitPinInfo,
    text: Arc<str>,
    size: Vec2u,
    visuals: ButtonVisuals,
}

impl Button {
    fn new() -> Self {
        let description = Self::describe(Direction4::Right, 3, 3);
        Self {
            out_pin: description.pins[0].to_info(),
            text: "PUSH".into(),
            size: 3.into(),
            visuals: ButtonVisuals {
                color: Color32::from_rgb(200, 30, 30),
                font_color: Color32::WHITE,
                rounding: Rounding::same(0.75),
                font_scale: 0.5,
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

    fn draw_button(
        state: Option<&CircuitStateContext>,
        text: &str,
        visuals: &ButtonVisuals,
        ctx: &PaintContext,
        semi_transparent: bool,
    ) {
        let color_mul = if semi_transparent { 0.5 } else { 1.0 };
        let state = state
            .map(|s| {
                s.read_circuit_internal_state::<ButtonState, _>(|state| state.state)
                    .unwrap_or_default()
            })
            .unwrap_or_default();
        let color = if state {
            let c = visuals.color.linear_multiply(0.77);
            Color32::from_rgba_premultiplied(c.r(), c.g(), c.b(), visuals.color.a())
        } else {
            visuals.color
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

        let font = FontId::monospace(ctx.screen.scale * visuals.font_scale);

        ctx.paint.text(
            ctx.rect.center(),
            Align2::CENTER_CENTER,
            text,
            font,
            visuals.font_color,
        );
    }

    fn describe_props(props: &CircuitPropertyStore) -> CircuitDescription<1> {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Right);
        let width = props
            .read("width", |v: &RangedValue<u32>| v.get())
            .unwrap_or(3);
        let height = props
            .read("height", |v: &RangedValue<u32>| v.get())
            .unwrap_or(3);
        Self::describe(dir, width, height)
    }

    fn describe(dir: Direction4, width: u32, height: u32) -> CircuitDescription<1> {
        let pin_pos = match dir {
            Direction4::Up => [width / 2, 0],
            Direction4::Left => [0, height / 2],
            Direction4::Down => [width / 2, height - 1],
            Direction4::Right => [width - 1, height / 2],
        };

        crate::circuits::CircuitDescription {
            size: [width, height].into(),
            pins: [crate::circuits::CircuitPinDescription {
                active: true,
                name: "out".into(),
                dir: InternalPinDirection::Outside,
                display_name: "Out".into(),
                display_dir: Some(dir),
                pos: pin_pos.into(),
            }],
        }
    }
}

impl CircuitImpl for Button {
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
                    .unwrap_or_else(|| "Button".into()),
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

        Self::draw_button(state, &self.text, &self.visuals, ctx, false);
        if !interactive {
            return;
        }

        let state = unwrap_option_or_return!(state);

        let id = ctx.ui.auto_id_with(uid);

        let interaction = ctx.ui.interact(ctx.rect, id, Sense::drag());
        if interaction.hovered() {
            ctx.ui.ctx().set_cursor_icon(CursorIcon::PointingHand);
        }
        let shift = ctx.ui.input(|input| input.modifiers.shift);
        if interaction.drag_started_by(PointerButton::Primary)
            || !shift && interaction.drag_released_by(PointerButton::Primary)
        {
            let new_state = state.write_circuit_internal_state::<ButtonState, _>(|s| {
                s.state = !s.state;
                s.state
            });
            self.out_pin.set_state(state, new_state.into());
        }
    }

    fn prop_changed(&self, prop_id: &str, resize: &mut bool, recreate_pins: &mut bool) {
        if prop_id == "dir" {
            *recreate_pins = true;
        }
        if prop_id == "width" || prop_id == "height" {
            *recreate_pins = true;
            *resize = true;
        }
    }

    fn apply_props(&mut self, circ: &Arc<Circuit>, changed: Option<&str>) {
        if matches!(changed, None | Some("color")) {
            if let Some(color) = circ.props.read_clone("color") {
                self.visuals.color = color;
            }
        }
        if matches!(changed, None | Some("stroke")) {
            if let Some(stroke) = circ.props.read_clone("stroke") {
                self.visuals.stroke = stroke;
            }
        }
        if matches!(changed, None | Some("font_color")) {
            if let Some(font_color) = circ.props.read_clone("font_color") {
                self.visuals.font_color = font_color;
            }
        }
        if matches!(changed, None | Some("text")) {
            if let Some(text) = circ.props.read("text", |s: &ArcString| s.get_arc()) {
                self.text = text;
            }
        }
        if matches!(changed, None | Some("width")) {
            if let Some(value) = circ.props.read("width", |v: &RangedValue<u32>| v.get()) {
                *self.size.x_mut() = value;
            }
        }
        if matches!(changed, None | Some("height")) {
            if let Some(value) = circ.props.read("height", |v: &RangedValue<u32>| v.get()) {
                *self.size.y_mut() = value;
            }
        }
        if matches!(changed, None | Some("font_scale")) {
            if let Some(value) = circ
                .props
                .read("font_scale", |v: &RangedValue<f32>| v.get())
            {
                self.visuals.font_scale = value;
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
        self.out_pin = description.pins[0].to_info();

        vec![self.out_pin.clone()].into_boxed_slice()
    }

    fn update_signals(&self, state_ctx: &CircuitStateContext, _: Option<usize>) {
        let state = state_ctx
            .read_circuit_internal_state::<ButtonState, _>(|state| state.state)
            .unwrap_or_default();
        self.out_pin.set_state(state_ctx, state.into());
    }

    fn load_internal(
        &self,
        _ctx: &CircuitStateContext,
        data: &serde_intermediate::Intermediate,
        _paste: bool,
        errors: &mut ErrorList,
    ) -> Option<Box<dyn InternalCircuitState>> {
        serde_intermediate::de::intermediate::deserialize::<ButtonState>(data)
            .report_error(errors)
            .map(|s| Box::new(s) as Box<dyn InternalCircuitState>)
    }

    fn size(&self, circ: &Arc<Circuit>) -> Vec2u {
        Self::describe_props(&circ.props).size
    }
}

#[derive(Default, Serialize, Deserialize)]
struct ButtonState {
    state: bool,
}

impl InternalCircuitState for ButtonState {
    fn serialize(&self, _: bool) -> serde_intermediate::Intermediate {
        serde_intermediate::to_intermediate(self).unwrap()
    }
}

#[derive(Debug)]
pub struct ButtonPreview {}

impl CircuitPreviewImpl for ButtonPreview {
    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, in_world: bool) {
        Button::draw_base(ctx, in_world);
        let button_ctx = ctx.with_rect(ctx.rect.shrink(ctx.screen.scale * 0.75));
        let color = props
            .read_clone("color")
            .unwrap_or(Color32::from_rgb(200, 30, 30));
        let text = props.read("text", |s: &ArcString| s.get_arc());
        let text = text.as_deref().unwrap_or("PUSH");

        let font_scale = props
            .read("font_scale", |v: &RangedValue<f32>| v.get())
            .unwrap_or(0.5);
        let rounding = props.read_clone("rounding").unwrap_or(Rounding::same(0.75));
        let font_color = props.read_clone("font_color").unwrap_or(Color32::WHITE);
        let stroke = props.read_clone("stroke").unwrap_or(Stroke::NONE);

        let visuals = ButtonVisuals {
            color,
            stroke,
            font_color,
            font_scale,
            rounding,
        };

        Button::draw_button(None, text, &visuals, &button_ctx, in_world);
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Button::new())
    }

    fn type_name(&self) -> DynStaticStr {
        "button".into()
    }

    fn load_copy_data(
        &self,
        _imp: &serde_intermediate::Intermediate,
        _internal: &serde_intermediate::Intermediate,
        _ctx: &Arc<SimulationContext>,
        _errors: &mut ErrorList,
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        Some(Box::new(ButtonPreview {}))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        CircuitPropertyStore::new([
            CircuitProperty::new("dir", "Direction", Direction4::Right),
            CircuitProperty::new("color", "Color", Color32::from_rgb(200, 30, 30)),
            CircuitProperty::new("stroke", "Stroke", Stroke::NONE),
            CircuitProperty::new("font_color", "Font color", Color32::WHITE),
            CircuitProperty::new("text", "Text", ArcString::from("PUSH")),
            CircuitProperty::new("width", "Width", RangedValue::new(3..=u32::MAX, 1, 3)),
            CircuitProperty::new("height", "Height", RangedValue::new(3..=u32::MAX, 1, 3)),
            CircuitProperty::new(
                "font_scale",
                "Font scale",
                RangedValue::new(0.01..=f32::MAX, 0.05, 0.5),
            ),
            CircuitProperty::new("rounding", "Rounding", Rounding::same(0.75)),
        ])
    }

    fn display_name(&self) -> DynStaticStr {
        "Button".into()
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        Button::describe_props(props).to_dyn()
    }
}
