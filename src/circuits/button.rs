use eframe::{
    egui::{PointerButton, Sense, CursorIcon},
    epaint::{Color32, FontId, Rounding},
};
use emath::{Align2, pos2, vec2};

use crate::{Direction4, describe_directional_circuit, unwrap_option_or_return};

use super::{*, props::CircuitProperty};

struct Button {
    out_pin: CircuitPinInfo,
    color: Color32,
    text: Arc<str>
}

impl Button {
    fn new() -> Self {
        let description = Self::describe(Direction4::Right);
        Self {
            out_pin: description.pins[0].to_info(),
            color: Color32::from_rgb(200, 30, 30),
            text: "PUSH".into()
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

    fn draw_button(state: Option<&CircuitStateContext>, color: Color32, text: &str, ctx: &PaintContext, semi_transparent: bool) {
        let color_mul = if semi_transparent { 0.5 } else { 1.0 };
        let state = state
            .map(|s| {
                s.read_circuit_internal_state::<ButtonState, _>(|state| state.state)
                    .unwrap_or_default()
            })
            .unwrap_or_default();
        let color = if state {
            let c = color.linear_multiply(0.77);
            Color32::from_rgba_premultiplied(c.r(), c.g(), c.b(), color.a())
        } else {
            color
        }
        .linear_multiply(color_mul);
        ctx.paint
            .circle_filled(ctx.rect.center(), ctx.rect.width() * 0.5, color);

        let font = FontId::monospace(ctx.screen.scale * 0.5);

        ctx.paint.text(
            ctx.rect.center(),
            Align2::CENTER_CENTER,
            text,
            font,
            Color32::WHITE,
        );
    }

    fn describe_props(props: &CircuitPropertyStore) -> CircuitDescription<1> {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Right);
        Self::describe(dir)
    }

    fn describe(dir: Direction4) -> CircuitDescription<1> {
        describe_directional_circuit! {
            default_dir: Right,
            dir: dir,
            size: [3, 3],

            "out": Outside, "Out", Right, [2, 1]
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
        match id {
            0 => Some(CircuitControlInfo {
                rect: Rect::from_min_size(pos2(0.75, 0.75), vec2(1.5, 1.5)),
                display_name: circuit.name().map(|arc| arc.into()).unwrap_or_else(|| "Button".into()),
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

        Self::draw_button(state, self.color, &self.text, ctx, false);
        if !interactive {
            return;
        }

        let state = unwrap_option_or_return!(state);

        let id = ctx.ui.auto_id_with(uid);

        let interaction = ctx.ui.interact(
            ctx.rect,
            id,
            Sense::drag(),
        );
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

    fn prop_changed(&self, prop_id: &str, _: &mut bool, recreate_pins: &mut bool) {
        if prop_id == "dir" {
            *recreate_pins = true
        }
    }

    fn apply_props(&mut self, circ: &Arc<Circuit>, changed: Option<&str>) {
        if matches!(changed, None | Some("btn_color")) {
            if let Some(color) = circ.props.read_clone("btn_color") {
                self.color = color;
            }
        }
        if matches!(changed, None | Some("btn_text")) {
            if let Some(text) = circ.props.read("btn_text", |s: &ArcString| s.get_arc()) {
                self.text = text;
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
        _: &CircuitStateContext,
        data: &serde_intermediate::Intermediate,
        _: bool
    ) -> Option<Box<dyn InternalCircuitState>> {
        serde_intermediate::de::intermediate::deserialize::<ButtonState>(data)
            .ok()
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
        let color = props.read_clone("btn_color").unwrap_or(Color32::from_rgb(200, 30, 30));
        let text = props.read("btn_text", |s: &ArcString| s.get_arc());
        let text = text.as_deref().unwrap_or("PUSH");

        Button::draw_button(None, color, text, &button_ctx, in_world);
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Button::new())
    }

    fn type_name(&self) -> DynStaticStr {
        "button".into()
    }

    fn load_copy_data(
        &self,
        _: &serde_intermediate::Intermediate,
        _: &serde_intermediate::Intermediate,
        _: &Arc<SimulationContext>
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        Some(Box::new(ButtonPreview {}))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        CircuitPropertyStore::new([
            CircuitProperty::new("dir", "Direction", Direction4::Right),
            CircuitProperty::new("btn_color", "Color", Color32::from_rgb(200, 30, 30)),
            CircuitProperty::new("btn_text", "Text", ArcString::from("PUSH")),
        ])
    }

    fn display_name(&self) -> DynStaticStr {
        "Button".into()
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        Button::describe_props(props).to_dyn()
    }
}
