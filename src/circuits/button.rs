use eframe::{
    egui::{PointerButton, Sense, CursorIcon},
    epaint::{Color32, FontId, Rounding},
};
use emath::Align2;

use crate::{Direction4, describe_directional_circuit};

use super::{*, props::CircuitProperty};

struct Button {
    out_pin: CircuitPinInfo,
}

impl Button {
    fn new() -> Self {
        let description = Self::describe(Direction4::Right);
        Self {
            out_pin: description.pins[0].to_info(),
        }
    }

    fn draw(state: Option<&CircuitStateContext>, ctx: &PaintContext, semi_transparent: bool) {
        let color_mul = if semi_transparent { 0.5 } else { 1.0 };
        ctx.paint.rect_filled(
            ctx.rect.expand(ctx.screen.scale * -0.5),
            Rounding::same(ctx.screen.scale * 0.25),
            Color32::from_gray(100).linear_multiply(color_mul),
        );

        let state = state
            .map(|s| {
                s.read_circuit_internal_state::<ButtonState, _>(|state| state.state)
                    .unwrap_or_default()
            })
            .unwrap_or_default();
        let color = if state {
            Color32::from_rgb(175, 20, 20)
        } else {
            Color32::from_rgb(200, 30, 30)
        }
        .linear_multiply(color_mul);
        ctx.paint
            .circle_filled(ctx.rect.center(), ctx.screen.scale * 0.75, color);

        let font = FontId::monospace(ctx.screen.scale * 0.5);

        ctx.paint.text(
            ctx.rect.center(),
            Align2::CENTER_CENTER,
            "PUSH",
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
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext) {
        Self::draw(Some(state_ctx), paint_ctx, false);

        // HACK: write proper circuit interactables
        let rect = paint_ctx.rect.expand(paint_ctx.screen.scale * -0.75);
        let interaction = paint_ctx.ui.interact(
            rect,
            paint_ctx.ui.auto_id_with(state_ctx.circuit.pos),
            Sense::drag(),
        );
        if interaction.hovered() {
            paint_ctx.ui.ctx().set_cursor_icon(CursorIcon::PointingHand);
        }
        let shift = paint_ctx.ui.input(|input| input.modifiers.shift);
        if interaction.drag_started_by(PointerButton::Primary)
            || !shift && interaction.drag_released_by(PointerButton::Primary)
        {
            let new_state = state_ctx.write_circuit_internal_state::<ButtonState, _>(|s| {
                s.state = !s.state;
                s.state
            });
            self.out_pin.set_state(state_ctx, new_state.into());
        }
    }

    fn prop_changed(&self, prop_id: &str, _: &mut bool, recreate_pins: &mut bool) {
        if prop_id == "dir" {
            *recreate_pins = true
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
    fn draw_preview(&self, _: &CircuitPropertyStore, ctx: &PaintContext, in_world: bool) {
        Button::draw(None, ctx, in_world);
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
            CircuitProperty::new("dir", "Direction", Direction4::Right)
        ])
    }

    fn display_name(&self) -> DynStaticStr {
        "Button".into()
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        Button::describe_props(props).to_dyn()
    }
}
