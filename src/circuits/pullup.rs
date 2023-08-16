use eframe::epaint::{Color32, Stroke};

use crate::circuits::*;

struct Circuit {
    pin: CircuitPinInfo,
}

impl Circuit {
    fn new() -> Self {
        Self {
            pin: CircuitPinInfo::new([0, 0], InternalPinDirection::Custom, "pin"),
        }
    }

    fn draw(ctx: &PaintContext, _: bool) {
        ctx.paint.circle_stroke(
            ctx.rect.center(),
            ctx.screen.scale * 0.5,
            Stroke::new(1.0, Color32::BLACK),
        )
    }
}

impl CircuitImpl for Circuit {
    fn draw(&self, _: &CircuitStateContext, paint_ctx: &PaintContext) {
        Circuit::draw(paint_ctx, false);
    }

    fn create_pins(&self) -> Box<[CircuitPinInfo]> {
        vec![self.pin.clone()].into_boxed_slice()
    }

    fn update_signals(&mut self, _: &CircuitStateContext, _: Option<usize>) {}

    fn custom_pin_mutate_state(
        &self,
        _: &CircuitStateContext,
        _: usize,
        state: &mut WireState,
    ) {
        if matches!(state, WireState::None) {
            *state = WireState::False;
        }
    }
}

#[derive(Debug)]
pub struct Preview {}

impl CircuitPreview for Preview {
    fn draw_preview(&self, ctx: &PaintContext, in_world: bool) {
        Circuit::draw(ctx, in_world);
    }

    fn size(&self) -> Vec2u {
        [1, 1].into()
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Circuit::new())
    }

    fn type_name(&self) -> DynStaticStr {
        "pullup".into()
    }

    fn load_impl_data(&self, _: &serde_intermediate::Intermediate) -> Option<Box<dyn CircuitPreview>> {
        Some(Box::new(Preview {}))
    }
}
