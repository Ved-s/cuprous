use crate::circuits::*;

struct Circuit {

}

impl Circuit {
    fn new() -> Self {
        Self {

        }
    }

    fn draw(ctx: &PaintContext, preview: bool) {
        todo!()
    }
}

impl CircuitImpl for Circuit {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext) {
        Circuit::draw(paint_ctx, false);
    }

    fn create_pins(&self) -> Box<[CircuitPinInfo]> {
        vec![].into_boxed_slice()
    }

    fn update_signals(&mut self, state_ctx: &CircuitStateContext, changed_pin: Option<usize>) {
        todo!()
    }
}

#[derive(Debug)]
pub struct Preview {}

impl CircuitPreview for Preview {
    fn draw_preview(&self, ctx: &PaintContext) {
        Circuit::draw(ctx, true);
    }

    fn size(&self) -> Vec2u {
        todo!()
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Circuit::new())
    }
}