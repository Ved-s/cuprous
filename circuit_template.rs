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

    fn size(props: &CircuitPropertyStore) -> Vec2u {
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

    fn size(&self, props: &CircuitPropertyStore) -> Vec2u {
        Circuit::size(props)
    }
}

#[derive(Debug)]
pub struct Preview {}

impl CircuitPreviewImpl for Preview {
    fn draw_preview(&self, ctx: &PaintContext) {
        Circuit::draw(ctx, true);
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Circuit::new())
    }

    fn size(&self, props: &CircuitPropertyStore) -> Vec2u {
        Circuit::size(props)
    }
}