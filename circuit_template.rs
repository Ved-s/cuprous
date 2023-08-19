use crate::circuits::*;

struct Circuit {}

impl Circuit {
    fn new() -> Self {
        Self {}
    }

    fn draw(ctx: &PaintContext, semi_transparent: bool) {
        todo!()
    }

    fn size(props: &CircuitPropertyStore) -> Vec2u {
        todo!()
    }

    fn pin_positions(props: &CircuitPropertyStore) -> _ {
        todo!()
    }
}

impl CircuitImpl for Circuit {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext) {
        Circuit::draw(paint_ctx, false);
    }

    fn create_pins(&mut self, props: &CircuitPropertyStore) -> Box<[CircuitPinInfo]> {
        let pin_positions = Circuit::pin_positions(props);
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
    fn type_name(&self) -> DynStaticStr {
        todo!()
    }

    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, in_world: bool) {
        Circuit::draw(ctx, in_world);
        draw_pins_preview(ctx, Circuit::size(props), Circuit::pin_positions(props))
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Circuit::new())
    }

    fn size(&self, props: &CircuitPropertyStore) -> Vec2u {
        Circuit::size(props)
    }

    fn load_impl_data(
        &self,
        data: &serde_intermediate::Intermediate,
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        Some(Box::new(Preview {}))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        todo!()
    }
}
