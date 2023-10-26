#![allow(warnings)]

use crate::circuits::*;

struct Template {}

impl Template {
    fn new() -> Self {
        Self {}
    }

    fn draw(state: Option<&CircuitStateContext>, ctx: &PaintContext, semi_transparent: bool) {
        todo!()
    }

    fn describe_props(props: &CircuitPropertyStore) -> CircuitDescription<0> {
        Self::describe()
    }

    fn describe() -> CircuitDescription<0> {
        todo!()
    }
}

impl CircuitImpl for Template {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext) {
        Template::draw(Some(state_ctx), paint_ctx, false);
    }

    fn create_pins(&mut self, circ: &Arc<Circuit>) -> Box<[CircuitPinInfo]> {
        let description = Self::describe_props(&circ.props);
        vec![].into_boxed_slice()
    }

    fn update_signals(&self, state_ctx: &CircuitStateContext, changed_pin: Option<usize>) {
        todo!()
    }

    fn size(&self, circ: &Arc<Circuit>) -> Vec2u {
        Self::describe_props(&circ.props).size
    }
}

pub struct TemplatePreview {}

impl CircuitPreviewImpl for TemplatePreview {
    fn type_name(&self) -> DynStaticStr {
        todo!()
    }

    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, in_world: bool) {
        Template::draw(None, ctx, in_world);
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Template::new())
    }

    fn load_impl_data(
        &self,
        data: &serde_intermediate::Intermediate,
        ctx: &Arc<SimulationContext>
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        Some(Box::new(TemplatePreview {}))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        todo!()
    }

    fn display_name(&self) -> DynStaticStr {
        todo!()
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        Template::describe_props(props).to_dyn()
    }
}
