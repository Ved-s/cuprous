use eframe::epaint::{Color32, Stroke};

use crate::circuits::*;

use super::props::CircuitPropertyStore;

struct Pullup {
    pin: CircuitPinInfo,
}

impl Pullup {
    fn new() -> Self {
        let description = Self::describe();
        Self {
            pin: description.pins[0].to_info(),
        }
    }

    fn draw(ctx: &PaintContext, _: bool) {
        ctx.paint.circle_stroke(
            ctx.rect.center(),
            ctx.screen.scale * 0.5,
            Stroke::new(1.0, Color32::BLACK),
        )
    }

    fn describe_props(_: &CircuitPropertyStore) -> CircuitDescription<1> {
        Self::describe()
    }

    fn describe() -> CircuitDescription<1> {
        CircuitDescription {
            size: [1, 1].into(),
            pins: [CircuitPinDescription {
                active: true,
                display_name: "".into(),
                display_dir: None,
                dir: InternalPinDirection::Custom,
                name: "pin".into(),
                pos: [0, 0].into(),
            }],
        }
    }
}

impl CircuitImpl for Pullup {
    fn draw(&self, _: &CircuitStateContext, paint_ctx: &PaintContext) {
        Pullup::draw(paint_ctx, false);
    }

    fn create_pins(&mut self, _: &Arc<Circuit>) -> Box<[CircuitPinInfo]> {
        vec![self.pin.clone()].into_boxed_slice()
    }

    fn update_signals(&self, _: &CircuitStateContext, _: Option<usize>) {}

    fn custom_pin_mutate_state(&self, _: &CircuitStateContext, _: usize, state: &mut WireState) {
        if matches!(state, WireState::None) {
            *state = WireState::False;
        }
    }

    fn size(&self, circ: &Arc<Circuit>) -> Vec2u {
        Self::describe_props(&circ.props).size
    }
}

#[derive(Debug)]
pub struct PullupPreview {}

impl CircuitPreviewImpl for PullupPreview {
    fn draw_preview(&self, _: &CircuitPropertyStore, ctx: &PaintContext, in_world: bool) {
        Pullup::draw(ctx, in_world);
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Pullup::new())
    }

    fn type_name(&self) -> DynStaticStr {
        "pullup".into()
    }

    fn load_impl_data(
        &self,
        _: &serde_intermediate::Intermediate,
        _: &Arc<SimulationContext>,
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        Some(Box::new(PullupPreview {}))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        Default::default()
    }

    fn display_name(&self) -> DynStaticStr {
        "Pullup".into()
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        Pullup::describe_props(props).to_dyn()
    }
}
