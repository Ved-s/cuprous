#![allow(warnings)]

use crate::{circuits::*, describe_directional_circuit};
use crate::circuits::props::CircuitProperty;

struct Template {
    dir: Direction4,
}

impl Template {

    const DEFAULT_DIR: Direction4 = Direction4::Right;

    fn new() -> Self {
        Self {
            dir: Self::DEFAULT_DIR
        }
    }

    fn draw(state: Option<&CircuitStateContext>, dir: Direction4, ctx: &PaintContext, semi_transparent: bool) {
        todo!()
    }

    fn describe_props(props: &CircuitPropertyStore) -> CircuitDescription<1> {
        let dir = props.read_clone("dir").unwrap_or(Self::DEFAULT_DIR);
        Self::describe(dir)
    }

    fn describe(dir: Direction4) -> CircuitDescription<1> {
        describe_directional_circuit! {
            default_dir: Self::DEFAULT_DIR,
            dir: dir,
            size: [1, 1],
            "pin": Inside, "Pin", Left, [0, 0],
        }
    }
}

impl CircuitImpl for Template {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext) {
        Template::draw(Some(state_ctx), self.dir, paint_ctx, false);
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

    fn prop_changed(&self, prop_id: &str, resize: &mut bool, recreate_pins: &mut bool) {
        if prop_id == "dir" {
            *resize = true;
            *recreate_pins = true;
        }
    }

    fn apply_props(&mut self, circ: &Arc<Circuit>, _: Option<&str>) {
        self.dir = circ.props.read_clone("dir").unwrap_or(Self::DEFAULT_DIR);
    }
}

pub struct TemplatePreview {}

impl CircuitPreviewImpl for TemplatePreview {
    fn type_name(&self) -> DynStaticStr {
        todo!()
    }

    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, in_world: bool) {
        let dir = props.read_clone("dir").unwrap_or(Template::DEFAULT_DIR);
        Template::draw(None, dir, ctx, in_world);
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Template::new())
    }

    fn load_copy_data(
        &self,
        imp: &serde_intermediate::Intermediate,
        internal: &serde_intermediate::Intermediate,
        ctx: &Arc<SimulationContext>
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        Some(Box::new(TemplatePreview {}))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        CircuitPropertyStore::new([
            CircuitProperty::new("dir", "Direction", Template::DEFAULT_DIR)
        ])
    }

    fn display_name(&self) -> DynStaticStr {
        todo!()
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        Template::describe_props(props).to_dyn()
    }
}
