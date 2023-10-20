#![allow(warnings)]

use crate::{circuits::*, describe_directional_circuit};
use crate::circuits::props::CircuitProperty;

struct Circuit {
    dir: Direction4,
}

impl Circuit {

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

impl CircuitImpl for Circuit {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext) {
        Circuit::draw(Some(state_ctx), self.dir, paint_ctx, false);
    }

    fn create_pins(&mut self, props: &CircuitPropertyStore) -> Box<[CircuitPinInfo]> {
        let description = Self::describe_props(props);
        vec![].into_boxed_slice()
    }

    fn update_signals(&self, state_ctx: &CircuitStateContext, changed_pin: Option<usize>) {
        todo!()
    }

    fn size(&self, props: &CircuitPropertyStore) -> Vec2u {
        Self::describe_props(props).size
    }

    fn prop_changed(&self, prop_id: &str, resize: &mut bool, recreate_pins: &mut bool) {
        if prop_id == "dir" {
            *resize = true;
            *recreate_pins = true;
        }
    }

    fn apply_props(&mut self, props: &CircuitPropertyStore, _: Option<&str>) {
        self.dir = props.read_clone("dir").unwrap_or(Self::DEFAULT_DIR);
    }
}

pub struct Preview {}

impl CircuitPreviewImpl for Preview {
    fn type_name(&self) -> DynStaticStr {
        todo!()
    }

    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, in_world: bool) {
        let dir = props.read_clone("dir").unwrap_or(Circuit::DEFAULT_DIR);
        Circuit::draw(None, dir, ctx, in_world);
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Circuit::new())
    }

    fn load_impl_data(
        &self,
        data: &serde_intermediate::Intermediate,
        ctx: &Arc<SimulationContext>
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        Some(Box::new(Preview {}))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        CircuitPropertyStore::new([
            CircuitProperty::new("dir", "Direction", Circuit::DEFAULT_DIR)
        ])
    }

    fn display_name(&self) -> DynStaticStr {
        todo!()
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        Circuit::describe_props(props).to_dyn()
    }
}
