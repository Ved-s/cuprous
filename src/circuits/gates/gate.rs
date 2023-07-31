use crate::circuits::*;

#[derive(Clone)]
pub struct GateTemplate {
    pub id: &'static str,
    pub bool_combiner: fn(bool, bool) -> bool,
    pub drawer: fn(&PaintContext, bool)
}

struct Circuit {
    template: GateTemplate,
    inputs: Box<[CircuitPinInfo]>,
    output: CircuitPinInfo,
}

impl Circuit {
    fn new(template: GateTemplate) -> Self {
        Self {
            template,
            inputs: vec![
                CircuitPinInfo::new([0, 0], InternalPinDirection::Inside),
                CircuitPinInfo::new([0, 2], InternalPinDirection::Inside),
            ]
            .into_boxed_slice(),
            output: CircuitPinInfo::new([3, 1], InternalPinDirection::Outside),
        }
    }
}

impl CircuitImpl for Circuit {
    fn draw(&self, _: &CircuitStateContext, paint_ctx: &PaintContext) {
        (self.template.drawer)(paint_ctx, false);
    }

    fn create_pins(&self) -> Box<[CircuitPinInfo]> {
        let mut vec = vec![self.output.clone()];
        vec.extend(self.inputs.iter().cloned());
        vec.into_boxed_slice()
    }

    fn update_signals(&mut self, state_ctx: &CircuitStateContext, _: Option<usize>) {
        let state = self
            .inputs
            .iter()
            .map(|i| i.get_input(state_ctx))
            .reduce(|a, b| a.combine_boolean(b, self.template.bool_combiner))
            .unwrap_or_default();
        self.output.set_output(state_ctx, state);
    }
}

pub struct Preview {
    pub template: GateTemplate
}

impl CircuitPreview for Preview {
    fn draw_preview(&self, ctx: &PaintContext, in_world: bool) {
        (self.template.drawer)(ctx, in_world);
    }

    fn size(&self) -> Vec2u {
        [4, 3].into()
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Circuit::new(self.template.clone()))
    }
}
