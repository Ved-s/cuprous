use eframe::epaint::{Color32, Stroke};
use emath::{vec2, Vec2};

use crate::{
    circuits::*,
    path::{PathItem, PathItemIterator},
};

#[derive(Clone)]
pub struct GateTemplate {
    pub id: &'static str,
    pub process_inputs: fn(&[bool]) -> bool,
    pub drawer: fn(&PaintContext, bool),
}

struct Circuit {
    template: GateTemplate,
    inputs: Box<[CircuitPinInfo]>,
    input_bools: Vec<bool>,
    output: CircuitPinInfo,
}

impl Circuit {
    fn new(template: GateTemplate) -> Self {
        Self {
            template,
            inputs: vec![
                CircuitPinInfo::new([0, 0], InternalPinDirection::Inside, "in_0"),
                CircuitPinInfo::new([0, 2], InternalPinDirection::Inside, "in_1"),
            ]
            .into_boxed_slice(),
            output: CircuitPinInfo::new([3, 1], InternalPinDirection::Outside, "out"),
            input_bools: Vec::with_capacity(2)
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
        let states = self
            .inputs
            .iter()
            .map(|i| i.get_input(state_ctx));
        self.input_bools.clear();
        for state in states {
            match state {
                WireState::None => continue,
                WireState::True => self.input_bools.push(true),
                WireState::False => self.input_bools.push(false),
                WireState::Error => {
                    self.output.set_output(state_ctx, WireState::Error);
                    return;
                },
            }
        }
        if self.input_bools.is_empty() {
            self.output.set_output(state_ctx, WireState::None);
        }
        else {
            self.output.set_output(state_ctx, (self.template.process_inputs)(&self.input_bools).into());
        }
    }
}

pub struct Preview {
    pub template: GateTemplate,
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

    fn type_name(&self) -> DynStaticStr {
        self.template.id.into()
    }

    fn load_impl_data(&self, _: &serde_intermediate::Intermediate) -> Option<Box<dyn CircuitPreview>> {
        Some(Box::new(Preview { template: self.template.clone() }))
    }
}

pub fn draw_from_path(ctx: &PaintContext, semi_transparent: bool, size: Vec2, path: &[PathItem]) {
    let opacity = if semi_transparent { 0.6 } else { 1.0 };

    let border_color = Color32::BLACK.linear_multiply(opacity);
    let fill_color = Color32::from_gray(200).linear_multiply(opacity);
    let straightness = (0.3 / (ctx.screen.scale.sqrt())).max(0.01);

    path.iter().cloned().create_path_shapes(
        ctx.rect.left_top().to_vec2(),
        vec2(
            1.0 / size.x * ctx.rect.width(),
            1.0 / size.y * ctx.rect.height(),
        ),
        fill_color,
        Stroke::new(2.0, border_color),
        straightness,
        |_, s| {
            ctx.paint.add(s);
        },
    );
}
