use eframe::epaint::{Color32, Stroke};
use emath::Vec2;

use crate::{
    circuits::{
        props::{CircuitProperty, CircuitPropertyStore},
        *,
    },
    describe_directional_circuit,
    path::{PathItem, PathItemIterator},
    vector::Vec2f,
    Direction4, Mutex,
};

#[derive(Clone)]
pub struct GateTemplate {
    pub id: &'static str,
    pub name: &'static str,
    pub process_inputs: fn(&[bool]) -> bool,
    pub drawer: fn(&PaintContext, f32, bool),
}

thread_local! {
    static INPUT_BOOLS: Mutex<Vec<bool>> = Default::default();
}

struct Circuit {
    template: GateTemplate,
    inputs: Box<[CircuitPinInfo]>,
    dir: Direction4,
    output: CircuitPinInfo,
}

impl Circuit {
    fn new(template: GateTemplate) -> Self {
        let description = Self::describe(Direction4::Right);
        Self {
            template,
            inputs: vec![
                description.pins[0].to_info(),
                description.pins[1].to_info(),
            ]
            .into_boxed_slice(),
            output: description.pins[2].to_info(),
            dir: Direction4::Right,
        }
    }

    fn describe_props(props: &CircuitPropertyStore) -> CircuitDescription<3> {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Right);
        Self::describe(dir)
    }

    fn describe(dir: Direction4) -> CircuitDescription<3> {
        describe_directional_circuit! {
            default_dir: Right,
            dir: dir,
            size: [4, 3],
            "in_0": Inside, "A", Left, [0, 0],
            "in_1": Inside, "B", Left, [0, 2],
            "out": Outside, "Out", Right, [3, 1],
        }
    }
}

impl CircuitImpl for Circuit {
    fn draw(&self, _: &CircuitStateContext, paint_ctx: &PaintContext) {
        let angle = self.dir.inverted_ud().angle_to_right();
        (self.template.drawer)(paint_ctx, angle, false);
    }

    fn create_pins(&mut self, props: &CircuitPropertyStore) -> Box<[CircuitPinInfo]> {
        let description = Self::describe_props(props);
        self.inputs = vec![
            description.pins[0].to_info(),
            description.pins[1].to_info(),
        ]
        .into_boxed_slice();
        self.output = description.pins[2].to_info();
        let mut vec = vec![self.output.clone()];
        vec.extend(self.inputs.iter().cloned());
        vec.into_boxed_slice()
    }

    fn update_signals(&self, state_ctx: &CircuitStateContext, _: Option<usize>) {
        let states = self.inputs.iter().map(|i| i.get_state(state_ctx));
        INPUT_BOOLS.with(|b| {
            let mut b = b.lock();

            b.clear();
            for state in states {
                match state {
                    WireState::None => continue,
                    WireState::True => b.push(true),
                    WireState::False => b.push(false),
                    WireState::Error => {
                        self.output.set_state(state_ctx, WireState::Error);
                        return;
                    }
                }
            }
            if b.is_empty() {
                self.output.set_state(state_ctx, WireState::None);
            } else {
                self.output
                    .set_state(state_ctx, (self.template.process_inputs)(&b).into());
            }
        });
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
        self.dir = props.read_clone("dir").unwrap_or(Direction4::Right);
    }
}

pub struct Preview {
    pub template: GateTemplate,
}

impl CircuitPreviewImpl for Preview {
    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, in_world: bool) {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Right);
        let angle = dir
            .inverted_ud()
            .angle_to_right();
        (self.template.drawer)(ctx, angle, in_world);
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Circuit::new(self.template.clone()))
    }

    fn type_name(&self) -> DynStaticStr {
        self.template.id.into()
    }

    fn load_impl_data(
        &self,
        _: &serde_intermediate::Intermediate,
        _: &BoardStorage
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        Some(Box::new(Preview {
            template: self.template.clone(),
        }))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        CircuitPropertyStore::new([CircuitProperty::new("dir", "Direction", Direction4::Right)])
    }

    fn display_name(&self) -> DynStaticStr {
        self.template.name.into()
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Right);
        Circuit::describe(dir).to_dyn()
    }
}

pub fn draw_from_path(
    ctx: &PaintContext,
    semi_transparent: bool,
    size: Vec2,
    angle: f32,
    path: &[PathItem],
) {
    let opacity = if semi_transparent { 0.6 } else { 1.0 };

    let border_color = Color32::BLACK.linear_multiply(opacity);
    let fill_color = Color32::from_gray(200).linear_multiply(opacity);
    let straightness = (0.3 / (ctx.screen.scale.sqrt())).max(0.01);

    path.iter().cloned().create_path_shapes(
        fill_color,
        Stroke::new(0.15 * ctx.screen.scale, border_color),
        straightness,
        |p| {
            ctx.rect.lerp_inside(
                Vec2f::from(p.to_vec2() / size)
                    .rotated_xy(angle, 0.5)
                    .into(),
            )
        },
        |_, s| {
            ctx.paint.add(s);
        },
    );
}
