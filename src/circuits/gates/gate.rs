use eframe::epaint::{Color32, Stroke};
use emath::Vec2;

use crate::{
    circuits::{
        props::{CircuitProperty, CircuitPropertyStore},
        *,
    },
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
        Self {
            template,
            inputs: vec![
                CircuitPinInfo::new([0, 0], InternalPinDirection::Inside, "in_0"),
                CircuitPinInfo::new([0, 2], InternalPinDirection::Inside, "in_1"),
            ]
            .into_boxed_slice(),
            output: CircuitPinInfo::new([3, 1], InternalPinDirection::Outside, "out"),
            dir: Direction4::Right,
        }
    }

    fn size(props: &CircuitPropertyStore) -> Vec2u {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Right);
        if dir.is_horizontal() {
            [4, 3].into()
        } else {
            [3, 4].into()
        }
    }

    /// [in_1, in_2, out]
    fn pin_positions(props: &CircuitPropertyStore) -> [[u32; 2]; 3] {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Right);
        match dir {
            Direction4::Up => [[0, 3], [2, 3], [1, 0]],
            Direction4::Left => [[3, 0], [3, 2], [0, 1]],
            Direction4::Down => [[0, 0], [2, 0], [1, 3]],
            Direction4::Right => [[0, 0], [0, 2], [3, 1]],
        }
    }
}

impl CircuitImpl for Circuit {
    fn draw(&self, _: &CircuitStateContext, paint_ctx: &PaintContext) {
        let angle = self.dir.inverted_ud().angle_to_right();
        (self.template.drawer)(paint_ctx, angle, false);
    }

    fn create_pins(&mut self, props: &CircuitPropertyStore) -> Box<[CircuitPinInfo]> {
        
        let pin_positions = Circuit::pin_positions(props);
        self.inputs = vec![
            CircuitPinInfo::new(pin_positions[0], InternalPinDirection::Inside, "in_0"),
            CircuitPinInfo::new(pin_positions[1], InternalPinDirection::Inside, "in_1"),
        ]
        .into_boxed_slice();
        self.output = CircuitPinInfo::new(pin_positions[2], InternalPinDirection::Outside, "out");
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
                self.output.set_state(
                    state_ctx,
                    (self.template.process_inputs)(&b).into(),
                );
            }
        });
    }

    fn size(&self, props: &CircuitPropertyStore) -> Vec2u {
        Circuit::size(props)
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
        let angle = props
            .read_clone("dir")
            .unwrap_or(Direction4::Right)
            .inverted_ud()
            .angle_to_right();
        (self.template.drawer)(ctx, angle, in_world);

        draw_pins_preview(ctx, self.size(props), Circuit::pin_positions(props))
    }

    fn size(&self, props: &CircuitPropertyStore) -> Vec2u {
        Circuit::size(props)
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
        Stroke::new(2.0, border_color),
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
