use std::sync::Arc;

use eframe::epaint::{Color32, PathShape, Stroke};
use emath::{pos2, vec2, Pos2};

use crate::{
    app::SimulationContext,
    circuits::{
        props::CircuitProperty, Circuit, CircuitDescription, CircuitImpl, CircuitPinInfo,
        CircuitPreviewImpl, CircuitPropertyStore, CircuitStateContext, InternalPinDirection,
    },
    describe_directional_circuit,
    state::WireState,
    vector::{Vec2f, Vec2u},
    Direction4, DynStaticStr, PaintContext, error::ErrorList,
};

struct Not {
    dir: Direction4,
    input: CircuitPinInfo,
    output: CircuitPinInfo,
}

impl Not {
    fn new() -> Self {
        let description = Self::describe(Direction4::Right);
        Self {
            input: description.pins[0].to_info(),
            output: description.pins[1].to_info(),
            dir: Direction4::Right,
        }
    }

    fn draw(ctx: &PaintContext, angle: f32, semi_transparent: bool) {
        let opacity = if semi_transparent { 0.6 } else { 1.0 };

        let border_color = Color32::BLACK.linear_multiply(opacity);
        let fill_color = Color32::from_gray(200).linear_multiply(opacity);

        let size = vec2(2.0, 1.0);
        let transformer = |p: Pos2| {
            ctx.rect.lerp_inside(
                Vec2f::from(p.to_vec2() / size)
                    .rotated_xy(angle, 0.5)
                    .into(),
            )
        };

        let points = vec![
            transformer(pos2(0.5, 0.1)),
            transformer(pos2(1.32, 0.5)),
            transformer(pos2(0.5, 0.9)),
        ];
        ctx.paint.add(PathShape {
            points,
            closed: true,
            fill: fill_color,
            stroke: Stroke::new(0.15 * ctx.screen.scale, border_color),
        });
        ctx.paint.circle(
            transformer(pos2(1.32, 0.5)),
            0.2 * ctx.screen.scale,
            fill_color,
            Stroke::new(0.15 * ctx.screen.scale, border_color),
        );
    }

    fn describe_props(props: &CircuitPropertyStore) -> CircuitDescription<2> {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Right);
        Self::describe(dir)
    }

    fn describe(dir: Direction4) -> CircuitDescription<2> {
        describe_directional_circuit! {
            default_dir: Right,
            dir: dir,
            size: [2, 1],

            "in": Inside, "In", Left, [0, 0],
            "out": Outside, "Out", Right, [1, 0]
        }
    }
}

impl CircuitImpl for Not {
    fn draw(&self, _: &CircuitStateContext, paint_ctx: &PaintContext) {
        let angle = self.dir.inverted_ud().angle_to_right();
        Not::draw(paint_ctx, angle, false);
    }

    fn create_pins(&mut self, circ: &Arc<Circuit>) -> Box<[CircuitPinInfo]> {
        let description = Not::describe_props(&circ.props);
        self.input = description.pins[0].to_info();
        self.output = description.pins[1].to_info();
        vec![self.input.clone(), self.output.clone()].into_boxed_slice()
    }

    fn update_signals(&self, state_ctx: &CircuitStateContext, _: Option<usize>) {
        let state = self.input.get_state(state_ctx);
        let state = match state {
            WireState::None => WireState::None,
            WireState::True => WireState::False,
            WireState::False => WireState::True,
            WireState::Error => WireState::Error,
        };
        self.output.set_state(state_ctx, state);
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
        self.dir = circ.props.read_clone("dir").unwrap_or(Direction4::Right);
    }
}

pub struct NotPreview {}

impl CircuitPreviewImpl for NotPreview {

    fn type_name(&self) -> DynStaticStr {
        "not".into()
    }

    fn display_name(&self) -> DynStaticStr {
        "NOT gate".into()
    }

    fn description(&self) -> DynStaticStr {
        "NOT gate, performing logical NOT operation.\n\
         Inverts the binary value of the input signal.\n\
         \n\
         None and Error are passed through.\
        ".into()
    }

    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, in_world: bool) {
        let angle = props
            .read_clone("dir")
            .unwrap_or(Direction4::Right)
            .inverted_ud()
            .angle_to_right();
        Not::draw(ctx, angle, in_world);
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Not::new())
    }

    fn load_copy_data(
        &self,
        _imp: &serde_intermediate::Intermediate,
        _internal: &serde_intermediate::Intermediate,
        _ctx: &Arc<SimulationContext>,
        _errors: &mut ErrorList,
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        Some(Box::new(NotPreview {}))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        CircuitPropertyStore::new([CircuitProperty::new("dir", "Direction", Direction4::Right)])
    }

    fn describe(&self, props: &CircuitPropertyStore) -> crate::circuits::DynCircuitDescription {
        Not::describe_props(props).to_dyn()
    }
}
