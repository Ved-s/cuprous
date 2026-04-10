use std::sync::Arc;

use eframe::{
    egui::{Color32, Pos2, Stroke},
    epaint::{PathShape, PathStroke},
};

use crate::{
    Direction8,
    components::{
        Component, ComponentCtx, ComponentImpl, ComponentPin, ComponentRenderingContext, ComponentTransform,
        ComponentUpdateReason, PinDescription, PinType,
    },
    state::wires::WireState,
    str::ArcStaticStr,
    vector::Vec2usize,
};

#[derive(Clone)]
pub struct Not;

pub struct NotInstance {
    input: Arc<ComponentPin>,
    output: Arc<ComponentPin>,
}

impl ComponentImpl for Not {
    type State = ();

    type Instance = NotInstance;

    fn id(&self) -> ArcStaticStr {
        "gate_not".into()
    }

    fn display_name(&self) -> ArcStaticStr {
        "NOT gate".into()
    }

    fn size(&self, _transform: ComponentTransform) -> Vec2usize {
        [2, 1].into()
    }

    fn occupies_quarter(&self, _transform: ComponentTransform, qpos: Vec2usize) -> bool {
        qpos.x >= 1 && qpos.x <= 2
    }

    fn describe_pins(&self, _transform: ComponentTransform) -> Box<[PinDescription]> {
        [
            PinDescription {
                pos: [0, 0].into(),
                id: "in".into(),
                display_name: "In".into(),
                dir: Some(Direction8::Left),
                ty: PinType::Inside,
            },
            PinDescription {
                pos: [1, 0].into(),
                id: "out".into(),
                display_name: "Out".into(),
                dir: Some(Direction8::Right),
                ty: PinType::Outside,
            },
        ]
        .into()
    }

    fn draw(&self, _component: Option<ComponentCtx<Self>>, ctx: &ComponentRenderingContext) {
        let border_color = Color32::BLACK;
        let fill_color = Color32::from_gray(200);

        let triangle_points: Vec<Pos2> = vec![
            ctx.transform_pos([0.5, 0.1].into()).into(),
            ctx.transform_pos([1.5, 0.5].into()).into(),
            ctx.transform_pos([0.5, 0.9].into()).into(),
        ];

        ctx.paint.add(PathShape {
            points: triangle_points,
            closed: true,
            fill: fill_color,
            stroke: PathStroke::new(0.15 * ctx.paint.screen.scale, border_color),
        });

        let circle_pos = ctx.transform_pos([1.32, 0.5].into());
        ctx.paint.circle(
            circle_pos.into(),
            0.2 * ctx.paint.screen.scale,
            fill_color,
            Stroke::new(0.15 * ctx.paint.screen.scale, border_color),
        );
    }

    fn create_instance(&self, component: &Arc<Component>) -> Self::Instance {
        let pins = component.pins.read();
        NotInstance {
            input: pins[0].pin.clone(),
            output: pins[1].pin.clone(),
        }
    }

    fn pins_changed(&self, component: &Component, instance: &mut Self::Instance) {
        let pins = component.pins.read();

        instance.input = pins[0].pin.clone();
        instance.output = pins[1].pin.clone();
    }

    fn update(&self, ctx: ComponentCtx<Self>, _reason: ComponentUpdateReason) {
        let val = ctx.instance.input.get_state(&ctx.state.components);
        let out = match val {
            WireState::None => WireState::None,
            WireState::Bool(b) => WireState::Bool(!b),
            WireState::Error => WireState::Error,
        };
        ctx.instance
            .output
            .set_output(&mut ctx.state.components, ctx.tasks, out);
    }
}
