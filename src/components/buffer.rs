use std::sync::Arc;

use eframe::{
    egui::{Color32, Pos2},
    epaint::{PathShape, PathStroke},
};

use crate::{
    Direction8,
    components::{
        Component, ComponentCtx, ComponentImpl, ComponentPin, ComponentRenderingContext, ComponentTransform,
        ComponentUpdateReason, PinDescription, PinType,
    },
    str::ArcStaticStr,
    vector::Vec2usize,
};

#[derive(Clone)]
pub struct Buffer;

pub struct BufferInstance {
    input: Arc<ComponentPin>,
    output: Arc<ComponentPin>,
}

impl ComponentImpl for Buffer {
    type State = ();

    type Instance = BufferInstance;

    fn id(&self) -> ArcStaticStr {
        "buffer".into()
    }

    fn display_name(&self) -> ArcStaticStr {
        "Buffer".into()
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
    }

    fn create_instance(&self, component: &Arc<Component>) -> Self::Instance {
        let pins = component.pins.read();
        BufferInstance {
            input: pins[0].pin.clone(),
            output: pins[1].pin.clone(),
        }
    }

    fn update(&self, ctx: ComponentCtx<Self>, _reason: ComponentUpdateReason) {
        let state = ctx.instance.input.get_state(&ctx.state.components);
        ctx.instance
            .output
            .set_output(&mut ctx.state.components, ctx.tasks, state);
    }
}
