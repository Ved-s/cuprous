use std::sync::Arc;

use eframe::{
    egui::{Color32, Pos2, Rect, Stroke},
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
    vector::{Vec2f, Vec2usize},
};

#[derive(Clone)]
pub struct ErrorFilter;

pub struct ErrorFilterInstance {
    input: Arc<ComponentPin>,
    output: Arc<ComponentPin>,
}

impl ComponentImpl for ErrorFilter {
    type State = ();

    type Instance = ErrorFilterInstance;

    fn id(&self) -> ArcStaticStr {
        "error_filter".into()
    }

    fn display_name(&self) -> ArcStaticStr {
        "Error filter".into()
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

        let tl = ctx.transform_pos([0.0, -0.1].into());
        let br = ctx.transform_pos([1.5 + 0.075, 1.1].into());

        let min = Vec2f::new(tl.x.min(br.x), tl.y.min(br.y));
        let max = Vec2f::new(tl.x.max(br.x), tl.y.max(br.y));

        let clip = Rect::from_min_max(min.into(), max.into());

        ctx.paint.with_clip_rect(clip).add(PathShape {
            points: triangle_points,
            closed: true,
            fill: fill_color,
            stroke: PathStroke::new(0.15 * ctx.paint.screen.scale, border_color),
        });

        let a = ctx.transform_pos([1.5, 0.1].into());
        let b = ctx.transform_pos([1.5, 0.9].into());

        ctx.paint.line_segment(
            [a.into(), b.into()],
            Stroke::new(0.15 * ctx.paint.screen.scale, border_color),
        );
    }

    fn create_instance(&self, component: &Arc<Component>) -> Self::Instance {
        let pins = component.pins.read();
        ErrorFilterInstance {
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
            WireState::Bool(v) => WireState::Bool(v),
            WireState::Error => {
                return;
            }
        };

        ctx.instance
            .output
            .set_output(&mut ctx.state.components, ctx.tasks, out);
    }
}
