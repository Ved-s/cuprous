use std::{ops::Deref, sync::Arc, time::Duration};

use eframe::egui::{Color32, FontId, Rect};

use crate::{
    Direction4, Direction8,
    components::ComponentUpdateReason,
    editor::QuarterPos,
    ext::IteratorProduct,
    pool::get_pooled,
    state::wires::WireState,
    str::ArcStaticStr,
    vector::Vec2usize,
    vertex_renderer::{ColoredTriangleBuffer, ColoredVertexRenderer},
};

use super::{
    Component, ComponentCtx, ComponentFlipSupport, ComponentImpl, ComponentPin, ComponentRenderingContext,
    ComponentRotationSupport, ComponentTransform, ComponentTransformSupport, FlipType, PinDescription,
    PinType, TransformSupport,
};

#[allow(unused)]
pub struct TestComponentInstance {
    pin_a: Arc<ComponentPin>,
    pin_b: Arc<ComponentPin>,
    pin_c: Arc<ComponentPin>,
    pin_d: Arc<ComponentPin>,
    pin_e: Arc<ComponentPin>,
}

#[derive(Default)]
pub struct TestComponentState {
    count: usize,
    clock: bool,
}

#[derive(Clone)]
pub struct Test;

impl ComponentImpl for Test {
    type State = TestComponentState;
    type Instance = TestComponentInstance;

    fn id(&self) -> ArcStaticStr {
        "test".into()
    }

    fn display_name(&self) -> ArcStaticStr {
        "Test component".into()
    }

    fn size(&self, _transform: ComponentTransform) -> Vec2usize {
        [4, 3].into()
    }

    fn occupies_quarter(&self, _transform: ComponentTransform, qpos: Vec2usize) -> bool {
        const QUARTERS: [[usize; 8]; 6] = [
            [0, 0, 0, 0, 0, 0, 0, 0],
            [0, 1, 1, 1, 1, 1, 0, 0],
            [0, 1, 1, 1, 1, 1, 1, 0],
            [0, 1, 1, 1, 1, 1, 1, 0],
            [0, 1, 1, 0, 1, 1, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0],
        ];

        QUARTERS[qpos.y][qpos.x] != 0
    }

    fn draw(&self, component: Option<ComponentCtx<Self>>, render: &ComponentRenderingContext) {
        let size = self.size(render.transform);

        let mut buffer = get_pooled::<ColoredTriangleBuffer>();

        for ((y, x), q) in (0..size.y)
            .product_clone(0..size.x)
            .product_clone(QuarterPos::ALL.iter().copied())
        {
            let pos = Vec2usize::new(x, y);
            let quarter_pos = pos * 2 + q.into_position();
            if !self.occupies_quarter(render.transform, quarter_pos) {
                continue;
            }

            let pos = pos.convert(|v| v as f32) + q.into_quarter_position_f32();

            let tl = render.transform_pos(pos);
            let br = render.transform_pos(pos + 0.5);

            let rect = Rect::from_two_pos(tl.into(), br.into());

            buffer.add_new_rect(
                rect.left_top(),
                rect.size(),
                Color32::WHITE.to_normalized_gamma_f32(),
            )
        }

        render.paint.custom_draw(move |ctx| {
            let mut vertexes = ColoredVertexRenderer::global(ctx.painter.gl());
            vertexes.draw(
                ctx.painter.gl(),
                ctx.paint_info.screen_size_px,
                buffer.deref(),
            );
        });

        let count = component
            .as_ref()
            .and_then(|c| c.read_internal_state().map(|s| s.count));

        if let Some(count) = count {
            let font = FontId {
                size: render.paint.screen.scale,
                family: eframe::egui::FontFamily::Monospace,
            };

            render.paint.painter.text(
                render.screen_rect.center(),
                eframe::egui::Align2::CENTER_CENTER,
                count,
                font,
                Color32::BLACK,
            );
        }
    }

    fn describe_pins(&self, _transform: ComponentTransform) -> Box<[PinDescription]> {
        [
            PinDescription {
                pos: [0, 0].into(),
                id: "a".into(),
                display_name: "A".into(),
                dir: Some(Direction8::UpLeft),
                ty: PinType::Outside,
            },
            PinDescription {
                pos: [0, 2].into(),
                id: "b".into(),
                display_name: "B".into(),
                dir: Some(Direction8::DownLeft),
                ty: PinType::Inside,
            },
            PinDescription {
                pos: [2, 0].into(),
                id: "c".into(),
                display_name: "C".into(),
                dir: Some(Direction8::Up),
                ty: PinType::Inside,
            },
            PinDescription {
                pos: [2, 2].into(),
                id: "d".into(),
                display_name: "D".into(),
                dir: Some(Direction8::Down),
                ty: PinType::Inside,
            },
            PinDescription {
                pos: [3, 1].into(),
                id: "e".into(),
                display_name: "E".into(),
                dir: Some(Direction8::Right),
                ty: PinType::Outside,
            },
        ]
        .into()
    }

    fn transform_support(&self) -> ComponentTransformSupport {
        ComponentTransformSupport {
            rotation: Some(ComponentRotationSupport {
                support: TransformSupport::Automatic,
                default_dir: Direction4::Right,
            }),
            flip: Some(ComponentFlipSupport {
                support: TransformSupport::Automatic,
                ty: FlipType::Vertical,
            }),
        }
    }

    fn create_instance(&self, component: &Arc<Component>) -> Self::Instance {
        let pins = component.pins.read();
        TestComponentInstance {
            pin_a: pins[0].pin.clone(),
            pin_b: pins[1].pin.clone(),
            pin_c: pins[2].pin.clone(),
            pin_d: pins[3].pin.clone(),
            pin_e: pins[4].pin.clone(),
        }
    }

    fn pins_changed(&self, component: &Component, instance: &mut Self::Instance) {
        let pins = component.pins.read();

        instance.pin_a = pins[0].pin.clone();
        instance.pin_b = pins[1].pin.clone();
        instance.pin_c = pins[2].pin.clone();
        instance.pin_d = pins[3].pin.clone();
        instance.pin_e = pins[4].pin.clone();
    }

    fn update(&self, mut ctx: ComponentCtx<Self>, reason: ComponentUpdateReason) {
        if let ComponentUpdateReason::ChangedPin(pin) = reason {
            ctx.write_internal_state().count += 1;

            if pin == 1 {
                let state = ctx.get_pin_input(&ctx.instance.pin_b);
                if let WireState::Bool(b) = state {
                    if !b {
                        ctx.reset_timer();
                    } else if ctx.get_timer().is_none() {
                        ctx.set_timer(ctx.time_provider().now(), Some(Duration::from_secs(1)));
                    }
                }
            }
        }

        if let ComponentUpdateReason::Timer = reason {
            let state = ctx.write_internal_state();
            state.clock = !state.clock;
            let clock = state.clock;

            ctx.set_pin_output(&ctx.instance.pin_a, WireState::Bool(clock));
        }

        ctx.set_pin_output(&ctx.instance.pin_e, WireState::Bool(true));
    }
}
