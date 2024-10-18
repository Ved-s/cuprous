use std::{ops::Deref, sync::Arc};

use eframe::egui::{Color32, FontId, Rect};

use crate::{editor::QuarterPos, ext::IteratorProduct, pool::get_pooled, state::WireState, str::ArcStaticStr, vector::Vec2usize, vertex_renderer::{ColoredTriangleBuffer, ColoredVertexRenderer}, Direction4, Direction8};

use super::{Circuit, CircuitCtx, CircuitFlipSupport, CircuitImpl, CircuitPin, CircuitRenderingContext, CircuitRotationSupport, CircuitTransform, CircuitTransformSupport, FlipType, PinDescription, PinType, TransformSupport};


pub struct TestCircuitInstance {
    pin_a: Arc<CircuitPin>,
    pin_b: Arc<CircuitPin>,
    pin_c: Arc<CircuitPin>,
    pin_d: Arc<CircuitPin>,
    pin_e: Arc<CircuitPin>,
}

#[derive(Default)]
pub struct TestCircuitState {
    count: usize,
}

#[derive(Clone)]
pub struct TestCircuit;

impl CircuitImpl for TestCircuit {
    type State = TestCircuitState;
    type Instance = TestCircuitInstance;

    fn id(&self) -> ArcStaticStr {
        "test".into()
    }

    fn display_name(&self) -> ArcStaticStr {
        "Test circuit".into()
    }

    fn size(&self, _transform: CircuitTransform) -> Vec2usize {
        [4, 3].into()
    }

    fn occupies_quarter(&self, _transform: CircuitTransform, qpos: Vec2usize) -> bool {
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

    fn draw(&self, mut circuit: Option<CircuitCtx<Self>>, render: &CircuitRenderingContext) {
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

        let count = circuit
            .as_ref()
            .and_then(|c| c.read_internal_state(|s| s.count));

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

        if let Some(cir) = &mut circuit {
            let blink_state = render.paint.ui.ctx().frame_nr() % 120 >= 60;
            cir.instance
                .pin_a
                .set_output(cir.state, cir.tasks, WireState::Bool(blink_state));
        }
    }

    fn describe_pins(&self, _transform: CircuitTransform) -> Box<[PinDescription]> {
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

    fn transform_support(&self) -> CircuitTransformSupport {
        CircuitTransformSupport {
            rotation: Some(CircuitRotationSupport {
                support: TransformSupport::Automatic,
                default_dir: Direction4::Right,
            }),
            flip: Some(CircuitFlipSupport {
                support: TransformSupport::Automatic,
                ty: FlipType::Vertical,
            }),
        }
    }

    fn create_instance(&self, circuit: &Arc<Circuit>) -> Self::Instance {
        let pins = circuit.pins.read();
        TestCircuitInstance {
            pin_a: pins[0].pin.clone(),
            pin_b: pins[1].pin.clone(),
            pin_c: pins[2].pin.clone(),
            pin_d: pins[3].pin.clone(),
            pin_e: pins[4].pin.clone(),
        }
    }
    fn update_signals(&self, mut ctx: CircuitCtx<Self>, changed_pin: Option<usize>) {
        if changed_pin.is_some_and(|c| c < 4) {
            ctx.write_internal_state(|s| s.count += 1);
        }

        ctx.set_pin_output(&ctx.instance.pin_e, WireState::Bool(true));
    }
}