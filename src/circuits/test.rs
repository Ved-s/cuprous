
use eframe::epaint::{Rounding, Color32};
use emath::Align2;

use super::*;

#[derive(Default, Serialize, Deserialize)]
pub struct State {
    state: bool,
    dir_in: bool,
}

impl InternalCircuitState for State {
    fn serialize(&self) -> serde_intermediate::Intermediate {
        serde_intermediate::to_intermediate(self).unwrap()
    }
}

pub struct Circuit {
    clock_pin: CircuitPinInfo,
    dir_pin: CircuitPinInfo,
    io_pin: CircuitPinInfo,
}

impl Circuit {
    fn new() -> Self {
        Self {
            clock_pin: CircuitPinInfo::new([0, 0], InternalPinDirection::Outside, "clock"),
            dir_pin: CircuitPinInfo::new([0, 1], InternalPinDirection::Inside, "dir"),
            io_pin: CircuitPinInfo::new(
                [1, 1],
                InternalPinDirection::StateDependent {
                    default: PinDirection::Outside,
                },
                "io"
            ),
        }
    }
}

impl CircuitImpl for Circuit {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext) {
        let font_id = eframe::egui::TextStyle::Monospace.resolve(paint_ctx.ui.style());

        let rect = {
            let mut r = paint_ctx.rect;
            *r.left_mut() += paint_ctx.screen.scale / 2.0;
            *r.right_mut() -= paint_ctx.screen.scale / 2.0;
            r
        };
        paint_ctx
            .paint
            .rect_filled(rect, Rounding::none(), Color32::from_gray(100));

        let wire = self.clock_pin.pin.read().unwrap().wire.map(|v| v as i32);

        paint_ctx.paint.text(
            rect.center_bottom(),
            Align2::CENTER_BOTTOM,
            state_ctx.circuit.id.to_string(),
            font_id.clone(),
            Color32::WHITE,
        );

        if let Some(wire) = wire {
            let pos = state_ctx.circuit.pos + self.clock_pin.pos.convert(|v| v as i32);
            let pos = paint_ctx.screen.world_to_screen_tile(pos) + paint_ctx.screen.scale / 2.0;
            let pos = pos + [paint_ctx.screen.scale / 4.0, 0.0];

            paint_ctx.paint.text(
                pos.into(),
                Align2::LEFT_CENTER,
                wire.to_string(),
                font_id,
                Color32::WHITE,
            );
        }
    }

    fn create_pins(&self) -> Box<[CircuitPinInfo]> {
        vec![
            self.clock_pin.clone(),
            self.dir_pin.clone(),
            self.io_pin.clone(),
        ]
        .into_boxed_slice()
    }

    fn update_signals(&mut self, state_ctx: &CircuitStateContext, pin: Option<usize>) {
        match pin {
            Some(1) => {
                let new_dir_in = matches!(self.dir_pin.get_input(state_ctx), WireState::True);
                let dir = state_ctx.write_circuit_internal_state::<State, _>(|cs| {
                    if cs.dir_in == new_dir_in {
                        None
                    } else {
                        cs.dir_in = new_dir_in;
                        Some(if new_dir_in {
                            PinDirection::Inside
                        } else {
                            PinDirection::Outside
                        })
                    }
                });
                if let Some(dir) = dir {
                    self.io_pin.set_direction(state_ctx, dir);
                }
            }
            None | Some(2) => {
                let dir_in =
                    state_ctx.read_circuit_internal_state::<State, _>(|cs| cs.dir_in);
                if !dir_in.is_some_and(|i| i) {
                    self.io_pin.set_output(state_ctx, WireState::True);
                }
            }
            _ => {}
        }

        if pin.is_none() {
            self.clock_pin.set_output(
                state_ctx,
                state_ctx
                    .read_circuit_internal_state::<State, _>(|s| s.state)
                    .unwrap_or_default()
                    .into(),
            );
        }
    }

    fn update(&mut self, state_ctx: &CircuitStateContext) {
        let new_state = state_ctx.write_circuit_internal_state::<State, _>(|s| {
            s.state = !s.state;
            s.state
        });

        self.clock_pin.set_output(state_ctx, new_state.into());
    }

    fn update_interval(&self, state_ctx: &CircuitStateContext) -> Option<Duration> {
        match state_ctx
            .read_circuit_internal_state::<State, _>(|s| s.state)
            .unwrap_or_default()
        {
            true => Some(Duration::from_millis(200)),
            false => Some(Duration::from_secs(2)),
        }
    }

    fn init_state(&self, state_ctx: &CircuitStateContext) {
        state_ctx.set_update_interval(self.update_interval(state_ctx));
    }
}

#[derive(Debug)]
pub struct Preview {}

impl CircuitPreview for Preview {
    fn draw_preview(&self, ctx: &PaintContext, _: bool) {
        ctx.paint.rect_filled(
            ctx.rect,
            Rounding::none(),
            Color32::from_rgba_unmultiplied(0, 255, 0, 100),
        );
    }

    fn size(&self) -> Vec2u {
        [2, 2].into()
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Circuit::new())
    }

    fn type_name(&self) -> DynStaticStr {
        "test".into()
    }
}