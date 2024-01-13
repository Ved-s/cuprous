use std::ops::Div;

use eframe::epaint::Rounding;
use emath::lerp;

use crate::circuits::props::CircuitProperty;
use crate::ui::drawing::draw_line_gradient;
use crate::vector::Vec2f;
use crate::{circuits::*, describe_directional_circuit};

use super::props::{RangedValue, Slider};

#[derive(Serialize, Deserialize, Default)]
struct ClockState {
    state: bool,
    enabled: bool,

    #[serde(skip, default)]
    cycle_start_ts: Option<Instant>,
}

impl InternalCircuitState for ClockState {
    fn serialize(&self, _copy: bool) -> serde_intermediate::Intermediate {
        serde_intermediate::to_intermediate(self).unwrap_or_default()
    }
}

struct Clock {
    out: CircuitPinInfo,
    ctrl: Option<CircuitPinInfo>,

    frequency: f32,
    pwm: f32,
}

impl Clock {
    fn new() -> Self {
        let desc = Self::describe(Direction4::Right, false);
        Self {
            out: desc.pins[0].to_info(),
            ctrl: desc.pins[1].to_active_info(),
            frequency: 1.0,
            pwm: 0.5,
        }
    }

    #[track_caller]
    fn schedule_next_update(frequency: f32, pwm: f32, state: &mut ClockState) -> Option<Duration> {
        if pwm <= 0.0 || pwm >= 1.0 {
            return Some(Duration::ZERO);
        }

        let cycle_length = Duration::from_secs_f32(1.0 / frequency);
        //let now = Instant::now();

        let cycle_elapsed = state
            .cycle_start_ts
            .map(|s| Instant::now().checked_duration_since(s).unwrap_or_default());

        let time = match (state.state, state.enabled, cycle_elapsed) {
            (true, _, Some(elapsed)) => cycle_length.mul_f32(pwm).saturating_sub(elapsed),
            (true, _, None) => cycle_length.mul_f32(pwm),
            (false, true, Some(elapsed)) => cycle_length.saturating_sub(elapsed),
            (false, true, None) => cycle_length.mul_f32(1.0 - pwm),
            _ => {
                state.cycle_start_ts = None;
                return None;
            }
        };

        if state.enabled && !state.state {
            state.cycle_start_ts = Some(Instant::now() + time);
        }

        Some(time)
    }

    fn draw(pwm: f32, enabled: bool, ctx: &PaintContext, semi_transparent: bool) {
        let color_mul = if semi_transparent { 0.5 } else { 1.0 };
        ctx.paint.rect_filled(
            ctx.rect.expand(ctx.screen.scale * -0.5),
            Rounding::same(ctx.screen.scale * 0.25),
            Color32::from_gray(208).linear_multiply(color_mul),
        );

        let pwm = pwm.clamp(0.0, 1.0);

        let transformer = |x: f32, y: f32| ctx.rect.lerp_inside(Vec2f::new(x, y).div(3.0).into());

        if enabled {
            let y = lerp(2.0..=1.0, pwm);
            draw_line_gradient(
                [transformer(0.625, y), transformer(2.375, y)],
                ctx.screen.scale * 0.15,
                &[
                    (0.0, Color32::TRANSPARENT),
                    (0.15, Color32::from_gray(184)),
                    (0.85, Color32::from_gray(184)),
                    (1.0, Color32::TRANSPARENT),
                ],
                ctx.paint,
            );
        }

        let true_color = if enabled {
            ctx.style.wire_colors.true_color()
        } else {
            Color32::GRAY
        };
        let false_color = if enabled {
            ctx.style.wire_colors.false_color()
        } else {
            Color32::GRAY
        };

        if pwm > 0.0 && pwm < 1.0 {
            draw_line_gradient(
                [transformer(1.0, 1.93), transformer(1.0, 1.07)],
                ctx.screen.scale * 0.15,
                &[(0.0, false_color), (1.0, true_color)],
                ctx.paint,
            );
            draw_line_gradient(
                [transformer(pwm + 1.0, 1.93), transformer(pwm + 1.0, 1.07)],
                ctx.screen.scale * 0.15,
                &[(0.0, false_color), (1.0, true_color)],
                ctx.paint,
            );
            draw_line_gradient(
                [transformer(2.0, 1.93), transformer(2.0, 1.07)],
                ctx.screen.scale * 0.15,
                &[(0.0, false_color), (1.0, true_color)],
                ctx.paint,
            );

            draw_line_gradient(
                [transformer(0.625, 2.0), transformer(1.075, 2.0)],
                ctx.screen.scale * 0.15,
                &[
                    (0.0, Color32::TRANSPARENT),
                    (0.555555, false_color),
                    (1.0, false_color),
                ],
                ctx.paint,
            );
            draw_line_gradient(
                [transformer(1.925, 1.0), transformer(2.375, 1.0)],
                ctx.screen.scale * 0.15,
                &[
                    (0.0, true_color),
                    (0.444444, true_color),
                    (1.0, Color32::TRANSPARENT),
                ],
                ctx.paint,
            );

            ctx.paint.rect_filled(
                Rect::from_min_max(
                    transformer(0.925, 1.0 - 0.075).round(),
                    transformer(1.0 + pwm + 0.075, 1.0 + 0.075).round(),
                ),
                Rounding::ZERO,
                true_color,
            );

            ctx.paint.rect_filled(
                Rect::from_min_max(
                    transformer(1.0 + pwm - 0.075, 2.0 - 0.075).round(),
                    transformer(2.075, 2.0 + 0.075).round(),
                ),
                Rounding::ZERO,
                false_color,
            );

            // ctx.paint.line_segment(
            //     [transformer(0.925, 1.0), transformer(1.0 + pwm + 0.075, 1.0)],
            //     Stroke::new(ctx.screen.scale * 0.15, true_color),
            // );
            // ctx.paint.line_segment(
            //     [transformer(1.0 + pwm - 0.075, 2.0), transformer(2.075, 2.0)],
            //     Stroke::new(ctx.screen.scale * 0.15, false_color),
            // );
        } else {
            let color = if pwm > 0.5 { true_color } else { false_color };
            let y = lerp(2.0..=1.0, pwm);
            draw_line_gradient(
                [transformer(0.625, y), transformer(2.375, y)],
                ctx.screen.scale * 0.15,
                &[
                    (0.0, Color32::TRANSPARENT),
                    (0.15, color),
                    (0.85, color),
                    (1.0, Color32::TRANSPARENT),
                ],
                ctx.paint,
            );
        }
    }

    fn describe_props(props: &CircuitPropertyStore) -> CircuitDescription<2> {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Right);
        let ctrl = props.read_clone("ctrl").unwrap_or(false);
        Self::describe(dir, ctrl)
    }

    fn describe(dir: Direction4, ctrl: bool) -> CircuitDescription<2> {
        describe_directional_circuit! {
            default_dir: Direction4::Right,
            dir: dir,
            size: [3, 3],
            "out": Outside, "Out", Right, [2, 1],
            "ctrl": Inside, "Control", Left, [0, 1], active: ctrl
        }
    }
}

impl CircuitImpl for Clock {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext) {
        let enabled = state_ctx
            .read_circuit_internal_state(|s: &ClockState| s.enabled || s.state)
            .unwrap_or_default();
        Clock::draw(self.pwm, enabled, paint_ctx, false);
    }

    fn create_pins(&mut self, circ: &Arc<Circuit>) -> Box<[CircuitPinInfo]> {
        let description = Self::describe_props(&circ.props);
        self.out = description.pins[0].to_info();
        self.ctrl = description.pins[1].to_active_info();
        let mut pins = vec![self.out.clone()];
        if let Some(ctrl) = self.ctrl.clone() {
            pins.push(ctrl);
        }
        pins.into()
    }

    fn update_signals(&self, state_ctx: &CircuitStateContext, changed_pin: Option<usize>) {
        if let None | Some(1) = changed_pin {
            let enabled = !self
                .ctrl
                .as_ref()
                .is_some_and(|ctrl| ctrl.get_state(state_ctx) != WireState::True);
            let (time, state) = state_ctx.write_circuit_internal_state(|s: &mut ClockState| {
                if s.enabled == enabled {
                    return (None, s.state);
                }
                s.enabled = enabled;
                if enabled {
                    s.state = true;
                }

                (
                    Some(Self::schedule_next_update(self.frequency, self.pwm, s)),
                    s.state,
                )
            });
            if let Some(time) = time {
                state_ctx.set_update_interval(time);
            }
            self.out.set_state(state_ctx, state.into());
        }
        if let None | Some(0) = changed_pin {
            self.out.set_state(
                state_ctx,
                state_ctx
                    .read_circuit_internal_state(|s: &ClockState| s.state)
                    .unwrap_or(false)
                    .into(),
            );
        }
    }

    fn update(&self, ctx: &CircuitStateContext, interval: &mut Option<Duration>) {
        let state = ctx.write_circuit_internal_state(|s: &mut ClockState| {
            if self.pwm <= 0.0 || self.pwm >= 1.0 {
                let state = self.pwm >= 0.5;
                s.state = state;
                return state;
            }

            if s.state || s.enabled {
                s.state = !s.state;
            }
            *interval = Self::schedule_next_update(self.frequency, self.pwm, s);
            s.state
        });

        self.out.set_state(ctx, state.into());
    }

    fn state_init(&self, ctx: &CircuitStateContext, first_init: bool) {
        let enabled = !self
            .ctrl
            .as_ref()
            .is_some_and(|ctrl| ctrl.get_state(ctx) != WireState::True);
        let time = ctx.write_circuit_internal_state(|s: &mut ClockState| {
            s.enabled = enabled;
            first_init.then(|| Self::schedule_next_update(self.frequency, self.pwm, s))
        });
        if let Some(time) = time {
            ctx.set_update_interval(time);
        }
    }

    fn size(&self, _circ: &Arc<Circuit>) -> Vec2u {
        [3, 3].into()
    }

    fn prop_changed(&self, prop_id: &str, _resize: &mut bool, recreate_pins: &mut bool) {
        match prop_id {
            "dir" | "ctrl" => *recreate_pins = true,
            _ => {}
        }
    }

    fn apply_props(&mut self, circ: &Arc<Circuit>, prop: Option<&str>) {
        if let None | Some("freq") = prop {
            self.frequency = circ
                .props
                .read("freq", |v: &RangedValue<f32>| v.get())
                .unwrap_or(1.0);
        }
        if let None | Some("pwm") = prop {
            self.pwm = circ
                .props
                .read("pwm", |v: &Slider<f32>| v.value)
                .unwrap_or(0.5);
        }

        if let None | Some("freq") | Some("pwm") = prop {
            for state in circ.board.states.states.read().iter() {
                let ctx = CircuitStateContext::new(state.clone(), circ.clone());
                let time = ctx.write_circuit_internal_state(|s: &mut ClockState| {
                    Self::schedule_next_update(self.frequency, self.pwm, s)
                });
                ctx.set_update_interval(time);
            }
        }
    }

    fn load_internal(
        &self,
        _ctx: &CircuitStateContext,
        data: &serde_intermediate::Intermediate,
        _paste: bool,
        _errors: &mut ErrorList,
    ) -> Option<Box<dyn InternalCircuitState>> {
        serde_intermediate::from_intermediate::<ClockState>(data)
            .ok()
            .map(|v| Box::new(v) as _)
    }
}

pub struct Preview {}

impl CircuitPreviewImpl for Preview {
    fn type_name(&self) -> DynStaticStr {
        "clock".into()
    }

    fn display_name(&self) -> DynStaticStr {
        "Clock".into()
    }

    fn description(&self) -> DynStaticStr {
        "TODO".into()
    }

    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, in_world: bool) {
        let pwm = props.read("pwm", |v: &Slider<f32>| v.value).unwrap_or(0.5);
        Clock::draw(pwm, true, ctx, in_world);
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Clock::new())
    }

    fn load_copy_data(
        &self,
        _imp: &serde_intermediate::Intermediate,
        _internal: &serde_intermediate::Intermediate,
        _ctx: &Arc<SimulationContext>,
        _errors: &mut ErrorList,
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        Some(Box::new(Preview {}))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        CircuitPropertyStore::new([
            CircuitProperty::new("dir", "Direction", Direction4::Right),
            CircuitProperty::new("ctrl", "Controlled", false),
            CircuitProperty::new(
                "freq",
                "Frequency (Hz)",
                RangedValue::new(0.0..=f32::MAX, 1.0, 1.0),
            ),
            CircuitProperty::new(
                "pwm",
                "PWM Fill",
                Slider {
                    range: 0.0..=1.0,
                    value: 0.5f32,
                },
            ),
        ])
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        Clock::describe_props(props).to_dyn()
    }
}
