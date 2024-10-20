pub mod and;

use std::sync::Arc;

use traitbox::traitbox;

use crate::{state::WireState, str::ArcStaticStr, vector::Vec2usize, Direction8};

use super::{Circuit, CircuitCtx, CircuitImpl, CircuitPin, CircuitRenderingContext, CircuitTransform, PinDescription, PinType};

struct GateOutput {
    out: bool,
    fin: bool,
}

trait GateImpl {
    fn id() -> &'static str;
    fn display_name() -> &'static str;

    fn init_state() -> bool;
    fn fold(state: &mut bool, input: bool) -> GateOutput;
    fn draw(ctx: &CircuitRenderingContext);
}

traitbox! {
    box GateImplBox;

    #[as_impl]
    trait GateImpl {
        fn id() -> &'static str;
        fn display_name() -> &'static str;
        fn init_state() -> bool;
        fn fold(state: &mut bool, input: bool) -> GateOutput;
        fn draw(ctx: &CircuitRenderingContext);
    }

    trait Clone {
        fn clone(&self) -> Self;
    }

    auto trait Send {}
    auto trait Sync {}
}

#[derive(Clone)]
pub struct Gate {
    imp: GateImplBox,
}

impl Gate {
    pub fn and() -> Self {
        Self { imp: GateImplBox::new(and::And) }
    }
}

pub struct GateInstance {
    inputs: Box<[Arc<CircuitPin>]>,
    output: Arc<CircuitPin>,
}

impl CircuitImpl for Gate {
    type State = ();

    type Instance = GateInstance;

    fn id(&self) -> ArcStaticStr {
        self.imp.id().into()
    }

    fn display_name(&self) -> ArcStaticStr {
        self.imp.display_name().into()
    }

    fn size(&self, _: CircuitTransform) -> Vec2usize {
        [4, 3].into()
    }

    fn occupies_quarter(&self, _: CircuitTransform, qpos: Vec2usize) -> bool {
        qpos.x != 0 && qpos.x != 7
    }

    fn describe_pins(&self, _: CircuitTransform) -> Box<[PinDescription]> {
        [
            PinDescription {
                pos: [0, 0].into(),
                id: "in_0".into(),
                display_name: "A".into(),
                dir: Some(Direction8::Left),
                ty: PinType::Inside,
            },
            PinDescription {
                pos: [0, 2].into(),
                id: "in_1".into(),
                display_name: "B".into(),
                dir: Some(Direction8::Left),
                ty: PinType::Inside,
            },
            PinDescription {
                pos: [3, 1].into(),
                id: "out".into(),
                display_name: "Out".into(),
                dir: Some(Direction8::Right),
                ty: PinType::Outside,
            }
        ].into()
    }

    fn draw(&self, _: Option<CircuitCtx<Self>>, render: &CircuitRenderingContext) {
        self.imp.draw(render);
    }

    fn create_instance(&self, circuit: &Arc<Circuit>) -> Self::Instance {
        let pins = circuit.pins.read();

        GateInstance {
            inputs: pins[..pins.len()-1].iter().map(|p| p.pin.clone()).collect(),
            output: pins[pins.len()-1].pin.clone(),
        }
    }

    // TODO: let user select what to do with None inputs

    fn update_signals(&self, mut ctx: CircuitCtx<Self>, _: Option<usize>) {

        let inputs = &ctx.instance.inputs;

        let output = 'compute: {

            let mut gate_state = self.imp.init_state();

            let mut out = false;
            let mut fin = false;
            let mut any = false;

            for input in inputs {
                let state = ctx.get_pin_input(input);
                match state {
                    WireState::None => continue,
                    WireState::Bool(b) => {
                        if fin {
                            continue;
                        }

                        let res = self.imp.fold(&mut gate_state, b);
                        out = res.out;
                        fin = res.fin;
                        any = true;

                    },
                    WireState::Error => {
                        break 'compute WireState::Error;
                    },
                }
            }

            if any {
                WireState::Bool(out)
            } else {
                WireState::None
            }
        };

        ctx.set_pin_output(&ctx.instance.output, output);
    }
}

