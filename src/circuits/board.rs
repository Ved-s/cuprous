use crate::{
    board::{CircuitBoard, CircuitDesign, CircuitDesignId},
    circuits::*,
    MaybeResolved,
};

struct ResolvedCircuitData {
    state: Option<Arc<State>>,
    design: Option<Arc<CircuitDesign>>,
}

struct Circuit {
    board: Option<Arc<RwLock<CircuitBoard>>>,
    inner: Option<MaybeResolved<usize, Arc<State>>>,
    design: CircuitDesignId,
}

// TODO: handle state destruction on remove
impl Circuit {
    fn new() -> Self {
        Self {
            board: None,
            inner: None,
            design: CircuitDesignId::new_resolved(None),
        }
    }

    fn resolve_data(&self) -> ResolvedCircuitData {
        let board = self.board.as_ref().map(|b| (b, b.read()));
        ResolvedCircuitData {
            state: board.as_ref().and_then(|(arcb, b)| {
                self.inner
                    .as_ref()
                    .map(|i| i.resolve(|i| b.states.get_or_create((*arcb).clone(), *i)))
            }),
            design: board.as_ref().and_then(|(_, b)| self.design.resolve(|| &b.designs)),
        }
    }

    fn draw(
        data: ResolvedCircuitData,
        state: Option<&CircuitStateContext>,
        ctx: &PaintContext,
        semi_transparent: bool,
    ) {
        todo!()
    }

    fn describe_props(
        data: ResolvedCircuitData,
        props: &CircuitPropertyStore,
    ) -> CircuitDescription<0> {
        Self::describe(data)
    }

    fn describe(data: ResolvedCircuitData) -> CircuitDescription<0> {
        todo!()
    }
}

impl CircuitImpl for Circuit {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext) {
        Circuit::draw(Some(state_ctx), paint_ctx, false);
    }

    fn create_pins(&mut self, props: &CircuitPropertyStore) -> Box<[CircuitPinInfo]> {
        let description = Self::describe_props(props);
        vec![].into_boxed_slice()
    }

    fn update_signals(&self, state_ctx: &CircuitStateContext, changed_pin: Option<usize>) {
        todo!()
    }

    fn size(&self, props: &CircuitPropertyStore) -> Vec2u {
        Self::describe_props(props).size
    }
}

pub struct Preview {
    board: Option<Arc<RwLock<CircuitBoard>>>,
}

impl CircuitPreviewImpl for Preview {
    fn type_name(&self) -> DynStaticStr {
        "board".into()
    }

    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, in_world: bool) {}

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        if let Some(board) = &self.board {
            let new_state = board.read().states.create_state(board.clone());
            Box::new(Circuit {
                inner: Some(MaybeResolved::new_resolved(new_state)),
            })
        } else {
            Box::new(Circuit::new())
        }
    }

    fn load_impl_data(
        &self,
        _: &serde_intermediate::Intermediate,
        boards: &BoardStorage,
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        todo!()
    }

    fn default_props(&self) -> CircuitPropertyStore {
        todo!()
    }

    fn display_name(&self) -> DynStaticStr {
        todo!()
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        todo!()
    }
}
