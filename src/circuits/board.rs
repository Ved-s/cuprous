use std::{collections::HashSet, ops::Deref};

use bimap::BiMap;

use crate::{
    board::{CircuitBoard, CircuitDesign},
    circuits::*,
    unwrap_option_or_return,
};

#[derive(Clone, Default)]
struct ResolvedCircuitData {
    board: Option<Arc<RwLock<CircuitBoard>>>,
    state: Option<Arc<State>>,
    design: Option<Arc<CircuitDesign>>,
}

#[derive(Debug, Clone, Copy)]
struct UnresolvedCircuitData {
    board: u128,
    state: usize,
    design: OptionalInt<usize>,
}
impl UnresolvedCircuitData {
    fn resolve_into(
        &self,
        resolved: &mut ResolvedCircuitData,
        boards: &BoardStorage,
    ) {
        if resolved.board.is_none() {
            resolved.board = boards.get(&self.board).cloned();
        }

        if let Some(board) = &resolved.board {
            if resolved.state.is_none() {
                resolved.state = board.read().states.get(self.state);
            }

            if resolved.design.is_none() {
                resolved.design = match self.design.get() {
                    Some(id) => board.read().designs.get(id),
                    None => Some(board.read().designs.current()),
                };
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
struct CircuitDataModel {
    board: u128,
    state: usize,
    design: usize,
}

impl CircuitDataModel {
    fn into_unresolved(self) -> UnresolvedCircuitData {
        UnresolvedCircuitData {
            board: self.board,
            state: self.state,
            design: OptionalInt::new(self.design),
        }
    }
}

#[derive(Default)]
struct Circuit {
    unresolved: Option<UnresolvedCircuitData>,
    resolved: ResolvedCircuitData,

    // Inner pin, outer pin
    // Resolves while executing
    pinmap: RwLock<BiMap<usize, usize>>,
    unresolved_inner_pins: RwLock<HashSet<usize>>,
    unresolved_outer_pins: RwLock<HashSet<usize>>,
    pins: Box<[CircuitPinInfo]>,
}

// TODO: handle state destruction on remove
impl Circuit {
    fn draw(
        data: &ResolvedCircuitData,
        state: Option<&CircuitStateContext>,
        ctx: &PaintContext,
        semi_transparent: bool,
    ) {
        todo!()
    }

    fn describe_props(
        data: &ResolvedCircuitData,
        _: &CircuitPropertyStore,
    ) -> DynCircuitDescription {
        Self::describe(data)
    }

    fn describe(data: &ResolvedCircuitData) -> DynCircuitDescription {
        let pins = data
            .design
            .as_ref()
            .map(|d| {
                d.pins
                    .iter()
                    .map(|p| CircuitPinDescription {
                        active: true,
                        display_name: p.display_name.clone(),
                        display_dir: p.display_dir,
                        dir: p.dir,
                        name: p.id.clone(),
                        pos: p.pos,
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_else(Vec::new);

        let size = data.design.as_ref().map(|d| d.size).unwrap_or(2.into());

        DynCircuitDescription {
            size,
            pins: pins.into(),
        }
    }

    pub fn resolve_inner_to_outer(&self, inner: usize) -> Option<usize> {
        if let Some(outer) = self.pinmap.read().get_by_left(&inner) {
            return Some(*outer);
        }
        if self.unresolved_inner_pins.read().contains(&inner) {
            return None;
        }

        fn resolve(data: &ResolvedCircuitData, inner: usize) -> Option<usize> {
            let board = data.board.as_ref()?.read();
            let str_id = board.pins.get_by_right(&inner)?;
            let id = data
                .design
                .as_ref()?
                .pins
                .iter()
                .enumerate()
                .find(|(_, p)| p.id.deref() == str_id.deref())?
                .0;
            Some(id)
        }

        let outer = resolve(&self.resolved, inner);
        match outer {
            Some(outer) => {
                self.pinmap.write().insert(inner, outer);
            }
            None => {
                self.unresolved_inner_pins.write().insert(inner);
            }
        }
        outer
    }

    pub fn resolve_outer_to_inner(&self, outer: usize) -> Option<usize> {
        if let Some(inner) = self.pinmap.read().get_by_right(&outer) {
            return Some(*inner);
        }
        if self.unresolved_outer_pins.read().contains(&outer) {
            return None;
        }

        fn resolve(data: &ResolvedCircuitData, outer: usize) -> Option<usize> {
            let str_id = data.design.as_ref()?.pins.get(outer)?.id.deref();
            let board = data.board.as_ref()?.read();
            let id = board.pins.get_by_left(str_id)?;
            Some(*id)
        }

        let inner = resolve(&self.resolved, outer);
        match inner {
            Some(inner) => {
                self.pinmap.write().insert(inner, outer);
            }
            None => {
                self.unresolved_outer_pins.write().insert(outer);
            }
        }
        inner
    }
}

impl CircuitImpl for Circuit {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext) {
        Circuit::draw(&self.resolved, Some(state_ctx), paint_ctx, false);
    }

    fn create_pins(&mut self, props: &CircuitPropertyStore) -> Box<[CircuitPinInfo]> {
        let description = Self::describe_props(&self.resolved, props);
        let pins = description
            .pins
            .iter()
            .map(|d| d.to_info())
            .collect::<Vec<_>>()
            .into_boxed_slice();
        self.pins = pins.clone();
        pins
    }

    fn update_signals(&self, state_ctx: &CircuitStateContext, changed_pin: Option<usize>) {
        let inner_board = &self.resolved.board;
        let inner_board = unwrap_option_or_return!(inner_board).read();

        let inner_state = &self.resolved.state;
        let inner_state = unwrap_option_or_return!(inner_state);

        fn handle_pin(
            this: &Circuit,
            inner_board: &CircuitBoard,
            inner_state: &State,
            id: usize,
            info: &CircuitPinInfo,
            state_ctx: &CircuitStateContext,
        ) {
            let inner_circuit = this
                .resolve_outer_to_inner(id)
                .and_then(|inner| inner_board.circuits.get(inner));

            // Somehow this circuit is not pin...
            if inner_circuit.is_some_and(|inner| inner.ty.deref() != super::pin::TYPEID) {
                return;
            }
            let dir = info.get_direction(state_ctx);
            let inner = inner_circuit.and_then(|i| {
                i.info
                    .read()
                    .pins
                    .first()
                    .map(|p| (p.clone(), CircuitStateContext::new(inner_state, i)))
            });

            match (dir, inner) {
                (PinDirection::Inside, Some((pin, state))) => {
                    pin.set_state(&state, info.get_state(state_ctx))
                }
                (PinDirection::Outside, Some((pin, state))) => {
                    info.set_state(state_ctx, pin.get_state(&state))
                }
                (PinDirection::Outside, None) => info.set_state(state_ctx, WireState::None),
                _ => {}
            }
        }

        match changed_pin {
            Some(id) => {
                if let Some(info) = self.pins.get(id) {
                    handle_pin(self, &inner_board, inner_state, id, info, state_ctx);
                }
            }
            None => {
                for (id, info) in self.pins.iter().enumerate() {
                    handle_pin(self, &inner_board, inner_state, id, info, state_ctx);
                }
            }
        }
    }

    fn size(&self, _: &CircuitPropertyStore) -> Vec2u {
        self.resolved.design.as_ref().map_or(2.into(), |d| d.size)
    }

    fn postload(&mut self, state: &CircuitStateContext, boards: &BoardStorage) {
        if let Some(unresolved) = &self.unresolved {
            unresolved.resolve_into(&mut self.resolved, boards);
        }

        if let Some(inner_state) = &self.resolved.state {
            *inner_state.parent.write() = Some(crate::state::StateParent {
                parent: state.global_state.get_self_arc(),
                circuit: state.circuit.id,
            })
        }
    }

    fn save(&self) -> serde_intermediate::Intermediate {
        let board_id = self
            .resolved
            .board
            .as_ref()
            .map_or_else(|| self.unresolved.map(|u| u.board), |b| Some(b.read().uid));
        let state_id = self
            .resolved
            .state
            .as_ref()
            .map_or_else(|| self.unresolved.map(|u| u.state), |s| Some(s.id));

        // Saving "current" design isn't allowed, it must be a specific id
        let design_id = self.resolved.design.as_ref().map_or_else(
            || self.unresolved.and_then(|u| u.design.get()),
            |d| Some(d.id),
        );

        let board_id = unwrap_option_or_return!(board_id, ().into());
        let state_id = unwrap_option_or_return!(state_id, ().into());
        let design_id = unwrap_option_or_return!(design_id, ().into());

        let model = CircuitDataModel {
            board: board_id,
            state: state_id,
            design: design_id,
        };
        serde_intermediate::to_intermediate(&model).unwrap_or_default()
    }

    fn load(&mut self, data: &serde_intermediate::Intermediate) {
        let model = serde_intermediate::from_intermediate::<CircuitDataModel>(data).ok();
        let model = unwrap_option_or_return!(model);

        self.unresolved = Some(model.into_unresolved());
    }
}

pub struct Preview {
    unresolved: Option<UnresolvedCircuitData>,
    resolved: ResolvedCircuitData,
}

impl CircuitPreviewImpl for Preview {
    fn type_name(&self) -> DynStaticStr {
        "board".into()
    }

    fn draw_preview(&self, _: &CircuitPropertyStore, ctx: &PaintContext, in_world: bool) {
        Circuit::draw(&self.resolved, None, ctx, in_world)
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Circuit {
            resolved: self.resolved.clone(),
            unresolved: self.unresolved,
            ..Circuit::default()
        })
    }

    fn load_impl_data(
        &self,
        data: &serde_intermediate::Intermediate,
        boards: &BoardStorage,
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        let model = serde_intermediate::from_intermediate::<CircuitDataModel>(data).ok();
        let model = unwrap_option_or_return!(model, None);

        let unresolved = model.into_unresolved();
        let mut resolved = ResolvedCircuitData::default();
        unresolved.resolve_into(&mut resolved, boards);

        Some(Box::new(Preview {
            unresolved: Some(unresolved),
            resolved,
        }))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        todo!()
    }

    fn display_name(&self) -> DynStaticStr {
        todo!()
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        Circuit::describe_props(&self.resolved, props)
    }
}
