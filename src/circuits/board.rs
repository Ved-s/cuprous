use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use bimap::BiMap;
use eframe::{
    egui::{TextStyle, WidgetText},
    epaint::{Color32, Rounding, Stroke, TextShape, FontId},
};
use emath::{pos2, Align2};

use crate::{
    board::{CircuitBoard, CircuitDesign},
    circuits::*,
    state::StateParent,
    unwrap_option_or_return,
};

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CircuitDataModel {
    board: u128,
    design: usize,

    size: Vec2u,
    pins: Vec<DynStaticStr>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
struct CircuitStateDataModel {
    state: usize,
}

#[derive(Default)]
pub struct BoardState {
    pasted: bool,
    parent_error: bool,
    state_id: Option<usize>,
    state: Option<Arc<State>>,
}

impl InternalCircuitState for BoardState {
    fn serialize(&self, _: bool) -> serde_intermediate::Intermediate {
        let state_id = self.state.as_ref().map(|s| s.id).or(self.state_id);

        state_id
            .map(|id| {
                serde_intermediate::to_intermediate(&CircuitStateDataModel { state: id })
                    .unwrap_or_default()
            })
            .unwrap_or_default()
    }
}

#[derive(Default)]
pub struct Board {
    board_id: Option<u128>,
    design_id: Option<usize>,

    board: Option<Arc<CircuitBoard>>,
    design: Option<Arc<CircuitDesign>>,

    // Inner pin, outer pin
    // Resolves while executing
    pinmap: RwLock<BiMap<usize, usize>>,
    unresolved_inner_pins: RwLock<HashSet<usize>>,
    unresolved_outer_pins: RwLock<HashSet<usize>>,
    pins: Box<[CircuitPinInfo]>,
    controls: Box<[(usize, usize)]>,

    saved_size: Option<Vec2u>,
    saved_pin_ids: Option<Vec<DynStaticStr>>,
}

impl Board {
    fn draw(
        board: Option<&Arc<CircuitBoard>>,
        design: Option<&Arc<CircuitDesign>>,
        state: Option<&Arc<State>>,
        ignore_state: bool,
        parent_error: bool,
        ctx: &PaintContext,
        semi_transparent: bool,
    ) {
        let trans = if semi_transparent { 0.6 } else { 1.0 };

        let draw_error = |s: &str| {
            ctx.paint.rect(
                ctx.rect,
                Rounding::ZERO,
                Color32::RED.gamma_multiply(0.8 * trans),
                Stroke::new(2.0, Color32::BLACK.gamma_multiply(trans)),
            );

            let rect = ctx.rect.shrink(2.0);
            let galley = WidgetText::from(s)
                .into_galley(ctx.ui, Some(true), rect.width(), TextStyle::Monospace)
                .galley;

            let pos = pos2(
                rect.left() + (rect.width() - galley.size().x) * 0.5,
                rect.top() + (rect.height() - galley.size().y) * 0.5,
            );

            ctx.paint.add(TextShape {
                pos,
                galley,
                underline: Stroke::NONE,
                override_text_color: Some(Color32::WHITE),
                angle: 0.0,
            })
        };

        if parent_error {
            draw_error("Parent error");
            return;
        }

        if board.is_none() {
            draw_error("Invalid board");
            return;
        }

        if state.is_none() && !ignore_state {
            draw_error("Invalid state");
            return;
        }

        let design = match design {
            Some(v) => v,
            None => {
                draw_error("Invalid design");
                return;
            }
        };

        design.draw_decorations(ctx.ui.painter(), ctx.rect, trans);
    }

    fn describe_props(
        design: Option<&CircuitDesign>,
        _: &CircuitPropertyStore,
    ) -> DynCircuitDescription {
        Self::describe(design)
    }

    fn describe(design: Option<&CircuitDesign>) -> DynCircuitDescription {
        let pins = design
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
            .unwrap_or_default();

        let size = design.map(|d| d.size).unwrap_or(2.into());

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

        fn resolve(
            board: Option<&CircuitBoard>,
            design: Option<&CircuitDesign>,
            inner: usize,
        ) -> Option<usize> {
            let pins = board?.pins.read();
            let str_id = pins.get_by_right(&inner)?;
            let id = design?
                .pins
                .iter()
                .enumerate()
                .find(|(_, p)| p.id.deref() == str_id.deref())?
                .0;
            Some(id)
        }

        let outer = resolve(self.board.as_deref(), self.design.as_deref(), inner);
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

        fn resolve(
            board: Option<&CircuitBoard>,
            design: Option<&CircuitDesign>,
            outer: usize,
        ) -> Option<usize> {
            let str_id = design?.pins.get(outer)?.id.deref();
            let pins = board?.pins.read();
            let id = pins.get_by_left(str_id)?;
            Some(*id)
        }

        let inner = resolve(self.board.as_deref(), self.design.as_deref(), outer);
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

impl CircuitImpl for Board {
    fn draw(&self, ctx: &CircuitStateContext, paint_ctx: &PaintContext) {
        let data =
            ctx.read_circuit_internal_state(|s: &BoardState| (s.state.clone(), s.parent_error));

        Board::draw(
            self.board.as_ref(),
            self.design.as_ref(),
            data.as_ref().and_then(|d| d.0.as_ref()),
            false,
            data.as_ref().is_some_and(|d| d.1),
            paint_ctx,
            false,
        );
    }

    fn control_count(&self, _: &Arc<Circuit>) -> Option<usize> {
        Some(self.controls.len())
    }

    fn control_info(&self, _: &Arc<Circuit>, id: usize) -> Option<CircuitControlInfo> {
        let (circuit, id) = self.controls.get(id).copied()?;
        let info = self.design.as_ref()?.controls.get(&(circuit, id))?;
        Some(
            CircuitControlInfo {
                rect: info.rect,
                display_name: info.display_name.get_arc().into(),
            }
        )
    }

    fn update_control(
        &self,
        id: usize,
        _: &Arc<Circuit>,
        state: Option<&CircuitStateContext>,
        ctx: &PaintContext,
        interactive: bool,
        uid: Id,
    ) {

        let draw_error = |s: &str| {
            ctx.paint.rect(
                ctx.rect,
                Rounding::ZERO,
                Color32::RED.gamma_multiply(0.8),
                Stroke::new(2.0, Color32::BLACK),
            );

            let rect = ctx.rect.shrink(2.0);
            let galley = WidgetText::from(s)
                .into_galley(ctx.ui, Some(true), rect.width(), TextStyle::Monospace)
                .galley;

            let pos = pos2(
                rect.left() + (rect.width() - galley.size().x) * 0.5,
                rect.top() + (rect.height() - galley.size().y) * 0.5,
            );

            ctx.paint.add(TextShape {
                pos,
                galley,
                underline: Stroke::NONE,
                override_text_color: Some(Color32::WHITE),
                angle: 0.0,
            })
        };

        let (circuit, id) = match self.controls.get(id).copied() {
            Some(v) => v,
            None => {
                draw_error("Invalid control");
                return;
            },
        };
        
        let board = match &self.board {
            Some(v) => v,
            None => {
                draw_error("Invalid board");
                return;
            },
        };
        let circuits = board.circuits.read();
        let circuit = match circuits.get(circuit) {
            Some(v) => v,
            None => {
                draw_error("Invalid circuit");
                return;
            },
        };

        let inner_state = match state {
            None => None,
            Some(state) => {
                let inner_state = state
                    .read_circuit_internal_state(|s: &BoardState| s.state.clone())
                    .flatten();
                let inner_state = match inner_state {
                    Some(v) => v,
                    None => {
                        draw_error("Invalid state");
                        return;
                    },
                };
                Some(CircuitStateContext::new(inner_state, circuit.clone()))
            },
        };

        circuit.imp.read().update_control(id, circuit, inner_state.as_ref(), ctx, interactive, uid.with((circuit.board.uid, circuit.pos)));
    }

    fn create_pins(&mut self, circ: &Arc<Circuit>) -> Box<[CircuitPinInfo]> {
        if self.design.is_none() {
            if let Some(saved_pins) = &self.saved_pin_ids {
                return saved_pins
                    .iter()
                    .map(|id| {
                        CircuitPinInfo::new(0, InternalPinDirection::Custom, id.clone(), "", None)
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
            }
        }

        let description = Self::describe_props(self.design.as_deref(), &circ.props);
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
        let inner_board = self.board.as_deref();
        let inner_board = unwrap_option_or_return!(inner_board);

        let inner_state = state_ctx
            .read_circuit_internal_state(|s: &BoardState| s.state.clone())
            .flatten();
        let inner_state = unwrap_option_or_return!(inner_state);

        fn handle_pin(
            this: &Board,
            inner_board: &CircuitBoard,
            inner_state: &Arc<State>,
            id: usize,
            info: &CircuitPinInfo,
            state_ctx: &CircuitStateContext,
        ) {
            let circuits = inner_board.circuits.read();
            let inner_circuit = this
                .resolve_outer_to_inner(id)
                .and_then(|inner| circuits.get(inner));

            // Somehow this circuit is not pin...
            if inner_circuit.is_some_and(|inner| inner.ty.deref() != super::pin::TYPEID) {
                return;
            }
            let dir = info.get_direction(state_ctx);
            let inner = inner_circuit.and_then(|i| {
                i.info.read().pins.first().map(|p| {
                    (
                        p.clone(),
                        CircuitStateContext::new(inner_state.clone(), i.clone()),
                    )
                })
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
                    handle_pin(self, inner_board, &inner_state, id, info, state_ctx);
                }
            }
            None => {
                for (id, info) in self.pins.iter().enumerate() {
                    handle_pin(self, inner_board, &inner_state, id, info, state_ctx);
                }
            }
        }
    }

    fn size(&self, _: &Arc<Circuit>) -> Vec2u {
        self.design
            .as_ref()
            .map(|d| d.size)
            .or(self.saved_size)
            .unwrap_or(2.into())
    }

    // Load board and design
    fn circuit_init(&mut self, circ: &Arc<Circuit>, _: bool) {
        if let (true, Some(id)) = (self.board.is_none(), &self.board_id) {
            let ctx = &circ.board.ctx;
            self.board = ctx.boards.read().get(id).map(|sb| sb.board.clone());
        }

        if let (true, Some(id), Some(board)) = (self.design.is_none(), self.design_id, &self.board)
        {
            self.design = board.designs.read().get(id);

            if self.design.is_some() {
                self.saved_size = None;
                self.saved_pin_ids = None;
            }
        }

        // Hackery
        if let Some(design) = &self.design {
            let mut info = circ.info.write();
            let pins: HashMap<_, _> =
                HashMap::from_iter(info.pins.iter().map(|i| (i.name.clone(), i.pin.clone())));

            let desc = Board::describe(Some(design));
            let pins: Vec<_> = desc
                .pins
                .iter()
                .enumerate()
                .map(|(i, p)| {
                    let pin = pins.get(&p.name).cloned();
                    match pin {
                        None => p.to_info(),
                        Some(pin) => {
                            {
                                let mut pin = pin.write();
                                pin.id.id = i;
                                pin.dir = p.dir;
                            }

                            CircuitPinInfo {
                                display_name: p.display_name.clone(),
                                display_dir: p.display_dir,
                                name: p.name.clone(),
                                pos: p.pos,
                                pin,
                            }
                        }
                    }
                })
                .collect();
            info.pins = pins.into_boxed_slice();
            info.size = design.size;

            self.pins = info.pins.clone();

            let mut controls: Vec<_> = design.controls.keys().copied().collect();
            controls.sort();
            self.controls = controls.into_boxed_slice();
        }
    }

    // Load state
    fn state_init(&self, ctx: &CircuitStateContext, first_init: bool) {
        if let Some(board) = self.board.as_ref() {
            let create_new = ctx.write_circuit_internal_state(|s: &mut BoardState| {
                if let Some(id) = s.state_id {
                    let state = board.states.get(id);
                    if let Some(state) = state {
                        if state.get_parent().is_some() {
                            // If circuit was pasted and there's a state conflict, create new state instead
                            if s.pasted {
                                return true;
                            }

                            s.parent_error = true;
                            return false;
                        }

                        state.set_parent(Some(StateParent {
                            state: ctx.global_state.clone(),
                            circuit: ctx.circuit.clone(),
                        }));
                        state.set_frozen(false);
                        s.state = Some(state);
                        return false;
                    }
                }
                true
            });

            if create_new && first_init {
                let state = board.states.create_state(board.clone());
                state.set_parent(Some(StateParent {
                    state: ctx.global_state.clone(),
                    circuit: ctx.circuit.clone(),
                }));
                ctx.set_circuit_internal_state(Some(BoardState {
                    parent_error: false,
                    pasted: false,
                    state: Some(state.clone()),
                    state_id: Some(state.id),
                }))
            }
        }
    }

    fn state_remove(&self, ctx: &CircuitStateContext) {
        ctx.write_circuit_internal_state(|s: &mut BoardState| {
            if let Some(state) = s.state.take() {
                state.set_parent(None);
                state.set_frozen(true);
            }
            s.state_id = None;
        });
    }

    fn load_internal(
        &self,
        _: &CircuitStateContext,
        data: &serde_intermediate::Intermediate,
        paste: bool,
    ) -> Option<Box<dyn InternalCircuitState>> {
        if let Ok(data) = serde_intermediate::from_intermediate::<CircuitStateDataModel>(data) {
            return Some(Box::new(BoardState {
                parent_error: false,
                pasted: paste,
                state_id: Some(data.state),
                state: None,
            }));
        }
        None
    }

    fn save(&self, circ: &Arc<Circuit>, _: bool) -> serde_intermediate::Intermediate {
        let board_id = self.board.as_ref().map(|b| b.uid).or(self.board_id);

        let design_id = self.design.as_ref().map(|d| d.id).or(self.design_id);

        let board_id = unwrap_option_or_return!(board_id, ().into());
        let design_id = unwrap_option_or_return!(design_id, ().into());

        let info = circ.info.read();

        let model = CircuitDataModel {
            board: board_id,
            design: design_id,
            size: info.size,
            pins: info.pins.iter().map(|p| p.name.clone()).collect(),
        };
        serde_intermediate::to_intermediate(&model).unwrap_or_default()
    }

    fn load(&mut self, _: &Arc<Circuit>, data: &serde_intermediate::Intermediate, _: bool) {
        let model = serde_intermediate::from_intermediate::<CircuitDataModel>(data).ok();
        let model = unwrap_option_or_return!(model);

        self.board_id = Some(model.board);
        self.design_id = Some(model.design);

        self.saved_size = Some(model.size);
        self.saved_pin_ids = Some(model.pins);
    }
}

#[derive(Default)]
pub struct BoardPreview {
    board: Option<Arc<CircuitBoard>>,
    design: Option<Arc<CircuitDesign>>,
}

impl BoardPreview {
    pub fn new_from_board(board: Arc<CircuitBoard>) -> Self {
        Self {
            board: Some(board),
            design: None,
        }
    }
}

impl CircuitPreviewImpl for BoardPreview {
    fn type_name(&self) -> DynStaticStr {
        "board".into()
    }

    fn draw_preview(&self, _: &CircuitPropertyStore, ctx: &PaintContext, in_world: bool) {
        let design = self
            .design
            .clone()
            .or_else(|| self.board.as_ref().map(|b| b.designs.read().current()));

        Board::draw(
            self.board.as_ref(),
            design.as_ref(),
            None,
            true,
            false,
            ctx,
            in_world,
        );

        if let (Some(design), Some(board)) = (&design, &self.board) {
            if in_world && board.pins.read().len() > design.pins.len() {
                ctx.paint.text(
                    ctx.rect.left_bottom(),
                    Align2::LEFT_TOP, 
                    "Some board pins aren't placed in its design.\nAdd them in the designer by right-clicking board name and choosing \"Design\"",
                    FontId::monospace(ctx.screen.scale * 0.5),
                    Color32::from_rgb(255, 128, 0)
                );
            }
        }
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        let design = self
            .design
            .clone()
            .or_else(|| self.board.as_ref().map(|b| b.designs.read().current()));

        Box::new(Board {
            board: self.board.clone(),
            design,
            ..Board::default()
        })
    }

    fn load_copy_data(
        &self,
        data: &serde_intermediate::Intermediate,
        _: &serde_intermediate::Intermediate,
        ctx: &Arc<SimulationContext>,
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        let model = serde_intermediate::from_intermediate::<CircuitDataModel>(data).ok();

        let board = model
            .as_ref()
            .and_then(|m| ctx.boards.read().get(&m.board).map(|sb| sb.board.clone()));
        let design = model
            .as_ref()
            .and_then(|m| board.as_ref().and_then(|b| b.designs.read().get(m.design)));

        Some(Box::new(BoardPreview { board, design }))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        CircuitPropertyStore::default()
    }

    fn display_name(&self) -> DynStaticStr {
        self.board
            .as_ref()
            .map(|b| b.name.read().get_arc().into())
            .unwrap_or("Circuit board".into())
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        let design = self
            .design
            .clone()
            .or_else(|| self.board.as_ref().map(|b| b.designs.read().current()));
        Board::describe_props(design.as_deref(), props)
    }
}
