use std::{collections::HashMap, ops::Deref, sync::Arc};

use eframe::{
    egui::{self, Margin},
    CreationContext,
};

use crate::{
    board::{ActiveCircuitBoard, CircuitBoard, StoredCircuitBoard},
    circuits::{self, CircuitPreview, CircuitPreviewImpl, CircuitStateContext},
    ui::editor::CircuitBoardEditor,
    DynStaticStr, RwLock
};

pub struct SimulationContext {
    pub previews: HashMap<DynStaticStr, Arc<CircuitPreview>>,
    pub boards: RwLock<HashMap<u128, StoredCircuitBoard>>,
}

pub struct App {
    editor: crate::ui::editor::CircuitBoardEditor,

    pub sim: Arc<SimulationContext>,
}

// TODO: fix coi sometimes not working by re-registering it and reloading
impl eframe::App for App {
    fn update(&mut self, ctx: &eframe::egui::Context, _: &mut eframe::Frame) {
        ctx.request_repaint();

        #[cfg(feature = "single_thread")]
        let sim_time = {
            let start_time = Instant::now();
            for board in self.sim.boards.read().values() {
                let states = board.board.read().states.clone();
                states.update();
            }
            Instant::now() - start_time
        };

        egui::CentralPanel::default()
            .frame(egui::Frame::central_panel(ctx.style().as_ref()).inner_margin(Margin::same(0.0)))
            .show(ctx, |ui| {
                self.editor.draw_background(ui);
                self.editor.draw_ui(ui);
            });
    }

    fn save(&mut self, _storage: &mut dyn eframe::Storage) {
        let boards = self.sim.boards.read();
        let guards: Vec<_> = boards.values().map(|b| b.board.read()).collect();
        let locks: Vec<_> = guards.iter().map(|g| g.sim_lock.write()).collect();
        let data: Vec<_> = guards.iter().map(|g| g.save(false)).collect();

        drop(locks);
        drop(guards);

        _storage.set_string("boards", ron::to_string(&data).unwrap());

        let previews = crate::io::CircuitPreviewCollectionData(HashMap::from_iter(
            self.sim
                .previews
                .iter()
                .filter_map(|(ty, p)| p.save().map(|d| (ty.clone(), d))),
        ));
        _storage.set_string("previews", ron::to_string(&previews).unwrap());
    }
}

impl App {
    pub fn create(cc: &CreationContext) -> Self {
        let previews = [
            Box::new(circuits::button::Preview {}) as Box<dyn CircuitPreviewImpl>,
            Box::new(circuits::gates::gate::Preview {
                template: circuits::gates::or::TEMPLATE,
            }),
            Box::new(circuits::gates::gate::Preview {
                template: circuits::gates::nor::TEMPLATE,
            }),
            Box::new(circuits::gates::gate::Preview {
                template: circuits::gates::xor::TEMPLATE,
            }),
            Box::new(circuits::gates::gate::Preview {
                template: circuits::gates::xnor::TEMPLATE,
            }),
            Box::new(circuits::gates::gate::Preview {
                template: circuits::gates::and::TEMPLATE,
            }),
            Box::new(circuits::gates::gate::Preview {
                template: circuits::gates::nand::TEMPLATE,
            }),
            Box::new(circuits::gates::not::Preview {}),
            Box::new(circuits::pullup::Preview {}),
            Box::new(circuits::transistor::Preview {}),
            Box::new(circuits::freq_meter::Preview {}),
            Box::new(circuits::pin::Preview {}),
        ];
        let preview_data = cc
            .storage
            .and_then(|s| s.get_string("previews"))
            .and_then(|s| ron::from_str::<crate::io::CircuitPreviewCollectionData>(&s).ok());
        let previews = HashMap::from_iter(previews.into_iter().map(|p| {
            let data = preview_data
                .as_ref()
                .and_then(|d| d.0.get(p.type_name().deref()));
            let p = match data {
                Some(d) => CircuitPreview::load_with_data(p, d),
                None => CircuitPreview::from_impl(p),
            };
            (p.imp.type_name(), Arc::new(p))
        }));

        let ctx = Arc::new(SimulationContext {
            previews,
            boards: Default::default(),
        });

        if let Some(storage) = cc.storage {
            if let Some(boards) = storage.get_string("boards") {
                if let Ok(data) = ron::from_str::<Vec<crate::io::CircuitBoardData>>(&boards) {
                    for data in &data {
                        let board = CircuitBoard::load(data, &ctx);
                        let uid = board.read().uid;
                        ctx.boards
                            .write()
                            .insert(uid, StoredCircuitBoard::new(board));
                    }
                }
            } else if let Some(main_board) = storage.get_string("board") {
                // TODO: do something with loading errors

                if let Ok(data) = ron::from_str::<crate::io::CircuitBoardData>(&main_board) {
                    let board = CircuitBoard::load(&data, &ctx);
                    let mut board_guard = board.write();
                    if board_guard.name.get_str().is_empty() {
                        board_guard.name = "main".into();
                    }
                    let uid = board_guard.uid;
                    drop(board_guard);

                    ctx.boards
                        .write()
                        .insert(uid, StoredCircuitBoard::new(board));
                }
            }
        }

        let boards: Vec<_> = ctx
            .boards
            .read()
            .values()
            .map(|s| s.board.clone())
            .collect();

        let mut states = vec![];
        let mut circuits = vec![];

        for board in boards {
            states.clear();
            circuits.clear();

            let guard = board.read();
            states.extend(guard.states.states().read().iter().cloned());
            circuits.extend(guard.circuits.iter().cloned());
            drop(guard);

            for state in states.drain(..) {
                for circuit in circuits.drain(..) {
                    let csc = CircuitStateContext::new(state.clone(), circuit.clone());
                    circuit.imp.write().postload(&csc, false);
                }
            }
        }

        Self::new(ctx)
    }

    pub fn new(ctx: Arc<SimulationContext>) -> Self {
        if ctx.boards.read().is_empty() {
            let mut board = CircuitBoard::new(ctx.clone());
            board.name = "main".into();
            ctx.boards.write().insert(
                board.uid,
                StoredCircuitBoard::new(Arc::new(RwLock::new(board))),
            );
        }

        #[cfg(not(feature = "single_thread"))]
        {
            for board in ctx.boards.read().values() {
                board.board.read().activate();
            }
        }

        let board = ActiveCircuitBoard::new_main(
            ctx.boards
                .read()
                .values()
                .next()
                .expect("Board list cannot be empty")
                .board
                .clone(),
        );

        let editor = CircuitBoardEditor::new(board, &ctx);

        Self {
            editor,
            sim: ctx,
        }
    }
}
