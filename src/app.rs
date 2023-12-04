use std::{collections::HashMap, ops::Deref, sync::Arc};

use eframe::{
    egui::{self, Margin, ScrollArea},
    CreationContext,
};
use emath::{vec2, Align2};

use crate::{
    board::{ActiveCircuitBoard, CircuitBoard, StoredCircuitBoard},
    circuits::{self, CircuitPreview, CircuitPreviewImpl},
    error::{ErrorList, ResultReport},
    ui::editor::CircuitBoardEditor,
    DynStaticStr, RwLock,
};

pub struct SimulationContext {
    pub previews: HashMap<DynStaticStr, Arc<CircuitPreview>>,
    pub boards: RwLock<HashMap<u128, StoredCircuitBoard>>,
}

pub struct App {
    editor: crate::ui::editor::CircuitBoardEditor,
    designer: Option<crate::ui::designer::Designer>,

    state_loading_errors: ErrorList,
    pub sim: Arc<SimulationContext>,
}

// TODO: fix coi sometimes not working by re-registering it and reloading
impl eframe::App for App {
    fn update(&mut self, ctx: &eframe::egui::Context, _frame: &mut eframe::Frame) {
        ctx.request_repaint();

        // TODO: timings
        #[cfg(feature = "single_thread")]
        //let sim_time =
        {
            use crate::time::Instant;
            let start_time = Instant::now();

            // Run simulation ethier 500 times or for 10ms
            for _ in 0..500 {
                for board in self.sim.boards.read().values() {
                    board.board.states.update();
                }

                if Instant::now()
                    .checked_duration_since(start_time)
                    .is_some_and(|d| d.as_millis() > 10)
                {
                    break;
                }
            }
            //Instant::now() - start_time
        };

        egui::CentralPanel::default()
            .frame(egui::Frame::central_panel(ctx.style().as_ref()).inner_margin(Margin::same(0.0)))
            .show(ctx, |ui| {
                let close_designer = match &mut self.designer {
                    Some(designer) => {
                        let result = designer.update(ui);
                        result.close
                    }
                    None => {
                        let result = self.editor.update(ui);

                        if let Some(designer) = result.designer_request {
                            self.designer = Some(designer);
                        }
                        false
                    }
                };

                if close_designer {
                    self.designer = None;
                }
            });

        if !self.state_loading_errors.is_empty() {
            let mut open = true;
            egui::Window::new("Loading errors")
                .anchor(Align2::CENTER_CENTER, vec2(0.0, 0.0))
                .fixed_pos(ctx.screen_rect().center())
                .resizable(false)
                .open(&mut open)
                .collapsible(false)
                .show(ctx, |ui| {
                    ui.vertical_centered(|ui| {
                        ui.label("Some errors were encountered while loading save state")
                    });
                    ui.add_space(10.0);
                    ScrollArea::new([false, true])
                        .min_scrolled_height(200.0)
                        .show(ui, |ui| {
                            self.state_loading_errors.show_ui(ui);
                        });
                    ui.add_space(10.0);
                    ui.vertical_centered(|ui| {
                        ui.label("State won't be saved while this window is open");
                    });
                });
            if !open {
                self.state_loading_errors.clear();
            }
        }
    }

    fn save(&mut self, _storage: &mut dyn eframe::Storage) {
        if !self.state_loading_errors.is_empty() {
            return;
        }

        let boards = self.sim.boards.read();
        let locks: Vec<_> = boards.values().map(|b| b.board.sim_lock.write()).collect();
        let data: Vec<_> = boards.values().map(|b| b.board.save(false)).collect();

        drop(locks);

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
            Box::new(circuits::button::ButtonPreview {}) as Box<dyn CircuitPreviewImpl>,
            Box::<circuits::gates::gate::GatePreview<circuits::gates::or::Or>>::default(),
            Box::<circuits::gates::gate::GatePreview<circuits::gates::xor::Xor>>::default(),
            Box::<circuits::gates::gate::GatePreview<circuits::gates::nor::Nor>>::default(),
            Box::<circuits::gates::gate::GatePreview<circuits::gates::xnor::Xnor>>::default(),
            Box::<circuits::gates::gate::GatePreview<circuits::gates::and::And>>::default(),
            Box::<circuits::gates::gate::GatePreview<circuits::gates::nand::Nand>>::default(),
            Box::new(circuits::gates::gate::Gate2497Preview),
            Box::new(circuits::gates::not::NotPreview {}),
            Box::new(circuits::pullup::PullupPreview {}),
            Box::new(circuits::transistor::TransistorPreview {}),
            Box::new(circuits::freq_meter::FreqMeterPreview {}),
            Box::new(circuits::pin::Preview {}),
            Box::<circuits::board::BoardPreview>::default(),
        ];
        let mut errors = ErrorList::new();
        let preview_data = cc
            .storage
            .and_then(|s| s.get_string("previews"))
            .and_then(|s| {
                ron::from_str::<crate::io::CircuitPreviewCollectionData>(&s)
                    .report_error(&mut errors.enter_context(|| "loading preview data"))
            });
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
                match ron::from_str::<Vec<crate::io::CircuitBoardData>>(&boards) {
                    Ok(data) => {
                        for data in &data {
                            let board = CircuitBoard::load(data, &ctx, &mut errors);
                            let uid = board.uid;
                            ctx.boards
                                .write()
                                .insert(uid, StoredCircuitBoard::new(board));
                        }
                    }
                    Err(e) => errors.enter_context(|| "loading board data").push_error(e),
                }
            } else if let Some(main_board) = storage.get_string("board") {
                let data = ron::from_str::<crate::io::CircuitBoardData>(&main_board)
                    .report_error(&mut errors.enter_context(|| "loading board data"));

                if let Some(data) = data {
                    let board = CircuitBoard::load(&data, &ctx, &mut errors);
                    let mut name = board.name.write();
                    if name.get_str().is_empty() {
                        let string = name.get_mut();
                        string.clear();
                        string.push_str("main");
                    }
                    drop(name);

                    ctx.boards
                        .write()
                        .insert(board.uid, StoredCircuitBoard::new(board));
                }
            }
        }

        Self::new(ctx, errors)
    }

    pub fn new(ctx: Arc<SimulationContext>, errors: ErrorList) -> Self {
        if ctx.boards.read().is_empty() {
            let board = CircuitBoard::new(ctx.clone(), "main");
            ctx.boards
                .write()
                .insert(board.uid, StoredCircuitBoard::new(Arc::new(board)));
        }

        for board in ctx.boards.read().values() {
            board.board.activate();
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
            designer: None,
            state_loading_errors: errors,
            sim: ctx,
        }
    }
}
