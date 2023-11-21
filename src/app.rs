use std::{collections::HashMap, ops::Deref, sync::Arc, num::NonZeroUsize};

use eframe::{
    egui::{self, Grid, Margin, TopBottomPanel},
    epaint::{Color32, Rounding},
    CreationContext,
};

use crate::{
    board::{ActiveCircuitBoard, CircuitBoard, StoredCircuitBoard},
    circuits::{self, CircuitPreview, CircuitPreviewImpl},
    time::Instant,
    ui::{
        editor::CircuitBoardEditor,
        side_panel::{PanelSide, SidePanel},
    },
    DynStaticStr, RwLock,
};

pub struct SimulationContext {
    pub previews: HashMap<DynStaticStr, Arc<CircuitPreview>>,
    pub boards: RwLock<HashMap<u128, StoredCircuitBoard>>,
}

pub struct App {
    editor: crate::ui::editor::CircuitBoardEditor,
    designer: Option<crate::ui::designer::Designer>,

    pub sim: Arc<SimulationContext>,
}

// TODO: fix coi sometimes not working by re-registering it and reloading
impl eframe::App for App {
    fn update(&mut self, ctx: &eframe::egui::Context, _: &mut eframe::Frame) {
        ctx.request_repaint();

        // TODO: timings
        #[cfg(feature = "single_thread")]
        //let sim_time =
        {
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
            .frame(egui::Frame::central_panel(ctx.style().as_ref()).inner_margin(Margin::same(0.0)).fill(Color32::from_gray(16)))
            .show(ctx, |ui| {
                // let close_designer = match &mut self.designer {
                //     Some(designer) => {
                //         let result = designer.update(ui);
                //         result.close
                //     }
                //     None => {
                //         let result = self.editor.update(ui);

                //         if let Some(designer) = result.designer_request {
                //             self.designer = Some(designer);
                //         }
                //         false
                //     }
                // };

                // if close_designer {
                //     self.designer = None;
                // }

                SidePanel::new(PanelSide::Left, "left")
                    .default_tab(None)
                    .frame(
                        egui::Frame::side_top_panel(&ctx.style())
                            .fill(Color32::from_gray(30))
                            .stroke(ctx.style().visuals.window_stroke)
                            .outer_margin(Margin::symmetric(0.0, 10.0))
                            .rounding(Rounding {
                                nw: 0.0,
                                ne: 6.0,
                                sw: 0.0,
                                se: 6.0,
                            }),
                    )
                    .show_separator_line(false)
                    .show(
                        ui,
                        NonZeroUsize::new(16).unwrap(),
                        |i| format!("Tab {i}").into(),
                        |i, ui| {
                            ui.horizontal(|ui| {
                                for i in 0..=i {
                                    ui.label(format!("Label {i}"));
                                    ui.end_row();
                                }
                            });
                        },
                    );

                SidePanel::new(PanelSide::Top, "top")
                    .default_tab(None)
                    .frame(
                        egui::Frame::side_top_panel(&ctx.style())
                            .fill(Color32::from_gray(30))
                            .stroke(ctx.style().visuals.window_stroke)
                            .outer_margin(Margin::symmetric(10.0, 0.0))
                            .rounding(Rounding {
                                nw: 0.0,
                                ne: 0.0,
                                sw: 6.0,
                                se: 6.0,
                            }),
                    )
                    .show_separator_line(false)
                    .show(
                        ui,
                        NonZeroUsize::new(16).unwrap(),
                        |i| format!("Tab {i}").into(),
                        |i, ui| {
                            ui.vertical(|ui| {
                                for i in 0..= (i % 3) {
                                    ui.label(format!("Label {i}"));
                                    ui.end_row();
                                }
                            });
                        },
                    );

                // SidePanel::new(PanelSide::Right, "right")
                //     .default_tab(None)
                //     .frame(
                //         egui::Frame::side_top_panel(&ctx.style())
                //             .fill(Color32::from_gray(30))
                //             .stroke(ctx.style().visuals.window_stroke)
                //             .outer_margin(Margin::symmetric(0.0, 10.0))
                //             .rounding(Rounding {
                //                 nw: 6.0,
                //                 ne: 0.0,
                //                 sw: 6.0,
                //                 se: 0.0,
                //             }),
                //     )
                //     .show_separator_line(false)
                //     .show(
                //         ui,
                //         &std::array::from_fn::<_, 16, _>(|i| i),
                //         |tab| format!("Tab {tab}").into(),
                //         |tab, ui| {
                //             ui.horizontal(|ui| {
                //                 for i in 0..=*tab {
                //                     ui.label(format!("Label {i}"));
                //                     ui.end_row();
                //                 }
                //             });
                //         },
                //     );

                // SidePanel::new(PanelSide::Bottom, "bottom")
                //     .default_tab(None)
                //     .frame(
                //         egui::Frame::side_top_panel(&ctx.style())
                //             .fill(Color32::from_gray(30))
                //             .stroke(ctx.style().visuals.window_stroke)
                //             .outer_margin(Margin::symmetric(10.0, 0.0))
                //             .rounding(Rounding {
                //                 nw: 6.0,
                //                 ne: 6.0,
                //                 sw: 0.0,
                //                 se: 0.0,
                //             }),
                //     )
                //     .show_separator_line(false)
                //     .show(
                //         ui,
                //         &std::array::from_fn::<_, 16, _>(|i| i),
                //         |tab| format!("Tab {tab}").into(),
                //         |tab, ui| {
                //             ui.vertical(|ui| {
                //                 for i in 0..=*tab {
                //                     ui.label(format!("Label {i}"));
                //                     ui.end_row();
                //                 }
                //             });
                //         },
                //     );
            });
    }

    fn save(&mut self, _storage: &mut dyn eframe::Storage) {
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
            Box::new(circuits::gates::gate::GatePreview {
                template: circuits::gates::or::TEMPLATE,
            }),
            Box::new(circuits::gates::gate::GatePreview {
                template: circuits::gates::nor::TEMPLATE,
            }),
            Box::new(circuits::gates::gate::GatePreview {
                template: circuits::gates::xor::TEMPLATE,
            }),
            Box::new(circuits::gates::gate::GatePreview {
                template: circuits::gates::xnor::TEMPLATE,
            }),
            Box::new(circuits::gates::gate::GatePreview {
                template: circuits::gates::and::TEMPLATE,
            }),
            Box::new(circuits::gates::gate::GatePreview {
                template: circuits::gates::nand::TEMPLATE,
            }),
            Box::new(circuits::gates::not::NotPreview {}),
            Box::new(circuits::pullup::PullupPreview {}),
            Box::new(circuits::transistor::TransistorPreview {}),
            Box::new(circuits::freq_meter::FreqMeterPreview {}),
            Box::new(circuits::pin::Preview {}),
            Box::<circuits::board::BoardPreview>::default(),
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
                        let uid = board.uid;
                        ctx.boards
                            .write()
                            .insert(uid, StoredCircuitBoard::new(board));
                    }
                }
            } else if let Some(main_board) = storage.get_string("board") {
                // TODO: do something with loading errors

                if let Ok(data) = ron::from_str::<crate::io::CircuitBoardData>(&main_board) {
                    let board = CircuitBoard::load(&data, &ctx);
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

        Self::new(ctx)
    }

    pub fn new(ctx: Arc<SimulationContext>) -> Self {
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
            sim: ctx,
        }
    }
}
