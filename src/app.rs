use std::{
    collections::HashMap,
    ops::{Deref, Not},
    sync::Arc,
};

use eframe::{
    egui::{self, CollapsingHeader, Frame, Margin, RichText, ScrollArea, Ui},
    epaint::Rounding,
    CreationContext,
};
use emath::{vec2, Align, Align2};

use crate::{
    board::{ActiveCircuitBoard, CircuitBoard, StoredCircuitBoard},
    circuits::{self, CircuitPreview, CircuitPreviewImpl},
    error::{ErrorList, ResultReport},
    evenly_spaced_out,
    ui::{editor::CircuitBoardEditor, side_panel::PanelSide},
    DynStaticStr, RwLock,
};

pub struct SimulationContext {
    pub previews: HashMap<DynStaticStr, Arc<CircuitPreview>>,
    pub boards: RwLock<HashMap<u128, StoredCircuitBoard>>,
}

impl SimulationContext {
    pub fn reset(&self) {
        for board in self.boards.write().drain() {
            board.1.board.destroy();
        }
    }
}

pub struct App {
    editor: crate::ui::editor::CircuitBoardEditor,
    designer: Option<crate::ui::designer::Designer>,

    state_loading_errors: ErrorList,
    state_saving_errors: ErrorList,
    state_name: Option<String>,
    state_to_load: Option<(Option<String>, crate::io::SaveStateData)>,
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

        #[cfg(feature = "wasm")]
        {
            if let Some((name, string)) = crate::web::take_state_input() {
                let state = ron::from_str::<crate::io::SaveStateData>(&string).report_error(
                    &mut self
                        .state_loading_errors
                        .enter_context(|| "deserializing savestate"),
                );

                if let Some(state) = state {
                    self.state_to_load = Some((Some(name), state));
                }
            }
        }

        let dropped_file = ctx.input(|input| {
            input.raw.dropped_files.get(0).and_then(|f| {
                let text = f
                    .path
                    .as_ref()
                    .and_then(|p| {
                        std::fs::read_to_string(p).report_error(&mut self.state_loading_errors)
                    })
                    .or_else(|| {
                        f.bytes.as_ref().and_then(|bytes| {
                            std::str::from_utf8(bytes)
                                .report_error(&mut self.state_loading_errors)
                                .map(String::from)
                        })
                    });

                text.map(|t| {
                    let name = f.name.is_empty().not().then(|| f.name.clone()).or_else(|| {
                        f.path
                            .as_ref()
                            .and_then(|p| p.file_name())
                            .map(|n| n.to_string_lossy().into_owned())
                    });
                    (name, t)
                })
            })
        });
        if let Some((name, string)) = dropped_file {
            let state = ron::from_str::<crate::io::SaveStateData>(&string).report_error(
                &mut self
                    .state_loading_errors
                    .enter_context(|| "deserializing savestate"),
            );

            if let Some(state) = state {
                self.state_to_load = Some((name, state));
            }
        }

        egui::CentralPanel::default()
            .frame(egui::Frame::central_panel(ctx.style().as_ref()).inner_margin(Margin::same(0.0)))
            .show(ctx, |ui| {
                match &mut self.designer {
                    Some(designer) => {
                        designer.background_update(ui);
                    }
                    None => {
                        self.editor.background_update(ui);
                    }
                };

                let close_designer = match &mut self.designer {
                    Some(designer) => {
                        let result = designer.ui_update(ui);
                        result.close
                    }
                    None => {
                        let result = self.editor.ui_update(ui);

                        if let Some(designer) = result.designer_request {
                            self.designer = Some(designer);
                        }
                        false
                    }
                };

                self.bottom_panel(ui);

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
                        .max_height(ctx.screen_rect().height() * 0.75)
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
        } else if !self.state_saving_errors.is_empty() {
            let mut open = true;
            egui::Window::new("Loading errors")
                .anchor(Align2::CENTER_CENTER, vec2(0.0, 0.0))
                .fixed_pos(ctx.screen_rect().center())
                .resizable(false)
                .open(&mut open)
                .collapsible(false)
                .show(ctx, |ui| {
                    ui.vertical_centered(|ui| {
                        ui.label("Some errors were encountered while saving save state")
                    });
                    ui.add_space(10.0);
                    ScrollArea::new([false, true])
                        .min_scrolled_height(200.0)
                        .max_height(ctx.screen_rect().height() * 0.75)
                        .show(ui, |ui| {
                            self.state_saving_errors.show_ui(ui);
                        });
                });
            if !open {
                self.state_saving_errors.clear();
            }
        } else if let Some(mut state) = self.state_to_load.take() {
            let mut open = true;
            let mut discard_state = false;
            let title = format!("Loading {}", state.0.as_deref().unwrap_or("state"));
            egui::Window::new(title)
                .anchor(Align2::CENTER_CENTER, vec2(0.0, 0.0))
                .fixed_pos(ctx.screen_rect().center())
                .resizable(false)
                .open(&mut open)
                .collapsible(false)
                .show(ctx, |ui| {
                    ui.vertical_centered(|ui| {
                        ui.label("Save existing before loading?");
                        ui.add_space(20.0);

                        #[derive(Debug, Clone, Copy)]
                        enum Action {
                            Yes,
                            No,
                            Cancel,
                        }
                        let mut action = None;

                        evenly_spaced_out! {
                            ui, horizontal,
                            |ui| {
                                if ui.button(" Yes ").clicked() { action = Some(Action::Yes); };
                                if ui.button(" No ").clicked() { action = Some(Action::No); };
                                if ui.button(" Cancel ").clicked() { action = Some(Action::Cancel); }
                            },
                        }

                        if let Some(action) = action {
                            match action {
                                Action::Cancel => {
                                    discard_state = true;
                                }
                                _ => {
                                    let c = if matches!(action, Action::Yes) {
                                        self.save_state()
                                    } else {
                                        true
                                    };
                                    if c {
                                        self.sim.reset();
                                        Self::load_boards(
                                            &state.1.boards,
                                            &self.sim,
                                            &mut self.state_loading_errors,
                                        );
                                        self.state_name = state.0.take();

                                        let boards = self.sim.boards.read();
                                        for board in boards.values() {
                                            board.board.activate();
                                        }

                                        let board = boards.values().next().expect("Boards must exist!");
                                        self.editor.board =
                                            ActiveCircuitBoard::new_main(board.board.clone());

                                        discard_state = true;
                                    }
                                }
                            }
                        }
                    });
                    ui.add_space(10.0);
                });
            if !open || discard_state {
                self.state_to_load = None;
            } else {
                self.state_to_load = Some(state)
            }
        }
    }

    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        if !self.state_loading_errors.is_empty() {
            return;
        }

        let boards = self.save_boards();
        if let Some(string) = ron::to_string(&boards).report_error(
            &mut self
                .state_saving_errors
                .enter_context(|| "serializing savestate"),
        ) {
            storage.set_string("boards", string);
        }

        let previews = crate::io::CircuitPreviewCollectionData(HashMap::from_iter(
            self.sim
                .previews
                .iter()
                .filter_map(|(ty, p)| p.save().map(|d| (ty.clone(), d))),
        ));
        storage.set_string("previews", ron::to_string(&previews).unwrap());
    }
}

impl App {
    pub fn create(cc: &CreationContext) -> Self {
        let previews = [
            Box::new(circuits::button::ButtonPreview {}) as Box<dyn CircuitPreviewImpl>,
            Box::new(circuits::led::LedPreview {}) as Box<dyn CircuitPreviewImpl>,
            Box::new(circuits::gates::gate::GatePreview::<circuits::gates::or::Or>::new()),
            Box::new(circuits::gates::gate::GatePreview::<
                circuits::gates::xor::Xor,
            >::new()),
            Box::new(circuits::gates::gate::GatePreview::<
                circuits::gates::nor::Nor,
            >::new()),
            Box::new(circuits::gates::gate::GatePreview::<
                circuits::gates::xnor::Xnor,
            >::new()),
            Box::new(circuits::gates::gate::GatePreview::<
                circuits::gates::and::And,
            >::new()),
            Box::new(circuits::gates::gate::GatePreview::<
                circuits::gates::nand::Nand,
            >::new()),
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
                    Ok(data) => Self::load_boards(&data, &ctx, &mut errors),
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
            state_saving_errors: Default::default(),
            sim: ctx,
            state_name: None,
            state_to_load: None,
        }
    }

    fn bottom_panel(&mut self, ui: &mut Ui) {
        type TabFn = fn(&mut App, &mut Ui);
        let tabs: &[(TabFn, &str)] = &[
            (Self::general_tab, "General"),
            #[cfg(debug_assertions)]
            (Self::debug_tab, "Debug"),
        ];

        crate::ui::side_panel::SidePanel::new(PanelSide::Bottom, "bottom_panel")
            .frame(
                Frame::side_top_panel(ui.style())
                    .rounding(Rounding {
                        ne: 5.0,
                        nw: 5.0,
                        se: 0.0,
                        sw: 0.0,
                    })
                    .outer_margin(Margin::symmetric(8.0, 0.0))
                    .inner_margin(Margin::symmetric(5.0, 5.0))
                    .stroke(ui.style().visuals.window_stroke),
            )
            .show_separator_line(false)
            .default_tab(Some(0))
            .show(
                ui,
                tabs.len(),
                |i| tabs[i].1.into(),
                |tab, ui| (tabs[tab].0)(self, ui),
            );
    }

    #[cfg(debug_assertions)]
    fn debug_tab<'a>(&'a mut self, ui: &'a mut Ui) {
        if ui.button("Reset simulation").clicked() {
            self.sim.reset();
        }
        if ui.button("Purge unused states").clicked() {
            let board = &self.editor.board.board;
            let mut states = board.states.states.write();
            let indexes: Vec<_> = states
                .inner
                .iter()
                .enumerate()
                .filter_map(|(i, s)| s.as_ref().is_some_and(|s| !s.is_being_used()).then_some(i))
                .collect();
            for index in indexes {
                states.remove(index);
            }
        }
    }

    fn general_tab(&mut self, ui: &mut Ui) {
        ui.set_min_height(100.0);

        ScrollArea::vertical()
            .min_scrolled_height(ui.ctx().screen_rect().height() * 0.8)
            .max_width(ui.max_rect().width() - 220.0)
            .show(ui, |ui| {
                ui.vertical(|ui| {
                    ui.label("Welcome to cuprous!");
                    CollapsingHeader::new("Controls").show(ui, |ui| {
                        ui.label("Left-click and drag to pan around");
                        ui.label("Scroll to zoom");
                        ui.label("0-9 to select items in inventory");
                        ui.label("Esc to deselect");
                        ui.label("While selecting, holding Shift will add to existing selection and Ctrl will remove from it");
                        ui.add_space(5.0);
                        ui.label("In editor:");
                        ui.add_space(2.0);
                        ui.label("  R to rotate selected component (if applicable)");
                        ui.label("  F to flip selected component (if applicable)");
                        let paused = if self.editor.board.state.is_frozen() {
                            "  P to resume current circuit simulation"
                        } else {
                            "  P to pause current circuit simulation"
                        };
                        ui.label(paused);
                        ui.add_space(5.0);
                    });
                    let editor_text = if self.designer.is_some() { "Editor mode" } else { "Editor mode (current mode)" };
                    CollapsingHeader::new(editor_text).id_source("tut_editor").show(ui, |ui| {
                        ui.label("Components panel on the left side contains built-in components and custom circuits");
                        ui.label("Click on custom circuit once to select it for placement and wice to open it");
                        ui.label("Designer in custom circuit context menu allows to change its appearance when placed");
                        ui.add_space(5.0);
                        ui.label("Property editor panel on the right side contains editable properties for selected components");
                        ui.add_space(5.0);
                    });
                    let designer_text = if self.designer.is_some() { "Designer mode (current mode)" } else { "Designer mode" };
                    CollapsingHeader::new(designer_text).id_source("tut_designer").show(ui, |ui| {
                        ui.label("Components panel on the left side contains circuit pins, exposable controls from placed components and (later) circuit decorations");
                        ui.label("Circuit decorations can be selected in the inventory (more will be added later)");
                        ui.add_space(5.0);
                        ui.label("Property editor panel on the right side contains editable properties for selected pins or decorations");
                        ui.add_space(5.0);
                        ui.label("Resulting circuit size can be adjusted by dragging sides of the purple rectangle");
                        ui.label("Note that all pins and decorations should be placed inside that size");
                        ui.add_space(5.0);
                        ui.label("Rectangle decorations can be moved by dragging them and resized by dragging their edges");
                        ui.label("Pins can be moved by dragging them");
                        ui.add_space(5.0);
                        ui.label("Some components placed on circuit board can be exposed as interactive controls on circuit's design.");
                        ui.label("Controls can be moved and resized, but will keep original aspect ratio.");
                        ui.add_space(5.0);
                    });
                    CollapsingHeader::new("Editor debug controls").show(ui, |ui| {
                        let debug = if self.editor.debug {
                            "F9 to disable visual debug mode"
                        } else {
                            "F9 to enable visual debug mode"
                        };
                        ui.label(debug);
                        ui.label("F8 to reload current board editor");
                        ui.label("F2 to reset current board state");
                        ui.label("Ctrl+F2 to reset all states of the current board");
                        ui.label("Shift+F2 to reset all states of all boards");
                        let queue = if self.editor.board.board.is_ordered_queue() {
                            "Q to switch current circuit to random queue mode"
                        } else {
                            "Q to switch current circuit to ordered queue mode"
                        };
                        ui.label(queue);
                        ui.add_space(5.0);
                    });
                    ui.label("Project is in early development stage, a lot of things will be added and will change");
                });
            });

        crate::ui::align_ui(ui, "state", vec2(1.0, 0.0), true, false, |ui| {
            ui.vertical(|ui| {
                // it makes text jumpy when resizing...
                crate::ui::align_ui(ui, "state_text_a", vec2(1.0, 0.0), false, true, |ui| {
                    ui.with_layout(ui.layout().with_cross_align(Align::Max), |ui| {
                        ui.label("Use buttons below to save or load the state of simulation");
                    });
                });
                crate::ui::align_ui(ui, "state_text_b", vec2(1.0, 0.0), false, true, |ui| {
                    ui.with_layout(ui.layout().with_cross_align(Align::Max), |ui| {
                        ui.label("Or drag and drop save state files here to load them");
                    });
                });
                crate::ui::align_ui(ui, "state_buttons", vec2(1.0, 0.0), false, true, |ui| {
                    ui.horizontal_wrapped(|ui| {
                        if ui
                            .button(RichText::new(" Save state ").size(13.0))
                            .clicked()
                        {
                            self.save_state();
                        }
                        if ui
                            .button(RichText::new(" Load state ").size(13.0))
                            .clicked()
                        {
                            self.load_state();
                        }
                    });
                });
            });
        });

        crate::ui::align_ui(ui, "links", vec2(1.0, 1.0), true, false, |ui| {
            ui.spacing_mut().item_spacing = vec2(0.0, 0.0);
            ui.vertical(|ui| {
                crate::ui::align_ui(ui, "egui", vec2(1.0, 0.0), false, true, |ui| {
                    ui.horizontal_wrapped(|ui| {
                        ui.label("Powered by ");
                        ui.hyperlink_to("egui", "https://github.com/emilk/egui");
                    });
                });
                crate::ui::align_ui(ui, "github", vec2(1.0, 0.0), false, true, |ui| {
                    ui.horizontal_wrapped(|ui| {
                        ui.label("Cuprous source is available on ");
                        ui.hyperlink_to("GitHub", "https://github.com/Ved-s/cuprous");
                    });
                });
                crate::ui::align_ui(ui, "support", vec2(1.0, 0.0), false, true, |ui| {
                    ui.horizontal_wrapped(|ui| {
                        ui.label("Support me on ");
                        ui.hyperlink_to("Boosty", "https://boosty.to/ved_s/donate");
                        ui.label(" or ");
                        ui.hyperlink_to("DonatePay", "https://new.donatepay.ru/@945314");
                    });
                });
            });
        });
    }

    fn save_state(&mut self) -> bool {
        #[cfg(not(feature = "wasm"))]
        {
            use std::io::Write;
            let fd = rfd::FileDialog::new()
                .set_title("Save state")
                .set_file_name(
                    self.state_name
                        .clone()
                        .unwrap_or_else(|| "state.ron".into()),
                )
                .add_filter("RON file", &["ron"]);
            let path = fd.save_file();

            if let Some(path) = path {
                let boards = self.save_boards();

                let save_state = crate::io::SaveStateData { boards };
                let string = ron::to_string(&save_state).report_error(
                    &mut self
                        .state_saving_errors
                        .enter_context(|| "serializing savestate"),
                );
                if let Some(string) = string {
                    return std::fs::File::create(path)
                        .and_then(|mut f| f.write_all(string.as_bytes()))
                        .report_error(
                            &mut self
                                .state_saving_errors
                                .enter_context(|| "saving savestate"),
                        )
                        .is_some();
                }
            }
            false
        }
        #[cfg(feature = "wasm")]
        {
            let boards = self.save_boards();

            let save_state = crate::io::SaveStateData { boards };
            let string = ron::to_string(&save_state).report_error(
                &mut self
                    .state_saving_errors
                    .enter_context(|| "serializing savestate"),
            );
            if let Some(string) = string {
                let name = self
                    .state_name
                    .clone()
                    .unwrap_or_else(|| "state.ron".into());
                crate::web::save_state(name, string);
            }
            true
        }
    }

    fn load_state(&mut self) {
        #[cfg(not(feature = "wasm"))]
        {
            let fd = rfd::FileDialog::new()
                .set_title("Load state")
                .add_filter("RON file", &["ron"]);
            let path = fd.pick_file();
            if let Some(path) = path {
                let string = std::fs::read_to_string(&path).report_error(
                    &mut self
                        .state_loading_errors
                        .enter_context(|| "loading savestate"),
                );
                if let Some(string) = string {
                    let state = ron::from_str::<crate::io::SaveStateData>(&string).report_error(
                        &mut self
                            .state_loading_errors
                            .enter_context(|| "deserializing savestate"),
                    );

                    if let Some(state) = state {
                        self.state_to_load = Some((
                            path.file_name().map(|n| n.to_string_lossy().into_owned()),
                            state,
                        ));
                    }
                }
            }
        }
        #[cfg(feature = "wasm")]
        {
            crate::web::request_load();
        }
    }

    fn save_boards(&mut self) -> Vec<crate::io::CircuitBoardData> {
        let boards = self.sim.boards.read();
        let locks: Vec<_> = boards.values().map(|b| b.board.sim_lock.write()).collect();
        let res = boards.values().map(|b| b.board.save(false)).collect();
        drop(locks);
        res
    }

    fn load_boards(
        data: &Vec<crate::io::CircuitBoardData>,
        ctx: &Arc<SimulationContext>,
        errors: &mut ErrorList,
    ) {
        for data in data {
            let board = CircuitBoard::load(data, ctx, errors);
            let uid = board.uid;
            ctx.boards
                .write()
                .insert(uid, StoredCircuitBoard::new(board));
        }
    }
}
