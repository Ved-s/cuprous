use std::{fs::File, path::PathBuf, sync::Arc};

use eframe::{
    egui,
    CreationContext,
};
use egui_dock::{DockArea, DockState, NodeIndex};
use eyre::Context as _;
use parking_lot::RwLock;

use crate::{
    circuits::CircuitBlueprint,
    simulation::SimulationCtx,
    tabs::{SafeTabType, Tab, TabSerde, TabType, TabViewer},
    Style,
};

pub const APP_NAME: &str = "cuprous-dev";

pub struct ErrorStrings {
    main: String,
    sub: String,
}

impl<T: AsRef<dyn std::error::Error>> From<T> for ErrorStrings {
    fn from(value: T) -> Self {
        let main = value.as_ref().to_string();
        let mut last = main.clone();
        let mut sub = String::new();

        let iter =
            std::iter::successors(value.as_ref().source(), |e| e.source()).map(|e| e.to_string());
        for err in iter {
            let str = err.to_string();
            if !last.ends_with(&str) {
                if !sub.is_empty() {
                    sub.push('\n');
                }
                sub.push_str(" - ");
                sub.push_str(&str);
            }
            last = str;
        }

        Self { main, sub }
    }
}

pub struct App {
    pub gl: Arc<glow::Context>,
    pub selected_item: Option<SelectedItem>,
    pub selected_tab: Option<TabType>,
    pub blueprints: Vec<Arc<RwLock<CircuitBlueprint>>>,
    pub style: Arc<Style>,
    pub sim: Arc<SimulationCtx>,

    pub data_dir: Option<PathBuf>,
    pub errors: Vec<ErrorStrings>,
}

impl App {
    pub fn create(cc: &CreationContext, mut errors: Vec<ErrorStrings>) -> Self {
        let blueprints = vec![
            Arc::new(RwLock::new(crate::circuits::test::TestCircuit.into())),
            Arc::new(RwLock::new(crate::circuits::button::Button.into())),
            Arc::new(RwLock::new(crate::circuits::gates::Gate::and().into())),
            Arc::new(RwLock::new(crate::circuits::gates::Gate::nand().into())),
            Arc::new(RwLock::new(crate::circuits::gates::Gate::or().into())),
            Arc::new(RwLock::new(crate::circuits::gates::Gate::nor().into())),
            Arc::new(RwLock::new(crate::circuits::gates::Gate::xor().into())),
            Arc::new(RwLock::new(crate::circuits::gates::Gate::xnor().into())),
        ];

        let data_dir = directories_next::ProjectDirs::from("", "", APP_NAME)
            .map(|proj_dirs| proj_dirs.data_dir().to_path_buf());

        let sim = data_dir.as_ref().and_then(|d| {
            let autosave = d.join("autosave");

            let file = match File::open(autosave) {
                Ok(f) => f,
                Err(e) if matches!(e.kind(), std::io::ErrorKind::NotFound) => return None,
                Err(e) => return Some(Err(eyre::Report::new(e).wrap_err("opening autosave"))),
            };

            Some(
                smoldata::read_from(file)
                    .map(|d| SimulationCtx::load(&d, &blueprints))
                    .wrap_err("loading simulation state"),
            )
        });
        
        let sim = sim.unwrap_or_else(|| Ok(SimulationCtx::new()));

        let sim = match sim {
            Ok(s) => s,
            Err(e) => {
                errors.push(e.into());
                SimulationCtx::new()
            }
        };

        Self {
            gl: cc.gl.clone().expect("started in OpenGL context"),
            selected_item: None,
            selected_tab: None,
            blueprints,
            style: Arc::new(Style::default()),
            sim,

            data_dir,
            errors,
        }
    }
}

pub struct DockedApp {
    dock: DockState<Tab>,
    app: App,
    sim_no_save: bool,
}

impl DockedApp {
    pub fn create(cc: &CreationContext) -> Self {
        let dock = cc
            .storage
            .and_then(|s| s.get_string("dock"))
            .map(|s| ron::from_str::<DockState<TabSerde>>(&s));

        let (dock, dock_error) = match dock {
            Some(Ok(d)) => (d, None),
            Some(Err(e)) => (DockState::new(vec![]), Some(e)),
            None => (DockState::new(vec![]), None),
        };

        let errors = dock_error
            .into_iter()
            .map(|e| eyre::Report::new(e).wrap_err("dock loading").into())
            .collect();

        let app = App::create(cc, errors);

        Self {
            dock: dock.map_tabs(|s| Tab::load(s, &app)),
            app,
            sim_no_save: false,
        }
    }
}

impl eframe::App for DockedApp {
    fn update(&mut self, ctx: &eframe::egui::Context, _frame: &mut eframe::Frame) {
        'b: {
            for tab in self.dock.iter_all_tabs() {
                if matches!(tab.1.ty(), SafeTabType::Loaded(TabType::BoardView)) {
                    break 'b;
                }
            }

            self.dock
                .main_surface_mut()
                .push_to_first_leaf(Tab::new(TabType::BoardView, &self.app));
        }

        'b: {
            for tab in self.dock.iter_all_tabs() {
                if matches!(tab.1.ty(), SafeTabType::Loaded(TabType::ComponentList)) {
                    break 'b;
                }
            }

            let tab = Tab::new(TabType::ComponentList, &self.app);

            let surface = self.dock.main_surface_mut();

            surface.split_left(NodeIndex::root(), 0.2, vec![tab]);
        }

        self.app.sim.temp_run();

        self.app.selected_tab = self
            .dock
            .find_active_focused()
            .and_then(|(_, tab)| tab.loaded_ty());

        DockArea::new(&mut self.dock).show(ctx, &mut TabViewer(&mut self.app));

        if !self.app.errors.is_empty() {
            let mut open = true;
            egui::Window::new("Errors").open(&mut open).show(ctx, |ui| {
                let bottom_size_id = ui.id().with("bottomsize");
                let bottom_size: egui::Vec2 = ui
                    .data(|data| data.get_temp(bottom_size_id))
                    .unwrap_or_default();

                egui_extras::StripBuilder::new(ui)
                    .size(egui_extras::Size::remainder())
                    .size(egui_extras::Size::exact(bottom_size.y))
                    .vertical(|mut strip| {
                        strip.cell(|ui| {
                            for error in &self.app.errors {
                                egui::CollapsingHeader::new(&error.main)
                                    .default_open(false)
                                    .show(ui, |ui| {
                                        ui.label(&error.sub);
                                    });
                            }
                        });
                        strip.cell(|ui| {
                            let max_rect = ui.max_rect();
                            let max_rect = egui::Rect::from_min_size(
                                [
                                    max_rect.min.x + (max_rect.width() - bottom_size.x) / 2.0,
                                    max_rect.min.y,
                                ]
                                .into(),
                                max_rect.size(),
                            );
                            let mut child_ui = ui.new_child(
                                egui::UiBuilder::new()
                                    .max_rect(max_rect)
                                    .layout(egui::Layout::left_to_right(egui::Align::Min)),
                            );

                            if child_ui.button("Exit without saving").clicked() {
                                self.sim_no_save = true;
                                ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                            }
                            if child_ui.button("Clear").clicked() {
                                self.app.errors.clear();
                            }

                            let bottom_size = child_ui.min_size();
                            ui.data_mut(|data| *data.get_temp_mut_or_default(bottom_size_id) = bottom_size);
                        })
                    })
            });

            if !open {
                self.app.errors.clear();
            }
        }
    }

    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        let dock = self.dock.map_tabs(|tab| tab.save());
        let dock_str = ron::to_string(&dock);
        if let Ok(dock) = dock_str {
            storage.set_string("dock", dock);
        }

        if let Some(data_dir) = self.app.data_dir.as_ref().filter(|_| !self.sim_no_save) {
            let autosave_path = data_dir.join("autosave");
            let savestate = self.app.sim.save();

            let res = 'save: {
                let file = match File::create(autosave_path) {
                    Ok(v) => v,
                    Err(e) => break 'save Err(e),
                };

                if let Err(e) = smoldata::write_into(&savestate, file) {
                    break 'save Err(e);
                }

                Ok(())
            };
            if let Err(e) = res {
                self.app.errors.push(
                    eyre::Report::new(e)
                        .wrap_err("saving simulation state")
                        .into(),
                );
            }
        }
    }
}

pub enum SelectedItem {
    Wires,
    Selection,
    Circuit(Arc<RwLock<CircuitBlueprint>>),
}
