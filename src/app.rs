use std::{collections::{HashMap, HashSet}, ops::Not, path::PathBuf, sync::{Arc, Weak}};

use eframe::{egui, CreationContext};
use egui_dock::{DockArea, DockState, NodeIndex};
use eyre::eyre;
use parking_lot::{Mutex, RwLock};
use smoldata::raw::RawValue;

use crate::{
    circuits::CircuitBlueprint, editor::BoardEditor, io::copystate, simulation::{SimulationCtx, SimulationStateData}, state::wires::WireState, tabs::{SafeTabType, Tab, TabSerde, TabType, TabViewer}, vector::{Vec2isize, Vec2usize}, Style
};

pub const APP_NAME: &str = "cuprous-dev";

pub const COPY_PASTE_BOARD_ITEMS_PREFIX: &str = "cuprousbrditms:";

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

    pub editors: Mutex<HashMap<u128, Weak<RwLock<BoardEditor>>>>,

    pub data_dir: Option<PathBuf>,
    pub errors: Vec<ErrorStrings>,
    pub last_active_editor: Option<LastEditorData>,
}

impl App {
    pub fn create(cc: &CreationContext, mut errors: Vec<ErrorStrings>) -> Self {
        let blueprints = vec![
            Arc::new(RwLock::new(crate::circuits::test::TestCircuit.into())),
            Arc::new(RwLock::new(crate::circuits::button::Button::default().into())),
            Arc::new(RwLock::new(crate::circuits::gates::Gate::and().into())),
            Arc::new(RwLock::new(crate::circuits::gates::Gate::nand().into())),
            Arc::new(RwLock::new(crate::circuits::gates::Gate::or().into())),
            Arc::new(RwLock::new(crate::circuits::gates::Gate::nor().into())),
            Arc::new(RwLock::new(crate::circuits::gates::Gate::xor().into())),
            Arc::new(RwLock::new(crate::circuits::gates::Gate::xnor().into())),
            Arc::new(RwLock::new(crate::circuits::gates::not::Not.into())),
            Arc::new(RwLock::new(
                crate::circuits::constant::Constant::new(WireState::Bool(false)).into(),
            )),
            Arc::new(RwLock::new(
                crate::circuits::constant::Constant::new(WireState::Bool(true)).into(),
            )),
            Arc::new(RwLock::new(crate::circuits::buffer::Buffer.into())),
            Arc::new(RwLock::new(
                crate::circuits::error_filter::ErrorFilter.into(),
            )),
        ];

        let data_dir = directories_next::ProjectDirs::from("", "", APP_NAME)
            .map(|proj_dirs| proj_dirs.data_dir().to_path_buf());

        // let sim = data_dir.as_ref().and_then(|d| {
        //     let autosave = d.join("autosave");

        //     let file = match File::open(autosave) {
        //         Ok(f) => f,
        //         Err(e) if matches!(e.kind(), std::io::ErrorKind::NotFound) => return None,
        //         Err(e) => return Some(Err(eyre::Report::new(e).wrap_err("opening autosave"))),
        //     };

        //     Some(
        //         smoldata::read_from(file)
        //             .map(|d| SimulationCtx::load(&d, &blueprints))
        //             .wrap_err("loading simulation state"),
        //     )
        // });

        // let sim = None;

        // let sim = sim.unwrap_or_else(|| Ok(SimulationCtx::new()));

        // let sim = match sim {
        //     Ok(s) => s,
        //     Err(e) => {
        //         errors.push(e.into());
        //         SimulationCtx::new()
        //     }
        // };

        let sim = SimulationCtx::new();

        Self {
            gl: cc.gl.clone().expect("started in OpenGL context"),
            selected_item: None,
            selected_tab: None,
            blueprints,
            style: Arc::new(Style::default()),
            sim,
            editors: Default::default(),
            last_active_editor: None,

            data_dir,
            errors,
        }
    }

    fn update(&mut self, ctx: &egui::Context) {
        let paste = ctx
            .wants_keyboard_input()
            .not()
            .then(|| {
                ctx.input(|input| {
                    input.events.iter().find_map(|e| match e {
                        eframe::egui::Event::Paste(s) => Some(s.clone()),
                        _ => None,
                    })
                })
            })
            .flatten();

        'trypaste: {
            let Some(paste) = paste else {
                break 'trypaste;
            };

            if paste.is_empty() {
                self.errors.push(eyre!("Clipboard is empty").into());
                break 'trypaste;
            }

            let data = match paste.strip_prefix(COPY_PASTE_BOARD_ITEMS_PREFIX) {
                Some(d) => d,
                None => {
                    self.errors.push(eyre!("Clipboard is empty").into());
                    break 'trypaste;
                }
            };

            let cur = std::io::Cursor::new(data.as_bytes());
            let base64 = base64::read::DecoderReader::new(
                cur,
                &base64::engine::general_purpose::STANDARD_NO_PAD,
            );
            let flate = flate2::read::DeflateDecoder::new(base64);
            let paste: copystate::CopyState = match smoldata::read_from(flate) {
                Ok(p) => p,
                Err(e) => {
                    self.errors
                        .push(eyre::Report::new(e).wrap_err("reading paste").into());
                    break 'trypaste;
                }
            };

            let pasted_circuits: Vec<_> = paste
                .circuits
                .into_iter()
                .filter_map(|c| {
                    let blueprint = self.blueprints.iter().find_map(|b| {
                        let b = b.read();
                        b.id.eq(&c.id).then(|| b.clone())
                    });

                    let Some(mut blueprint) = blueprint else {
                        self.errors
                            .push(eyre!("unknown circuit id \"{}\"", c.id).into());
                        return None;
                    };

                    blueprint.transform.dir = c.dir;
                    blueprint.transform.flip = c.flip;

                    if let Some(config) = c.config {
                        if let Err(e) = blueprint.imp.load_config(&config) {
                            self.errors.push(
                                e.wrap_err(format!(
                                    "loading circuit config for \"{}\" ({})",
                                    blueprint.display_name, blueprint.id
                                ))
                                .into(),
                            );
                        }
                    }

                    blueprint.recalculate();

                    Some(PasteCircuit {
                        pos: c.pos,
                        blueprint,
                        instance: c.instance,
                        state: c.state,
                    })
                })
                .collect();

            let mut size = Vec2usize::single_value(0);

            for c in &pasted_circuits {
                let max = c.pos + c.blueprint.transformed_size;
                size = [max.x.max(size.x), max.y.max(size.y)].into();
            }

            for p in &paste.wire_points {
                let max = *p + 1;
                size = [max.x.max(size.x), max.y.max(size.y)].into();
            }

            for p in &paste.wire_parts {
                let pos2 = p.pos.convert(|v| v as isize) + p.dir.into_dir_isize() * p.len as isize;

                let max =
                    Vec2isize::new(pos2.x.max(p.pos.x as isize), pos2.y.max(p.pos.y as isize)) + 1;
                if max.x > 0 {
                    size.x = size.x.max(max.x as usize);
                }
                if max.y > 0 {
                    size.y = size.y.max(max.y as usize);
                }
            }

            let paste = Paste {
                wire_parts: paste.wire_parts,
                wire_points: paste.wire_points,
                circuits: pasted_circuits,
                size,
            };

            self.selected_item = Some(SelectedItem::Paste(paste));
        }
    }
    
    pub fn get_board_editor(&self, state: &SimulationStateData) -> Arc<RwLock<BoardEditor>> {
        let mut editors = self.editors.lock();
        editors.retain(|_, v| v.upgrade().is_some());
        if let Some(editor) = editors.get(&state.uid()).and_then(|w| w.upgrade()) {
            return editor;
        }

        let ed = Arc::new(RwLock::new(BoardEditor::new(state.board())));
        editors.insert(state.uid(), Arc::downgrade(&ed));
        ed
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
                if matches!(tab.1.ty(), SafeTabType::Loaded(TabType::CircuitProps)) {
                    break 'b;
                }
            }

            let tab = Tab::new(TabType::CircuitProps, &self.app);

            let surface = self.dock.main_surface_mut();

            surface.split_right(NodeIndex::root(), 0.2, vec![tab]);
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

        self.app.update(ctx);

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
                            ui.data_mut(|data| {
                                *data.get_temp_mut_or_default(bottom_size_id) = bottom_size
                            });
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

        // if let Some(data_dir) = self.app.data_dir.as_ref().filter(|_| !self.sim_no_save) {
        //     let autosave_path = data_dir.join("autosave");
        //     let savestate = self.app.sim.save();

        //     let res = 'save: {
        //         let file = match File::create(autosave_path) {
        //             Ok(v) => v,
        //             Err(e) => break 'save Err(e),
        //         };

        //         if let Err(e) = smoldata::write_into(&savestate, file) {
        //             break 'save Err(e);
        //         }

        //         Ok(())
        //     };
        //     if let Err(e) = res {
        //         self.app.errors.push(
        //             eyre::Report::new(e)
        //                 .wrap_err("saving simulation state")
        //                 .into(),
        //         );
        //     }
        // }
    }
}

pub struct LastEditorData {
    pub editor: Weak<RwLock<BoardEditor>>,
    pub boardview_id: usize,
    pub selected_circuits: HashSet<usize>,
    pub selection_update_counter: usize,
}

pub struct PasteCircuit {
    pub pos: Vec2usize,
    pub blueprint: CircuitBlueprint,
    pub instance: Option<RawValue>,
    pub state: Option<RawValue>,
}

pub struct Paste {
    pub wire_parts: Vec<copystate::WirePart>,
    pub wire_points: Vec<Vec2usize>,
    pub circuits: Vec<PasteCircuit>,
    pub size: Vec2usize,
}

pub enum SelectedItem {
    Wires,
    Selection,
    Circuit(Arc<RwLock<CircuitBlueprint>>),
    Paste(Paste),
}
