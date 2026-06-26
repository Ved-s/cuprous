use std::{
    collections::HashMap,
    io::ErrorKind,
    ops::{Deref, Not},
    path::PathBuf,
    sync::{Arc, Weak},
};

use eframe::{CreationContext, Storage, egui::{self, Key}};
use egui_dock::{DockArea, DockState, NodeIndex};
use eyre::eyre;
use parking_lot::RwLock;
use smoldata::{SmolRead, raw::RawValue};

use crate::{
    Style,
    board::Board,
    components::{ComponentBlueprint, ComponentImplBox},
    editor::{BoardEditor, BoardEditorSharedState},
    io::copystate,
    simulation::SimulationCtx,
    state::wires::WireState,
    storage::{Filesystem, ItemType},
    str::ArcStaticStr,
    tabs::{SafeTabType, Tab, TabSerde, TabType, TabViewer},
    vector::{Vec2isize, Vec2usize},
};

pub const COPY_PASTE_BOARD_ITEMS_PREFIX: &str = "cuprousbrditms:";

const BLUEPRINT_DATA_DIR: &str = "blueprint_data";
const PROJECT_AUTOSAVE_DIR: &str = "project_autosave";

pub struct ErrorStrings {
    pub main: String,
    pub sub: String,
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
    pub blueprints: HashMap<ArcStaticStr, Arc<RwLock<ComponentBlueprint>>>,
    pub blueprint_order: Vec<ArcStaticStr>,

    pub style: Arc<Style>,
    pub sim: Arc<SimulationCtx>,

    pub editors: HashMap<u128, Weak<RwLock<BoardEditor>>>,
    pub editor_shared: HashMap<u128, BoardEditorSharedState>,

    pub errors: Vec<ErrorStrings>,
    pub last_active_editor: Option<Weak<RwLock<BoardEditor>>>,

    pub fs: Box<dyn Filesystem>,
    pub egui_storage: Box<dyn Storage>,
}

impl App {
    pub fn create(
        cc: &CreationContext,
        mut errors: Vec<ErrorStrings>,
        mut fs: Box<dyn Filesystem>,
        egui_storage: Box<dyn Storage>,
    ) -> Self {
        let mut blueprints: Vec<ComponentImplBox> = vec![
            crate::components::test::Test.into(),
            crate::components::button::Button::default().into(),
            crate::components::gates::Gate::and().into(),
            crate::components::gates::Gate::nand().into(),
            crate::components::gates::Gate::or().into(),
            crate::components::gates::Gate::nor().into(),
            crate::components::gates::Gate::xor().into(),
            crate::components::gates::Gate::xnor().into(),
            crate::components::gates::not::Not.into(),
            crate::components::constant::Constant::new(WireState::Bool(false)).into(),
            crate::components::buffer::Buffer.into(),
            crate::components::error_filter::ErrorFilter.into(),
            crate::components::clock::Clock::default().into(),
            crate::components::transistor::Transistor::default().into(),
            crate::components::relay::Relay::default().into(),

            #[cfg(feature = "wip_circuits")]
            crate::components::world_io::WorldIO::default().into(),
        ];

        let mut loaded_blueprints = HashMap::new();
        let mut blueprint_order = vec![];
        let mut path = PathBuf::new();

        for mut b in blueprints.drain(..) {
            let id = b.id();

            path.clear();
            path.push(BLUEPRINT_DATA_DIR);
            path.push(id.deref());
            let res = fs.readfile(&path, &mut |r| {
                let mut reader = match smoldata::reader::Reader::new(r) {
                    Ok(r) => r,
                    Err(e) => {
                        errors.push(ErrorStrings::from(
                            eyre::Report::new(e)
                                .wrap_err(format!("Reading blueprint data for {id}")),
                        ));
                        return Ok(());
                    }
                };

                let raw = match RawValue::read(reader.read()) {
                    Ok(r) => r,
                    Err(e) => {
                        errors.push(ErrorStrings::from(
                            eyre::Report::new(e)
                                .wrap_err(format!("Reading blueprint data for {id}")),
                        ));
                        return Ok(());
                    }
                };

                if let Err(e) = b.load_config(&raw) {
                    errors.push(ErrorStrings::from(
                        e.wrap_err(format!("Loading blueprint data for {id}")),
                    ));
                }

                Ok(())
            });

            if let Err(e) = res
                && !matches!(e.kind(), std::io::ErrorKind::NotFound)
            {
                errors.push(ErrorStrings::from(
                    eyre::Report::new(e).wrap_err(format!("Reading blueprint data for {id}")),
                ));
            }

            let b = Arc::new(RwLock::new(ComponentBlueprint::new(b)));
            blueprint_order.push(id.clone());

            assert!(
                loaded_blueprints.insert(id, b).is_none(),
                "dev error: multiple blueprints with same id"
            );
        }

        let sim_loadable = match fs.stat(PROJECT_AUTOSAVE_DIR.as_ref()) {
            Err(e) if matches!(e.kind(), ErrorKind::NotFound) => false,
            Ok(ItemType::Directory) => true,
            Ok(_) => {
                errors.push(ErrorStrings {
                    main: format!("Failed to load autosave from {PROJECT_AUTOSAVE_DIR}"),
                    sub: "Path is not a directory".into(),
                });
                false
            }
            Err(e) => {
                errors.push(ErrorStrings::from(eyre::Report::new(e).wrap_err(format!(
                    "Failed to load autosave from {PROJECT_AUTOSAVE_DIR}"
                ))));
                false
            }
        };

        let sim = if !sim_loadable {
            SimulationCtx::new()
        } else {
            let mut save_project_dir =
                crate::storage::FilesystemDirectory::new(&mut *fs, PROJECT_AUTOSAVE_DIR.into())
                    .unwrap();
            SimulationCtx::load(&mut save_project_dir, &loaded_blueprints, &mut errors)
        };

        Self {
            gl: cc.gl.clone().expect("started in OpenGL context"),
            selected_item: None,
            selected_tab: None,
            blueprints: loaded_blueprints,
            blueprint_order,
            style: Arc::new(Style::default()),
            sim,
            editors: Default::default(),
            editor_shared: Default::default(),
            last_active_editor: None,

            errors,

            fs,
            egui_storage,
        }
    }

    pub fn update(&mut self, ctx: &egui::Context) {
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

            let pasted_components: Vec<_> = paste
                .components
                .into_iter()
                .filter_map(|c| {
                    let blueprint = self.blueprints.get(&c.id).map(|b| b.read().clone());

                    let Some(mut blueprint) = blueprint else {
                        self.errors
                            .push(eyre!("unknown component id \"{}\"", c.id).into());
                        return None;
                    };

                    blueprint.transform.dir = c.dir;
                    blueprint.transform.flip = c.flip;

                    if let Some(config) = c.config
                        && let Err(e) = blueprint.imp.load_config(&config)
                    {
                        self.errors.push(
                            e.wrap_err(format!(
                                "loading component config for \"{}\" ({})",
                                blueprint.display_name, blueprint.id
                            ))
                            .into(),
                        );
                    }

                    blueprint.recalculate();

                    Some(PasteComponent {
                        pos: c.pos,
                        blueprint,
                        instance: c.instance,
                        state: c.state,
                        timer: c.timer,
                    })
                })
                .collect();

            let mut size = Vec2usize::single_value(0);

            for c in &pasted_components {
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
                components: pasted_components,
                size,
            };

            self.selected_item = Some(SelectedItem::Paste(paste));
        }

        for v in self.editor_shared.values_mut() {
            v.update();
        }
    }

    pub fn get_board_editor(&mut self, board: &Arc<Board>) -> Arc<RwLock<BoardEditor>> {
        self.editors.retain(|_, v| v.upgrade().is_some());
        if let Some(editor) = self.editors.get(&board.uid()).and_then(|w| w.upgrade()) {
            return editor;
        }

        let ed = Arc::new(RwLock::new(BoardEditor::new(board.clone())));
        self.editors.insert(board.uid(), Arc::downgrade(&ed));
        ed
    }

    fn save(&mut self) {
        'save_blueprints: {
            if let Err(e) = self.fs.mkdir(BLUEPRINT_DATA_DIR.as_ref()) {
                self.errors.push(ErrorStrings::from(
                    eyre::Report::new(e).wrap_err("Creating blueprint data directory"),
                ));
                break 'save_blueprints;
            }

            let mut path = PathBuf::new();
            for (id, b) in &self.blueprints {
                path.clear();
                path.push(BLUEPRINT_DATA_DIR);
                path.push(id.deref());

                let config = b.read().imp.save_config();
                let config = match config {
                    None => {
                        self.fs.rmfile(&path).ok();
                        continue;
                    }
                    Some(c) => c,
                };

                let res = self
                    .fs
                    .writefile(&path, &mut |w| smoldata::write_into(&config, w));

                if let Err(e) = res {
                    self.errors.push(ErrorStrings::from(
                        eyre::Report::new(e).wrap_err(format!("Saving blueprint data for {id}")),
                    ));
                }
            }
        }

        'save_project: {
            if let Err(e) = self.fs.mkdir(PROJECT_AUTOSAVE_DIR.as_ref()) {
                self.errors.push(ErrorStrings::from(
                    eyre::Report::new(e).wrap_err("Project autosave directory"),
                ));
                break 'save_project;
            }

            let mut save_project_dir = crate::storage::FilesystemDirectory::new(
                &mut *self.fs,
                PROJECT_AUTOSAVE_DIR.into(),
            )
            .unwrap();
            self.sim.save(&mut save_project_dir, &mut self.errors);
        }
    }
}

pub struct DockedApp {
    dock: DockState<Tab>,
    app: App,
    no_save: bool,
}

impl DockedApp {
    pub fn create(
        cc: &CreationContext,
        fs: Box<dyn Filesystem>,
        egui_storage: Box<dyn Storage>,
    ) -> Self {
        let dock = egui_storage
            .get_string("dock")
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

        let mut app = App::create(cc, errors, fs, egui_storage);

        Self {
            dock: dock.map_tabs(|s| Tab::load(s, &mut app)),
            app,
            no_save: false,
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
                .push_to_first_leaf(Tab::new(TabType::BoardView, &mut self.app));
        }

        'b: {
            for tab in self.dock.iter_all_tabs() {
                if matches!(tab.1.ty(), SafeTabType::Loaded(TabType::ComponentProps)) {
                    break 'b;
                }
            }

            let tab = Tab::new(TabType::ComponentProps, &mut self.app);

            let surface = self.dock.main_surface_mut();

            surface.split_right(NodeIndex::root(), 0.2, vec![tab]);
        }

        'b: {
            for tab in self.dock.iter_all_tabs() {
                if matches!(tab.1.ty(), SafeTabType::Loaded(TabType::ComponentList)) {
                    break 'b;
                }
            }

            let tab = Tab::new(TabType::ComponentList, &mut self.app);

            let surface = self.dock.main_surface_mut();

            surface.split_left(NodeIndex::root(), 0.2, vec![tab]);
        }

        'b: {
            if !ctx.input(|input| input.key_pressed(Key::F12) && input.modifiers.shift) {
                break 'b;
            }

            for tab in self.dock.iter_all_tabs() {
                if matches!(tab.1.ty(), SafeTabType::Loaded(TabType::DebugInfo)) {
                    break 'b;
                }
            }

            let tab = Tab::new(TabType::DebugInfo, &mut self.app);

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
                                self.no_save = true;
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

    fn save(&mut self, _: &mut dyn eframe::Storage) {
        if self.no_save {
            return;
        }

        let dock = self.dock.map_tabs(|tab| tab.save());
        let dock_str = ron::to_string(&dock);
        if let Ok(dock) = dock_str {
            self.app.egui_storage.set_string("dock", dock);
        }

        self.app.save();
    }
}

pub struct PasteComponent {
    pub pos: Vec2usize,
    pub blueprint: ComponentBlueprint,
    pub instance: Option<RawValue>,
    pub state: Option<RawValue>,
    pub timer: Option<(u128, Option<u128>)>,
}

pub struct Paste {
    pub wire_parts: Vec<copystate::WirePart>,
    pub wire_points: Vec<Vec2usize>,
    pub components: Vec<PasteComponent>,
    pub size: Vec2usize,
}

pub enum SelectedItem {
    Wires,
    Selection,
    Component(Arc<RwLock<ComponentBlueprint>>),
    Paste(Paste),
}
