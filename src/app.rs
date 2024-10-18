use std::sync::Arc;

use eframe::CreationContext;
use egui_dock::{DockArea, DockState, NodeIndex};
use parking_lot::RwLock;

use crate::{
    circuits::CircuitBlueprint, simulation::SimulationCtx, tabs::{SafeTabType, Tab, TabSerde, TabType, TabViewer}, Style
};

pub struct App {
    pub gl: Arc<glow::Context>,
    pub selected_item: Option<SelectedItem>,
    pub selected_tab: Option<TabType>,
    pub blueprints: Vec<Arc<RwLock<CircuitBlueprint>>>,
    pub style: Arc<Style>,
    pub sim: Arc<SimulationCtx>,
}

impl App {
    pub fn create(cc: &CreationContext, dock_load_error: Option<ron::error::SpannedError>) -> Self {
        let blueprints = vec![
            Arc::new(RwLock::new(crate::circuits::test::TestCircuit.into())),
            Arc::new(RwLock::new(crate::circuits::button::Button.into())),
        ];

        Self {
            gl: cc.gl.clone().expect("started in OpenGL context"),
            selected_item: None,
            selected_tab: None,
            blueprints,
            style: Arc::new(Style::default()),
            sim: SimulationCtx::new(),
        }
    }
}

pub struct DockedApp {
    dock: DockState<Tab>,
    app: App,
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

        let app = App::create(cc, dock_error);

        Self {
            dock: dock.map_tabs(|s| Tab::load(s, &app)),
            app,
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
    }

    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        let dock = self.dock.map_tabs(|tab| tab.save());
        let dock_str = ron::to_string(&dock);
        if let Ok(dock) = dock_str {
            storage.set_string("dock", dock);
        }
    }
}

pub enum SelectedItem {
    Wires,
    Selection,
    Circuit(Arc<RwLock<CircuitBlueprint>>),
}
