use eframe::CreationContext;
use egui_dock::{DockArea, DockState};

use crate::tabs::{SafeTabType, Tab, TabSerde, TabType, TabViewer};

pub struct App {}

impl App {
    pub fn create(cc: &CreationContext, dock_load_error: Option<ron::error::SpannedError>) -> Self {
        Self {}
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

        Self {
            dock: dock.map_tabs(Tab::load),
            app: App::create(cc, dock_error),
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
                .push_to_first_leaf(Tab::new(TabType::BoardView));
        }

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
