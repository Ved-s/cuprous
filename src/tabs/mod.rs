mod board_view;
mod component_list;

use std::{
    error::Error,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

use eframe::egui::{Id, Ui, WidgetText};
use serde::{Deserialize, Serialize};

use crate::{app::App, define_tab_type, str::ArcStaticStr};

define_tab_type! {
    #[derive(Clone, Copy)]
    pub enum TabType {
        #[id = "component_list"]
        #[impl = component_list::ComponentList, loadable = true]
        #[title = "Components"]
        #[closeable = false]
        ComponentList,

        #[id = "board_view"]
        #[impl = board_view::BoardView, loadable = true]
        #[title = "Board view"]
        #[closeable = false]
        BoardView,
    }
}

static TAB_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone)]
pub enum SafeTabType {
    Loaded(TabType),
    Unloaded(Arc<str>),
}

#[derive(Serialize, Deserialize)]
pub struct TabSerde {
    id: ArcStaticStr,

    #[serde(skip_serializing_if = "Option::is_none", default)]
    data: Option<String>,
}

pub struct Tab {
    id: usize,
    ty: SafeTabType,
    imp: Box<dyn TabImpl>,
}

impl Tab {
    pub fn new(ty: TabType, app: &App) -> Self {
        Self {
            id: Self::next_id(),
            ty: SafeTabType::Loaded(ty),
            imp: ty.create_impl(app),
        }
    }

    pub fn load(d: &TabSerde, app: &App) -> Self {
        let imp = TabType::from_id(&d.id).map(|ty| {
            (
                ty,
                match &d.data {
                    Some(d) => ty.load_impl(d, app),
                    None => Ok(ty.create_impl(app)),
                },
            )
        });

        let ty = match &imp {
            None => SafeTabType::Unloaded(d.id.clone().into()),
            Some((ty, _)) => SafeTabType::Loaded(*ty),
        };

        let imp = match imp {
            Some((_, Ok(imp))) => imp,
            Some((_, Err(e))) => Box::new(ErrorTab {
                error: format!("Error while loading tab: {e}"),
                data: d.data.clone(),
            }),
            None => Box::new(ErrorTab {
                error: format!("Unknown tab id: {}", d.id),
                data: d.data.clone(),
            }),
        };

        Self {
            id: Self::next_id(),
            ty,
            imp,
        }
    }

    pub fn save(&self) -> TabSerde {
        let id = match &self.ty {
            SafeTabType::Loaded(ty) => ty.id().into(),
            SafeTabType::Unloaded(a) => a.clone().into(),
        };
        TabSerde {
            id,
            data: self.imp.save_data(),
        }
    }

    pub fn ty(&self) -> SafeTabType {
        self.ty.clone()
    }

    pub fn loaded_ty(&self) -> Option<TabType> {
        match &self.ty {
            SafeTabType::Loaded(ty) => Some(*ty),
            SafeTabType::Unloaded(_) => None,
        }
    }

    pub fn next_id() -> usize {
        TAB_ID_COUNTER.fetch_add(1, Ordering::Relaxed)
    }
}

pub trait TabCreation
where
    Self: Sized,
{
    fn new(app: &App) -> Self;

    fn load(data: &str, app: &App) -> Result<Self, Box<dyn Error>> {
        let _ = data;
        Ok(Self::new(app))
    }
}

pub trait TabImpl {
    fn update(&mut self, app: &mut App, ui: &mut Ui);

    fn title(&self) -> Option<WidgetText> {
        None
    }

    fn save_data(&self) -> Option<String> {
        None
    }

    fn show_reset_button(&self) -> bool {
        false
    }

    fn tab_style_override(&self, global: &egui_dock::TabStyle) -> Option<egui_dock::TabStyle> {
        let _ = global;
        None
    }
}

struct ErrorTab {
    error: String,
    data: Option<String>,
}

impl TabImpl for ErrorTab {
    fn update(&mut self, _app: &mut App, ui: &mut Ui) {
        ui.horizontal_wrapped(|ui| {
            ui.label(&self.error);
        });
    }

    fn save_data(&self) -> Option<String> {
        self.data.clone()
    }

    fn show_reset_button(&self) -> bool {
        true
    }
}

pub struct TabViewer<'a>(pub &'a mut App);

impl<'a> egui_dock::TabViewer for TabViewer<'a> {
    type Tab = Tab;

    fn id(&mut self, tab: &mut Self::Tab) -> Id {
        Id::new("cuprtabs").with(tab.id)
    }

    fn closeable(&mut self, tab: &mut Self::Tab) -> bool {
        match &tab.ty {
            SafeTabType::Loaded(ty) => ty.closeable(),
            SafeTabType::Unloaded(_) => true,
        }
    }

    fn tab_style_override(&self, tab: &Self::Tab, global_style: &egui_dock::TabStyle) -> Option<egui_dock::TabStyle> {
        tab.imp.tab_style_override(global_style)
    }

    fn title(&mut self, tab: &mut Self::Tab) -> WidgetText {
        tab.imp.title().unwrap_or_else(|| match &tab.ty {
            SafeTabType::Loaded(ty) => ty.title().into(),
            SafeTabType::Unloaded(_) => "Unloaded tab".into(),
        })
    }

    fn ui(&mut self, ui: &mut Ui, tab: &mut Self::Tab) {
        let reset_button_ty = match tab.ty {
            SafeTabType::Loaded(ty) => tab.imp.show_reset_button().then_some(ty),
            SafeTabType::Unloaded(_) => None,
        };

        if let Some(ty) = reset_button_ty {
            ui.vertical_centered(|ui| {
                if ui.button("Reset tab").clicked() {
                    tab.imp = ty.create_impl(self.0);
                }
            });
        }

        tab.imp.update(self.0, ui);
    }
}
