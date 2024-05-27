use super::{TabCreation, TabImpl};

pub struct BoardView {

}

impl TabCreation for BoardView {
    fn new() -> Self {
        Self {}
    }

    fn load(_data: &str) -> Result<Self, Box<dyn std::error::Error>> {
        Err("Test error".into())
    }
}

impl TabImpl for BoardView {
    fn update(&mut self, app: &mut crate::app::App, ui: &mut eframe::egui::Ui) {
    }
}