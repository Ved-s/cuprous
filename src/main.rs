use app::DockedApp;

mod app;
mod vertex_renderer;
mod vector;
mod tabs;
mod str;
mod macros;

fn main() -> Result<(), eframe::Error> {
    let options = eframe::NativeOptions {
        viewport: eframe::egui::ViewportBuilder::default().with_title("cuprous"),
        ..Default::default()
    };

    eframe::run_native(
        "cuprous-dev",
        options,
        Box::new(|cc| Box::new(DockedApp::create(cc))),
    )
}
