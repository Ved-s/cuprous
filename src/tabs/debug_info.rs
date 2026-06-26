use eframe::egui::Ui;
use std::fmt::Write;
use crate::{app::App, tabs::{TabCreation, TabImpl}};

pub struct DebugInfo {

}

impl TabCreation for DebugInfo {
    fn new(_app: &mut App) -> Self {
        DebugInfo {  }
    }
}

impl TabImpl for DebugInfo {
    fn update(&mut self, app: &mut App, ui: &mut Ui) {
        ui.collapsing("Multiwire connections", |ui| {
            for board in app.sim.boards().read().values() {

                let components = board.components().read();

                ui.collapsing(format!("Board {} ({:032x})", board.name().read(), board.uid()), |ui| {
                    let connections = board.multiwire_connections().read();

                    let mut string = String::new();

                    for (wire, connections) in connections.iter() {
                        writeln!(&mut string, "wire {}:", wire).ok();

                        for (component, pin) in connections {
                            write!(&mut string, " -> ").ok();
                            let Some(component) = components.get(*component) else {
                                writeln!(&mut string, "<err> id {component}").ok();
                                continue;
                            };
                            write!(&mut string, "{} id {}, pin ", component.imp.read().imp.display_name(), component.id).ok();

                            match component.pins.read().get(*pin) {
                                None => write!(&mut string, "<err> id {pin}").ok(),
                                Some(pin) => {
                                    write!(&mut string, "{} id {}", pin.desc.display_name, pin.pin.id).ok()
                                },
                            };
                            writeln!(&mut string).ok();
                        }
                    }

                    ui.label(string)
                });
            }
        });
    }
}