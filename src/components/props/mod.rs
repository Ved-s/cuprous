use std::{
    any::{Any, TypeId},
    mem::discriminant,
};

use eframe::egui::{ComboBox, Ui, UiKind};

use crate::{state::wires::WireState, str::ArcStaticStr};

#[derive(Debug, Clone)]
pub struct PropertyInfo {
    pub id: ArcStaticStr,
    pub display_name: ArcStaticStr,
    pub type_id: TypeId,

    /// Changing this property affects component size, which tiles component occupies, or component pins in any way
    pub affects_geometry_or_pins: bool,
}

pub trait PropertyValue: Any {
    fn clone_dyn(&self) -> Box<dyn PropertyValue>;
    fn clone_into_dyn(&self, other: &mut dyn PropertyValue);

    /// Return new value when changed
    fn ui(&self, ui: &mut Ui) -> Option<Box<dyn PropertyValue>>;
}

impl PropertyValue for WireState {
    fn clone_dyn(&self) -> Box<dyn PropertyValue> {
        Box::new(self.clone())
    }

    fn clone_into_dyn(&self, other: &mut dyn PropertyValue) {
        if let Some(other) = (other as &mut dyn Any).downcast_mut::<Self>() {
            self.clone_into(other);
        }
    }

    fn ui(&self, ui: &mut Ui) -> Option<Box<dyn PropertyValue>> {
        fn state_name(state: &WireState) -> &'static str {
            match state {
                WireState::None => "None",
                WireState::Bool(true) => "True",
                WireState::Bool(false) => "False",
                WireState::Error => "Error",
            }
        }

        fn state_loose_eq(a: &WireState, b: &WireState) -> bool {
            if discriminant(a) != discriminant(b) {
                return false;
            }

            match (a, b) {
                (WireState::Bool(a), WireState::Bool(b)) => a == b,
                _ => true,
            }
        }

        let mut new = None;

        ComboBox::new("wsvalue", "")
            .selected_text(state_name(self))
            .show_ui(ui, |ui| {
                let values = &[
                    WireState::None,
                    WireState::Bool(false),
                    WireState::Bool(true),
                    WireState::Error,
                ];
                for v in values {
                    let checked = state_loose_eq(v, self);
                    if ui.selectable_label(checked, state_name(v)).clicked() && !checked {
                        new = Some(v.clone());

                        ui.close_kind(UiKind::Menu);
                    }
                }
            });

        if let WireState::Bool(b) = self {
            let mut b = *b;
            if ui.checkbox(&mut b, "Bool value").changed() {
                new = Some(WireState::Bool(b));
            }
        }

        new.map(|v| Box::new(v) as Box<_>)
    }
}

impl PropertyValue for bool {
    fn clone_dyn(&self) -> Box<dyn PropertyValue> {
        Box::new(*self)
    }

    fn clone_into_dyn(&self, other: &mut dyn PropertyValue) {
        if let Some(other) = (other as &mut dyn Any).downcast_mut::<Self>() {
            self.clone_into(other);
        }
    }

    fn ui(&self, ui: &mut Ui) -> Option<Box<dyn PropertyValue>> {
        let mut checked = *self;
        ui.checkbox(&mut checked, "");

        if *self != checked {
            return Some(Box::new(checked));
        }

        None
    }
}