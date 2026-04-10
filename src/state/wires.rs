use std::{any::type_name, ops::Deref};

use smoldata::{SmolRead, SmolWrite, reader::ReadError};

use crate::{Style, board::Wire};

#[derive(Clone, Default, PartialEq, Eq)]
pub enum WireState {
    #[default]
    None,
    Bool(bool),
    Error,
}
impl WireState {
    pub fn combine(&mut self, other: &WireState) {
        let this = std::mem::take(self);
        *self = match (this, other) {
            (WireState::None, other) => other.clone(),
            (other, WireState::None) => other,

            (WireState::Error, _) | (_, WireState::Error) => WireState::Error,

            (WireState::Bool(a), WireState::Bool(b)) => {
                if a == *b {
                    WireState::Bool(a)
                } else {
                    WireState::Error
                }
            }
        };
    }

    pub fn type_eq(&self, other: &WireState) -> bool {
        match (self, other) {
            (WireState::None, WireState::None) => true,
            (WireState::Bool(a), WireState::Bool(b)) => a == b,
            (WireState::Error, WireState::Error) => true,

            (WireState::None, _) => false,
            (WireState::Bool(_), _) => false,
            (WireState::Error, _) => false,
        }
    }
}

impl SmolWrite for WireState {
    fn write(&self, writer: smoldata::writer::ValueWriter) -> std::io::Result<()> {
        match self {
            WireState::None => writer.write_none(),
            WireState::Bool(b) => writer.write_primitive(*b),
            WireState::Error => writer.write_unit_variant("Error"),
        }
    }
}

impl SmolRead for WireState {
    fn read(reader: smoldata::reader::ValueReader) -> smoldata::reader::ReadResult<Self> {
        let read = reader.read()?;
        match read {
            smoldata::reader::ValueReading::Primitive(smoldata::reader::Primitive::Bool(b)) => {
                Ok(Self::Bool(b))
            }
            smoldata::reader::ValueReading::Option(None) => Ok(Self::None),
            smoldata::reader::ValueReading::Enum(er) => {
                let (name, ty) = er.read_variant()?;
                match name.deref() {
                    "Error" => {
                        ty.take_unit_variant().map_err(|e| {
                            ReadError::from(e.with_variant_name_of::<Self>("Error"))
                        })?;
                        Ok(Self::Error)
                    }
                    _ => {
                        let err = ReadError::UnexpectedEnumVariant {
                            name,
                            type_name: type_name::<Self>(),
                        };
                        Err(err.into())
                    }
                }
            }
            rest => {
                let err = smoldata::reader::UnexpectedValueError {
                    found: rest.ty(),
                    expected: smoldata::reader::ValueTypeRequirement::Custom(
                        "None, Bool or Error enum".into(),
                    ),
                };
                Err(ReadError::from(err.with_type_name_of::<Self>()).into())
            }
        }
    }
}

#[derive(Default)]
pub struct BoardWiresState {
    pub wires: Vec<WireState>,
}

impl BoardWiresState {
    pub fn wire_color(&self, wire: &Wire, style: &Style) -> eframe::egui::Color32 {
        let state = self.get_wire(wire.id);

        // TODO: wire color overrides
        style.wire_colors.get(&state)
    }

    pub fn get_wire(&self, id: usize) -> WireState {
        self.wires.get(id).cloned().unwrap_or_default()
    }

    /// Returns true if value was changed
    pub fn set_wire(&mut self, id: usize, state: WireState) -> bool {
        if self.wires.len() <= id {
            if state == WireState::default() {
                return false;
            }

            let add = id - self.wires.len() + 1;
            self.wires.reserve(add);
            for _ in 0..add {
                self.wires.push(WireState::default());
            }
        }

        if self.wires[id] == state {
            return false;
        }

        self.wires[id] = state;
        true
    }

    pub fn reset(&mut self) {
        self.wires.clear();
    }
}
