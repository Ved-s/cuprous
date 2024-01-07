use eframe::egui::{ComboBox, Ui};

use crate::{circuits::*, describe_directional_custom_circuit, Direction4};

use super::props::{CircuitProperty, CircuitPropertyImpl};

#[allow(clippy::upper_case_acronyms)]
#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub enum TransistorType {
    #[default]
    NPN,
    PNP,
}

pub struct Transistor {
    collector: CircuitPinInfo,
    base: CircuitPinInfo,
    emitter: CircuitPinInfo,
    ty: TransistorType,
    dir: Direction4,
    flip: bool,
}

impl Transistor {
    fn new() -> Self {
        let description = Self::describe(Direction4::Left, false);
        Self {
            collector: description.pins[0].to_info(),
            base: description.pins[1].to_info(),
            emitter: description.pins[2].to_info(),
            ty: TransistorType::NPN,
            dir: Direction4::Left,
            flip: false,
        }
    }

    fn describe_props(props: &CircuitPropertyStore) -> CircuitDescription<3> {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Left);
        let flip = props.read_clone("flip").unwrap_or(false);
        Self::describe(dir, flip)
    }

    fn describe(dir: Direction4, flip: bool) -> CircuitDescription<3> {
        describe_directional_custom_circuit! {
            default_dir: Left,
            dir: dir,
            flip: flip,
            size: [2, 3],
            "collector": Inside, "Collector", Up, [1, 0],
            "base":      Inside, "Base", Left, [0, 1],
            "emitter":   Outside, "Emitter", Down, [1, 2],

            dir_proc: |dir| if flip && dir != Left { dir.inverted() } else { dir },
            pos_proc: |pos| if flip { [pos[0], 2 - pos[1]] } else { pos }
        }
    }

    pub fn is_open_state(ty: TransistorType, base: &WireState) -> bool {
        matches!(
            (ty, base),
            (TransistorType::NPN, WireState::True) | (TransistorType::PNP, WireState::False)
        )
    }
}

impl CircuitImpl for Transistor {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext) {
        let collector_color = self
            .collector
            .wire_or_self_color(state_ctx, paint_ctx.style);
        let base_color = self.base.wire_or_self_color(state_ctx, paint_ctx.style);
        let emitter_color = self.emitter.wire_or_self_color(state_ctx, paint_ctx.style);

        let base = self.base.get_state(state_ctx);
        let angle = self.dir.inverted_ud().angle_to_left();
        let open = Self::is_open_state(self.ty, &base);

        crate::graphics::transistor(
            self.ty,
            angle,
            self.flip,
            collector_color,
            base_color,
            emitter_color,
            open,
            paint_ctx,
        );
    }

    fn create_pins(&mut self, circ: &Arc<Circuit>) -> Box<[CircuitPinInfo]> {
        let dscription = Self::describe_props(&circ.props);

        self.collector = dscription.pins[0].to_info();
        self.base = dscription.pins[1].to_info();
        self.emitter = dscription.pins[2].to_info();
        vec![
            self.collector.clone(),
            self.base.clone(),
            self.emitter.clone(),
        ]
        .into_boxed_slice()
    }

    fn update_signals(&self, state_ctx: &CircuitStateContext, _: Option<usize>) {
        let base = self.base.get_state(state_ctx);

        if let WireState::Error = base {
            self.emitter.set_state(state_ctx, WireState::Error);
            return;
        }

        let open = Self::is_open_state(self.ty, &base);

        let output = match open {
            true => self.collector.get_state(state_ctx),
            false => WireState::None,
        };

        self.emitter.set_state(state_ctx, output);
    }

    fn size(&self, circ: &Arc<Circuit>) -> Vec2u {
        Self::describe_props(&circ.props).size
    }

    fn apply_props(&mut self, circ: &Arc<Circuit>, changed: Option<&str>) {
        if matches!(changed, None | Some("dir")) {
            self.dir = circ.props.read_clone("dir").unwrap_or(Direction4::Left);
        }
        if matches!(changed, None | Some("flip")) {
            self.flip = circ.props.read_clone("flip").unwrap_or(false);
        }
        if matches!(changed, None | Some("ty")) {
            self.ty = circ.props.read_clone("ty").unwrap_or(TransistorType::NPN);
        }
    }

    fn prop_changed(&self, prop_id: &str, resize: &mut bool, recreate_pins: &mut bool) {
        (*resize, *recreate_pins) = match prop_id {
            "dir" => (true, true),
            "flip" => (false, true),
            _ => (false, false),
        }
    }
}

#[derive(Debug)]
pub struct TransistorPreview {}

impl CircuitPreviewImpl for TransistorPreview {
    fn type_name(&self) -> DynStaticStr {
        "transistor".into()
    }

    fn display_name(&self) -> DynStaticStr {
        "Transistor".into()
    }

    fn description(&self) -> DynStaticStr {
        "A component that imitates how real-world transistors work.\n\
         Passes any signal from Collector to Emitter if Base receives correct signal level.\n\
         PNP-type transistor opens when Base is low or False,\n\
         NPN-type transistor opens when Base is high or True.\n\
         Always closed when Base is not connected or in None state,\n\
         produces Error on the Emitter when Base receives Error.\
        "
        .into()
    }

    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, _: bool) {
        let angle = props
            .read_clone("dir")
            .unwrap_or(Direction4::Left)
            .inverted_ud()
            .angle_to_left();
        let flip = props.read_clone("flip").unwrap_or(false);
        let ty = props.read_clone("ty").unwrap_or(TransistorType::NPN);
        let false_color = ctx.style.wire_colors.false_color();
        crate::graphics::transistor(
            ty,
            angle,
            flip,
            false_color,
            false_color,
            false_color,
            Transistor::is_open_state(ty, &WireState::False),
            ctx,
        );
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Transistor::new())
    }

    fn load_copy_data(
        &self,
        _imp: &serde_intermediate::Intermediate,
        _internal: &serde_intermediate::Intermediate,
        _ctx: &Arc<SimulationContext>,
        _errors: &mut ErrorList,
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        Some(Box::new(TransistorPreview {}))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        CircuitPropertyStore::new([
            CircuitProperty::new("dir", "Direction", Direction4::Left),
            CircuitProperty::new("flip", "Flip", false),
            CircuitProperty::new("ty", "Type", TransistorType::NPN),
        ])
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        Transistor::describe_props(props).to_dyn()
    }
}

impl CircuitPropertyImpl for TransistorType {
    fn equals(&self, other: &dyn CircuitPropertyImpl) -> bool {
        other.is_type_and(|o: &Self| o == self)
    }

    fn ui(&mut self, ui: &mut Ui, not_equal: bool) -> Option<Box<dyn CircuitPropertyImpl>> {
        let old = *self;
        let mut changed = false;
        ComboBox::from_id_source("trtype_ui")
            .selected_text(if not_equal {
                Default::default()
            } else {
                match *self {
                    TransistorType::NPN => "NPN",
                    TransistorType::PNP => "PNP",
                }
            })
            .show_ui(ui, |ui| {
                for ty in [TransistorType::NPN, TransistorType::PNP] {
                    let name = match ty {
                        TransistorType::NPN => "NPN",
                        TransistorType::PNP => "PNP",
                    };
                    let res = ui.selectable_value(self, ty, name);
                    if res.changed() || res.clicked() {
                        changed = true;
                    }
                }
            });
        changed.then(|| Box::new(old) as Box<dyn CircuitPropertyImpl>)
    }

    fn clone(&self) -> Box<dyn CircuitPropertyImpl> {
        Box::new(Clone::clone(self))
    }

    fn save(&self) -> serde_intermediate::Intermediate {
        serde_intermediate::to_intermediate(&self).unwrap_or_default()
    }

    fn load(&mut self, data: &serde_intermediate::Intermediate) {
        if let Ok(d) = serde_intermediate::de::intermediate::deserialize(data) {
            *self = d;
        }
    }

    fn copy_into(&self, other: &mut dyn CircuitPropertyImpl) {
        if let Some(r) = other.downcast_mut() {
            *r = *self;
        }
    }
}

impl<'de> Deserialize<'de> for TransistorType {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(match char::deserialize(deserializer)? {
            'n' => TransistorType::PNP,
            _ => TransistorType::NPN,
        })
    }
}

impl Serialize for TransistorType {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match *self {
            TransistorType::NPN => 'p'.serialize(serializer),
            TransistorType::PNP => 'n'.serialize(serializer),
        }
    }
}
