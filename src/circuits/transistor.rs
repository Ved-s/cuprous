use eframe::{
    egui::{ComboBox, Ui},
    epaint::{PathShape, Stroke},
};
use emath::vec2;

use crate::{circuits::*, describe_directional_custom_circuit, vector::Vec2f, Direction4};

use super::props::{CircuitProperty, CircuitPropertyImpl};

#[allow(clippy::upper_case_acronyms)]
#[derive(Default, Clone, Copy, PartialEq, Eq)]
enum Type {
    #[default]
    NPN,
    PNP,
}

struct Transistor {
    collector: CircuitPinInfo,
    base: CircuitPinInfo,
    emitter: CircuitPinInfo,
    ty: Type,
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
            ty: Type::NPN,
            dir: Direction4::Left,
            flip: false,
        }
    }

    fn draw(
        ty: Type,
        state: Option<(&CircuitStateContext, [WireState; 3])>,
        ctx: &PaintContext,
        angle: f32,
        flip: bool,
    ) {
        let collector_color = state
            .map(|s| s.1[0].color())
            .unwrap_or_else(|| WireState::False.color());
        let base_color = state
            .map(|s| s.1[1].color())
            .unwrap_or_else(|| WireState::False.color());
        let emitter_color = state
            .map(|s| s.1[2].color())
            .unwrap_or_else(|| WireState::False.color());
        let middle_color = state
            .map(|s| match Self::is_open_state(ty, s.1[1]) {
                true => s.1[0].color(),
                false => s.1[1].color(),
            })
            .unwrap_or_else(|| WireState::False.color());

        let thickness = ActiveCircuitBoard::WIRE_THICKNESS * ctx.screen.scale;

        let collector_stroke = Stroke::new(thickness, collector_color);
        let base_stroke = Stroke::new(thickness, base_color);
        let emitter_stroke = Stroke::new(thickness, emitter_color);
        let middle_stroke = Stroke::new(thickness, middle_color);

        let size = vec2(2.0, 3.0);
        let trf = |x: f32, y: f32| {
            let y = y / size.y;
            let y = if flip { 1.0 - y } else { y };
            ctx.rect
                .lerp_inside(Vec2f::from([x / size.x, y]).rotated_xy(angle, 0.5).into())
        };

        let painter = ctx.paint;

        painter.line_segment([trf(0.5, 1.5), trf(0.9, 1.5)], base_stroke);

        painter.line_segment([trf(1.5, 0.5), trf(1.5, 0.8)], collector_stroke);
        painter.line_segment([trf(0.92, 1.33), trf(1.526, 0.73)], collector_stroke);

        painter.line_segment([trf(1.5, 2.5), trf(1.5, 2.2)], emitter_stroke);
        painter.line_segment([trf(0.92, 1.66), trf(1.526, 2.27)], emitter_stroke);

        painter.line_segment([trf(0.9, 1.0), trf(0.9, 2.0)], middle_stroke);

        let verts = match ty {
            Type::NPN => [trf(1.35, 1.70), trf(1.45, 2.19), trf(0.96, 2.09)],
            Type::PNP => [trf(1.08, 0.78), trf(1.00, 1.26), trf(1.47, 1.17)],
        };

        let arrow_color = match ty {
            Type::NPN => emitter_color,
            Type::PNP => collector_color,
        };

        painter.add(PathShape {
            points: verts.into(),
            closed: true,
            fill: arrow_color,
            stroke: Stroke::NONE,
        });
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

    fn is_open_state(ty: Type, base: WireState) -> bool {
        matches!(
            (ty, base),
            (Type::NPN, WireState::True) | (Type::PNP, WireState::False)
        )
    }
}

impl CircuitImpl for Transistor {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext) {
        let collector = self.collector.get_state(state_ctx);
        let base = self.base.get_state(state_ctx);
        let emitter = self
            .emitter
            .get_wire_state(state_ctx)
            .unwrap_or_else(|| self.emitter.get_state(state_ctx));

        let angle = self.dir.inverted_ud().angle_to_left();

        Self::draw(
            self.ty,
            Some((state_ctx, [collector, base, emitter])),
            paint_ctx,
            angle,
            self.flip,
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

        let open = Self::is_open_state(self.ty, base);

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
            self.ty = circ.props.read_clone("ty").unwrap_or(Type::NPN);
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

    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, _: bool) {
        let angle = props
            .read_clone("dir")
            .unwrap_or(Direction4::Left)
            .inverted_ud()
            .angle_to_left();
        let flip = props.read_clone("flip").unwrap_or(false);
        let ty = props.read_clone("ty").unwrap_or(Type::NPN);
        Transistor::draw(ty, None, ctx, angle, flip);
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Transistor::new())
    }

    fn load_impl_data(
        &self,
        _: &serde_intermediate::Intermediate,
        _: &Arc<SimulationContext>,
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        Some(Box::new(TransistorPreview {}))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        CircuitPropertyStore::new([
            CircuitProperty::new("dir", "Direction", Direction4::Left),
            CircuitProperty::new("flip", "Flip", false),
            CircuitProperty::new("ty", "Type", Type::NPN),
        ])
    }

    fn display_name(&self) -> DynStaticStr {
        "Transistor".into()
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        Transistor::describe_props(props).to_dyn()
    }
}

impl CircuitPropertyImpl for Type {
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
                    Type::NPN => "NPN",
                    Type::PNP => "PNP",
                }
            })
            .show_ui(ui, |ui| {
                for ty in [Type::NPN, Type::PNP] {
                    let name = match ty {
                        Type::NPN => "NPN",
                        Type::PNP => "PNP",
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

impl<'de> Deserialize<'de> for Type {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(match char::deserialize(deserializer)? {
            'n' => Type::PNP,
            _ => Type::NPN,
        })
    }
}

impl Serialize for Type {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match *self {
            Type::NPN => 'p'.serialize(serializer),
            Type::PNP => 'n'.serialize(serializer),
        }
    }
}
