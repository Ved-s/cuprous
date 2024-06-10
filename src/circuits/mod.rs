use std::sync::Arc;

use eframe::egui::{self, Color32, Rounding, Stroke};
use parking_lot::{Mutex, RwLock};

use crate::{
    selection::SelectionRenderer, str::ArcStaticStr, vector::{Vec2isize, Vec2usize}, Direction8, PaintContext, Screen
};

pub struct Circuit {
    pub id: usize,
    pub info: RwLock<CircuitInfo>,
    pub imp: RwLock<CircuitImplBox>,
}

pub struct CircuitInfo {
    pub pos: Vec2isize,
    pub size: Vec2usize,
}

#[derive(Clone, Copy)]
pub enum PinType {
    Custom,
    Inside,
    Outside,
}

#[derive(Clone)]
pub struct PinDescription {
    pub pos: Vec2usize,
    pub id: ArcStaticStr,
    pub display_name: ArcStaticStr,
    pub dir: Option<Direction8>,
    pub ty: PinType,
}

pub struct CircuitSelectionRenderingContext<'a> {
    pub renderer: Arc<Mutex<SelectionRenderer>>,
    pub custom_selection: &'a mut bool,
}

pub struct CircuitRenderingContext<'a> {
    pub screen: Screen,
    pub painter: &'a egui::Painter,
    pub ui: &'a egui::Ui,
    pub rect: egui::Rect,
    pub selection: Option<CircuitSelectionRenderingContext<'a>>,
}

impl<'a> CircuitRenderingContext<'a> {
    pub fn new(
        ctx: &'a PaintContext,
        world_pos: Vec2isize,
        size: Vec2usize,
        selection: Option<CircuitSelectionRenderingContext<'a>>,
    ) -> Self {
        let screen_pos = ctx.screen.world_to_screen_tile(world_pos);
        let screen_size = size.convert(|v| v as f32) * ctx.screen.scale;
        let rect = egui::Rect::from_min_size(screen_pos.into(), screen_size.into());

        Self {
            screen: ctx.screen,
            painter: ctx.painter,
            ui: ctx.ui,
            rect,

            selection,
        }
    }
}

pub trait CircuitImpl: Clone + Send + Sync {
    fn id(&self) -> ArcStaticStr;
    fn display_name(&self) -> ArcStaticStr;
    fn size(&self) -> Vec2usize;
    fn occupies_quarter(&self, qpos: Vec2usize) -> bool;
    fn draw(&self, ctx: &CircuitRenderingContext);
    fn describe_pins(&self) -> Box<[PinDescription]>;
}
traitbox::traitbox! {
    pub box CircuitImplBox;

    #[as_impl]
    trait CircuitImpl {
        fn id(&self) -> crate::str::ArcStaticStr;
        fn display_name(&self) -> crate::str::ArcStaticStr;
        fn size(&self) -> crate::vector::Vec2usize;
        fn occupies_quarter(&self, qpos: crate::vector::Vec2usize) -> bool;
        fn draw(&self, ctx: &crate::circuits::CircuitRenderingContext);
        fn describe_pins(&self) -> Box<[crate::circuits::PinDescription]>;
    }

    trait Clone {
        fn clone(&self) -> Self;
    }

    auto trait Send {}
    auto trait Sync {}
}

pub struct CircuitBlueprint {
    pub id: ArcStaticStr,
    pub display_name: ArcStaticStr,
    pub imp: CircuitImplBox,
    pub size: Vec2usize,
    pub pins: Box<[PinDescription]>,
}

impl CircuitBlueprint {
    pub fn new(imp: CircuitImplBox) -> Self {
        Self { 
            id: imp.id(),
            display_name: imp.display_name(),
            size: imp.size(), 
            pins: imp.describe_pins(),
            imp,
        }
    }
}

impl<T: CircuitImpl + 'static> From<T> for CircuitBlueprint {
    fn from(value: T) -> Self {
        Self::new(CircuitImplBox::new(value))
    }
}

#[derive(Clone)]
pub struct TestCircuit;

impl CircuitImpl for TestCircuit {
    fn id(&self) -> ArcStaticStr {
        "test".into()
    }
    
    fn display_name(&self) -> ArcStaticStr {
        "Test circuit".into()
    }

    fn size(&self) -> Vec2usize {
        [3, 3].into()
    }

    fn occupies_quarter(&self, qpos: Vec2usize) -> bool {
        !(qpos.x == 0 || qpos.y == 0 || qpos.x == 5 || qpos.y == 5)
    }

    fn draw(&self, ctx: &CircuitRenderingContext) {
        let rect = ctx.rect.shrink(ctx.screen.scale * 0.5);
        ctx.painter.rect(
            rect,
            Rounding::ZERO,
            Color32::WHITE,
            Stroke::new(0.1 * ctx.screen.scale, Color32::BLACK),
        );
    }

    fn describe_pins(&self) -> Box<[PinDescription]> {
        let mut pins = Vec::with_capacity(8);
        for dir in Direction8::ALL {
            let name_lower = match dir {
                Direction8::Up => "u",
                Direction8::UpRight => "ur",
                Direction8::Right => "r",
                Direction8::DownRight => "dr",
                Direction8::Down => "d",
                Direction8::DownLeft => "dl",
                Direction8::Left => "l",
                Direction8::UpLeft => "ul",
            };

            let name_upper = match dir {
                Direction8::Up => "U Pin",
                Direction8::UpRight => "UR Pin",
                Direction8::Right => "R Pin",
                Direction8::DownRight => "DR Pin",
                Direction8::Down => "D Pin",
                Direction8::DownLeft => "DL Pin",
                Direction8::Left => "L Pin",
                Direction8::UpLeft => "UL Pin",
            };

            let desc = PinDescription {
                pos: (dir.into_dir_isize() + 1).convert(|v| v as usize),
                id: name_lower.into(),
                display_name: name_upper.into(),
                dir: Some(dir),
                ty: PinType::Custom,
            };
            pins.push(desc);
        }
        pins.into()
    }
}
