use std::sync::Arc;

use eframe::egui::{self, Color32, Rounding, Stroke};
use parking_lot::{Mutex, RwLock};

use crate::{
    selection::SelectionRenderer,
    vector::{Vec2isize, Vec2usize},
    PaintContext, Screen,
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

pub struct CircuitRenderingContext<'a> {
    pub screen: Screen,
    pub painter: &'a egui::Painter,
    pub ui: &'a egui::Ui,
    pub rect: egui::Rect,
    pub selection_renderer: Option<Arc<Mutex<SelectionRenderer>>>,
    pub custom_selection: &'a mut bool,
}

impl<'a> CircuitRenderingContext<'a> {
    pub fn new(
        ctx: &'a PaintContext,
        world_pos: Vec2isize,
        size: Vec2usize,
        selection_renderer: Option<Arc<Mutex<SelectionRenderer>>>,
        custom_selection: &'a mut bool,
    ) -> Self {
        let screen_pos = ctx.screen.world_to_screen_tile(world_pos);
        let screen_size = size.convert(|v| v as f32) * ctx.screen.scale;
        let rect = egui::Rect::from_min_size(screen_pos.into(), screen_size.into());

        Self {
            screen: ctx.screen,
            painter: ctx.painter,
            ui: ctx.ui,
            rect,

            selection_renderer,
            custom_selection,
        }
    }
}

pub trait CircuitImpl: Clone + Send + Sync {
    fn size(&self) -> Vec2usize;
    fn occupies_quarter(&self, qpos: Vec2usize) -> bool;
    fn draw(&self, ctx: &CircuitRenderingContext);
}

traitbox::traitbox! {
    pub box CircuitImplBox;

    #[as_impl]
    trait CircuitImpl {
        fn size(&self) -> crate::vector::Vec2usize;
        fn occupies_quarter(&self, qpos: crate::vector::Vec2usize) -> bool;
        fn draw(&self, ctx: &crate::circuits::CircuitRenderingContext);
    }

    trait Clone {
        fn clone(&self) -> Self;
    }

    auto trait Send {}
    auto trait Sync {}
}

#[derive(Clone)]
pub struct TestCircuit;

impl CircuitImpl for TestCircuit {
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
}
