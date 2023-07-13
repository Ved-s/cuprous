use std::sync::{RwLock, Arc};

use eframe::{
    egui::Sense,
    epaint::{Color32, Rounding},
};
use emath::Rect;

use crate::{
    containers::{Chunks2D, FixedVec},
    vector::{IsZero, Vec2f, Vec2u, Vector, Vec2i},
    PaintContext,
};

pub struct CircuitInfo {
    pub size: Vec2u,
    pub pins: Box<[Arc<RwLock<CircuitPin>>]> 
}

#[derive(Debug)]
pub struct CircuitPin {
    pub pos: Vec2u,
    pub wire: Option<usize>
}

impl CircuitPin {
    fn new(pos: Vec2u) -> Self {
        Self {
            pos,
            wire: None
        }
    }
}

pub struct Circuit {
    pub id: usize,
    pub info: RwLock<CircuitInfo>,
    pub imp: Box<dyn CircuitImpl>,
}

impl Circuit {
    fn create(id: usize, preview: &dyn CircuitPreview) -> Self {
        let imp = preview.create_impl();
        Self {
            id,
            info: RwLock::new(CircuitInfo {
                size: preview.size(),
                pins: imp.create_pins()
            }),
            imp
        }
    }

    fn dummy() -> Self {
        pub struct DummyCircuitImpl {}

        impl CircuitImpl for DummyCircuitImpl {
            fn draw(&self, _: &PaintContext) {}

            fn create_pins(&self) -> Box<[Arc<RwLock<CircuitPin>>]> {
                Box::new([])
            }
        }

        Self {
            id: 0,
            info: RwLock::new(CircuitInfo {
                size: 0.into(),
                pins: Box::new([])
            }),
            imp: Box::new(DummyCircuitImpl {})
        }
    }
}

pub trait CircuitImpl {
    fn draw(&self, ctx: &PaintContext);

    fn create_pins(&self) -> Box<[Arc<RwLock<CircuitPin>>]>;
}

pub trait CircuitPreview: std::fmt::Debug {
    fn draw_preview(&self, ctx: &PaintContext);
    fn size(&self) -> Vec2u;
    fn create_impl(&self) -> Box<dyn CircuitImpl>;
}

#[derive(Default)]
pub struct TileNode {
    origin_dist: Vector<2, u32>,
    circuit: usize,
}

pub struct Tiles {
    pub nodes: Chunks2D<16, TileNode>,
    pub cirtuits: FixedVec<Circuit>,
}

impl Tiles {

    pub fn new() -> Self {
        Self {
            nodes: Default::default(),
            cirtuits: vec![Circuit::dummy()].into(),
        }
    }

    pub fn update(&mut self, ctx: &PaintContext, selected: Option<&Box<dyn CircuitPreview>>) {
        ctx.draw_chunks(
            &self.nodes,
            self,
            |n| n.circuit > 0,
            |node, pos, ctx, this, _| {
                if !node.origin_dist.is_zero()
                    && pos.x() != ctx.bounds.tiles_tl.x()
                    && pos.y() != ctx.bounds.tiles_tl.y()
                {
                    return;
                }

                let circuit = match this.cirtuits.get(node.circuit) {
                    None => return,
                    Some(c) => c,
                };

                let circ_info = circuit.info.read().unwrap();

                let pos = pos - node.origin_dist.convert_values(|v| v as i32);
                let screen_pos = ctx.screen.world_to_screen_tile(pos);
                let screen_size = circ_info.size.convert_values(|v| v as f32) * ctx.screen.scale;
                let rect = Rect::from_min_size(screen_pos.into(), screen_size.into());
                let circ_ctx = ctx.with_rect(rect);
                circuit.imp.draw(&circ_ctx);
            },
        );

        match selected {
            None => return,
            Some(p) => self.handle_preview(ctx, p),
        };
    }

    fn handle_preview(&mut self, ctx: &PaintContext<'_>, preview: &Box<dyn CircuitPreview>) {
        let mouse_tile_pos = ctx
            .egui_ctx
            .input(|input| input.pointer.interact_pos())
            .map(|p| ctx.screen.screen_to_world(Vec2f::from(p)));
        let mouse_tile_pos_i = match mouse_tile_pos {
            None => return,
            Some(v) => v.convert_values(|v| v.floor() as i32),
        };
        let size = preview.size();
        if size.x() == 0 || size.y() == 0 {
            return;
        }
        let place_pos = mouse_tile_pos_i - size.convert_values(|v| v as i32) / 2;
        let rect = Rect::from_min_size(
            ctx.screen.world_to_screen_tile(place_pos).into(),
            (size.convert_values(|v| v as f32) * ctx.screen.scale).into(),
        );
        preview.draw_preview(&ctx.with_rect(rect));
        let interaction = ctx.ui.interact(ctx.rect, ctx.ui.id(), Sense::click());

        if interaction.clicked_by(eframe::egui::PointerButton::Primary) {
            for j in 0..size.y() {
                for i in 0..size.x() {
                    let x = place_pos.x() + i as i32;
                    let y = place_pos.y() + j as i32;
                    if self
                        .nodes
                        .get(x as isize, y as isize)
                        .is_some_and(|n| n.circuit > 0)
                    {
                        return;
                    }
                }
            }

            let cid = self.cirtuits.first_free_pos();
            let circ = Circuit::create(cid, preview.as_ref());
            self.cirtuits.set(circ, cid);

            for j in 0..size.y() {
                for i in 0..size.x() {
                    let x = place_pos.x() + i as i32;
                    let y = place_pos.y() + j as i32;
                    let node = self.nodes.get_or_create_mut(x as isize, y as isize);

                    node.circuit = cid;
                    node.origin_dist = [i, j].into();
                }
            }
        }
    }

    pub fn pin_at(&self, pos: Vec2i) -> Option<Arc<RwLock<CircuitPin>>> {
        let circ = self.nodes.get(pos.x() as isize, pos.y() as isize)?;
        if circ.circuit == 0 {
            return None;
        }
        let pos = circ.origin_dist;
        let circ = self.cirtuits.get(circ.circuit)?;

        circ.info.read().unwrap().pins.iter().find(|p| p.read().unwrap().pos == pos).map(|arc| arc.clone())
    }
}

impl Default for Tiles {
    fn default() -> Self {
        Self {
            nodes: Default::default(),
            cirtuits: vec![].into(),
        }
    }
}

pub struct TestCircuitImpl {
    pin: Arc<RwLock<CircuitPin>>
}

impl TestCircuitImpl {
    fn new() -> Self {
        Self {
            pin: Arc::new(RwLock::new(CircuitPin::new(0.into())))
        }
    }
}

impl CircuitImpl for TestCircuitImpl {
    fn draw(&self, ctx: &PaintContext) {
        ctx.paint
            .rect_filled(ctx.rect, Rounding::none(), Color32::GREEN);
    }

    fn create_pins(&self) -> Box<[Arc<RwLock<CircuitPin>>]> {
        vec![self.pin.clone()].into_boxed_slice()
    }
}

#[derive(Debug)]
pub struct TestCircuitPreview {
    pub a: usize,
    pub b: usize
}

impl CircuitPreview for TestCircuitPreview {
    fn draw_preview(&self, ctx: &PaintContext) {
        ctx.paint
            .rect_filled(ctx.rect, Rounding::none(), Color32::GREEN);
    }

    fn size(&self) -> Vec2u {
        [2, 2].into()
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(TestCircuitImpl::new())
    }
}
