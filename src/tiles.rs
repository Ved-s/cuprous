use std::{ops::ControlFlow, sync::RwLock};

use eframe::{epaint::{Color32, Rounding}, egui::Sense};
use emath::Rect;

use crate::{
    containers::{Chunks2D, FixedVec},
    vector::{Vec2f, Vec2u, Vector, IsZero, Vec2i},
    PaintContext,
};

pub trait Circuit {
    fn id(&self) -> usize;
    fn draw(&self, ctx: &PaintContext);
    fn size(&self) -> Vec2i;
}

pub trait CircuitPreview: std::fmt::Debug {
    fn draw_preview(&self, ctx: &PaintContext);
    fn size(&self) -> Vec2u;
    fn create(&self, id: usize) -> Box<dyn Circuit>;
}

#[derive(Default)]
pub struct TileNode {
    origin_dist: Vector<2, u32>,
    circuit: usize,
}

pub struct Tiles {
    pub nodes: Chunks2D<16, TileNode>,
    pub cirtuits: FixedVec<Box<dyn Circuit>>,
}

impl Tiles {
    pub fn update(&mut self, ctx: &PaintContext, selected: Option<&Box<dyn CircuitPreview>>) {

        ctx.draw_chunks(&self.nodes, self, |n| n.circuit > 0, |node, pos, ctx, this, _| {

            if !node.origin_dist.is_zero() && pos.x() != ctx.bounds.tiles_tl.x() && pos.y() != ctx.bounds.tiles_tl.y() {
                return;
            } 

            let circuit = match this.cirtuits.get(node.circuit) {
                None => return,
                Some(c) => c,
            };

            let pos = pos - node.origin_dist.convert_values(|v| v as i32);
            let screen_pos = ctx.screen.world_to_screen_tile(pos);
            let screen_size = circuit.size().convert_values(|v| v as f32) * ctx.screen.scale;
            let rect = Rect::from_min_size(screen_pos.into(), screen_size.into());
            let circ_ctx = ctx.with_rect(rect);
            circuit.draw(&circ_ctx);
        });

        match selected {
            None => return,
            Some(p) => self.handle_preview(ctx, p),
        };
    }

    fn handle_preview(&mut self, ctx: &PaintContext<'_>, preview: &Box<dyn CircuitPreview>) {
        let mouse_tile_pos = ctx
            .egui_ctx
            .input(|input| input.pointer.interact_pos())
            .map(|p| {
                ctx.screen
                    .screen_to_world(Vec2f::from(p))
            });
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
                    if self.nodes.get(x as isize, y as isize).is_some_and(|n| n.circuit > 0) {
                        return;
                    }
                }
            }

            let cid = self.cirtuits.first_free_pos();
            let circ = preview.create(cid);
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

    pub fn new() -> Self {
        Self {
            nodes: Default::default(),
            cirtuits: vec![Box::new(DummyCircuit {}) as Box<dyn Circuit>].into(),
        }
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

pub struct DummyCircuit {}

impl Circuit for DummyCircuit {
    fn draw(&self, _: &PaintContext) {}

    fn id(&self) -> usize {
        0
    }

    fn size(&self) -> Vec2i {
        0.into()
    }
}

pub struct TestCircuit {
    id: usize
}

impl Circuit for TestCircuit {
    fn draw(&self, ctx: &PaintContext) {
        ctx.paint
            .rect_filled(ctx.rect, Rounding::none(), Color32::GREEN);
    }

    fn id(&self) -> usize {
        self.id
    }

    fn size(&self) -> Vec2i {
        [2, 2].into()
    }
}


#[derive(Debug)]
pub struct TestCircuitPreview {
}


impl CircuitPreview for TestCircuitPreview {
    fn draw_preview(&self, ctx: &PaintContext) {
        ctx.paint
            .rect_filled(ctx.rect, Rounding::none(), Color32::GREEN);
    }

    fn size(&self) -> Vec2u {
        [2, 2].into()
    }

    fn create(&self, id: usize) -> Box<dyn Circuit> {
        Box::new(TestCircuit { id })
    }
}
