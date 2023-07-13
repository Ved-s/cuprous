use std::sync::{Arc, RwLock};

use eframe::{
    egui::Sense,
    epaint::{Color32, Rounding},
};
use emath::{Align2, Rect};

use crate::{
    containers::{Chunks2D, FixedVec},
    vector::{IsZero, Vec2f, Vec2i, Vec2u, Vector},
    OptionalInt, PaintContext, wires::Wires,
};

pub struct CircuitInfo {
    pub size: Vec2u,
    pub pins: Box<[(Vec2u, Arc<RwLock<CircuitPin>>)]>,
}

#[derive(Debug)]
pub struct CircuitPin {
    pub wire: Option<usize>,
}

impl CircuitPin {
    fn new() -> Self {
        Self { wire: None }
    }
}

pub struct Circuit {
    pub id: usize,
    pub pos: Vec2i,
    pub info: RwLock<CircuitInfo>,
    pub imp: Box<dyn CircuitImpl>,
}

impl Circuit {
    fn create(id: usize, pos: Vec2i, preview: &dyn CircuitPreview) -> Self {
        let imp = preview.create_impl();
        Self {
            id,
            pos,
            info: RwLock::new(CircuitInfo {
                size: preview.size(),
                pins: imp.create_pins(),
            }),
            imp,
        }
    }
}

pub trait CircuitImpl {
    fn draw(&self, circuit: &Circuit, ctx: &PaintContext);

    fn create_pins(&self) -> Box<[(Vec2u, Arc<RwLock<CircuitPin>>)]>;
}

pub trait CircuitPreview: std::fmt::Debug {
    fn draw_preview(&self, ctx: &PaintContext);
    fn size(&self) -> Vec2u;
    fn create_impl(&self) -> Box<dyn CircuitImpl>;
}

#[derive(Default)]
pub struct CircuitNode {
    origin_dist: Vector<2, u32>,
    circuit: OptionalInt<usize>,
}

pub struct Circuits {
    pub nodes: Chunks2D<16, CircuitNode>,
    pub cirtuits: FixedVec<Circuit>,
}

impl Circuits {
    pub fn new() -> Self {
        Self {
            nodes: Default::default(),
            cirtuits: vec![].into(),
        }
    }

    pub fn update(&mut self, wires: &mut Wires, ctx: &PaintContext, selected: Option<&Box<dyn CircuitPreview>>) {
        ctx.draw_chunks(
            &self.nodes,
            self,
            |n| n.circuit.is_some(),
            |node, pos, ctx, this, _| {
                if !node.origin_dist.is_zero()
                    && pos.x() != ctx.bounds.tiles_tl.x()
                    && pos.y() != ctx.bounds.tiles_tl.y()
                {
                    return;
                }
                let circ_id = match node.circuit.get() {
                    None => return,
                    Some(c) => c,
                };

                let circuit = match this.cirtuits.get(circ_id) {
                    None => return,
                    Some(c) => c,
                };

                let circ_info = circuit.info.read().unwrap();

                let pos = pos - node.origin_dist.convert_values(|v| v as i32);
                let screen_pos = ctx.screen.world_to_screen_tile(pos);
                let screen_size = circ_info.size.convert_values(|v| v as f32) * ctx.screen.scale;
                let rect = Rect::from_min_size(screen_pos.into(), screen_size.into());
                let circ_ctx = ctx.with_rect(rect);
                circuit.imp.draw(&circuit, &circ_ctx);
            },
        );

        match selected {
            None => return,
            Some(p) => self.handle_preview(wires, ctx, p),
        };
    }

    fn handle_preview(&mut self, wires: &mut Wires, ctx: &PaintContext<'_>, preview: &Box<dyn CircuitPreview>) {
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
                        .is_some_and(|n| n.circuit.is_some())
                    {
                        return;
                    }
                }
            }

            let cid = self.cirtuits.first_free_pos();
            let circ = Circuit::create(cid, place_pos, preview.as_ref());
            let circ = self.cirtuits.set(circ, cid).value_ref;

            for j in 0..size.y() {
                for i in 0..size.x() {
                    let x = place_pos.x() + i as i32;
                    let y = place_pos.y() + j as i32;
                    let node = self.nodes.get_or_create_mut(x as isize, y as isize);

                    node.circuit.set(Some(cid));
                    node.origin_dist = [i, j].into();
                }
            }

            for pin in circ.info.read().unwrap().pins.iter() {
                if let Some(wire) = wires.create_intersection(place_pos + pin.0.convert_values(|v| v as i32), None) {
                    pin.1.write().unwrap().wire = Some(wire.id)
                }
            }
        }
    }

    pub fn pin_at(&self, pos: Vec2i) -> Option<Arc<RwLock<CircuitPin>>> {
        let circ = self.nodes.get(pos.x() as isize, pos.y() as isize)?;
        let pos = circ.origin_dist;
        let circ = circ.circuit.get()?;
        let circ = self.cirtuits.get(circ)?;

        circ.info
            .read()
            .unwrap()
            .pins
            .iter()
            .find(|p| p.0 == pos)
            .map(|p| p.1.clone())
    }
}

impl Default for Circuits {
    fn default() -> Self {
        Self {
            nodes: Default::default(),
            cirtuits: vec![].into(),
        }
    }
}

pub struct TestCircuitImpl {
    pin: Arc<RwLock<CircuitPin>>,
}

impl TestCircuitImpl {
    fn new() -> Self {
        Self {
            pin: Arc::new(RwLock::new(CircuitPin::new())),
        }
    }
}

impl CircuitImpl for TestCircuitImpl {
    fn draw(&self, circ: &Circuit, ctx: &PaintContext) {
        let font_id = eframe::egui::TextStyle::Monospace.resolve(ctx.ui.style());
        ctx.paint.rect_filled(
            ctx.rect,
            Rounding::none(),
            Color32::from_rgba_unmultiplied(0, 255, 0, 100),
        );

        let wire = self
            .pin
            .read()
            .unwrap()
            .wire
            .map(|v| v as i32)
            .unwrap_or(-1);

        let text = format!("{}, {}", circ.id, wire);

        ctx.paint.text(
            ctx.rect.center(),
            Align2::CENTER_CENTER,
            text,
            font_id,
            Color32::WHITE,
        );
    }

    fn create_pins(&self) -> Box<[(Vec2u, Arc<RwLock<CircuitPin>>)]> {
        vec![([0, 0].into(), self.pin.clone())].into_boxed_slice()
    }
}

#[derive(Debug)]
pub struct TestCircuitPreview {
    pub a: usize,
    pub b: usize,
}

impl CircuitPreview for TestCircuitPreview {
    fn draw_preview(&self, ctx: &PaintContext) {
        ctx.paint.rect_filled(
            ctx.rect,
            Rounding::none(),
            Color32::from_rgba_unmultiplied(0, 255, 0, 100),
        );
    }

    fn size(&self) -> Vec2u {
        [2, 2].into()
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(TestCircuitImpl::new())
    }
}
