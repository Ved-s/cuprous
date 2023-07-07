use std::{num::NonZeroU32, sync::RwLock, f32::consts::TAU, collections::{HashSet, hash_map::DefaultHasher}, hash::Hasher};

use eframe::{egui::{self, Sense}, epaint::{Color32, Rounding}};
use emath::{vec2, Rect};

use crate::{
    containers::{Chunks2D, FixedVec},
    vector::{Vec2i, Vec2f},
    PaintContext, TileDrawBounds, SizeCalc,
};

#[derive(Debug, Default)]
pub struct Wire {
    id: usize,
    color: Color32,
    nodes: Vec<Vec2i>,
}

impl Wire {
    fn dummy() -> Self {
        Self {
            id: 0,
            color: Color32::from_rgb(255, 0, 255),
            nodes: vec![],
        }
    }
}

pub struct WirePart {
    pub pos: Vec2i,
    pub length: NonZeroU32,
    pub vertical: bool,
}

#[derive(Default, Clone, Copy)]
pub struct WireNode {
    up: u32,
    left: u32,
    wire: usize,
}

impl SizeCalc for WireNode {
    fn calc_size_inner(&self) -> usize {
        0
    }
}

#[derive(Default)]
pub struct Wires {
    pub wire_drag_pos: Option<Vec2i>,
    pub wire_nodes: Chunks2D<16, WireNode>,
    pub wires: FixedVec<Wire>,

    pub wire_parts_drawn: RwLock<u32>,
}

impl Wires {
    pub fn new() -> Self {
        Self {
            wires: vec![Wire::dummy()].into(),
            ..Default::default()
        }
    }

    pub fn update(&mut self, ctx: &PaintContext) {
        *self.wire_parts_drawn.write().unwrap() = 0;

        ctx.draw_chunks(
            &self.wire_nodes,
            ctx.rect,
            ctx.paint,
            ctx.bounds,
            &(&self, ctx),
            |node| node.wire > 0 || node.up > 0 || node.left > 0,
            |node, pos, _, ctx, paint, bounds, lookaround| {
                let this = ctx.0;
                let ctx = ctx.1;

                fn draw_wire(
                    dist: u32,
                    next_dist: u32,
                    wire: usize,
                    vertical: bool,
                    this: &Wires,
                    pos: Vec2i,
                    ctx: &PaintContext,
                    bounds: &TileDrawBounds,
                    color: Color32,
                ) {
                    if dist == 0 && wire == 0 {
                        return;
                    }

                    let edge = match vertical {
                        true => pos.y() == bounds.tiles_br.y(),
                        false => pos.x() == bounds.tiles_br.x(),
                    };

                    if (wire == 0 || dist == 0) && !edge {
                        return;
                    }

                    let length = match next_dist {
                        0 => {
                            if dist == 0 {
                                return;
                            } else {
                                dist
                            }
                        }
                        node_dist => {
                            if node_dist == dist + 1 {
                                node_dist
                            } else {
                                dist
                            }
                        }
                    };

                    let part = WirePart {
                        length: NonZeroU32::new(length).unwrap(),
                        pos: pos
                            - if vertical {
                                [0, dist as i32]
                            } else {
                                [dist as i32, 0]
                            },
                        vertical,
                    };

                    this.draw_wire_part(ctx, &part, color)
                }

                if node.wire == 0 && node.up == 0 && node.left == 0 {
                    return;
                }

                let wires = this.wire_at_node(pos, node);

                let wire_color_v = wires
                    .up()
                    .map_or(Color32::from_rgb(255, 100, 255), |w| w.color);

                let wire_color_h = wires
                    .left()
                    .map_or(Color32::from_rgb(255, 100, 255), |w| w.color);

                let next_node_v = lookaround.get_relative(0, 1);
                let next_node_h = lookaround.get_relative(1, 0);

                draw_wire(
                    node.left,
                    next_node_h.map_or(0, |n| n.left),
                    node.wire,
                    false,
                    this,
                    pos,
                    ctx,
                    bounds,
                    wire_color_h,
                );
                draw_wire(
                    node.up,
                    next_node_v.map_or(0, |n| n.up),
                    node.wire,
                    true,
                    this,
                    pos,
                    ctx,
                    bounds,
                    wire_color_v,
                );

                if node.wire > 0 {
                    //let count = [
                    //    node.left > 0,
                    //    node.up > 0,
                    //    next_node_h.is_some_and(|n| n.left == 1),
                    //    next_node_v.is_some_and(|n| n.up == 1),
                    //]
                    //.iter()
                    //.filter(|v| **v)
                    //.count();
                    //if count > 2 {
                    //    this.draw_wire_intersection(paint, pos, wire.color)
                    //}

                    if node.left > 0
                        && node.up > 0
                        && next_node_h.is_some_and(|n| n.left == 1)
                        && next_node_v.is_some_and(|n| n.up == 1)
                    {
                        if let Some(wire) = self.wires.get(node.wire) {
                            Wires::draw_wire_intersection(ctx, pos, wire.color)
                        }
                    }
                }

                let correct_up = node.up == 0
                    || lookaround
                        .get_relative(0, -(node.up as isize))
                        .is_some_and(|n| n.wire > 0)
                        && (1..node.up as isize)
                            .all(|p| lookaround.get_relative(0, -p).is_some_and(|n| n.wire == 0));

                let correct_left = node.left == 0
                    || lookaround
                        .get_relative(-(node.left as isize), 0)
                        .is_some_and(|n| n.wire > 0)
                        && (1..node.left as isize)
                            .all(|p| lookaround.get_relative(-p, 0).is_some_and(|n| n.wire == 0));

                if !correct_up || !correct_left {
                    let pos = ctx.screen.world_to_screen_tile(pos);

                    let rect = Rect::from_min_size(pos.into(), vec2(ctx.screen.scale, ctx.screen.scale));
                    paint.rect_filled(
                        rect,
                        Rounding::none(),
                        Color32::from_rgba_unmultiplied(255, 0, 0, 100),
                    );
                }
            },
        );

        let mouse_tile_pos = ctx.egui_ctx
            .input(|input| input.pointer.interact_pos())
            .map(|p| ctx.screen.screen_to_world(Vec2f::from(p) - ctx.rect.left_top()));

        let mouse_tile_pos_i = mouse_tile_pos.map(|p| p.convert_values(|v| v.floor() as i32));

        let drawing_wire = Self::calc_wire_part(self.wire_drag_pos, mouse_tile_pos_i);
        if let Some(ref part) = drawing_wire {
            self.draw_wire_part(ctx, part, Color32::GRAY);
        }

        let interaction = ctx.ui.interact(ctx.rect, ctx.ui.id(), Sense::drag());

        if self.wire_drag_pos.is_none() && interaction.drag_started_by(egui::PointerButton::Primary)
        {
            self.wire_drag_pos = mouse_tile_pos_i;
        } else if self.wire_drag_pos.is_some()
            && interaction.drag_released_by(egui::PointerButton::Primary)
        {
            self.wire_drag_pos = None;

            // TODO: travel wires and equalize wire IDs
            if let Some(part) = drawing_wire {
                self.place_wire_part(part);
            }
        }
    }

    fn draw_wire_part(&self, ctx: &PaintContext, part: &WirePart, color: Color32) {
        let screen = &ctx.screen;
        let thickness = ctx.screen.scale * 0.25;

        let pos = screen.world_to_screen_tile(part.pos) + ((screen.scale - thickness) * 0.5);
        let length = screen.scale * part.length.get() as f32 + thickness;

        let rect_size = match part.vertical {
            true => vec2(thickness, length),
            false => vec2(length, thickness),
        };
        let rect = Rect::from_min_size(pos.into(), rect_size);
        ctx.paint.rect_filled(rect, Rounding::none(), color);

        *self.wire_parts_drawn.write().unwrap() += 1;
    }

    fn draw_wire_intersection(ctx: &PaintContext, pos: Vec2i, color: Color32) {
        let screen = &ctx.screen;
        let thickness = screen.scale * 0.4;

        let pos = screen.world_to_screen_tile(pos) + ((screen.scale - thickness) * 0.5);

        let rect = Rect::from_min_size(pos.into(), vec2(thickness, thickness));
        ctx.paint.rect_filled(rect, Rounding::none(), color);
    }

    fn calc_wire_part(from: Option<Vec2i>, to: Option<Vec2i>) -> Option<WirePart> {
        if let Some(from) = from {
            if let Some(to) = to {
                if from != to {
                    let angle = (to - from).convert_values(|v| v as f32).angle_to_x();
                    let axis = (angle / (TAU / 4.0)).round() as i32 % 4;

                    let origin = match axis {
                        1 => [from.x(), to.y()].into(),
                        2 => [to.x(), from.y()].into(),
                        _ => from,
                    };
                    let length = match axis {
                        0 => to.x() - from.x(),
                        1 => from.y() - to.y(),
                        2 => from.x() - to.x(),
                        3 => to.y() - from.y(),
                        _ => unreachable!(),
                    } as u32;

                    let vertical = match axis {
                        1 | 3 => true,
                        _ => false,
                    };

                    let part = WirePart {
                        pos: origin,
                        length: NonZeroU32::new(length).unwrap(),
                        vertical,
                    };

                    Some(part)
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    fn place_wire_part(&mut self, part: WirePart) {
        fn split_wire_at(this: &mut Wires, at: Vec2i, wire: usize) {
            let node = this.wire_nodes.get(at.x() as isize, at.y() as isize);
            let node = match node {
                Some(v) => *v,
                None => return,
            };

            fn split_wire_dir(
                wires: &mut Chunks2D<16, WireNode>,
                at: Vec2i,
                vertical: bool,
                dist: u32,
            ) {
                for split_dist in 1.. {
                    let node =
                        match vertical {
                            true => wires
                                .get_mut(at.x() as isize, at.y() as isize + split_dist as isize),
                            false => wires
                                .get_mut(at.x() as isize + split_dist as isize, at.y() as isize),
                        };
                    let node = match node {
                        Some(v) => v,
                        None => break,
                    };
                    let node_dir = match vertical {
                        true => &mut node.up,
                        false => &mut node.left,
                    };
                    if *node_dir != split_dist + dist {
                        // went off the rails!
                        break;
                    }
                    *node_dir = split_dist;

                    if node.wire > 0 {
                        break;
                    }
                }
            }

            if node.wire > 0 {
                return;
            }

            if node.up > 0 {
                split_wire_dir(&mut this.wire_nodes, at, true, node.up);
            }
            if node.left > 0 {
                split_wire_dir(&mut this.wire_nodes, at, false, node.left);
            }
            let node = this
                .wire_nodes
                .get_or_create_mut(at.x() as isize, at.y() as isize);

            node.wire = wire;
            let wire = this.wires.get_mut(wire).unwrap();
            wire.nodes.push(at);
        }

        let wires_crossed = {
            let mut wires_crossed = HashSet::new();
            for i in 0..=part.length.get() {
                let pos = part.pos
                    + match part.vertical {
                        true => [0, i as i32],
                        false => [i as i32, 0],
                    };

                let node = match self.wire_nodes.get(pos.x() as isize, pos.y() as isize) {
                    None => continue,
                    Some(v) => v,
                };

                if i == 0 || i == part.length.get() {
                    match self.wires_at(pos) {
                        TileWires::None => (),
                        TileWires::One { wire, dir: _ } => {
                            wires_crossed.insert(wire.id);
                        }
                        TileWires::Two { left, up } => {
                            wires_crossed.insert(left.id);
                            wires_crossed.insert(up.id);
                        }
                    }
                } else if node.wire > 0 {
                    wires_crossed.insert(node.wire);
                }
            }
            wires_crossed
        };

        let new_wire = match wires_crossed.len() {
            0 => self.create_wire().id,
            1 => *wires_crossed.iter().next().unwrap(),
            _ => {
                let main_wire = self
                    .wires
                    .iter()
                    .filter(|v| wires_crossed.contains(&v.id))
                    .max_by(|x, y| x.nodes.len().cmp(&y.nodes.len()))
                    .expect("Some matching wires")
                    .id;

                for wire in wires_crossed {
                    if wire != main_wire {
                        self.merge_wires(main_wire, wire)
                    }
                }
                main_wire
            }
        };

        split_wire_at(self, part.pos, new_wire);
        split_wire_at(
            self,
            part.pos
                + match part.vertical {
                    true => [0, part.length.get() as i32],
                    false => [part.length.get() as i32, 0],
                },
            new_wire,
        );

        let mut dist = 0;
        for i in 0..=part.length.get() {
            let pos = part.pos
                + match part.vertical {
                    true => [0, i as i32],
                    false => [i as i32, 0],
                };

            let node = self
                .wire_nodes
                .get_or_create_mut(pos.x() as isize, pos.y() as isize);

            if i > 0 {
                match part.vertical {
                    true => node.up = dist,
                    false => node.left = dist,
                }
            }
            if node.wire > 0 {
                dist = 1
            } else {
                dist += 1;
            }
            if (i == 0 || i == part.length.get()) && node.wire != new_wire {
                node.wire = new_wire;
                self.wires.get_mut(new_wire).unwrap().nodes.push(pos);
            }
        }
    }

    fn wires_at(&self, pos: Vec2i) -> TileWires<'_> {
        match self.wire_nodes.get(pos.x() as isize, pos.y() as isize) {
            None => TileWires::None,
            Some(node) => self.wire_at_node(pos, node),
        }
    }

    fn wire_at_node(&self, pos: Vec2i, node: &WireNode) -> TileWires {
        if node.wire > 0 {
            return match self.wires.get(node.wire) {
                None => TileWires::None,
                Some(wire) => TileWires::One {
                    wire,
                    dir: WireDirection::None,
                },
            };
        }

        let up = match node.up {
            0 => None,
            up => self
                .wire_nodes
                .get(pos.x() as isize, (pos.y() - up as i32) as isize)
                .and_then(|n| match n.wire {
                    0 => None,
                    wire => self.wires.get(wire),
                }),
        };
        let left = match node.left {
            0 => None,
            left => self
                .wire_nodes
                .get((pos.x() - left as i32) as isize, pos.y() as isize)
                .and_then(|n| match n.wire {
                    0 => None,
                    wire => self.wires.get(wire),
                }),
        };

        match (left, up) {
            (None, None) => TileWires::None,
            (None, Some(u)) => TileWires::One {
                wire: u,
                dir: WireDirection::Up,
            },
            (Some(l), None) => TileWires::One {
                wire: l,
                dir: WireDirection::Left,
            },
            (Some(left), Some(up)) => TileWires::Two { left, up },
        }
    }

    fn create_wire(&mut self) -> &mut Wire {
        let id = self.wires.first_free_pos();

        let mut hash = DefaultHasher::new();
        hash.write_usize(id);
        hash.write_u16(61942);
        let color = hash.finish().to_le_bytes();

        let wire = Wire {
            id,
            color: Color32::from_rgb(color[0], color[1], color[2]),
            ..Default::default()
        };

        self.wires.set(wire, id).value_ref
    }

    fn merge_wires(&mut self, wire: usize, with: usize) {
        if wire == 0
            || !self.wires.exists(wire)
            || with == 0
            || !self.wires.exists(with)
            || wire == with
        {
            return;
        }

        let Wire {
            id: _,
            color: _,
            nodes,
        } = self.wires.remove(with).unwrap();

        for npos in nodes.iter() {
            match self
                .wire_nodes
                .get_mut(npos.x() as isize, npos.y() as isize)
            {
                None => {}
                Some(n) => n.wire = wire,
            }
        }

        let wire = &mut self.wires.get_mut(wire).unwrap();
        wire.nodes.extend(nodes);
    }
}


enum WireDirection {
    None,
    Up,
    Left,
}

enum TileWires<'a> {
    None,
    One { wire: &'a Wire, dir: WireDirection },
    Two { left: &'a Wire, up: &'a Wire },
}

impl TileWires<'_> {
    fn up(&self) -> Option<&Wire> {
        match self {
            TileWires::One { wire, dir }
                if matches!(dir, WireDirection::Up) || matches!(dir, WireDirection::None) =>
            {
                Some(wire)
            }
            TileWires::Two { left: _, up } => Some(up),
            _ => None,
        }
    }

    fn left(&self) -> Option<&Wire> {
        match self {
            TileWires::One { wire, dir }
                if matches!(dir, WireDirection::Left) || matches!(dir, WireDirection::None) =>
            {
                Some(wire)
            }
            TileWires::Two { left, up: _ } => Some(left),
            _ => None,
        }
    }
}