use std::{
    collections::{hash_map::DefaultHasher, HashSet},
    f32::consts::TAU,
    hash::Hasher,
    num::NonZeroU32,
    ops::ControlFlow,
    sync::RwLock, array,
};

use eframe::{
    egui::{self, Key, Sense},
    epaint::{Color32, Rounding},
};
use emath::{vec2, Rect};

use crate::{
    containers::{Chunks2D, ChunksLookaround, FixedVec},
    vector::{Vec2f, Vec2i, Vector},
    PaintContext, SizeCalc, TileDrawBounds,
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

impl SizeCalc for Wire {
    fn calc_size_inner(&self) -> usize {
        self.nodes.calc_size_inner()
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
    pub nodes: Chunks2D<16, WireNode>,
    pub wires: FixedVec<Wire>,

    pub parts_drawn: RwLock<u32>,
}

impl Wires {
    pub fn new() -> Self {
        Self {
            wires: vec![Wire::dummy()].into(),
            ..Default::default()
        }
    }

    pub fn update(&mut self, ctx: &PaintContext, selected: bool) {
        *self.parts_drawn.write().unwrap() = 0;

        ctx.draw_chunks(
            &self.nodes,
            &self,
            |node| node.wire > 0 || node.up > 0 || node.left > 0,
            |node, pos, ctx, this, lookaround| {
                this.draw_wires(ctx, node, pos, lookaround);
            },
        );

        if !selected {
            self.wire_drag_pos = None;
            return;
        }

        let mouse_tile_pos = ctx
            .egui_ctx
            .input(|input| input.pointer.interact_pos())
            .map(|p| {
                ctx.screen
                    .screen_to_world(Vec2f::from(p))
            });

        let mouse_tile_pos_i = mouse_tile_pos.map(|p| p.convert_values(|v| v.floor() as i32));

        let drawing_wire = Self::calc_wire_part(self.wire_drag_pos, mouse_tile_pos_i);
        if let Some(ref part) = drawing_wire {
            self.draw_wire_part(ctx, part, Color32::GRAY);
        }

        let interaction = ctx
            .ui
            .interact(ctx.rect, ctx.ui.id(), Sense::click_and_drag());

        if self.wire_drag_pos.is_none() && interaction.drag_started_by(egui::PointerButton::Primary)
        {
            self.wire_drag_pos = mouse_tile_pos_i;
        } else if self.wire_drag_pos.is_some()
            && interaction.drag_released_by(egui::PointerButton::Primary)
        {
            self.wire_drag_pos = None;

            if let Some(part) = drawing_wire {
                self.place_wire_part(part);
            }
        }

        if let Some(mouse_pos) = mouse_tile_pos_i {
            if interaction.clicked_by(egui::PointerButton::Primary) && self.wire_drag_pos.is_none()
            {
                self.try_toggle_node_intersection(mouse_pos);
            }

            for i in 0..4 {
                let vertical = i & 1 == 1;
                let forward = i & 2 == 2;
                if let Some(found) = self.find_node(mouse_pos, vertical, forward) {
                    let world_pos = ctx.screen.world_to_screen_tile(found.pos);
                    let size = vec2(ctx.screen.scale, ctx.screen.scale);
                    let rect = Rect::from_min_size(world_pos.into(), size);

                    let r = if vertical { 255 } else { 0 };
                    let g = if forward { 255 } else { 0 };
                    let color = Color32::from_rgba_unmultiplied(r, g, 0, 128);
                    ctx.paint.rect_filled(rect, Rounding::none(), color);
                }
            }
        }
    }

    fn try_toggle_node_intersection(&mut self, pos: Vec2i) {
        let node = self.nodes.get(pos.x() as isize, pos.y() as isize);

        let node = match node {
            None => return,
            Some(ref n) if n.wire == 0 && n.left == 0 && n.up == 0 => return,
            Some(v) => v,
        };

        let center = node.wire > 0;
        let left = node.left > 0;
        let up = node.up > 0;
        let right = (center || left)
            && self
                .nodes
                .get(pos.x() as isize + 1, pos.y() as isize)
                .is_some_and(|n| {
                    let pointer = if center { 1 } else { node.left + 1 };
                    n.left == pointer
                });
        let down = (center || up)
            && self
                .nodes
                .get(pos.x() as isize, pos.y() as isize + 1)
                .is_some_and(|n| {
                    let pointer = if center { 1 } else { node.up + 1 };
                    n.up == pointer
                });

        if up != down || left != right {
            return;
        }

        if center {
            self.remove_intersection_at_node(pos, *node, true)
        } else {
            self.create_intersection_at_node(pos, *node, None)
        }
    }

    fn draw_wires(
        &self,
        ctx: &PaintContext<'_>,
        node: &WireNode,
        pos: Vector<2, i32>,
        lookaround: &ChunksLookaround<'_, 16, WireNode>
    ) {
        fn draw_wire(
            dist: u32,
            next_dist: u32,
            wire: usize,
            vertical: bool,
            this: &Wires,
            pos: Vec2i,
            ctx: &PaintContext,
            color: Color32,
        ) {
            if dist == 0 && wire == 0 {
                return;
            }

            let edge = match vertical {
                true =>  pos.y() == ctx.bounds.tiles_br.y(),
                false => pos.x() == ctx.bounds.tiles_br.x(),
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
        let wires = self.wire_at_node(pos, node);
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
            self,
            pos,
            ctx,
            wire_color_h,
        );
        draw_wire(
            node.up,
            next_node_v.map_or(0, |n| n.up),
            node.wire,
            true,
            self,
            pos,
            ctx,
            wire_color_v,
        );
        if node.wire > 0 {
            let possible_intersection = if ctx.egui_ctx.input(|input| input.modifiers.shift) {
                true
            } else {
                node.left > 0
                    && node.up > 0
                    && next_node_h.is_some_and(|n| n.left == 1)
                    && next_node_v.is_some_and(|n| n.up == 1)
            };

            if possible_intersection {
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
            ctx.paint.rect_filled(
                rect,
                Rounding::none(),
                Color32::from_rgba_unmultiplied(255, 0, 0, 100),
            );
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

        *self.parts_drawn.write().unwrap() += 1;
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

    fn create_intersection(&mut self, pos: Vec2i, wire: Option<usize>) {
        let node = self.nodes.get(pos.x() as isize, pos.y() as isize);
        let node = match node {
            Some(v) => *v,
            None => return,
        };
        self.create_intersection_at_node(pos, node, wire)
    }

    // wire param: if Some(w) will use wire w, if None, will try to figure ot which wire to use (with merging wires)
    fn create_intersection_at_node(&mut self, pos: Vec2i, node: WireNode, wire: Option<usize>) {
        fn fix_pointers(
            wires: &mut Chunks2D<16, WireNode>,
            pos: Vec2i,
            vertical: bool,
            dist: u32,
        ) {
            for int_dist in 1.. {
                let node = match vertical {
                    true => wires.get_mut(pos.x() as isize, pos.y() as isize + int_dist as isize),
                    false => wires.get_mut(pos.x() as isize + int_dist as isize, pos.y() as isize),
                };
                let node = match node {
                    Some(v) => v,
                    None => break,
                };
                let node_dir = match vertical {
                    true => &mut node.up,
                    false => &mut node.left,
                };
                if *node_dir != int_dist + dist {
                    // went off the rails!
                    break;
                }
                *node_dir = int_dist;

                if node.wire > 0 {
                    break;
                }
            }
        }

        if node.wire > 0 || node.up == 0 && node.left == 0 {
            return;
        }

        let wire_up = self
            .find_node_from_node(&node, pos, true, false)
            .map(|f| f.wire)
            .unwrap_or(0);
        let wire_left = self
            .find_node_from_node(&node, pos, false, false)
            .map(|f| f.wire)
            .unwrap_or(0);

        let wire = wire.unwrap_or_else(|| {
            if wire_up == wire_left {
                wire_up
            } else if wire_up == 0 {
                wire_left
            } else if wire_left == 0 {
                wire_up
            } else {
                let up = self.wires.get(wire_up);
                let left = self.wires.get(wire_left);

                match (up, left) {
                    (Some(wu), Some(wl)) => {
                        if wu.nodes.len() > wl.nodes.len() {
                            let id = wu.id;
                            self.merge_wires(wu.id, wl.id);
                            id
                        } else {
                            let id = wl.id;
                            self.merge_wires(wl.id, wu.id);
                            id
                        }
                    }
                    _ => 0,
                }
            }
        });
        if wire == 0 {
            return;
        }

        if node.up > 0 {
            fix_pointers(&mut self.nodes, pos, true, node.up);
        }
        if node.left > 0 {
            fix_pointers(&mut self.nodes, pos, false, node.left);
        }
        let node = self
            .nodes
            .get_or_create_mut(pos.x() as isize, pos.y() as isize);

        node.wire = wire;
        let wire = self.wires.get_mut(wire).unwrap();
        wire.nodes.push(pos);
    }

    fn remove_intersection(&mut self, pos: Vec2i, split: bool) {
        let node = self.nodes.get(pos.x() as isize, pos.y() as isize);
        let node = match node {
            Some(v) => *v,
            None => return,
        };
        self.remove_intersection_at_node(pos, node, split)
    }

    // split param: if intersection was of 2 wires, split them
    fn remove_intersection_at_node(&mut self, pos: Vec2i, node: WireNode, split: bool) {
        fn fix_pointers(
            wires: &mut Chunks2D<16, WireNode>,
            pos: Vec2i,
            vertical: bool,
            dist: u32,
        ) {
            for int_dist in 1.. {
                let node = match vertical {
                    true => wires.get_mut(pos.x() as isize, pos.y() as isize + int_dist as isize),
                    false => wires.get_mut(pos.x() as isize + int_dist as isize, pos.y() as isize),
                };
                let node = match node {
                    Some(v) => v,
                    None => break,
                };
                let node_dir = match vertical {
                    true => &mut node.up,
                    false => &mut node.left,
                };
                if *node_dir != int_dist {
                    // went off the rails!
                    break;
                }
                *node_dir = int_dist + dist;

                if node.wire > 0 {
                    break;
                }
            }
        }
        if node.wire == 0 || node.up == 0 && node.left == 0 {
            return;
        }

        let split = split && node.up > 0 && node.left > 0;

        if node.up > 0 {
            fix_pointers(&mut self.nodes, pos, true, node.up);
        }
        if node.left > 0 {
            fix_pointers(&mut self.nodes, pos, false, node.left);
        }

        let wire_id = node.wire;
        let node = self
            .nodes
            .get_or_create_mut(pos.x() as isize, pos.y() as isize);

        node.wire = 0;
        let wire = self.wires.get_mut(wire_id).unwrap();
        if let Some(posi) = wire.nodes.iter().enumerate().find(|v| *v.1 == pos).map(|v| v.0) {
            wire.nodes.remove(posi);
        }

        if split {
            self.split_wires(wire_id)
        }
    }

    fn place_wire_part(&mut self, part: WirePart) {
        let part = match self.optimize_part(part) {
            Some(value) => value,
            None => return,
        };

        let wires_crossed = {
            let mut wires_crossed = HashSet::new();
            for i in 0..=part.length.get() {
                let pos = part.pos
                    + match part.vertical {
                        true => [0, i as i32],
                        false => [i as i32, 0],
                    };

                let node = match self.nodes.get(pos.x() as isize, pos.y() as isize) {
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

        self.create_intersection(part.pos, Some(new_wire));
        self.create_intersection(
            part.pos
                + match part.vertical {
                    true => [0, part.length.get() as i32],
                    false => [part.length.get() as i32, 0],
                },
                Some(new_wire),
        );

        let mut dist = 0;
        for i in 0..=part.length.get() {
            let pos = part.pos
                + match part.vertical {
                    true => [0, i as i32],
                    false => [i as i32, 0],
                };

            let node = self
                .nodes
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

    fn optimize_part(&mut self, part: WirePart) -> Option<WirePart> {
        let mut part_pos = part.pos;
        let mut part_len = part.length.get() as i32;

        let new_start = self
            .nodes
            .get(part_pos.x() as isize, part_pos.y() as isize)
            .and_then(|n| {
                if n.wire > 0 {
                    None
                } else {
                    self.find_node_from_node(n, part_pos, part.vertical, true)
                }
            });

        if let Some(found) = new_start {
            part_len -= found.distance.get() as i32;
            part_pos = found.pos;
        }

        let end_pos = match part.vertical {
            true => part_pos + [0, part_len],
            false => part_pos + [part_len, 0],
        };

        let new_end_dist = self
            .nodes
            .get(end_pos.x() as isize, end_pos.y() as isize)
            .and_then(|n| {
                self.find_node_from_node(n, end_pos, part.vertical, false)
                    .map(|v| v.distance)
            });

        match new_end_dist {
            None => {}
            Some(dist) => {
                part_len -= dist.get() as i32;
            }
        }

        if part_len <= 0 {
            return None;
        }
        let part = WirePart {
            pos: part_pos.convert_values(|v| v as i32),
            length: NonZeroU32::new(part_len as u32).unwrap(),
            vertical: part.vertical,
        };
        Some(part)
    }

    fn wires_at(&self, pos: Vec2i) -> TileWires<'_> {
        match self.nodes.get(pos.x() as isize, pos.y() as isize) {
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
                .nodes
                .get(pos.x() as isize, (pos.y() - up as i32) as isize)
                .and_then(|n| match n.wire {
                    0 => None,
                    wire => self.wires.get(wire),
                }),
        };
        let left = match node.left {
            0 => None,
            left => self
                .nodes
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

        self.set_node_wires(nodes.iter(), wire);

        let wire = &mut self.wires.get_mut(wire).unwrap();
        wire.nodes.extend(nodes);
    }

    fn find_node<'a>(&'a self, pos: Vec2i, vertical: bool, forward: bool) -> Option<FoundNode> {
        let node = self.nodes.get(pos.x() as isize, pos.y() as isize)?;
        self.find_node_from_node(node, pos, vertical, forward)
    }

    fn find_node_from_node<'a>(
        &'a self,
        node: &WireNode,
        pos: Vec2i,
        vertical: bool,
        forward: bool,
    ) -> Option<FoundNode> {
        let pointer = match vertical {
            true => node.up,
            false => node.left,
        };

        if forward {
            let start = if node.wire > 0 { 0 } else { pointer };
            for i in 1.. {
                let offset = start + i;
                let target_pos = match vertical {
                    true => pos + [0, i as i32],
                    false => pos + [i as i32, 0],
                };
                let target = self
                    .nodes
                    .get(target_pos.x() as isize, target_pos.y() as isize)?;
                let target_pointer = match vertical {
                    true => target.up,
                    false => target.left,
                };
                if target_pointer != offset {
                    break;
                }
                if target.wire > 0 {
                    return Some(FoundNode {
                        node: *target,
                        wire: target.wire,
                        pos: target_pos,
                        distance: NonZeroU32::new(i).unwrap(),
                    });
                }
            }
            None
        } else {
            if pointer == 0 {
                None
            } else {
                let target_pos = match vertical {
                    true => pos - [0, pointer as i32],
                    false => pos - [pointer as i32, 0],
                };
                let target = self
                    .nodes
                    .get(target_pos.x() as isize, target_pos.y() as isize)?;
                if target.wire == 0 {
                    None
                } else {
                    Some(FoundNode {
                        node: *target,
                        wire: target.wire,
                        pos: target_pos,
                        distance: NonZeroU32::new(pointer).unwrap(),
                    })
                }
            }
        }
    }

    fn split_wires(&mut self, id: usize) {
        if id == 0 {
            return;
        }
        let wire = match self.wires.get(id) {
            None => return,
            Some(v) => v,
        };

        let mut groups = vec![];

        let mut remaining_nodes: HashSet<_> = wire.nodes.iter().map(|v| *v).collect();
        let mut queue = vec![];

        while remaining_nodes.len() > 0 {
            let mut group = vec![];
            let start = *remaining_nodes.iter().next().unwrap();
            queue.push(start);

            while let Some(pos) = queue.pop() {
                if !remaining_nodes.remove(&pos) {
                    continue;
                }

                group.push(pos);
                
                let (ints, intc) = self.node_neighboring_intersections(pos, Some(id));
                for inti in 0..intc {
                    let int = ints[inti];
                    if remaining_nodes.contains(&int) {
                        queue.push(int);
                    }
                }
            }

            groups.push(group);
        }

        if groups.len() <= 1 {
            return;
        }

        let this_wire_idx = groups.iter().enumerate().max_by(|a, b| a.1.len().cmp(&b.1.len())).unwrap().0;

        for (groupid, group) in groups.drain(..).enumerate() {
            let wire = if groupid == this_wire_idx {
                self.wires.get_mut(id).unwrap()
            } else {
                let wire = self.create_wire().id;
                self.set_node_wires(group.iter(), wire);
                self.wires.get_mut(wire).unwrap()
            };
            wire.nodes = group
        }
    }

    fn node_neighboring_intersections(&self, pos: Vec2i, wire: Option<usize>) -> ([Vec2i; 4], usize) {
        let mut arr = [Vec2i::default(); 4];
        let mut arrpos = 0;

        let node = match self.nodes.get(pos.x() as isize, pos.y() as isize) {
            None => return (arr, 0),
            Some(v) => v,
        };

        for i in 0..4 {
            let vertical = i & 1 == 1;
            let forward = i & 2 == 2;
            if let Some(f) = self.find_node_from_node(node, pos, vertical, forward) {
                if !wire.is_some_and(|v| v != f.wire) {
                    arr[arrpos] = f.pos;
                    arrpos += 1;
                }
            }
        }
        (arr, arrpos)
    }

    fn set_node_wires<'a>(&mut self, positions: impl Iterator<Item = &'a Vec2i>, wire: usize) {
        for npos in positions {
            match self.nodes.get_mut(npos.x() as isize, npos.y() as isize) {
                None => {}
                Some(n) => n.wire = wire,
            }
        }

        // TODO: correctly handle wire changes
    }
}

struct FoundNode {
    node: WireNode,
    wire: usize,
    pos: Vec2i,
    distance: NonZeroU32,
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
