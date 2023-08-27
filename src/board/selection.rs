use std::collections::HashSet;

use eframe::{
    egui::{self, Sense},
    epaint::{Color32, Rounding, Stroke},
};
use emath::Rect;

use crate::{
    unwrap_option_or_continue,
    vector::{Vec2f, Vec2i, Vec2u},
    wires::WirePart,
    Direction2, Direction4, PaintContext,
};

use super::ActiveCircuitBoard;

#[derive(Hash, PartialEq, Eq)]
pub enum SelectedWorldObject {
    WirePart { pos: Vec2i, dir: Direction2 },
    Circuit { id: usize },
}

enum SelectionMode {
    Include,
    Exclude,
}

pub struct Selection {
    start_pos: Option<Vec2f>,
    rect: Option<Rect>,
    change: HashSet<SelectedWorldObject>,
    mode: SelectionMode,
    pub selection: HashSet<SelectedWorldObject>,
}

impl Selection {
    pub fn fill_color() -> Color32 {
        Color32::from_rgba_unmultiplied(200, 200, 255, 10)
    }

    pub fn border_color() -> Color32 {
        Color32::WHITE
    }

    pub fn new() -> Self {
        Self {
            start_pos: None,
            rect: None,
            selection: HashSet::new(),
            change: HashSet::new(),
            mode: SelectionMode::Include,
        }
    }

    pub fn pre_update_selection(
        &mut self,
        board: &ActiveCircuitBoard,
        ctx: &PaintContext,
        selected: bool,
    ) {
        fn draw_selection(
            this: &ActiveCircuitBoard,
            item: &SelectedWorldObject,
            ctx: &PaintContext,
            possible_points: &mut HashSet<Vec2i>,
        ) {
            match item {
                SelectedWorldObject::WirePart { pos, dir } => {
                    if let Some(w) = this.find_wire_node(*pos, (*dir).into()) {
                        let part = WirePart {
                            pos: *pos,
                            dir: *dir,
                            length: w.distance,
                        };
                        let rect = ActiveCircuitBoard::calc_wire_part_rect(&ctx.screen, &part);
                        let rect = rect.expand(2.0);
                        ctx.paint
                            .rect_filled(rect, Rounding::none(), Color32::WHITE);

                        possible_points.insert(*pos);
                        possible_points.insert(w.pos);
                    }
                }
                SelectedWorldObject::Circuit { id } => {
                    if let Some(circ) = this.board.read().circuits.get(*id) {
                        let rect_pos = ctx.screen.world_to_screen_tile(circ.pos);
                        let rect_size =
                            circ.info.read().size.convert(|v| v as f32) * ctx.screen.scale;
                        let rect = Rect::from_min_size(rect_pos.into(), rect_size.into());
                        let rect = rect.expand(2.0);
                        ctx.paint.rect(
                            rect,
                            Rounding::none(),
                            Selection::fill_color(),
                            Stroke::new(2.0, Selection::border_color()),
                        );
                    }
                }
            }
        }

        if !selected {
            self.start_pos = None;
            self.rect = None;
        } else {
            let mouse_tile_pos = ctx
                .egui_ctx
                .input(|input| input.pointer.interact_pos())
                .map(|p| ctx.screen.screen_to_world(Vec2f::from(p)));

            if let Some(selection_start_pos) = self.start_pos {
                if let Some(mouse_tile_pos) = mouse_tile_pos {
                    let start = selection_start_pos;
                    let end = mouse_tile_pos;

                    let min = Vec2f::from([start.x().min(end.x()), start.y().min(end.y())]);
                    let max = Vec2f::from([start.x().max(end.x()), start.y().max(end.y())]);

                    let min_tile = min.convert(|v| v.floor() as i32);
                    let max_tile = max.convert(|v| v.ceil() as i32);

                    let rect_min = ctx.screen.world_to_screen(min);
                    let rect_max = ctx.screen.world_to_screen(max);
                    let rect = Rect::from_min_max(rect_min.into(), rect_max.into());
                    self.rect = Some(rect);

                    ctx.paint.rect_filled(
                        rect,
                        Rounding::none(),
                        Color32::from_rgba_unmultiplied(200, 200, 255, 10),
                    );

                    self.selection_update_changes(
                        board,
                        min_tile,
                        (max_tile - min_tile).convert(|v| v as u32),
                    );
                } else {
                    self.rect = None;
                }
            } else {
                self.rect = None;
            }

            let interaction = ctx
                .ui
                .interact(ctx.rect, ctx.ui.id(), Sense::click_and_drag());

            if self.start_pos.is_none() && interaction.drag_started_by(egui::PointerButton::Primary)
            {
                self.start_pos = mouse_tile_pos;
                self.change.clear();

                let (shift, ctrl) = ctx
                    .egui_ctx
                    .input(|input| (input.modifiers.shift, input.modifiers.ctrl));
                if !shift && !ctrl {
                    self.selection.clear();
                }

                if ctrl {
                    self.mode = SelectionMode::Exclude;
                } else {
                    self.mode = SelectionMode::Include;
                }
            } else if self.start_pos.is_some()
                && interaction.drag_released_by(egui::PointerButton::Primary)
            {
                self.start_pos = None;

                match self.mode {
                    SelectionMode::Include => self.selection.extend(self.change.drain()),
                    SelectionMode::Exclude => {
                        for i in self.change.drain() {
                            self.selection.remove(&i);
                        }
                    }
                }

                self.change.clear();
            }
        }

        let mut possible_points = HashSet::new();

        for item in self.selection.iter() {
            if matches!(self.mode, SelectionMode::Exclude) && self.change.contains(item) {
                continue;
            }
            draw_selection(board, item, ctx, &mut possible_points);
        }
        if matches!(self.mode, SelectionMode::Include) {
            for item in self.change.iter() {
                if self.selection.contains(item) {
                    continue;
                }
                draw_selection(board, item, ctx, &mut possible_points);
            }
        }

        let shift = ctx.egui_ctx.input(|input| input.modifiers.shift);
        for point in possible_points {
            if board.should_draw_wire_point(point, shift) {
                let node =
                    unwrap_option_or_continue!(board.wire_nodes.get(point.convert(|v| v as isize)));
                let all_connections_selected = Direction4::iter_all().all(|dir| {
                    node.get_dir(dir).is_none_or(|_| {
                        let (part_dir, forward) = dir.into_dir2();

                        let part_pos = if forward {
                            Some(point)
                        } else {
                            board
                                .find_wire_node_from_node(node, point, dir)
                                .map(|f| f.pos)
                        };

                        match part_pos {
                            None => false,
                            Some(part_pos) => {
                                let part = SelectedWorldObject::WirePart {
                                    pos: part_pos,
                                    dir: part_dir,
                                };

                                match self.mode {
                                    SelectionMode::Include => {
                                        self.selection.contains(&part)
                                            || self.change.contains(&part)
                                    }
                                    SelectionMode::Exclude => {
                                        self.selection.contains(&part)
                                            && !self.change.contains(&part)
                                    }
                                }
                            }
                        }
                    })
                });

                if all_connections_selected {
                    let rect = ActiveCircuitBoard::calc_wire_point_rect(&ctx.screen, point);
                    let rect = rect.expand(2.0);
                    ctx.paint
                        .rect_filled(rect, Rounding::none(), Color32::WHITE);
                }
            }
        }
    }

    pub fn update_selection(&mut self, ctx: &PaintContext) {
        if let Some(rect) = self.rect {
            ctx.paint
                .rect_stroke(rect, Rounding::none(), Stroke::new(1.0, Color32::WHITE));
        }
    }

    fn selection_update_changes(&mut self, board: &ActiveCircuitBoard, pos: Vec2i, size: Vec2u) {
        self.change.clear();

        for (pos, node) in board
            .wire_nodes
            .iter_area(pos.convert(|v| v as isize), size.convert(|v| v as usize))
        {
            let pos = pos.convert(|v| v as i32);

            //if let Some(wire) = node.wire.get() {
            //    self.selection_change
            //        .insert(SelectedWorldObject::WirePoint { id: wire, pos });
            //}
            if node.wire.is_some() {
                for dir in Direction4::iter_all() {
                    let node = board.find_wire_node_from_node(node, pos, dir);
                    let node = unwrap_option_or_continue!(node);
                    let (dir, forward) = dir.into_dir2();

                    self.change.insert(SelectedWorldObject::WirePart {
                        pos: if forward { pos } else { node.pos },
                        dir,
                    });
                }
            } else {
                for dir in [Direction4::Right, Direction4::Down] {
                    let node = board.find_wire_node_from_node(node, pos, dir);
                    let node = unwrap_option_or_continue!(node);
                    self.change.insert(SelectedWorldObject::WirePart {
                        pos: node.pos,
                        dir: dir.into_dir2().0,
                    });
                }
            }
        }
        for (_, node) in board
            .circuit_nodes
            .iter_area(pos.convert(|v| v as isize), size.convert(|v| v as usize))
        {
            let circuit = unwrap_option_or_continue!(node.circuit.get());

            self.change
                .insert(SelectedWorldObject::Circuit { id: circuit });
        }
    }
}

impl Default for Selection {
    fn default() -> Self {
        Self::new()
    }
}
