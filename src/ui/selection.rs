use std::{collections::HashSet, hash::Hash, marker::PhantomData};

use eframe::{
    egui::{self, layers::ShapeIdx, Sense},
    epaint::{Color32, Rounding, Shape, Stroke},
};
use emath::Rect;

use crate::{vector::Vec2f, PaintContext};

use super::InventoryItem;

#[derive(Clone, Copy)]
pub enum SelectionMode {
    Include,
    Exclude,
}

pub trait SelectionImpl<O, P> {
    fn collect_changes(&mut self, pass: &P, changes: &mut HashSet<O>, rect: Rect);

    fn draw_object_selection(
        &mut self,
        pass: &P,
        object: &O,
        ctx: &PaintContext,
        shapes: &mut Vec<Shape>,
    );
    fn post_draw_selection(
        &mut self,
        pass: &P,
        ctx: &PaintContext,
        mode: SelectionMode,
        selected: &HashSet<O>,
        change: &HashSet<O>,
        shapes: &mut Vec<Shape>,
    ) {
        let _ = (pass, ctx, mode, selected, change, shapes);
    }
}

pub struct Selection<I, O, P>
where
    I: SelectionImpl<O, P>,
    O: Hash + Eq,
{
    start_pos: Option<Vec2f>,
    pub rect: Option<Rect>,
    pub change: HashSet<O>,
    pub mode: SelectionMode,
    pub selection: HashSet<O>,
    imp: I,
    phantom: PhantomData<P>,
    prev_frame_shapes: usize,

    shape_indexes: Vec<ShapeIdx>,
    shapes: Vec<Shape>,
}

pub fn selection_fill_color() -> Color32 {
    Color32::from_rgba_unmultiplied(200, 200, 255, 10)
}

pub fn selection_border_color() -> Color32 {
    Color32::WHITE
}

impl<I, O, P> Selection<I, O, P>
where
    I: SelectionImpl<O, P>,
    O: Hash + Eq,
{
    pub fn new(imp: I) -> Self {
        Self {
            start_pos: None,
            rect: None,
            selection: HashSet::new(),
            change: HashSet::new(),
            mode: SelectionMode::Include,
            imp,
            phantom: PhantomData,
            prev_frame_shapes: 0,
            shape_indexes: vec![],
            shapes: vec![],
        }
    }

    pub fn pre_update_selection(&mut self, pass: &P, ctx: &PaintContext, selected: bool) {
        if !selected {
            self.start_pos = None;
            self.rect = None;
        } else {
            let mouse_tile_pos = ctx
                .ui
                .input(|input| input.pointer.interact_pos())
                .map(|p| ctx.screen.screen_to_world(Vec2f::from(p)));

            if let Some(selection_start_pos) = self.start_pos {
                if let Some(mouse_tile_pos) = mouse_tile_pos {
                    let start = selection_start_pos;
                    let end = mouse_tile_pos;

                    let min = Vec2f::from([start.x().min(end.x()), start.y().min(end.y())]);
                    let max = Vec2f::from([start.x().max(end.x()), start.y().max(end.y())]);

                    let rect_min = ctx.screen.world_to_screen(min);
                    let rect_max = ctx.screen.world_to_screen(max);
                    let rect = Rect::from_min_max(rect_min.into(), rect_max.into());
                    self.rect = Some(rect);

                    ctx.paint.rect_filled(
                        rect,
                        Rounding::ZERO,
                        Color32::from_rgba_unmultiplied(200, 200, 255, 10),
                    );

                    self.change.clear();
                    self.imp.collect_changes(
                        pass,
                        &mut self.change,
                        Rect::from_min_max(min.into(), max.into()),
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
                    .ui
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

        let mut min_shapes = 0;
        for object in self.selection.iter() {
            if matches!(self.mode, SelectionMode::Exclude) && self.change.contains(object) {
                continue;
            }
            min_shapes += 1;
        }
        if matches!(self.mode, SelectionMode::Include) {
            for object in self.change.iter() {
                if self.selection.contains(object) {
                    continue;
                }
                min_shapes += 1;
            }
        }

        let alloc_shapes = min_shapes.max(self.prev_frame_shapes);
        self.shape_indexes.clear();
        for _ in 0..alloc_shapes {
            self.shape_indexes.push(ctx.paint.add(Shape::Noop))
        }
    }

    pub fn update_selection(&mut self, pass: &P, ctx: &PaintContext) {
        self.shapes.clear();

        for object in self.selection.iter() {
            if matches!(self.mode, SelectionMode::Exclude) && self.change.contains(object) {
                continue;
            }
            self.imp
                .draw_object_selection(pass, object, ctx, &mut self.shapes);
        }
        if matches!(self.mode, SelectionMode::Include) {
            for object in self.change.iter() {
                if self.selection.contains(object) {
                    continue;
                }
                self.imp
                    .draw_object_selection(pass, object, ctx, &mut self.shapes);
            }
        }

        self.imp.post_draw_selection(
            pass,
            ctx,
            self.mode,
            &self.selection,
            &self.change,
            &mut self.shapes,
        );

        self.prev_frame_shapes = self.shapes.len();

        for (idx, shape) in self.shape_indexes.drain(..).zip(self.shapes.drain(..)) {
            ctx.paint.set(idx, shape);
        }

        if let Some(rect) = self.rect {
            ctx.paint
                .rect_stroke(rect, Rounding::ZERO, Stroke::new(1.0, Color32::WHITE));
        }
    }

    pub fn iter_selection(&self) -> impl Iterator<Item = &O> {
        self.selection
            .iter()
            .filter(|o| !matches!(self.mode, SelectionMode::Exclude) || !self.change.contains(o))
            .chain(self.change.iter().filter(|_| matches!(self.mode, SelectionMode::Include)))
    }

    pub fn is_selected(&self, obj: &O) -> bool {
        let selected = self.selection.contains(obj);
        let change = self.change.contains(obj);
        match self.mode {
            SelectionMode::Include => selected || change,
            SelectionMode::Exclude => selected && !change,
        }
    }
}

impl<I, O, P> Default for Selection<I, O, P>
where
    I: SelectionImpl<O, P> + Default,
    O: Hash + Eq,
{
    fn default() -> Self {
        Self::new(I::default())
    }
}

pub struct SelectionInventoryItem<I: Clone> {
    id: I,
}

impl<I: Clone> SelectionInventoryItem<I> {
    pub fn new(id: I) -> Self {
        Self { id }
    }
}

impl<I: Clone> InventoryItem<I> for SelectionInventoryItem<I> {
    fn id(&self) -> I {
        self.id.clone()
    }

    fn draw(&self, ctx: &PaintContext) {
        let rect = ctx.rect.shrink2(ctx.rect.size() / 5.0);
        ctx.paint
            .rect_filled(rect, Rounding::ZERO, selection_fill_color());
        let rect_corners = [
            rect.left_top(),
            rect.right_top(),
            rect.right_bottom(),
            rect.left_bottom(),
            rect.left_top(),
        ];

        let mut shapes = vec![];
        Shape::dashed_line_many(
            &rect_corners,
            Stroke::new(1.0, selection_border_color()),
            3.0,
            2.0,
            &mut shapes,
        );

        shapes.into_iter().for_each(|s| {
            ctx.paint.add(s);
        });
    }
}
