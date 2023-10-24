use std::{collections::HashSet, hash::Hash, marker::PhantomData};

use eframe::{
    egui::{self, Sense},
    epaint::{Color32, Rounding, Stroke, Shape},
};
use emath::Rect;

use crate::{
    vector::Vec2f,
    PaintContext,
};

use super::InventoryItem;

#[derive(Clone, Copy)]
pub enum SelectionMode {
    Include,
    Exclude,
}

pub trait SelectionImpl<O, P> {
    fn collect_changes(&mut self, pass: &P, changes: &mut HashSet<O>, rect: Rect);

    fn draw_object_selection(&mut self, pass: &P, object: &O, ctx: &PaintContext);
    fn post_draw_selection(&mut self, pass: &P, ctx: &PaintContext, mode: SelectionMode, selected: &HashSet<O>, change: &HashSet<O>) {
        let _ = (pass, ctx, mode, selected, change);
    }
}

pub struct Selection<I, O, P>
where
    I: SelectionImpl<O, P>,
    O: Hash + Eq,
{
    start_pos: Option<Vec2f>,
    rect: Option<Rect>,
    change: HashSet<O>,
    mode: SelectionMode,
    pub selection: HashSet<O>,
    imp: I,
    phantom: PhantomData<P>,
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
                        Rounding::none(),
                        Color32::from_rgba_unmultiplied(200, 200, 255, 10),
                    );

                    self.change.clear();
                    self.imp.collect_changes(
                        pass,
                        &mut self.change,
                        rect,
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

        for object in self.selection.iter() {
            if matches!(self.mode, SelectionMode::Exclude) && self.change.contains(object) {
                continue;
            }
            self.imp.draw_object_selection(pass, object, ctx);
        }
        if matches!(self.mode, SelectionMode::Include) {
            for object in self.change.iter() {
                if self.selection.contains(object) {
                    continue;
                }
                self.imp.draw_object_selection(pass, object, ctx);
            }
        }

        self.imp.post_draw_selection(pass, ctx, self.mode, &self.selection, &self.change);
    }

    pub fn update_selection(&mut self, ctx: &PaintContext) {
        if let Some(rect) = self.rect {
            ctx.paint
                .rect_stroke(rect, Rounding::none(), Stroke::new(1.0, Color32::WHITE));
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
    id: I
}

impl<I: Clone> SelectionInventoryItem<I> {
    pub fn new(id: I) -> Self { Self { id } }
}

impl<I: Clone> InventoryItem<I> for SelectionInventoryItem<I> {
    fn id(&self) -> I {
        self.id.clone()
    }

    fn draw(&self, ctx: &PaintContext) {
        let rect = ctx.rect.shrink2(ctx.rect.size() / 5.0);
        ctx.paint
            .rect_filled(rect, Rounding::none(), selection_fill_color());
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
