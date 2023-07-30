use std::{borrow::BorrowMut, collections::HashSet, rc::Rc, sync::Arc, ops::DerefMut, cell::RefCell};

use eframe::{
    egui::{Margin, PointerButton, Sense, Widget},
    epaint::{Color32, Rounding, Stroke},
};
use emath::{vec2, Rect, Vec2};

use crate::{PaintContext, RwLock};

pub enum InventoryItemGroup {
    SingleItem(Box<dyn InventoryItem>),
    Group(Vec<Box<dyn InventoryItem>>),
}

pub trait InventoryItem {
    fn id<'a>(&'a self) -> &'a str;
    fn draw(&self, ctx: &PaintContext);
}

pub struct Inventory<'a> {
    pub selected: &'a mut Option<String>,
    pub groups: &'a Vec<InventoryItemGroup>,
    pub item_size: Vec2,
    pub item_margin: Margin,
    pub margin: Margin,
}

#[derive(Clone, Default)]
struct InventoryMemory {
    last_group_items: Arc<RwLock<HashSet<String>>>,
}

impl Widget for Inventory<'_> {
    fn ui(self, ui: &mut eframe::egui::Ui) -> eframe::egui::Response {
        let (selected_item, selected_sub_item) = match self.selected {
            None => (None, None),
            Some(selected) => self
                .groups
                .iter()
                .enumerate()
                .find_map(|(i, item)| match item {
                    InventoryItemGroup::SingleItem(item) => {
                        if item.id() == *selected {
                            Some((Some(i), None))
                        } else {
                            None
                        }
                    }
                    InventoryItemGroup::Group(items) => items
                        .iter()
                        .enumerate()
                        .find_map(|(ii, item)| {
                            if item.id() == *selected {
                                Some(ii)
                            } else {
                                None
                            }
                        })
                        .map(|found| (Some(i), Some(found))),
                })
                .unwrap_or((None, None)),
        };

        let selected_group = selected_sub_item.and_then(|_| {
            selected_item
                .and_then(|i| self.groups.get(i))
                .and_then(|item| match item {
                    InventoryItemGroup::SingleItem(_) => None,
                    InventoryItemGroup::Group(vec) => Some(vec),
                })
        });

        let full_item_size = self.item_size + self.item_margin.sum();

        let width = match selected_group {
            Some(group) => full_item_size.x * (self.groups.len() - 1 + group.len()) as f32,
            None => full_item_size.x * self.groups.len() as f32,
        };

        let (id, rect) = ui.allocate_space(vec2(width, full_item_size.y) + self.margin.sum());
        let rect = apply_margin_to_rect(rect, self.margin);

        let response = ui.interact(rect, id, Sense::click());

        let paint = ui.painter_at(rect);
        let rounding = Rounding::same(3.0);
        paint.rect(
            rect,
            rounding,
            Color32::from_gray(32),
            Stroke::new(1.0, Color32::from_gray(100)),
        );

        let paint_ctx = PaintContext::new_on_ui(ui, rect);

        let click_pos = response
            .clicked_by(PointerButton::Primary)
            .then(|| response.interact_pointer_pos())
            .flatten();

        let mut x = 0.0;
        let mut selected_changed = false;
        for (i, item) in self.groups.iter().enumerate() {
            let selected = selected_item.is_some_and(|ii| ii == i);
            match item {
                InventoryItemGroup::Group(items) => {
                    if items.is_empty() {
                        continue;
                    }

                    if !selected {

                        let icon_index = ui.memory(|mem| mem.data.get_temp::<InventoryMemory>(id).and_then(|mem| {
                            let last_group_items = mem.last_group_items.read().unwrap();
                            for (i, item) in items.iter().enumerate() {
                                if last_group_items.contains(item.id()) {
                                    return Some(i);
                                }
                            }
                            None
                        }));
                        let icon_index = icon_index.unwrap_or(0);

                        let rect =
                            Rect::from_min_size(rect.left_top() + vec2(x, 0.0), full_item_size);

                        paint_ctx.paint.circle_filled(rect.lerp_inside([0.9, 0.9].into()), rect.width() / 20.0, Color32::GREEN);
                        let item = &items[icon_index];
                        
                        if click_pos.is_some_and(|pos| rect.contains(pos)) {
                            let id = item.id();
                            if self.selected.as_ref().is_some_and(|sel| sel == id) {
                                *self.selected = None;
                            } else {
                                *self.selected = Some(id.to_string());
                            }
                            selected_changed = true;
                        }
                        let rect = apply_margin_to_rect(rect, self.item_margin);
                        let item_ctx = paint_ctx.with_rect(rect);
                        item.draw(&item_ctx);
                        x += full_item_size.x;
                    } else {
                        let group_rect = Rect::from_min_size(
                            rect.left_top() + vec2(x, 0.0),
                            full_item_size * vec2(items.len() as f32, 1.0),
                        );

                        paint_ctx.paint.rect(
                            group_rect.shrink(0.5),
                            rounding,
                            Color32::from_gray(48),
                            Stroke::new(1.0, Color32::from_gray(100)),
                        );

                        for (i, item) in items.iter().enumerate() {
                            let rect =
                                Rect::from_min_size(rect.left_top() + vec2(x, 0.0), full_item_size);
                            let selected = selected_sub_item.is_some_and(|si| si == i);
                            if selected {
                                paint_ctx.paint.rect(
                                    rect,
                                    rounding,
                                    Color32::from_gray(64),
                                    Stroke::new(1.0, Color32::from_gray(128)),
                                );
                            }
                            if click_pos.is_some_and(|pos| rect.contains(pos)) {
                                let id = item.id();
                                if self.selected.as_ref().is_some_and(|sel| sel == id) {
                                    *self.selected = None;
                                } else {
                                    *self.selected = Some(id.to_string());
                                }
                                selected_changed = true;
                            }
                            let rect = apply_margin_to_rect(rect, self.item_margin);
                            let item_ctx = paint_ctx.with_rect(rect);
                            item.draw(&item_ctx);
                            x += full_item_size.x;
                        }
                    }
                }
                InventoryItemGroup::SingleItem(item) => {
                    let rect = Rect::from_min_size(rect.left_top() + vec2(x, 0.0), full_item_size);
                    if selected {
                        paint_ctx.paint.rect(
                            rect,
                            rounding,
                            Color32::from_gray(64),
                            Stroke::new(1.0, Color32::from_gray(128)),
                        );
                    }
                    if click_pos.is_some_and(|pos| rect.contains(pos)) {
                        let id = item.id();
                        if self.selected.as_ref().is_some_and(|sel| sel == id) {
                            *self.selected = None;
                        } else {
                            *self.selected = Some(id.to_string());
                        }
                    }
                    let rect = apply_margin_to_rect(rect, self.item_margin);
                    let item_ctx = paint_ctx.with_rect(rect);
                    item.draw(&item_ctx);
                    x += full_item_size.x;
                }
            }
        }

        if selected_changed {
            if let Some(selected_id) = self.selected.as_ref() {
                if let Some(group) = selected_group {
                    ui.memory_mut(|mem| {
                        let m = mem.data.get_temp_mut_or_default::<InventoryMemory>(id);
                        let mut last_group_items = m.last_group_items.write().unwrap();
                        for item in group {
                            last_group_items.remove(item.id());
                        } 
                        last_group_items.insert(selected_id.clone());
                    });
                }
            }
        }

        response
    }
}

fn apply_margin_to_rect(rect: Rect, margin: Margin) -> Rect {
    Rect {
        min: rect.min + margin.left_top(),
        max: rect.max - margin.right_bottom(),
    }
}

fn apply_margin_to_rect_inverse(rect: Rect, margin: Margin) -> Rect {
    Rect {
        min: rect.min - margin.left_top(),
        max: rect.max + margin.right_bottom(),
    }
}
