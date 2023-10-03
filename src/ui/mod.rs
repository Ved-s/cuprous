use std::{any::TypeId, collections::HashSet, f32::consts::TAU, ops::Deref, sync::Arc};

use eframe::{
    egui::{
        panel::{self, PanelState},
        FontSelection, Grid, Id, InnerResponse, Key, Label, Margin, PointerButton, Response, Sense,
        SidePanel, TextStyle, Ui, Widget, WidgetText,
    },
    epaint::{Color32, PathShape, Rounding, Stroke, TextShape},
};
use emath::{pos2, vec2, Rect, Rot2, Vec2};

use crate::{
    circuits::props::{CircuitProperty, CircuitPropertyImpl, CircuitPropertyStore},
    DynStaticStr, PaintContext, RwLock,
};

pub enum InventoryItemGroup {
    SingleItem(Box<dyn InventoryItem>),
    Group(Vec<Box<dyn InventoryItem>>),
}

pub trait InventoryItem {
    fn id(&self) -> &str;
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

const NUMBER_KEYS: [Key; 10] = [
    Key::Num0,
    Key::Num1,
    Key::Num2,
    Key::Num3,
    Key::Num4,
    Key::Num5,
    Key::Num6,
    Key::Num7,
    Key::Num8,
    Key::Num9,
];

impl Inventory<'_> {
    fn get_group_icon_item_id(
        group: &[Box<dyn InventoryItem>],
        ui: &Ui,
        self_id: Id,
    ) -> Option<usize> {
        ui.memory(|mem| {
            mem.data
                .get_temp::<InventoryMemory>(self_id)
                .and_then(|mem| {
                    let last_group_items = mem.last_group_items.read();
                    for (i, item) in group.iter().enumerate() {
                        if last_group_items.contains(item.id()) {
                            return Some(i);
                        }
                    }
                    None
                })
        })
    }

    fn update_selected(
        &mut self,
        ui: &Ui,
        current_index: Option<usize>,
        current_sub_index: Option<usize>,
    ) -> (Option<usize>, Option<usize>) {
        let digit_key = ui.input(|input| {
            NUMBER_KEYS
                .iter()
                .enumerate()
                .find(|t| input.num_presses(*t.1) == 1)
                .map(|t| t.0)
        });

        let index = match digit_key {
            None => return (current_index, current_sub_index),
            Some(0) => {
                *self.selected = None;
                return (None, None);
            }
            Some(digit) => digit - 1,
        };

        let shift = ui.input(|input| input.modifiers.shift);

        if shift {
            if current_sub_index == Some(index) {
                *self.selected = None;
                return (None, None);
            }

            let selected_group = current_index
                .and_then(|i| self.groups.get(i))
                .and_then(|item| match item {
                    InventoryItemGroup::SingleItem(_) => None,
                    InventoryItemGroup::Group(vec) => Some(vec),
                });

            if let Some(group) = selected_group {
                if let Some(item) = group.get(index) {
                    *self.selected = Some(item.id().to_owned());
                    return (current_index, Some(index));
                }
            }
        } else if current_index == Some(index) {
            *self.selected = None;
            return (None, None);
        } else if let Some(item) = self.groups.get(index) {
            match item {
                InventoryItemGroup::SingleItem(item) => {
                    *self.selected = Some(item.id().to_owned());
                    return (Some(index), None);
                }
                InventoryItemGroup::Group(group) => {
                    let item_index =
                        Inventory::get_group_icon_item_id(group, ui, ui.next_auto_id())
                            .unwrap_or(0);

                    if let Some(item) = group.get(item_index) {
                        *self.selected = Some(item.id().to_owned());
                        return (Some(index), Some(item_index));
                    }
                }
            }
        }
        (current_index, current_sub_index)
    }
}

impl Widget for Inventory<'_> {
    fn ui(mut self, ui: &mut eframe::egui::Ui) -> eframe::egui::Response {
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

        let (selected_item, selected_sub_item) =
            self.update_selected(ui, selected_item, selected_sub_item);

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
                        let icon_index =
                            Inventory::get_group_icon_item_id(items, ui, id).unwrap_or(0);

                        let rect =
                            Rect::from_min_size(rect.left_top() + vec2(x, 0.0), full_item_size);

                        paint_ctx.paint.circle_filled(
                            rect.lerp_inside([0.9, 0.9].into()),
                            rect.width() / 20.0,
                            Color32::GREEN,
                        );
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
                        let mut last_group_items = m.last_group_items.write();
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

#[derive(PartialEq, Eq, Hash, Clone)]
struct PropertyId {
    id: DynStaticStr,
    name: DynStaticStr,
    ty: TypeId,
}

impl PropertyId {
    pub fn id_of(prop: &CircuitProperty) -> Self {
        Self {
            id: prop.id(),
            name: prop.name(),
            ty: prop.imp().type_id(),
        }
    }
}

pub struct PropertyStoreItem<'a, T: Clone> {
    pub store: &'a CircuitPropertyStore,
    pub id: T,
}

impl<'a, T: Clone> PropertyStoreItem<'a, T> {
    pub fn new(id: T, store: &'a CircuitPropertyStore) -> Self {
        Self { store, id }
    }
}

impl<'a, T: Clone> From<(T, &'a CircuitPropertyStore)> for PropertyStoreItem<'a, T> {
    fn from(value: (T, &'a CircuitPropertyStore)) -> Self {
        Self::new(value.0, value.1)
    }
}

pub struct PropertyEditor {
    ids_cache: HashSet<PropertyId>,
    ids_cache_remove: HashSet<PropertyId>,
}

pub struct PropertyEditorResponse<T> {
    pub response: Response,
    pub changes: Vec<ChangedProperty<T>>,
}

pub struct ChangedProperty<T> {
    pub id: DynStaticStr,
    pub affected_values: Vec<T>,
    pub old_value: Box<dyn CircuitPropertyImpl>,
}

// awful code
impl PropertyEditor {
    pub fn new() -> Self {
        Self {
            ids_cache: HashSet::new(),
            ids_cache_remove: HashSet::new(),
        }
    }

    pub fn ui<'a, T: Clone>(
        &'a mut self,
        ui: &mut Ui,
        props: impl IntoIterator<Item = PropertyStoreItem<'a, T>>,
    ) -> PropertyEditorResponse<T> {
        let mut stores = vec![];
        self.ids_cache.clear();
        self.ids_cache_remove.clear();

        for (i, store) in props.into_iter().enumerate() {
            let map = store.store.inner().read();
            if i == 0 {
                self.ids_cache.extend(map.values().map(PropertyId::id_of));
            } else {
                self.ids_cache_remove.clear();
                self.ids_cache_remove
                    .extend(
                        self.ids_cache
                            .iter()
                            .filter_map(|id| match map.get(&id.id) {
                                None => Some(id.clone()),
                                Some(p) => (PropertyId::id_of(p) != *id).then(|| id.clone()),
                            }),
                    );
                for str in self.ids_cache_remove.iter() {
                    self.ids_cache.remove(str.deref());
                }
            }
            stores.push(store);
        }

        let response = Grid::new(("prop_editor", stores.len())).show(ui, |ui| {
            let mut guards: Vec<_> = stores
                .iter()
                .map(|s| (s.id.clone(), s.store.inner().write()))
                .collect();
            let mut changes = Vec::new();
            let mut none = true;
            for id in self.ids_cache.iter() {
                let mut props: Vec<_> = guards
                    .iter_mut()
                    .filter_map(|(i, s)| {
                        s.get_mut(&id.id)
                            .filter(|p| p.imp().type_id() == id.ty)
                            .map(|p| (i.clone(), p))
                    })
                    .collect();

                if props.is_empty() {
                    continue;
                }

                let equal = props.windows(2).all(|w| w[0].1.imp().equals(w[1].1.imp()));

                ui.label(id.name.deref());

                if let Some(old) = props[0].1.imp_mut().ui(ui, !equal) {
                    let mut vec = vec![];
                    let (id, prop) = props.remove(0);
                    vec.push(id);
                    for other_prop in props {
                        if !other_prop.1.imp().equals(prop.imp()) {
                            prop.imp().copy_into(other_prop.1.imp_mut());
                            vec.push(other_prop.0);
                        }
                    }
                    changes.push(ChangedProperty {
                        id: prop.id(),
                        affected_values: vec,
                        old_value: old,
                    });
                }
                none = false;
                ui.end_row();
            }

            if none {
                ui.vertical_centered(|ui| Label::new("No editable properties").wrap(false).ui(ui));
            }

            changes
        });
        PropertyEditorResponse {
            response: response.response,
            changes: response.inner,
        }
    }
}

impl Default for PropertyEditor {
    fn default() -> Self {
        Self::new()
    }
}

fn apply_margin_to_rect(rect: Rect, margin: Margin) -> Rect {
    Rect {
        min: rect.min + margin.left_top(),
        max: rect.max - margin.right_bottom(),
    }
}

pub struct CollapsibleSidePanel {
    id: Id,
    text: WidgetText,
    active: bool,
    panel_transformer: Option<Box<dyn Fn(SidePanel) -> SidePanel>>,
    header_offset: f32,
    side: panel::Side,
}

pub struct CollapsibleSidePanelResponse<R> {
    pub header: Response,
    pub full_rect: Rect,
    pub panel: Option<InnerResponse<R>>,
}

#[allow(unused)]
impl CollapsibleSidePanel {
    pub fn new(id: impl Into<Id>, text: impl Into<WidgetText>) -> Self {
        Self {
            id: id.into(),
            text: text.into(),
            active: true,
            panel_transformer: None,
            header_offset: 10.0,
            side: panel::Side::Left,
        }
    }

    pub fn active(self, active: bool) -> Self {
        Self { active, ..self }
    }

    pub fn panel_transformer(
        self,
        panel_transformer: Option<Box<dyn Fn(SidePanel) -> SidePanel>>,
    ) -> Self {
        Self {
            panel_transformer,
            ..self
        }
    }

    pub fn header_offset(self, header_offset: f32) -> Self {
        Self {
            header_offset,
            ..self
        }
    }

    pub fn side(self, side: panel::Side) -> Self {
        Self { side, ..self }
    }

    pub fn show<R>(
        self,
        ui: &mut Ui,
        add_content: impl FnOnce(&mut Ui) -> R,
    ) -> CollapsibleSidePanelResponse<R> {
        let CollapsibleSidePanel {
            id,
            text,
            active,
            panel_transformer,
            header_offset,
            side,
        } = self;

        ui.push_id(id, |ui| {
            let open_id = ui.id().with("_collapsed");
            let open = ui.ctx().data(|mem| mem.get_temp(open_id).unwrap_or(true));

            let panel_id = ui.id().with("_panel");
            let animation = ui
                .ctx()
                .animate_bool(ui.id().with("_open-anim"), open && self.active);

            let expanded_width = PanelState::load(ui.ctx(), panel_id)
                .map(|state| state.rect.width())
                .unwrap_or_default();

            let offset = if animation > 0.0 {
                expanded_width * animation
            } else {
                0.0
            };

            let painter = ui.painter();
            let header_text = text
                .fallback_text_style(TextStyle::Monospace)
                .into_galley(
                    ui,
                    Some(false),
                    f32::INFINITY,
                    FontSelection::Style(TextStyle::Monospace),
                )
                .galley;

            let size = vec2(
                header_text.rect.height() + 10.0,
                header_text.rect.width() + header_text.rect.height() + 20.0,
            );
            let pos = match side {
                panel::Side::Left => pos2(offset, header_offset),
                panel::Side::Right => pos2(ui.max_rect().width() - size.x - offset, header_offset),
            };

            let text_color =
                ui.style()
                    .visuals
                    .text_color()
                    .gamma_multiply(if active { 1.0 } else { 0.6 });
            let arrow_rect = Rect::from_center_size(
                pos + vec2(size.x / 2.0, size.x / 2.0),
                vec2(size.x / 3.0, size.x / 3.0),
            );

            let mut points = vec![
                arrow_rect.right_top(),
                arrow_rect.left_center(),
                arrow_rect.right_bottom(),
            ];
            let rotation = match side {
                panel::Side::Left => Rot2::from_angle((1.0 - animation) * TAU * 0.5),
                panel::Side::Right => Rot2::from_angle(animation * TAU * 0.5),
            };
            for p in &mut points {
                *p = arrow_rect.center() + rotation * (*p - arrow_rect.center());
            }

            let rect = Rect::from_min_size(pos, size);

            let rounding = match side {
                panel::Side::Left => Rounding {
                    ne: 5.0,
                    se: 5.0,
                    ..Default::default()
                },
                panel::Side::Right => Rounding {
                    nw: 5.0,
                    sw: 5.0,
                    ..Default::default()
                },
            };

            painter.rect(
                rect,
                rounding,
                ui.style().visuals.panel_fill,
                ui.style().visuals.window_stroke,
            );

            painter.add(PathShape {
                points,
                closed: true,
                fill: text_color,
                stroke: Stroke::NONE,
            });

            let (text_pos, text_angle) = match side {
                panel::Side::Left => (vec2(size.x - 5.0, size.x), TAU * 0.25),
                panel::Side::Right => (vec2(5.0, size.y - 10.0), TAU * 0.75),
            };

            painter.add(TextShape {
                pos: pos + text_pos,
                galley: header_text,
                underline: Stroke::NONE,
                override_text_color: Some(text_color),
                angle: text_angle,
            });

            let response = ui.interact(rect, ui.id().with("_header"), Sense::click_and_drag());
            if response.clicked() {
                ui.ctx().data_mut(|data| {
                    let open = data.get_temp_mut_or(open_id, true);
                    *open = !*open;
                })
            }

            let clip = ui.clip_rect();
            let full_rect = match side {
                panel::Side::Left => Rect::from_min_size(pos2(0.0, 0.0), vec2(rect.right(), clip.height())),
                panel::Side::Right => Rect::from_min_size(pos2(rect.left(), 0.0), vec2(clip.width() - rect.left(), clip.height())),
            };

            let panel = if animation > 0.0 {
                let panel_offset = match side {
                    panel::Side::Left => (1.0 - animation) * -expanded_width,
                    panel::Side::Right => (1.0 - animation) * expanded_width,
                };

                let rect = Rect::from_min_size(clip.min + vec2(panel_offset, 0.0), clip.size());

                let mut panel_ui = ui.child_ui(rect, *ui.layout());

                let p = SidePanel::new(side, panel_id).resizable(false);
                let p = match panel_transformer {
                    Some(pt) => pt(p),
                    None => p,
                };
                Some(p.show_inside(&mut panel_ui, add_content))
            } else {
                None
            };

            CollapsibleSidePanelResponse {
                header: response,
                full_rect,
                panel,
            }
        })
        .inner
    }
}
