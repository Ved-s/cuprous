use std::{any::TypeId, collections::HashSet, hash::Hash, ops::Deref, sync::Arc};

use eframe::{
    egui::{
        CollapsingHeader, CursorIcon, DragValue, FontSelection, Grid, Id, Key,
        Label, Margin, PointerButton, Response, Sense, TextStyle, Ui, Widget,
        WidgetInfo, WidgetText, WidgetType,
    },
    epaint::{Color32, Rounding, Stroke, TextShape},
};
use emath::{vec2, Rect, Vec2};
use serde::{Deserialize, Serialize};

pub mod designer;
pub mod drawing;
pub mod editor;
pub mod selection;
pub mod side_panel;

use crate::{
    circuits::props::{CircuitProperty, CircuitPropertyImpl, CircuitPropertyStore},
    DynStaticStr, PaintContext, RwLock,
};

pub enum InventoryItemGroup<I> {
    SingleItem(Box<dyn InventoryItem<I>>),
    Group(Vec<Box<dyn InventoryItem<I>>>),
}

pub trait InventoryItem<I> {
    fn id(&self) -> I;
    fn draw(&self, ctx: &PaintContext);
}

pub struct Inventory<'a, I>
where
    I: Eq + Clone + Hash + Send + Sync + 'static,
{
    selected: &'a mut Option<I>,
    groups: &'a [InventoryItemGroup<I>],
    item_size: Vec2,
    item_margin: Margin,
    margin: Margin,
}

#[derive(Clone)]
struct InventoryMemory<I> {
    last_group_items: Arc<RwLock<HashSet<I>>>,
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

impl<'a, I> Inventory<'a, I>
where
    I: Eq + Clone + Hash + Send + Sync + 'static,
{
    pub fn new(selected: &'a mut Option<I>, groups: &'a [InventoryItemGroup<I>]) -> Self {
        Self {
            selected,
            groups,
            item_size: vec2(28.0, 28.0),
            item_margin: Margin::same(6.0),
            margin: Margin::default(),
        }
    }

    fn get_group_icon_item_id(
        group: &[Box<dyn InventoryItem<I>>],
        ui: &Ui,
        self_id: Id,
    ) -> Option<usize> {
        ui.memory(|mem| {
            mem.data
                .get_temp::<InventoryMemory<I>>(self_id)
                .and_then(|mem| {
                    let last_group_items = mem.last_group_items.read();
                    for (i, item) in group.iter().enumerate() {
                        if last_group_items.contains(&item.id()) {
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
        if ui.ctx().wants_keyboard_input() {
            return (current_index, current_sub_index);
        }

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
                    *self.selected = Some(item.id());
                    return (current_index, Some(index));
                }
            }
        } else if current_index == Some(index) {
            *self.selected = None;
            return (None, None);
        } else if let Some(item) = self.groups.get(index) {
            match item {
                InventoryItemGroup::SingleItem(item) => {
                    *self.selected = Some(item.id());
                    return (Some(index), None);
                }
                InventoryItemGroup::Group(group) => {
                    let item_index =
                        Inventory::get_group_icon_item_id(group, ui, ui.next_auto_id())
                            .unwrap_or(0);

                    if let Some(item) = group.get(item_index) {
                        *self.selected = Some(item.id());
                        return (Some(index), Some(item_index));
                    }
                }
            }
        }
        (current_index, current_sub_index)
    }

    fn ui(
        mut self,
        ui: &mut eframe::egui::Ui,
        name_resolver: impl Fn(&I) -> Option<DynStaticStr>,
    ) -> eframe::egui::Response {
        let (selected_item, selected_sub_item) = match &self.selected {
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

        let mut response = ui.interact(rect, id, Sense::click());

        let paint = ui.painter_at(rect);
        let rounding = Rounding::same(3.0);
        paint.rect(
            rect,
            rounding,
            Color32::from_gray(32),
            Stroke::new(1.0, Color32::from_gray(100)),
        );

        if let Some(selected) = &self.selected {
            let name = name_resolver(selected);

            if let Some(name) = name {
                let galley = WidgetText::from(name.deref())
                    .fallback_text_style(TextStyle::Monospace)
                    .into_galley(
                        ui,
                        Some(false),
                        f32::INFINITY,
                        FontSelection::Style(TextStyle::Monospace),
                    )
                    .galley;
                let inv_width = rect.width();
                let size = galley.rect.size() + vec2(12.0, 6.0);
                let width_diff = size.x - (inv_width - 3.0);

                let rect = Rect::from_min_size(rect.left_bottom() + vec2(3.0, 0.0), size);
                ui.allocate_rect(rect, Sense::hover());
                let paint = ui.painter();
                paint.rect(
                    rect,
                    Rounding {
                        nw: 0.0,
                        ne: width_diff.clamp(0.0, 3.0),
                        sw: 3.0,
                        se: 3.0,
                    },
                    ui.style().visuals.panel_fill,
                    ui.style().visuals.window_stroke,
                );

                paint.add(TextShape {
                    pos: rect.min + vec2(6.0, 3.0),
                    galley,
                    underline: Stroke::NONE,
                    override_text_color: Some(ui.style().visuals.text_color()),
                    angle: 0.0,
                });
            }
        }

        let paint_ctx = PaintContext::new_on_ui(ui, rect, 1.0);

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

                            *self.selected = match &self.selected {
                                Some(sel) => {
                                    if *sel == id {
                                        None
                                    } else {
                                        Some(id)
                                    }
                                }
                                None => Some(id),
                            };
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
                                *self.selected = match &self.selected {
                                    Some(sel) => {
                                        if *sel == id {
                                            None
                                        } else {
                                            Some(id)
                                        }
                                    }
                                    None => Some(id),
                                };
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
                        *self.selected = match &self.selected {
                            Some(sel) => {
                                if *sel == id {
                                    None
                                } else {
                                    Some(id)
                                }
                            }
                            None => Some(id),
                        };
                    }
                    let rect = apply_margin_to_rect(rect, self.item_margin);
                    let item_ctx = paint_ctx.with_rect(rect);
                    item.draw(&item_ctx);
                    x += full_item_size.x;
                }
            }
        }

        if selected_changed {
            response.mark_changed();
            if let Some(selected) = &self.selected {
                if let Some(group) = selected_group {
                    ui.memory_mut(|mem| {
                        let m =
                            mem.data
                                .get_temp_mut_or_insert_with::<InventoryMemory<I>>(id, || {
                                    InventoryMemory {
                                        last_group_items: Default::default(),
                                    }
                                });
                        let mut last_group_items = m.last_group_items.write();
                        for item in group {
                            last_group_items.remove(&item.id());
                        }
                        last_group_items.insert(selected.clone());
                    });
                }
            }
        }

        response
    }
}

pub struct PaintableInventoryItem<I, F>
where
    I: Clone,
    F: Fn(&PaintContext),
{
    id: I,
    painter: F,
}

impl<I, F> PaintableInventoryItem<I, F>
where
    I: Clone,
    F: Fn(&PaintContext),
{
    pub fn new(id: I, painter: F) -> Self {
        Self { id, painter }
    }
}

impl<I, F> InventoryItem<I> for PaintableInventoryItem<I, F>
where
    I: Clone,
    F: Fn(&PaintContext),
{
    fn id(&self) -> I {
        self.id.clone()
    }

    fn draw(&self, ctx: &PaintContext) {
        (self.painter)(ctx);
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

    pub new: Box<dyn CircuitPropertyImpl>,
    /// (T, old)
    pub affected_values: Vec<(T, Box<dyn CircuitPropertyImpl>)>,
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
                    self.ids_cache.remove(str);
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
                    vec.push((id, old));
                    for other_prop in props {
                        if !other_prop.1.imp().equals(prop.imp()) {
                            let old = other_prop.1.imp().clone();
                            prop.imp().copy_into(other_prop.1.imp_mut());
                            vec.push((other_prop.0, old));
                        }
                    }
                    changes.push(ChangedProperty {
                        id: prop.id(),
                        new: prop.imp().clone(),
                        affected_values: vec,
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

#[must_use = "You should put this widget in an ui with `ui.add(widget);`"]
pub struct DoubleSelectableLabel {
    selected: bool,
    outline: bool,
    text: WidgetText,
    hover_color: Color32,
    fill_color: Option<Color32>,
    outline_stroke: Stroke,
}

impl DoubleSelectableLabel {
    pub fn new(
        selected: bool,
        outline: bool,
        text: impl Into<WidgetText>,
        hover_color: Color32,
        fill_color: Option<Color32>,
        outline_stroke: Stroke,
    ) -> Self {
        Self {
            selected,
            outline,
            text: text.into(),
            hover_color,
            fill_color,
            outline_stroke,
        }
    }
}

impl Widget for DoubleSelectableLabel {
    fn ui(self, ui: &mut Ui) -> Response {
        let Self {
            selected,
            text,
            outline,
            hover_color,
            fill_color,
            outline_stroke,
        } = self;

        let button_padding = ui.spacing().button_padding;
        let total_extra = button_padding + button_padding;

        let wrap_width = ui.available_width() - total_extra.x;
        let text = text.into_galley(ui, None, wrap_width, TextStyle::Button);

        let mut desired_size = total_extra + text.size();
        desired_size.y = desired_size.y.max(ui.spacing().interact_size.y);
        let (rect, response) = ui.allocate_at_least(desired_size, Sense::click());
        response.widget_info(|| {
            WidgetInfo::selected(WidgetType::SelectableLabel, selected, text.text())
        });

        if ui.is_rect_visible(response.rect) {
            let text_pos = ui
                .layout()
                .align_size_within_rect(text.size(), rect.shrink2(button_padding))
                .min;

            let visuals = ui.style().interact_selectable(&response, selected);

            let hover = response.hovered() || response.highlighted() || response.has_focus();

            let fill = if selected {
                fill_color.unwrap_or(visuals.weak_bg_fill)
            } else if hover {
                hover_color
            } else {
                Color32::TRANSPARENT
            };

            let stroke = if outline {
                outline_stroke
            } else {
                Stroke::NONE
            };

            if fill != Color32::default() || stroke != Stroke::default() {
                let rect = rect.expand(visuals.expansion);

                ui.painter().rect(rect, visuals.rounding, fill, stroke);
            }

            text.paint_with_visuals(ui.painter(), text_pos, &visuals);
        }

        response
    }
}

#[derive(Default, Debug, Clone, Copy)]
pub struct Sides {
    pub top: bool,
    pub left: bool,
    pub right: bool,
    pub bottom: bool,
    pub center: bool,
}

impl Sides {
    pub const ALL: Self = Self {
        top: true,
        left: true,
        right: true,
        bottom: true,
        center: true,
    };

    pub const CENTER: Self = Self {
        top: false,
        left: false,
        right: false,
        bottom: false,
        center: true,
    };

    pub fn any(self) -> bool {
        self.top || self.left || self.right || self.bottom || self.center
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct RectVisuals {
    pub rounding: Rounding,
    pub fill: Color32,
    pub stroke: Stroke,
}
impl RectVisuals {
    fn scaled_by(self, scale: f32) -> Self {
        Self {
            rounding: Rounding {
                nw: self.rounding.nw * scale,
                ne: self.rounding.ne * scale,
                sw: self.rounding.sw * scale,
                se: self.rounding.se * scale,
            },
            fill: self.fill,
            stroke: Stroke {
                width: self.stroke.width * scale,
                color: self.stroke.color,
            },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum DragState {
    None,
    Started(Sides),
    Dragging(Sides),
    Ended(Sides),
}

pub fn rect_editor(
    rect: &mut Rect,
    sides: Sides,
    ui: &Ui,
    id: Id,
    constrain: impl FnOnce(Sides, &mut Rect),
    visuals: RectVisuals,
) -> DragState {
    custom_rect_editor(rect, sides, ui, id, constrain, |rect, _, ui| {
        let paint = ui.painter();
        paint.rect(rect, visuals.rounding, visuals.fill, visuals.stroke);
    })
}

pub fn custom_rect_editor(
    rect: &mut Rect,
    sides: Sides,
    ui: &Ui,
    id: Id,
    constrain: impl FnOnce(Sides, &mut Rect),
    painter: impl FnOnce(Rect, Sides, &Ui),
) -> DragState {
    #[derive(Default, Clone, Copy)]
    struct State {
        pos: Vec2,
        sides: Sides,
        drag: bool,
    }

    fn rect_editor_logic(
        rect: &mut Rect,
        sides: Sides,
        ui: &Ui,
        id: Id,
        grab: f32,
    ) -> (State, bool) {
        let editing = ui.memory(|mem| mem.is_being_dragged(id));

        let state = if editing {
            ui.memory(|mem| mem.data.get_temp(id).unwrap_or_default())
        } else if let Some(pointer) = ui.ctx().pointer_latest_pos() {
            let on_top = ui
                .ctx()
                .layer_id_at(pointer)
                .map_or(true, |top_layer_id| top_layer_id == ui.layer_id());

            let draggable = rect.expand(grab).contains(pointer);

            if draggable {
                let right = sides.right && on_top && rect.right() - grab <= pointer.x;
                let bottom = sides.bottom && on_top && rect.bottom() - grab <= pointer.y;

                let top = !bottom && sides.top && on_top && pointer.y <= rect.top() + grab;
                let left = !right && sides.left && on_top && pointer.x <= rect.left() + grab;

                State {
                    sides: Sides {
                        top,
                        left,
                        right,
                        bottom,
                        center: sides.center && !top && !left && !right && !bottom,
                    },
                    pos: pointer - rect.left_top(),
                    drag: false,
                }
            } else {
                State::default()
            }
        } else {
            State::default()
        };

        if ui.input(|i| i.pointer.primary_pressed()) && state.sides.any() {
            ui.memory_mut(|mem| {
                mem.set_dragged_id(id);
                mem.data.insert_temp(
                    id,
                    State {
                        drag: true,
                        ..state
                    },
                );
            });
        }

        let any_drag = ui.memory(|mem| mem.is_anything_being_dragged());
        let editing = ui.memory(|mem| mem.is_being_dragged(id));

        if state.drag && !editing {
            ui.memory_mut(|mem| {
                mem.data.insert_temp(id, State::default());
            });
        }

        let state = if state.sides.any() && any_drag && !editing {
            Default::default()
        } else {
            state
        };

        let sides = state.sides;

        if sides.top && sides.left {
            ui.ctx().set_cursor_icon(CursorIcon::ResizeNorthWest);
        } else if sides.top && sides.right {
            ui.ctx().set_cursor_icon(CursorIcon::ResizeNorthEast);
        } else if sides.bottom && sides.left {
            ui.ctx().set_cursor_icon(CursorIcon::ResizeSouthWest);
        } else if sides.bottom && sides.right {
            ui.ctx().set_cursor_icon(CursorIcon::ResizeSouthEast);
        } else if sides.top {
            ui.ctx().set_cursor_icon(CursorIcon::ResizeNorth);
        } else if sides.left {
            ui.ctx().set_cursor_icon(CursorIcon::ResizeWest);
        } else if sides.right {
            ui.ctx().set_cursor_icon(CursorIcon::ResizeEast);
        } else if sides.bottom {
            ui.ctx().set_cursor_icon(CursorIcon::ResizeSouth);
        } else if sides.center {
            ui.ctx().set_cursor_icon(CursorIcon::Move);
        }

        if editing {
            if let Some(pointer) = ui.ctx().pointer_latest_pos() {
                if state.sides.center {
                    let size = rect.size();
                    *rect = Rect::from_min_size(pointer - state.pos, size);
                }

                if state.sides.top {
                    rect.min.y = pointer.y.min(rect.max.y);
                }
                if state.sides.left {
                    rect.min.x = pointer.x.min(rect.max.x);
                }
                if state.sides.right {
                    rect.max.x = pointer.x.max(rect.min.x);
                }
                if state.sides.bottom {
                    rect.max.y = pointer.y.max(rect.min.y);
                }
            }
        }

        (state, editing)
    }

    fn rect_editor_visuals(rect: Rect, ui: &Ui, id: Id, sides: Sides, grab: f32) {
        let hover_rounding = Rounding::same(grab / 2.0);
        let hover_color = Color32::WHITE.linear_multiply(0.3);
        let paint = ui.painter();

        let top = ui.ctx().animate_bool(id.with("__hover_top"), sides.top);
        if top > 0.0 {
            paint.rect(
                Rect::from_min_size(
                    rect.left_top() - vec2(grab, grab),
                    vec2(rect.width() + grab * 2.0, grab * 2.0),
                ),
                hover_rounding,
                hover_color.linear_multiply(top),
                Stroke::NONE,
            )
        }

        let left = ui.ctx().animate_bool(id.with("__hover_left"), sides.left);
        if left > 0.0 {
            paint.rect(
                Rect::from_min_size(
                    rect.left_top() - vec2(grab, grab),
                    vec2(grab * 2.0, rect.height() + grab * 2.0),
                ),
                hover_rounding,
                hover_color.linear_multiply(left),
                Stroke::NONE,
            )
        }

        let bottom = ui
            .ctx()
            .animate_bool(id.with("__hover_bottom"), sides.bottom);
        if bottom > 0.0 {
            paint.rect(
                Rect::from_min_size(
                    rect.left_bottom() - vec2(grab, grab),
                    vec2(rect.width() + grab * 2.0, grab * 2.0),
                ),
                hover_rounding,
                hover_color.linear_multiply(bottom),
                Stroke::NONE,
            )
        }

        let right = ui.ctx().animate_bool(id.with("__hover_right"), sides.right);
        if right > 0.0 {
            paint.rect(
                Rect::from_min_size(
                    rect.right_top() - vec2(grab, grab),
                    vec2(grab * 2.0, rect.height() + grab * 2.0),
                ),
                hover_rounding,
                hover_color.linear_multiply(right),
                Stroke::NONE,
            )
        }

        let center = ui
            .ctx()
            .animate_bool(id.with("__hover_center"), sides.center);
        if center > 0.0 {
            paint.rect(
                rect.expand(2.0),
                Rounding::ZERO,
                Color32::TRANSPARENT,
                Stroke::new(2.0, hover_color.linear_multiply(center)),
            )
        }
    }

    let grab = ui.style().interaction.resize_grab_radius_side;

    let (state, editing) = rect_editor_logic(rect, sides, ui, id, grab);

    if editing {
        constrain(state.sides, rect);
    }
    painter(*rect, state.sides, ui);

    rect_editor_visuals(*rect, ui, id, state.sides, grab);

    match (state.drag, editing) {
        (true, true) => DragState::Dragging(state.sides),
        (true, false) => DragState::Ended(state.sides),
        (false, true) => DragState::Started(state.sides),
        (false, false) => DragState::None,
    }
}

#[derive(Clone, Copy)]
pub enum RectProperty {
    Top,
    Left,
    Width,
    Height,
}

impl RectProperty {
    pub const ALL: [Self; 4] = [Self::Top, Self::Left, Self::Width, Self::Height];
}

#[derive(Default, Clone, Copy)]
pub struct RectPropertyChanges {
    pub top: bool,
    pub left: bool,
    pub width: bool,
    pub height: bool,
}

impl RectPropertyChanges {
    pub fn set_prop(&mut self, prop: RectProperty, value: bool) {
        match prop {
            RectProperty::Top => self.top = value,
            RectProperty::Left => self.left = value,
            RectProperty::Width => self.width = value,
            RectProperty::Height => self.height = value,
        }
    }

    pub fn get_prop(&self, prop: RectProperty) -> bool {
        match prop {
            RectProperty::Top => self.top,
            RectProperty::Left => self.left,
            RectProperty::Width => self.width,
            RectProperty::Height => self.height,
        }
    }

    pub fn any(self) -> bool {
        self.top || self.left || self.width || self.height
    }

    pub fn iter(self) -> impl Iterator<Item = RectProperty> {
        RectProperty::ALL.into_iter().filter(move |prop| self.get_prop(*prop))
    }
}

pub fn rect_properties_editor(
    rect: &mut Rect,
    ui: &mut Ui,
    id: impl Into<Id>,
    text: impl Into<WidgetText>,
    constrain: impl Fn(RectProperty, &mut Rect),
) -> RectPropertyChanges {
    let id = id.into();
    let resp = CollapsingHeader::new(text)
        .id_source((id, "ch"))
        .show(ui, |ui| {
            Grid::new((id, "grid")).show(ui, |ui| {
                let mut changes = RectPropertyChanges::default();
                for prop in RectProperty::ALL {
                    let (mut value, name) = match prop {
                        RectProperty::Top => (rect.top(), "Top"),
                        RectProperty::Left => (rect.left(), "Left"),
                        RectProperty::Width => (rect.width(), "Width"),
                        RectProperty::Height => (rect.height(), "Height"),
                    };
                    ui.label(name);
                    let resp = DragValue::new(&mut value).speed(0.05).ui(ui);
                    if resp.changed() {
                        match prop {
                            RectProperty::Top => { let height = rect.height(); rect.set_top(value); rect.set_height(height); },
                            RectProperty::Left => { let width = rect.width(); rect.set_left(value); rect.set_width(width); },
                            RectProperty::Width => rect.set_width(value),
                            RectProperty::Height => rect.set_height(value),
                        }
                        constrain(prop, rect);
                        changes.set_prop(prop, true);
                    }
                    ui.end_row();
                }
                changes
            })
        });

    resp.body_returned
        .map(|resp| resp.inner)
        .unwrap_or_default()
}

#[macro_export]
macro_rules! __count {
    () => (0usize);
    ( $x:tt $($xs:tt)* ) => (1usize + $crate::__count!($($xs)*));
}

// https://gist.github.com/RocketPrinter/d292d1c7a5a6066139f8daf1901a0048
/// ONE MACRO TO RULE ALL THE DUMB TRICKS
#[macro_export]
macro_rules! evenly_spaced_out {
    ($ui:ident, horizontal, $(|$item_ui:ident|$item: tt,)+) => {
        $ui.horizontal(|ui|{
            const SIZE: usize = $crate::__count!($($item_ui)*);

            let id = ui.id().with("_ultimate_centerer");

            let mut width_arr = ui.data(|data|data.get_temp::<[f32;SIZE]>(id).unwrap_or_default());
            let expected_space = (ui.available_width() - width_arr.iter().sum::<f32>()) / (SIZE + 1) as f32;

            let mut width_changed = false;
            // != 0 if the previous element has changed width
            let mut last_width_change = 0.;

            let mut i = 0;
            $(
                // if the previous item was longer or shorter than it needs to be we account for that so the rest of the items don't shift
                ui.add_space((expected_space -  last_width_change).max(0.));

                let new_width = ui.scope(|$item_ui| $item).response.rect.width();

                #[allow(unused_assignments)]{last_width_change = new_width - width_arr[i];}

                if width_arr[i] != new_width {
                    width_changed = true;
                    width_arr[i] = new_width;
                }

                #[allow(unused_assignments)]{i+=1;}
            )+

            ui.data_mut(|data|data.insert_temp(id, width_arr));
            if width_changed {
                ui.ctx().request_repaint();
            }
        });

    };
    ($ui:ident, vertical, $($item: tt,)+) => {
        // to implement when needed
    };
}