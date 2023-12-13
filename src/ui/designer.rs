use std::{
    f32::consts::TAU,
    ops::{Deref, DerefMut},
    sync::Arc,
};

use eframe::{
    egui::{
        CollapsingHeader, ComboBox, Frame, Grid, Key, Label, Margin, PointerButton,
        ScrollArea, Sense, Slider, TextEdit, TextStyle, Ui, Widget,
    },
    epaint::{Color32, FontId, Rounding, Shape, Stroke},
};
use emath::{pos2, vec2, Align2, Pos2, Rangef, Rect};

use crate::{
    board::{
        CircuitDesign, CircuitDesignControl, CircuitDesignPin, CircuitDesignStorage, Decoration,
        DecorationType,
    },
    circuits::InternalPinDirection,
    ext::{IteratorEqExt, IteratorExt},
    state::WireState,
    vector::{Vec2f, Vec2u},
    Direction4, DynStaticStr, PaintContext, PanAndZoom, RwLock,
};

use super::{
    custom_rect_editor,
    drawing::{self, align_rect_scaled},
    rect_editor, rect_properties_editor,
    selection::{
        selection_border_color, selection_fill_color, Selection, SelectionImpl,
        SelectionInventoryItem,
    },
    side_panel::{PanelSide, SidePanel},
    DragState, Inventory, InventoryItemGroup, PaintableInventoryItem,
    RectProperty, RectPropertyChanges, RectVisuals, Sides,
};

pub struct DesignerResponse {
    pub close: bool,
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum SelectedItemId {
    Selection,
    Pin(DynStaticStr),
    Control(usize, usize),
    Rect,
}

#[derive(PartialEq, Eq, Hash)]
enum SelectedDesignObject {
    Decoration(usize),
    Pin(usize),
    Control(usize, usize),
}

#[derive(PartialEq, Eq)]
enum SelectedDesignObjectType {
    Decoration(DecorationType),
    Pin,
    Control,
}

pub struct CircuitDesignPinInfo {
    pub dir: InternalPinDirection,
    pub display_dir: Option<Direction4>,
    pub display_name: DynStaticStr,
}

pub struct CircuitDesignControlInfo {
    pub rect: Rect,
    pub display_name: DynStaticStr,
}

pub struct ControlProvider {
    pub id: usize,
    pub count: usize,
}

pub trait DesignProvider {
    fn get_storage(&self) -> Arc<RwLock<CircuitDesignStorage>>;

    fn get_pin_ids(&self) -> Vec<DynStaticStr>;
    fn get_pin(&self, id: &DynStaticStr) -> Option<CircuitDesignPinInfo>;

    fn get_control_provider_data(&self) -> Vec<ControlProvider>;
    fn get_control_info(&self, provider: usize, id: usize) -> Option<CircuitDesignControlInfo>;
    fn paint_control(&self, provider: usize, id: usize, ctx: &PaintContext);

    fn has_custom_config(&self) -> bool {
        false
    }
    fn custom_config_ui(&self, ui: &mut Ui) {
        let _ = ui;
    }
}

#[derive(Default)]
struct DesignerSelectionImpl {}

impl SelectionImpl for DesignerSelectionImpl {
    type Pass = CircuitDesign;
    type Object = SelectedDesignObject;

    fn collect_changes(
        &mut self,
        pass: &CircuitDesign,
        changes: &mut std::collections::HashSet<SelectedDesignObject>,
        rect: Rect,
    ) {
        for (i, deco) in pass.decorations.iter().enumerate() {
            match deco {
                Decoration::Rect { rect: r, visuals } => {
                    if rect.expand(visuals.stroke.width * 0.5).intersects(*r) {
                        changes.insert(SelectedDesignObject::Decoration(i));
                    }
                }
            }
        }
        for (i, pin) in pass.pins.iter().enumerate() {
            let center = pin.pos.convert(|v| v as f32 + 0.5);
            let pin_rect = Rect::from_center_size(center.into(), vec2(0.5, 0.5));
            if pin_rect.intersects(rect) {
                changes.insert(SelectedDesignObject::Pin(i));
            }
        }

        for ((provider, id), control) in pass.controls.iter() {
            if control.rect.intersects(rect) {
                changes.insert(SelectedDesignObject::Control(*provider, *id));
            }
        }
    }

    fn draw_object_selection(
        &mut self,
        pass: &CircuitDesign,
        object: &SelectedDesignObject,
        ctx: &crate::PaintContext,
        shapes: &mut Vec<Shape>,
    ) {
        match object {
            SelectedDesignObject::Decoration(deco) => {
                if let Some(deco) = pass.decorations.get(*deco) {
                    let rect = match deco {
                        Decoration::Rect { rect, visuals } => {
                            rect.expand(visuals.stroke.width * 0.5)
                        }
                    };

                    let rect = ctx.screen.world_to_screen_rect(rect).expand(2.0);
                    shapes.push(Shape::rect_filled(
                        rect,
                        Rounding::ZERO,
                        selection_fill_color(),
                    ));
                    ctx.paint.rect_stroke(
                        rect,
                        Rounding::ZERO,
                        Stroke::new(2.0, selection_border_color()),
                    );
                }
            }
            SelectedDesignObject::Pin(id) => {
                if let Some(pin) = pass.pins.get(*id) {
                    let center = pin.pos.convert(|v| v as f32 + 0.5);
                    let pin_rect = Rect::from_center_size(center.into(), vec2(0.5, 0.5));
                    let scr_rect = ctx.screen.world_to_screen_rect(pin_rect);

                    shapes.push(Shape::rect_filled(
                        scr_rect,
                        Rounding::ZERO,
                        selection_fill_color(),
                    ));
                    ctx.paint.rect_stroke(
                        scr_rect,
                        Rounding::ZERO,
                        Stroke::new(2.0, selection_border_color()),
                    );
                }
            }
            SelectedDesignObject::Control(provider, control) => {
                if let Some(info) = pass.controls.get(&(*provider, *control)) {
                    let scr_rect = ctx.screen.world_to_screen_rect(info.rect).expand(1.0);
                    shapes.push(Shape::rect_filled(
                        scr_rect,
                        Rounding::ZERO,
                        selection_fill_color(),
                    ));
                    ctx.paint.rect_stroke(
                        scr_rect,
                        Rounding::ZERO,
                        Stroke::new(2.0, selection_border_color()),
                    );
                }
            }
        }
    }
}

pub struct Designer {
    storage: Arc<RwLock<CircuitDesignStorage>>,
    provider: Box<dyn DesignProvider>,
    pan_zoom: PanAndZoom,
    selected_id: Option<SelectedItemId>,

    inventory: Box<[InventoryItemGroup<SelectedItemId>]>,
    selection: Selection<DesignerSelectionImpl>,

    selected_pin_dir: Option<Direction4>,
    default_rect_visuals: Arc<RwLock<RectVisuals>>,
    rect_drag_start: Option<Vec2f>,
}

impl Designer {
    pub fn new(provider: Box<dyn DesignProvider>) -> Self {
        let storage = provider.get_storage();

        let size = storage.read().current().size;
        let pan_zoom = PanAndZoom::new(size.convert(|v| v as f32) / 2.0, 16.0);

        let rect_visuals = Arc::new(RwLock::new(RectVisuals {
            rounding: Rounding::ZERO,
            fill: Color32::WHITE,
            stroke: Stroke::new(0.1, Color32::BLACK),
        }));

        let rect_visuals_clone = rect_visuals.clone();
        let rect_painter = move |ctx: &PaintContext| {
            let rect_visuals = rect_visuals_clone.read().scaled_by(ctx.rect.width() * 0.20);
            let rect = ctx.rect.shrink2(ctx.rect.size() / 5.0);

            ctx.paint.rect(
                rect,
                rect_visuals.rounding,
                rect_visuals.fill,
                rect_visuals.stroke,
            );
        };

        Self {
            storage,
            provider,
            pan_zoom,
            selected_id: None,
            inventory: vec![
                InventoryItemGroup::SingleItem(Box::new(SelectionInventoryItem::new(
                    SelectedItemId::Selection,
                ))),
                InventoryItemGroup::SingleItem(Box::new(PaintableInventoryItem::new(
                    SelectedItemId::Rect,
                    rect_painter,
                ))),
            ]
            .into_boxed_slice(),
            selection: Default::default(),
            selected_pin_dir: None,
            default_rect_visuals: rect_visuals,
            rect_drag_start: None,
        }
    }

    pub fn background_update(&mut self, ui: &mut Ui) {
        let rect = ui.max_rect();

        self.pan_zoom.update(ui, rect, self.selected_id.is_none());

        let designs = self.storage.clone();
        let mut designs = designs.write();
        let design = designs.current_mut();

        #[allow(clippy::collapsible_if)]
        if !ui.ctx().wants_keyboard_input() {
            if ui.input(|input| input.key_pressed(Key::R)) {
                if self.selection.selection.is_empty() {
                    if let Some(dir) = &mut self.selected_pin_dir {
                        *dir = dir.rotate_clockwise();
                    }
                } else {
                    for object in self.selection.iter() {
                        if let SelectedDesignObject::Pin(id) = object {
                            if let Some(pin) = design.pins.get_mut(*id) {
                                pin.display_dir = pin.display_dir.map(|d| d.rotate_clockwise());
                            }
                        }
                    }
                }
            }

            if ui.input(|input| input.key_pressed(Key::Delete)) {
                for i in (0..design.pins.len()).rev() {
                    if self.selection.contains(&SelectedDesignObject::Pin(i)) {
                        design.pins.remove(i);
                    }
                }

                for i in (0..design.decorations.len()).rev() {
                    if self
                        .selection
                        .contains(&SelectedDesignObject::Decoration(i))
                    {
                        design.decorations.remove(i);
                    }
                }

                for object in self.selection.iter() {
                    if let SelectedDesignObject::Control(provider, control) = *object {
                        design.controls.remove(&(provider, control));
                    }
                }

                self.selection.clear();
            }
        }

        let screen = self.pan_zoom.to_screen(rect);
        let paint = ui.painter_at(rect);
        drawing::draw_grid(
            screen.wld_pos,
            screen.scale.into(),
            0.into(),
            screen.scr_rect,
            &paint,
        );

        let ctx = PaintContext {
            screen,
            paint: &paint,
            rect,
            ui,
        };

        let world_font = FontId::new(screen.scale * 0.5, eframe::epaint::FontFamily::Monospace);

        let color = Color32::from_rgb(134, 114, 135);

        let mut size_rect = Rect::from_min_size(
            screen.world_to_screen(0.0.into()).into(),
            (design.size.convert(|v| v as f32) * screen.scale).into(),
        );

        if self.selected_id.is_none() {
            let state = super::rect_editor(
                &mut size_rect,
                Sides {
                    top: false,
                    left: false,
                    right: true,
                    bottom: true,
                    center: false,
                },
                ui,
                ui.id().with("circuit_resize"),
                |_, rect| {
                    rect.min = screen.world_to_screen(0.0.into()).into();
                },
                RectVisuals {
                    rounding: Rounding::ZERO,
                    fill: color.linear_multiply(0.3),
                    stroke: Stroke::new(2.0, color),
                },
            );

            if !matches!(state, DragState::None) {
                design.size = (Vec2f::from(size_rect.size()) / screen.scale)
                    .convert(|v| (v.round() as u32).max(1))
            }
        } else {
            paint.rect(
                size_rect,
                Rounding::ZERO,
                color.linear_multiply(0.3),
                Stroke::new(2.0, color),
            );
        }

        paint.text(
            size_rect.left_bottom() + vec2(0.1, 0.1) * screen.scale,
            Align2::LEFT_TOP,
            format!("Circuit size: {}x{}", design.size.x, design.size.y),
            world_font,
            color,
        );

        drawing::draw_cross(&screen, rect, &paint);

        self.selection.pre_update_selection(
            design,
            &ctx,
            self.selected_id == Some(SelectedItemId::Selection),
        );

        for (i, decoration) in design.decorations.iter_mut().enumerate() {
            match decoration {
                Decoration::Rect { rect, visuals } => {
                    rect.max.x = rect.max.x.max(rect.min.x + 0.05);
                    rect.max.y = rect.max.y.max(rect.min.y + 0.05);

                    let mut scr_rect = screen.world_to_screen_rect(*rect);
                    let scaled_stroke =
                        Stroke::new(visuals.stroke.width * screen.scale, visuals.stroke.color);
                    if self.selected_id.is_none() {
                        let id = ui.id().with("deco_rect").with(i);
                        let visuals = RectVisuals {
                            stroke: scaled_stroke,
                            ..*visuals
                        };

                        let shift = ui.input(|input| input.modifiers.shift);
                        let state = rect_editor(
                            &mut scr_rect,
                            Sides::ALL,
                            ui,
                            id,
                            |sides, rect| {
                                rect.max.x = rect.max.x.max(rect.min.x + screen.scale * 0.05);
                                rect.max.y = rect.max.y.max(rect.min.y + screen.scale * 0.05);
                                if shift && sides.any() {
                                    let mut r = screen.screen_to_world_rect(*rect);
                                    if sides.center {
                                        let size = r.size();
                                        let x = (r.left() * 2.0).round() * 0.5;
                                        let y = (r.top() * 2.0).round() * 0.5;
                                        r = Rect::from_min_size(pos2(x, y), size);
                                    }
                                    if sides.top {
                                        r.set_top((r.top() * 2.0).round() * 0.5);
                                    }
                                    if sides.left {
                                        r.set_left((r.left() * 2.0).round() * 0.5);
                                    }
                                    if sides.right {
                                        r.set_right((r.right() * 2.0).round() * 0.5);
                                    }
                                    if sides.bottom {
                                        r.set_bottom((r.bottom() * 2.0).round() * 0.5);
                                    }
                                    *rect = screen.world_to_screen_rect(r);
                                }
                            },
                            visuals,
                        );

                        if !matches!(state, DragState::None) {
                            *rect = screen.screen_to_world_rect(scr_rect);
                        }
                    } else {
                        paint.rect(scr_rect, visuals.rounding, visuals.fill, scaled_stroke);
                    }
                }
            }
        }

        for ((provider, control), info) in design.controls.iter_mut() {
            info.rect.max.x = info.rect.max.x.max(rect.min.x + 0.05);
            info.rect.max.y = info.rect.max.y.max(rect.min.y + 0.05);

            let mut scr_rect = screen.world_to_screen_rect(info.rect);
            if self.selected_id.is_none() {
                let id = ui.id().with("control_rect").with((*provider, *control));

                let shift = ui.input(|input| input.modifiers.shift);
                let state = custom_rect_editor(
                    &mut scr_rect,
                    Sides::ALL,
                    ui,
                    id,
                    |sides, rect| {
                        rect.max.x = rect.max.x.max(rect.min.x + screen.scale * 0.05);
                        rect.max.y = rect.max.y.max(rect.min.y + screen.scale * 0.05);
                        if sides.any() {
                            if shift {
                                let mut r = screen.screen_to_world_rect(*rect);
                                if sides.center {
                                    let size = r.size();
                                    let x = (r.left() * 2.0).round() * 0.5;
                                    let y = (r.top() * 2.0).round() * 0.5;
                                    r = Rect::from_min_size(pos2(x, y), size);
                                }

                                if sides.top {
                                    r.set_top((r.top() * 2.0).round() * 0.5);
                                }
                                if sides.left {
                                    r.set_left((r.left() * 2.0).round() * 0.5);
                                }
                                if sides.right {
                                    r.set_right((r.right() * 2.0).round() * 0.5);
                                }
                                if sides.bottom {
                                    r.set_bottom((r.bottom() * 2.0).round() * 0.5);
                                }
                                *rect = screen.world_to_screen_rect(r);
                            }
                            if !sides.center {
                                let orig_info = self.provider.get_control_info(*provider, *control);
                                if let Some(orig_info) = orig_info {
                                    let mut size = rect.size();
                                    if sides.left || sides.right {
                                        size.y = (size.x / orig_info.rect.width())
                                            * orig_info.rect.height();
                                    } else if sides.top || sides.bottom {
                                        size.x = (size.y / orig_info.rect.height())
                                            * orig_info.rect.width();
                                    }

                                    let (anchor, align) = if sides.left && sides.top {
                                        (rect.right_bottom(), vec2(-1.0, -1.0))
                                    } else if sides.top {
                                        (rect.left_bottom(), vec2(1.0, -1.0))
                                    } else if sides.left {
                                        (rect.right_top(), vec2(-1.0, 1.0))
                                    } else {
                                        (rect.left_top(), vec2(1.0, 1.0))
                                    };
                                    *rect = Rect::from_two_pos(anchor, anchor + size * align);
                                }
                            }
                        }
                    },
                    |rect, _, _| {
                        let ctx = ctx.with_rect(rect);
                        self.provider.paint_control(*provider, *control, &ctx);
                    },
                );

                if !matches!(state, DragState::None) {
                    info.rect = screen.screen_to_world_rect(scr_rect);
                }
            } else {
                let ctx = ctx.with_rect(scr_rect);
                self.provider.paint_control(*provider, *control, &ctx);
            }
        }

        for (i, pin) in design.pins.iter_mut().enumerate() {
            let center = ctx
                .screen
                .world_to_screen(pin.pos.convert(|v| v as f32 + 0.5));
            let mut rect = Rect::from_center_size(center.into(), vec2(0.5, 0.5) * ctx.screen.scale);

            if self.selected_id.is_none() {
                let id = ui.id().with("pin_rect").with(i);
                let state = rect_editor(
                    &mut rect,
                    Sides::CENTER,
                    ui,
                    id,
                    |_, rect| {
                        let pos = ctx.screen.screen_to_world(rect.center().into());
                        let pos = pos.convert(|v| v.max(0.0) as u32);

                        let center = ctx.screen.world_to_screen(pos.convert(|v| v as f32 + 0.5));
                        *rect = Rect::from_center_size(
                            center.into(),
                            vec2(0.5, 0.5) * ctx.screen.scale,
                        );
                    },
                    RectVisuals {
                        rounding: Rounding::ZERO,
                        fill: Color32::TRANSPARENT,
                        stroke: Stroke::NONE,
                    },
                );

                if !matches!(state, DragState::None) {
                    let pos = ctx.screen.screen_to_world(rect.center().into());
                    pin.pos = pos.convert(|v| v.max(0.0) as u32);
                }
            }

            self.draw_pin(pin.dir, pin.display_dir, pin.pos, &ctx);
        }

        crate::ui::drawing::draw_pin_names(
            [0, 0].into(),
            design
                .pins
                .iter()
                .map(|p| (p.pos, p.display_name.deref(), p.display_dir)),
            1.0,
            0.5,
            &ctx,
        );

        self.selection.update_selection(design, &ctx);

        if let Some(selected) = &self.selected_id {
            let mouse_tile_pos = ctx
                .ui
                .input(|input| input.pointer.interact_pos())
                .map(|p| ctx.screen.screen_to_world(Vec2f::from(p)));

            if let Some(mouse_tile_pos) = mouse_tile_pos {
                match selected {
                    SelectedItemId::Selection => {}
                    SelectedItemId::Pin(id) => 'm: {
                        for pin in design.pins.iter() {
                            if pin.id == *id {
                                break 'm;
                            }
                        }

                        let mouse_tile_pos_i = mouse_tile_pos.convert(|v| v.floor() as i32);

                        if mouse_tile_pos_i.x < 0 || mouse_tile_pos_i.y < 0 {
                            break 'm;
                        }
                        let mouse_tile_pos_i = mouse_tile_pos_i.convert(|v| v as u32);

                        let info = self.provider.get_pin(id);
                        let info = unwrap_option_or_break!(info, 'm);

                        self.draw_pin(info.dir, self.selected_pin_dir, mouse_tile_pos_i, &ctx);

                        let interaction = ctx.ui.interact(ctx.rect, ctx.ui.id(), Sense::click());
                        if interaction.clicked_by(PointerButton::Primary) {
                            design.pins.push(CircuitDesignPin {
                                id: id.clone(),
                                pos: mouse_tile_pos_i,
                                dir: info.dir,
                                display_dir: self.selected_pin_dir,
                                display_name: info.display_name,
                            });

                            self.selected_id = None;
                        }
                    }
                    SelectedItemId::Rect => {
                        let mouse_tile_pos = match ui.input(|input| input.modifiers.shift) {
                            false => mouse_tile_pos,
                            true => mouse_tile_pos.convert(|v| (v * 2.0).round() * 0.5),
                        };

                        let interaction = ctx.ui.interact(ctx.rect, ctx.ui.id(), Sense::drag());

                        if interaction.drag_started_by(PointerButton::Primary) {
                            self.rect_drag_start = Some(mouse_tile_pos);
                        }
                        if let Some(rect_drag_start) = self.rect_drag_start {
                            let rect =
                                Rect::from_two_pos(rect_drag_start.into(), mouse_tile_pos.into());
                            let visuals = *self.default_rect_visuals.read();
                            let scaled_visuals = visuals.scaled_by(ctx.screen.scale);

                            ctx.paint.rect(
                                ctx.screen.world_to_screen_rect(rect),
                                scaled_visuals.rounding,
                                scaled_visuals.fill,
                                scaled_visuals.stroke,
                            );

                            if interaction.drag_released_by(PointerButton::Primary) {
                                design.decorations.push(Decoration::Rect { rect, visuals });
                                self.rect_drag_start = None;
                            }
                        }
                    }
                    SelectedItemId::Control(provider, control) => 'm: {
                        if design.controls.contains_key(&(*provider, *control)) {
                            break 'm;
                        }

                        let info = self.provider.get_control_info(*provider, *control);
                        let info = unwrap_option_or_break!(info, 'm);

                        let rect_pos = mouse_tile_pos - info.rect.size() / 2.0;
                        let rect_pos = match ui.input(|input| input.modifiers.shift) {
                            false => rect_pos,
                            true => rect_pos.convert(|v| (v * 2.0).round() * 0.5),
                        };

                        let rect = Rect::from_min_size(rect_pos.into(), info.rect.size());

                        let screen_rect = ctx.screen.world_to_screen_rect(rect);
                        let paint_ctx = ctx.with_rect(screen_rect);
                        self.provider.paint_control(*provider, *control, &paint_ctx);

                        let interaction = ctx.ui.interact(ctx.rect, ctx.ui.id(), Sense::click());

                        if interaction.clicked_by(PointerButton::Primary) {
                            design.controls.insert(
                                (*provider, *control),
                                CircuitDesignControl {
                                    rect,
                                    display_name: info.display_name.clone().into(),
                                },
                            );

                            self.selected_id = None;
                        }
                    }
                }
            }
        }
    }

    pub fn ui_update(&mut self, ui: &mut Ui) -> DesignerResponse {

        let designs = self.storage.clone();
        let mut designs = designs.write();
        let design = designs.current_mut();

        self.components_ui(ui, design);
        self.properties_ui(ui, design);

        drop(designs);

        let rect = crate::ui::side_panel::remaining_rect(ui).shrink(10.0);
        let mut ui = ui.child_ui(rect, *ui.layout());

        if ui.input(|input| input.key_pressed(Key::Escape))
            || ui.input(|input| {
                input.pointer.button_released(PointerButton::Secondary)
                    && !input.pointer.is_decidedly_dragging()
            })
        {
            self.selected_id = None;
            self.selection.clear();
        }
        let close = ui.button("Exit designer").clicked();

        let inv_resp =
            Inventory::new(&mut self.selected_id, &self.inventory).ui(&mut ui, |id| match id {
                SelectedItemId::Selection => Some("Selection".into()),
                SelectedItemId::Pin(id) => {
                    self.provider.get_pin(id).map(|i| i.display_name.clone())
                }
                SelectedItemId::Rect => Some("Rectangle".into()),
                SelectedItemId::Control(provider_id, control_id) => self
                    .provider
                    .get_control_info(*provider_id, *control_id)
                    .map(|i| i.display_name),
            });

        if inv_resp.changed() {
            self.selected_pin_dir = None;
            self.rect_drag_start = None;
        }

        DesignerResponse { close }
    }

    fn draw_pin(
        &self,
        dir: InternalPinDirection,
        display_dir: Option<Direction4>,
        pos: Vec2u,
        ctx: &PaintContext<'_>,
    ) {
        let (rect_off, rect_size, angle) = match display_dir {
            Some(Direction4::Up) => (vec2(-0.5, -0.65), vec2(1.0, 1.15), TAU * 0.75),
            Some(Direction4::Left) => (vec2(-0.65, -0.5), vec2(1.15, 1.0), TAU * 0.5),
            Some(Direction4::Right) => (vec2(-0.5, -0.5), vec2(1.15, 1.0), 0.0),
            Some(Direction4::Down) => (vec2(-0.5, -0.5), vec2(1.0, 1.15), TAU * 0.25),
            None => (vec2(-0.5, -0.5), vec2(1.0, 1.0), 0.0),
        };

        let center = ctx.screen.world_to_screen(pos.convert(|v| v as f32 + 0.5));

        let rect = Rect::from_min_size(
            Pos2::from(center) + rect_off * ctx.screen.scale,
            rect_size * ctx.screen.scale,
        );

        {
            let ctx = ctx.with_rect(rect);

            let pico = match dir {
                InternalPinDirection::StateDependent { default: _ } => None,
                InternalPinDirection::Inside => Some(true),
                InternalPinDirection::Outside => Some(false),
                InternalPinDirection::Custom => None,
            };

            crate::graphics::outside_pin(
                WireState::False,
                display_dir.is_some(),
                pico,
                angle,
                &ctx,
            );
        }
    }

    fn components_ui(&mut self, ui: &mut Ui, design: &mut CircuitDesign) {
        let style = ui.style().clone();
        SidePanel::new(PanelSide::Left, "components-ui")
            .frame(
                Frame::side_top_panel(&style)
                    .rounding(Rounding {
                        ne: 5.0,
                        nw: 0.0,
                        se: 5.0,
                        sw: 0.0,
                    })
                    .outer_margin(Margin::symmetric(0.0, 8.0))
                    .inner_margin(Margin::symmetric(5.0, 5.0))
                    .stroke(style.visuals.window_stroke),
            )
            .show_separator_line(false)
            .default_tab(Some(0))
            .show(
                ui,
                1,
                |_| "Components".into(),
                |_, ui| {
                    ScrollArea::vertical().show(ui, |ui| {
                        let font = TextStyle::Monospace.resolve(ui.style());

                        let pins = self.provider.get_pin_ids();

                        if !pins.is_empty() {
                            CollapsingHeader::new("Pins")
                                .default_open(true)
                                .show(ui, |ui| {
                                    for id in pins {
                                        let info =
                                            unwrap_option_or_continue!(self.provider.get_pin(&id));

                                        ui.horizontal(|ui| {
                                            let resp = ui.allocate_response(
                                                vec2(font.size * 1.15, font.size),
                                                Sense::hover(),
                                            );

                                            let paint_ctx =
                                                PaintContext::new_on_ui(ui, resp.rect, font.size);

                                            let pico = match info.dir {
                                                InternalPinDirection::StateDependent {
                                                    default: _,
                                                } => None,
                                                InternalPinDirection::Inside => Some(true),
                                                InternalPinDirection::Outside => Some(false),
                                                InternalPinDirection::Custom => None,
                                            };
                                            let angle =
                                                if pico == Some(true) { TAU * 0.5 } else { 0.0 };
                                            crate::graphics::outside_pin(
                                                WireState::False,
                                                true,
                                                pico,
                                                angle,
                                                &paint_ctx,
                                            );

                                            let selected = match &self.selected_id {
                                                Some(SelectedItemId::Pin(sel_id)) => {
                                                    sel_id.deref() == id.deref()
                                                }
                                                _ => false,
                                            };

                                            if ui
                                                .selectable_label(
                                                    selected,
                                                    info.display_name.deref(),
                                                )
                                                .clicked()
                                            {
                                                let existing_pin =
                                                    design.pins.iter().find_index(|p| p.id == id);

                                                if let Some(existing_pin) = existing_pin {
                                                    self.selection.clear();
                                                    self.selection.selection.insert(
                                                        SelectedDesignObject::Pin(existing_pin),
                                                    );

                                                    if let Some(pin) = design.pins.get(existing_pin)
                                                    {
                                                        let center =
                                                            pin.pos.convert(|v| v as f32 + 0.5);

                                                        self.pan_zoom.center_pos = center;
                                                        self.pan_zoom.scale =
                                                            self.pan_zoom.scale.max(8.0)
                                                    }
                                                } else {
                                                    self.selected_id = match selected {
                                                        true => None,
                                                        false => {
                                                            Some(SelectedItemId::Pin(id.clone()))
                                                        }
                                                    };
                                                    self.selected_pin_dir = info.display_dir;
                                                }
                                            }
                                        });
                                    }
                                });
                        }

                        let control_providers = self.provider.get_control_provider_data();
                        if !control_providers.is_empty() {
                            CollapsingHeader::new("Controls")
                                .default_open(true)
                                .show(ui, |ui| {
                                    for provider_data in control_providers {
                                        for control in 0..provider_data.count {
                                            let control_data = self
                                                .provider
                                                .get_control_info(provider_data.id, control);
                                            let control_data =
                                                unwrap_option_or_continue!(control_data);

                                            ui.horizontal(|ui| {
                                                let resp = ui.allocate_response(
                                                    vec2(font.size, font.size),
                                                    Sense::hover(),
                                                );
                                                let (rect, scale) = align_rect_scaled(
                                                    resp.rect.left_top(),
                                                    resp.rect.size(),
                                                    control_data.rect.size(),
                                                );

                                                let ctx = PaintContext::new_on_ui(ui, rect, scale);

                                                self.provider.paint_control(
                                                    provider_data.id,
                                                    control,
                                                    &ctx,
                                                );

                                                let selected = match &self.selected_id {
                                                    Some(SelectedItemId::Control(
                                                        prov_id,
                                                        ctl_id,
                                                    )) => {
                                                        *prov_id == provider_data.id
                                                            && *ctl_id == control
                                                    }
                                                    _ => false,
                                                };

                                                if ui
                                                    .selectable_label(
                                                        selected,
                                                        control_data.display_name.deref(),
                                                    )
                                                    .clicked()
                                                {
                                                    if let Some(info) = design
                                                        .controls
                                                        .get(&(provider_data.id, control))
                                                    {
                                                        self.selection.clear();
                                                        self.selection.selection.insert(
                                                            SelectedDesignObject::Control(
                                                                provider_data.id,
                                                                control,
                                                            ),
                                                        );

                                                        self.pan_zoom.center_pos =
                                                            info.rect.center().into();
                                                        self.pan_zoom.scale =
                                                            self.pan_zoom.scale.max(8.0)
                                                    } else {
                                                        self.selected_id = match selected {
                                                            true => None,
                                                            false => Some(SelectedItemId::Control(
                                                                provider_data.id,
                                                                control,
                                                            )),
                                                        };
                                                    }
                                                }
                                            });
                                        }
                                    }
                                });
                        }
                    })
                },
            );
    }

    fn properties_ui(&mut self, ui: &mut Ui, design: &mut CircuitDesign) {
        let style = ui.style().clone();

        let active = if !self.selection.selection.is_empty() {
            true
        } else if self.selected_id.is_some() {
            self.selected_id
                .as_ref()
                .is_some_and(|s| matches!(s, SelectedItemId::Pin(_) | SelectedItemId::Rect))
        } else {
            self.provider.has_custom_config()
        };

        let tabs = if active { 1 } else { 0 };
        SidePanel::new(PanelSide::Right, "prop-ui")
            .frame(
                Frame::side_top_panel(&style)
                    .rounding(Rounding {
                        nw: 5.0,
                        ne: 0.0,
                        sw: 5.0,
                        se: 0.0,
                    })
                    .outer_margin(Margin::symmetric(0.0, 8.0))
                    .inner_margin(Margin::symmetric(5.0, 5.0))
                    .stroke(style.visuals.window_stroke),
            )
            .show_separator_line(false)
            .default_tab(Some(0))
            .size_range(Rangef::new(120.0, f32::MAX))
            .show(
                ui,
                tabs,
                |_| "Properties editor".into(),
                |_, ui| {
                    ui.set_min_width(120.0);
                    if !self.selection.selection.is_empty() {
                        let same_type = self
                            .selection
                            .iter()
                            .filter_map(|o| match o {
                                SelectedDesignObject::Decoration(id) => design
                                    .decorations
                                    .get(*id)
                                    .map(|d| SelectedDesignObjectType::Decoration(d.ty())),
                                SelectedDesignObject::Pin(id) => {
                                    design.pins.get(*id).map(|_| SelectedDesignObjectType::Pin)
                                }
                                SelectedDesignObject::Control(provider, control) => design
                                    .controls
                                    .contains_key(&(*provider, *control))
                                    .then_some(SelectedDesignObjectType::Control),
                            })
                            .same();
                        match same_type {
                            None => {
                                ui.vertical_centered(|ui| {
                                    Label::new("No editable properties").wrap(false).ui(ui);
                                });
                            }
                            Some(SelectedDesignObjectType::Decoration(DecorationType::Rect)) => {
                                let iter = design.decorations.iter_mut().enumerate().filter_map(
                                    |(i, d)| {
                                        let key = SelectedDesignObject::Decoration(i);
                                        if !self.selection.contains(&key) {
                                            return None;
                                        }
                                        match d {
                                            Decoration::Rect { rect, visuals } => {
                                                Some((rect, visuals))
                                            }
                                        }
                                    },
                                );

                                Self::rect_properties(iter, ui, true);
                            }
                            Some(SelectedDesignObjectType::Pin) => {
                                let iter =
                                    design.pins.iter_mut().enumerate().filter_map(|(i, p)| {
                                        let key = SelectedDesignObject::Pin(i);
                                        self.selection.contains(&key).then_some(&mut p.display_dir)
                                    });

                                Self::pin_properties(iter, ui);
                            }
                            Some(SelectedDesignObjectType::Control) => {
                                let iter = design.controls.iter_mut().filter_map(
                                    |((provider, control), data)| {
                                        let key =
                                            SelectedDesignObject::Control(*provider, *control);
                                        self.selection
                                            .contains(&key)
                                            .then_some((*provider, *control, data))
                                    },
                                );

                                Self::control_properties(iter, ui, self.provider.deref());
                            }
                        };
                    } else if let Some(selected) = &self.selected_id {
                        match selected {
                            SelectedItemId::Selection => {}
                            SelectedItemId::Pin(_) => {
                                Self::pin_properties([&mut self.selected_pin_dir].into_iter(), ui);
                            }
                            SelectedItemId::Rect => {
                                let mut visuals = self.default_rect_visuals.write();
                                let mut dummy = Rect::NOTHING;
                                Self::rect_properties(
                                    [(&mut dummy, visuals.deref_mut())].into_iter(),
                                    ui,
                                    false,
                                )
                            }
                            SelectedItemId::Control(_, _) => {}
                        }
                    } else if self.provider.has_custom_config() {
                        self.provider.custom_config_ui(ui);
                    }
                },
            );
    }

    fn rect_properties<'a, I>(mut iter: I, ui: &mut Ui, rect_ediror: bool)
    where
        I: Iterator<Item = (&'a mut Rect, &'a mut RectVisuals)>,
    {
        let (rect, visuals) = unwrap_option_or_return!(iter.next());

        let mut fill_changed = false;
        let mut stroke_width_changed = false;
        let mut stroke_color_changed = false;
        let mut rect_changes = RectPropertyChanges::default();

        Grid::new("rect_props").show(ui, |ui| {
            ui.label("Fill color");
            fill_changed = ui.color_edit_button_srgba(&mut visuals.fill).changed();
            ui.end_row();

            ui.label("Stroke width");
            stroke_width_changed = ui
                .add(Slider::new(&mut visuals.stroke.width, 0.0..=1.0))
                .changed();
            ui.end_row();

            ui.label("Stroke color");
            stroke_color_changed = ui
                .color_edit_button_srgba(&mut visuals.stroke.color)
                .changed();
            ui.end_row();
        });
        if rect_ediror {
            rect_changes = rect_properties_editor(rect, ui, "rect_edit", "Rectangle", |_, _| {});
        }
        let visuals = *visuals;
        let rect = *rect;

        let changed =
            fill_changed || stroke_color_changed || stroke_width_changed || rect_changes.any();

        if changed {
            for (other_rect, other_vis) in iter {
                if fill_changed {
                    other_vis.fill = visuals.fill;
                }
                if stroke_width_changed {
                    other_vis.stroke.width = visuals.stroke.width;
                }
                if stroke_color_changed {
                    other_vis.stroke.color = visuals.stroke.color;
                }

                if rect_changes.any() {
                    for prop in rect_changes.iter() {
                        match prop {
                            RectProperty::Top => {
                                let height = other_rect.height();
                                other_rect.set_top(rect.top());
                                other_rect.set_height(height);
                            }
                            RectProperty::Left => {
                                let width = other_rect.width();
                                other_rect.set_left(rect.left());
                                other_rect.set_width(width);
                            }
                            RectProperty::Width => other_rect.set_width(rect.width()),
                            RectProperty::Height => other_rect.set_height(rect.height()),
                        }
                    }
                }
            }
        }
    }

    fn pin_properties<'a, I>(mut iter: I, ui: &mut Ui)
    where
        I: Iterator<Item = &'a mut Option<Direction4>>,
    {
        let value = unwrap_option_or_return!(iter.next());
        let mut changed = false;

        fn dir_name(dir: Option<Direction4>) -> &'static str {
            match dir {
                Some(dir) => dir.name(),
                None => "None",
            }
        }

        Grid::new("pin_props").show(ui, |ui| {
            ui.label("Direction");
            ComboBox::from_id_source(ui.next_auto_id())
                .selected_text(dir_name(*value))
                .show_ui(ui, |ui| {
                    let iter = [None].into_iter().chain(Direction4::iter_all().map(Some));

                    for dir in iter {
                        let res = ui.selectable_value(value, dir, dir_name(dir));
                        if res.changed() || res.clicked() {
                            changed = true;
                        }
                    }
                });
            ui.end_row();
        });
        if changed {
            let value = *value;

            for v in iter {
                *v = value;
            }
        }
    }

    fn control_properties<'a, I>(mut iter: I, ui: &mut Ui, design_provider: &dyn DesignProvider)
    where
        I: Iterator<Item = (usize, usize, &'a mut CircuitDesignControl)>,
    {
        let (provider, control, value) = unwrap_option_or_return!(iter.next());
        let mut name_changed = false;

        let constrain = |prop: RectProperty, orig_rect: Option<Rect>, rect: &mut Rect| {
            if let Some(orig_rect) = orig_rect {
                match prop {
                    RectProperty::Width => {
                        rect.set_height((rect.width() / orig_rect.width()) * orig_rect.height())
                    }
                    RectProperty::Height => {
                        rect.set_width((rect.height() / orig_rect.height()) * orig_rect.width())
                    }
                    _ => {}
                }
            }
        };

        Grid::new("control_props").show(ui, |ui| {
            ui.label("Name");
            name_changed = TextEdit::singleline(value.display_name.get_mut())
                .min_size(vec2(100.0, 0.0))
                .ui(ui)
                .changed();
            ui.end_row();
        });

        let orig_rect = design_provider
            .get_control_info(provider, control)
            .map(|i| i.rect);

        let rect_changes = rect_properties_editor(
            &mut value.rect,
            ui,
            "rect_edit",
            "Rectangle",
            |prop, rect| constrain(prop, orig_rect, rect),
        );

        if name_changed || rect_changes.any() {
            for (provider, control, other_value) in iter {
                if name_changed {
                    other_value.display_name = value.display_name.clone();
                }

                if rect_changes.any() {
                    let orig_rect = design_provider
                        .get_control_info(provider, control)
                        .map(|i| i.rect);

                    for prop in rect_changes.iter() {
                        match prop {
                            RectProperty::Top => {
                                let height = other_value.rect.height();
                                other_value.rect.set_top(value.rect.top());
                                other_value.rect.set_height(height);
                            }
                            RectProperty::Left => {
                                let width = other_value.rect.width();
                                other_value.rect.set_left(value.rect.left());
                                other_value.rect.set_width(width);
                            }
                            RectProperty::Width => other_value.rect.set_width(value.rect.width()),
                            RectProperty::Height => {
                                other_value.rect.set_height(value.rect.height())
                            }
                        }
                        constrain(prop, orig_rect, &mut other_value.rect);
                    }
                }
            }
        }
    }
}
