use std::{f32::consts::TAU, ops::Deref, sync::Arc};

use eframe::{
    egui::{
        self, CollapsingHeader, Frame, Grid, Key, Label, Margin, Sense, SidePanel, Slider,
        TextStyle, Ui, Widget,
    },
    epaint::{Color32, FontId, Rounding, Stroke, Shape, RectShape},
};
use emath::{vec2, Align2, Rect};

use crate::{
    board::{CircuitDesign, CircuitDesignStorage, Decoration, DecorationType},
    circuits::InternalPinDirection,
    ext::IteratorSameExt,
    state::WireState,
    vector::Vec2f,
    Direction4, DynStaticStr, PaintContext, PanAndZoom, RwLock,
};

use super::{
    drawing, rect_editor,
    selection::{
        selection_border_color, selection_fill_color, Selection, SelectionImpl,
        SelectionInventoryItem, SelectionMode,
    },
    CollapsibleSidePanel, Inventory, InventoryItemGroup, RectVisuals, Sides,
};

pub struct DesignerResponse {
    pub close: bool,
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum SelectedItemId {
    Selection,
    Pin(DynStaticStr),
}

#[derive(PartialEq, Eq, Hash)]
enum SelectedDesignObject {
    Decoration(usize),
}

#[derive(PartialEq, Eq)]
enum SelectedDesignObjectType {
    Decoration(DecorationType),
}

pub struct CircuitDesignPinInfo {
    pub dir: InternalPinDirection,
    pub display_dir: Option<Direction4>,
    pub display_name: DynStaticStr,
}

pub trait DesignProvider {
    fn get_storage(&self) -> Arc<RwLock<CircuitDesignStorage>>;
    fn get_pin_ids(&self) -> Vec<DynStaticStr>;
    fn get_pin(&self, id: &DynStaticStr) -> Option<CircuitDesignPinInfo>;
}

#[derive(Default)]
struct DesignerSelectionImpl {}

impl SelectionImpl<SelectedDesignObject, CircuitDesign> for DesignerSelectionImpl {
    fn collect_changes(
        &mut self,
        pass: &CircuitDesign,
        changes: &mut std::collections::HashSet<SelectedDesignObject>,
        rect: Rect,
    ) {
        for (i, deco) in pass.decorations.iter().enumerate() {
            match deco {
                Decoration::Rect {
                    rect: r,
                    visuals,
                } => {
                    if rect.expand(visuals.stroke.width * 0.5).intersects(*r) {
                        changes.insert(SelectedDesignObject::Decoration(i));
                    }
                }
            }
        }
    }

    fn draw_object_selection(
        &mut self,
        pass: &CircuitDesign,
        object: &SelectedDesignObject,
        ctx: &crate::PaintContext,
        shapes: &mut Vec<Shape>
    ) {
        match object {
            SelectedDesignObject::Decoration(deco) => {
                if let Some(deco) = pass.decorations.get(*deco) {
                    let rect = match deco {
                        Decoration::Rect { rect, visuals } => rect.expand(visuals.stroke.width * 0.5),
                    };

                    let rect = ctx.screen.world_to_screen_rect(rect).expand(2.0);
                    shapes.push(Shape::Rect(RectShape::new(
                        rect,
                        Rounding::ZERO,
                        selection_fill_color(),
                        Stroke::new(2.0, selection_border_color()),
                    )));
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
    selection: Selection<DesignerSelectionImpl, SelectedDesignObject, CircuitDesign>,

    circuit_size_rect: Option<Rect>,
}

impl Designer {
    pub fn new(provider: Box<dyn DesignProvider>) -> Self {
        let storage = provider.get_storage();

        let size = storage.read().current().size;
        let pan_zoom = PanAndZoom::new(size.convert(|v| v as f32) / 2.0, 16.0);

        Self {
            storage,
            provider,
            pan_zoom,
            selected_id: None,
            inventory: vec![InventoryItemGroup::SingleItem(Box::new(
                SelectionInventoryItem::new(SelectedItemId::Selection),
            ))]
            .into_boxed_slice(),
            selection: Default::default(),

            circuit_size_rect: None,
        }
    }

    pub fn update(&mut self, ui: &mut Ui) -> DesignerResponse {
        let rect = ui.max_rect();

        self.pan_zoom.update(ui, rect, self.selected_id.is_none());

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

        let designs = self.storage.clone();
        let mut designs = designs.write();
        let design = designs.current_mut();

        let world_font = FontId::new(screen.scale * 0.5, eframe::epaint::FontFamily::Monospace);

        let color = Color32::from_rgb(134, 114, 135);

        let mut size_rect = self.circuit_size_rect.unwrap_or_else(|| {
            Rect::from_min_size(
                screen.world_to_screen(0.0.into()).into(),
                (design.size.convert(|v| v as f32) * screen.scale).into(),
            )
        });

        if self.selected_id.is_none() {
            let sides = super::rect_editor(
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

            if sides.any() {
                self.circuit_size_rect = Some(size_rect);
                design.size = (Vec2f::from(size_rect.size()) / screen.scale)
                    .convert(|v| (v.round() as u32).max(1))
            } else {
                self.circuit_size_rect = None;
            }
        } else {
            self.circuit_size_rect = None;
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
            format!("Circuit size: {}x{}", design.size.x(), design.size.y()),
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
                    let mut scr_rect = screen.world_to_screen_rect(*rect);
                    let scaled_stroke = Stroke::new(visuals.stroke.width * screen.scale, visuals.stroke.color);
                    if self.selected_id.is_none() {
                        let id = ui.id().with("deco_rect").with(i);
                        let visuals = RectVisuals {
                            stroke: scaled_stroke,
                            ..*visuals
                        };

                        let shift = ui.input(|input| input.modifiers.shift);
                        let sides =
                            rect_editor(&mut scr_rect, Sides::ALL, ui, id, |sides, rect| {
                                if shift && sides.any() && !sides.center {
                                    let mut r = screen.screen_to_world_rect(*rect);
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
                            }, visuals);

                        if sides.any() {
                            *rect = screen.screen_to_world_rect(scr_rect);
                        }
                    } else {
                        paint.rect(scr_rect, visuals.rounding, visuals.fill, scaled_stroke);
                    }
                }
            }
        }

        self.selection.update_selection(design, &ctx);

        let components_rect = self.components_ui(ui);
        let properties_rect = self.properties_ui(ui, design);

        drop(designs);

        let mut rect = ui.clip_rect();
        rect.min.x = components_rect.right();
        rect.max.x = properties_rect.left();
        rect = rect.shrink(10.0);
        let mut ui = ui.child_ui(rect, *ui.layout());

        if ui.input(|input| input.key_pressed(Key::Escape)) {
            self.selected_id = None;
        }

        let close = ui.button("Exit designer").clicked();

        Inventory::new(&mut self.selected_id, &self.inventory).ui(&mut ui, |id| match id {
            SelectedItemId::Selection => Some("Selection".into()),
            SelectedItemId::Pin(id) => self.provider.get_pin(id).map(|i| i.display_name.clone()),
        });

        DesignerResponse { close }
    }

    fn components_ui(&mut self, ui: &mut Ui) -> Rect {
        let style = ui.style().clone();
        CollapsibleSidePanel::new("components-ui", "Components")
            .header_offset(20.0)
            .side(egui::panel::Side::Left)
            .panel_transformer(Some(Box::new(move |panel: SidePanel| {
                panel
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
            })))
            .show(ui, |ui| {
                let font = TextStyle::Monospace.resolve(ui.style());

                let pins = self.provider.get_pin_ids();

                if !pins.is_empty() {
                    CollapsingHeader::new("Pins")
                        .default_open(true)
                        .show(ui, |ui| {
                            for id in pins {
                                let info = unwrap_option_or_continue!(self.provider.get_pin(&id));

                                ui.horizontal(|ui| {
                                    let resp = ui.allocate_response(
                                        vec2(font.size * 2.0, font.size),
                                        Sense::hover(),
                                    );

                                    let paint_ctx =
                                        PaintContext::new_on_ui(ui, resp.rect, font.size);

                                    let pico = match info.dir {
                                        InternalPinDirection::StateDependent { default: _ } => None,
                                        InternalPinDirection::Inside => Some(false),
                                        InternalPinDirection::Outside => Some(true),
                                        InternalPinDirection::Custom => None,
                                    };
                                    let angle = if pico == Some(false) { TAU * 0.5 } else { 0.0 };
                                    crate::circuits::pin::Pin::draw(
                                        None,
                                        WireState::False,
                                        WireState::False,
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
                                        .selectable_label(selected, info.display_name.deref())
                                        .clicked()
                                    {
                                        self.selected_id = match selected {
                                            true => None,
                                            false => Some(SelectedItemId::Pin(id.clone())),
                                        };
                                    }
                                });
                            }
                        });
                }
            })
            .full_rect
    }

    fn properties_ui(&self, ui: &mut Ui, design: &mut CircuitDesign) -> Rect {
        let style = ui.style().clone();

        let active = !self.selection.selection.is_empty();

        CollapsibleSidePanel::new("prop-ui", "Properties editor")
            .active(active)
            .header_offset(20.0)
            .side(egui::panel::Side::Right)
            .panel_transformer(Some(Box::new(move |panel: SidePanel| {
                panel
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
            })))
            .show(ui, |ui| {
                if !self.selection.selection.is_empty() {
                    let same_type = self
                        .selection
                        .selection
                        .iter()
                        .filter(|v| {
                            matches!(self.selection.mode, SelectionMode::Include)
                                || !self.selection.change.contains(v)
                        })
                        .filter_map(|o| match o {
                            SelectedDesignObject::Decoration(id) => design
                                .decorations
                                .get(*id)
                                .map(|d| SelectedDesignObjectType::Decoration(d.ty())),
                        })
                        .same();
                    match same_type {
                        None => {
                            ui.vertical_centered(|ui| {
                                Label::new("No editable properties").wrap(false).ui(ui);
                            });
                        }
                        Some(SelectedDesignObjectType::Decoration(DecorationType::Rect)) => {
                            let iter = design.decorations.iter_mut().filter_map(|d| match d {
                                Decoration::Rect { rect: _, visuals } => Some(visuals),
                                _ => None,
                            });

                            Self::rect_properties(iter, ui);
                        }
                    };
                }
            })
            .full_rect
    }

    fn rect_properties<'a, I>(mut iter: I, ui: &mut Ui)
    where
        I: Iterator<Item = &'a mut RectVisuals>,
    {
        let value = unwrap_option_or_return!(iter.next());

        let mut fill_changed = false;
        let mut stroke_width_changed = false;
        let mut stroke_color_changed = false;

        Grid::new("rect_props").show(ui, |ui| {
            ui.label("Fill color");
            fill_changed = ui.color_edit_button_srgba(&mut value.fill).changed();
            ui.end_row();

            ui.label("Stroke width");
            stroke_width_changed = ui
                .add(Slider::new(&mut value.stroke.width, 0.0..=1.0))
                .changed();
            ui.end_row();

            ui.label("Stroke color");
            stroke_color_changed = ui
                .color_edit_button_srgba(&mut value.stroke.color)
                .changed();
            ui.end_row();
        });
        let value = *value;

        let changed = fill_changed || stroke_color_changed || stroke_width_changed;

        if changed {
            for rect in iter {
                if fill_changed {
                    rect.fill = value.fill;
                }
                if stroke_width_changed {
                    rect.stroke.width = value.stroke.width;
                }
                if stroke_color_changed {
                    rect.stroke.color = value.stroke.color;
                }
            }
        }
    }
}
