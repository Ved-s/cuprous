use std::{f32::consts::PI, ops::Deref, sync::Arc};

use eframe::{
    egui::{
        pos2, vec2, CollapsingHeader, Color32, FontSelection, Margin, Painter, Pos2, Rect,
        Response, Rounding, Sense, Shape, Stroke, Ui, WidgetText,
    },
    epaint::PathShape,
};

use crate::{app::SelectedItem, circuits::CircuitRenderingContext, vector::Vec2f, Screen, Style};

use super::{TabCreation, TabImpl};

pub struct ComponentList {}

impl TabCreation for ComponentList {
    fn new(_app: &crate::app::App) -> Self {
        Self {}
    }
}

impl TabImpl for ComponentList {
    fn update(&mut self, app: &mut crate::app::App, ui: &mut eframe::egui::Ui) {
        CollapsingHeader::new("Tools")
            .default_open(true)
            .show(ui, |ui| {
                let wires = matches!(app.selected_item, Some(SelectedItem::Wires));
                if selectable_icon_label(ui, 0.0, wires, "Wire".into(), &mut wire_icon).clicked() {
                    if wires {
                        app.selected_item = None;
                    } else {
                        app.selected_item = Some(SelectedItem::Wires);
                    }
                };
                let selection = matches!(app.selected_item, Some(SelectedItem::Selection));
                if selectable_icon_label(
                    ui,
                    0.0,
                    selection,
                    "Selection".into(),
                    &mut |rect, ui| selection_icon(rect, ui.painter(), &app.style),
                )
                .clicked()
                {
                    if selection {
                        app.selected_item = None;
                    } else {
                        app.selected_item = Some(SelectedItem::Selection);
                    }
                }
            });


        CollapsingHeader::new("Circuits")
            .default_open(true)
            .show(ui, |ui| {
                let selected = app.selected_item.as_ref().and_then(|i| match i {
                    SelectedItem::Circuit(c) => Some(c),
                    _ => None,
                });
                let mut new_selected = None;
                for blueprint in &app.blueprints {
                    let selected = selected.is_some_and(|s| Arc::ptr_eq(s, blueprint));

                    let mut icon = |rect: Rect, ui: &mut Ui| {
                        let blueprint = blueprint.read();

                        let sizef = blueprint.size.convert(|v| v as f32);
                        let scale = (rect.width() / sizef.x).min(rect.height() / sizef.y);
                        let scaled_size = sizef * scale;
                        let pos = (Vec2f::from(rect.size()) - scaled_size) / 2.0 + rect.left_top();
                        let rect = Rect::from_min_size(pos.into(), scaled_size.into());

                        let screen = Screen::new(rect, 0.0.into(), scale);
                        let ctx = CircuitRenderingContext {
                            screen,
                            painter: ui.painter(),
                            ui,
                            rect,
                            selection: None,
                        };

                        blueprint.imp.draw(&ctx);
                    };

                    let res = selectable_icon_label(
                        ui,
                        0.0,
                        selected,
                        blueprint.read().display_name.deref().into(),
                        &mut icon,
                    );

                    if res.clicked() {
                        if selected {
                            new_selected = Some(None);
                        } else {
                            new_selected = Some(Some(SelectedItem::Circuit(blueprint.clone())));
                        }
                    }
                }

                if let Some(new_selected) = new_selected {
                    app.selected_item = new_selected;
                }
            });
    }
}

fn selectable_icon_label(
    ui: &mut Ui,
    min_width: f32,
    selected: bool,
    text: WidgetText,
    icon: &mut dyn FnMut(Rect, &mut Ui),
) -> Response {
    let padding = Margin::same(2.0);
    let spacing = 3.0;

    let icon_size = FontSelection::Default.resolve(ui.style()).size + 2.0;
    let max_text_width =
        ui.available_width() - (icon_size + spacing + padding.left + padding.right);

    let galley = text.into_galley(ui, Some(true), max_text_width, FontSelection::Default);

    let size = vec2(
        galley.size().x + spacing + icon_size,
        icon_size.max(galley.size().y),
    );

    let size = size + padding.sum();
    let size = vec2(size.x.max(min_width), size.y);
    let (rect, resp) = ui.allocate_exact_size(size, Sense::click());

    if !ui.is_rect_visible(resp.rect) {
        return resp;
    }

    let visuals = ui.style().interact_selectable(&resp, selected);

    if selected || resp.hovered() || resp.highlighted() || resp.has_focus() {
        let rect = rect.expand(visuals.expansion);

        ui.painter().rect(
            rect,
            visuals.rounding,
            visuals.weak_bg_fill,
            visuals.bg_stroke,
        );
    }

    let icon_y = rect.top() + (rect.height() - icon_size) * 0.5;
    let text_y = rect.top() + (rect.height() - galley.size().y) * 0.5;

    let icon_rect = Rect::from_min_size(
        pos2(rect.left() + padding.left, icon_y),
        vec2(icon_size, icon_size),
    );

    icon(icon_rect, ui);

    ui.painter().galley(
        pos2(rect.left() + padding.left + icon_size + spacing, text_y),
        galley,
        visuals.text_color(),
    );

    resp
}

fn wire_icon(rect: Rect, ui: &mut Ui) {
    // let color = pass
    //     .false_color_override
    //     .unwrap_or_else(|| ctx.style.wire_colors.false_color());

    let color = Color32::DARK_GREEN;

    let rect1 = Rect::from_center_size(rect.lerp_inside([0.2, 0.2].into()), rect.size() * 0.3);
    let rect2 = Rect::from_center_size(rect.lerp_inside([0.8, 0.8].into()), rect.size() * 0.3);

    let painter = ui.painter();

    painter.line_segment([rect1.center(), rect2.center()], Stroke::new(2.5, color));

    painter.add(PathShape {
        points: rotated_rect_shape(rect1, PI * 0.25, rect1.center()),
        closed: true,
        fill: color,
        stroke: Stroke::NONE,
    });

    painter.add(PathShape {
        points: rotated_rect_shape(rect2, PI * 0.25, rect2.center()),
        closed: true,
        fill: color,
        stroke: Stroke::NONE,
    });
}

fn selection_icon(rect: Rect, painter: &Painter, style: &Style) {
    // Why is rendering so wonky, just give me non-blurred pixels!!!
    let rect = Rect::from_min_max(rect.left_top().ceil(), rect.right_bottom().floor());
    let size = rect.width().min(rect.height()).floor();
    let rect = Rect::from_min_size(rect.left_top(), vec2(size, size));

    let painter = painter.with_clip_rect(rect);

    painter.rect_filled(rect, Rounding::ZERO, style.selection_fill);
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
        Stroke::new(1.0, style.selection_border),
        3.0,
        2.0,
        &mut shapes,
    );

    shapes.into_iter().for_each(|s| {
        painter.add(s);
    });
}

fn rotated_rect_shape(rect: Rect, angle: f32, origin: Pos2) -> Vec<Pos2> {
    let mut points = vec![
        rect.left_top(),
        rect.right_top(),
        rect.right_bottom(),
        rect.left_bottom(),
    ];

    let cos = angle.cos();
    let sin = angle.sin();

    for p in points.iter_mut() {
        let pl = *p - origin;

        let x = cos * pl.x - sin * pl.y;
        let y = sin * pl.x + cos * pl.y;
        *p = pos2(x, y) + origin.to_vec2();
    }

    points
}
