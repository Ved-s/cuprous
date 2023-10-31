use std::f32::consts::TAU;

use eframe::{
    egui::{self, FontSelection, WidgetText},
    epaint::{Color32, FontId, Rounding, Stroke, TextShape},
};
use emath::{pos2, vec2, Pos2, Rect, Vec2};

use crate::{
    vector::{Vec2f, Vec2isize, Vec2u},
    Direction4, PaintContext, Screen,
};

pub fn draw_dynamic_grid(
    screen: &Screen,
    cell_size: f32,
    highlight_lines: Vec2u,
    paint: &egui::Painter,
) {
    let mut grid_ds_cell_size = screen.scale;
    while grid_ds_cell_size < cell_size * 0.5 {
        grid_ds_cell_size *= cell_size;
    }
    draw_grid(
        screen.wld_pos * screen.scale / grid_ds_cell_size,
        grid_ds_cell_size.into(),
        highlight_lines,
        screen.scr_rect,
        paint,
    );
}

pub fn draw_grid(
    pos: Vec2f,
    cell_size: Vec2f,
    highlight_lines: Vec2u,
    rect: emath::Rect,
    paint: &egui::Painter,
) {
    let pos = pos * cell_size;
    let visible_cells = (Vec2f::from(rect.size()) / cell_size).convert(|v| v as i32 + 2);
    let start = (pos / cell_size).convert(|v| v as i32);
    let off = pos % cell_size;

    let dim_stroke = Stroke::new(1.0, Color32::from_gray(64));
    let highlight_stroke = Stroke::new(1.5, Color32::from_gray(96));

    for i in 0..visible_cells.x() {
        let x = i + start.x();
        if highlight_lines.x() > 0 && x % highlight_lines.x() as i32 == 0 {
            continue;
        }

        let pos = rect.left() + cell_size.x() * i as f32 - off.x();
        paint.line_segment(
            [pos2(pos, rect.top()), pos2(pos, rect.bottom())],
            dim_stroke,
        );
    }

    for i in 0..visible_cells.y() {
        let y = i + start.y();
        if highlight_lines.y() > 0 && y % highlight_lines.y() as i32 == 0 {
            continue;
        }

        let pos = rect.top() + cell_size.y() * i as f32 - off.y();
        paint.line_segment(
            [pos2(rect.left(), pos), pos2(rect.right(), pos)],
            dim_stroke,
        );
    }

    let hightlight_cells =
        visible_cells.combine_with(
            highlight_lines,
            |v, m| if m == 0 { 0 } else { v / m as i32 + 2 },
        );
    let highlight_off = pos % (cell_size * highlight_lines.convert(|v| v as f32));

    for i in 0..hightlight_cells.x() {
        let pos =
            rect.left() + cell_size.x() * i as f32 * highlight_lines.x() as f32 - highlight_off.x();
        paint.line_segment(
            [pos2(pos, rect.top()), pos2(pos, rect.bottom())],
            highlight_stroke,
        );
    }

    for i in 0..hightlight_cells.y() {
        let pos =
            rect.top() + cell_size.y() * i as f32 * highlight_lines.y() as f32 - highlight_off.y();
        paint.line_segment(
            [pos2(rect.left(), pos), pos2(rect.right(), pos)],
            highlight_stroke,
        );
    }

    if start.x() <= 0 && visible_cells.x() + start.x() >= 0 {
        let pos = rect.left() + cell_size.x() * -start.x() as f32 - off.x();
        paint.line_segment(
            [pos2(pos, rect.top()), pos2(pos, rect.bottom())],
            Stroke::new(1.0, Color32::GREEN),
        );
    }

    if start.y() <= 0 && visible_cells.y() + start.y() >= 0 {
        let pos = rect.top() + cell_size.y() * -start.y() as f32 - off.y();
        paint.line_segment(
            [pos2(rect.left(), pos), pos2(rect.right(), pos)],
            Stroke::new(1.0, Color32::RED),
        );
    }
}

pub fn draw_cross(screen: &Screen, bounds: Rect, paint: &egui::Painter) {
    let mut cross_pos = screen.world_to_screen(0.0.into());

    *cross_pos.x_mut() = cross_pos.x().clamp(bounds.left(), bounds.right());
    *cross_pos.y_mut() = cross_pos.y().clamp(bounds.top(), bounds.bottom());

    let unit = Vec2f::single_value(screen.scale);

    let cross_stroke = Stroke::new(2.0, Color32::WHITE);

    paint.line_segment(
        [
            pos2(cross_pos.x() - unit.x(), cross_pos.y()),
            pos2(cross_pos.x() + unit.x(), cross_pos.y()),
        ],
        cross_stroke,
    );
    paint.line_segment(
        [
            pos2(cross_pos.x(), cross_pos.y() - unit.y()),
            pos2(cross_pos.x(), cross_pos.y() + unit.y()),
        ],
        cross_stroke,
    );
}

pub fn align_rect_scaled(pos: Pos2, size: Vec2, rect_size: Vec2) -> (Rect, f32) {
    let scale = (size.x / rect_size.x).min(size.y / rect_size.y);
    let new_size = rect_size * scale;
    let offset = vec2((size.x - new_size.x) * 0.5, (size.y - new_size.y) * 0.5);
    (Rect::from_min_size(pos + offset, new_size), scale)
}

pub fn draw_pin_names<'a>(
    pos: Vec2isize,
    pins: impl Iterator<Item = (Vec2u, &'a str, Option<Direction4>)>,
    directional_offset: f32,
    directionless_offset: f32,
    ctx: &PaintContext,
) {
    fn draw_pin_name(
        name: &str,
        dir: Option<Direction4>,
        circ_pos: Vec2isize,
        pin_pos: Vec2u,
        directional_offset: f32,
        directionless_offset: f32,
        ctx: &PaintContext,
    ) {
        if name.is_empty() {
            return;
        }

        let galley = WidgetText::from(name).into_galley(
            ctx.ui,
            Some(false),
            f32::INFINITY,
            FontSelection::FontId(FontId::monospace(ctx.screen.scale * 0.5)),
        );

        //         n|
        //         i|
        //         P|
        //      +--*--+
        //      |  *  * Pin
        //  Pin * Pin |
        //      +--*--+
        //        |P
        //        |i          | marks text bottom
        //        |n

        let textsize = galley.size();
        let (dtx, dty, angle) = match dir {
            Some(Direction4::Up) => (-textsize.y * 0.5, -directional_offset, TAU * 0.75),
            Some(Direction4::Left) => (-textsize.x - directional_offset, -textsize.y * 0.5, 0.0),
            Some(Direction4::Down) => (textsize.y * 0.5, directional_offset, TAU * 0.25),
            Some(Direction4::Right) => (directional_offset, -textsize.y * 0.5, 0.0),

            None => (-textsize.x * 0.5, directionless_offset, 0.0),
        };

        let (drx, dry, vertical) = match dir {
            Some(Direction4::Up) => (-textsize.y * 0.5, -directional_offset - textsize.x, true),
            Some(Direction4::Left) => (-textsize.x - directional_offset, -textsize.y * 0.5, false),
            Some(Direction4::Down) => (-textsize.y * 0.5, directional_offset, true),
            Some(Direction4::Right) => (directional_offset, -textsize.y * 0.5, false),

            None => (-textsize.x * 0.5, directionless_offset, false),
        };

        let centerpos =
            Pos2::from(ctx.screen.world_to_screen(
                circ_pos.convert(|v| v as f32) + pin_pos.convert(|v| v as f32) + 0.5,
            ));

        let textpos = centerpos + vec2(dtx, dty);
        let rectpos = centerpos + vec2(drx, dry);
        let rectsize = match vertical {
            true => vec2(textsize.y, textsize.x),
            false => textsize,
        };
        let rect = Rect::from_min_size(rectpos, rectsize).expand(ctx.screen.scale * 0.1);

        let visual = &ctx.ui.style().visuals;
        ctx.paint.rect(
            rect,
            Rounding::same(ctx.screen.scale * 0.15),
            visual.window_fill.linear_multiply(0.6),
            visual.window_stroke,
        );
        ctx.paint.add(TextShape {
            pos: textpos,
            galley: galley.galley,
            underline: Stroke::NONE,
            override_text_color: Some(visual.text_color()),
            angle,
        });
    }

    for (pin_pos, pin_name, pin_dir) in pins {
        draw_pin_name(
            pin_name,
            pin_dir,
            pos,
            pin_pos,
            directional_offset * ctx.screen.scale,
            directionless_offset * ctx.screen.scale,
            ctx,
        );
    }
}
