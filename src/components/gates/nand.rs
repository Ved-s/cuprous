use std::ops::Div;

use eframe::{
    egui::{Color32, Stroke},
    epaint::{PathShape, PathStroke},
};

use crate::{
    components::ComponentRenderingContext,
    path::{Path, PointPath},
};

use super::{GateImpl, GateOutput};

#[derive(Clone)]
pub struct Nand;

impl GateImpl for Nand {
    fn id() -> &'static str {
        "gate_nand"
    }

    fn display_name() -> &'static str {
        "NAND gate"
    }

    fn init_state() -> bool {
        false
    }

    fn fold(_: &mut bool, input: bool) -> GateOutput {
        if !input {
            GateOutput {
                out: true,
                fin: true,
            }
        } else {
            GateOutput {
                out: false,
                fin: false,
            }
        }
    }

    fn draw(ctx: &ComponentRenderingContext) {
        let size = ctx.world_size().convert(|v| v as f32);

        let border_color = Color32::BLACK;
        let fill_color = Color32::from_gray(200);
        let straightness = (0.3 / (ctx.paint.screen.scale.sqrt()))
            .div(size.y)
            .max(0.02);

        let path = PointPath::new(0.5, 0.0)
            .line_to(size.x * 0.4, 0.0)
            .quadratic_bezier(
                size.x - 0.75,
                0.0,
                size.x - 0.75,
                size.y / 2.0,
                straightness,
            )
            .quadratic_bezier(size.x - 0.75, size.y, size.x * 0.4, size.y, straightness)
            .line_to(0.5, size.y);

        let points = path
            .iter_points(|v| ctx.transform_pos(v))
            .map(Into::into)
            .collect();

        let path = PathShape {
            points,
            closed: true,
            fill: fill_color,
            stroke: PathStroke::new(0.15 * ctx.paint.screen.scale, border_color),
        };

        ctx.paint.painter.add(path);

        let circle_pos = ctx.transform_pos([size.x - 0.68, size.y / 2.0].into());
        ctx.paint.circle(
            circle_pos.into(),
            0.2 * ctx.paint.screen.scale,
            fill_color,
            Stroke::new(0.15 * ctx.paint.screen.scale, border_color),
        );
    }
}
