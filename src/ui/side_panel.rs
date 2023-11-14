use std::f32::consts::TAU;

use eframe::{
    egui::{
        self, FontSelection, Frame, Id, InnerResponse, Margin, Sense, TextStyle, TopBottomPanel,
        Ui, WidgetText,
    },
    epaint::{RectShape, Rounding, Shape, Stroke, TextShape},
};
use emath::{pos2, vec2, Rangef, Rect, Vec2, Pos2};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum PanelSide {
    Top,
    Left,
    Right,
    Bottom,
}

#[derive(Clone)]
struct SidePanelState<T> {
    tab: Option<T>,
    size: f32,
}

#[derive(Clone)]
struct SidePanelUiState {
    frame: u64,
    rem_rect: Rect,
}

pub struct SidePanel<T>
where
    T: Clone + Eq + 'static,
{
    side: PanelSide,
    id: Id,
    frame: Option<Frame>,
    resizable: bool,
    show_separator_line: bool,
    default_size: Option<f32>,
    size_range: Rangef,
    default_tab: Option<T>,
    tab_spacing: Vec2,
    tab_inner_margin: Margin,
}

impl<T> SidePanel<T>
where
    T: Clone + Eq + 'static,
{
    pub fn new(side: PanelSide, id: impl Into<Id>) -> Self {
        Self {
            side,
            id: id.into(),
            frame: None,
            resizable: false,
            show_separator_line: true,
            default_size: None,
            size_range: Rangef::new(20.0, f32::INFINITY),
            default_tab: None,
            tab_spacing: vec2(5.0, 1.0),
            tab_inner_margin: Margin::same(5.0),
        }
    }

    pub fn frame(self, frame: Frame) -> Self {
        Self {
            frame: Some(frame),
            ..self
        }
    }

    pub fn resizable(self, resizable: bool) -> Self {
        Self { resizable, ..self }
    }

    pub fn show_separator_line(self, show_separator_line: bool) -> Self {
        Self {
            show_separator_line,
            ..self
        }
    }

    pub fn default_size(self, default_size: f32) -> Self {
        Self {
            default_size: Some(default_size),
            ..self
        }
    }

    pub fn size_range(self, size_range: Rangef) -> Self {
        Self { size_range, ..self }
    }

    pub fn default_tab(self, tab: Option<T>) -> Self {
        Self {
            default_tab: tab,
            ..self
        }
    }

    pub fn tab_spacing(self, tab_spacing: Vec2) -> Self {
        Self {
            tab_spacing,
            ..self
        }
    }

    pub fn tab_inner_margin(self, tab_inner_margin: Margin) -> Self {
        Self {
            tab_inner_margin,
            ..self
        }
    }

    pub fn show<'a, R>(
        self,
        ui: &mut Ui,
        tabs: &[T],
        tab_name: impl Fn(&T) -> WidgetText,
        tab_contents: impl FnOnce(&T, &mut Ui) -> R,
    ) -> InnerResponse<Option<R>> {
        let Self {
            side,
            default_tab,
            frame,
            id,
            resizable,
            show_separator_line,
            default_size,
            size_range,
            tab_spacing,
            tab_inner_margin,
        } = self;

        let state = ui.data(|data| data.get_temp::<SidePanelState<T>>(id));

        let rem_rect = ui
            .data(|data| {
                data.get_temp::<SidePanelUiState>(ui.id().with("__side_panel_state"))
                    .and_then(|s| (s.frame == ui.ctx().frame_nr()).then_some(s.rem_rect))
            })
            .unwrap_or_else(|| ui.max_rect());

        let mut shape_idx: Vec<_> = (0..2 * tabs.len())
            .map(|_| ui.painter().add(Shape::Noop))
            .collect();

        let panel_ui_rect = rem_rect;

        let panel_ui = ui.child_ui_with_id_source(panel_ui_rect, *ui.layout(), id.with("ui"));
        let panel_id = panel_ui.id().with("panel");

        let active_tab = state.map(|s| s.tab.clone()).unwrap_or(default_tab);
        let active_tab = active_tab.as_ref();

        let add_contents = move |ui: &mut Ui| active_tab.map(|tab| tab_contents(tab, ui));
        let frame = frame.unwrap_or_else(|| Frame::side_top_panel(ui.style()));

        let default_size = default_size.unwrap_or_else(|| {
            let vec = ui.style().spacing.interact_size;
            match side {
                PanelSide::Top | PanelSide::Bottom => vec.y,
                PanelSide::Left | PanelSide::Right => vec.x,
            }
        });

        let mut resp = match side {
            PanelSide::Top => TopBottomPanel::top(panel_id)
                .frame(frame)
                .resizable(resizable)
                .show_separator_line(show_separator_line)
                .height_range(size_range)
                .default_height(default_size)
                .show_inside(ui, add_contents),
            PanelSide::Left => egui::SidePanel::left(panel_id)
                .frame(frame)
                .resizable(resizable)
                .show_separator_line(show_separator_line)
                .width_range(size_range)
                .default_width(default_size)
                .show_inside(ui, add_contents),
            PanelSide::Right => egui::SidePanel::right(panel_id)
                .frame(frame)
                .resizable(resizable)
                .show_separator_line(show_separator_line)
                .width_range(size_range)
                .default_width(default_size)
                .show_inside(ui, add_contents),
            PanelSide::Bottom => TopBottomPanel::bottom(panel_id)
                .frame(frame)
                .resizable(resizable)
                .show_separator_line(show_separator_line)
                .height_range(size_range)
                .default_height(default_size)
                .show_inside(ui, add_contents),
        };

        let panel_rect = resp.response.rect;

        let size = match side {
            PanelSide::Top | PanelSide::Bottom => resp.response.rect.height(),
            PanelSide::Left | PanelSide::Right => resp.response.rect.width(),
        };

        let (colmin, colmax) = match side {
            PanelSide::Top => (
                rem_rect.left() + frame.outer_margin.left + frame.rounding.sw,
                rem_rect.width() - frame.outer_margin.right - frame.rounding.se,
            ),
            PanelSide::Left => (
                rem_rect.top() + frame.outer_margin.top + frame.rounding.ne,
                rem_rect.height() - frame.outer_margin.bottom - frame.rounding.se,
            ),
            PanelSide::Right => (
                rem_rect.top() + frame.outer_margin.top + frame.rounding.nw,
                rem_rect.height() - frame.outer_margin.bottom - frame.rounding.sw,
            ),
            PanelSide::Bottom => (
                rem_rect.left() + frame.outer_margin.left + frame.rounding.nw,
                rem_rect.width() - frame.outer_margin.right - frame.rounding.ne,
            ),
        };
        let mut pos = pos2(colmin, size);
        let mut lineheight = 0.0f32;

        for (i, tab) in tabs.iter().enumerate() {
            let text_idx = unwrap_option_or_break!(shape_idx.pop());
            let rect_idx = unwrap_option_or_break!(shape_idx.pop());

            let galley = tab_name(tab)
                .into_galley(
                    ui,
                    Some(false),
                    f32::INFINITY,
                    FontSelection::Style(TextStyle::Monospace),
                )
                .galley;

            let tabsize = galley.size() + tab_inner_margin.sum();
            if pos.x + tabsize.x > colmax {
                pos.x = colmin;
                pos.y += lineheight + tab_spacing.y;
                lineheight = 0.0;
            } else if pos.x > colmin {
                pos.x += tab_spacing.x;
            }
            lineheight = lineheight.max(tabsize.y);

            let rect = match side {
                PanelSide::Left | PanelSide::Right => {
                    Rect::from_min_size(pos2(pos.y, pos.x), vec2(tabsize.y, tabsize.x))
                }
                PanelSide::Top | PanelSide::Bottom => Rect::from_min_size(pos, tabsize),
            };

            let (textpos, textangle) = match side {
                PanelSide::Top => (rect.left_top() + tab_inner_margin.left_top(), 0.0),
                PanelSide::Left => (
                    rect.left_top() + tab_inner_margin.left_top() + vec2(galley.size().y, 0.0),
                    TAU * 0.25,
                ),
                PanelSide::Right => todo!(),
                PanelSide::Bottom => todo!(),
            };

            let text_color = ui.style().visuals.text_color().gamma_multiply(
                if active_tab.is_some_and(|a| *a == *tab) {
                    1.0
                } else {
                    0.6
                },
            );

            let text = TextShape {
                pos: textpos,
                galley,
                underline: Stroke::NONE,
                override_text_color: Some(text_color),
                angle: textangle,
            };

            let tabresp = ui.interact(rect, id.with(("tab", i)), Sense::click());
            resp.response = resp.response.union(tabresp);

            let rect = match side {
                PanelSide::Top => Rect { min: Pos2 { y: panel_rect.bottom(), ..rect.min}, ..rect},
                PanelSide::Left => Rect { min: Pos2 { x: panel_rect.right(), ..rect.min}, ..rect},
                PanelSide::Right => Rect { max: Pos2 { x: panel_rect.left(), ..rect.max}, ..rect},
                PanelSide::Bottom => Rect { max: Pos2 { y: panel_rect.top(), ..rect.max}, ..rect},
            };

            let rounding = 5.0;

            let rounding = match side {
                PanelSide::Top => Rounding { se: rounding, sw: rounding, ..Default::default() },
                PanelSide::Left => Rounding { se: rounding, ne: rounding, ..Default::default() },
                PanelSide::Right => Rounding { sw: rounding, nw: rounding, ..Default::default() },
                PanelSide::Bottom => Rounding { ne: rounding, nw: rounding, ..Default::default() },
            };

            ui.painter().set(
                rect_idx,
                Shape::Rect(RectShape::new(
                    rect,
                    rounding,
                    ui.style().visuals.panel_fill,
                    ui.style().visuals.window_stroke,
                )),
            );
            ui.painter().set(text_idx, Shape::Text(text));
            pos.x += tabsize.x;
        }

        resp
    }
}
