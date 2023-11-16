use std::{
    f32::consts::TAU,
    num::NonZeroUsize,
    ops::Neg,
    sync::Arc,
};

use eframe::{
    egui::{
        self, FontSelection, Frame, Id, Margin, Response, Sense, TextStyle, TopBottomPanel, Ui,
        WidgetText,
    },
    epaint::{Galley, RectShape, Rounding, Shape, Stroke, TextShape},
};
use emath::{pos2, vec2, Pos2, Rangef, Rect, Vec2};

#[derive(Clone, Copy, PartialEq, Eq)]
#[allow(unused)]
pub enum PanelSide {
    Top,
    Left,
    Right,
    Bottom,
}

#[derive(Clone)]
struct SidePanelState {
    tab: Option<usize>,
    tab_width_cache: Vec<f32>,
}

#[derive(Clone)]
struct SidePanelUiState {
    frame: u64,
    rem_rect: Rect,
}

pub struct SidePanelResponse<T> {
    pub inner: Option<T>,
    pub response: Option<Response>,
}

pub struct SidePanel {
    side: PanelSide,
    id: Id,
    frame: Option<Frame>,
    resizable: bool,
    show_separator_line: bool,
    default_size: Option<f32>,
    size_range: Rangef,
    default_tab: Option<usize>,
    tab_spacing: Vec2,
    tab_inner_margin: Margin,
}

#[allow(dead_code)]
impl SidePanel {
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

    pub fn default_tab(self, tab: Option<usize>) -> Self {
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

    pub fn show<R: 'static>(
        self,
        ui: &mut Ui,
        tab_count: NonZeroUsize,
        tab_name: impl Fn(usize) -> WidgetText,
        tab_contents: impl FnOnce(usize, &mut Ui) -> R + 'static,
    ) -> SidePanelResponse<R> {
        let tab_names: Vec<_> = (0..tab_count.get())
            .map(|i| {
                tab_name(i)
                    .into_galley(
                        ui,
                        Some(false),
                        f32::INFINITY,
                        FontSelection::Style(TextStyle::Monospace),
                    )
                    .galley
            })
            .collect();
        let mut result = None;
        let response = self.show_dyn(
            ui,
            &tab_names,
            Box::new(|i, ui| {
                result = Some(tab_contents(i, ui));
            }),
        );
        SidePanelResponse {
            inner: result,
            response,
        }
    }

    #[allow(clippy::type_complexity)]
    pub fn show_dyn<'a>(
        self,
        ui: &mut Ui,
        tab_names: &[Arc<Galley>],
        tab_contents: Box<dyn FnOnce(usize, &mut Ui) + 'a>,
    ) -> Option<Response> {
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

        let state = ui.data(|data| data.get_temp::<SidePanelState>(id));

        let rem_rect = ui
            .data(|data| {
                data.get_temp::<SidePanelUiState>(ui.id().with("__side_panel_state"))
                    .and_then(|s| (s.frame == ui.ctx().frame_nr()).then_some(s.rem_rect))
            })
            .unwrap_or_else(|| ui.max_rect());

        let mut shape_idx: Vec<_> = (0..2 * tab_names.len())
            .map(|_| ui.painter().add(Shape::Noop))
            .collect();

        let mut active_tab = state.as_ref().map(|s| s.tab).unwrap_or(default_tab);

        let frame = frame.unwrap_or_else(|| Frame::side_top_panel(ui.style()));
        let empty_tab_size = match side {
            PanelSide::Top | PanelSide::Bottom => frame.inner_margin.sum().y,
            PanelSide::Left | PanelSide::Right => frame.inner_margin.sum().x,
        };
        let tab_width = match active_tab {
            None => Some(empty_tab_size),
            Some(index) => state
                .as_ref()
                .map(|s| s.tab_width_cache.get(index).copied().unwrap_or_default()),
        };

        let offset = tab_width
            .map(|w| {
                let target = if active_tab.is_some() { w } else { 0.0 };
                w - ui.ctx().animate_value_with_time(
                    id.with("__size_anim"),
                    target,
                    ui.style().animation_time,
                )
            })
            .unwrap_or_default();

        let panel_additional_size = offset.min(0.0).neg();
        let offset = offset.max(0.0);

        let size_range = match active_tab.is_some() {
            true => size_range,
            false => Rangef::new(panel_additional_size, f32::INFINITY),
        };

        let rect_offset = match side {
            PanelSide::Top => vec2(0.0, -offset),
            PanelSide::Left => vec2(-offset, 0.0),
            PanelSide::Right => vec2(offset, 0.0),
            PanelSide::Bottom => vec2(0.0, offset),
        };

        let panel_visible = active_tab.is_some() || !tab_width.is_some_and(|w| offset >= w);
        let panel_ui_rect = rem_rect.translate(rect_offset);

        let mut panel_ui = ui.child_ui_with_id_source(panel_ui_rect, *ui.layout(), id.with("ui"));
        let panel_id = panel_ui.id().with("panel");

        let inner_ui_translation = match side {
            PanelSide::Top => vec2(0.0, panel_additional_size),
            PanelSide::Left => vec2(panel_additional_size, 0.0),
            _ => vec2(0.0, 0.0),
        };

        let add_contents = |ui: &mut Ui| {
            let rect = ui.max_rect();
            let mut inner_ui = ui.child_ui(rect.translate(inner_ui_translation), *ui.layout());
            if let Some(tab) = active_tab.as_ref() {
                tab_contents(*tab, &mut inner_ui)
            }
            ui.expand_to_include_rect(inner_ui.min_rect());
        };

        let default_size = default_size.unwrap_or_else(|| {
            let vec = ui.style().spacing.interact_size;
            match side {
                PanelSide::Top | PanelSide::Bottom => vec.y,
                PanelSide::Left | PanelSide::Right => vec.x,
            }
        });

        let resp = if panel_visible {
            let resp = match side {
                PanelSide::Top => TopBottomPanel::top(panel_id)
                    .frame(frame)
                    .resizable(resizable)
                    .show_separator_line(show_separator_line)
                    .height_range(size_range)
                    .default_height(default_size)
                    .show_inside(&mut panel_ui, add_contents),
                PanelSide::Left => egui::SidePanel::left(panel_id)
                    .frame(frame)
                    .resizable(resizable)
                    .show_separator_line(show_separator_line)
                    .width_range(size_range)
                    .default_width(default_size)
                    .show_inside(&mut panel_ui, add_contents),
                PanelSide::Right => egui::SidePanel::right(panel_id)
                    .frame(frame)
                    .resizable(resizable)
                    .show_separator_line(show_separator_line)
                    .width_range(size_range)
                    .default_width(default_size)
                    .show_inside(&mut panel_ui, add_contents),
                PanelSide::Bottom => TopBottomPanel::bottom(panel_id)
                    .frame(frame)
                    .resizable(resizable)
                    .show_separator_line(show_separator_line)
                    .height_range(size_range)
                    .default_height(default_size)
                    .show_inside(&mut panel_ui, add_contents),
            };
            Some(resp.response)
        } else {
            None
        };

        let panel_rect = resp.as_ref().map(|r| r.rect).unwrap_or_else(|| match side {
            PanelSide::Top => Rect::from_min_size(
                panel_ui_rect.left_top(),
                vec2(panel_ui_rect.width(), empty_tab_size),
            ),
            PanelSide::Left => Rect::from_min_size(
                panel_ui_rect.left_top(),
                vec2(empty_tab_size, panel_ui_rect.height()),
            ),
            PanelSide::Right => Rect::from_min_size(
                panel_ui_rect.right_top() - vec2(empty_tab_size, 0.0),
                vec2(empty_tab_size, panel_ui_rect.height()),
            ),
            PanelSide::Bottom => Rect::from_min_size(
                panel_ui_rect.left_bottom() - vec2(0.0, empty_tab_size),
                vec2(panel_ui_rect.width(), empty_tab_size),
            ),
        });
        let mut response = resp;

        let size = match side {
            PanelSide::Top | PanelSide::Bottom => panel_rect.height(),
            PanelSide::Left | PanelSide::Right => panel_rect.width(),
        } - panel_additional_size;

        let (colmin, rowmin, colmax) = match side {
            PanelSide::Top => (
                rem_rect.left() + frame.outer_margin.left + frame.rounding.sw,
                panel_rect.bottom(),
                rem_rect.width() - frame.outer_margin.right - frame.rounding.se,
            ),
            PanelSide::Left => (
                rem_rect.top() + frame.outer_margin.top + frame.rounding.ne,
                panel_rect.right(),
                rem_rect.height() - frame.outer_margin.bottom - frame.rounding.se,
            ),
            PanelSide::Right => (
                rem_rect.top() + frame.outer_margin.top + frame.rounding.nw,
                panel_rect.left(),
                rem_rect.height() - frame.outer_margin.bottom - frame.rounding.sw,
            ),
            PanelSide::Bottom => (
                rem_rect.left() + frame.outer_margin.left + frame.rounding.nw,
                panel_rect.top(),
                rem_rect.width() - frame.outer_margin.right - frame.rounding.ne,
            ),
        };
        let mut pos = pos2(colmin, rowmin);
        let mut lineheight = 0.0f32;

        let tab_row_dir = match side {
            PanelSide::Left | PanelSide::Top => 1.0,
            PanelSide::Right | PanelSide::Bottom => -1.0,
        };

        for (i, tab_name) in tab_names.iter().enumerate() {
            let text_idx = unwrap_option_or_break!(shape_idx.pop());
            let rect_idx = unwrap_option_or_break!(shape_idx.pop());

            let tabsize = tab_name.size() + tab_inner_margin.sum();
            if pos.x + tabsize.x > colmax {
                pos.x = colmin;
                pos.y += (lineheight + tab_spacing.y) * tab_row_dir;
                lineheight = 0.0;
            } else if pos.x > colmin {
                pos.x += tab_spacing.x;
            }
            lineheight = lineheight.max(tabsize.y);

            let rect = match side {
                PanelSide::Top => Rect::from_min_size(pos, tabsize),
                PanelSide::Left => {
                    Rect::from_min_size(pos2(pos.y, pos.x), vec2(tabsize.y, tabsize.x))
                }
                PanelSide::Right => {
                    Rect::from_min_size(pos2(pos.y - tabsize.y, pos.x), vec2(tabsize.y, tabsize.x))
                }
                PanelSide::Bottom => Rect::from_min_size(pos2(pos.x, pos.y - tabsize.y), tabsize),
            };

            let (textpos, textangle) = match side {
                PanelSide::Top => (rect.left_top() + tab_inner_margin.left_top(), 0.0),
                PanelSide::Left => (
                    rect.left_top() + tab_inner_margin.left_top() + vec2(tab_name.size().y, 0.0),
                    TAU * 0.25,
                ),
                PanelSide::Right => (
                    rect.left_top() + tab_inner_margin.left_top() + vec2(0.0, tab_name.size().x),
                    TAU * 0.75,
                ),
                PanelSide::Bottom => (rect.left_top() + tab_inner_margin.left_top(), 0.0),
            };

            let this_tab_active = active_tab == Some(i);
            let text_color = panel_ui
                .style()
                .visuals
                .text_color()
                .gamma_multiply(if this_tab_active { 1.0 } else { 0.6 });

            let text = TextShape {
                pos: textpos,
                galley: tab_name.clone(),
                underline: Stroke::NONE,
                override_text_color: Some(text_color),
                angle: textangle,
            };

            let tabresp = panel_ui.interact(rect, id.with(("tab", i)), Sense::click());
            if tabresp.clicked() {
                if this_tab_active {
                    active_tab = None;
                } else {
                    active_tab = Some(i);
                }
            }

            let rect = match side {
                PanelSide::Top => Rect {
                    min: Pos2 {
                        y: panel_rect.bottom() - 2.0,
                        ..rect.min
                    },
                    ..rect
                },
                PanelSide::Left => Rect {
                    min: Pos2 {
                        x: panel_rect.right() - 2.0,
                        ..rect.min
                    },
                    ..rect
                },
                PanelSide::Right => Rect {
                    max: Pos2 {
                        x: panel_rect.left() + 2.0,
                        ..rect.max
                    },
                    ..rect
                },
                PanelSide::Bottom => Rect {
                    max: Pos2 {
                        y: panel_rect.top() + 2.0,
                        ..rect.max
                    },
                    ..rect
                },
            };

            let rounding = 5.0;

            let rounding = match side {
                PanelSide::Top => Rounding {
                    se: rounding,
                    sw: rounding,
                    ..Default::default()
                },
                PanelSide::Left => Rounding {
                    se: rounding,
                    ne: rounding,
                    ..Default::default()
                },
                PanelSide::Right => Rounding {
                    sw: rounding,
                    nw: rounding,
                    ..Default::default()
                },
                PanelSide::Bottom => Rounding {
                    ne: rounding,
                    nw: rounding,
                    ..Default::default()
                },
            };
            let style = panel_ui.style();
            let rect_fill = if this_tab_active || tabresp.hovered() {
                style.visuals.panel_fill.linear_multiply(1.4)
            } else {
                style.visuals.panel_fill
            };

            let rect_stroke = if this_tab_active || tabresp.hovered() {
                style.visuals.window_stroke.color.linear_multiply(1.4)
            } else {
                style.visuals.window_stroke.color
            };

            panel_ui.painter().set(
                rect_idx,
                Shape::Rect(RectShape::new(
                    rect,
                    rounding,
                    rect_fill,
                    Stroke::new(1.0, rect_stroke),
                )),
            );
            panel_ui.painter().set(text_idx, Shape::Text(text));
            pos.x += tabsize.x;
            response = match response {
                Some(response) => Some(response.union(tabresp)),
                None => Some(tabresp),
            };
        }

        let mut width_cache = state.map(|s| s.tab_width_cache).unwrap_or_default();

        if let Some(index) = active_tab {
            while width_cache.len() <= index {
                width_cache.push(0.0);
            }
            if (width_cache[index] - size).abs() > 0.1 {
                width_cache[index] = size;
            }
        }

        let state = SidePanelState {
            tab: active_tab,
            tab_width_cache: width_cache,
        };

        ui.data_mut(|data| data.insert_temp(id, state));

        response
    }
}
