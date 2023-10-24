use std::sync::Arc;

use eframe::{
    egui::{self, Frame, Key, Margin, SidePanel, TextStyle, Ui},
    epaint::{Color32, FontId, Rounding, Stroke},
};
use emath::{vec2, Align2, Rect};

use crate::{
    board::{CircuitDesign, CircuitDesignStorage},
    circuits::InternalPinDirection,
    Direction4, DynStaticStr, PaintContext, PanAndZoom, RwLock, vector::Vec2f,
};

use super::{
    drawing,
    selection::{Selection, SelectionImpl, SelectionInventoryItem},
    CollapsibleSidePanel, Inventory, InventoryItemGroup, Sides,
};

pub struct DesignerResponse {
    pub close: bool,
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum SelectedItemId {
    Selection,
}

#[derive(PartialEq, Eq, Hash)]
enum SelectedDesignObject {}

pub struct CircuitDesignPinInfo {
    pub dir: InternalPinDirection,
    pub display_dir: Option<Direction4>,
    pub display_name: DynStaticStr,
}

pub trait DesignProvider {
    fn get_storage(&self) -> Arc<RwLock<CircuitDesignStorage>>;
    fn get_pin_ids(&self) -> Vec<DynStaticStr>;
    fn get_pin(&self, id: DynStaticStr) -> Option<CircuitDesignPinInfo>;
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
    }

    fn draw_object_selection(
        &mut self,
        pass: &CircuitDesign,
        object: &SelectedDesignObject,
        ctx: &crate::PaintContext,
    ) {
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

        let mut designs = self.storage.write();
        let design = designs.current_mut();

        let world_font = FontId::new(screen.scale * 0.5, eframe::epaint::FontFamily::Monospace);

        let color = Color32::from_rgb(134, 114, 135);

        let mut size_rect = self.circuit_size_rect.unwrap_or_else(|| {
            Rect::from_min_size(
                screen.world_to_screen(0.0.into()).into(),
                (design.size.convert(|v| v as f32) * screen.scale).into(),
            )
        });

        let sides = super::rect_resizer(
            &mut size_rect,
            Sides {
                top: false,
                left: false,
                right: true,
                bottom: true,
            },
            ui,
            ui.id().with("circuit_resize"),
            |_, rect| {
                rect.min = screen.world_to_screen(0.0.into()).into();
            },
            color.linear_multiply(0.3),
            Stroke::new(2.0, color),
        );

        if sides.any() {
            self.circuit_size_rect = Some(size_rect);
            design.size = (Vec2f::from(size_rect.size()) / screen.scale).convert(|v| (v.round() as u32).max(1))
        }
        else {
            self.circuit_size_rect = None;
        }

        // paint.rect(
        //     size_rect,
        //     Rounding::none(),
        //     color.linear_multiply(0.3),
        //     Stroke::new(2.0, color),
        // );
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

        self.selection.update_selection(&ctx);

        drop(designs);

        let components_rect = self.components_ui(ui);

        let mut rect = ui.clip_rect();
        rect.min.x += components_rect.width();
        rect = rect.shrink(10.0);
        let mut ui = ui.child_ui(rect, *ui.layout());

        if ui.input(|input| input.key_pressed(Key::Escape)) {
            self.selected_id = None;
        }

        let close = ui.button("Exit designer").clicked();

        Inventory::new(&mut self.selected_id, &self.inventory).ui(&mut ui, |id| match id {
            SelectedItemId::Selection => Some("Selection".into()),
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
            })
            .full_rect
    }
}
