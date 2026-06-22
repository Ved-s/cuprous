use std::sync::Arc;

use eframe::{
    egui::{Align, Color32, FontId, Stroke, StrokeKind, text::LayoutJob, vec2},
    epaint::TextShape,
};
use smoldata::raw::RawValue;

use crate::{
    components::{ComponentImpl, ComponentTransformSupport, PinType},
    str::{ArcRefStr, ArcStaticStr},
    vector::Vec2usize,
};

use super::{
    Component, ComponentCtx, ComponentRenderingContext, ComponentTransform, ComponentUpdateReason,
    PinDescription,
};

#[derive(Default)]
pub struct UnloadedComponentState {
    data: Option<RawValue>,
}

pub struct UnloadedComponentInstance {
    data: Option<RawValue>,
}

#[derive(Clone)]
pub struct UnloadedComponent {
    data: Option<RawValue>,
    discovered_pins: Vec<PinDescription>,
    id: ArcStaticStr,
    size: Vec2usize,
}

impl UnloadedComponent {
    pub fn new(id: ArcStaticStr) -> Self {
        Self {
            data: None,
            discovered_pins: vec![],
            id,
            size: 1.into(),
        }
    }

    pub fn add_pin(&mut self, id: ArcRefStr, pos: Vec2usize) {
        for p in &self.discovered_pins {
            if p.id == id {
                return;
            }
        }

        let id = ArcStaticStr::from(Arc::from(id));

        self.discovered_pins.push(PinDescription {
            pos,
            id: id.clone(),
            display_name: id,
            dir: None,
            ty: PinType::Inside,
        });

        if pos.x >= self.size.x || pos.y >= self.size.y {
            self.size.x = self.size.x.max(pos.x + 1);
            self.size.y = self.size.y.max(pos.y + 1);
        }
    }

    pub fn has_pin(&self, id: &str) -> bool {
        self.discovered_pins.iter().any(|p| p.id == id)
    }

    // todo: after loading all referenced pins, set their `dir`, requires info overlay when hovering components to be useful
}

impl ComponentImpl for UnloadedComponent {
    type State = UnloadedComponentState;

    type Instance = UnloadedComponentInstance;

    fn id(&self) -> ArcStaticStr {
        self.id.clone()
    }

    fn display_name(&self) -> ArcStaticStr {
        self.id.clone()
    }

    fn transform_support(&self) -> ComponentTransformSupport {
        ComponentTransformSupport {
            rotation: None,
            flip: None,
        }
    }

    fn size(&self, _transform: ComponentTransform) -> Vec2usize {
        self.size
    }

    fn describe_pins(&self, _transform: ComponentTransform) -> Box<[PinDescription]> {
        self.discovered_pins.clone().into_boxed_slice()
    }

    fn draw(&self, _component: Option<ComponentCtx<Self>>, render: &ComponentRenderingContext) {
        render.paint.rect(
            render.screen_rect.expand(render.paint.screen.scale * -0.5),
            0.0,
            Color32::from_gray(64),
            Stroke::new(0.05 * render.paint.screen.scale, Color32::from_rgb(220, 92, 92)),
            StrokeKind::Middle,
        );

        if render.paint.screen.scale < 1.0 {
            return;
        }

        let mut layout = LayoutJob::simple(
            format!("Unloaded component: {}", self.id),
            FontId::monospace(render.paint.screen.scale * 0.8),
            Color32::WHITE,
            render.screen_rect.width(),
        );
        layout.halign = Align::Center;

        let text = render.paint.layout_job(layout);

        render.paint.add(TextShape::new(
            render.screen_rect.center() - text.size() * vec2(0.0, 0.5),
            text,
            Color32::WHITE,
        ));
    }

    fn create_instance(&self, _component: &Arc<Component>) -> Self::Instance {
        Self::Instance { data: None }
    }

    fn update(&self, _ctx: ComponentCtx<Self>, _reason: ComponentUpdateReason) {}

    fn load_config(&mut self, data: &RawValue) -> Result<(), eyre::Report> {
        self.data = Some(data.clone());
        Ok(())
    }

    fn load_instance(
        &self,
        _component: &Arc<Component>,
        data: &RawValue,
    ) -> Result<Self::Instance, eyre::Report> {
        Ok(Self::Instance {
            data: Some(data.clone()),
        })
    }

    fn load_state(
        &self,
        _component: &Arc<Component>,
        _instance: &Self::Instance,
        data: &RawValue,
    ) -> Result<Self::State, eyre::Report> {
        Ok(Self::State {
            data: Some(data.clone()),
        })
    }

    fn save_config(&self) -> Option<RawValue> {
        self.data.clone()
    }

    fn save_instance(&self, _component: &Component, instance: &Self::Instance) -> Option<RawValue> {
        instance.data.clone()
    }

    fn save_state(
        &self,
        _component: &Component,
        _instance: &Self::Instance,
        state: &Self::State,
    ) -> Option<RawValue> {
        state.data.clone()
    }
}
