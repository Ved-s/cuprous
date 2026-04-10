use std::any::Any;

use crate::{containers::FixedVec, state::wires::WireState};

#[derive(Default)]
pub struct ComponentState {
    pub pins: Vec<WireState>,
    pub internal: Option<Box<dyn Any + Send + Sync>>,
}

#[derive(Default)]
pub struct BoardComponentsState {
    pub inner: FixedVec<ComponentState>,
}

impl BoardComponentsState {
    pub fn get_pin(&self, component: usize, id: usize) -> WireState {
        self.inner
            .get(component)
            .and_then(|c| c.pins.get(id).cloned())
            .unwrap_or_default()
    }

    /// Returns true if value was changed
    pub fn set_pin(&mut self, component: usize, id: usize, state: WireState) -> bool {
        let component = self.inner.get_or_create_mut(component, Default::default);

        if component.pins.len() <= id {
            if state == WireState::default() {
                return false;
            }

            let add = id - component.pins.len() + 1;
            component.pins.reserve(add);
            for _ in 0..add {
                component.pins.push(WireState::default());
            }
        }

        if component.pins[id] == state {
            return false;
        }

        component.pins[id] = state;
        true
    }

    pub fn read_internal_component_state<S>(&self, id: usize) -> Option<&S>
    where
        S: 'static,
    {
        let component = self.inner.get(id)?;
        let internal = component.internal.as_ref()?.downcast_ref()?;
        Some(internal)
    }

    pub fn write_internal_component_state<S>(&mut self, id: usize) -> &mut S
    where
        S: Default + Send + Sync + 'static,
    {
        let existing = self.inner.get_or_create_mut(id, Default::default);

        let state = existing
            .internal
            .get_or_insert_with(|| Box::new(<S as Default>::default()));

        if state.downcast_ref::<S>().is_none() {
            *state = Box::new(<S as Default>::default());
        }

        state.downcast_mut().unwrap()
    }

    pub fn drop_component(&mut self, id: usize, pin: Option<usize>) {
        match pin {
            Some(p) => {
                if let Some(component) = self.inner.get_mut(id)
                    && let Some(pin) = component.pins.get_mut(p)
                {
                    *pin = WireState::None;
                }
            }
            None => {
                self.inner.remove(id);
            }
        }
    }

    pub fn reset(&mut self) {
        self.inner.clear();
    }
}
