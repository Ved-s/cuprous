use std::{
    any::{Any, TypeId},
    collections::HashMap,
};

use eframe::egui::{Ui, ComboBox};

use crate::{unwrap_option_or_return, Direction4, DynStaticStr, RwLock, unwrap_option_or_continue};

#[derive(Default)]
pub struct CircuitPropertyStore(RwLock<HashMap<DynStaticStr, CircuitProperty>>);

impl CircuitPropertyStore {
    pub fn new(props: impl IntoIterator<Item = CircuitProperty>) -> Self {
        Self(RwLock::new(HashMap::from_iter(
            props.into_iter().map(|p| (p.id(), p)),
        )))
    }

    pub fn read_clone<T: CircuitPropertyImpl + Clone>(&self, name: &str) -> Option<T> {
        self.read(name, Clone::clone)
    }

    pub fn read<T, F, R>(&self, id: &str, f: F) -> Option<R>
    where
        T: CircuitPropertyImpl,
        F: FnOnce(&T) -> R,
    {
        let lock = self.0.read();
        let p = lock.get(id)?.imp.as_ref();
        p.downcast_ref::<T>().map(f)
    }

    pub fn read_dyn<F, R>(&self, id: &str, f: F) -> Option<R>
    where
        F: FnOnce(&CircuitProperty) -> R,
    {
        let lock = self.0.read();
        let p = lock.get(id)?;
        Some(f(p))
    }

    /// Will not write if property doesn't exist
    pub fn write<T, F, R>(&self, id: &str, f: F) -> Option<R>
    where
        T: CircuitPropertyImpl,
        F: FnOnce(&mut T) -> R,
    {
        let mut lock = self.0.write();
        let p = lock.get_mut(id);
        let p = &mut unwrap_option_or_return!(p, None).imp;
        p.downcast_mut::<T>().map(f)
    }

    /// Will not write if property doesn't exist
    pub fn write_dyn<F, R>(&self, id: &str, f: F) -> Option<R>
    where
        F: FnOnce(&mut CircuitProperty) -> R,
    {
        let mut lock = self.0.write();
        lock.get_mut(id).map(f)
    }

    pub fn save(&self) -> crate::io::CircuitPropertyStoreData {
        crate::io::CircuitPropertyStoreData(HashMap::from_iter(
            self.0
                .read()
                .values()
                .map(|p| (p.id.clone(), p.imp.save())),
        ))
    }

    pub fn load(
        &self,
        data: &crate::io::CircuitPropertyStoreData
    ) {
        let mut lock = self.0.write();
        for (id, data) in data.0.iter() {
            let prop = lock.get_mut(id);
            let prop = unwrap_option_or_continue!(prop);
            prop.imp.load(data);
        }
    }

    pub fn has_property(&self, id: &str, ty: Option<TypeId>) -> bool {
        let lock = self.0.read();
        match ty {
            Some(ty) => lock.get(id).is_some_and(|v| v.imp().type_id() == ty),
            None => lock.contains_key(id),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.0.read().is_empty()
    }

    pub fn inner(&self) -> &RwLock<HashMap<DynStaticStr, CircuitProperty>> {
        &self.0
    }
}

impl Clone for CircuitPropertyStore {
    fn clone(&self) -> Self {
        Self(RwLock::new(HashMap::from_iter(
            self.0.read().values().map(|p| {
                let clone = p.clone();
                (clone.id(), clone)
            }),
        )))
    }
}

pub trait CircuitPropertyImpl: Any + Send + Sync {
    fn equals(&self, other: &dyn CircuitPropertyImpl) -> bool;

    /// Draw ui for property.
    /// Return old value if value changed
    fn ui(&mut self, ui: &mut Ui, not_equal: bool) -> Option<Box<dyn CircuitPropertyImpl>>;
    fn clone(&self) -> Box<dyn CircuitPropertyImpl>;
    fn load(&mut self, data: &serde_intermediate::Intermediate);
    fn save(&self) -> serde_intermediate::Intermediate;
    fn copy_into(&self, other: &mut dyn CircuitPropertyImpl);
}

impl dyn CircuitPropertyImpl {

    #[inline]
    pub fn is<T: CircuitPropertyImpl>(&self) -> bool {
        let t = TypeId::of::<T>();
        let concrete = self.type_id();
        t == concrete
    }

    #[inline]
    pub fn downcast_ref<T: CircuitPropertyImpl>(&self) -> Option<&T> {
        if self.is::<T>() {
            Some(unsafe { &*(self as *const dyn CircuitPropertyImpl as *const T) })
        } else {
            None
        }
    }

    #[inline]
    pub fn downcast_mut<T: CircuitPropertyImpl>(&mut self) -> Option<&mut T> {
        if self.is::<T>() {
            Some(unsafe { &mut *(self as *mut dyn CircuitPropertyImpl as *mut T) })
        } else {
            None
        }
    }

    pub fn is_type_and<T, F>(&self, f: F) -> bool
    where
        T: CircuitPropertyImpl,
        F: FnOnce(&T) -> bool,
    {
        self.downcast_ref::<T>().map(f).unwrap_or(false)
    }

    pub fn map_type<T, O, F>(&self, f: F) -> Option<O>
    where
        T: CircuitPropertyImpl,
        F: FnOnce(&T) -> O,
    {
        self.downcast_ref::<T>().map(f)
    }
}

pub struct CircuitProperty {
    imp: Box<dyn CircuitPropertyImpl>,
    id: DynStaticStr,
    name: DynStaticStr
}

impl CircuitProperty {
    pub fn new<T: CircuitPropertyImpl>(id: impl Into<DynStaticStr>, name: impl Into<DynStaticStr>, prop: T) -> Self {
        Self::new_dynamic(id, name, Box::new(prop))
    }

    pub fn new_dynamic(id: impl Into<DynStaticStr>, name: impl Into<DynStaticStr>, prop: Box<dyn CircuitPropertyImpl>) -> Self {
        Self {
            imp: prop,
            id: id.into(),
            name: name.into(),
        }
    }

    pub fn new_default<T: CircuitPropertyImpl + Default>(id: impl Into<DynStaticStr>, name: impl Into<DynStaticStr>) -> Self {
        Self::new(id, name, T::default())
    }

    pub fn imp(&self) -> &dyn CircuitPropertyImpl {
        self.imp.as_ref()
    }

    pub fn imp_mut(&mut self) -> &mut dyn CircuitPropertyImpl {
        self.imp.as_mut()
    }

    pub fn id(&self) -> DynStaticStr {
        self.id.clone()
    }

    pub fn name(&self) -> DynStaticStr {
        self.name.clone()
    }
}

impl Clone for CircuitProperty {
    fn clone(&self) -> Self {
        Self { imp: self.imp.clone(), id: self.id.clone(), name: self.name.clone() }
    }
}

impl CircuitPropertyImpl for Direction4 {
    fn equals(&self, other: &dyn CircuitPropertyImpl) -> bool {
        other.is_type_and(|o: &Self| o == self)
    }

    fn ui(&mut self, ui: &mut Ui, not_equal: bool) -> Option<Box<dyn CircuitPropertyImpl>> {
        let old = *self;
        let mut changed = false;
        ComboBox::from_id_source("dir4_ui")
        .selected_text(if not_equal { Default::default() } else { self.name() })
        .show_ui(ui, |ui| {
            for dir in Direction4::iter_all() {
                let res = ui.selectable_value(self, dir, dir.name());
                if res.changed() || res.clicked() {
                    changed = true;
                }
            }
        });
        changed.then(|| Box::new(old) as Box<dyn CircuitPropertyImpl>)
    }

    fn clone(&self) -> Box<dyn CircuitPropertyImpl> {
        Box::new(Clone::clone(self))
    }

    fn save(&self) -> serde_intermediate::Intermediate {
        serde_intermediate::to_intermediate(&self).unwrap_or_default()
    }

    fn load(&mut self, data: &serde_intermediate::Intermediate) {
        if let Ok(d) = serde_intermediate::de::intermediate::deserialize(data) {
            *self = d;
        }
    }

    fn copy_into(&self, other: &mut dyn CircuitPropertyImpl) {
        if let Some(r) = other.downcast_mut() {
            *r = *self;
        }
    }
}
