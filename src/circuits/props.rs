use std::{
    any::{Any, TypeId},
    collections::HashMap,
};

use eframe::egui::Ui;

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
        let lock = self.0.read().unwrap();
        let b = lock.get(id)?.imp.as_ref();
        b.downcast_ref::<T>().map(f)
    }

    /// Will not write if property doesn't exist
    pub fn write<T, F>(&self, id: &str, f: F)
    where
        T: CircuitPropertyImpl,
        F: FnOnce(&mut T),
    {
        let mut lock = self.0.write().unwrap();
        let b = lock.get_mut(id);
        let b = &mut unwrap_option_or_return!(b).imp;
        if let Some(t) = b.downcast_mut::<T>() {
            f(t);
        }
    }

    pub fn save(&self) -> crate::io::CircuitPropertyStoreData {
        crate::io::CircuitPropertyStoreData(HashMap::from_iter(
            self.0
                .read()
                .unwrap()
                .values()
                .map(|p| (p.id.clone(), p.imp.save())),
        ))
    }

    pub fn load(
        &self,
        data: &crate::io::CircuitPropertyStoreData
    ) {
        let mut lock = self.0.write().unwrap();
        for (id, data) in data.0.iter() {
            let prop = lock.get_mut(id);
            let prop = unwrap_option_or_continue!(prop);
            prop.imp.load(data);
        }
    }

    pub fn is_empty(&self) -> bool {
        self.0.read().unwrap().is_empty()
    }
}

impl Clone for CircuitPropertyStore {
    fn clone(&self) -> Self {
        Self(RwLock::new(HashMap::from_iter(
            self.0.read().unwrap().values().map(|p| {
                let clone = p.clone();
                (clone.id(), clone)
            }),
        )))
    }
}

pub trait CircuitPropertyImpl: Any + Send + Sync {
    fn equals(&self, other: &dyn CircuitPropertyImpl) -> bool;
    fn ui(&mut self, ui: &mut Ui, changed: &mut bool);
    fn clone(&self) -> Box<dyn CircuitPropertyImpl>;
    fn load(&mut self, data: &serde_intermediate::Intermediate);
    fn save(&self) -> serde_intermediate::Intermediate;
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
}

impl Clone for CircuitProperty {
    fn clone(&self) -> Self {
        Self { imp: self.imp.clone(), id: self.id.clone(), name: self.name.clone() }
    }
}

impl CircuitProperty {
    pub fn id(&self) -> DynStaticStr {
        self.id.clone()
    }

    pub fn name(&self) -> DynStaticStr {
        self.name.clone()
    }
}

impl CircuitPropertyImpl for Direction4 {
    fn equals(&self, other: &dyn CircuitPropertyImpl) -> bool {
        other.is_type_and(|o: &Self| o == self)
    }

    fn ui(&mut self, _: &mut Ui, _: &mut bool) {}

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
}
