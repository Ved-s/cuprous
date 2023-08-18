use std::{
    any::{Any, TypeId},
    collections::HashMap,
};

use eframe::egui::Ui;

use crate::{unwrap_option_or_return, Direction4, DynStaticStr, RwLock, unwrap_option_or_continue};

#[derive(Default)]
pub struct CircuitPropertyStore(RwLock<HashMap<TypeId, Box<dyn CircuitProperty>>>);

impl CircuitPropertyStore {
    pub fn new(props: impl IntoIterator<Item = Box<dyn CircuitProperty>>) -> Self {
        Self(RwLock::new(HashMap::from_iter(
            props.into_iter().map(|p| (p.as_ref().type_id(), p)),
        )))
    }

    pub fn read_clone<T: CircuitProperty + Clone>(&self) -> Option<T> {
        self.read(Clone::clone)
    }

    pub fn read<T, F, R>(&self, f: F) -> Option<R>
    where
        T: CircuitProperty,
        F: FnOnce(&T) -> R,
    {
        let id = TypeId::of::<T>();
        let lock = self.0.read().unwrap();
        let b = lock.get(&id)?.as_ref();
        if b.type_id() != id {
            None
        } else {
            Some(f(unsafe {
                &*(b as *const dyn CircuitProperty as *const T)
            }))
        }
    }

    /// Will not write if property doesn't exist
    pub fn write<T, F>(&self, f: F)
    where
        T: CircuitProperty,
        F: FnOnce(&mut T),
    {
        let id = TypeId::of::<T>();
        let mut lock = self.0.write().unwrap();
        let b = lock.get_mut(&id);
        let b = unwrap_option_or_return!(b);
        let t = unsafe { &mut *(b.as_mut() as *mut dyn CircuitProperty as *mut T) };
        f(t);
    }

    pub fn save(&self) -> crate::io::CircuitPropertyStoreData {
        crate::io::CircuitPropertyStoreData(HashMap::from_iter(
            self.0
                .read()
                .unwrap()
                .values()
                .map(|p| (p.type_name(), p.save())),
        ))
    }

    pub fn load(
        &self,
        data: &crate::io::CircuitPropertyStoreData
    ) {
        let mut lock = self.0.write().unwrap();
        for (ty, data) in data.0.iter() {
            let prop = lock.values_mut().find(|p| p.type_name() == *ty);
            let prop = unwrap_option_or_continue!(prop);
            prop.load(data);
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
                let clone = p.as_ref().clone();
                (clone.as_ref().type_id(), clone)
            }),
        )))
    }
}

pub trait CircuitProperty: Any + Send + Sync {
    fn type_name(&self) -> DynStaticStr;
    fn equals(&self, other: &dyn CircuitProperty) -> bool;
    fn ui(&mut self, ui: &mut Ui, changed: &mut bool);
    fn clone(&self) -> Box<dyn CircuitProperty>;
    fn load(&mut self, data: &serde_intermediate::Intermediate);
    fn save(&self) -> serde_intermediate::Intermediate;
    //fn as_any(&self) -> &dyn Any;
}

impl dyn CircuitProperty {
    #[inline]
    pub fn downcast_ref<T: CircuitProperty>(&self) -> Option<&T> {
        if TypeId::of::<T>() == self.type_id() {
            Some(unsafe { &*(self as *const dyn CircuitProperty as *const T) })
        } else {
            None
        }
    }

    pub fn is_type_and<T, F>(&self, f: F) -> bool
    where
        T: CircuitProperty,
        F: FnOnce(&T) -> bool,
    {
        self.downcast_ref::<T>().map(f).unwrap_or(false)
    }

    pub fn map_type<T, O, F>(&self, f: F) -> Option<O>
    where
        T: CircuitProperty,
        F: FnOnce(&T) -> O,
    {
        self.downcast_ref::<T>().map(f)
    }
}

#[derive(Clone)]
pub struct DirectionProp(pub Direction4);

impl Default for DirectionProp {
    fn default() -> Self {
        Self(Direction4::Right)
    }
}

impl CircuitProperty for DirectionProp {
    fn type_name(&self) -> DynStaticStr {
        "dir".into()
    }

    fn equals(&self, other: &dyn CircuitProperty) -> bool {
        other.is_type_and(|o: &Self| o.0 == self.0)
    }

    fn ui(&mut self, _: &mut Ui, _: &mut bool) {}

    fn clone(&self) -> Box<dyn CircuitProperty> {
        Box::new(Clone::clone(self))
    }

    fn save(&self) -> serde_intermediate::Intermediate {
        serde_intermediate::to_intermediate(&self.0).unwrap_or_default()
    }

    fn load(&mut self, data: &serde_intermediate::Intermediate) {
        if let Ok(d) = serde_intermediate::de::intermediate::deserialize(data) {
            self.0 = d;
        }
    }
}
