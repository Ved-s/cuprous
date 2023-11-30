use std::{
    any::{Any, TypeId},
    collections::HashMap,
    ops::{Deref, RangeInclusive, RangeFrom},
    sync::Arc,
};

use eframe::{
    egui::{self, ComboBox, DragValue, Ui, Widget},
    epaint::{Color32, Rounding, Stroke},
};

use crate::{
    unwrap_option_or_return, unwrap_option_or_continue, ArcString, Direction4, DynStaticStr, Mutex,
    RwLock,
};

pub struct CircuitPropertyStore(RwLock<HashMap<DynStaticStr, CircuitProperty>>);

impl Default for CircuitPropertyStore {
    fn default() -> Self {
        Self::new([])
    }
}

impl CircuitPropertyStore {
    pub fn new(props: impl IntoIterator<Item = CircuitProperty>) -> Self {
        Self(RwLock::new(HashMap::from_iter(
            Self::default_props().chain(props).map(|p| (p.id(), p)),
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
            self.0.read().values().map(|p| (p.id.clone(), p.imp.save())),
        ))
    }

    pub fn load(&self, data: &crate::io::CircuitPropertyStoreData) {
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

    fn default_props() -> impl Iterator<Item = CircuitProperty> {
        [
            CircuitProperty::new("name", "Name", ArcString::default()),
            CircuitProperty::new("label_dir", "Label dir", Direction4::Down),
        ]
        .into_iter()
    }
}

impl Clone for CircuitPropertyStore {
    fn clone(&self) -> Self {
        Self(RwLock::new(HashMap::from_iter(self.0.read().values().map(
            |p| {
                let clone = p.clone();
                (clone.id(), clone)
            },
        ))))
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
    name: DynStaticStr,
}

impl CircuitProperty {
    pub fn new<T: CircuitPropertyImpl>(
        id: impl Into<DynStaticStr>,
        name: impl Into<DynStaticStr>,
        prop: T,
    ) -> Self {
        Self::new_dynamic(id, name, Box::new(prop))
    }

    pub fn new_dynamic(
        id: impl Into<DynStaticStr>,
        name: impl Into<DynStaticStr>,
        prop: Box<dyn CircuitPropertyImpl>,
    ) -> Self {
        Self {
            imp: prop,
            id: id.into(),
            name: name.into(),
        }
    }

    pub fn new_default<T: CircuitPropertyImpl + Default>(
        id: impl Into<DynStaticStr>,
        name: impl Into<DynStaticStr>,
    ) -> Self {
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
        Self {
            imp: self.imp.clone(),
            id: self.id.clone(),
            name: self.name.clone(),
        }
    }
}

#[derive(Clone)]
pub struct RangedValue<T>
where
    T: emath::Numeric + Send + Sync,
{
    range: RangeInclusive<T>,
    change_speed: T,
    value: T,
}

impl<T> RangedValue<T>
where
    T: emath::Numeric + Send + Sync,
{
    pub fn new(range: RangeInclusive<T>, change_speed: T, value: T) -> Self {
        Self {
            range,
            change_speed,
            value,
        }
    }
    pub fn new_from(range: RangeFrom<T>, change_speed: T, value: T) -> Self {
        let range = range.start ..= T::MAX;
        Self {
            range,
            change_speed,
            value,
        }
    }

    pub fn get(&self) -> T {
        self.value
    }

    pub fn set(&mut self, value: T) {
        if value < *self.range.start() {
            self.value = *self.range.start()
        }
        if value > *self.range.end() {
            self.value = *self.range.end()
        }
        self.value = value;
    }
}

impl CircuitPropertyImpl for Direction4 {
    fn equals(&self, other: &dyn CircuitPropertyImpl) -> bool {
        other.is_type_and(|o: &Self| o == self)
    }

    fn ui(&mut self, ui: &mut Ui, not_equal: bool) -> Option<Box<dyn CircuitPropertyImpl>> {
        let old = *self;
        let mut changed = false;
        ui.skip_ahead_auto_ids(1);
        ComboBox::from_id_source(ui.next_auto_id())
            .selected_text(if not_equal {
                Default::default()
            } else {
                self.name()
            })
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

impl CircuitPropertyImpl for bool {
    fn equals(&self, other: &dyn CircuitPropertyImpl) -> bool {
        other.is_type_and(|o: &Self| o == self)
    }

    fn ui(&mut self, ui: &mut Ui, not_equal: bool) -> Option<Box<dyn CircuitPropertyImpl>> {
        let text = if not_equal {
            "<many>"
        } else if *self {
            "True"
        } else {
            "False"
        };

        let mut selected = !not_equal && *self;
        let old = *self;
        if ui.toggle_value(&mut selected, text).changed() {
            *self = selected;
            Some(Box::new(old))
        } else {
            None
        }
    }

    fn clone(&self) -> Box<dyn CircuitPropertyImpl> {
        Box::new(*self)
    }

    fn load(&mut self, data: &serde_intermediate::Intermediate) {
        if let Ok(d) = serde_intermediate::de::intermediate::deserialize(data) {
            *self = d;
        }
    }

    fn save(&self) -> serde_intermediate::Intermediate {
        serde_intermediate::to_intermediate(self).unwrap_or_default()
    }

    fn copy_into(&self, other: &mut dyn CircuitPropertyImpl) {
        if let Some(r) = other.downcast_mut() {
            *r = *self;
        }
    }
}

impl CircuitPropertyImpl for ArcString {
    fn equals(&self, other: &dyn CircuitPropertyImpl) -> bool {
        other.is_type_and(|s: &ArcString| s.get_str().deref() == self.get_str().deref())
    }

    fn ui(&mut self, ui: &mut Ui, not_equal: bool) -> Option<Box<dyn CircuitPropertyImpl>> {
        let old = self.get_arc();

        let id = ui.auto_id_with("arcstr_prop_ui");
        ui.skip_ahead_auto_ids(1);
        let memory_id = id.with("__memory");

        #[derive(Clone, Default)]
        struct State {
            locked: bool,
            arc: Arc<Mutex<String>>,
        }

        let state = ui.data_mut(|data| data.get_temp_mut_or_default::<State>(memory_id).clone());
        let mut locked_string = state.arc.lock();
        let mut empty = String::new();

        let edit_string = if state.locked {
            &mut locked_string
        } else if not_equal {
            &mut empty
        } else {
            self.get_mut()
        };

        let resp = ui.text_edit_singleline(edit_string);
        let old = if resp.changed() {
            let str = self.get_mut();
            str.clear();
            str.push_str(&locked_string);
            Some(Box::new(ArcString::from(old.deref())) as _)
        } else {
            None
        };

        if resp.gained_focus() {
            let self_str = self.get_str();
            let current_str = if not_equal { empty.as_str() } else { &self_str };
            locked_string.clear();
            locked_string.push_str(current_str);
            ui.data_mut(|data| data.get_temp_mut_or_default::<State>(memory_id).locked = true);
        };

        if state.locked && !resp.has_focus() {
            ui.data_mut(|data| data.get_temp_mut_or_default::<State>(memory_id).locked = false);
        };

        old
    }

    fn clone(&self) -> Box<dyn CircuitPropertyImpl> {
        Box::new(Clone::clone(self))
    }

    fn load(&mut self, data: &serde_intermediate::Intermediate) {
        if let Some(str) = data.as_str() {
            let string = self.get_mut();
            string.clear();
            string.push_str(str);
        }
    }

    fn save(&self) -> serde_intermediate::Intermediate {
        serde_intermediate::to_intermediate(self.get_str().deref()).unwrap_or_default()
    }

    fn copy_into(&self, other: &mut dyn CircuitPropertyImpl) {
        if let Some(r) = other.downcast_mut::<Self>() {
            let string = r.get_mut();
            string.clear();
            string.push_str(self.get_str().deref());
        }
    }
}

impl CircuitPropertyImpl for Color32 {
    fn equals(&self, other: &dyn CircuitPropertyImpl) -> bool {
        other.is_type_and(|s: &Self| *s == *self)
    }

    fn ui(&mut self, ui: &mut Ui, not_equal: bool) -> Option<Box<dyn CircuitPropertyImpl>> {
        let old = *self;

        let mut color = if not_equal { Color32::BLACK } else { *self };

        if ui.color_edit_button_srgba(&mut color).changed() {
            *self = color;
            Some(Box::new(old))
        } else {
            None
        }
    }

    fn clone(&self) -> Box<dyn CircuitPropertyImpl> {
        Box::new(Clone::clone(self))
    }

    fn load(&mut self, data: &serde_intermediate::Intermediate) {
        if let Ok(color) = serde_intermediate::from_intermediate(data) {
            *self = color;
        }
    }

    fn save(&self) -> serde_intermediate::Intermediate {
        serde_intermediate::to_intermediate(self).unwrap_or_default()
    }

    fn copy_into(&self, other: &mut dyn CircuitPropertyImpl) {
        if let Some(r) = other.downcast_mut::<Self>() {
            *r = *self;
        }
    }
}

impl<T> CircuitPropertyImpl for RangedValue<T>
where
    T: emath::Numeric + Send + Sync,
{
    fn equals(&self, other: &dyn CircuitPropertyImpl) -> bool {
        other.is_type_and(|s: &Self| s.value == self.value)
    }

    fn ui(&mut self, ui: &mut Ui, not_equal: bool) -> Option<Box<dyn CircuitPropertyImpl>> {
        let old = Clone::clone(self);

        let mut value = if not_equal {
            *self.range.start()
        } else {
            self.value
        };

        let resp = DragValue::new(&mut value)
            .clamp_range(self.range.clone())
            .speed(self.change_speed.to_f64())
            .ui(ui);

        if resp.changed() {
            self.set(value);
            Some(Box::new(old))
        } else {
            None
        }
    }

    fn clone(&self) -> Box<dyn CircuitPropertyImpl> {
        Box::new(Clone::clone(self))
    }

    fn load(&mut self, data: &serde_intermediate::Intermediate) {
        if let Ok(value) = serde_intermediate::from_intermediate(data) {
            self.set(T::from_f64(value));
        }
    }

    fn save(&self) -> serde_intermediate::Intermediate {
        serde_intermediate::to_intermediate(&self.value.to_f64()).unwrap_or_default()
    }

    fn copy_into(&self, other: &mut dyn CircuitPropertyImpl) {
        if let Some(r) = other.downcast_mut::<Self>() {
            r.set(self.value);
        }
    }
}

impl CircuitPropertyImpl for Rounding {
    fn equals(&self, other: &dyn CircuitPropertyImpl) -> bool {
        other.is_type_and(|o: &Self| o == self)
    }

    fn ui(&mut self, ui: &mut Ui, _: bool) -> Option<Box<dyn CircuitPropertyImpl>> {
        let old = *self;
        let changed = egui::Grid::new(ui.next_auto_id().with("rounding_grid")).show(ui, |ui| {
            let nw = DragValue::new(&mut self.nw)
                .clamp_range(0.0..=f32::MAX)
                .speed(0.05)
                .ui(ui)
                .changed();
            let ne = DragValue::new(&mut self.ne)
                .clamp_range(0.0..=f32::MAX)
                .speed(0.05)
                .ui(ui)
                .changed();
            ui.end_row();
            let sw = DragValue::new(&mut self.sw)
                .clamp_range(0.0..=f32::MAX)
                .speed(0.05)
                .ui(ui)
                .changed();
            let se = DragValue::new(&mut self.se)
                .clamp_range(0.0..=f32::MAX)
                .speed(0.05)
                .ui(ui)
                .changed();
            ui.end_row();

            nw || ne || sw || se
        });

        changed
            .inner
            .then(|| Box::new(old) as Box<dyn CircuitPropertyImpl>)
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

impl CircuitPropertyImpl for Stroke {
    fn equals(&self, other: &dyn CircuitPropertyImpl) -> bool {
        other.is_type_and(|o: &Self| o == self)
    }

    fn ui(&mut self, ui: &mut Ui, _: bool) -> Option<Box<dyn CircuitPropertyImpl>> {
        let old = *self;
        let changed = ui.horizontal(|ui| {
            let width = DragValue::new(&mut self.width)
                .clamp_range(0.0..=f32::MAX)
                .speed(0.05)
                .ui(ui)
                .changed();
            let color = ui.color_edit_button_srgba(&mut self.color).changed();

            width || color
        });

        changed
            .inner
            .then(|| Box::new(old) as Box<dyn CircuitPropertyImpl>)
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
