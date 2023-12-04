use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

use eframe::egui::{CollapsingHeader, Id, Ui};

use crate::DynStaticStr;

pub struct ErrorList {
    pub errors: Vec<Box<dyn Display + Send + Sync + 'static>>,
    pub inner: Vec<ErrorContext>,
}

pub struct ErrorContext {
    pub context: DynStaticStr,
    pub list: ErrorList,
}

pub struct ErrorContextGuard<'a, F, S>
where
    F: FnOnce() -> S,
    S: Into<DynStaticStr>,
{
    parent: &'a mut ErrorList,
    list: ErrorList,
    context: Option<F>,
}

impl<'a, F, S> Drop for ErrorContextGuard<'a, F, S>
where
    F: FnOnce() -> S,
    S: Into<DynStaticStr>,
{
    fn drop(&mut self) {
        if !self.list.errors.is_empty() || !self.list.inner.is_empty() {
            if let Some(ctx_gen) = self.context.take() {
                let list = std::mem::replace(&mut self.list, ErrorList::new());
                self.parent.inner.push(ErrorContext {
                    context: ctx_gen().into(),
                    list,
                });
            }
        }
    }
}

impl ErrorList {
    pub fn new() -> Self {
        Self {
            errors: vec![],
            inner: vec![],
        }
    }

    pub fn enter_context<F, S>(&mut self, f: F) -> ErrorContextGuard<'_, F, S>
    where
        F: FnOnce() -> S,
        S: Into<DynStaticStr>,
    {
        ErrorContextGuard {
            parent: self,
            list: ErrorList::new(),
            context: Some(f),
        }
    }

    pub fn with_context<F, S, R>(&mut self, ctx: F, inner: impl FnOnce(&mut ErrorList) -> R) -> R
    where
        F: FnOnce() -> S,
        S: Into<DynStaticStr>,
    {
        let mut ctx = self.enter_context(ctx);
        inner(&mut ctx)
    }

    pub fn push_error<E: Display + Send + Sync + 'static>(&mut self, error: E) {
        self.errors.push(Box::new(error));
    }

    pub fn clear_error<T, E: Display + Send + Sync + 'static>(
        &mut self,
        res: Result<T, E>,
    ) -> Option<T> {
        match res {
            Ok(t) => Some(t),
            Err(e) => {
                self.push_error(e);
                None
            }
        }
    }

    pub fn report_none<T, E: Display + Send + Sync + 'static>(
        &mut self,
        opt: Option<T>,
        err: impl FnOnce() -> E,
    ) -> Option<T> {
        match opt {
            Some(t) => Some(t),
            None => {
                self.push_error(err());
                None
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty() && self.inner.is_empty()
    }

    pub fn clear(&mut self) {
        self.errors.clear();
        self.inner.clear();
    }

    pub fn show_ui(&self, ui: &mut Ui) {
        fn add_error_context(errors: &ErrorContext, ui: &mut Ui, id: Id) {
            let string = format!("{}:", errors.context.deref());
            CollapsingHeader::new(string)
                .id_source(id)
                .default_open(true)
                .show(ui, |ui| {
                    add_error_list(&errors.list, ui, id.with("list"));
                });
        }

        fn add_error_list(errors: &ErrorList, ui: &mut Ui, id: Id) {
            let inner_id = id.with("inner");
            for (i, ctx) in errors.inner.iter().enumerate() {
                add_error_context(ctx, ui, inner_id.with(i));
            }
            for error in errors.errors.iter() {
                let display = format!("{}", &error);
                ui.label(display);
            }
        }
        add_error_list(self, ui, ui.next_auto_id().with("error_list"));
        ui.skip_ahead_auto_ids(1);
    }
}

impl Default for ErrorList {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, F, S> Deref for ErrorContextGuard<'a, F, S>
where
    F: FnOnce() -> S,
    S: Into<DynStaticStr>,
{
    type Target = ErrorList;

    fn deref(&self) -> &Self::Target {
        &self.list
    }
}

impl<'a, F, S> DerefMut for ErrorContextGuard<'a, F, S>
where
    F: FnOnce() -> S,
    S: Into<DynStaticStr>,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.list
    }
}

pub trait OptionReport {
    fn report_none<E: Display + Send + Sync + 'static>(
        self,
        list: &mut ErrorList,
        err: impl FnOnce() -> E,
    ) -> Self;
}

pub trait ResultReport<T, E>
where
    E: Display + Send + Sync + 'static,
{
    fn report_error(self, list: &mut ErrorList) -> Option<T>;
}

impl<T> OptionReport for Option<T> {
    fn report_none<E: Display + Send + Sync + 'static>(
        self,
        list: &mut ErrorList,
        err: impl FnOnce() -> E,
    ) -> Self {
        list.report_none(self, err)
    }
}

impl<T, E: Display + Send + Sync + 'static> ResultReport<T, E> for Result<T, E> {
    fn report_error(self, list: &mut ErrorList) -> Option<T> {
        list.clear_error(self)
    }
}
