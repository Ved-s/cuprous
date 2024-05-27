use std::{fmt::{Debug, Display}, ops::Deref, sync::Arc};

use serde::{Deserialize, Serialize};

pub type ArcStaticStr = ArcRefStr<'static>;

#[derive(Clone)]
pub enum ArcRefStr<'a> {
    Ref(&'a str),
    Arc(Arc<str>),
}

impl<'a> Deref for ArcRefStr<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            ArcRefStr::Ref(s) => s,
            ArcRefStr::Arc(a) => a.deref(),
        }
    }
}

impl<'a> From<&'a str> for ArcRefStr<'a> {
    fn from(value: &'a str) -> Self {
        Self::Ref(value)
    }
}

impl<'a> From<Arc<str>> for ArcRefStr<'a> {
    fn from(value: Arc<str>) -> Self {
        Self::Arc(value)
    }
}

impl<'a> From<String> for ArcRefStr<'a> {
    fn from(value: String) -> Self {
        Self::Arc(value.into())
    }
}

impl<'a> From<ArcRefStr<'a>> for Arc<str> {
    fn from(value: ArcRefStr<'a>) -> Self {
        match value {
            ArcRefStr::Ref(r) => r.into(),
            ArcRefStr::Arc(a) => a,
        }
    }
}

impl<'a> Serialize for ArcRefStr<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer {
        self.deref().serialize(serializer)
    }
}

impl<'de, 'a> Deserialize<'de> for ArcRefStr<'a> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de> {
        Ok(Self::Arc(<&str>::deserialize(deserializer)?.into()))
    }
}

impl<'a> Display for ArcRefStr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.deref(), f)
    }
}

impl<'a> Debug for ArcRefStr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArcRefStr::Ref(r) => {
                f.write_str("ref ")?;
                Debug::fmt(r, f)
            },
            ArcRefStr::Arc(a) => {
                f.write_str("arc ")?;
                Debug::fmt(a.deref(), f)
            },
        }
        
    }
}
