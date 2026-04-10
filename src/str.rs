use std::{
    fmt::{Debug, Display},
    ops::Deref,
    sync::Arc,
};

use serde::{Deserialize, Serialize};
use smoldata::{
    SmolRead, SmolWrite,
    reader::{ReadError, UnexpectedValueResultExt},
};

pub type ArcStaticStr = ArcRefStr<'static>;

#[derive(Clone)]
pub enum ArcRefStr<'a> {
    Ref(&'a str),
    Arc(Arc<str>),
}

impl Deref for ArcRefStr<'_> {
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

impl From<Arc<str>> for ArcRefStr<'_> {
    fn from(value: Arc<str>) -> Self {
        Self::Arc(value)
    }
}

impl From<String> for ArcRefStr<'_> {
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

impl std::hash::Hash for ArcRefStr<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.deref().hash(state);
    }
}

impl<T: Deref<Target = str>> PartialEq<T> for ArcRefStr<'_> {
    fn eq(&self, other: &T) -> bool {
        self.deref().eq(other.deref())
    }
}

impl Eq for ArcRefStr<'_> {}

impl Serialize for ArcRefStr<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.deref().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for ArcRefStr<'_> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Self::Arc(<&str>::deserialize(deserializer)?.into()))
    }
}

impl Display for ArcRefStr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.deref(), f)
    }
}

impl Debug for ArcRefStr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArcRefStr::Ref(r) => {
                f.write_str("ref ")?;
                Debug::fmt(r, f)
            }
            ArcRefStr::Arc(a) => {
                f.write_str("arc ")?;
                Debug::fmt(a.deref(), f)
            }
        }
    }
}

impl<'a> From<ArcRefStr<'a>> for smoldata::str::RefArcStr<'a> {
    fn from(val: ArcRefStr<'a>) -> Self {
        match val {
            ArcRefStr::Ref(v) => smoldata::str::RefArcStr::Str(v),
            ArcRefStr::Arc(v) => smoldata::str::RefArcStr::Arc(v),
        }
    }
}

impl SmolWrite for ArcRefStr<'_> {
    fn write(&self, writer: smoldata::writer::ValueWriter) -> std::io::Result<()> {
        writer.write_string(self.clone())
    }
}

impl SmolRead for ArcRefStr<'_> {
    fn read(reader: smoldata::reader::ValueReader) -> smoldata::reader::ReadResult<Self> {
        let str = reader
            .read()?
            .take_string()
            .with_type_name_of::<Self>()
            .map_err(ReadError::from)?;
        Ok(match str.read()? {
            smoldata::str::SdString::Empty => Self::Ref(""),
            smoldata::str::SdString::Arc(a) => Self::Arc(a),
            smoldata::str::SdString::Owned(v) => Self::Arc(v.into()),
        })
    }
}
