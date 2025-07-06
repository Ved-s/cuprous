use smoldata::{raw::RawValue, SmolReadWrite};

use crate::{str::ArcStaticStr, vector::Vec2usize, Direction4, Direction4Half};


#[derive(Default, SmolReadWrite)]
pub struct CopyState {
    pub wire_parts: Vec<WirePart>,
    pub wire_points: Vec<Vec2usize>,
    pub circuits: Vec<Circuit>,
}

#[derive(SmolReadWrite)]
pub struct WirePart {
    pub pos: Vec2usize,
    pub dir: Direction4Half,
    pub len: u32,
}

#[derive(SmolReadWrite)]
pub struct Circuit {
    pub id: ArcStaticStr,
    pub pos: Vec2usize,
    pub dir: Direction4,
    pub flip: bool,
    pub config: Option<RawValue>,
    pub instance: Option<RawValue>,
    pub state: Option<RawValue>,
}