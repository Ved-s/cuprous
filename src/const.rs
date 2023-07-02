pub trait True {}
pub trait False {}
pub struct Bool<const VALUE: bool> {}

impl True for Bool<true> {}
impl False for Bool<false> {}