pub trait IteratorSameExt<T>
where
    Self: Iterator<Item = T>,
{
    fn same(self) -> Option<T>;
}

impl<I, T> IteratorSameExt<T> for I
where
    I: Iterator<Item = T>,
    T: Eq,
{
    fn same(mut self) -> Option<T> {
        let first = self.next()?;
        for item in self {
            if item != first {
                return None;
            }
        }
        Some(first)
    }
}
