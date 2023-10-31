macro_rules! extend_trait {
    (
        $tv:vis impl $name:ident$(<$($gname:ident $(: $gbound:tt)?),+>)?
        extends $ext:ident $(<$($egen:ident $(= $egenty:ident)? ),+>)?
        $(where $($whname:ident: $whbound:tt),+)? {
            $(
                fn $fnname:ident($($fnarg:tt)*) -> $fnret:ty $fnbody:block
            )*
        }) => {

        $tv trait $name$(<$($gname$(: $gbound)?),+>)? $(where $($whname: $whbound),+)? {
            $(fn $fnname($($fnarg)*) -> $fnret)*;
        }

        impl<TTarget: $ext $(<$($egen $(= $egenty)? ),+ >)? $(,$($gname$(: $gbound)?),+)?> $name$(<$($gname),+>)? for TTarget {
            $(
                fn $fnname($($fnarg)*) -> $fnret $fnbody
            )*
        }
    };
}

extend_trait! {
    pub impl IteratorEqExt<T: Eq> extends Iterator<Item = T> {
        fn same(self) -> Option<T> {
            let mut iter = self;
            let first = iter.next()?;
            for item in iter {
                if item != first {
                    return None;
                }
            }
            Some(first)
        }
    }
}

extend_trait! {
    pub impl IteratorExt<T> extends Iterator<Item = T> {
        fn find_index(self, predicate: impl Fn(T) -> bool) -> Option<usize> {
            for (i, v) in self.enumerate() {
                if predicate(v) {
                    return Some(i);
                }
            }
            None
        }
    }
}

pub enum ConditionalIterator<I, T: Iterator<Item = I>, F: Iterator<Item = I>> {
    True(T),
    False(F),
}

impl<I, T, F> Iterator for ConditionalIterator<I, T, F> where T: Iterator<Item = I>, F: Iterator<Item = I> {
    type Item = I;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ConditionalIterator::True(t) => t.next(),
            ConditionalIterator::False(f) => f.next(),
        }
    }
}

pub trait IteratorConditionExt<I, T, F>
where
    Self: Sized,
    T: Iterator<Item = I>,
    F: Iterator<Item = I>,
{
    fn condition(
        self,
        condition: bool,
        on_true: impl FnOnce(Self) -> T,
        on_false: impl FnOnce(Self) -> F,
    ) -> ConditionalIterator<I, T, F>;
}

impl<TTarget, I, T, F> IteratorConditionExt<I, T, F> for TTarget
where
    TTarget: Iterator<Item = I>,
    T: Iterator<Item = I>,
    F: Iterator<Item = I>,
{
    fn condition(
        self,
        condition: bool,
        on_true: impl FnOnce(Self) -> T,
        on_false: impl FnOnce(Self) -> F,
    ) -> ConditionalIterator<I, T, F> {
        if condition {
            ConditionalIterator::True(on_true(self))
        } else {
            ConditionalIterator::False(on_false(self))
        }
    }
}
