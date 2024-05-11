use std::mem::MaybeUninit;

macro_rules! extend_trait {
    (
        $tv:vis impl $name:ident$(<$($gname:ident $(: $gbound:tt)?),+>)?
        extends $ext:ident $(<$($egen:ident $(= $egenty:ident)? ),+>)?
        $(where $($whname:ident: $whbound:tt),+)? {
            $(
                fn $fnname:ident($($fnarg:tt)*) -> $fnret:ty $fnbody:block
            )*
        }
    ) => {
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

pub trait IteratorExt
where
    Self: Iterator + Sized,
{
    fn find_index(self, predicate: impl Fn(Self::Item) -> bool) -> Option<usize>;
    fn chunks<const C: usize>(self) -> ChunksIterator<Self, C>;
}
impl<I: Iterator> IteratorExt for I {
    fn find_index(self, predicate: impl Fn(I::Item) -> bool) -> Option<usize> {
        for (i, v) in self.enumerate() {
            if predicate(v) {
                return Some(i);
            }
        }
        None
    }
    
    fn chunks<const C: usize>(self) -> ChunksIterator<Self, C> {
        ChunksIterator { inner: self }
    }
}

pub enum ConditionalIterator<I, T: Iterator<Item = I>, F: Iterator<Item = I>> {
    True(T),
    False(F),
}

impl<I, T, F> Iterator for ConditionalIterator<I, T, F>
where
    T: Iterator<Item = I>,
    F: Iterator<Item = I>,
{
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

pub struct ChunksIterator<I, const C: usize>
where
    I: Iterator,
{
    inner: I,
}

impl<I, const C: usize> ChunksIterator<I, C>
where
    I: Iterator,
{
    #[allow(unused)]
    const VALID: () = {
        if C == 0 {
            panic!("ChunksIterator with size of 0 is invalid");
        }
    };
}

impl<I, const C: usize> Iterator for ChunksIterator<I, C>
where
    I: Iterator,
{
    type Item = [I::Item; C];

    fn next(&mut self) -> Option<Self::Item> {
        let mut buf = std::array::from_fn(|_| MaybeUninit::uninit());

        #[allow(clippy::needless_range_loop)]
        for i in 0..C {
            buf[i] = MaybeUninit::new(self.inner.next()?);
        }

        Some(unsafe { assume_init_array(buf) })
    }
}

impl<I, const C: usize> ExactSizeIterator for ChunksIterator<I, C>
where
    I: ExactSizeIterator,
{
    fn len(&self) -> usize {
        self.inner.len() / C
    }
}

/// MaybeUninit::array_assume_init
unsafe fn assume_init_array<T, const C: usize>(arr: [MaybeUninit<T>; C]) -> [T; C] {
    let ptr = &arr as *const [MaybeUninit<T>; C];
    let ptr = *(&ptr as *const *const [MaybeUninit<T>; C] as *const *const [T; C]);
    let transmuted = unsafe { ptr.read() };

    #[allow(clippy::forget_non_drop)]
    std::mem::forget(arr);
    transmuted
}
