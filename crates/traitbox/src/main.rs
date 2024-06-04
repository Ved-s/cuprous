trait MyTrait {
    type Stuff: Default;

    fn make_stuff() -> Self::Stuff;
    fn do_stuff(&self, s: &Self::Stuff) -> usize;
    fn do_ref(&self, a: usize) -> usize;
    fn do_mut(&mut self, a: usize) -> usize;
    fn do_owned(self, a: usize) -> usize;
}

traitbox::traitbox! {
    #[downcast]
    box MyTraitBox;

    #[as_impl]
    trait MyTrait {
        type Stuff;
        
        fn make_stuff() -> Self::Stuff;
        
        #[on_cast_fail = &Self::Stuff::default()]
        fn do_stuff(&self, s: &Self::Stuff) -> usize;
        fn do_ref(&self, a: usize) -> usize;
        fn do_mut(&mut self, a: usize) -> usize;
        fn do_owned(self, a: usize) -> usize;
    }

    trait Clone {
        fn clone(&self) -> Self;
    }

    auto trait Sync {}
    auto trait Send {}

    impl {
        fn stuff_name<T: MyTrait>() -> &'static str
        where
            T::Stuff: 'static
        {
            std::any::type_name::<T::Stuff>()
        }
    }
}

use std::fmt::Debug;

traitbox::traitbox! {
    box DefaultBox;

    #[as_impl]
    trait Default {
        fn default() -> Self;
    }

    trait Debug {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
    }
}

/*
traitbox::traitbox! {
    #[downcast]
    box MyTraitBox;

    #[as_impl]
    trait MyTrait {
        type Stuff;

        #[invalid_type_cast = Self::Stuff::default()]
        fn do_stuff(&self, s: Self::Stuff) -> usize;
    }

    trait Clone {
        fn clone(&self) -> Self;
    }

    auto trait Sync {}
    auto trait Send {}

    impl {
        fn make_stuff<T: MyTrait>() -> T::Stuff {
            T::Stuff::default()
        }
    }
}
*/

fn main() {
    let b = DefaultBox::new(165usize);
    dbg!(b.default());
}

#[derive(Clone)]
struct Test(usize);

impl MyTrait for Test {
    type Stuff = usize;

    fn make_stuff() -> Self::Stuff {
        100500900
    }

    fn do_stuff(&self, s: &Self::Stuff) -> usize {
        self.0 + *s
    }

    fn do_ref(&self, a: usize) -> usize {
        self.0 + a
    }

    fn do_mut(&mut self, a: usize) -> usize {
        self.0 += a;
        self.0
    }

    fn do_owned(self, a: usize) -> usize {
        self.0 + a
    }
}
