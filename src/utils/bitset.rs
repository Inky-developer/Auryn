use std::{fmt::Debug, marker::PhantomData};

pub trait BitsetItem {
    fn index(self) -> u8;

    /// Constructs a bitset item from an index.
    fn from_index(index: u8) -> Self;
}

pub struct Bitset<T> {
    set: u128,
    _items: PhantomData<[T]>,
}

impl<T> Bitset<T> {
    pub const fn new() -> Self {
        Self {
            set: 0,
            _items: PhantomData,
        }
    }

    pub const fn add_raw(&mut self, index: u8) {
        assert!(index < 128, "Bitset can only hold indexes of 127 and lower");
        self.set |= 1 << index;
    }

    pub const fn remove_raw(&mut self, index: u8) {
        assert!(index < 128, "Bitset can only hold indexes of 127 and lower");
        self.set &= !(1 << index);
    }

    pub const fn is_empty(&self) -> bool {
        self.set == 0
    }

    pub const fn len(&self) -> u8 {
        self.set.count_ones() as u8
    }
}

impl<T> Bitset<T>
where
    T: BitsetItem,
{
    pub fn add(&mut self, item: T) {
        let index = item.index();
        self.add_raw(index);
    }

    pub fn remove(&mut self, item: T) {
        let index = item.index();
        self.remove_raw(index);
    }

    pub fn contains(&self, item: T) -> bool {
        let index = item.index();
        assert!(index < 128, "Bitset can only hold indexes of 127 and lower");
        self.set & (1 << index) != 0
    }
}

impl<T> Extend<T> for Bitset<T>
where
    T: BitsetItem,
{
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        for item in iter {
            self.add(item);
        }
    }
}

impl<T> IntoIterator for Bitset<T>
where
    T: BitsetItem,
{
    type Item = T;
    type IntoIter = BitsetIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        BitsetIter { set: self }
    }
}

impl<T> Default for Bitset<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Clone for Bitset<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Bitset<T> {}

impl<T> Debug for Bitset<T>
where
    T: BitsetItem + Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_set().entries(*self).finish()
    }
}

pub struct BitsetIter<T> {
    set: Bitset<T>,
}

impl<T> Iterator for BitsetIter<T>
where
    T: BitsetItem,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let set = self.set.set;
        if set == 0 {
            return None;
        }

        let first_element_index = set.trailing_zeros();
        self.set.set &= !(1 << first_element_index);
        let item = T::from_index(first_element_index.try_into().unwrap());

        Some(item)
    }
}

/// Creates an enum with unitary variants that implements the [`BitsetItem`] trait
#[macro_export]
macro_rules! bitset_item {
    ($(#[$meta:meta])* $vis:vis enum $name:ident { $($variants:ident),* $(,)? }) => {
        $(#[$meta])*
        #[repr(u8)]
        $vis enum $name {
            $($variants),*
        }

        impl $name {
            pub const VARIANTS: &[Self] = &[
                $(Self::$variants),*
            ];
        }

        impl $crate::utils::bitset::BitsetItem for $name {
            fn index(self) -> u8 {
                self as u8
            }

            fn from_index(index: u8) -> Self {
                Self::VARIANTS[index as usize]
            }
        }

    };
}

/// Creates a bitset with the given elements at compile time
#[macro_export]
macro_rules! bitset [
    ($($items:expr),*) => {
        const {
            let mut set = $crate::utils::bitset::Bitset::new();
            $(set.add_raw(($items) as u8);)*
            set
        }
    }
];

#[cfg(test)]
mod tests {
    use crate::utils::bitset::Bitset;

    bitset_item! {
        #[derive(Debug, Clone, Copy, Eq, PartialEq)]
        pub enum MyEnum {
            A,
            B,
            C
        }
    }

    #[test]
    fn it_works() {
        let mut bitset = Bitset::default();
        assert!(bitset.is_empty());
        assert_eq!(bitset.len(), 0);
        assert_eq!(bitset.into_iter().next(), None);
        assert!(!bitset.contains(MyEnum::A));

        bitset.add(MyEnum::B);
        assert!(!bitset.is_empty());
        assert_eq!(bitset.len(), 1);
        insta::assert_compact_debug_snapshot!(bitset, @"{B}");
        assert!(!bitset.contains(MyEnum::A));
        assert!(bitset.contains(MyEnum::B));

        bitset.add(MyEnum::C);
        assert!(!bitset.is_empty());
        assert_eq!(bitset.len(), 2);
        insta::assert_compact_debug_snapshot!(bitset, @"{B, C}");

        bitset.add(MyEnum::A);
        assert!(!bitset.is_empty());
        assert_eq!(bitset.len(), 3);
        insta::assert_compact_debug_snapshot!(bitset, @"{A, B, C}");

        bitset.remove(MyEnum::C);
        insta::assert_compact_debug_snapshot!(bitset, @"{A, B}");
    }

    #[test]
    fn test_macro() {
        let my_set: Bitset<MyEnum> = bitset![MyEnum::A, MyEnum::C];
        insta::assert_compact_debug_snapshot!(my_set, @"{A, C}");
    }
}
