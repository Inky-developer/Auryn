pub struct Flag<T> {
    pub value: T,
    pub name: &'static str,
}

#[macro_export]
macro_rules! bitflags {
    {$(#[$meta:meta])* $vis:vis struct $name:ident: $repr:ty { $($item_vis:vis const $item_name:ident = $item_val:expr;)* }} => {
        $(#[$meta])*
        $vis struct $name($repr);

        impl $name {
            $(
                $item_vis const $item_name: Self = Self($item_val);
            )*

            pub const FLAGS: [$crate::bitflags::Flag<Self>; $crate::count!($($item_name)*)] = [$(
                $crate::bitflags::Flag {
                    value: Self::$item_name,
                    name: stringify!($item_name)
                }
            ),*];

            pub fn get(&self) -> $repr {
                self.0
            }

            pub fn active_variants(&self) -> impl Iterator<Item = $crate::bitflags::Flag<Self>> {
                Self::FLAGS.into_iter().filter(|flag| self.0 & flag.value.0 != 0)
            }
        }

        impl std::ops::BitOr for $name {
            type Output = Self;

            fn bitor(self, other: Self) -> Self {
                Self(self.0 | other.0)
            }
        }

        impl std::ops::BitOrAssign for $name {
            fn bitor_assign(&mut self, other: Self) {
                *self = Self(self.0 | other.0);
            }
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                for (index, variant) in self.active_variants().enumerate() {
                    if index != 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", variant.name)?;
                }

                Ok(())
            }
        }

    };
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_bitflags() {
        bitflags! {
            #[derive(Clone, Copy, Eq, PartialEq)]
            pub struct Bitflags: u8 {
                pub const A = 1;
                pub const B = 2;
            }
        }

        assert_eq!(Bitflags::A.get(), 1);
        assert_eq!(Bitflags::B.get(), 2);

        let c = Bitflags::A | Bitflags::B;
        assert_eq!(c.get(), 3);

        assert_eq!(
            c.active_variants()
                .map(|flag| flag.value)
                .collect::<Vec<_>>(),
            vec![Bitflags::A, Bitflags::B]
        );

        assert_eq!(format!("{c:?}"), "A | B");
    }
}
