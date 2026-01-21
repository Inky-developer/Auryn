pub mod bidirectional_map;
pub mod bitflags;
pub mod bitset;
pub mod fast_map;
pub mod graph;
pub mod macros_utils;
pub mod small_string;

pub fn default<T: Default>() -> T {
    Default::default()
}
