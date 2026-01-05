pub mod bidirectional_map;
pub mod fast_map;
pub mod graph;
pub mod small_string;
pub mod bitset;

pub fn default<T: Default>() -> T {
    Default::default()
}
