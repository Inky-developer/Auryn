pub mod bidirectional_map;
pub mod bitset;
pub mod fast_map;
pub mod graph;
pub mod small_string;

pub fn default<T: Default>() -> T {
    Default::default()
}
