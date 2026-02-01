//! Extended std, provides various utility data structures

pub mod bidirectional_map;
pub mod bitflags;
pub mod bitset;
pub mod fast_map;
pub mod graph;
pub mod macros_utils;
pub mod small_string;

pub use bidirectional_map::BidirectionalMap;
pub use bitset::Bitset;
pub use fast_map::{FastIndexMap, FastIndexSet, FastMap, FastSet};
pub use graph::Graph;
pub use small_string::SmallString;

pub fn default<T: Default>() -> T {
    Default::default()
}
