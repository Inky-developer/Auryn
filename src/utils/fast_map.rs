use indexmap::IndexMap;
use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet};

pub type FastMap<K, V> = FxHashMap<K, V>;
pub type FastSet<V> = FxHashSet<V>;

pub type FastIndexMap<K, V> = IndexMap<K, V, FxBuildHasher>;
