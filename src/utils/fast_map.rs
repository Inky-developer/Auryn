use indexmap::IndexMap;
use rustc_hash::{FxBuildHasher, FxHashMap};

pub type FastMap<K, V> = FxHashMap<K, V>;

pub type FastIndexMap<K, V> = IndexMap<K, V, FxBuildHasher>;
