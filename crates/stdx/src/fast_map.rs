use ordermap::{OrderMap, OrderSet};
use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet};

pub type FastHasher = FxBuildHasher;

pub type FastMap<K, V> = FxHashMap<K, V>;
pub type FastSet<V> = FxHashSet<V>;

pub type FastIndexMap<K, V> = OrderMap<K, V, FastHasher>;
pub type FastIndexSet<V> = OrderSet<V, FastHasher>;
