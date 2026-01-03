use std::{borrow::Borrow, fmt::Debug, hash::Hash};

use crate::utils::fast_map::FastMap;

pub struct BidirectionalMap<K, V> {
    k_to_v: FastMap<K, V>,
    v_to_k: FastMap<V, K>,
}

impl<K, V> BidirectionalMap<K, V>
where
    K: Eq + Hash,
    V: Eq + Hash,
{
    pub fn get_by_key<Q>(&self, k: &Q) -> Option<&V>
    where
        Q: Hash + Eq,
        K: Borrow<Q>,
    {
        self.k_to_v.get(k)
    }

    pub fn get_by_value<Q>(&self, v: &Q) -> Option<&K>
    where
        Q: Hash + Eq,
        V: Borrow<Q>,
    {
        self.v_to_k.get(v)
    }
}

impl<K, V> BidirectionalMap<K, V>
where
    K: Clone + Eq + Hash,
    V: Clone + Eq + Hash,
{
    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        let prev_value = self.k_to_v.insert(k.clone(), v.clone());
        self.v_to_k.insert(v, k);

        prev_value
    }
}

impl<K, V> Default for BidirectionalMap<K, V> {
    fn default() -> Self {
        Self {
            k_to_v: Default::default(),
            v_to_k: Default::default(),
        }
    }
}

impl<K, V> Debug for BidirectionalMap<K, V>
where
    K: Debug,
    V: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.k_to_v.fmt(f)
    }
}
