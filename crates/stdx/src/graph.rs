use std::{default::Default, hash::Hash};

use crate::fast_map::{FastMap, FastSet};

/// A simple directed graph implementation
#[derive(Debug)]
pub struct Graph<K, V> {
    vertices: FastMap<K, V>,
    edges: FastMap<K, FastSet<K>>,
}

impl<K, V> Default for Graph<K, V> {
    fn default() -> Self {
        Graph {
            vertices: FastMap::default(),
            edges: FastMap::default(),
        }
    }
}

impl<K, V> Graph<K, V>
where
    K: Eq + Hash + Copy,
{
    /// Inserts the `value` with the given `key` as id into the graph.
    /// Returns the old value.
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.vertices.insert(key, value)
    }

    pub fn vertex(&self, key: K) -> &V {
        self.vertices.get(&key).unwrap()
    }

    pub fn vertex_mut(&mut self, key: K) -> &mut V {
        self.vertices.get_mut(&key).unwrap()
    }

    pub fn edges(&self, key: K) -> impl Iterator<Item = K> {
        self.edges
            .get(&key)
            .into_iter()
            .flat_map(|it| it.iter().copied())
    }

    pub fn connect(&mut self, key: K, target: K) {
        self.edges.entry(key).or_default().insert(target);
    }

    /// Invers all edges so that instead of pointing from A to B they will point from B to A.
    pub fn invert_edges(&mut self) {
        let old_edges = std::mem::take(&mut self.edges);
        for (start, end_set) in old_edges {
            for end in end_set {
                self.connect(end, start);
            }
        }
    }

    /// Returns an order of all keys where for any element all edges are ordered before it (ignoring cycles)
    pub fn order_topologically(&self, starts: impl IntoIterator<Item = K>) -> Vec<K> {
        fn dfs<K, V>(this: &Graph<K, V>, key: K, buf: &mut Vec<K>, visited: &mut FastSet<K>)
        where
            K: Eq + Hash + Copy,
        {
            visited.insert(key);
            for edge in this.edges(key) {
                if !visited.contains(&edge) {
                    dfs(this, edge, buf, visited);
                }
            }
            buf.push(key);
        }

        let mut buf = Vec::new();
        let mut visited = FastSet::default();
        for idx in starts {
            if !visited.contains(&idx) {
                dfs(self, idx, &mut buf, &mut visited);
            }
        }
        buf
    }

    pub fn keys(&self) -> impl Iterator<Item = K> {
        self.vertices.keys().copied()
    }

    pub fn into_vertices(self) -> FastMap<K, V> {
        self.vertices
    }
}

#[cfg(test)]
mod tests {
    use crate::graph::Graph;

    #[test]
    fn test_topological_order() {
        let mut g = Graph::default();
        g.insert(0, "A");
        g.insert(1, "B");
        g.insert(2, "C");
        g.insert(3, "D");

        g.connect(0, 1);
        g.connect(0, 2);
        g.connect(1, 3);
        g.connect(2, 3);
        g.connect(3, 0);

        assert_eq!(g.order_topologically([0]), vec![3, 2, 1, 0]);
    }
}
