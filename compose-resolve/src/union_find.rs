use fxhash::FxHashMap;
use std::fmt::Debug;
use std::hash::Hash;

#[derive(Debug, Clone)]
pub struct UnionFind<T> {
    elems: FxHashMap<T, UnionFindId<T>>,
    parents: Vec<UnionFindId<T>>,
    ranks: Vec<usize>,
}

impl<T> PartialEq for UnionFind<T>
where
    T: Eq + Hash,
{
    fn eq(&self, other: &Self) -> bool {
        self.elems == other.elems && self.parents == other.parents && self.ranks == other.ranks
    }
}

pub struct UnionFindId<T> {
    id: usize,
    phantom: std::marker::PhantomData<T>,
}

impl<T> Clone for UnionFindId<T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            phantom: std::marker::PhantomData,
        }
    }
}
impl<T> Copy for UnionFindId<T> {}

impl<T> UnionFindId<T> {
    fn new(id: usize) -> Self {
        Self {
            id,
            phantom: std::marker::PhantomData,
        }
    }

    fn as_usize(&self) -> usize {
        self.id
    }
}

impl<T> PartialEq for UnionFindId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<T> Eq for UnionFindId<T> {}
impl<T> Hash for UnionFindId<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<T> Debug for UnionFindId<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("UnionFindId").field(&self.id).finish()
    }
}

impl<T> UnionFind<T>
where
    T: Eq + Hash,
{
    pub fn new() -> Self {
        UnionFind {
            elems: FxHashMap::default(),
            parents: vec![],
            ranks: vec![],
        }
    }

    pub fn add(&mut self, elem: T) {
        self.elems
            .insert(elem, UnionFindId::new(self.parents.len()));
        self.parents.push(UnionFindId::new(self.parents.len()));
        self.ranks.push(0);
    }

    pub fn find(&self, elem: &T) -> Option<UnionFindId<T>> {
        let mut elem_id = *self.elems.get(elem)?;

        loop {
            if elem_id == *self.parents.get(elem_id.as_usize())? {
                break Some(elem_id);
            }

            elem_id = *self.parents.get(elem_id.as_usize())?;
        }
    }

    pub fn find_shorten(&mut self, elem: &T) -> Option<UnionFindId<T>> {
        let elem_id = *self.elems.get(elem)?;
        self.find_shorten_id(elem_id)
    }

    fn find_shorten_id(&mut self, id: UnionFindId<T>) -> Option<UnionFindId<T>> {
        let parent = self.parents[id.as_usize()];
        if parent == id {
            return Some(id);
        }
        let new_parent = self.find_shorten_id(parent)?;
        self.parents[id.as_usize()] = new_parent;

        Some(new_parent)
    }

    pub fn union(&mut self, elem1: &T, elem2: &T) {
        let id1 = self.find(elem1).unwrap();
        let id2 = self.find(elem2).unwrap();
        if id1 == id2 {
            return;
        }

        if self.ranks[id1.as_usize()] < self.ranks[id2.as_usize()] {
            self.parents[id1.as_usize()] = id2;
            return;
        }

        self.parents[id2.as_usize()] = id1;

        if self.ranks[id1.as_usize()] == self.ranks[id2.as_usize()] {
            self.ranks[id1.as_usize()] += 1;
        }
    }

    pub fn same_set(&self, elem1: &T, elem2: &T) -> bool {
        self.find(elem1) == self.find(elem2)
    }
}

impl<T> Default for UnionFind<T>
where
    T: Eq + Hash,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<T> FromIterator<T> for UnionFind<T>
where
    T: Eq + Hash,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut uf = Self::new();
        for elem in iter {
            uf.add(elem);
        }
        uf
    }
}

#[cfg(test)]
mod tests {
    use crate::union_find::{UnionFind, UnionFindId};

    #[test]
    fn find_union() {
        let mut uf = (0..10).collect::<UnionFind<_>>();
        uf.union(&0, &1);
        uf.union(&2, &3);
        uf.union(&4, &5);

        uf.union(&0, &6);
        uf.union(&6, &7);

        uf.union(&2, &8);
        uf.union(&8, &9);

        assert_eq!(uf.find(&0), Some(UnionFindId::new(0)));
        assert_eq!(uf.find(&1), Some(UnionFindId::new(0)));

        assert_eq!(uf.find(&2), Some(UnionFindId::new(2)));
        assert_eq!(uf.find(&3), Some(UnionFindId::new(2)));

        assert_eq!(uf.find(&4), Some(UnionFindId::new(4)));
        assert_eq!(uf.find(&5), Some(UnionFindId::new(4)));

        assert_eq!(uf.find(&6), Some(UnionFindId::new(0)));
        assert_eq!(uf.find(&7), Some(UnionFindId::new(0)));

        assert_eq!(uf.find(&8), Some(UnionFindId::new(2)));
        assert_eq!(uf.find(&9), Some(UnionFindId::new(2)));
    }

    #[test]
    fn find_union_shorten() {
        let mut uf = (0..10).collect::<UnionFind<_>>();

        uf.union(&0, &4);
        uf.union(&4, &5);
        uf.union(&5, &6);
        uf.union(&4, &2);

        uf.union(&7, &8);
        uf.union(&8, &9);
        uf.union(&0, &7);


        assert_eq!(uf.parents[9], UnionFindId::new(7));
        assert_eq!(uf.find_shorten(&9), Some(UnionFindId::new(0)));
        assert_eq!(uf.parents[9], UnionFindId::new(0));
    }

    #[test]
    fn same_set() {
        let mut uf = (0..10).collect::<UnionFind<_>>();
        uf.union(&0, &1);
        uf.union(&2, &3);
        uf.union(&4, &5);

        uf.union(&0, &6);
        uf.union(&6, &7);

        uf.union(&2, &8);
        uf.union(&8, &9);

        assert!(uf.same_set(&0, &1));
        assert!(uf.same_set(&2, &3));
        assert!(uf.same_set(&4, &5));

        assert!(uf.same_set(&6, &7));
        assert!(uf.same_set(&8, &9));

        assert!(uf.same_set(&2, &9));
        assert!(uf.same_set(&0, &6));

        assert!(!uf.same_set(&0, &2));
        assert!(!uf.same_set(&4, &8));
        assert!(!uf.same_set(&6, &9));
        assert!(!uf.same_set(&7, &8));
    }
}
