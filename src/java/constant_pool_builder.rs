use std::ops::Index;

use crate::{
    java::class::{ConstantPoolEntry, ConstantPoolIndex},
    utils::{fast_map::FastIndexMap, small_string::SmallString},
};

#[derive(Debug, Default)]
pub struct ConstantPoolBuilder {
    entries: FastIndexMap<ConstantPoolEntry, ConstantPoolIndex>,
}

impl ConstantPoolBuilder {
    pub fn build(self) -> Vec<ConstantPoolEntry> {
        let mut as_vec = self.entries.into_iter().collect::<Vec<_>>();
        as_vec.sort_unstable_by_key(|(_, key)| key.0);
        as_vec.into_iter().map(|(value, _)| value).collect()
    }

    pub fn add(&mut self, entry: ConstantPoolEntry) -> ConstantPoolIndex {
        let entry = self.entries.entry(entry);
        let index: u16 = entry
            .index()
            .try_into()
            .expect("Should not be too many entries");
        *entry.or_insert(ConstantPoolIndex::new(index + 1))
    }

    pub fn add_utf8(&mut self, text: SmallString) -> ConstantPoolIndex {
        self.add(ConstantPoolEntry::Utf8(text))
    }

    pub fn add_class(&mut self, class_name: SmallString) -> ConstantPoolIndex {
        let name_index = self.add_utf8(class_name);
        self.add(ConstantPoolEntry::Class { name_index })
    }

    pub fn add_name_and_type(
        &mut self,
        name: SmallString,
        r#type: SmallString,
    ) -> ConstantPoolIndex {
        let name_index = self.add_utf8(name);
        let type_index = self.add_utf8(r#type);
        self.add(ConstantPoolEntry::NameAndType {
            name_index,
            type_index,
        })
    }

    pub fn add_field_ref(
        &mut self,
        class: SmallString,
        name: SmallString,
        field_type: SmallString,
    ) -> ConstantPoolIndex {
        let class_index = self.add_class(class);
        let name_and_type_index = self.add_name_and_type(name, field_type);
        self.add(ConstantPoolEntry::FieldRef {
            class_index,
            name_and_type_index,
        })
    }

    pub fn add_method_ref(
        &mut self,
        class: SmallString,
        name: SmallString,
        method_type: SmallString,
    ) -> ConstantPoolIndex {
        let class_index = self.add_class(class);
        let name_and_type_index = self.add_name_and_type(name, method_type);
        self.add(ConstantPoolEntry::MethodRef {
            class_index,
            name_and_type_index,
        })
    }

    pub fn add_string(&mut self, string: SmallString) -> ConstantPoolIndex {
        let string_index = self.add_utf8(string);
        self.add(ConstantPoolEntry::String { string_index })
    }

    pub fn add_integer(&mut self, integer: i32) -> ConstantPoolIndex {
        self.add(ConstantPoolEntry::Integer { integer })
    }

    pub fn get_string_index(&mut self) -> ConstantPoolIndex {
        self.add_class("java/lang/String".into())
    }
}

impl Index<ConstantPoolIndex> for ConstantPoolBuilder {
    type Output = ConstantPoolEntry;

    fn index(&self, index: ConstantPoolIndex) -> &Self::Output {
        self.entries
            .get_index(index.0.get() as usize - 1)
            .map(|(k, _)| k)
            .expect("Should be a valid index")
    }
}
