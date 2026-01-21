use std::{num::NonZeroU16, ops::Index};

use crate::{
    auryn::codegen_java::representation::FieldDescriptor,
    java::class::{ConstantPool, ConstantPoolEntry, ConstantPoolIndex},
    utils::{default, fast_map::FastMap, small_string::SmallString},
};

#[derive(Debug)]
pub struct ConstantPoolBuilder {
    cache: FastMap<ConstantPoolEntry, ConstantPoolIndex>,
    entries: Vec<Option<ConstantPoolEntry>>,
    next_index: NonZeroU16,
}

impl ConstantPoolBuilder {
    pub fn build(self) -> ConstantPool {
        ConstantPool {
            entries: self.entries.into_boxed_slice(),
        }
    }

    pub fn add(&mut self, entry: ConstantPoolEntry) -> ConstantPoolIndex {
        if let Some(index) = self.cache.get(&entry) {
            return *index;
        }

        let index = ConstantPoolIndex(self.next_index);
        let size = entry.size_in_constant_pool();
        self.next_index = self.next_index.checked_add(size).unwrap();

        self.cache.insert(entry.clone(), index);
        self.entries.push(Some(entry));
        for _ in 1..size {
            self.entries.push(None);
        }

        index
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
        method_descriptor: SmallString,
    ) -> ConstantPoolIndex {
        let class_index = self.add_class(class);
        let name_and_type_index = self.add_name_and_type(name, method_descriptor);
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

    pub fn add_long(&mut self, long: i64) -> ConstantPoolIndex {
        self.add(ConstantPoolEntry::Long { long })
    }

    pub fn add_array_class(&mut self, inner: FieldDescriptor) -> ConstantPoolIndex {
        self.add_class(
            FieldDescriptor::Array {
                dimension_count: 1,
                descriptor: Box::new(inner),
            }
            .to_string()
            .into(),
        )
    }

    pub fn get_string_index(&mut self) -> ConstantPoolIndex {
        self.add_class("java/lang/String".into())
    }

    pub fn get_code_attribute_index(&mut self) -> ConstantPoolIndex {
        self.add_utf8("Code".into())
    }
}

impl Index<ConstantPoolIndex> for ConstantPoolBuilder {
    type Output = ConstantPoolEntry;

    fn index(&self, index: ConstantPoolIndex) -> &Self::Output {
        self.entries[index.0.get() as usize - 1].as_ref().unwrap()
    }
}

impl Default for ConstantPoolBuilder {
    fn default() -> Self {
        Self {
            cache: default(),
            entries: vec![None],
            next_index: NonZeroU16::new(1).unwrap(),
        }
    }
}
