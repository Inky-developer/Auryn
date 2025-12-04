use crate::java::class;

#[derive(Debug, Clone)]
pub struct FieldDescriptor(pub String);

#[derive(Debug, Clone)]
pub struct MethodDescriptor(pub String);

#[derive(Debug, Clone)]
pub enum ConstantValue {
    String(String),
}

#[derive(Debug, Clone)]
pub enum Instruction {
    GetStatic {
        class_name: String,
        name: String,
        field_type: FieldDescriptor,
    },
    InvokeVirtual {
        class_name: String,
        name: String,
        method_type: MethodDescriptor,
    },
    LoadConstant {
        value: ConstantValue,
    },
    ReturnNull,
}

#[derive(Debug)]
pub struct Assembler {
    class_data: class::ClassData,
    instructions: Vec<class::Instruction>,
}

impl Assembler {
    pub fn new(class_name: String) -> Self {
        Self {
            class_data: class::ClassData::new(class_name),
            instructions: Vec::new(),
        }
    }

    pub fn assemble(mut self) -> class::ClassData {
        let name_index = self.class_data.add_utf8("main".to_string());
        let descriptor_index = self
            .class_data
            .add_utf8("([Ljava/lang/String;)V".to_string());
        let code_name_index = self.class_data.add_utf8("Code".to_string());
        let method = class::Method {
            flags: (class::MethodAccessFlag::Public as u16)
                | (class::MethodAccessFlag::Static as u16),
            name_index,
            descriptor_index,
            attributes: vec![class::AttributeInfo {
                name_index: code_name_index,
                attribute: class::Attribute::Code(class::CodeAttribute {
                    max_locals: 1,
                    code: self.instructions,
                }),
            }],
        };

        self.class_data.methods.push(method);

        self.class_data
    }

    pub fn add_all(&mut self, instructions: impl IntoIterator<Item = Instruction>) {
        for instruction in instructions {
            self.add(instruction);
        }
    }

    pub fn add(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::GetStatic {
                class_name,
                name,
                field_type,
            } => {
                let field_ref_index = self
                    .class_data
                    .add_field_ref(class_name, name, field_type.0);
                self.instructions
                    .push(class::Instruction::GetStatic(field_ref_index));
            }
            Instruction::InvokeVirtual {
                class_name,
                name,
                method_type,
            } => {
                let method_ref_index =
                    self.class_data
                        .add_method_ref(class_name, name, method_type.0);
                self.instructions
                    .push(class::Instruction::InvokeVirtual(method_ref_index));
            }
            Instruction::LoadConstant { value } => {
                let constant_index = match value {
                    ConstantValue::String(string) => self.class_data.add_string(string),
                };
                let constant_index = constant_index
                    .0
                    .get()
                    .try_into()
                    .expect("TODO: Implement support for higher indexes");
                self.instructions
                    .push(class::Instruction::Ldc(constant_index));
            }
            Instruction::ReturnNull => {
                self.instructions.push(class::Instruction::Return);
            }
        }
    }
}

impl class::ClassData {
    fn add_constant(&mut self, entry: class::ConstantPoolEntry) -> class::ConstantPoolIndex {
        let index = self.constant_pool.len() + 1;
        self.constant_pool.push(entry);
        class::ConstantPoolIndex::new(
            index
                .try_into()
                .expect("Should not be more than u16::MAX elements"),
        )
    }

    fn add_utf8(&mut self, text: String) -> class::ConstantPoolIndex {
        self.add_constant(class::ConstantPoolEntry::Utf8(text))
    }

    fn add_class(&mut self, class_name: String) -> class::ConstantPoolIndex {
        let name_index = self.add_utf8(class_name);
        self.add_constant(class::ConstantPoolEntry::Class { name_index })
    }

    fn add_name_and_type(&mut self, name: String, type_: String) -> class::ConstantPoolIndex {
        let name_index = self.add_utf8(name);
        let type_index = self.add_utf8(type_);
        self.add_constant(class::ConstantPoolEntry::NameAndType {
            name_index,
            type_index,
        })
    }

    fn add_field_ref(
        &mut self,
        class: String,
        name: String,
        field_type: String,
    ) -> class::ConstantPoolIndex {
        let class_index = self.add_class(class);
        let name_and_type_index = self.add_name_and_type(name, field_type);
        self.add_constant(class::ConstantPoolEntry::FieldRef {
            class_index,
            name_and_type_index,
        })
    }

    fn add_method_ref(
        &mut self,
        class: String,
        name: String,
        method_type: String,
    ) -> class::ConstantPoolIndex {
        let class_index = self.add_class(class);
        let name_and_type_index = self.add_name_and_type(name, method_type);
        self.add_constant(class::ConstantPoolEntry::MethodRef {
            class_index,
            name_and_type_index,
        })
    }

    fn add_string(&mut self, string: String) -> class::ConstantPoolIndex {
        let string_index = self.add_utf8(string);
        self.add_constant(class::ConstantPoolEntry::String { string_index })
    }
}

#[cfg(test)]
mod tests {
    use crate::java::assembler::{
        Assembler, ConstantValue, FieldDescriptor, Instruction, MethodDescriptor,
    };

    #[test]
    fn test_hello_world() {
        let mut assembler = Assembler::new("Helloworld".to_string());
        assembler.add_all([
            Instruction::GetStatic {
                class_name: "java/lang/System".to_string(),
                name: "out".to_string(),
                field_type: FieldDescriptor("Ljava/io/PrintStream;".to_string()),
            },
            Instruction::LoadConstant {
                value: ConstantValue::String("Hello World!".to_string()),
            },
            Instruction::InvokeVirtual {
                class_name: "java/io/PrintStream".to_string(),
                name: "println".to_string(),
                method_type: MethodDescriptor("(Ljava/lang/String;)V".to_string()),
            },
            Instruction::ReturnNull,
        ]);

        let class_data = assembler.assemble();

        insta::assert_debug_snapshot!(class_data);
    }
}
