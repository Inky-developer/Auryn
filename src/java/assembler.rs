use std::fmt::Display;

use crate::java::{
    class::{self, StackMapTableAttribute, TypeCategory, VerificationTypeInfo},
    constant_pool_builder::ConstantPoolBuilder,
    source_graph::{BasicBlock, BasicBlockId, SourceGraph},
};

/// https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-FieldType
#[derive(Debug, Clone)]
pub enum FieldDescriptor {
    Byte,
    Char,
    Double,
    Float,
    Integer,
    Long,
    Object(String),
    Short,
    Boolean,
    Array {
        dimension_count: u8,
        descriptor: Box<FieldDescriptor>,
    },
    // Technically not part of field descriptors, but is easier implemented than adding a new enum like FieldDescriptorWithVoid
    Void,
}

impl FieldDescriptor {
    /// https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-4.10.1.2
    pub fn to_verification_type(
        &self,
        constant_pool_builder: &mut ConstantPoolBuilder,
    ) -> VerificationTypeInfo {
        match self {
            FieldDescriptor::Byte
            | FieldDescriptor::Char
            | FieldDescriptor::Integer
            | FieldDescriptor::Short
            | FieldDescriptor::Boolean => VerificationTypeInfo::Integer,
            FieldDescriptor::Double => VerificationTypeInfo::Double,
            FieldDescriptor::Float => VerificationTypeInfo::Float,
            FieldDescriptor::Long => VerificationTypeInfo::Long,
            FieldDescriptor::Object(name) => {
                let constant_pool_index = constant_pool_builder.add_class(name.clone());
                VerificationTypeInfo::Object {
                    constant_pool_index,
                }
            }
            // Arrays are represented as classes with the name matching their field descriptor
            // https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-4.7.4
            FieldDescriptor::Array { .. } => {
                let constant_pool_index = constant_pool_builder.add_class(self.to_string());
                VerificationTypeInfo::Object {
                    constant_pool_index,
                }
            }
            FieldDescriptor::Void => {
                panic!("Should never try to convert a void descriptor into a verification type!")
            }
        }
    }
}

impl Display for FieldDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FieldDescriptor::Byte => write!(f, "B"),
            FieldDescriptor::Char => write!(f, "C"),
            FieldDescriptor::Double => write!(f, "D"),
            FieldDescriptor::Float => write!(f, "F"),
            FieldDescriptor::Integer => write!(f, "I"),
            FieldDescriptor::Long => write!(f, "J"),
            FieldDescriptor::Object(class_name) => write!(f, "L{class_name};"),
            FieldDescriptor::Short => write!(f, "S"),
            FieldDescriptor::Boolean => write!(f, "Z"),
            FieldDescriptor::Array {
                dimension_count,
                descriptor,
            } => {
                for _ in 0..*dimension_count {
                    write!(f, "[")?;
                }
                descriptor.fmt(f)?;
                Ok(())
            }
            FieldDescriptor::Void => write!(f, "V"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MethodDescriptor {
    pub arguments: Vec<FieldDescriptor>,
    pub return_type: FieldDescriptor,
}

impl Display for MethodDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for arg in &self.arguments {
            arg.fmt(f)?;
        }
        write!(f, ")")?;

        self.return_type.fmt(f)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum ConstantValue {
    String(String),
    Integer(i32),
}
impl ConstantValue {
    pub fn to_verification_type(
        &self,
        constant_pool_builder: &mut ConstantPoolBuilder,
    ) -> VerificationTypeInfo {
        match self {
            ConstantValue::String(_) => VerificationTypeInfo::Object {
                constant_pool_index: constant_pool_builder.get_string_index(),
            },
            ConstantValue::Integer(_) => VerificationTypeInfo::Integer,
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Primitive {
    Integer,
}

impl Primitive {
    pub fn to_verification_type(&self) -> VerificationTypeInfo {
        match self {
            Primitive::Integer => VerificationTypeInfo::Integer,
        }
    }

    pub fn stack_size(&self) -> u16 {
        self.to_verification_type().category().stack_size()
    }
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
    IAdd,
    IMul,
    Store(VariableId),
    Load(VariableId),
    Nop,
    /// Pops a single value of the given category from the stack
    Pop(TypeCategory),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct VariableId {
    pub(super) index: u16,
    pub r#type: Primitive,
}

#[derive(Debug)]
pub struct Assembler {
    pub constant_pool: ConstantPoolBuilder,
    blocks: SourceGraph,
    next_variable_index: u16,
    pub function_arguments: Vec<VerificationTypeInfo>,
}

impl Assembler {
    pub fn new() -> Self {
        let mut constant_pool = ConstantPoolBuilder::default();
        let main_function_arguments = vec![VerificationTypeInfo::Object {
            constant_pool_index: constant_pool.add_class("[Ljava/lang/String;".to_string()),
        }];
        let next_variable_index = main_function_arguments.len() as u16;
        Self {
            constant_pool,
            blocks: SourceGraph::default(),
            next_variable_index,
            function_arguments: main_function_arguments,
        }
    }

    pub fn assemble(mut self, class_name: String) -> class::ClassData {
        let (class_instructions, verification_frames) = self
            .blocks
            .assemble(&mut self.constant_pool, self.function_arguments);
        let name_index = self.constant_pool.add_utf8("main".to_string());
        let descriptor_index = self
            .constant_pool
            .add_utf8("([Ljava/lang/String;)V".to_string());
        let code_name_index = self.constant_pool.add_utf8("Code".to_string());
        let stack_map_frame = StackMapTableAttribute::from(verification_frames);
        // It must be at least 2 bigger than the highest index to a 2-sized variable
        let max_locals = self.next_variable_index + 1;
        let method = class::Method {
            flags: (class::MethodAccessFlag::Public as u16)
                | (class::MethodAccessFlag::Static as u16),
            name_index,
            descriptor_index,
            attributes: vec![class::AttributeInfo {
                name_index: code_name_index,
                attribute: class::Attribute::Code(class::CodeAttribute {
                    max_locals,
                    code: class_instructions,
                    attributes: vec![class::AttributeInfo {
                        name_index: self.constant_pool.add_utf8("StackMapTable".to_string()),
                        attribute: class::Attribute::StackMapTable(stack_map_frame),
                    }],
                }),
            }],
        };

        let mut class_data = class::ClassData::new(class_name, self.constant_pool);
        class_data.methods.push(method);
        class_data
    }

    pub fn current_block_id(&self) -> BasicBlockId {
        self.blocks.current
    }

    pub fn add_all(&mut self, instructions: impl IntoIterator<Item = Instruction>) {
        for instruction in instructions {
            self.add(instruction);
        }
    }

    pub fn add(&mut self, instruction: Instruction) {
        self.blocks.add(instruction);
    }

    pub fn alloc_variable(&mut self, r#type: Primitive) -> VariableId {
        let id = self.next_variable_index;
        self.next_variable_index += r#type.stack_size();
        VariableId { index: id, r#type }
    }

    pub fn add_block(&mut self) -> BasicBlockId {
        self.blocks.add_block()
    }

    pub fn current_block_mut(&mut self) -> &mut BasicBlock {
        self.blocks.current_block_mut()
    }

    pub fn set_current_block_id(&mut self, new_block_id: BasicBlockId) {
        self.blocks.current = new_block_id;
    }
}

#[cfg(test)]
mod tests {
    use crate::java::assembler::{
        Assembler, ConstantValue, FieldDescriptor, Instruction, MethodDescriptor,
    };

    #[test]
    fn test_hello_world() {
        let mut assembler = Assembler::new();
        assembler.add_all([
            Instruction::GetStatic {
                class_name: "java/lang/System".to_string(),
                name: "out".to_string(),
                field_type: FieldDescriptor::Object("java/io/PrintStream".to_string()),
            },
            Instruction::LoadConstant {
                value: ConstantValue::String("Hello World!".to_string()),
            },
            Instruction::InvokeVirtual {
                class_name: "java/io/PrintStream".to_string(),
                name: "println".to_string(),
                method_type: MethodDescriptor {
                    arguments: vec![FieldDescriptor::Object("java/lang/String".to_string())],
                    return_type: FieldDescriptor::Void,
                },
            },
            Instruction::ReturnNull,
        ]);

        let class_data = assembler.assemble("Helloworld".to_string());

        insta::assert_debug_snapshot!(class_data);
    }
}
