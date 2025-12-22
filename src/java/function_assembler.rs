use std::fmt::Display;

use crate::{
    java::{
        class::{self, ConstantPoolEntry, ConstantPoolIndex, TypeCategory, VerificationTypeInfo},
        constant_pool_builder::ConstantPoolBuilder,
        source_graph::{BasicBlock, BasicBlockId, SourceGraph},
    },
    utils::small_string::SmallString,
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
    Object(SmallString),
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
    pub fn string() -> Self {
        FieldDescriptor::Object("java/lang/String".into())
    }

    pub fn print_stream() -> Self {
        FieldDescriptor::Object("java/io/PrintStream".into())
    }

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
                let constant_pool_index = constant_pool_builder.add_class(self.to_string().into());
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
    pub parameters: Vec<FieldDescriptor>,
    pub return_type: FieldDescriptor,
}

impl Display for MethodDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for arg in &self.parameters {
            arg.fmt(f)?;
        }
        write!(f, ")")?;

        self.return_type.fmt(f)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum ConstantValue {
    String(SmallString),
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
    Object(ConstantPoolIndex),
}

impl Primitive {
    pub fn to_field_descriptor(self, pool: &mut ConstantPoolBuilder) -> FieldDescriptor {
        match self {
            Primitive::Integer => FieldDescriptor::Integer,
            Primitive::Object(constant_pool_index) => {
                let ConstantPoolEntry::Class { name_index } = &pool[constant_pool_index] else {
                    unreachable!("Invalid object ref");
                };
                let ConstantPoolEntry::Utf8(text) = &pool[*name_index] else {
                    unreachable!("Invalid class ref");
                };
                FieldDescriptor::Object(text.clone())
            }
        }
    }

    pub fn to_verification_type(self) -> VerificationTypeInfo {
        match self {
            Primitive::Integer => VerificationTypeInfo::Integer,
            Primitive::Object(object) => VerificationTypeInfo::Object {
                constant_pool_index: object,
            },
        }
    }

    pub fn stack_size(&self) -> u16 {
        self.to_verification_type().category().stack_size()
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    /// Loads a class variable
    GetStatic {
        class_name: SmallString,
        name: SmallString,
        field_descriptor: FieldDescriptor,
    },
    /// Invokes an instance method
    InvokeVirtual {
        class_name: SmallString,
        name: SmallString,
        method_descriptor: MethodDescriptor,
    },
    /// Invokes a class method
    InvokeStatic {
        class_name: SmallString,
        name: SmallString,
        method_descriptor: MethodDescriptor,
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
pub struct FunctionAssembler<'a> {
    pub function_name: SmallString,
    pub descriptor: MethodDescriptor,
    pub constant_pool: &'a mut ConstantPoolBuilder,
    blocks: SourceGraph,
    next_variable_index: u16,
}

impl<'a> FunctionAssembler<'a> {
    pub fn new(
        name: SmallString,
        descriptor: MethodDescriptor,
        constant_pool: &'a mut ConstantPoolBuilder,
    ) -> Self {
        let next_variable_index = descriptor.parameters.len().try_into().unwrap();
        Self {
            function_name: name,
            descriptor,
            constant_pool,
            blocks: SourceGraph::default(),
            next_variable_index,
        }
    }

    pub fn assemble(self) -> class::Method {
        let function_parameters = self
            .descriptor
            .parameters
            .iter()
            .map(|it| it.to_verification_type(self.constant_pool))
            .collect();
        let (class_instructions, stack_map_frame) = self
            .blocks
            .assemble(self.constant_pool, function_parameters);
        let name_index = self.constant_pool.add_utf8(self.function_name);
        let descriptor_index = self
            .constant_pool
            .add_utf8(self.descriptor.to_string().into());
        let code_name_index = self.constant_pool.add_utf8("Code".into());
        // It must be at least 2 bigger than the highest index to a 2-sized variable
        let max_locals = self.next_variable_index + 1;
        class::Method {
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
                        name_index: self.constant_pool.add_utf8("StackMapTable".into()),
                        attribute: class::Attribute::StackMapTable(stack_map_frame),
                    }],
                }),
            }],
        }
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
    use crate::java::{
        constant_pool_builder::ConstantPoolBuilder,
        function_assembler::{
            ConstantValue, FieldDescriptor, FunctionAssembler, Instruction, MethodDescriptor,
        },
    };

    #[test]
    fn test_hello_world() {
        let mut pool = ConstantPoolBuilder::default();
        let mut assembler = FunctionAssembler::new(
            "main".into(),
            MethodDescriptor {
                parameters: vec![],
                return_type: FieldDescriptor::Void,
            },
            &mut pool,
        );
        assembler.add_all([
            Instruction::GetStatic {
                class_name: "java/lang/System".into(),
                name: "out".into(),
                field_descriptor: FieldDescriptor::print_stream(),
            },
            Instruction::LoadConstant {
                value: ConstantValue::String("Hello World!".into()),
            },
            Instruction::InvokeVirtual {
                class_name: "java/io/PrintStream".into(),
                name: "println".into(),
                method_descriptor: MethodDescriptor {
                    parameters: vec![FieldDescriptor::string()],
                    return_type: FieldDescriptor::Void,
                },
            },
            Instruction::ReturnNull,
        ]);

        let method = assembler.assemble();

        insta::assert_debug_snapshot!((pool, method));
    }
}
