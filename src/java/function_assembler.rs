use std::fmt::Display;

use crate::{
    java::{
        class::{self, ConstantPoolIndex, PrimitiveType, TypeCategory, VerificationTypeInfo},
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
}

impl FieldDescriptor {
    pub fn string() -> Self {
        FieldDescriptor::Object("java/lang/String".into())
    }

    pub fn print_stream() -> Self {
        FieldDescriptor::Object("java/io/PrintStream".into())
    }

    pub fn into_primitive(self) -> Primitive {
        match self {
            FieldDescriptor::Byte
            | FieldDescriptor::Char
            | FieldDescriptor::Integer
            | FieldDescriptor::Short
            | FieldDescriptor::Boolean => Primitive::Integer,
            FieldDescriptor::Object(name) => Primitive::Object(name),
            FieldDescriptor::Array {
                dimension_count,
                descriptor,
            } => {
                assert_eq!(dimension_count, 1, "Higher dimensions not implemented yet");
                Primitive::Array(Box::new(descriptor.into_primitive()))
            }
            other => todo!("No primitive for {other} yet"),
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
        }
    }
}

/// https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-ReturnDescriptor
#[derive(Debug, Clone)]
pub enum ReturnDescriptor {
    Value(FieldDescriptor),
    Void,
}

impl ReturnDescriptor {
    pub fn into_primitive(self) -> Option<Primitive> {
        match self {
            ReturnDescriptor::Value(value) => Some(value.into_primitive()),
            ReturnDescriptor::Void => None,
        }
    }
}

impl Display for ReturnDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReturnDescriptor::Value(field_descriptor) => field_descriptor.fmt(f),
            ReturnDescriptor::Void => f.write_str("V"),
        }
    }
}

impl From<FieldDescriptor> for ReturnDescriptor {
    fn from(value: FieldDescriptor) -> Self {
        ReturnDescriptor::Value(value)
    }
}

#[derive(Debug, Clone)]
pub struct MethodDescriptor {
    pub parameters: Vec<FieldDescriptor>,
    pub return_type: ReturnDescriptor,
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
    pub fn to_primitive(&self) -> Primitive {
        match self {
            ConstantValue::String(_) => Primitive::string(),
            ConstantValue::Integer(_) => Primitive::Integer,
        }
    }
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Primitive {
    Integer,
    Array(Box<Primitive>),
    Object(SmallString),
}

impl Primitive {
    pub fn string() -> Self {
        Primitive::Object("java/lang/String".into())
    }

    pub fn to_primitive_type_or_object(
        self,
        pool: &mut ConstantPoolBuilder,
    ) -> Result<PrimitiveType, ConstantPoolIndex> {
        match self {
            Primitive::Integer => Ok(PrimitiveType::Int),
            Primitive::Object(descriptor) => Err(pool.add_class(descriptor)),
            Primitive::Array(inner) => {
                Err(pool.add_class(inner.into_field_descriptor().to_string().into()))
            }
        }
    }

    pub fn into_field_descriptor(self) -> FieldDescriptor {
        match self {
            Primitive::Integer => FieldDescriptor::Integer,
            Primitive::Object(r#type) => FieldDescriptor::Object(r#type),
            Primitive::Array(element_type) => FieldDescriptor::Array {
                dimension_count: 1,
                descriptor: Box::new(element_type.into_field_descriptor()),
            },
        }
    }

    pub fn into_verification_type(self, pool: &mut ConstantPoolBuilder) -> VerificationTypeInfo {
        match self {
            Primitive::Integer => VerificationTypeInfo::Integer,
            Primitive::Array(inner) => VerificationTypeInfo::Object {
                constant_pool_index: pool.add_array_class(inner.into_field_descriptor()),
            },
            Primitive::Object(object) => VerificationTypeInfo::Object {
                constant_pool_index: pool.add_class(object),
            },
        }
    }

    pub fn category(&self) -> TypeCategory {
        match self {
            Primitive::Integer | Primitive::Array(_) | Primitive::Object(_) => TypeCategory::Normal,
        }
    }

    pub fn stack_size(&self) -> u16 {
        self.category().stack_size()
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
    /// Creates a new array of the given type
    NewArray(Primitive),
    /// Stores a value into an array at a given index.
    /// Stack: ..., arrayref, index, value -> ...
    ArrayStore(Primitive),
    /// Loads a value of the given type from an array
    /// Stack: ..., arrayref, index -> ..., value
    ArrayLoad(Primitive),
    /// Returns the length of the given array
    ArrayLength,
    IAdd,
    ISub,
    IMul,
    Store(VariableId),
    Load(VariableId),
    Nop,
    /// Pops a single value of the given category from the stack
    Pop(TypeCategory),
    /// Duplicates the topmost stack entry
    Dup(TypeCategory),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VariableId {
    pub index: u16,
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
            .clone()
            .into_iter()
            .map(|it| {
                it.into_primitive()
                    .into_verification_type(self.constant_pool)
            })
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
            ReturnDescriptor,
        },
    };

    #[test]
    fn test_hello_world() {
        let mut pool = ConstantPoolBuilder::default();
        let mut assembler = FunctionAssembler::new(
            "main".into(),
            MethodDescriptor {
                parameters: vec![],
                return_type: ReturnDescriptor::Void,
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
                    return_type: ReturnDescriptor::Void,
                },
            },
        ]);

        let method = assembler.assemble();

        insta::assert_debug_snapshot!((pool, method));
    }
}
