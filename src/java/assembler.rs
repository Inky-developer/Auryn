use std::{fmt::Display, marker::PhantomData};

use crate::{
    java::{
        class::{self, JumpPoint, StackMapFrame, StackMapTableAttribute, VerificationTypeInfo},
        constant_pool_builder::ConstantPoolBuilder,
        symbolic_evaluation::SymbolicEvaluator,
    },
    utils::fast_map::FastMap,
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

pub mod primitive {
    pub trait IsPrimitiveType {
        // The size of this type in the local variable frame. Either 1 or 2
        const SIZE: u16;
    }

    #[derive(Debug, Clone, Copy)]
    pub struct Integer;
    impl IsPrimitiveType for Integer {
        const SIZE: u16 = 1;
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
    IStore(VariableId<primitive::Integer>),
    ILoad(VariableId<primitive::Integer>),
    Goto(JumpPoint),
    Nop,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct VariableId<T>(u16, PhantomData<T>);

#[derive(Debug)]
pub struct VerificationFrame {
    locals: Vec<VerificationTypeInfo>,
    stack: Vec<VerificationTypeInfo>,
}

#[derive(Debug)]
pub struct Assembler {
    constant_pool: ConstantPoolBuilder,
    instructions: Vec<class::Instruction>,
    // Map from instruction byte index to stack map frame that should be generated for it
    verification_frames: FastMap<u16, VerificationFrame>,
    symbolic_evaluator: SymbolicEvaluator,
    next_variable_index: u16,
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
            instructions: Vec::new(),
            verification_frames: FastMap::default(),
            symbolic_evaluator: SymbolicEvaluator::new(main_function_arguments),
            next_variable_index,
        }
    }

    pub fn assemble(mut self, class_name: String) -> class::ClassData {
        let name_index = self.constant_pool.add_utf8("main".to_string());
        let descriptor_index = self
            .constant_pool
            .add_utf8("([Ljava/lang/String;)V".to_string());
        let code_name_index = self.constant_pool.add_utf8("Code".to_string());
        let stack_map_frame = StackMapTableAttribute::from(self.verification_frames);
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
                    code: self.instructions,
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

    pub fn add_all(&mut self, instructions: impl IntoIterator<Item = Instruction>) {
        for instruction in instructions {
            self.add(instruction);
        }
    }

    pub fn add(&mut self, instruction: Instruction) {
        self.symbolic_evaluator
            .eval(&instruction, &mut self.constant_pool);
        let class_instruction = self.convert_to_class_instruction(instruction);
        self.instructions.push(class_instruction);
    }

    pub fn add_jump_target(&mut self) -> JumpPoint {
        // FIXME: lets maybe not have quadratic complexity lol
        let mut scratch_buffer = Vec::new();
        for instruction in &self.instructions {
            instruction
                .serialize(&mut scratch_buffer, 0)
                .expect("Should be able to write");
        }
        let current_byte_index: u16 = scratch_buffer
            .len()
            .try_into()
            .expect("Should not have too many instructions");

        let frame = VerificationFrame {
            locals: self.symbolic_evaluator.locals.clone(),
            stack: self.symbolic_evaluator.stack.clone(),
        };
        self.verification_frames.insert(current_byte_index, frame);

        JumpPoint(current_byte_index)
    }

    pub fn alloc_variable<T: primitive::IsPrimitiveType>(&mut self) -> VariableId<T> {
        let id = self.next_variable_index;
        self.next_variable_index += T::SIZE;
        VariableId(id, PhantomData)
    }
}

impl Assembler {
    fn convert_to_class_instruction(&mut self, instruction: Instruction) -> class::Instruction {
        match instruction {
            Instruction::GetStatic {
                class_name,
                name,
                field_type,
            } => {
                let field_ref_index =
                    self.constant_pool
                        .add_field_ref(class_name, name, field_type.to_string());
                class::Instruction::GetStatic(field_ref_index)
            }
            Instruction::InvokeVirtual {
                class_name,
                name,
                method_type,
            } => {
                let method_ref_index =
                    self.constant_pool
                        .add_method_ref(class_name, name, method_type.to_string());
                class::Instruction::InvokeVirtual(method_ref_index)
            }
            Instruction::LoadConstant { value } => {
                let constant_index = match value {
                    ConstantValue::String(string) => self.constant_pool.add_string(string),
                    ConstantValue::Integer(integer) => self.constant_pool.add_integer(integer),
                };
                let constant_index = constant_index
                    .0
                    .get()
                    .try_into()
                    .expect("TODO: Implement support for higher indexes");
                class::Instruction::Ldc(constant_index)
            }
            Instruction::ReturnNull => class::Instruction::Return,
            Instruction::IAdd => class::Instruction::IAdd,
            Instruction::IMul => class::Instruction::IMul,
            Instruction::IStore(index) => class::Instruction::IStore(index.0),
            Instruction::ILoad(index) => class::Instruction::ILoad(index.0),
            Instruction::Goto(jump_point) => class::Instruction::Goto(jump_point),
            Instruction::Nop => class::Instruction::Nop,
        }
    }
}

impl From<FastMap<u16, VerificationFrame>> for StackMapTableAttribute {
    /// https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-4.7.4
    fn from(value: FastMap<u16, VerificationFrame>) -> Self {
        let mut current_offset: u16 = 0;
        let mut sorted_entries = value.into_iter().collect::<Vec<_>>();
        sorted_entries.sort_unstable_by_key(|(index, _)| *index);

        // Skip frames at index 0, since they will be automatically generated by java
        let frames = sorted_entries
            .into_iter()
            .map(move |(byte_index, frame)| {
                // An offset of 0 indicates that the frame applies to the next instruction, so we need to subtract 1 here, unless this is the first frame
                // In which case this must not be done according to spec.
                let offset_to_last: u16 = if current_offset == 0 {
                    byte_index
                } else {
                    (byte_index - current_offset - 1)
                        .try_into()
                        .expect("Should not to to big")
                };
                current_offset = byte_index;

                // TODO: Try encode into a more compact representation
                StackMapFrame::Full {
                    offset_delta: offset_to_last,
                    locals: frame.locals,
                    stack: frame.stack,
                }
            })
            .collect();

        StackMapTableAttribute { entries: frames }
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
