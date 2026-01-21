use crate::{
    auryn::codegen_java::representation::{
        FieldDescriptor, ImplicitArgs, MethodDescriptor, Representation,
    },
    java::{
        class::{self, PrimitiveType, TypeCategory, VerificationTypeInfo},
        constant_pool_builder::ConstantPoolBuilder,
        source_graph::{BasicBlock, BasicBlockId, SourceGraph},
    },
    utils::small_string::SmallString,
};

#[derive(Debug, Clone)]
pub enum ConstantValue {
    String(SmallString),
    Integer(i32),
    Long(i64),
    Boolean(bool),
}
impl ConstantValue {
    pub fn to_primitive(&self) -> Representation {
        match self {
            ConstantValue::String(_) => Representation::string(),
            ConstantValue::Integer(_) => Representation::Integer,
            ConstantValue::Long(_) => Representation::Long,
            ConstantValue::Boolean(_) => Representation::Boolean,
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
            ConstantValue::Integer(_) | ConstantValue::Boolean(_) => VerificationTypeInfo::Integer,
            ConstantValue::Long(_) => VerificationTypeInfo::Long,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    /// Creaes a new object of the given class name
    New(SmallString),
    /// Loads a class variable
    GetStatic {
        class_name: SmallString,
        name: SmallString,
        field_descriptor: FieldDescriptor,
    },
    /// Loads a non static field from a class
    GetField {
        class_name: SmallString,
        name: SmallString,
        field_descriptor: FieldDescriptor,
    },
    /// Stores a non static field into a class
    PutField {
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
    /// Invokes a init or super method
    InvokeSpecial {
        class_name: SmallString,
        name: SmallString,
        method_descriptor: MethodDescriptor,
    },
    LoadConstant {
        value: ConstantValue,
    },
    /// Creates a new array of the given type
    NewArray(Representation),
    /// Stores a value into an array at a given index.
    /// Stack: ..., arrayref, index, value -> ...
    ArrayStore(Representation),
    /// Loads a value of the given type from an array
    /// Stack: ..., arrayref, index -> ..., value
    ArrayLoad(Representation),
    /// Returns the length of the given array
    ArrayLength,
    Add(PrimitiveType),
    Sub(PrimitiveType),
    Mul(PrimitiveType),
    Div(PrimitiveType),
    Rem(PrimitiveType),
    Store(VariableId),
    Load(VariableId),
    IntToLong,
    LongToInt,
    Nop,
    /// Does not correspond to any jvm instruction.
    /// Assumes that the top most stack value is now an object of the given type
    Transmute(SmallString),
    /// Pops a single value of the given category from the stack
    Pop(TypeCategory),
    /// Duplicates the topmost stack entry
    Dup(TypeCategory),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VariableId {
    pub index: u16,
    pub r#type: Representation,
}

#[derive(Debug)]
pub struct FunctionAssembler<'a> {
    pub function_name: SmallString,
    pub descriptor: MethodDescriptor,
    pub constant_pool: &'a mut ConstantPoolBuilder,
    implicit_args: ImplicitArgs,
    blocks: SourceGraph,
    next_variable_index: u16,
}

impl<'a> FunctionAssembler<'a> {
    pub fn new(
        name: SmallString,
        descriptor: MethodDescriptor,
        implicit_args: ImplicitArgs,
        constant_pool: &'a mut ConstantPoolBuilder,
    ) -> Self {
        Self {
            function_name: name,
            next_variable_index: descriptor.first_variable_index(implicit_args),
            implicit_args,
            descriptor,
            constant_pool,
            blocks: SourceGraph::default(),
        }
    }

    pub fn assemble(self) -> class::Method {
        let name_index = self.constant_pool.add_utf8(self.function_name);
        let descriptor_index = self
            .constant_pool
            .add_utf8(self.descriptor.to_string().into());
        let this_type = self.implicit_args.as_verification_type(name_index);
        let function_parameters = std::iter::chain(
            this_type,
            self.descriptor.parameters.clone().into_iter().map(|it| {
                it.into_primitive()
                    .into_verification_type(self.constant_pool)
            }),
        )
        .collect();
        let (class_instructions, stack_map_frame) = self
            .blocks
            .assemble(self.constant_pool, function_parameters);

        let flags = {
            let mut flags = class::MethodAccessFlags::PUBLIC;
            if matches!(self.implicit_args, ImplicitArgs::None) {
                flags |= class::MethodAccessFlags::STATIC;
            }
            flags
        };

        let code_name_index = self.constant_pool.get_code_attribute_index();
        // It must be at least 2 bigger than the highest index to a 2-sized variable
        let max_locals = self.next_variable_index + 1;
        class::Method {
            flags,
            name_index,
            descriptor_index,
            attributes: vec![class::AttributeInfo {
                name_index: code_name_index,
                attribute: class::Attribute::Code(class::CodeAttribute {
                    max_locals,
                    // TODO calculate that
                    max_stack: 8,
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

    pub fn alloc_variable(&mut self, r#type: Representation) -> VariableId {
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
    use crate::{
        auryn::codegen_java::representation::{
            FieldDescriptor, ImplicitArgs, MethodDescriptor, ReturnDescriptor,
        },
        java::{
            constant_pool_builder::ConstantPoolBuilder,
            function_assembler::{ConstantValue, FunctionAssembler, Instruction},
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
            ImplicitArgs::None,
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
