use std::fmt::Debug;

use crate::{
    auryn::codegen_java::representation::ReturnDescriptor,
    java::{
        class::VerificationTypeInfo,
        constant_pool_builder::ConstantPoolBuilder,
        function_assembler::Instruction,
        source_graph::{BasicBlock, BlockFinalizer},
    },
};

/// Symbolic evaluator that keeps track of data relevant for stack map frames
#[derive(Debug)]
pub struct SymbolicEvaluator {
    pub stack: Vec<VerificationTypeInfo>,
    pub locals: Vec<VerificationTypeInfo>,
}

impl SymbolicEvaluator {
    pub fn new(function_arguments: Vec<VerificationTypeInfo>) -> Self {
        Self {
            stack: Vec::default(),
            locals: function_arguments,
        }
    }

    pub fn eval_block(&mut self, block: &BasicBlock, pool: &mut ConstantPoolBuilder) {
        for instruction in &block.instructions {
            self.eval(instruction, pool);
        }

        self.eval_finalizer(&block.finalizer, pool);
    }

    pub fn eval_finalizer(&mut self, finalizer: &BlockFinalizer, _pool: &mut ConstantPoolBuilder) {
        match finalizer {
            BlockFinalizer::BranchValueCmp {
                value_type,
                comparison: _,
                positive_block: _,
                negative_block: _,
            } => {
                let expected = value_type.to_verification_type();
                assert_eq!(self.stack.pop(), Some(expected));
                assert_eq!(self.stack.pop(), Some(expected));
            }
            BlockFinalizer::BranchInteger {
                comparison: _,
                positive_block: _,
                negative_block: _,
            } => {
                assert_eq!(self.stack.pop(), Some(VerificationTypeInfo::Integer));
            }
            BlockFinalizer::Goto(_) => {}
            BlockFinalizer::ReturnNull => {}
            BlockFinalizer::ReturnObject => {
                assert!(matches!(
                    self.stack.pop(),
                    Some(VerificationTypeInfo::Object { .. })
                ));
            }
            BlockFinalizer::ReturnInteger | BlockFinalizer::ReturnBoolean => {
                assert_eq!(self.stack.pop(), Some(VerificationTypeInfo::Integer));
            }
            BlockFinalizer::ReturnLong => {
                assert_eq!(self.stack.pop(), Some(VerificationTypeInfo::Long));
            }
        }
    }

    pub fn eval(&mut self, instruction: &Instruction, pool: &mut ConstantPoolBuilder) {
        match instruction {
            Instruction::New(class_name) => {
                self.stack.push(VerificationTypeInfo::Object {
                    constant_pool_index: pool.add_class(class_name.clone()),
                });
            }
            Instruction::GetStatic {
                field_descriptor: field_type,
                ..
            } => {
                let verification_type = field_type
                    .clone()
                    .into_primitive()
                    .into_verification_type(pool);
                self.stack.push(verification_type);
            }
            Instruction::GetField {
                class_name,
                field_descriptor,
                ..
            } => {
                assert_eq!(
                    self.stack.pop(),
                    Some(VerificationTypeInfo::Object {
                        constant_pool_index: pool.add_class(class_name.clone())
                    })
                );
                self.stack.push(
                    field_descriptor
                        .clone()
                        .into_primitive()
                        .into_verification_type(pool),
                );
            }
            Instruction::PutField {
                class_name,
                field_descriptor,
                ..
            } => {
                assert_eq!(
                    self.stack.pop(),
                    Some(
                        field_descriptor
                            .clone()
                            .into_primitive()
                            .into_verification_type(pool)
                    )
                );
                assert_eq!(
                    self.stack.pop(),
                    Some(VerificationTypeInfo::Object {
                        constant_pool_index: pool.add_class(class_name.clone())
                    })
                );
            }
            Instruction::InvokeVirtual {
                method_descriptor, ..
            }
            | Instruction::InvokeStatic {
                method_descriptor, ..
            }
            | Instruction::InvokeSpecial {
                method_descriptor, ..
            } => {
                for argument in method_descriptor.parameters.iter().rev() {
                    let verification_type = argument
                        .clone()
                        .into_primitive()
                        .into_verification_type(pool);
                    assert_eq!(self.stack.pop(), Some(verification_type));
                }
                if matches!(
                    instruction,
                    Instruction::InvokeVirtual { .. } | Instruction::InvokeSpecial { .. }
                ) {
                    self.stack
                        .pop()
                        .expect("Should supply objectref when calling a method");
                }

                if let ReturnDescriptor::Value(value_type) = &method_descriptor.return_type {
                    self.stack.push(
                        value_type
                            .clone()
                            .into_primitive()
                            .into_verification_type(pool),
                    )
                }
            }
            Instruction::LoadConstant { value } => {
                let verification_type = value.to_verification_type(pool);
                self.stack.push(verification_type);
            }
            Instruction::Add(p)
            | Instruction::Mul(p)
            | Instruction::Sub(p)
            | Instruction::Div(p)
            | Instruction::Rem(p) => {
                let expected = p.to_verification_type();
                assert_eq!(self.stack.pop(), Some(expected));
                assert_eq!(self.stack.pop(), Some(expected));
                self.stack.push(expected);
            }
            Instruction::Store(id) => {
                let verification_type = id.r#type.clone().into_verification_type(pool);
                assert_eq!(self.stack.pop(), Some(verification_type));
                // FIXME: Probably need to rework the whole locals allocation system :(
                let is_new_allocation = id.index
                    == self
                        .locals
                        .iter()
                        .map(|l| l.category().stack_size())
                        .sum::<u16>();
                if is_new_allocation {
                    self.locals.push(verification_type);
                }
            }
            Instruction::Load(id) => {
                self.stack
                    .push(id.r#type.clone().into_verification_type(pool));
            }
            Instruction::Transmute(to) => {
                assert!(matches!(
                    self.stack.pop(),
                    Some(VerificationTypeInfo::Object { .. })
                ));
                self.stack.push(VerificationTypeInfo::Object {
                    constant_pool_index: pool.add_class(to.clone()),
                });
            }
            Instruction::Nop => {}
            Instruction::Pop(category) => {
                let value = self.stack.pop();
                assert_eq!(value.map(|it| it.category()), Some(*category))
            }
            Instruction::NewArray(primitive) => {
                let len_val = self.stack.pop();
                assert_eq!(len_val, Some(VerificationTypeInfo::Integer));

                let element_type = primitive.clone().into_field_descriptor();
                let array_type = pool.add_array_class(element_type);
                self.stack.push(VerificationTypeInfo::Object {
                    constant_pool_index: array_type,
                });
            }
            Instruction::ArrayStore(primitive) => {
                let value = self.stack.pop();
                assert_eq!(value, Some(primitive.clone().into_verification_type(pool)));

                let index = self.stack.pop();
                assert_eq!(index, Some(VerificationTypeInfo::Integer));

                let element_type = primitive.clone().into_field_descriptor();
                let array_type = pool.add_array_class(element_type);
                let array = self.stack.pop();
                assert_eq!(
                    array,
                    Some(VerificationTypeInfo::Object {
                        constant_pool_index: array_type
                    })
                )
            }
            Instruction::ArrayLoad(primitive) => {
                let index = self.stack.pop();
                assert_eq!(index, Some(VerificationTypeInfo::Integer));

                let element_type = primitive.clone().into_field_descriptor();
                let array_type = pool.add_array_class(element_type);
                let array = self.stack.pop();
                assert_eq!(
                    array,
                    Some(VerificationTypeInfo::Object {
                        constant_pool_index: array_type
                    })
                );

                self.stack
                    .push(primitive.clone().into_verification_type(pool));
            }
            Instruction::ArrayLength => {
                let array = self.stack.pop();
                assert!(matches!(array, Some(VerificationTypeInfo::Object { .. })));
                self.stack.push(VerificationTypeInfo::Integer);
            }
            Instruction::Dup(type_category) => {
                let info = self.stack.last().unwrap();
                assert_eq!(info.category(), *type_category);
                self.stack.push(*info);
            }
            Instruction::IntToLong => {
                assert_eq!(self.stack.pop(), Some(VerificationTypeInfo::Integer));
                self.stack.push(VerificationTypeInfo::Long);
            }
            Instruction::LongToInt => {
                assert_eq!(self.stack.pop(), Some(VerificationTypeInfo::Long));
                self.stack.push(VerificationTypeInfo::Integer);
            }
        }
    }
}

impl From<Frame> for SymbolicEvaluator {
    fn from(Frame { locals, stack }: Frame) -> Self {
        Self { locals, stack }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Frame {
    pub locals: Vec<VerificationTypeInfo>,
    pub stack: Vec<VerificationTypeInfo>,
}

impl Frame {
    pub fn combine(self, other: &Frame) -> Frame {
        for (a, b) in self.locals.iter().zip(other.locals.iter()) {
            assert_eq!(a, b);
        }

        // FIXME: The locals handling does not seem entirely correct. TODO: Read up on it is supposed to work.
        let locals = if self.locals.len() < other.locals.len() {
            self.locals
        } else {
            other.locals.clone()
        };

        assert_eq!(self.stack, other.stack);
        Frame {
            locals,
            stack: self.stack,
        }
    }
}

impl From<&SymbolicEvaluator> for Frame {
    fn from(value: &SymbolicEvaluator) -> Self {
        Self {
            locals: value.locals.clone(),
            stack: value.stack.clone(),
        }
    }
}
