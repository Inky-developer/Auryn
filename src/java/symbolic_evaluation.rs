use std::fmt::Debug;

use crate::java::{
    class::VerificationTypeInfo,
    constant_pool_builder::ConstantPoolBuilder,
    function_assembler::{Instruction, ReturnDescriptor},
    source_graph::{BasicBlock, BlockFinalizer},
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
            BlockFinalizer::BranchIntegerCmp {
                comparison: _,
                positive_block: _,
                negative_block: _,
            } => {
                assert_eq!(self.stack.pop(), Some(VerificationTypeInfo::Integer));
                assert_eq!(self.stack.pop(), Some(VerificationTypeInfo::Integer));
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
            BlockFinalizer::ReturnInteger => {
                assert_eq!(self.stack.pop(), Some(VerificationTypeInfo::Integer));
            }
        }
    }

    pub fn eval(&mut self, instruction: &Instruction, pool: &mut ConstantPoolBuilder) {
        match instruction {
            Instruction::GetStatic {
                field_descriptor: field_type,
                ..
            } => {
                let verification_type = field_type.to_verification_type(pool);
                self.stack.push(verification_type);
            }
            Instruction::InvokeVirtual {
                method_descriptor, ..
            }
            | Instruction::InvokeStatic {
                method_descriptor, ..
            } => {
                for argument in method_descriptor.parameters.iter().rev() {
                    let verification_type = argument.to_verification_type(pool);
                    assert_eq!(self.stack.pop(), Some(verification_type));
                }
                if matches!(instruction, Instruction::InvokeVirtual { .. }) {
                    self.stack
                        .pop()
                        .expect("Should supply objectref when calling a method");
                }

                if let ReturnDescriptor::Value(value_type) = &method_descriptor.return_type {
                    self.stack.push(value_type.to_verification_type(pool))
                }
            }
            Instruction::LoadConstant { value } => {
                let verification_type = value.to_verification_type(pool);
                self.stack.push(verification_type);
            }
            Instruction::IAdd | Instruction::IMul | Instruction::ISub => {
                assert_eq!(self.stack.pop(), Some(VerificationTypeInfo::Integer));
                assert_eq!(self.stack.pop(), Some(VerificationTypeInfo::Integer));
                self.stack.push(VerificationTypeInfo::Integer);
            }
            Instruction::Store(id) => {
                assert_eq!(self.stack.pop(), Some(id.r#type.to_verification_type()));
                // FIXME: Probably need to rework the whole locals allocation system :(
                if self.locals.len() == id.index.into() {
                    self.locals.push(id.r#type.to_verification_type());
                }
            }
            Instruction::Load(id) => {
                self.stack.push(id.r#type.to_verification_type());
            }
            Instruction::Nop => {}
            Instruction::Pop(category) => {
                let value = self.stack.pop();
                assert_eq!(value.map(|it| it.category()), Some(*category))
            }
            Instruction::NewArray(primitive) => {
                let len_val = self.stack.pop();
                assert_eq!(len_val, Some(VerificationTypeInfo::Integer));

                let element_type = primitive.to_field_descriptor(pool);
                let array_type = pool.add_array_class(element_type);
                self.stack.push(VerificationTypeInfo::Object {
                    constant_pool_index: array_type,
                });
            }
            Instruction::ArrayStore(primitive) => {
                let value = self.stack.pop();
                assert_eq!(value, Some(primitive.to_verification_type()));

                let index = self.stack.pop();
                assert_eq!(index, Some(VerificationTypeInfo::Integer));

                let element_type = primitive.to_field_descriptor(pool);
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

                let element_type = primitive.to_field_descriptor(pool);
                let array_type = pool.add_array_class(element_type);
                let array = self.stack.pop();
                assert_eq!(
                    array,
                    Some(VerificationTypeInfo::Object {
                        constant_pool_index: array_type
                    })
                );

                self.stack.push(primitive.to_verification_type());
            }
            Instruction::Dup(type_category) => {
                let info = self.stack.last().unwrap();
                assert_eq!(info.category(), *type_category);
                self.stack.push(*info);
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
