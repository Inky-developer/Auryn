use std::fmt::Debug;

use crate::java::{
    assembler::Instruction,
    class::VerificationTypeInfo,
    constant_pool_builder::ConstantPoolBuilder,
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
        }
    }

    pub fn eval(&mut self, instruction: &Instruction, pool: &mut ConstantPoolBuilder) {
        match instruction {
            Instruction::GetStatic { field_type, .. } => {
                let verification_type = field_type.to_verification_type(pool);
                self.stack.push(verification_type);
            }
            Instruction::InvokeVirtual { method_type, .. } => {
                for argument in method_type.arguments.iter().rev() {
                    let verification_type = argument.to_verification_type(pool);
                    assert_eq!(self.stack.pop(), Some(verification_type));
                }
                self.stack
                    .pop()
                    .expect("Should supply objectref when calling a method");
            }
            Instruction::LoadConstant { value } => {
                let verification_type = value.to_verification_type(pool);
                self.stack.push(verification_type);
            }
            Instruction::ReturnNull => {}
            Instruction::IAdd | Instruction::IMul => {
                assert_eq!(self.stack.pop(), Some(VerificationTypeInfo::Integer));
                assert_eq!(self.stack.pop(), Some(VerificationTypeInfo::Integer));
                self.stack.push(VerificationTypeInfo::Integer);
            }
            Instruction::IStore(id) => {
                assert_eq!(self.stack.pop(), Some(VerificationTypeInfo::Integer));
                // FIXME: Probably need to rework the whole locals allocation system :(
                if self.locals.len() == id.0.into() {
                    self.locals.push(VerificationTypeInfo::Integer);
                }
            }
            Instruction::ILoad(_) => {
                self.stack.push(VerificationTypeInfo::Integer);
            }
            Instruction::Nop => {}
            Instruction::Pop(category) => {
                let value = self.stack.pop();
                assert_eq!(value.map(|it| it.category()), Some(*category))
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
