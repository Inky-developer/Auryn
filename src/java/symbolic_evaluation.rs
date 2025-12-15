use std::fmt::Debug;

use crate::java::{
    assembler::Instruction, class::VerificationTypeInfo, constant_pool_builder::ConstantPoolBuilder,
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

    pub fn eval(&mut self, instruction: &Instruction, pool: &mut ConstantPoolBuilder) {
        match instruction {
            // https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.getstatic
            Instruction::GetStatic { field_type, .. } => {
                let verification_type = field_type.to_verification_type(pool);
                self.stack.push(verification_type);
            }
            // https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.invokevirtual
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
            Instruction::IStore(_) => {
                assert_eq!(self.stack.pop(), Some(VerificationTypeInfo::Integer));
            }
            Instruction::ILoad(_) => {
                self.stack.push(VerificationTypeInfo::Integer);
            }
            Instruction::Goto(_) => {}
            Instruction::IfIcmp { .. } => {
                assert_eq!(self.stack.pop(), Some(VerificationTypeInfo::Integer));
                assert_eq!(self.stack.pop(), Some(VerificationTypeInfo::Integer));
            }
            Instruction::Nop => {}
        }
    }
}
