use std::{cmp::Ordering, fmt::Debug};

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
    /// Evaluates the block and returns the highest recorded stack usage
    pub fn eval_block(&mut self, block: &BasicBlock, pool: &mut ConstantPoolBuilder) -> u16 {
        let mut highest_stack_usage = self.stack_usage();
        for instruction in &block.instructions {
            self.eval(instruction, pool);
            highest_stack_usage = u16::max(highest_stack_usage, self.stack_usage());
        }

        self.eval_finalizer(&block.finalizer, pool);
        u16::max(highest_stack_usage, self.stack_usage())
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

    pub fn stack_usage(&self) -> u16 {
        self.stack.iter().map(|it| it.category().stack_size()).sum()
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
                for _ in method_descriptor.parameters.iter().rev() {
                    // Can't compare with the argument due to java subtyping rules
                    // E.g. String can be passed to a method that expects Object
                    assert!(self.stack.pop().is_some());
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
                /// Converts a variable id into an index into the locals array
                fn id_to_index(locals: &[VerificationTypeInfo], id: u16) -> usize {
                    let mut byte_offset: u16 = 0;
                    for (index, local) in locals.iter().enumerate() {
                        if byte_offset == id {
                            return index;
                        }
                        byte_offset += local.category().stack_size();
                    }
                    panic!("Invalid id {id} for {locals:?}")
                }

                let verification_type = id.r#type.clone().into_verification_type(pool);
                assert_eq!(self.stack.pop(), Some(verification_type));
                let current_slot_count: u16 =
                    self.locals.iter().map(|l| l.category().stack_size()).sum();
                let is_new_allocation = id.index >= current_slot_count;
                if is_new_allocation {
                    // Since locals is an array containing information about all variables,
                    // we need to fill in the predecessors of the current variable first
                    for _ in current_slot_count..id.index {
                        self.locals.push(VerificationTypeInfo::Top);
                    }
                    self.locals.push(verification_type);
                } else {
                    let index = id_to_index(&self.locals, id.index);
                    self.locals[index] = verification_type;
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
        let locals = Self::combine_locals(&self.locals, &other.locals);
        // No need for combing the stacks, because the stack must always be in the same state,
        // independent of the previous block.
        let stack = self.stack;
        assert_eq!(&stack, &other.stack);
        Frame { locals, stack }
    }

    fn combine_locals<'a>(
        lhs: &'a [VerificationTypeInfo],
        rhs: &'a [VerificationTypeInfo],
    ) -> Vec<VerificationTypeInfo> {
        let mut result = Vec::new();
        let mut lhs_index = 0_usize;
        let mut rhs_index = 0_usize;
        loop {
            let lhs = lhs.get(lhs_index);
            let rhs = rhs.get(rhs_index);
            let (Some(lhs), Some(rhs)) = (lhs, rhs) else {
                break;
            };

            if lhs == rhs {
                result.push(*lhs);
            } else {
                // For two different types, use n instances of the top type to fill the array.
                // n is the size of the biggest type found.
                let n = u16::max(lhs.category().stack_size(), rhs.category().stack_size());
                for _ in 0..n {
                    result.push(VerificationTypeInfo::Top);
                }
                // Then update the index of the smaller type to match the index of the bigger type.
                // For example, if lhs has size 1 and rhs has size 2, then lhs should be logically [Top, Top].
                // So the index needs to be increased by 1 to match rhs again.
                match lhs
                    .category()
                    .stack_size()
                    .cmp(&rhs.category().stack_size())
                {
                    Ordering::Equal => {}
                    Ordering::Greater => {
                        rhs_index +=
                            (lhs.category().stack_size() - rhs.category().stack_size()) as usize;
                    }
                    Ordering::Less => {
                        lhs_index +=
                            (rhs.category().stack_size() - lhs.category().stack_size()) as usize;
                    }
                }
            }

            // Finally advance to the next index
            lhs_index += 1;
            rhs_index += 1;
        }

        // Then, extend the result by Top types until it matches the size of the larger locals array
        let lhs_size: u16 = lhs.iter().map(|it| it.category().stack_size()).sum();
        let rhs_size: u16 = rhs.iter().map(|it| it.category().stack_size()).sum();
        let diff = lhs_size.abs_diff(rhs_size);
        for _ in 0..diff {
            result.push(VerificationTypeInfo::Top);
        }

        result
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

#[cfg(test)]
mod tests {
    use crate::java::{class::VerificationTypeInfo, symbolic_evaluation::Frame};

    #[test]
    fn test_combine_locals() {
        use VerificationTypeInfo::*;

        assert_eq!(Frame::combine_locals(&[], &[]), vec![]);
        assert_eq!(Frame::combine_locals(&[Top], &[]), vec![Top]);
        assert_eq!(Frame::combine_locals(&[], &[Top]), vec![Top]);
        assert_eq!(Frame::combine_locals(&[Top], &[Top]), vec![Top]);

        assert_eq!(
            Frame::combine_locals(&[Double], &[Top, Top]),
            vec![Top, Top]
        );
        assert_eq!(
            Frame::combine_locals(&[Double, Integer], &[Top, Top, Integer]),
            vec![Top, Top, Integer]
        );
    }

    #[test]
    fn store_updates_top_local() {
        use crate::{
            auryn::codegen_java::representation::Representation,
            java::{
                constant_pool_builder::ConstantPoolBuilder,
                function_assembler::{Instruction, VariableId},
                symbolic_evaluation::SymbolicEvaluator,
            },
        };
        use VerificationTypeInfo::*;

        // Simulate a block where local 1 is Top (only initialized on one branch),
        // and we store an Integer into it.
        let mut pool = ConstantPoolBuilder::default();
        let mut evaluator = SymbolicEvaluator {
            locals: vec![Integer, Top],
            stack: vec![Integer],
        };

        evaluator.eval(
            &Instruction::Store(VariableId {
                index: 1,
                r#type: Representation::Integer,
            }),
            &mut pool,
        );

        assert_eq!(evaluator.locals, vec![Integer, Integer]);
    }
}
