use crate::{
    auryn::{
        air::{
            data::{
                Air, AirBlock, AirBlockFinalizer, AirBlockId, AirConstant, AirExpression,
                AirExpressionKind, AirNode, AirNodeKind, AirValueId, Assignment, BinaryOperation,
                Intrinsic, IntrinsicCall,
            },
            types::Type,
        },
        tokenizer::BinaryOperatorToken,
    },
    java::{
        assembler::{
            Assembler, ConstantValue, FieldDescriptor, Instruction, MethodDescriptor, Primitive,
            VariableId,
        },
        class::{ClassData, Comparison, TypeCategory, VerificationTypeInfo},
        source_graph::{BasicBlockId, BlockFinalizer},
    },
    utils::fast_map::{FastMap, FastSet},
};
use indexmap::IndexSet;

pub fn query_class(class_name: String, air: &Air) -> ClassData {
    let mut generator = Generator::new();
    generator.generate_from_air(air);

    generator.finish(class_name)
}

struct Generator {
    assembler: Assembler,
    variable_map: FastMap<AirValueId, VariableId>,
    block_translation: FastMap<AirBlockId, BasicBlockId>,
    generated_blocks: FastSet<AirBlockId>,
    pending_blocks: IndexSet<AirBlockId>,
}

impl Generator {
    pub fn new() -> Self {
        let mut block_translation = FastMap::default();
        block_translation.insert(AirBlockId::ROOT, BasicBlockId(0));
        Self {
            assembler: Assembler::new(),
            block_translation,
            variable_map: FastMap::default(),
            generated_blocks: FastSet::default(),
            pending_blocks: IndexSet::default(),
        }
    }

    pub fn finish(self, class_name: String) -> ClassData {
        self.assembler.assemble(class_name)
    }

    pub fn generate_from_air(&mut self, air: &Air) {
        let root_block = air.root_block();
        self.generate_block(root_block, AirBlockId::ROOT);

        while let Some(id) = self.pending_blocks.pop() {
            self.generate_block(&air.blocks[&id], id);
        }
    }
}

impl Generator {
    fn translate_block_id(&mut self, air_block_id: AirBlockId) -> BasicBlockId {
        if !self.generated_blocks.contains(&air_block_id) {
            self.pending_blocks.insert(air_block_id);
        }
        *self
            .block_translation
            .entry(air_block_id)
            .or_insert_with(|| self.assembler.add_block())
    }

    fn _intrinsics_print_int(&mut self) {
        let result_id = self.assembler.alloc_variable(Primitive::Integer);
        self.assembler.add_all([
            Instruction::Store(result_id),
            Instruction::GetStatic {
                class_name: "java/lang/System".to_string(),
                name: "out".to_string(),
                field_type: FieldDescriptor::Object("java/io/PrintStream".to_string()),
            },
            Instruction::Load(result_id),
            Instruction::InvokeVirtual {
                class_name: "java/io/PrintStream".to_string(),
                name: "println".to_string(),
                // method_type: MethodDescriptor("(I)V".to_string()),
                method_type: MethodDescriptor {
                    arguments: vec![FieldDescriptor::Integer],
                    return_type: FieldDescriptor::Void,
                },
            },
        ]);
    }
}

impl Generator {
    fn generate_block(&mut self, block: &AirBlock, air_id: AirBlockId) {
        self.generated_blocks.insert(air_id);
        let block_id = self.translate_block_id(air_id);
        self.assembler.set_current_block_id(block_id);

        for node in &block.nodes {
            self.generate_node(node);
        }

        self.generate_finalizer(&block.finalizer);
    }

    fn generate_finalizer(&mut self, finalizer: &AirBlockFinalizer) {
        match finalizer {
            AirBlockFinalizer::Return => {
                self.assembler.current_block_mut().finalizer = BlockFinalizer::ReturnNull
            }
            AirBlockFinalizer::Goto(target) => {
                let block_id = self.translate_block_id(*target);
                self.assembler.current_block_mut().finalizer = BlockFinalizer::Goto(block_id);
            }
            AirBlockFinalizer::Branch {
                value,
                pos_block,
                neg_block,
            } => {
                let pos_block_id = self.translate_block_id(*pos_block);
                let neg_block_id = self.translate_block_id(*neg_block);
                let result = self.generate_expression(value);
                assert_eq!(result, Some(TypeCategory::Normal));
                self.assembler.current_block_mut().finalizer = BlockFinalizer::BranchInteger {
                    comparison: Comparison::NotEqual,
                    positive_block: pos_block_id,
                    negative_block: neg_block_id,
                };
            }
        }
    }

    fn generate_node(&mut self, node: &AirNode) {
        match &node.kind {
            AirNodeKind::Assignment(assignment) => self.generate_assignment(assignment),
            AirNodeKind::Expression(expression) => {
                let leftover = self.generate_expression(expression);
                if let Some(leftover) = leftover {
                    self.assembler.add(Instruction::Pop(leftover));
                }
            }
        }
    }

    fn generate_assignment(&mut self, assignment: &Assignment) {
        self.generate_expression(&assignment.expression);

        let variable_id = if self.variable_map.contains_key(&assignment.target) {
            self.variable_map[&assignment.target]
        } else {
            let variable_type = assignment.expression.r#type.computed().as_primitive();
            let variable_id = self.assembler.alloc_variable(variable_type);
            self.variable_map.insert(assignment.target, variable_id);
            variable_id
        };

        self.assembler.add(Instruction::Store(variable_id));
    }

    /// The return value indicates the stack usage
    fn generate_expression(&mut self, expression: &AirExpression) -> Option<TypeCategory> {
        match &expression.kind {
            AirExpressionKind::Constant(constant) => Some(self.generate_constant(constant)),
            AirExpressionKind::BinaryOperator(binary_operator) => {
                Some(self.generate_binary_operation(binary_operator))
            }
            AirExpressionKind::Variable(variable) => Some(self.generate_variable(variable)),
            AirExpressionKind::IntrinsicCall(intrinsic) => self.generate_intrinsic_call(intrinsic),
            AirExpressionKind::Error => unreachable!("Codegen was started with invalid air"),
        }
    }

    fn generate_constant(&mut self, constant: &AirConstant) -> TypeCategory {
        match constant {
            AirConstant::Number(number) => {
                let value = ConstantValue::Integer(*number);
                let category = value
                    .to_verification_type(&mut self.assembler.constant_pool)
                    .category();
                self.assembler.add(Instruction::LoadConstant { value });
                category
            }
        }
    }

    fn generate_binary_operation(&mut self, operation: &BinaryOperation) -> TypeCategory {
        let lhs_category = self.generate_expression(&operation.lhs);
        let rhs_category = self.generate_expression(&operation.rhs);
        assert_eq!(lhs_category, rhs_category);
        assert_eq!(lhs_category, Some(TypeCategory::Normal));

        match operation.operator {
            BinaryOperatorToken::Plus => {
                self.assembler.add(Instruction::IAdd);
            }
            BinaryOperatorToken::Times => {
                self.assembler.add(Instruction::IMul);
            }
            BinaryOperatorToken::Equal => self.generate_comparison(Comparison::Equal),
            BinaryOperatorToken::NotEqual => self.generate_comparison(Comparison::NotEqual),
            BinaryOperatorToken::Greater => self.generate_comparison(Comparison::Greater),
            BinaryOperatorToken::GreaterOrEqual => {
                self.generate_comparison(Comparison::GreaterOrEqual)
            }
            BinaryOperatorToken::Less => self.generate_comparison(Comparison::Less),
            BinaryOperatorToken::LessOrEqual => self.generate_comparison(Comparison::LessOrEqual),
        };

        VerificationTypeInfo::Integer.category()
    }

    fn generate_comparison(&mut self, comparison: Comparison) {
        let pos_block_id = self.assembler.add_block();
        let neg_block_id = self.assembler.add_block();
        let next_block_id = self.assembler.add_block();
        self.assembler.current_block_mut().finalizer = BlockFinalizer::BranchIntegerCmp {
            comparison,
            positive_block: pos_block_id,
            negative_block: neg_block_id,
        };

        self.assembler.set_current_block_id(pos_block_id);
        self.assembler.add(Instruction::LoadConstant {
            value: ConstantValue::Integer(1),
        });
        self.assembler.current_block_mut().finalizer = BlockFinalizer::Goto(next_block_id);

        self.assembler.set_current_block_id(neg_block_id);
        self.assembler.add(Instruction::LoadConstant {
            value: ConstantValue::Integer(0),
        });
        self.assembler.current_block_mut().finalizer = BlockFinalizer::Goto(next_block_id);

        self.assembler.set_current_block_id(next_block_id);
    }

    fn generate_variable(&mut self, variable: &AirValueId) -> TypeCategory {
        let variable_id = self.variable_map[variable];
        self.assembler.add(Instruction::Load(variable_id));
        variable_id.r#type.to_verification_type().category()
    }

    fn generate_intrinsic_call(&mut self, intrinsic: &IntrinsicCall) -> Option<TypeCategory> {
        match intrinsic.intrinsic {
            Intrinsic::Print => {
                self.assembler.add(Instruction::GetStatic {
                    class_name: "java/lang/System".to_string(),
                    name: "out".to_string(),
                    field_type: FieldDescriptor::Object("java/io/PrintStream".to_string()),
                });
                self.generate_expression(&intrinsic.arguments[0]);
                self.assembler.add(Instruction::InvokeVirtual {
                    class_name: "java/io/PrintStream".to_string(),
                    name: "println".to_string(),
                    // method_type: MethodDescriptor("(I)V".to_string()),
                    method_type: MethodDescriptor {
                        arguments: vec![FieldDescriptor::Integer],
                        return_type: FieldDescriptor::Void,
                    },
                });

                None
            }
        }
    }

    // fn generate_if_statement(&mut self, if_statement: &IfStatement) -> CodegenResult {
    //     self.generate_expression(if_statement.expression()?)?;
    //
    //     let pos_block = self.assembler.add_block();
    //     let next_block = self.assembler.add_block();
    //     self.assembler.current_block_mut().finalizer = BlockFinalizer::BranchInteger {
    //         comparison: Comparison::NotEqual,
    //         positive_block: pos_block,
    //         negative_block: next_block,
    //     };
    //
    //     self.assembler.set_current_block_id(pos_block);
    //     self.generate_block(if_statement.block()?)?;
    //     self.assembler.current_block_mut().finalizer = BlockFinalizer::Goto(next_block);
    //
    //     self.assembler.set_current_block_id(next_block);
    //
    //     Ok(())
    // }
    //
    // fn generate_loop(&mut self, loop_statement: &LoopStatement) -> CodegenResult {
    //     let loop_block = self.assembler.add_block();
    //     let next_block = self.assembler.add_block();
    //
    //     self.assembler.current_block_mut().finalizer = BlockFinalizer::Goto(loop_block);
    //     self.assembler.set_current_block_id(loop_block);
    //
    //     self.loops.push(LoopInfo {
    //         _continue_target: loop_block,
    //         break_target: next_block,
    //     });
    //     self.generate_block(loop_statement.block()?)?;
    //     self.loops.pop();
    //
    //     self.assembler.current_block_mut().finalizer = BlockFinalizer::Goto(loop_block);
    //
    //     // Currently everything that goes here is dead code
    //     self.assembler.set_current_block_id(next_block);
    //
    //     Ok(())
    // }
    //
    // fn generate_break(&mut self, _break_statement: &BreakStatement) -> CodegenResult {
    //     let Some(loop_info) = self.loops.last() else {
    //         panic!("Not in a loop D:");
    //     };
    //
    //     // This block is for potential code that comes after the break statement
    //     let dead_code_block = self.assembler.add_block();
    //
    //     self.assembler.current_block_mut().finalizer = BlockFinalizer::Goto(loop_info.break_target);
    //     self.assembler.set_current_block_id(dead_code_block);
    //
    //     Ok(())
    // }
    //
    // fn generate_update(&mut self, _update: &VariableUpdate) -> CodegenResult {
    //     // self.generate_expression(update.expression()?)?;
    //     //
    //     // let variable_id = self.variable_map[&update.ident];
    //     // self.assembler.add(Instruction::IStore(variable_id));
    //
    //     Ok(())
    // }
}

impl Type {
    fn as_primitive(&self) -> Primitive {
        match self {
            Type::Number => Primitive::Integer,
            Type::Null => todo!("Decide how to represent the null type"),
            Type::Error => unreachable!("Type error should not be present in air"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        auryn::{
            air::query_air, ast::query_ast2, codegen_java::query_class, file_id::FileId,
            parser::Parser,
        },
        java::class::ClassData,
    };

    fn generate_class(input: &str) -> ClassData {
        let result = Parser::new(FileId::MAIN_FILE, input).parse();
        let ast = query_ast2(result.syntax_tree.as_ref().unwrap());
        let air = query_air(ast.unwrap());
        assert!(air.diagnostics.is_empty());

        query_class("Helloworld".to_string(), &air.air)
    }

    #[test]
    fn test_simple() {
        insta::assert_debug_snapshot!(generate_class("1 + 2 * 3"));
    }

    #[test]
    fn test_print() {
        insta::assert_debug_snapshot!(generate_class("print(2 * 3)"));
    }

    #[test]
    fn test_assignment() {
        insta::assert_debug_snapshot!(generate_class("let a = 1"));
        insta::assert_debug_snapshot!(generate_class("let a = 1\nprint(a)"));
        insta::assert_debug_snapshot!(generate_class("let a = 7\na = a * a\nprint(a)"));
    }

    #[test]
    fn test_weird() {
        insta::assert_debug_snapshot!(generate_class("loop { loop {} }"));
    }

    #[test]
    fn test_stack_map_table_generation() {
        insta::assert_debug_snapshot!(generate_class(
            "loop {\nif 1 {\nprint(42)\n}\nprint(100)\n}"
        ));
    }

    #[test]
    #[should_panic]
    fn test_reject_invalid_variable() {
        insta::assert_debug_snapshot!(generate_class("let a = a"));
    }
}
