use crate::{
    auryn::{
        air::{
            data::{
                AirBlock, AirBlockFinalizer, AirBlockId, AirConstant, AirExpression,
                AirExpressionKind, AirFunction, AirFunctionId, AirNode, AirNodeKind, AirValueId,
                Assignment, BinaryOperation, Call, Intrinsic, IntrinsicCall,
            },
            types::Type,
        },
        codegen_java::{class_generator::GeneratedMethodData, representation::get_representation},
        tokenizer::BinaryOperatorToken,
    },
    java::{
        class::{self, Comparison, VerificationTypeInfo},
        constant_pool_builder::ConstantPoolBuilder,
        function_assembler::{
            ConstantValue, FieldDescriptor, FunctionAssembler, Instruction, MethodDescriptor,
            ReturnDescriptor, VariableId,
        },
        source_graph::{BasicBlockId, BlockFinalizer},
    },
    utils::{
        fast_map::{FastMap, FastSet},
        small_string::SmallString,
    },
};
use indexmap::IndexSet;

pub fn generate_function(
    pool: &mut ConstantPoolBuilder,
    function_infos: &FastMap<AirFunctionId, GeneratedMethodData>,
    class_name: &SmallString,
    function: &AirFunction,
    mangled_name: SmallString,
    method_descriptor: MethodDescriptor,
) -> class::Method {
    let mut generator = FunctionGenerator::new(
        mangled_name,
        function,
        method_descriptor,
        class_name,
        function_infos,
        pool,
    );
    generator.generate_from_function(function);

    generator.finish()
}

struct FunctionGenerator<'a> {
    assembler: FunctionAssembler<'a>,
    variable_map: FastMap<AirValueId, VariableId>,
    block_translation: FastMap<AirBlockId, BasicBlockId>,
    generated_blocks: FastSet<AirBlockId>,
    pending_blocks: IndexSet<AirBlockId>,
    function_infos: &'a FastMap<AirFunctionId, GeneratedMethodData>,
    class_name: &'a SmallString,
}

impl<'a> FunctionGenerator<'a> {
    pub fn new(
        name: SmallString,
        function: &AirFunction,
        method_descriptor: MethodDescriptor,
        class_name: &'a SmallString,
        function_infos: &'a FastMap<AirFunctionId, GeneratedMethodData>,
        pool: &'a mut ConstantPoolBuilder,
    ) -> Self {
        let Type::Function(function_type) = function.r#type.computed() else {
            panic!("Invalid function type");
        };
        let mut block_translation = FastMap::default();
        block_translation.insert(AirBlockId::ROOT, BasicBlockId(0));

        let mut variable_map = FastMap::default();

        let mut variable_index = 0;
        for (parameter, argument_id) in function_type.parameters.iter().zip(function.argument_ids())
        {
            if let Some(primitive) = get_representation(pool, parameter) {
                let variable_id = VariableId {
                    index: variable_index,
                    r#type: primitive,
                };
                variable_index += 1;
                variable_map.insert(argument_id, variable_id);
            }
        }

        let assembler = FunctionAssembler::new(name, method_descriptor, pool);

        Self {
            class_name,
            function_infos,
            assembler,
            block_translation,
            variable_map,
            generated_blocks: FastSet::default(),
            pending_blocks: IndexSet::default(),
        }
    }

    pub fn finish(self) -> class::Method {
        self.assembler.assemble()
    }

    pub fn generate_from_function(&mut self, function: &AirFunction) {
        let root_block = &function.blocks[&AirBlockId::ROOT];
        self.generate_block(root_block, AirBlockId::ROOT);

        while let Some(id) = self.pending_blocks.pop() {
            self.generate_block(&function.blocks[&id], id);
        }
    }
}

impl FunctionGenerator<'_> {
    fn translate_block_id(&mut self, air_block_id: AirBlockId) -> BasicBlockId {
        if !self.generated_blocks.contains(&air_block_id) {
            self.pending_blocks.insert(air_block_id);
        }
        *self
            .block_translation
            .entry(air_block_id)
            .or_insert_with(|| self.assembler.add_block())
    }
}

impl FunctionGenerator<'_> {
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
            AirBlockFinalizer::Return(r#return) => {
                let result = r#return
                    .expression()
                    .and_then(|expr| self.generate_expression(expr));
                let finalizer = match result {
                    None => BlockFinalizer::ReturnNull,
                    Some(r#type) => match r#type {
                        VerificationTypeInfo::Integer => BlockFinalizer::ReturnInteger,
                        VerificationTypeInfo::Object { .. } => BlockFinalizer::ReturnObject,
                        other => panic!("Cannot return {other:?}"),
                    },
                };
                self.assembler.current_block_mut().finalizer = finalizer
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
                assert_eq!(result, Some(VerificationTypeInfo::Integer));
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
                    self.assembler.add(Instruction::Pop(leftover.category()));
                }
            }
        }
    }

    fn generate_assignment(&mut self, assignment: &Assignment) {
        self.generate_expression(&assignment.expression);

        let variable_id = if self.variable_map.contains_key(&assignment.target) {
            Some(self.variable_map[&assignment.target])
        } else {
            let air_type = assignment.expression.r#type.computed();
            if let Some(primitive) = get_representation(self.assembler.constant_pool, air_type) {
                let variable_id = self.assembler.alloc_variable(primitive);
                self.variable_map.insert(assignment.target, variable_id);
                Some(variable_id)
            } else {
                None
            }
        };

        if let Some(variable_id) = variable_id {
            self.assembler.add(Instruction::Store(variable_id));
        }
    }

    /// The return value indicates the stack usage
    fn generate_expression(&mut self, expression: &AirExpression) -> Option<VerificationTypeInfo> {
        match &expression.kind {
            AirExpressionKind::Constant(constant) => Some(self.generate_constant(constant)),
            AirExpressionKind::BinaryOperator(binary_operator) => {
                Some(self.generate_binary_operation(binary_operator))
            }
            AirExpressionKind::Variable(variable) => Some(self.generate_variable(variable)),
            AirExpressionKind::Call(call) => self.generate_call(call),
            AirExpressionKind::IntrinsicCall(intrinsic) => self.generate_intrinsic_call(intrinsic),
            AirExpressionKind::Error => unreachable!("Codegen was started with invalid air"),
        }
    }

    fn generate_constant(&mut self, constant: &AirConstant) -> VerificationTypeInfo {
        match constant {
            AirConstant::Number(number) => {
                let value = ConstantValue::Integer(*number);
                let r#type = value.to_verification_type(self.assembler.constant_pool);
                self.assembler.add(Instruction::LoadConstant { value });
                r#type
            }
            AirConstant::String(text) => {
                let value = ConstantValue::String(text.clone());
                let r#type = value.to_verification_type(self.assembler.constant_pool);
                self.assembler.add(Instruction::LoadConstant { value });
                r#type
            }
        }
    }

    fn generate_binary_operation(&mut self, operation: &BinaryOperation) -> VerificationTypeInfo {
        let lhs_type = self.generate_expression(&operation.lhs);
        let rhs_type = self.generate_expression(&operation.rhs);
        assert_eq!(lhs_type, rhs_type);
        assert_eq!(lhs_type, Some(VerificationTypeInfo::Integer));

        match operation.operator {
            BinaryOperatorToken::Plus => {
                self.assembler.add(Instruction::IAdd);
            }
            BinaryOperatorToken::Minus => {
                self.assembler.add(Instruction::ISub);
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

        VerificationTypeInfo::Integer
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

    fn generate_variable(&mut self, variable: &AirValueId) -> VerificationTypeInfo {
        let variable_id = self.variable_map[variable];
        self.assembler.add(Instruction::Load(variable_id));
        variable_id.r#type.to_verification_type()
    }

    fn generate_call(&mut self, call: &Call) -> Option<VerificationTypeInfo> {
        for argument in &call.arguments {
            self.generate_expression(argument);
        }

        let GeneratedMethodData {
            generated_name,
            method_descriptor,
        } = &self.function_infos[&call.function];
        self.assembler.add(Instruction::InvokeStatic {
            class_name: self.class_name.clone(),
            name: generated_name.clone(),
            method_descriptor: method_descriptor.clone(),
        });
        method_descriptor
            .return_type
            .to_verification_type(self.assembler.constant_pool)
    }

    fn generate_intrinsic_call(
        &mut self,
        intrinsic: &IntrinsicCall,
    ) -> Option<VerificationTypeInfo> {
        match intrinsic.intrinsic {
            Intrinsic::Print => {
                self.assembler.add(Instruction::GetStatic {
                    class_name: "java/lang/System".into(),
                    name: "out".into(),
                    field_descriptor: FieldDescriptor::print_stream(),
                });
                self.generate_expression(&intrinsic.arguments[0]);
                let field_descriptor = match intrinsic.arguments[0].r#type.computed() {
                    Type::Number => FieldDescriptor::Integer,
                    Type::String => FieldDescriptor::string(),
                    ty => {
                        self.assembler.add(Instruction::LoadConstant {
                            value: ConstantValue::String(ty.to_string().into()),
                        });
                        FieldDescriptor::string()
                    }
                };
                self.assembler.add(Instruction::InvokeVirtual {
                    class_name: "java/io/PrintStream".into(),
                    name: "println".into(),
                    // method_type: MethodDescriptor("(I)V".to_string()),
                    method_descriptor: MethodDescriptor {
                        parameters: vec![field_descriptor],
                        return_type: ReturnDescriptor::Void,
                    },
                });

                None
            }
        }
    }
}
