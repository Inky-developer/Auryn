use crate::{
    auryn::{
        air::data::{
            self, Air, AirBlock, AirBlockFinalizer, AirBlockId, AirConstant, AirExpression,
            AirExpressionKind, AirFunction, AirFunctionId, AirNode, AirNodeKind, AirType,
            AirValueId, Call, IntrinsicCall,
        },
        ast::ast_node::{
            self, Assignment, BinaryOperation, Block, BreakStatement, Expression, FunctionCall,
            FunctionDefinition, Ident, IfStatement, LoopStatement, NumberLiteral, Parenthesis,
            Root, Statement, StringLiteral, Value, VariableUpdate,
        },
        diagnostic::{Diagnostic, DiagnosticError, DiagnosticKind},
        syntax_id::SyntaxId,
        syntax_tree::SyntaxToken,
    },
    utils::{fast_map::FastMap, small_string::SmallString},
};

#[derive(Debug)]
pub struct AirOutput {
    pub air: Air,
    pub diagnostics: Vec<Diagnostic>,
}

pub fn transform_ast(ast: Root) -> AirOutput {
    let mut transformer = AstTransformer::default();
    transformer.transform_root(ast);
    let functions = transformer.functions;
    AirOutput {
        air: Air { functions },
        diagnostics: transformer.diagnostics,
    }
}

#[derive(Debug, Default)]
struct AstTransformer {
    functions: FastMap<AirFunctionId, AirFunction>,
    namespace: FastMap<SmallString, AirFunctionId>,
    diagnostics: Vec<Diagnostic>,
}

impl AstTransformer {
    pub fn transform_root(&mut self, root: Root) {
        let Ok(file) = root.file() else {
            return;
        };

        for function in file.functions() {
            self.register_function(function);
        }

        for function in file.functions() {
            self.transform_function(function);
        }
    }

    fn register_function(&mut self, function_definition: FunctionDefinition) {
        let Ok(ident) = function_definition.ident() else {
            return;
        };

        self.namespace
            .insert(ident.text.clone(), AirFunctionId(function_definition.id()));
    }

    fn transform_function(&mut self, function_definition: FunctionDefinition) {
        let Ok(ident) = function_definition.ident() else {
            return;
        };
        let Ok(parameters) = function_definition.parameter_list() else {
            return;
        };
        let Ok(block) = function_definition.block() else {
            return;
        };

        let ident = ident.text.clone();
        let declared_parameters = parameters
            .parameters()
            .filter_map(|param| {
                param.r#type().ok().and_then(|ty| match ty {
                    ast_node::Type::Ident(ident) => {
                        ident.ident().ok().map(|ident| ident.text.clone())
                    }
                })
            })
            .collect();

        let mut function_transformer =
            FunctionTransformer::new(&mut self.diagnostics, &self.namespace);
        function_transformer.transform_function_body(block);

        let function_id = self.namespace[&ident];
        let function = AirFunction {
            r#type: AirType::Inferred,
            declared_parameters,
            ident,
            blocks: function_transformer.finished_blocks,
        };

        self.functions.insert(function_id, function);
    }
}

/// Represents an unfinished [`AirBlock`]
#[derive(Debug, Default)]
struct BlockBuilder {
    nodes: Vec<AirNode>,
}

#[derive(Debug)]
pub struct LoopInfo {
    _continue_target: AirBlockId,
    break_target: AirBlockId,
}

#[derive(Debug)]
struct FunctionTransformer<'a> {
    block_builders: FastMap<AirBlockId, BlockBuilder>,
    current_builder: AirBlockId,
    finished_blocks: FastMap<AirBlockId, AirBlock>,
    next_value_id: usize,
    variables: FastMap<SmallString, AirValueId>,
    diagnostics: &'a mut Vec<Diagnostic>,
    namespace: &'a FastMap<SmallString, AirFunctionId>,
    loops: Vec<LoopInfo>,
}

impl<'a> FunctionTransformer<'a> {
    fn new(
        diagnostics: &'a mut Vec<Diagnostic>,
        namespace: &'a FastMap<SmallString, AirFunctionId>,
    ) -> Self {
        Self {
            block_builders: FastMap::default(),
            current_builder: AirBlockId(0),
            finished_blocks: FastMap::default(),
            next_value_id: 1,
            variables: FastMap::default(),
            diagnostics,
            namespace,
            loops: Vec::new(),
        }
    }

    fn add_block(&mut self) -> AirBlockId {
        let block_id = AirBlockId(self.finished_blocks.len() + self.block_builders.len());
        self.block_builders
            .insert(block_id, BlockBuilder::default());
        block_id
    }

    fn finish_block(&mut self, next_block_id: AirBlockId, finalizer: AirBlockFinalizer) {
        let id = self.current_builder;
        self.current_builder = next_block_id;
        let block = self
            .block_builders
            .remove(&id)
            .expect("Should not try to remove block that is not being built");
        self.finished_blocks.insert(
            id,
            AirBlock {
                nodes: block.nodes,
                finalizer,
            },
        );
    }

    fn add_node(&mut self, node: AirNode) {
        self.block_builders
            .get_mut(&self.current_builder)
            .unwrap()
            .nodes
            .push(node);
    }

    fn add_error(&mut self, id: SyntaxId, error: DiagnosticError) {
        self.diagnostics
            .push(Diagnostic::new(id, DiagnosticKind::Error(error)))
    }

    fn create_variable(&mut self, token: &SyntaxToken, value_id: AirValueId) {
        if self.variables.contains_key(&token.text) {
            self.add_error(
                token.id,
                DiagnosticError::RedefinedVariable {
                    ident: token.text.clone(),
                },
            )
        }
        self.variables.insert(token.text.clone(), value_id);
    }

    fn create_value(&mut self) -> AirValueId {
        let id = AirValueId(self.next_value_id);
        self.next_value_id += 1;
        id
    }
}

impl FunctionTransformer<'_> {
    fn transform_function_body(&mut self, block: Block) {
        let block_id = self.add_block();
        self.transform_block(block);
        self.finish_block(block_id, AirBlockFinalizer::Return);
        assert!(self.block_builders.is_empty());
    }

    fn transform_block(&mut self, block: Block) {
        for statement in block.statements() {
            self.transform_statement(statement);
        }
    }

    fn transform_statement(&mut self, statement: Statement) {
        match statement {
            Statement::Assignment(assignment) => self.transform_assignment(assignment),
            Statement::IfStatement(if_statement) => self.transform_if_statement(if_statement),
            Statement::LoopStatement(loop_statement) => {
                self.transform_loop_statement(loop_statement)
            }
            Statement::BreakStatement(break_statement) => {
                self.transform_break_statement(break_statement)
            }
            Statement::VariableUpdate(variable_update) => {
                self.transform_variable_update(variable_update)
            }
            Statement::Expression(expression) => {
                let expression = self.transform_expression(expression);
                self.add_node(AirNode {
                    id: expression.id,
                    kind: AirNodeKind::Expression(Box::new(expression)),
                });
            }
        };
    }

    fn transform_assignment(&mut self, assignment: Assignment) {
        let Ok(expression) = assignment.expression() else {
            return;
        };

        let Ok(ident) = assignment.ident() else {
            return;
        };

        let expression = self.transform_expression(expression);
        let id = self.create_value();
        self.create_variable(ident, id);
        self.add_node(AirNode {
            id: assignment.id(),
            kind: AirNodeKind::Assignment(data::Assignment {
                target: id,
                expression: Box::new(expression),
            }),
        });
    }

    fn transform_if_statement(&mut self, if_statement: IfStatement) {
        let Ok(expression) = if_statement.expression() else {
            return;
        };
        let expression = self.transform_expression(expression);

        let Ok(block) = if_statement.block() else {
            return;
        };

        let pos_block_id = self.add_block();
        let next_block_id = self.add_block();

        self.finish_block(
            pos_block_id,
            AirBlockFinalizer::Branch {
                value: Box::new(expression),
                pos_block: pos_block_id,
                neg_block: next_block_id,
            },
        );
        self.transform_block(block);
        self.finish_block(next_block_id, AirBlockFinalizer::Goto(next_block_id));
    }

    fn transform_loop_statement(&mut self, loop_statement: LoopStatement) {
        let Ok(block) = loop_statement.block() else {
            return;
        };

        let loop_body = self.add_block();
        let next_block_id = self.add_block();

        self.finish_block(loop_body, AirBlockFinalizer::Goto(loop_body));
        self.loops.push(LoopInfo {
            break_target: next_block_id,
            _continue_target: loop_body,
        });
        self.transform_block(block);
        self.loops.pop().expect("Should have something to pop");
        self.finish_block(next_block_id, AirBlockFinalizer::Goto(loop_body));
    }

    fn transform_break_statement(&mut self, r#break: BreakStatement) {
        let Some(loop_info) = self.loops.last() else {
            self.add_error(r#break.id(), DiagnosticError::BreakOutsideLoop);
            return;
        };
        let break_target = loop_info.break_target;

        let dead_code_block = self.add_block();
        self.finish_block(dead_code_block, AirBlockFinalizer::Goto(break_target));
    }

    fn transform_variable_update(&mut self, variable_update: VariableUpdate) {
        let Ok(expression) = variable_update.expression() else {
            return;
        };
        let expression = self.transform_expression(expression);

        let Ok(ident) = variable_update.ident() else {
            return;
        };
        let Some(&variable_id) = self.variables.get(&ident.text) else {
            self.add_error(
                ident.id,
                DiagnosticError::UndefinedVariable {
                    ident: ident.text.clone(),
                },
            );
            return;
        };

        self.add_node(AirNode {
            id: variable_update.id(),
            kind: AirNodeKind::Assignment(data::Assignment {
                target: variable_id,
                expression: Box::new(expression),
            }),
        });
    }

    fn transform_expression(&mut self, expression: Expression) -> AirExpression {
        match expression {
            Expression::BinaryOperation(binary_operation) => {
                self.transform_binary_operation(binary_operation)
            }
            Expression::Value(value) => self.transform_value(value),
        }
    }

    fn transform_binary_operation(&mut self, binary_operation: BinaryOperation) -> AirExpression {
        if let Ok(lhs) = binary_operation.lhs()
            && let Ok(rhs) = binary_operation.rhs()
            && let Ok(operator) = binary_operation.binary_operator()
        {
            let lhs = self.transform_expression(lhs);
            let rhs = self.transform_expression(rhs);

            AirExpression::new(
                binary_operation.id(),
                AirExpressionKind::BinaryOperator(data::BinaryOperation {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    operator,
                }),
            )
        } else {
            AirExpression::error(binary_operation.id())
        }
    }

    fn transform_value(&mut self, value: Value) -> AirExpression {
        match value {
            Value::NumberLiteral(number) => self.transform_number(number),
            Value::StringLiteral(string) => self.transform_string(string),
            Value::Ident(ident) => self.transform_ident(ident),
            Value::FunctionCall(function_call) => self.transform_function_call(function_call),
            Value::Parenthesis(parenthesis) => self.transform_parenthesis(parenthesis),
        }
    }

    fn transform_number(&mut self, number: NumberLiteral) -> AirExpression {
        let Ok(token) = number.value() else {
            return AirExpression::error(number.id());
        };

        let Ok(value) = token.text.as_ref().parse() else {
            self.add_error(number.id(), DiagnosticError::InvalidNumber);
            return AirExpression::error(number.id());
        };

        AirExpression::new(
            number.id(),
            AirExpressionKind::Constant(AirConstant::Number(value)),
        )
    }

    fn transform_string(&mut self, string: StringLiteral) -> AirExpression {
        let Ok(token) = string.value() else {
            return AirExpression::error(string.id());
        };

        let text = token.text.trim_start_matches("\"").trim_end_matches("\"");
        AirExpression::new(
            token.id,
            AirExpressionKind::Constant(AirConstant::String(text.into())),
        )
    }

    fn transform_ident(&mut self, ident: Ident) -> AirExpression {
        let Ok(ident) = ident.ident() else {
            return AirExpression::error(ident.id());
        };

        let Some(&variable_id) = self.variables.get(&ident.text) else {
            self.add_error(
                ident.id,
                DiagnosticError::UndefinedVariable {
                    ident: ident.text.clone(),
                },
            );
            return AirExpression::error(ident.id);
        };

        AirExpression::new(ident.id, AirExpressionKind::Variable(variable_id))
    }

    fn transform_function_call(&mut self, function_call: FunctionCall) -> AirExpression {
        let Ok(ident) = function_call.ident() else {
            return AirExpression::error(function_call.id());
        };
        let Ok(argument_list) = function_call.argument_list() else {
            return AirExpression::error(function_call.id());
        };

        let arguments = argument_list
            .arguments()
            .map(|arg| self.transform_expression(arg))
            .collect();

        if let Ok(intrinsic) = ident.text.parse() {
            AirExpression::new(
                function_call.id(),
                AirExpressionKind::IntrinsicCall(IntrinsicCall {
                    intrinsic,
                    arguments,
                }),
            )
        } else {
            let function = self.namespace[&ident.text];
            AirExpression::new(
                function_call.id(),
                AirExpressionKind::Call(Call {
                    function,
                    arguments,
                }),
            )
        }
    }

    fn transform_parenthesis(&mut self, parenthesis: Parenthesis) -> AirExpression {
        let Ok(expression) = parenthesis.expression() else {
            return AirExpression::error(parenthesis.id());
        };
        self.transform_expression(expression)
    }
}
