use crate::{
    auryn::{
        air::{
            air,
            air::{
                Air, AirBlock, AirBlockFinalizer, AirBlockId, AirConstant, AirExpression,
                AirExpressionKind, AirNode, AirNodeKind, AirValueId, Intrinsic, IntrinsicCall,
            },
        },
        ast::ast_node::{
            Assignment, BinaryOperation, Block, BreakStatement, Expression, FunctionCall, Ident,
            IfStatement, LoopStatement, Number, Parenthesis, Root, Statement, Value,
            VariableUpdate,
        },
        parser::{DiagnosticError, DiagnosticKind},
    },
    utils::{fast_map::FastMap, small_string::SmallString},
};

#[derive(Debug)]
pub struct AirOutput {
    pub air: Air,
    pub diagnostics: Vec<DiagnosticKind>,
}

pub fn transform_ast(ast: Root) -> AirOutput {
    let mut transformer = AstTransformer::new();
    transformer.transform_root(ast);
    AirOutput {
        air: Air {
            blocks: transformer.finished_blocks,
        },
        diagnostics: transformer.diagnostics,
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
pub struct AstTransformer {
    block_builders: FastMap<AirBlockId, BlockBuilder>,
    current_builder: AirBlockId,
    finished_blocks: FastMap<AirBlockId, AirBlock>,
    next_value_id: usize,
    variables: FastMap<SmallString, AirValueId>,
    diagnostics: Vec<DiagnosticKind>,
    loops: Vec<LoopInfo>,
}

impl AstTransformer {
    fn new() -> Self {
        Self {
            block_builders: FastMap::default(),
            current_builder: AirBlockId(0),
            finished_blocks: FastMap::default(),
            next_value_id: 1,
            variables: FastMap::default(),
            diagnostics: Vec::new(),
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

    fn add_error(&mut self, error: DiagnosticError) {
        self.diagnostics.push(DiagnosticKind::Error(error))
    }

    fn create_variable(&mut self, ident: SmallString, value_id: AirValueId) {
        if self.variables.contains_key(&ident) {
            self.add_error(DiagnosticError::RedefinedVariable {
                ident: ident.clone(),
            })
        }
        self.variables.insert(ident, value_id);
    }

    fn create_value(&mut self) -> AirValueId {
        let id = AirValueId(self.next_value_id);
        self.next_value_id += 1;
        id
    }
}

impl AstTransformer {
    fn transform_root(&mut self, root: Root) {
        let Ok(block) = root.block() else {
            return;
        };
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
        self.create_variable(ident.text.clone(), id);
        self.add_node(AirNode {
            kind: AirNodeKind::Assignment(air::Assignment {
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

    fn transform_break_statement(&mut self, _: BreakStatement) {
        let Some(loop_info) = self.loops.last() else {
            self.add_error(DiagnosticError::BreakOutsideLoop);
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
            self.add_error(DiagnosticError::UndefinedVariable {
                ident: ident.text.clone(),
            });
            return;
        };

        self.add_node(AirNode {
            kind: AirNodeKind::Assignment(air::Assignment {
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
        let Ok(lhs) = binary_operation.lhs() else {
            return AirExpression::ERROR;
        };
        let Ok(rhs) = binary_operation.rhs() else {
            return AirExpression::ERROR;
        };
        let Ok(operator) = binary_operation.binary_operator() else {
            return AirExpression::ERROR;
        };

        let lhs = self.transform_expression(lhs);
        let rhs = self.transform_expression(rhs);

        AirExpression::new(AirExpressionKind::BinaryOperator(air::BinaryOperation {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            operator,
        }))
    }

    fn transform_value(&mut self, value: Value) -> AirExpression {
        match value {
            Value::Number(number) => self.transform_number(number),
            Value::Ident(ident) => self.transform_ident(ident),
            Value::FunctionCall(function_call) => self.transform_function_call(function_call),
            Value::Parenthesis(parenthesis) => self.transform_parenthesis(parenthesis),
        }
    }

    fn transform_number(&mut self, number: Number) -> AirExpression {
        let Ok(token) = number.value() else {
            return AirExpression::ERROR;
        };

        let Ok(value) = token.text.as_ref().parse() else {
            self.add_error(DiagnosticError::InvalidNumber);
            return AirExpression::ERROR;
        };

        AirExpression::new(AirExpressionKind::Constant(AirConstant::Number(value)))
    }

    fn transform_ident(&mut self, ident: Ident) -> AirExpression {
        let Ok(ident) = ident.ident() else {
            return AirExpression::ERROR;
        };

        let Some(&variable_id) = self.variables.get(&ident.text) else {
            self.add_error(DiagnosticError::UndefinedVariable {
                ident: ident.text.clone(),
            });
            return AirExpression::ERROR;
        };

        AirExpression::new(AirExpressionKind::Variable(variable_id))
    }

    fn transform_function_call(&mut self, function_call: FunctionCall) -> AirExpression {
        let Ok(ident) = function_call.ident() else {
            return AirExpression::ERROR;
        };
        let Ok(argument_list) = function_call.argument_list() else {
            return AirExpression::ERROR;
        };

        let arguments = argument_list
            .arguments()
            .map(|arg| self.transform_expression(arg))
            .collect();

        let intrinsic = match ident.text.as_ref() {
            "print" => Intrinsic::Print,
            _ => {
                self.add_error(DiagnosticError::UnknownIntrinsic {
                    ident: ident.text.clone(),
                });
                return AirExpression::ERROR;
            }
        };

        AirExpression::new(AirExpressionKind::IntrinsicCall(IntrinsicCall {
            intrinsic,
            arguments,
        }))
    }

    fn transform_parenthesis(&mut self, parenthesis: Parenthesis) -> AirExpression {
        let Ok(expression) = parenthesis.expression() else {
            return AirExpression::ERROR;
        };
        self.transform_expression(expression)
    }
}
