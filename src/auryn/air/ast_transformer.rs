use std::str::FromStr;

use crate::{
    auryn::{
        air::{
            data::{
                self, AirBlock, AirBlockFinalizer, AirBlockId, AirConstant, AirExpression,
                AirExpressionKind, AirFunction, AirLocalValueId, AirModuleId, AirNode, AirNodeKind,
                AirPlace, AirPlaceKind, AirStaticValue, AirStaticValueId, AirType, AirValueId,
                Call, ExternFunctionKind, FunctionReference, Globals, ReturnValue, TypeAliasId,
                UnaryOperation, UnaryOperator, UnresolvedExternMember,
            },
            namespace::{Namespace, UserDefinedTypeId},
            typecheck::{type_context::TypeId, types},
            unresolved_type::UnresolvedType,
        },
        ast::ast_node::{
            Accessor, ArgumentList, Assignment, AstError, BinaryOperation, Block, BooleanLiteral,
            BreakStatement, ContinueStatement, Expression, ExternBlock, ExternBlockItem,
            ExternBlockItemKind, ExternTypeBody, ExternTypeBodyItemKind, FunctionDefinition, Ident,
            IfStatement, IfStatementElse, Item, LoopStatement, NumberLiteral, Parenthesis, Path,
            PostfixOperation, PostfixOperator, PrefixNot, ReturnStatement, Root, Statement,
            StringLiteral, Struct, StructBody, StructLiteral, StructLiteralField,
            StructuralTypeField, Type, TypeAlias, Value, ValueOrPostfix, VariableUpdate,
            WhileStatement,
        },
        diagnostic::{DiagnosticError, Diagnostics},
        syntax_id::SyntaxId,
        syntax_tree::SyntaxToken,
    },
    utils::{default, fast_map::FastMap, small_string::SmallString},
};

#[derive(Debug)]
pub struct TransformerOutput {
    pub globals: Globals,
    pub namespace: Namespace,
    pub diagnostics: Diagnostics,
}

pub fn query_globals(
    ast: Root,
    included_modules: impl IntoIterator<Item = (SmallString, AirModuleId)>,
) -> TransformerOutput {
    let mut transformer = AstTransformer::new(Namespace::with_modules(included_modules));
    transformer.transform_root(ast);
    TransformerOutput {
        globals: transformer.globals,
        namespace: transformer.namespace,
        diagnostics: transformer.diagnostics,
    }
}

#[derive(Debug)]
struct AstTransformer {
    globals: Globals,
    namespace: Namespace,
    diagnostics: Diagnostics,
}

impl AstTransformer {
    fn new(namespace: Namespace) -> Self {
        Self {
            namespace,
            globals: default(),
            diagnostics: default(),
        }
    }

    pub fn transform_root(&mut self, root: Root) {
        let Ok(file) = root.file() else {
            return;
        };

        for item in file.items() {
            self.register_item(item);
        }

        for item in file.items() {
            self.transform_item(item);
        }
    }

    fn register_item(&mut self, item: Item) {
        match item {
            Item::FunctionDefinition(function_definition) => {
                self.register_function(function_definition)
            }
            Item::ExternBlock(block) => self.register_extern_block(block),
            Item::TypeAlias(type_alias) => self.register_type_alias(type_alias),
            Item::Struct(struct_definition) => self.register_struct(struct_definition),
        }
    }

    fn register_function(&mut self, function_definition: FunctionDefinition) {
        let Ok(ident) = function_definition.ident() else {
            return;
        };

        self.namespace.statics.insert(
            ident.text.clone(),
            AirStaticValueId(function_definition.id()),
        );
    }

    fn register_extern_block(&mut self, block: ExternBlock) {
        for item in block.items() {
            self.register_extern_block_item(item);
        }
    }

    fn register_extern_block_item(&mut self, item: ExternBlockItem) {
        let Ok(kind) = item.kind() else {
            return;
        };

        match kind {
            ExternBlockItemKind::ExternType(extern_type) => {
                let Ok(ident) = extern_type.ident() else {
                    return;
                };

                self.namespace.types.insert(
                    ident.text.clone(),
                    UserDefinedTypeId::Extern(TypeId::new(extern_type.id())),
                );
            }
        }
    }

    fn register_type_alias(&mut self, type_alias: TypeAlias) {
        let Ok(ident) = type_alias.ident() else {
            return;
        };
        self.namespace.types.insert(
            ident.text.clone(),
            UserDefinedTypeId::TypeAlias(TypeAliasId(ident.id)),
        );
    }

    fn register_struct(&mut self, struct_def: Struct) {
        let Ok(ident) = struct_def.ident() else {
            return;
        };
        self.namespace.types.insert(
            ident.text.clone(),
            UserDefinedTypeId::Struct(TypeId::new(ident.id)),
        );
    }

    fn transform_item(&mut self, item: Item) {
        match item {
            Item::FunctionDefinition(function_definition) => {
                self.transform_function(function_definition)
            }
            Item::TypeAlias(type_alias) => self.transform_type_alias(type_alias),
            Item::Struct(struct_def) => self.transform_struct(struct_def),
            Item::ExternBlock(block) => self.transform_extern_block(block),
        }
    }

    fn transform_extern_block(&mut self, block: ExternBlock) {
        if let Ok(target) = block.extern_target()
            && target.string_literal_text().as_str() != "java"
        {
            self.diagnostics
                .add(target.id, DiagnosticError::UnexpectedExternTarget);
        }
        for item in block.items() {
            self.transform_extern_block_item(item);
        }
    }

    fn transform_extern_block_item(&mut self, item: ExternBlockItem) {
        let Ok(kind) = item.kind() else {
            return;
        };
        match kind {
            ExternBlockItemKind::ExternType(extern_type) => {
                let Ok(ident) = extern_type.ident() else {
                    return;
                };
                let extern_path = item.metadata().and_then(|metadata| metadata.value());
                let Ok(extern_path) = extern_path else {
                    self.diagnostics
                        .add(item.id(), DiagnosticError::ExternTypeRequiresMetadata);
                    return;
                };
                let Ok(extern_body) = extern_type.body() else {
                    return;
                };
                let def_id = self.namespace.types[&ident.text];
                let extern_members = self.collect_extern_type_members(def_id, extern_body);

                let extern_path = extern_path.string_literal_text().into();
                let extern_type = AirType::Unresolved(UnresolvedType::Extern {
                    id: extern_type.id(),
                    extern_name: extern_path,
                    members: extern_members,
                });
                self.globals.types.insert(def_id, extern_type);
            }
        }
    }

    fn collect_extern_type_members(
        &mut self,
        def_id: UserDefinedTypeId,
        body: ExternTypeBody,
    ) -> FastMap<SmallString, UnresolvedExternMember> {
        let mut result = FastMap::default();

        for item in body.items() {
            let Ok(metadata) = item.metadata() else {
                self.diagnostics
                    .add(item.id(), DiagnosticError::ExternTypeRequiresMetadata);
                continue;
            };
            let Ok(metadata_token) = metadata.value() else {
                continue;
            };
            let extern_name = metadata_token.string_literal_text().into();

            let Ok(kind) = item.kind() else {
                continue;
            };

            match kind {
                ExternTypeBodyItemKind::ExternTypeStaticLet(static_let) => {
                    let Ok(ident) = static_let.ident() else {
                        continue;
                    };
                    let Ok(r#type) = static_let.r#type() else {
                        continue;
                    };

                    let Ok(unresolved) = transform_to_unresolved(&self.namespace, r#type) else {
                        continue;
                    };
                    let member = UnresolvedExternMember::StaticLet {
                        r#type: unresolved,
                        extern_name,
                    };
                    result.insert(ident.text.clone(), member);
                }
                ExternTypeBodyItemKind::ExternTypeFunction(function) => {
                    let Ok(ident) = function.ident() else {
                        continue;
                    };
                    let Ok(parameters) = function.parameters() else {
                        continue;
                    };

                    let syntax_id = ident.id;
                    let ident = ident.text.clone();
                    let declared_parameters = parameters
                        .parameters()
                        .filter_map(|param| {
                            param
                                .r#type()
                                .ok()
                                .and_then(|ty| transform_to_unresolved(&self.namespace, ty).ok())
                        })
                        .collect();
                    let declared_return_type = function
                        .return_type()
                        .ok()
                        .and_then(|ty| ty.r#type().ok())
                        .and_then(|ty| transform_to_unresolved(&self.namespace, ty).ok())
                        .map(Box::new);
                    let kind = if function.is_static() {
                        ExternFunctionKind::Static
                    } else {
                        ExternFunctionKind::Method
                    };

                    let member = UnresolvedExternMember::Function {
                        unresolved_type: UnresolvedType::Function {
                            parameters_reference: parameters.id(),
                            parameters: declared_parameters,
                            return_type: declared_return_type,
                            reference: FunctionReference::Extern {
                                extern_name: extern_name.clone(),
                                kind,
                                parent: Box::new(AirType::Unresolved(UnresolvedType::DefinedType(
                                    def_id,
                                ))),
                                syntax_id,
                            },
                        },
                        ident: ident.clone(),
                        extern_name,
                    };

                    result.insert(ident, member);
                }
            }
        }

        result
    }

    fn transform_type_alias(&mut self, type_alias: TypeAlias) {
        let Ok(ident) = type_alias.ident() else {
            return;
        };
        let Ok(r#type) = type_alias.r#type() else {
            return;
        };

        let Ok(r#type) = transform_to_unresolved(&self.namespace, r#type) else {
            return;
        };
        self.globals
            .type_aliases
            .insert(TypeAliasId(ident.id), AirType::Unresolved(r#type));
    }

    fn transform_struct(&mut self, struct_def: Struct) {
        let Ok(ident) = struct_def.ident() else {
            return;
        };
        let id = TypeId::new(ident.id);
        let Ok(body) = struct_def.body() else {
            return;
        };
        let Ok(fields) = self.transform_struct_body(body) else {
            return;
        };
        self.globals.types.insert(
            UserDefinedTypeId::Struct(id),
            AirType::Unresolved(UnresolvedType::Struct {
                id: id.syntax_id(),
                ident: ident.text.clone(),
                fields,
            }),
        );
    }

    fn transform_struct_body(
        &self,
        body: StructBody,
    ) -> Result<Vec<(SmallString, UnresolvedType)>, AstError> {
        let fields = body
            .fields()
            .map(|field| transform_field_to_unresolved(&self.namespace, field))
            .collect::<Result<_, _>>()?;
        Ok(fields)
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
                param
                    .r#type()
                    .ok()
                    .and_then(|ty| transform_to_unresolved(&self.namespace, ty).ok())
            })
            .collect::<Vec<_>>();
        let declared_return_type = function_definition
            .return_type()
            .ok()
            .and_then(|r#type| r#type.r#type().ok())
            .and_then(|r#type| transform_to_unresolved(&self.namespace, r#type).ok())
            .map(Box::new);

        let parameter_idents = parameters
            .parameters()
            .filter_map(|param| param.ident().ok().map(|ident| ident.text.clone()))
            .collect::<Vec<_>>();

        let function_transformer =
            FunctionTransformer::new(parameter_idents, &mut self.diagnostics, &self.namespace);
        let blocks = function_transformer.transform_function_body(block);

        let function_id = self.namespace.unwrap_function(&ident);
        let function = AirFunction {
            r#type: AirType::Inferred,
            unresolved_type: UnresolvedType::Function {
                parameters_reference: parameters.id(),
                parameters: declared_parameters,
                return_type: declared_return_type,
                reference: FunctionReference::UserDefined(function_id),
            },
            ident,
            blocks,
        };

        self.globals.functions.insert(function_id, function);
        self.globals
            .statics
            .insert(function_id.0, AirStaticValue::Function(function_id));
    }
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub variables: FastMap<SmallString, AirLocalValueId>,
}

/// Represents an unfinished [`AirBlock`]
#[derive(Debug)]
struct BlockBuilder {
    nodes: Vec<AirNode>,
    scope: Scope,
}

impl BlockBuilder {
    fn new(scope: Scope) -> Self {
        Self {
            nodes: default(),
            scope,
        }
    }
}

#[derive(Debug)]
pub struct LoopInfo {
    continue_target: AirBlockId,
    break_target: AirBlockId,
}

#[derive(Debug)]
struct FunctionTransformer<'a> {
    block_builders: FastMap<AirBlockId, BlockBuilder>,
    current_builder: AirBlockId,
    finished_blocks: FastMap<AirBlockId, AirBlock>,
    next_value_id: usize,
    diagnostics: &'a mut Diagnostics,
    namespace: &'a Namespace,
    loops: Vec<LoopInfo>,
}

impl<'a> FunctionTransformer<'a> {
    fn new(
        parameter_idents: Vec<SmallString>,
        diagnostics: &'a mut Diagnostics,
        namespace: &'a Namespace,
    ) -> Self {
        let variables = parameter_idents
            .into_iter()
            .enumerate()
            .map(|(index, ident)| (ident, AirLocalValueId(index)))
            .collect::<FastMap<_, _>>();
        let next_value_id = variables.len();
        let mut block_builders = FastMap::default();
        block_builders.insert(AirBlockId(0), BlockBuilder::new(Scope { variables }));
        Self {
            block_builders,
            current_builder: AirBlockId(0),
            finished_blocks: FastMap::default(),
            next_value_id,
            diagnostics,
            namespace,
            loops: Vec::new(),
        }
    }

    fn current_block_mut(&mut self) -> &mut BlockBuilder {
        self.block_builders.get_mut(&self.current_builder).unwrap()
    }

    fn add_block(&mut self) -> AirBlockId {
        let block_id = AirBlockId(self.finished_blocks.len() + self.block_builders.len());
        let scope = self.current_block_mut().scope.clone();
        self.block_builders
            .insert(block_id, BlockBuilder::new(scope));
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
        self.current_block_mut().nodes.push(node);
    }

    fn create_variable(&mut self, token: &SyntaxToken, value_id: AirLocalValueId) {
        self.current_block_mut()
            .scope
            .variables
            .insert(token.text.clone(), value_id);
    }

    fn create_value(&mut self) -> AirLocalValueId {
        let id = AirLocalValueId(self.next_value_id);
        self.next_value_id += 1;
        id
    }
}

impl FunctionTransformer<'_> {
    fn transform_function_body(mut self, block: Block) -> FastMap<AirBlockId, AirBlock> {
        let block_id = AirBlockId(0);
        self.transform_block(block);
        // The Return(None) is just a placeholder, could also be replaced with an actual error variant
        self.finish_block(
            block_id,
            AirBlockFinalizer::Return(data::ReturnValue::Null(block.id())),
        );
        assert!(self.block_builders.is_empty());
        self.finished_blocks
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
            Statement::WhileStatement(while_statement) => {
                self.transform_while_statement(while_statement)
            }
            Statement::BreakStatement(break_statement) => {
                self.transform_break_statement(break_statement)
            }
            Statement::ContinueStatement(continue_statement) => {
                self.transform_continue_statement(continue_statement)
            }
            Statement::ReturnStatement(return_statement) => {
                self.transform_return_statement(return_statement)
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

        let expected_type = assignment
            .r#type()
            .and_then(|r#type| transform_to_unresolved(self.namespace, r#type))
            .ok();

        let expression = self.transform_expression(expression);
        let id = self.create_value();
        self.create_variable(ident, id);
        self.add_node(AirNode {
            id: assignment.id(),
            kind: AirNodeKind::Assignment(data::Assignment {
                target: id,
                expected_type,
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
        let neg_block_id = self.add_block();
        let next_block_id = self.add_block();

        self.finish_block(
            pos_block_id,
            AirBlockFinalizer::Branch {
                value: Box::new(expression),
                pos_block: pos_block_id,
                neg_block: neg_block_id,
            },
        );
        self.transform_block(block);
        self.finish_block(neg_block_id, AirBlockFinalizer::Goto(next_block_id));

        if let Ok(neg_block_or_statement) = if_statement.r#else() {
            match neg_block_or_statement {
                IfStatementElse::Block(neg_block) => self.transform_block(neg_block),
                IfStatementElse::Statement(statement) => self.transform_statement(statement),
            }
        }
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
            continue_target: loop_body,
        });
        self.transform_block(block);
        self.loops.pop().expect("Should have something to pop");
        self.finish_block(next_block_id, AirBlockFinalizer::Goto(loop_body));
    }

    fn transform_while_statement(&mut self, while_statement: WhileStatement) {
        let Ok(condition) = while_statement.expression() else {
            return;
        };
        let condition = self.transform_expression(condition);
        let Ok(block) = while_statement.block() else {
            return;
        };

        let body_block = self.add_block();
        let main_body_block = self.add_block();
        let next_block = self.add_block();

        self.finish_block(body_block, AirBlockFinalizer::Goto(body_block));
        self.loops.push(LoopInfo {
            continue_target: body_block,
            break_target: next_block,
        });

        self.finish_block(
            main_body_block,
            AirBlockFinalizer::Branch {
                value: Box::new(condition),
                pos_block: main_body_block,
                neg_block: next_block,
            },
        );

        self.transform_block(block);
        self.loops.pop().expect("Should have something to pop");
        self.finish_block(next_block, AirBlockFinalizer::Goto(body_block));
    }

    fn transform_break_statement(&mut self, r#break: BreakStatement) {
        let Some(loop_info) = self.loops.last() else {
            self.diagnostics
                .add(r#break.id(), DiagnosticError::BreakOutsideLoop);
            return;
        };
        let break_target = loop_info.break_target;

        let dead_code_block = self.add_block();
        self.finish_block(dead_code_block, AirBlockFinalizer::Goto(break_target));
    }

    fn transform_continue_statement(&mut self, r#continue: ContinueStatement) {
        let Some(loop_info) = self.loops.last() else {
            self.diagnostics
                .add(r#continue.id(), DiagnosticError::ContinueOutsideLoop);
            return;
        };
        let continue_target = loop_info.continue_target;

        let dead_code_block = self.add_block();
        self.finish_block(dead_code_block, AirBlockFinalizer::Goto(continue_target));
    }

    fn transform_return_statement(&mut self, r#return: ReturnStatement) {
        let expression = r#return
            .expression()
            .ok()
            .map(|expression| Box::new(self.transform_expression(expression)));

        let dead_code_block = self.add_block();
        self.finish_block(
            dead_code_block,
            AirBlockFinalizer::Return(
                expression.map_or(ReturnValue::Null(r#return.id()), |expr| {
                    data::ReturnValue::Expression(expr)
                }),
            ),
        );
    }

    fn transform_variable_update(&mut self, variable_update: VariableUpdate) {
        let Ok(expression) = variable_update.expression() else {
            return;
        };
        let expression = self.transform_expression(expression);

        let Ok(token) = variable_update.assignment_token() else {
            return;
        };

        let Ok(path) = variable_update.path() else {
            return;
        };
        let Ok(path) = self.transform_path(path) else {
            return;
        };

        self.add_node(AirNode {
            id: variable_update.id(),
            kind: AirNodeKind::Update(data::Update {
                target: path,
                operator: token,
                expression: Box::new(expression),
            }),
        });
    }

    fn transform_path(&mut self, path: Path) -> Result<AirPlace, AstError> {
        let expression = path.expression()?;
        let expression = self.transform_expression(expression);
        let kind = match expression.kind {
            AirExpressionKind::Variable(id) => match id {
                AirValueId::Local(local_id) => AirPlaceKind::Variable(local_id),
                AirValueId::Global(_) | AirValueId::Intrinsic(_) => {
                    self.diagnostics
                        .add(path.id(), DiagnosticError::ImmutableVariableUpdate);
                    return Err(AstError);
                }
            },
            AirExpressionKind::Accessor(accessor) => AirPlaceKind::Accessor(accessor),
            _ => {
                self.diagnostics
                    .add(path.id(), DiagnosticError::InvalidPlace);
                return Err(AstError);
            }
        };
        Ok(AirPlace {
            id: path.id(),
            kind,
            r#type: AirType::Inferred,
        })
    }

    fn transform_expression(&mut self, expression: Expression) -> AirExpression {
        match expression {
            Expression::BinaryOperation(binary_operation) => {
                self.transform_binary_operation(binary_operation)
            }
            Expression::PostfixOperation(postfix_operation) => {
                self.transform_postfix_operation(postfix_operation)
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

    fn transform_postfix_operation(&mut self, postfix_op: PostfixOperation) -> AirExpression {
        let (Ok(value), Ok(operator)) = (postfix_op.value(), postfix_op.operator()) else {
            return AirExpression::error(postfix_op.id());
        };

        match operator {
            PostfixOperator::ArgumentList(argument_list) => {
                self.transform_postfix_call(postfix_op.id(), value, argument_list)
            }
            PostfixOperator::Accessor(accessor) => {
                self.transform_accessor(postfix_op.id(), value, accessor)
            }
        }
    }

    fn transform_value_or_postfix_operation(
        &mut self,
        value_or_postfix: ValueOrPostfix,
    ) -> AirExpression {
        match value_or_postfix {
            ValueOrPostfix::Value(value) => self.transform_value(value),
            ValueOrPostfix::Postfix(postfix_operation) => {
                self.transform_postfix_operation(postfix_operation)
            }
        }
    }

    fn transform_postfix_call(
        &mut self,
        id: SyntaxId,
        value: ValueOrPostfix,
        argument_list: ArgumentList,
    ) -> AirExpression {
        let function = Box::new(self.transform_value_or_postfix_operation(value));
        let arguments = argument_list
            .arguments()
            .map(|arg| self.transform_expression(arg))
            .collect();

        AirExpression::new(
            id,
            AirExpressionKind::Call(Call {
                function,
                arguments,
            }),
        )
    }

    fn transform_accessor(
        &mut self,
        id: SyntaxId,
        value: ValueOrPostfix,
        accessor: Accessor,
    ) -> AirExpression {
        let value = self.transform_value_or_postfix_operation(value);
        let Ok(ident) = accessor.ident() else {
            return AirExpression::error(value.id);
        };

        AirExpression::new(
            id,
            AirExpressionKind::Accessor(data::Accessor {
                value: Box::new(value),
                ident: ident.text.clone(),
                ident_id: ident.id,
            }),
        )
    }

    fn transform_value(&mut self, value: Value) -> AirExpression {
        match value {
            Value::NumberLiteral(number) => self.transform_number(number),
            Value::StringLiteral(string) => self.transform_string(string),
            Value::PrefixNot(not) => self.transform_prefix_not(not),
            Value::BooleanLiteral(boolean) => self.transform_boolean(boolean),
            Value::Ident(ident) => self.transform_ident(ident),
            Value::Parenthesis(parenthesis) => self.transform_parenthesis(parenthesis),
            Value::StructLiteral(struct_literal) => self.transform_struct_literal(struct_literal),
        }
    }

    fn transform_number(&mut self, number: NumberLiteral) -> AirExpression {
        let Ok(token) = number.value() else {
            return AirExpression::error(number.id());
        };

        let Ok(value) = token.text.as_ref().parse() else {
            self.diagnostics
                .add(number.id(), DiagnosticError::InvalidNumber);
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

        let text = token.string_literal_text().into();
        AirExpression::new(
            token.id,
            AirExpressionKind::Constant(AirConstant::String(text)),
        )
    }

    fn transform_prefix_not(&mut self, not: PrefixNot) -> AirExpression {
        let Ok(value) = not.value() else {
            return AirExpression::error(not.id());
        };
        let expression = Box::new(self.transform_value_or_postfix_operation(value));
        AirExpression::new(
            not.id(),
            AirExpressionKind::UnaryOperator(UnaryOperation {
                operator: UnaryOperator::Not,
                expression,
            }),
        )
    }

    fn transform_boolean(&mut self, boolean: BooleanLiteral) -> AirExpression {
        AirExpression::new(
            boolean.id(),
            AirExpressionKind::Constant(AirConstant::Boolean(boolean.value())),
        )
    }

    fn transform_ident(&mut self, ident: Ident) -> AirExpression {
        let Ok(ident) = ident.ident() else {
            return AirExpression::error(ident.id());
        };

        if let Some(&variable_id) = self.current_block_mut().scope.variables.get(&ident.text) {
            return AirExpression::new(
                ident.id,
                AirExpressionKind::Variable(AirValueId::Local(variable_id)),
            );
        }

        if let Some(variable_id) = self.namespace.get(&ident.text) {
            return AirExpression::new(
                ident.id,
                AirExpressionKind::Variable(AirValueId::Global(variable_id)),
            );
        }

        if let Ok(intrinsic) = ident.text.parse() {
            return AirExpression::new(
                ident.id,
                AirExpressionKind::Variable(AirValueId::Intrinsic(intrinsic)),
            );
        };

        if let Some(type_id) = self.namespace.types.get(&ident.text) {
            return AirExpression::new(
                ident.id,
                AirExpressionKind::Type(AirType::Unresolved(UnresolvedType::DefinedType(*type_id))),
            );
        }

        if let Ok(intrinsic_type) = types::Type::from_str(&ident.text) {
            return AirExpression::new(
                ident.id,
                AirExpressionKind::Type(AirType::Computed(intrinsic_type)),
            );
        }

        self.diagnostics.add(
            ident.id,
            DiagnosticError::UndefinedVariable {
                ident: ident.text.clone(),
            },
        );
        AirExpression::error(ident.id)
    }

    fn transform_parenthesis(&mut self, parenthesis: Parenthesis) -> AirExpression {
        let Ok(expression) = parenthesis.expression() else {
            return AirExpression::error(parenthesis.id());
        };
        self.transform_expression(expression)
    }

    fn transform_struct_literal(&mut self, struct_literal: StructLiteral) -> AirExpression {
        let fields = struct_literal
            .fields()
            .flat_map(|it| self.transform_struct_literal_field(it))
            .collect();

        AirExpression::new(
            struct_literal.id(),
            AirExpressionKind::Constant(AirConstant::StructLiteral(fields)),
        )
    }

    fn transform_struct_literal_field(
        &mut self,
        field: StructLiteralField,
    ) -> Option<(SmallString, AirExpression)> {
        let ident = field.ident().ok()?;
        let expression = field.value().ok()?;
        Some((ident.text.clone(), self.transform_expression(expression)))
    }
}

fn transform_to_unresolved(
    namespace: &Namespace,
    r#type: Type,
) -> Result<UnresolvedType, AstError> {
    match r#type {
        Type::StructuralType(structural) => {
            let fields = structural
                .fields()
                .map(|field| transform_field_to_unresolved(namespace, field))
                .collect::<Result<_, _>>()?;
            Ok(UnresolvedType::Structural(fields))
        }
        Type::ArrayType(inner) => match inner.r#type() {
            Ok(r#type) => Ok(UnresolvedType::Array(
                r#type.id(),
                Box::new(transform_to_unresolved(namespace, r#type)?),
            )),
            Err(err) => Err(err),
        },
        Type::UnitType(_) => Ok(UnresolvedType::Unit),
        Type::Ident(ident) => ident
            .ident()
            .map(|token| match namespace.types.get(&token.text) {
                Some(def_id) => UnresolvedType::DefinedType(*def_id),
                None => UnresolvedType::Ident(token.id, token.text.clone()),
            }),
    }
}

fn transform_field_to_unresolved(
    namespace: &Namespace,
    field: StructuralTypeField,
) -> Result<(SmallString, UnresolvedType), AstError> {
    let ident = field.ident()?;
    let ty = field.r#type()?;

    Ok((ident.text.clone(), transform_to_unresolved(namespace, ty)?))
}
