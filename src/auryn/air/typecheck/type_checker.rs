use std::collections::VecDeque;

use crate::{
    auryn::{
        air::{
            data::{
                Accessor, Air, AirBlock, AirBlockFinalizer, AirBlockId, AirConstant, AirExpression,
                AirExpressionKind, AirFunction, AirFunctionId, AirLocalValueId, AirNode,
                AirNodeKind, AirStaticValue, AirStaticValueId, AirType, AirValueId, Assignment,
                BinaryOperation, Call, FunctionReference, Intrinsic, ReturnValue,
                UnresolvedExternMember, UnresolvedType,
            },
            namespace::UserDefinedTypeId,
            typecheck::{
                type_context::{TypeContext, TypeId},
                types::{
                    ExternType, ExternTypeMember, FunctionItemType, FunctionParameters, Type,
                    TypeView,
                },
            },
        },
        diagnostic::{DiagnosticError, Diagnostics},
        syntax_id::SyntaxId,
        tokenizer::BinaryOperatorToken,
    },
    utils::{
        default,
        fast_map::{FastMap, FastSet},
    },
};

pub fn typecheck_air(air: &mut Air, diagnostics: Diagnostics) -> Diagnostics {
    Typechecker::new(diagnostics).infer(air)
}

#[derive(Debug)]
struct FunctionContext {
    variables: FastMap<AirLocalValueId, Type>,
    return_type: Type,
}

impl Default for FunctionContext {
    fn default() -> Self {
        Self {
            variables: Default::default(),
            return_type: Type::Error,
        }
    }
}

impl FunctionContext {
    fn clear(&mut self, expected_return_type: Type) {
        let FunctionContext {
            variables,
            return_type,
        } = self;
        variables.clear();
        *return_type = expected_return_type;
    }
}

/// Typechecking works with a simple `check` and `infer` algorithm.
/// If the type of a node is unbounded, the typechecker will call `infer` on it.
/// If the type is bounded, for example by a `let var: Type = ..` statement, the typecheck will call check with
/// the appropriate type instead.
/// This means that type inference can not work across air nodes right now.
/// <https://dl.acm.org/doi/10.1145/3450952>
#[derive(Debug, Default)]
pub struct Typechecker {
    functions: FastMap<AirFunctionId, TypeId<FunctionItemType>>,
    function: FunctionContext,
    statics: FastMap<AirStaticValueId, AirStaticValue>,
    ty_ctx: TypeContext,
    diagnostics: Diagnostics,
}

impl Typechecker {
    pub fn new(diagnostics: Diagnostics) -> Self {
        Self {
            functions: default(),
            function: default(),
            statics: default(),
            ty_ctx: default(),
            diagnostics,
        }
    }

    pub fn infer(mut self, air: &mut Air) -> Diagnostics {
        self.compute_defined_types(&mut air.types);
        self.statics = air.statics.clone();

        for (id, function) in &mut air.functions {
            self.infer_function_signature(*id, function);
        }

        for function in air.functions.values_mut() {
            self.infer_function_body(function);
        }

        air.ty_ctx = self.ty_ctx;

        self.diagnostics
    }

    fn compute_defined_types(&mut self, types: &mut FastMap<UserDefinedTypeId, AirType>) {
        for r#type in types.values_mut() {
            self.resolve_if_unresolved(r#type);
        }
    }

    #[track_caller]
    fn expect_type(&mut self, received: &AirExpression, expected: Type) {
        if let AirType::Computed(got) = &received.r#type
            && got
                .as_view(&self.ty_ctx)
                .is_subtype(expected.as_view(&self.ty_ctx))
        {
            return;
        }

        self.diagnostics.add(
            received.id,
            DiagnosticError::TypeMismatch {
                expected: expected.as_view(&self.ty_ctx).to_string(),
                got: received.r#type.as_view(&self.ty_ctx).to_string(),
            },
        )
    }

    #[track_caller]
    fn expect_type_at(&mut self, at: SyntaxId, received: Type, expected: Type) {
        if received
            .as_view(&self.ty_ctx)
            .is_subtype(expected.as_view(&self.ty_ctx))
        {
            return;
        }

        self.diagnostics.add(
            at,
            DiagnosticError::TypeMismatch {
                expected: expected.as_view(&self.ty_ctx).to_string(),
                got: received.as_view(&self.ty_ctx).to_string(),
            },
        );
    }
}

impl Typechecker {
    fn resolve_if_unresolved(&mut self, air_type: &mut AirType) {
        match air_type {
            AirType::Inferred => unreachable!("Cannot infer type of a type definition"),
            AirType::Unresolved(unresolved) => {
                let resolved = self.resolve_type(unresolved);
                *air_type = AirType::Computed(resolved);
            }
            AirType::Computed(_) => {}
        };
    }

    fn resolve_type(&mut self, unresolved: &UnresolvedType) -> Type {
        match unresolved {
            UnresolvedType::DefinedType(user_defined_type_id) => user_defined_type_id.to_type(),
            UnresolvedType::Unit => Type::Unit,
            UnresolvedType::Ident(id, ident) => match ident.parse() {
                Ok(r#type) => r#type,
                Err(_) => {
                    self.diagnostics.add(
                        *id,
                        DiagnosticError::UndefinedVariable {
                            ident: ident.clone(),
                        },
                    );
                    Type::Error
                }
            },
            UnresolvedType::Function {
                parameters_reference,
                parameters,
                return_type,
                reference,
            } => {
                let parameters = parameters
                    .iter()
                    .map(|param| self.resolve_type(param))
                    .collect();
                let return_type = return_type
                    .as_ref()
                    .map_or(Type::Unit, |ty| self.resolve_type(ty));
                Type::FunctionItem(self.ty_ctx.add_function_item(
                    reference.syntax_id(),
                    FunctionItemType {
                        parameters: FunctionParameters::Constrained {
                            parameters,
                            parameters_reference: *parameters_reference,
                        },
                        return_type,
                        reference: reference.clone(),
                    },
                ))
            }
            UnresolvedType::Array(id, inner) => {
                let element_type = self.resolve_type(inner);
                self.ty_ctx.array_of(*id, element_type)
            }
            // TODO: Prevent infinite recursion and handle recursive definitions
            UnresolvedType::Extern {
                id,
                extern_name,
                members,
            } => {
                let members = members
                    .iter()
                    .map(|(ident, member)| {
                        let member = match member {
                            UnresolvedExternMember::StaticLet {
                                r#type,
                                extern_name,
                                ..
                            } => ExternTypeMember {
                                extern_name: extern_name.clone(),
                                r#type: self.resolve_type(r#type),
                            },
                            UnresolvedExternMember::Function {
                                unresolved_type,
                                extern_name,
                                ..
                            } => ExternTypeMember {
                                r#type: self.resolve_type(unresolved_type),
                                extern_name: extern_name.clone(),
                            },
                        };
                        (ident.clone(), member)
                    })
                    .collect();
                Type::Extern(self.ty_ctx.add_extern(
                    *id,
                    ExternType {
                        extern_name: extern_name.clone(),
                        members,
                    },
                ))
            }
        }
    }

    fn infer_function_signature(&mut self, id: AirFunctionId, function: &mut AirFunction) {
        let computed_ty = self.resolve_type(&function.unresolved_type);
        function.r#type = AirType::Computed(computed_ty);
        let Type::FunctionItem(function_type) = computed_ty else {
            unreachable!("Should compute a function type for a function!");
        };
        self.functions.insert(id, function_type);
    }

    fn infer_function_body(&mut self, function: &mut AirFunction) {
        let TypeView::FunctionItem(function_type) = function.r#type.as_view(&self.ty_ctx) else {
            panic!("Invalid type for function: {:?}", function.r#type);
        };

        self.function.clear(function_type.value.return_type);
        for (id, r#type) in function
            .argument_ids()
            .zip(function_type.value.constrained_parameters().iter())
        {
            self.function.variables.insert(id, *r#type);
        }

        let mut visited_blocks = FastSet::default();
        let mut pending_blocks = VecDeque::default();

        pending_blocks.push_front(AirBlockId::ROOT);

        while let Some(id) = pending_blocks.pop_front() {
            let block = function.blocks.get_mut(&id).unwrap();
            visited_blocks.insert(id);

            self.infer_block(block, |next_id| {
                if !visited_blocks.contains(&next_id) && !pending_blocks.contains(&next_id) {
                    pending_blocks.push_back(next_id);
                }
            });
        }
    }

    fn infer_block(&mut self, block: &mut AirBlock, on_next_id: impl FnMut(AirBlockId)) {
        for node in &mut block.nodes {
            self.infer_node(node);
        }

        self.infer_finalizer(&mut block.finalizer, on_next_id);
    }

    fn infer_finalizer(
        &mut self,
        finalizer: &mut AirBlockFinalizer,
        mut on_next_id: impl FnMut(AirBlockId),
    ) {
        match finalizer {
            AirBlockFinalizer::Return(expression) => match expression {
                ReturnValue::Expression(expression) => {
                    self.check_expression(expression, self.function.return_type);
                }
                ReturnValue::Null(id) => {
                    self.expect_type_at(*id, Type::Unit, self.function.return_type);
                }
            },
            AirBlockFinalizer::Goto(next_id) => on_next_id(*next_id),
            AirBlockFinalizer::Branch {
                value,
                pos_block,
                neg_block,
            } => {
                self.check_expression(value, Type::Bool);
                on_next_id(*pos_block);
                on_next_id(*neg_block);
            }
        }
    }

    fn infer_node(&mut self, node: &mut AirNode) {
        match &mut node.kind {
            AirNodeKind::Assignment(assignment) => self.infer_assignment(assignment),
            AirNodeKind::Expression(expression) => self.infer_expression(expression),
        }
    }

    fn infer_assignment(&mut self, assignment: &mut Assignment) {
        if let Some(expected_type) = self.function.variables.get(&assignment.target) {
            self.check_expression(&mut assignment.expression, *expected_type);
        } else {
            if let Some(expected_type) = &assignment.expected_type {
                let expected_type = self.resolve_type(expected_type);
                self.check_expression(&mut assignment.expression, expected_type);
            } else {
                self.infer_expression(&mut assignment.expression);
            }
            self.function
                .variables
                .insert(assignment.target, assignment.expression.r#type.computed());
        }
    }

    fn infer_expression(&mut self, expression: &mut AirExpression) {
        let expression_type = match &mut expression.kind {
            AirExpressionKind::Constant(constant) => self.infer_constant(expression.id, constant),
            AirExpressionKind::BinaryOperator(binary_operator) => {
                self.infer_binary_operator(binary_operator)
            }
            AirExpressionKind::Variable(value) => self.infer_value(value),
            AirExpressionKind::Type(r#type) => self.ty_ctx.meta_of(expression.id, *r#type),
            AirExpressionKind::Accessor(accessor) => self.infer_accessor(accessor),
            AirExpressionKind::Call(call) => self.typecheck_call(expression.id, call, None),
            AirExpressionKind::Error => Type::Error,
        };

        expression.r#type = AirType::Computed(expression_type);
    }

    fn check_expression(&mut self, expression: &mut AirExpression, expected: Type) {
        let inferred_type = match &mut expression.kind {
            AirExpressionKind::Constant(constant) => {
                self.check_constant(expression.id, constant, expected)
            }
            AirExpressionKind::BinaryOperator(binary_operator) => {
                self.infer_binary_operator(binary_operator)
            }
            AirExpressionKind::Variable(variable) => self.infer_value(variable),
            AirExpressionKind::Type(r#type) => self.ty_ctx.meta_of(expression.id, *r#type),
            AirExpressionKind::Accessor(accessor) => self.infer_accessor(accessor),
            AirExpressionKind::Call(call) => {
                self.typecheck_call(expression.id, call, Some(expected))
            }
            AirExpressionKind::Error => Type::Error,
        };
        self.expect_type_at(expression.id, inferred_type, expected);
        expression.r#type = AirType::Computed(inferred_type);
    }

    fn infer_constant(&mut self, id: SyntaxId, constant: &AirConstant) -> Type {
        match constant {
            AirConstant::Number(value) => self.ty_ctx.number_literal_of(id, *value),
            AirConstant::Boolean(_) => Type::Bool,
            AirConstant::String(_) => Type::String,
        }
    }

    fn check_constant(&mut self, id: SyntaxId, constant: &AirConstant, expected: Type) -> Type {
        match constant {
            AirConstant::Number(value) => {
                if !expected.as_view(&self.ty_ctx).is_subtype(TypeView::Number) {
                    self.diagnostics.add(
                        id,
                        DiagnosticError::TypeMismatch {
                            expected: TypeView::Number.to_string(),
                            got: expected.as_view(&self.ty_ctx).to_string(),
                        },
                    );
                }
                if let TypeView::NumberLiteral(expected_value) = expected.as_view(&self.ty_ctx)
                    && expected_value.value.value != *value
                {
                    self.diagnostics.add(
                        id,
                        DiagnosticError::TypeMismatch {
                            expected: expected_value.value.value.to_string(),
                            got: value.to_string(),
                        },
                    );
                }

                // If we don't expected any concrete number type, use I32 as the default type
                let actual_type = if matches!(expected, Type::Number) {
                    Type::I32
                } else {
                    expected
                };

                if let Some(value_range) = actual_type.int_value_range()
                    && !value_range.contains(value)
                {
                    self.diagnostics.add(
                        id,
                        DiagnosticError::ValueOutsideRange {
                            range: value_range,
                            got: *value,
                            r#type: actual_type.as_view(&self.ty_ctx).to_string(),
                        },
                    );
                }

                actual_type
            }
            AirConstant::String(_) => {
                self.expect_type_at(id, Type::String, expected);
                expected
            }
            AirConstant::Boolean(_) => {
                self.expect_type_at(id, Type::Bool, expected);
                expected
            }
        }
    }

    fn infer_binary_operator(&mut self, binary_operator: &mut BinaryOperation) -> Type {
        let parameter_type = match binary_operator.operator {
            BinaryOperatorToken::Plus
            | BinaryOperatorToken::Minus
            | BinaryOperatorToken::Times
            | BinaryOperatorToken::Divide
            | BinaryOperatorToken::Remainder
            | BinaryOperatorToken::Equal
            | BinaryOperatorToken::NotEqual
            | BinaryOperatorToken::Greater
            | BinaryOperatorToken::GreaterOrEqual
            | BinaryOperatorToken::Less
            | BinaryOperatorToken::LessOrEqual => Type::Number,
            BinaryOperatorToken::And | BinaryOperatorToken::Or => Type::Bool,
        };

        self.check_expression(&mut binary_operator.lhs, parameter_type);
        self.check_expression(
            &mut binary_operator.rhs,
            binary_operator.lhs.r#type.computed(),
        );
        self.expect_type(&binary_operator.rhs, binary_operator.lhs.r#type.computed());

        match binary_operator.operator {
            BinaryOperatorToken::Plus
            | BinaryOperatorToken::Minus
            | BinaryOperatorToken::Times
            | BinaryOperatorToken::Divide
            | BinaryOperatorToken::Remainder => {
                let result_type = binary_operator.lhs.r#type.computed();
                if matches!(result_type, Type::NumberLiteral(_)) {
                    self.diagnostics.add(
                        binary_operator.lhs.id,
                        DiagnosticError::UnsupportedOperationWithType {
                            operation: binary_operator.operator.to_token_kind().as_str().into(),
                            r#type: result_type.as_view(&self.ty_ctx).to_string(),
                        },
                    );
                }
                result_type
            }
            BinaryOperatorToken::Equal
            | BinaryOperatorToken::NotEqual
            | BinaryOperatorToken::Greater
            | BinaryOperatorToken::GreaterOrEqual
            | BinaryOperatorToken::Less
            | BinaryOperatorToken::LessOrEqual
            | BinaryOperatorToken::And
            | BinaryOperatorToken::Or => Type::Bool,
        }
    }

    fn infer_value(&mut self, value: &AirValueId) -> Type {
        match value {
            AirValueId::Local(local_value_id) => *self
                .function
                .variables
                .get(local_value_id)
                .expect("Should have type for value"),
            AirValueId::Global(global_value_id) => {
                let AirStaticValue::Function(function_id) = self
                    .statics
                    .get(global_value_id)
                    .expect("Should have type for value");
                let function = self.functions[function_id];
                Type::FunctionItem(function)
            }
            AirValueId::Intrinsic(intrinsic) => intrinsic.r#type(),
        }
    }

    fn infer_accessor(&mut self, accessor: &mut Accessor) -> Type {
        self.infer_expression(&mut accessor.value);
        let value_type_view = accessor.value.r#type.as_view(&self.ty_ctx);
        match value_type_view.get_member(&accessor.ident) {
            Some(member_type_view) => member_type_view.as_type(),
            None => {
                self.diagnostics.add(
                    accessor.ident_id,
                    DiagnosticError::UndefinedProperty {
                        value_id: accessor.value.id,
                        r#type: value_type_view.to_string(),
                        ident: accessor.ident.clone(),
                    },
                );
                Type::Error
            }
        }
    }

    fn typecheck_call(&mut self, id: SyntaxId, call: &mut Call, expected: Option<Type>) -> Type {
        self.infer_expression(&mut call.function);

        let TypeView::FunctionItem(function_type) = call.function.r#type.as_view(&self.ty_ctx)
        else {
            self.diagnostics.add(
                id,
                DiagnosticError::TypeMismatch {
                    expected: "function".into(),
                    got: call.function.r#type.as_view(&self.ty_ctx).to_string(),
                },
            );
            return Type::Error;
        };
        let return_type = function_type.value.return_type;

        // Small hack while intrinsics are more powerful than user defined functions.
        // E.g. `arrayOf` is variadic
        if let FunctionReference::Intrinsic(intrinsic) = function_type.value.reference {
            return self.typecheck_intrinsic(id, intrinsic, &mut call.arguments, expected);
        }

        if let FunctionParameters::Constrained {
            parameters,
            parameters_reference,
        } = function_type.value.parameters.clone()
        {
            if parameters.len() != call.arguments.len() {
                self.diagnostics.add(
                    id,
                    DiagnosticError::MismatchedArgumentCount {
                        expected: parameters.len(),
                        got: call.arguments.len(),
                        parameter_def: Some(parameters_reference),
                    },
                );
            }

            for (expected, actual) in parameters.into_iter().zip(call.arguments.iter_mut()) {
                self.check_expression(actual, expected);
            }
        }

        return_type
    }

    /// Handles both inference and checking because I am lazy
    fn typecheck_intrinsic(
        &mut self,
        id: SyntaxId,
        intrinsic: Intrinsic,
        arguments: &mut [AirExpression],
        expected: Option<Type>,
    ) -> Type {
        match intrinsic {
            Intrinsic::Print => self.typecheck_intrinsic_print(id, arguments),
            Intrinsic::UnsafeTransmute => {
                self.typecheck_intrinsic_transmute(id, arguments, expected)
            }
            Intrinsic::ArrayOf => match expected {
                Some(expected) => {
                    self.check_intrinsic_array_of(id, arguments, expected);
                    expected
                }
                None => self.infer_intrinsic_array_of(id, arguments),
            },
            Intrinsic::ArrayOfZeros => {
                self.typecheck_intrinsic_array_of_zeros(id, arguments, expected)
            }
            Intrinsic::ArrayGet => self.typecheck_intrinsic_array_get(id, arguments, expected),
            Intrinsic::ArraySet => self.typecheck_intrinsic_array_set(id, arguments),
            Intrinsic::ArrayLen => self.typecheck_intrinsic_array_len(id, arguments),
        }
    }

    fn typecheck_intrinsic_print(&mut self, id: SyntaxId, arguments: &mut [AirExpression]) -> Type {
        let [first] = arguments else {
            self.diagnostics.add(
                id,
                DiagnosticError::MismatchedArgumentCount {
                    expected: 1,
                    got: arguments.len(),
                    parameter_def: None,
                },
            );
            return Type::Unit;
        };
        self.infer_expression(first);
        Type::Unit
    }

    fn typecheck_intrinsic_transmute(
        &mut self,
        id: SyntaxId,
        arguments: &mut [AirExpression],
        expected: Option<Type>,
    ) -> Type {
        let [value] = arguments else {
            self.diagnostics.add(
                id,
                DiagnosticError::MismatchedArgumentCount {
                    expected: 1,
                    got: arguments.len(),
                    parameter_def: None,
                },
            );
            return Type::Error;
        };
        self.infer_expression(value);
        let Some(expected) = expected else {
            self.diagnostics.add(id, DiagnosticError::InferenceFailed);
            return Type::Error;
        };

        expected
    }

    fn check_intrinsic_array_of(
        &mut self,
        id: SyntaxId,
        arguments: &mut [AirExpression],
        expected: Type,
    ) {
        let TypeView::Array(array) = expected.as_view(&self.ty_ctx) else {
            let array_type = self.ty_ctx.array_of(id, Type::Top).as_view(&self.ty_ctx);
            self.diagnostics.add(
                id,
                DiagnosticError::TypeMismatch {
                    expected: array_type.to_string(),
                    got: expected.as_view(&self.ty_ctx).to_string(),
                },
            );
            return;
        };
        let element_type = array.element_type;
        for argument in arguments {
            self.check_expression(argument, element_type);
        }
    }

    fn infer_intrinsic_array_of(&mut self, id: SyntaxId, arguments: &mut [AirExpression]) -> Type {
        let [first, rest @ ..] = arguments else {
            self.diagnostics.add(
                id,
                DiagnosticError::MismatchedArgumentCount {
                    expected: 1,
                    got: 0,
                    parameter_def: None,
                },
            );
            return Type::Error;
        };

        self.infer_expression(first);
        let element_type = first.r#type.computed();

        for argument in rest {
            self.check_expression(argument, element_type);
        }

        self.ty_ctx.array_of(id, element_type)
    }

    fn typecheck_intrinsic_array_of_zeros(
        &mut self,
        id: SyntaxId,
        arguments: &mut [AirExpression],
        expected: Option<Type>,
    ) -> Type {
        let [count] = arguments else {
            self.diagnostics.add(
                id,
                DiagnosticError::MismatchedArgumentCount {
                    expected: 1,
                    got: arguments.len(),
                    parameter_def: None,
                },
            );
            return self.ty_ctx.array_of(id, Type::Error);
        };

        self.check_expression(count, Type::I32);

        let Some(expected) = expected else {
            self.diagnostics.add(id, DiagnosticError::InferenceFailed);
            return self.ty_ctx.array_of(id, Type::Error);
        };
        if expected.as_view(&self.ty_ctx).is_abstract() {
            self.diagnostics.add(id, DiagnosticError::InferenceFailed);
            return self.ty_ctx.array_of(id, Type::Error);
        }
        let any_array = self.ty_ctx.array_of(id, Type::Top);
        self.expect_type_at(id, expected, any_array);
        expected
    }

    fn typecheck_intrinsic_array_get(
        &mut self,
        id: SyntaxId,
        arguments: &mut [AirExpression],
        expected: Option<Type>,
    ) -> Type {
        let [array, index] = arguments else {
            self.diagnostics.add(
                id,
                DiagnosticError::MismatchedArgumentCount {
                    expected: 2,
                    got: arguments.len(),
                    parameter_def: None,
                },
            );
            return Type::Error;
        };

        let expected = expected.unwrap_or(Type::Top);
        let expected_type = self.ty_ctx.array_of(id, expected);
        self.check_expression(array, expected_type);
        self.check_expression(index, Type::I32);

        let TypeView::Array(array_type) = array.r#type.as_view(&self.ty_ctx) else {
            return Type::Error;
        };

        array_type.element_type
    }

    fn typecheck_intrinsic_array_set(
        &mut self,
        id: SyntaxId,
        arguments: &mut [AirExpression],
    ) -> Type {
        let [array, index, value] = arguments else {
            self.diagnostics.add(
                id,
                DiagnosticError::MismatchedArgumentCount {
                    expected: 3,
                    got: arguments.len(),
                    parameter_def: None,
                },
            );
            return Type::Error;
        };

        let expected_type = self.ty_ctx.array_of(id, Type::Top);
        self.check_expression(array, expected_type);
        self.check_expression(index, Type::I32);

        let TypeView::Array(array_type) = array.r#type.as_view(&self.ty_ctx) else {
            return Type::Error;
        };
        let element_type = array_type.element_type;

        self.check_expression(value, element_type);

        Type::Unit
    }

    fn typecheck_intrinsic_array_len(
        &mut self,
        id: SyntaxId,
        arguments: &mut [AirExpression],
    ) -> Type {
        let [array] = arguments else {
            self.diagnostics.add(
                id,
                DiagnosticError::MismatchedArgumentCount {
                    expected: 1,
                    got: arguments.len(),
                    parameter_def: None,
                },
            );
            return Type::I32;
        };

        let expected_type = self.ty_ctx.array_of(id, Type::Top);
        self.check_expression(array, expected_type);

        Type::I32
    }
}
