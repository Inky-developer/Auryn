use std::collections::VecDeque;

use stdx::{FastMap, FastSet, Graph, SmallString, default};

use crate::auryn::{
    air::{
        data::{
            Accessor, Air, AirBlock, AirBlockFinalizer, AirBlockId, AirConstant, AirExpression,
            AirExpressionKind, AirFunction, AirFunctionId, AirGenericArguments, AirLocalValueId,
            AirNode, AirNodeKind, AirPlaceKind, AirStaticValue, AirStaticValueId, AirType,
            AirValueId, Assignment, Call, Globals, Intrinsic, ReturnValue, TypeAliasId,
            UnaryOperator, Update,
        },
        namespace::UserDefinedTypeId,
        typecheck::{
            bounds::{Bound, BoundView, MaybeBounded},
            generics::GenericInference,
            resolver::{self, Resolver, ResolverError, ResolverResult},
            type_context::{TypeContext, TypeId},
            types::{
                FunctionItemType, FunctionParameters, GenericType, StructuralType, Type, TypeView,
            },
        },
        unresolved_type::UnresolvedType,
    },
    diagnostics::{
        diagnostic::Diagnostics,
        errors::{
            CircularTypeAlias, ExpectedFunction, ExpectedStruct, InferenceFailed, InvalidCast,
            MismatchedArgumentCount, MissingFields, TypeMismatch, UndefinedProperty,
            UnexpectedFields, UnsupportedOperationWithType, ValueOutsideRange,
        },
    },
    syntax_id::{Spanned, SyntaxId},
    tokenizer::BinaryOperatorToken,
};

pub fn typecheck_air(globals: Globals, diagnostics: Diagnostics) -> (Air, Diagnostics) {
    Typechecker::new(diagnostics).infer(globals)
}

#[derive(Debug)]
struct FunctionContext {
    type_variables: Vec<GenericType>,
    variables: FastMap<AirLocalValueId, Type>,
    return_type: Type,
}

impl Default for FunctionContext {
    fn default() -> Self {
        Self {
            type_variables: default(),
            variables: default(),
            return_type: Type::Error,
        }
    }
}

impl FunctionContext {
    fn clear(&mut self, expected_return_type: Type) {
        let FunctionContext {
            variables,
            type_variables,
            return_type,
        } = self;
        variables.clear();
        type_variables.clear();
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
    type_aliasses: FastMap<TypeAliasId, AirType>,
    resolver: Resolver,
    ty_ctx: TypeContext,
    diagnostics: Diagnostics,
}

impl Typechecker {
    pub fn new(diagnostics: Diagnostics) -> Self {
        Self {
            functions: default(),
            function: default(),
            statics: default(),
            type_aliasses: default(),
            ty_ctx: default(),
            resolver: default(),
            diagnostics,
        }
    }

    pub fn infer(mut self, mut globals: Globals) -> (Air, Diagnostics) {
        self.statics = std::mem::take(&mut globals.statics);

        {
            let type_aliasses = std::mem::take(&mut globals.type_aliases);
            let alias_order = order_aliasses(&type_aliasses);
            self.type_aliasses = type_aliasses;
            for alias_id in alias_order {
                let mut air_ty = self.type_aliasses.remove(&alias_id).unwrap();
                if let AirType::Unresolved(unresolved) = &air_ty {
                    let ty = match self.try_resolve_type(unresolved) {
                        Ok(ty) => ty,
                        Err(ResolverError::CircularTypeAlias(circular_type_id)) => {
                            self.diagnostics.add(
                                alias_id.0,
                                CircularTypeAlias {
                                    circular_type_alias: circular_type_id,
                                },
                            );
                            Type::Error
                        }
                    };
                    air_ty = AirType::Computed(ty);
                }
                self.type_aliasses.insert(alias_id, air_ty);
            }
        }
        self.compute_defined_types(&mut globals);
        self.infer_functions(&mut globals);

        globals.statics = self.statics;
        globals.type_aliases = self.type_aliasses;

        let air = Air {
            globals,
            ty_ctx: self.ty_ctx,
        };
        (air, self.diagnostics)
    }

    fn compute_defined_types(&mut self, globals: &mut Globals) {
        for r#type in globals.types.values_mut() {
            self.resolve_if_unresolved(r#type);
        }

        for (id, function) in &mut globals.functions {
            self.infer_function_signature(*id, function);
        }
    }

    fn infer_functions(&mut self, globals: &mut Globals) {
        for function in globals.functions.values_mut() {
            self.infer_function_body(function);
        }
    }

    #[track_caller]
    fn expect_type_at(&mut self, at: SyntaxId, received: Type, expected: MaybeBounded) {
        if expected
            .as_view(&self.ty_ctx)
            .contains(received.as_view(&self.ty_ctx))
        {
            return;
        }

        self.diagnostics.add(
            at,
            TypeMismatch {
                expected,
                got: received,
            },
        );
    }
}

impl Typechecker {
    fn try_resolve_type(&mut self, unresolved: &UnresolvedType) -> ResolverResult {
        let mut ctx = resolver::Context {
            ty_ctx: &mut self.ty_ctx,
            diagnostics: &mut self.diagnostics,
            statics: &self.statics,
            type_aliasses: &self.type_aliasses,
            type_parameters: &self.function.type_variables,
        };
        self.resolver.resolve_type(&mut ctx, unresolved)
    }

    fn resolve_type(&mut self, unresolved: &UnresolvedType) -> Type {
        self.try_resolve_type(unresolved).expect("Should not fail")
    }

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
        self.function
            .type_variables
            .extend(function_type.type_parameters.iter().cloned());
        for (id, r#type) in function
            .argument_ids()
            .zip(function_type.value.parameters().iter())
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
                    self.check_expression(expression, self.function.return_type.as_bounded());
                }
                ReturnValue::Null(id) => {
                    self.expect_type_at(
                        *id,
                        self.ty_ctx.unit_type(),
                        self.function.return_type.as_bounded(),
                    );
                }
            },
            AirBlockFinalizer::Goto(next_id) => on_next_id(*next_id),
            AirBlockFinalizer::Branch {
                value,
                pos_block,
                neg_block,
            } => {
                self.check_expression(value, Type::Bool.as_bounded());
                on_next_id(*pos_block);
                on_next_id(*neg_block);
            }
        }
    }

    fn infer_node(&mut self, node: &mut AirNode) {
        match &mut node.kind {
            AirNodeKind::Assignment(assignment) => self.infer_assignment(assignment),
            AirNodeKind::Update(update) => self.infer_update(update),
            AirNodeKind::Expression(expression) => self.infer_expression(expression),
        }
    }

    fn infer_assignment(&mut self, assignment: &mut Assignment) {
        if let Some(expected_type) = &assignment.expected_type {
            let expected_type = self.resolve_type(expected_type);
            self.check_expression(&mut assignment.expression, expected_type.as_bounded());
        } else {
            self.infer_expression(&mut assignment.expression);
        }
        self.function
            .variables
            .insert(assignment.target, assignment.expression.r#type.computed());
    }

    fn infer_update(&mut self, update: &mut Update) {
        let expected_type = match &mut update.target.kind {
            AirPlaceKind::Variable(id) => *self.function.variables.get(&*id).unwrap(),
            AirPlaceKind::Accessor(accessor) => self.infer_accessor(accessor),
        };
        update.target.r#type = AirType::Computed(expected_type);
        self.check_expression(&mut update.expression, expected_type.as_bounded());
        if let Some(op) = update.operator.to_binary_operator() {
            self.infer_binary_operator(
                &mut AirExpression {
                    id: update.target.id,
                    r#type: AirType::Computed(expected_type),
                    kind: AirExpressionKind::Synthetic,
                },
                &mut update.expression,
                op,
            );
        }
    }

    fn infer_expression(&mut self, expression: &mut AirExpression) {
        let expression_type = match &mut expression.kind {
            AirExpressionKind::Constant(constant) => self.infer_constant(expression.id, constant),
            AirExpressionKind::BinaryOperator(binary_operator) => self.infer_binary_operator(
                &mut binary_operator.lhs,
                &mut binary_operator.rhs,
                binary_operator.operator,
            ),
            AirExpressionKind::UnaryOperator(unary_operator) => {
                self.infer_unary_operator(&mut unary_operator.expression, unary_operator.operator)
            }
            AirExpressionKind::Variable(value) => self.infer_value(value),
            AirExpressionKind::Type(r#type) => {
                self.resolve_if_unresolved(r#type);
                self.ty_ctx.meta_of(r#type.computed())
            }
            AirExpressionKind::Accessor(accessor) => self.infer_accessor(accessor),
            AirExpressionKind::Call(call) => self.typecheck_call(expression.id, call, None),
            // Synthetic expressions are only generated during type checking and already have their type computed
            AirExpressionKind::Synthetic => expression.r#type.computed(),
            AirExpressionKind::Error(_) => Type::Error,
        };

        expression.r#type = AirType::Computed(expression_type);
    }

    fn check_expression(&mut self, expression: &mut AirExpression, expected: MaybeBounded) {
        let inferred_type = match &mut expression.kind {
            AirExpressionKind::Constant(constant) => {
                self.check_constant(expression.id, constant, expected)
            }
            AirExpressionKind::BinaryOperator(binary_operator) => self.infer_binary_operator(
                &mut binary_operator.lhs,
                &mut binary_operator.rhs,
                binary_operator.operator,
            ),
            AirExpressionKind::UnaryOperator(unary_operator) => {
                self.infer_unary_operator(&mut unary_operator.expression, unary_operator.operator)
            }
            AirExpressionKind::Variable(variable) => self.infer_value(variable),
            AirExpressionKind::Type(r#type) => {
                self.resolve_if_unresolved(r#type);
                self.ty_ctx.meta_of(r#type.computed())
            }
            AirExpressionKind::Accessor(accessor) => self.infer_accessor(accessor),
            AirExpressionKind::Call(call) => {
                self.typecheck_call(expression.id, call, Some(expected))
            }
            // Synthetic expressions are only generated during type checking and already have their type computed
            AirExpressionKind::Synthetic => expression.r#type.computed(),
            AirExpressionKind::Error(_) => Type::Error,
        };
        self.expect_type_at(expression.id, inferred_type, expected);
        expression.r#type = AirType::Computed(inferred_type);
    }

    fn infer_constant(&mut self, id: SyntaxId, constant: &mut AirConstant) -> Type {
        match constant {
            AirConstant::Number(value) => self.ty_ctx.number_literal_of(*value),
            AirConstant::Boolean(_) => Type::Bool,
            AirConstant::String(_) => Type::String,
            AirConstant::StructLiteral {
                struct_type: Some((struct_ident_id, struct_type)),
                fields,
            } => {
                self.resolve_if_unresolved(struct_type);
                let Type::Struct(struct_id) = struct_type.computed() else {
                    self.diagnostics.add(
                        *struct_ident_id,
                        ExpectedStruct {
                            got: struct_type.computed(),
                        },
                    );
                    return Type::Error;
                };

                self.check_struct_literal(
                    id,
                    |ty_ctx| {
                        (
                            &ty_ctx.get(struct_id).structural,
                            Some(struct_id.syntax_id()),
                        )
                    },
                    fields,
                );
                struct_type.computed()
            }
            AirConstant::StructLiteral {
                struct_type: None,
                fields: struct_literal,
            } => {
                let types = struct_literal
                    .iter_mut()
                    .map(|(ident, expr)| {
                        self.infer_expression(expr);
                        (ident.clone(), expr.r#type.computed())
                    })
                    .collect::<Vec<_>>();
                self.ty_ctx.structural_of(StructuralType { fields: types })
            }
        }
    }

    fn check_constant(
        &mut self,
        id: SyntaxId,
        constant: &mut AirConstant,
        expected: MaybeBounded,
    ) -> Type {
        match constant {
            AirConstant::Number(value) => {
                let default_type = Type::I32;
                let expected = match expected {
                    MaybeBounded::Type(ty) => ty,
                    // Lets just use I32 as a default type for numbers right now.
                    // Note that this is different to what is returned for Bound::Top, because Bound::Number
                    // requires the ability to do runtime calculations, which Type::NumberLiteral does not support.
                    MaybeBounded::Bounded(Bound::Number) => default_type,
                    // If the bound is not strong enough, we just fall back to inference
                    _ => return self.infer_constant(id, constant),
                };
                if !BoundView::Number.contains(expected.as_view(&self.ty_ctx)) {
                    self.diagnostics.add(
                        id,
                        TypeMismatch {
                            got: default_type,
                            expected: expected.as_bounded(),
                        },
                    );
                }
                if let TypeView::NumberLiteral(expected_value) = expected.as_view(&self.ty_ctx)
                    && expected_value.value.value != *value
                {
                    let expected = TypeView::NumberLiteral(expected_value)
                        .as_type()
                        .as_bounded();
                    let got = self.ty_ctx.number_literal_of(*value);
                    self.diagnostics.add(id, TypeMismatch { expected, got });
                }

                if let Some(value_range) = expected.int_value_range()
                    && !value_range.contains(value)
                {
                    self.diagnostics.add(
                        id,
                        ValueOutsideRange {
                            range: value_range,
                            got: *value,
                            r#type: expected,
                        },
                    );
                }

                expected
            }
            AirConstant::String(_) => Type::String,
            AirConstant::Boolean(_) => Type::Bool,
            // For named structs the type cannot change, so lets just infer it
            AirConstant::StructLiteral {
                struct_type: Some(_),
                ..
            } => self.infer_constant(id, constant),
            AirConstant::StructLiteral {
                struct_type: None,
                fields: struct_literal,
            } => {
                let MaybeBounded::Type(Type::Structural(structural_id)) = expected else {
                    // the error will be created in check_expression
                    return self.infer_constant(id, constant);
                };

                self.check_struct_literal(
                    id,
                    |ty_ctx| (ty_ctx.get(structural_id), None),
                    struct_literal,
                );
                Type::Structural(structural_id)
            }
        }
    }

    fn check_struct_literal(
        &mut self,
        id: SyntaxId,
        get_expected: impl FnOnce(&TypeContext) -> (&StructuralType, Option<SyntaxId>),
        got: &mut Vec<(Spanned<SmallString>, AirExpression)>,
    ) {
        let (expected, expected_def) = get_expected(&self.ty_ctx);
        if let Err(()) = check_fields_equal(
            &mut self.diagnostics,
            id,
            expected_def,
            expected.fields.iter().map(|(name, _)| name),
            got.iter().map(|(name, _)| name),
        ) {
            return;
        }

        let expected_types = expected.fields.iter().cloned().collect::<FastMap<_, _>>();
        for (name, expression) in got {
            let expected_type = *expected_types.get(name.as_ref()).unwrap();
            self.check_expression(expression, MaybeBounded::Type(expected_type));
        }
    }

    fn infer_binary_operator(
        &mut self,
        lhs: &mut AirExpression,
        rhs: &mut AirExpression,
        operator: BinaryOperatorToken,
    ) -> Type {
        let parameter_bound = match operator {
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
            | BinaryOperatorToken::LessOrEqual => Bound::Number.as_bounded(),
            BinaryOperatorToken::And | BinaryOperatorToken::Or => Type::Bool.as_bounded(),
        };

        self.check_expression(lhs, parameter_bound);
        self.check_expression(rhs, lhs.r#type.computed().as_bounded());

        match operator {
            BinaryOperatorToken::Plus
            | BinaryOperatorToken::Minus
            | BinaryOperatorToken::Times
            | BinaryOperatorToken::Divide
            | BinaryOperatorToken::Remainder => {
                let result_type = lhs.r#type.computed();
                if matches!(result_type, Type::NumberLiteral(_)) {
                    self.diagnostics.add(
                        lhs.id,
                        UnsupportedOperationWithType {
                            operation: operator.to_token_kind().as_str().into(),
                            r#type: result_type,
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

    fn infer_unary_operator(&mut self, expression: &mut AirExpression, op: UnaryOperator) -> Type {
        let UnaryOperator::Not = op;
        self.check_expression(expression, Type::Bool.as_bounded());
        Type::Bool
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
            AirValueId::Intrinsic(intrinsic) => intrinsic.r#type(&mut self.ty_ctx),
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
                    UndefinedProperty {
                        value_id: accessor.value.id,
                        r#type: value_type_view.as_type(),
                        ident: accessor.ident.clone(),
                    },
                );
                Type::Error
            }
        }
    }

    fn typecheck_call(
        &mut self,
        id: SyntaxId,
        call: &mut Call,
        expected: Option<MaybeBounded>,
    ) -> Type {
        self.infer_expression(&mut call.function);
        match call.function.r#type.computed() {
            Type::FunctionItem(function_type) => {
                self.typecheck_call_to_function_item(id, call, function_type, expected)
            }
            Type::Intrinsic(intrinsic_type) => self.typecheck_call_to_intrinsic(
                id,
                self.ty_ctx.get_intrinsic(intrinsic_type).intrinsic,
                &mut call.arguments,
                expected,
            ),
            other => {
                self.diagnostics.add(id, ExpectedFunction { got: other });
                Type::Error
            }
        }
    }

    fn typecheck_call_to_function_item(
        &mut self,
        id: SyntaxId,
        call: &mut Call,
        function_type_id: TypeId<FunctionItemType>,
        expected: Option<MaybeBounded>,
    ) -> Type {
        let function_type = self.ty_ctx.get_function_item(function_type_id);
        let num_generic_args = function_type.type_parameters.len();
        let return_type = function_type.return_type;

        let FunctionParameters {
            parameters,
            parameters_reference,
        } = function_type.parameters.clone();
        if parameters.len() != call.arguments.len() {
            self.diagnostics.add(
                id,
                MismatchedArgumentCount {
                    expected: parameters.len(),
                    got: call.arguments.len(),
                    parameter_def: Some(parameters_reference),
                },
            );
        }

        let mut inference = GenericInference::default();

        if let Some(MaybeBounded::Type(ty)) = expected {
            inference.add_inferred(&self.ty_ctx, return_type, ty);
        }

        for (expected, actual) in parameters.into_iter().zip(call.arguments.iter_mut()) {
            let bound = inference.get_bound_for_inference(&self.ty_ctx, expected);
            self.check_expression(actual, bound);
            inference.add_inferred(&self.ty_ctx, expected, actual.r#type.computed());
        }

        let result = inference.get_resolved(&self.ty_ctx, return_type);

        let inferred_args = inference.into_inferred();
        assert_eq!(
            num_generic_args,
            inferred_args.len(),
            "TODO: Add error message that type could not be inferred"
        );
        call.generic_arguments = AirGenericArguments::Computed(inferred_args);

        result
    }

    /// Handles both inference and checking because I am lazy
    fn typecheck_call_to_intrinsic(
        &mut self,
        id: SyntaxId,
        intrinsic: Intrinsic,
        arguments: &mut [AirExpression],
        expected: Option<MaybeBounded>,
    ) -> Type {
        match intrinsic {
            Intrinsic::Print => self.typecheck_intrinsic_print(id, arguments),
            Intrinsic::UnsafeTransmute => {
                self.typecheck_intrinsic_transmute(id, arguments, expected)
            }
            Intrinsic::Cast => self.typecheck_intrinsic_cast(id, arguments, expected),
            Intrinsic::ArrayOf => match expected {
                Some(expected) => self.check_intrinsic_array_of(id, arguments, expected),
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
                MismatchedArgumentCount {
                    expected: 1,
                    got: arguments.len(),
                    parameter_def: None,
                },
            );
            return self.ty_ctx.unit_type();
        };
        self.infer_expression(first);
        self.ty_ctx.unit_type()
    }

    fn typecheck_intrinsic_transmute(
        &mut self,
        id: SyntaxId,
        arguments: &mut [AirExpression],
        expected: Option<MaybeBounded>,
    ) -> Type {
        let [value] = arguments else {
            self.diagnostics.add(
                id,
                MismatchedArgumentCount {
                    expected: 1,
                    got: arguments.len(),
                    parameter_def: None,
                },
            );
            return Type::Error;
        };
        self.infer_expression(value);
        let Some(expected) = expected.and_then(|it| it.as_type()) else {
            self.diagnostics.add(id, InferenceFailed);
            return Type::Error;
        };

        expected
    }

    fn typecheck_intrinsic_cast(
        &mut self,
        id: SyntaxId,
        arguments: &mut [AirExpression],
        expected: Option<MaybeBounded>,
    ) -> Type {
        let Some(MaybeBounded::Type(expected)) = expected else {
            self.diagnostics.add(id, InferenceFailed);
            return Type::Error;
        };

        let [value] = arguments else {
            self.diagnostics.add(
                id,
                MismatchedArgumentCount {
                    expected: 1,
                    got: arguments.len(),
                    parameter_def: None,
                },
            );
            return expected;
        };

        self.infer_expression(value);
        let received = value.r#type.computed();

        if !matches!(
            (received, expected),
            (Type::I32 | Type::Bool, Type::I64) | (Type::I64 | Type::Bool, Type::I32),
        ) {
            self.diagnostics.add(
                id,
                InvalidCast {
                    from: received,
                    to: expected,
                },
            );
        }

        expected
    }

    fn check_intrinsic_array_of(
        &mut self,
        id: SyntaxId,
        arguments: &mut [AirExpression],
        expected: MaybeBounded,
    ) -> Type {
        let Some(expected) = expected.as_type() else {
            return self.infer_intrinsic_array_of(id, arguments);
        };
        let TypeView::Array(array) = expected.as_view(&self.ty_ctx) else {
            let array_type = self.ty_ctx.array_bound_of(Bound::Top);
            self.diagnostics.add(
                id,
                TypeMismatch {
                    expected: MaybeBounded::Bounded(array_type),
                    got: expected,
                },
            );
            return self.ty_ctx.array_of(Type::Error);
        };
        let element_type = array.element_type;
        for argument in arguments {
            self.check_expression(argument, element_type.as_bounded());
        }
        expected
    }

    fn infer_intrinsic_array_of(&mut self, id: SyntaxId, arguments: &mut [AirExpression]) -> Type {
        let [first, rest @ ..] = arguments else {
            self.diagnostics.add(
                id,
                MismatchedArgumentCount {
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
            self.check_expression(argument, element_type.as_bounded());
        }

        self.ty_ctx.array_of(element_type)
    }

    fn typecheck_intrinsic_array_of_zeros(
        &mut self,
        id: SyntaxId,
        arguments: &mut [AirExpression],
        expected: Option<MaybeBounded>,
    ) -> Type {
        let [count] = arguments else {
            self.diagnostics.add(
                id,
                MismatchedArgumentCount {
                    expected: 1,
                    got: arguments.len(),
                    parameter_def: None,
                },
            );
            return self.ty_ctx.array_of(Type::Error);
        };

        self.check_expression(count, Type::I32.as_bounded());

        let Some(expected) = expected.and_then(|it| it.as_type()) else {
            self.diagnostics.add(id, InferenceFailed);
            return self.ty_ctx.array_of(Type::Error);
        };
        let any_array = self.ty_ctx.array_bound_of(Bound::Top);
        self.expect_type_at(id, expected, any_array.as_bounded());
        expected
    }

    fn typecheck_intrinsic_array_get(
        &mut self,
        id: SyntaxId,
        arguments: &mut [AirExpression],
        expected: Option<MaybeBounded>,
    ) -> Type {
        let [array, index] = arguments else {
            self.diagnostics.add(
                id,
                MismatchedArgumentCount {
                    expected: 2,
                    got: arguments.len(),
                    parameter_def: None,
                },
            );
            return Type::Error;
        };

        let element_bound = expected.unwrap_or(Bound::Top.as_bounded());
        let expected_bound = match element_bound {
            MaybeBounded::Bounded(bound) => self.ty_ctx.array_bound_of(bound).as_bounded(),
            MaybeBounded::Type(ty) => self.ty_ctx.array_of(ty).as_bounded(),
        };
        self.check_expression(array, expected_bound);
        self.check_expression(index, Type::I32.as_bounded());

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
                MismatchedArgumentCount {
                    expected: 3,
                    got: arguments.len(),
                    parameter_def: None,
                },
            );
            return Type::Error;
        };

        let expected_bound = self.ty_ctx.array_bound_of(Bound::Top);
        self.check_expression(array, expected_bound.as_bounded());
        self.check_expression(index, Type::I32.as_bounded());

        let TypeView::Array(array_type) = array.r#type.as_view(&self.ty_ctx) else {
            return Type::Error;
        };
        let element_type = array_type.element_type;

        self.check_expression(value, element_type.as_bounded());

        self.ty_ctx.unit_type()
    }

    fn typecheck_intrinsic_array_len(
        &mut self,
        id: SyntaxId,
        arguments: &mut [AirExpression],
    ) -> Type {
        let [array] = arguments else {
            self.diagnostics.add(
                id,
                MismatchedArgumentCount {
                    expected: 1,
                    got: arguments.len(),
                    parameter_def: None,
                },
            );
            return Type::I32;
        };

        let expected_bound = self.ty_ctx.array_bound_of(Bound::Top);
        self.check_expression(array, expected_bound.as_bounded());

        Type::I32
    }
}

/// Because type aliasses can depend on each other (but not recursively),
/// they need to be sorted and then evaluated in an order that they can depend on each other.
fn order_aliasses(aliasses: &FastMap<TypeAliasId, AirType>) -> Vec<TypeAliasId> {
    let mut graph = Graph::default();
    for (id, ty) in aliasses {
        if let AirType::Unresolved(unresolved) = &ty {
            unresolved.visit_contained_types(&mut |ty| {
                if let UnresolvedType::DefinedType(UserDefinedTypeId::TypeAlias(alias)) = ty {
                    graph.connect(*id, *alias);
                }
            });
        }

        graph.insert(*id, ty);
    }

    graph.order_topologically(graph.keys())
}

fn check_fields_equal<'a>(
    diagnostics: &mut Diagnostics,
    id: SyntaxId,
    expected_def_id: Option<SyntaxId>,
    expected: impl Iterator<Item = &'a SmallString> + Clone,
    received: impl Iterator<Item = &'a Spanned<SmallString>> + Clone,
) -> Result<(), ()> {
    let mut did_report_error = false;

    let expected_set = expected.clone().collect::<FastSet<_>>();
    let received_map = received
        .clone()
        .map(|spanned| (&spanned.value, spanned.syntax_id))
        .collect::<FastMap<_, _>>();
    let unexpected_fields = received_map
        .keys()
        .filter(|field| !expected_set.contains(*field))
        .map(|field| (**field).clone())
        .collect::<Vec<_>>();
    if !unexpected_fields.is_empty() {
        let id = if unexpected_fields.len() == 1 {
            received_map[&unexpected_fields[0]]
        } else {
            id
        };
        diagnostics.add(
            id,
            UnexpectedFields {
                unexpected_fields,
                def_id: expected_def_id,
            },
        );
        did_report_error = true;
    }

    let missing_fields = expected_set
        .iter()
        .filter(|field| !received_map.contains_key(*field))
        .map(|field| (*field).clone())
        .collect::<Vec<_>>();
    if !missing_fields.is_empty() {
        diagnostics.add(
            id,
            MissingFields {
                missing_fields,
                def_id: expected_def_id,
            },
        );
        did_report_error = true;
    }

    if did_report_error { Err(()) } else { Ok(()) }
}
