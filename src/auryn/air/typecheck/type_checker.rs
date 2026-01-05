use std::collections::VecDeque;

use crate::{
    auryn::{
        air::{
            data::{
                Accessor, Air, AirBlock, AirBlockFinalizer, AirBlockId, AirConstant, AirExpression,
                AirExpressionKind, AirFunction, AirFunctionId, AirLocalValueId, AirNode,
                AirNodeKind, AirStaticValue, AirStaticValueId, AirType, AirValueId, Assignment,
                BinaryOperation, Call, FunctionReference, Intrinsic, ReturnValue, UnresolvedType,
            },
            namespace::UserDefinedTypeId,
            typecheck::{
                type_context::{TypeContext, TypeId, TypeView},
                types::{
                    ArrayType, ExternType, ExternTypeMember, FunctionParameters, FunctionType, Type,
                },
            },
        },
        diagnostic::{DiagnosticError, Diagnostics},
        syntax_id::SyntaxId,
    },
    utils::{
        default,
        fast_map::{FastMap, FastSet},
    },
};

pub fn typecheck_air(air: &mut Air, diagnostics: Diagnostics) -> Diagnostics {
    Typechecker::new(diagnostics).typecheck(air)
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

#[derive(Debug, Default)]
pub struct Typechecker {
    functions: FastMap<AirFunctionId, TypeId<FunctionType>>,
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

    pub fn typecheck(mut self, air: &mut Air) -> Diagnostics {
        self.compute_defined_types(&mut air.types);
        self.statics = air.statics.clone();

        for (id, function) in &mut air.functions {
            self.typecheck_function_signature(*id, function);
        }

        for function in air.functions.values_mut() {
            self.typecheck_function_body(function);
        }

        air.ty_ctx = self.ty_ctx;

        self.diagnostics
    }

    fn compute_defined_types(&mut self, types: &mut FastMap<UserDefinedTypeId, AirType>) {
        for r#type in types.values_mut() {
            self.resolve_if_unresolved(r#type);
        }
    }

    fn expect_type(&mut self, received: &AirExpression, expected: Type) {
        if let AirType::Computed(got) = &received.r#type
            && got == &expected
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

    fn expect_type_at(&mut self, at: SyntaxId, received: Type, expected: Type) {
        if received == expected {
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

    fn expect_assignable(&mut self, received: &AirExpression, expected: Type) {
        if let AirType::Computed(got) = &received.r#type
            && got.is_subtype(&expected)
        {
            return;
        }

        self.diagnostics.add(
            received.id,
            DiagnosticError::TypeMismatch {
                expected: expected.as_view(&self.ty_ctx).to_string(),
                got: received.r#type.as_view(&self.ty_ctx).to_string(),
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
                    .map_or(Type::Null, |ty| self.resolve_type(ty));
                Type::Function(self.ty_ctx.add_function(
                    reference.as_user_defined().0.0,
                    FunctionType {
                        parameters: FunctionParameters::Constrained {
                            parameters,
                            parameters_reference: *parameters_reference,
                        },
                        return_type,
                        reference: *reference,
                    },
                ))
            }
            UnresolvedType::Array(id, inner) => {
                let element_type = self.resolve_type(inner);
                Type::Array(self.ty_ctx.add_array(*id, ArrayType { element_type }))
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
                        (
                            ident.clone(),
                            ExternTypeMember {
                                extern_name: member.extern_name.clone(),
                                r#type: self.resolve_type(&member.r#type),
                            },
                        )
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

    fn typecheck_function_signature(&mut self, id: AirFunctionId, function: &mut AirFunction) {
        let computed_ty = self.resolve_type(&function.unresolved_type);
        function.r#type = AirType::Computed(computed_ty);
        let Type::Function(function_type) = computed_ty else {
            unreachable!("Should compute a function type for a function!");
        };
        self.functions.insert(id, function_type);
    }

    fn typecheck_function_body(&mut self, function: &mut AirFunction) {
        let TypeView::Function(function_type) = function.r#type.as_view(&self.ty_ctx) else {
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

            self.typecheck_block(block, |next_id| {
                if !visited_blocks.contains(&next_id) && !pending_blocks.contains(&next_id) {
                    pending_blocks.push_back(next_id);
                }
            });
        }
    }

    fn typecheck_block(&mut self, block: &mut AirBlock, on_next_id: impl FnMut(AirBlockId)) {
        for node in &mut block.nodes {
            self.typecheck_node(node);
        }

        self.typecheck_finalizer(&mut block.finalizer, on_next_id);
    }

    fn typecheck_finalizer(
        &mut self,
        finalizer: &mut AirBlockFinalizer,
        mut on_next_id: impl FnMut(AirBlockId),
    ) {
        match finalizer {
            AirBlockFinalizer::Return(expression) => match expression {
                ReturnValue::Expression(expression) => {
                    self.typecheck_expression(expression);
                    self.expect_type(expression, self.function.return_type);
                }
                ReturnValue::Null(id) => {
                    self.expect_type_at(*id, Type::Null, self.function.return_type);
                }
            },
            AirBlockFinalizer::Goto(next_id) => on_next_id(*next_id),
            AirBlockFinalizer::Branch {
                value,
                pos_block,
                neg_block,
            } => {
                self.typecheck_expression(value);
                self.expect_type(value, Type::Number);
                on_next_id(*pos_block);
                on_next_id(*neg_block);
            }
        }
    }

    fn typecheck_node(&mut self, node: &mut AirNode) {
        match &mut node.kind {
            AirNodeKind::Assignment(assignment) => self.typecheck_assignment(assignment),
            AirNodeKind::Expression(expression) => self.typecheck_expression(expression),
        }
    }

    fn typecheck_assignment(&mut self, assignment: &mut Assignment) {
        self.typecheck_expression(&mut assignment.expression);
        if let Some(expected_type) = self.function.variables.get(&assignment.target) {
            self.expect_assignable(&assignment.expression, *expected_type);
        } else {
            self.function
                .variables
                .insert(assignment.target, assignment.expression.r#type.computed());
        }
    }

    fn typecheck_expression(&mut self, expression: &mut AirExpression) {
        let expression_type = match &mut expression.kind {
            AirExpressionKind::Constant(constant) => self.typecheck_constant(constant),
            AirExpressionKind::BinaryOperator(binary_operator) => {
                self.typecheck_binary_operator(binary_operator)
            }
            AirExpressionKind::Variable(value) => self.typecheck_value(value),
            // Maybe we should correctly represent the type of a type, but for now lets just use itself as its type
            AirExpressionKind::Type(r#type) => *r#type,
            AirExpressionKind::Accessor(accessor) => self.typecheck_accessor(accessor),
            AirExpressionKind::Call(call) => self.typecheck_call(expression.id, call),
            AirExpressionKind::Error => Type::Error,
        };

        expression.r#type = AirType::Computed(expression_type);
    }

    fn typecheck_constant(&self, constant: &AirConstant) -> Type {
        match constant {
            AirConstant::Number(_) => Type::Number,
            AirConstant::String(_) => Type::String,
        }
    }

    fn typecheck_binary_operator(&mut self, binary_operator: &mut BinaryOperation) -> Type {
        self.typecheck_expression(&mut binary_operator.lhs);
        self.typecheck_expression(&mut binary_operator.rhs);

        self.expect_type(&binary_operator.lhs, Type::Number);
        self.expect_type(&binary_operator.rhs, Type::Number);

        Type::Number
    }

    fn typecheck_value(&mut self, value: &AirValueId) -> Type {
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
                Type::Function(function)
            }
            AirValueId::Intrinsic(intrinsic) => intrinsic.r#type(),
        }
    }

    fn typecheck_accessor(&mut self, accessor: &mut Accessor) -> Type {
        self.typecheck_expression(&mut accessor.value);
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

    fn typecheck_call(&mut self, id: SyntaxId, call: &mut Call) -> Type {
        self.typecheck_expression(&mut call.function);
        for argument in &mut call.arguments {
            self.typecheck_expression(argument);
        }

        let TypeView::Function(function_type) = call.function.r#type.as_view(&self.ty_ctx) else {
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
            return self.typecheck_intrinsic(id, intrinsic, &mut call.arguments);
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

            for (expected, actual) in parameters.into_iter().zip(call.arguments.iter()) {
                self.expect_assignable(actual, expected);
            }
        }

        return_type
    }

    fn typecheck_intrinsic(
        &mut self,
        id: SyntaxId,
        intrinsic: Intrinsic,
        arguments: &mut [AirExpression],
    ) -> Type {
        match intrinsic {
            Intrinsic::Print => self.typecheck_intrinsic_print(id, arguments),
            Intrinsic::ArrayOf => self.typecheck_intrinsic_array_of(id, arguments),
            Intrinsic::ArrayOfZeros => self.typecheck_intrinsic_array_of_zeros(id, arguments),
            Intrinsic::ArrayGet => self.typecheck_intrinsic_array_get(id, arguments),
            Intrinsic::ArraySet => self.typecheck_intrinsic_array_set(id, arguments),
            Intrinsic::ArrayLen => self.typecheck_intrinsic_array_len(id, arguments),
        }
    }

    fn typecheck_intrinsic_print(&mut self, id: SyntaxId, arguments: &[AirExpression]) -> Type {
        if arguments.len() != 1 {
            self.diagnostics.add(
                id,
                DiagnosticError::MismatchedArgumentCount {
                    expected: 1,
                    got: arguments.len(),
                    parameter_def: None,
                },
            );
        }
        Type::Null
    }

    fn typecheck_intrinsic_array_of(&mut self, id: SyntaxId, arguments: &[AirExpression]) -> Type {
        // Currently an array needs at least one element or we cannot compute its type
        let [first, ..] = arguments else {
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

        let element_type = first.r#type.computed();

        for argument in arguments {
            self.expect_type(argument, element_type);
        }

        Type::Array(self.ty_ctx.add_array(id, ArrayType { element_type }))
    }

    fn typecheck_intrinsic_array_of_zeros(
        &mut self,
        id: SyntaxId,
        arguments: &[AirExpression],
    ) -> Type {
        let return_type = Type::Array(self.ty_ctx.add_array(
            id,
            ArrayType {
                element_type: Type::Number,
            },
        ));
        let [count] = arguments else {
            self.diagnostics.add(
                id,
                DiagnosticError::MismatchedArgumentCount {
                    expected: 1,
                    got: arguments.len(),
                    parameter_def: None,
                },
            );
            return return_type;
        };

        self.expect_type(count, Type::Number);

        return_type
    }

    fn typecheck_intrinsic_array_get(&mut self, id: SyntaxId, arguments: &[AirExpression]) -> Type {
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

        let TypeView::Array(array_type) = array.r#type.as_view(&self.ty_ctx) else {
            self.diagnostics.add(
                array.id,
                DiagnosticError::TypeMismatch {
                    expected: "array".into(),
                    got: array.r#type.as_view(&self.ty_ctx).to_string(),
                },
            );
            return Type::Error;
        };
        let element_type = array_type.element_type;

        self.expect_type(index, Type::Number);
        element_type
    }

    fn typecheck_intrinsic_array_set(&mut self, id: SyntaxId, arguments: &[AirExpression]) -> Type {
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

        let TypeView::Array(array_type) = array.r#type.as_view(&self.ty_ctx) else {
            self.diagnostics.add(
                array.id,
                DiagnosticError::TypeMismatch {
                    expected: "array".into(),
                    got: array.r#type.as_view(&self.ty_ctx).to_string(),
                },
            );
            return Type::Error;
        };
        let element_type = array_type.element_type;
        self.expect_type(index, Type::Number);
        self.expect_type(value, element_type);

        Type::Null
    }

    fn typecheck_intrinsic_array_len(&mut self, id: SyntaxId, arguments: &[AirExpression]) -> Type {
        let [array] = arguments else {
            self.diagnostics.add(
                id,
                DiagnosticError::MismatchedArgumentCount {
                    expected: 1,
                    got: arguments.len(),
                    parameter_def: None,
                },
            );
            return Type::Number;
        };

        if !matches!(array.r#type.computed(), Type::Array(_)) {
            self.diagnostics.add(
                array.id,
                DiagnosticError::TypeMismatch {
                    expected: "array".into(),
                    got: array.r#type.as_view(&self.ty_ctx).to_string(),
                },
            );
            return Type::Number;
        }

        Type::Number
    }
}
