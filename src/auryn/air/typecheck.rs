use crate::{
    auryn::{
        air::{
            data::{
                Air, AirBlock, AirBlockFinalizer, AirConstant, AirExpression, AirExpressionKind,
                AirNode, AirNodeKind, AirType, AirValueId, Assignment, BinaryOperation,
                IntrinsicCall,
            },
            types::Type,
        },
        diagnostic::{Diagnostic, DiagnosticError, DiagnosticKind},
        syntax_id::SyntaxId,
    },
    utils::fast_map::FastMap,
};

pub fn typecheck_air(air: &mut Air) -> Vec<Diagnostic> {
    Typechecker::default().typecheck(air)
}

#[derive(Debug, Default)]
pub struct Typechecker {
    variables: FastMap<AirValueId, Type>,
    diagnostics: Vec<Diagnostic>,
}

impl Typechecker {
    pub fn typecheck(mut self, air: &mut Air) -> Vec<Diagnostic> {
        for block in air.blocks.values_mut() {
            self.typecheck_block(block);
        }

        self.diagnostics
    }

    fn expect_type(&mut self, received: &AirExpression, expected: Type) {
        if let AirType::Computed(got) = &received.r#type
            && got == &expected
        {
            return;
        }

        self.add_error(
            received.id,
            DiagnosticError::TypeMismatch {
                expected,
                got: received.r#type.clone(),
            },
        )
    }

    fn add_error(&mut self, id: SyntaxId, error: DiagnosticError) {
        self.diagnostics
            .push(Diagnostic::new(id, DiagnosticKind::Error(error)))
    }
}

impl Typechecker {
    fn typecheck_block(&mut self, block: &mut AirBlock) {
        for node in &mut block.nodes {
            self.typecheck_node(node);
        }

        self.typecheck_finalizer(&mut block.finalizer);
    }

    fn typecheck_finalizer(&mut self, finalizer: &mut AirBlockFinalizer) {
        match finalizer {
            AirBlockFinalizer::Return => {}
            AirBlockFinalizer::Goto(_) => {}
            AirBlockFinalizer::Branch { value, .. } => {
                self.typecheck_expression(value);
                self.expect_type(value, Type::Number);
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
        if let Some(expected_type) = self.variables.get(&assignment.target) {
            self.expect_type(&assignment.expression, expected_type.clone());
        } else {
            self.variables.insert(
                assignment.target,
                assignment.expression.r#type.computed().clone(),
            );
        }
    }

    fn typecheck_expression(&mut self, expression: &mut AirExpression) {
        let expression_type = match &mut expression.kind {
            AirExpressionKind::Constant(constant) => self.typecheck_constant(constant),
            AirExpressionKind::BinaryOperator(binary_operator) => {
                self.typecheck_binary_operator(binary_operator)
            }
            AirExpressionKind::Variable(value) => self.typecheck_value(value),
            AirExpressionKind::IntrinsicCall(intrinsic) => {
                self.typecheck_intrinsic(expression.id, intrinsic)
            }
            AirExpressionKind::Error => Type::Error,
        };

        expression.r#type = AirType::Computed(expression_type);
    }

    fn typecheck_constant(&self, constant: &AirConstant) -> Type {
        match constant {
            AirConstant::Number(_) => Type::Number,
        }
    }

    fn typecheck_binary_operator(&mut self, binary_operator: &mut BinaryOperation) -> Type {
        self.typecheck_expression(&mut binary_operator.lhs);
        self.typecheck_expression(&mut binary_operator.rhs);

        self.expect_type(&binary_operator.lhs, Type::Number);
        self.expect_type(&binary_operator.rhs, Type::Number);

        Type::Number
    }

    fn typecheck_value(&self, value: &AirValueId) -> Type {
        let computed_type = self
            .variables
            .get(value)
            .expect("Should have type for value");
        computed_type.clone()
    }

    fn typecheck_intrinsic(&mut self, id: SyntaxId, intrinsic: &mut IntrinsicCall) -> Type {
        for argument in &mut intrinsic.arguments {
            self.typecheck_expression(argument);
        }

        let signature = intrinsic.intrinsic.signature();
        if signature.0.len() != intrinsic.arguments.len() {
            self.add_error(
                id,
                DiagnosticError::MismatchedParameterCount {
                    expected: signature.0.len(),
                    got: intrinsic.arguments.len(),
                },
            );
        }

        for (expected, actual) in signature.0.iter().zip(intrinsic.arguments.iter()) {
            self.expect_type(actual, expected.clone());
        }

        Type::Null
    }
}
