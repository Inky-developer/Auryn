use stdx::{FastIndexSet, FastMap, FastSet};

use crate::auryn::air::{
    data::{
        Accessor, Air, AirBlock, AirBlockFinalizer, AirBlockId, AirConstant, AirExpression,
        AirExpressionKind, AirFunction, AirFunctionId, AirGenericArguments, AirNode, AirNodeKind,
        AirPlace, AirPlaceKind, AirType, Assignment, BinaryOperation, Call, CallKind,
        FunctionReference, Globals, ReturnValue, UnaryOperation, Update,
    },
    typecheck::{
        bounds::MaybeBounded,
        generics::GenericInference,
        type_context::{TypeContext, TypeId},
        types::{FunctionItemType, FunctionParameters, Type},
    },
};

/// A function where all generic arguments are removed
pub struct Monomorphization {
    pub function_id: AirFunctionId,
    pub blocks: FastMap<AirBlockId, AirBlock>,
    pub ty: TypeId<FunctionItemType>,
}

pub type Monomorphizations = Vec<Monomorphization>;

/// Takes all functions in the air and resolves the generics.
/// If a function is called multiple times with different generic arguments,
/// multiple different monomorphizations will be created for it.
pub fn monomorphize(air: &mut Air) -> Monomorphizations {
    let main_function = air.main_function().0;
    let Air { globals, ty_ctx } = air;

    let mut done = Vec::new();
    let mut queue = Queue::default();
    queue.add(main_function, Vec::new());
    while let Some((id, args)) = queue.next() {
        let mono = monomorphize_function(&mut queue, ty_ctx, globals, id, args);
        done.push(mono);
    }
    done
}

#[derive(Debug, Default)]
struct Queue {
    pending: FastIndexSet<(AirFunctionId, Vec<Type>)>,
    completed: FastSet<(AirFunctionId, Vec<Type>)>,
}

impl Queue {
    pub fn next(&mut self) -> Option<(AirFunctionId, Vec<Type>)> {
        self.pending.pop()
    }

    pub fn add(&mut self, id: AirFunctionId, args: Vec<Type>) {
        let mono = (id, args);
        if !self.completed.contains(&mono) {
            self.completed.insert(mono.clone());
            self.pending.insert(mono);
        }
    }
}

fn monomorphize_function(
    queue: &mut Queue,
    ty_ctx: &mut TypeContext,
    globals: &Globals,
    id: AirFunctionId,
    args: Vec<Type>,
) -> Monomorphization {
    let mut builder = FunctionBuilder {
        queue,
        ty_ctx,
        inference: GenericInference::from_already_inferred(args),
    };
    let (blocks, ty) = builder.build(&globals.functions[&id]);
    Monomorphization {
        function_id: id,
        blocks,
        ty,
    }
}

#[derive(Debug)]
struct FunctionBuilder<'a> {
    queue: &'a mut Queue,
    ty_ctx: &'a mut TypeContext,
    inference: GenericInference,
}

impl FunctionBuilder<'_> {
    pub fn build(
        &mut self,
        function: &AirFunction,
    ) -> (FastMap<AirBlockId, AirBlock>, TypeId<FunctionItemType>) {
        let blocks = function
            .blocks
            .iter()
            .map(|(id, block)| (*id, self.transform_block(block)))
            .collect();

        let Type::FunctionItem(ty) = function.r#type.computed() else {
            unreachable!("Invalid function type")
        };
        let ty = self.resolve_function_type(ty);

        (blocks, ty)
    }

    fn resolve_type(&mut self, ty: Type) -> Type {
        {
            match self.inference.resolve_generic_type(self.ty_ctx, ty) {
                MaybeBounded::Type(val) => val,
                MaybeBounded::Bounded(bound) => panic!(
                    "Could not resolve {}, best I could do is infer this bound: {}",
                    ty.as_view(self.ty_ctx),
                    bound.as_view(self.ty_ctx)
                ),
            }
        }
    }

    fn resolve(&mut self, air_type: &AirType) -> AirType {
        let ty = air_type.computed();
        AirType::Computed(self.resolve_type(ty))
    }

    fn resolve_function_type(&mut self, ty: TypeId<FunctionItemType>) -> TypeId<FunctionItemType> {
        let FunctionItemType {
            type_parameters: _,
            parameters,
            return_type,
            reference,
        } = self.ty_ctx.get(ty).clone();
        let parameters = FunctionParameters {
            parameters_reference: parameters.parameters_reference,
            parameters: parameters
                .parameters
                .iter()
                .map(|ty| self.resolve_type(*ty))
                .collect(),
        };
        let function = FunctionItemType {
            type_parameters: Vec::new(),
            parameters,
            return_type: self.resolve_type(return_type),
            reference: match reference {
                FunctionReference::UserDefined(air_function_id) => {
                    FunctionReference::UserDefined(air_function_id)
                }
                FunctionReference::Extern {
                    parent,
                    kind,
                    extern_name,
                    syntax_id,
                } => FunctionReference::Extern {
                    parent: Box::new(AirType::Computed(parent.computed())),
                    kind,
                    extern_name: extern_name.clone(),
                    syntax_id,
                },
            },
        };
        self.ty_ctx.add(None, function)
    }

    fn transform_block(&mut self, block: &AirBlock) -> AirBlock {
        let nodes = block
            .nodes
            .iter()
            .map(|node| self.transform_node(node))
            .collect();
        let finalizer = self.transform_finalizer(&block.finalizer);
        AirBlock { nodes, finalizer }
    }

    fn transform_finalizer(&mut self, finalizer: &AirBlockFinalizer) -> AirBlockFinalizer {
        match finalizer {
            AirBlockFinalizer::Return(return_value) => {
                AirBlockFinalizer::Return(match return_value {
                    ReturnValue::Expression(air_expression) => {
                        ReturnValue::Expression(Box::new(self.transform_expression(air_expression)))
                    }
                    ReturnValue::Null(syntax_id) => ReturnValue::Null(*syntax_id),
                })
            }
            AirBlockFinalizer::Goto(air_block_id) => AirBlockFinalizer::Goto(*air_block_id),
            AirBlockFinalizer::Branch {
                value,
                pos_block,
                neg_block,
            } => AirBlockFinalizer::Branch {
                value: Box::new(self.transform_expression(value)),
                pos_block: *pos_block,
                neg_block: *neg_block,
            },
        }
    }

    fn transform_node(&mut self, node: &AirNode) -> AirNode {
        let kind = match &node.kind {
            AirNodeKind::Assignment(assignment) => {
                AirNodeKind::Assignment(self.transform_assignment(assignment))
            }
            AirNodeKind::Update(update) => AirNodeKind::Update(self.transform_update(update)),
            AirNodeKind::Expression(air_expression) => {
                AirNodeKind::Expression(Box::new(self.transform_expression(air_expression)))
            }
        };

        AirNode { id: node.id, kind }
    }

    fn transform_assignment(&mut self, assignment: &Assignment) -> Assignment {
        Assignment {
            target: assignment.target,
            // Does not matter at this point
            expected_type: None,
            expression: Box::new(self.transform_expression(&assignment.expression)),
        }
    }

    fn transform_update(&mut self, update: &Update) -> Update {
        Update {
            target: self.transform_place(&update.target),
            operator: update.operator,
            expression: Box::new(self.transform_expression(&update.expression)),
        }
    }

    fn transform_place(&mut self, place: &AirPlace) -> AirPlace {
        let kind = match &place.kind {
            AirPlaceKind::Variable(air_local_value_id) => {
                AirPlaceKind::Variable(*air_local_value_id)
            }
            AirPlaceKind::Accessor(accessor) => AirPlaceKind::Accessor(Accessor {
                value: Box::new(self.transform_expression(&accessor.value)),
                ident: accessor.ident.clone(),
                ident_id: accessor.ident_id,
            }),
        };
        AirPlace {
            id: place.id,
            r#type: self.resolve(&place.r#type),
            kind,
        }
    }

    fn transform_expression(&mut self, expression: &AirExpression) -> AirExpression {
        let kind = match &expression.kind {
            AirExpressionKind::Constant(air_constant) => {
                AirExpressionKind::Constant(self.transform_constant(air_constant))
            }
            AirExpressionKind::BinaryOperator(binary_operation) => {
                AirExpressionKind::BinaryOperator(BinaryOperation {
                    lhs: Box::new(self.transform_expression(&binary_operation.lhs)),
                    rhs: Box::new(self.transform_expression(&binary_operation.rhs)),
                    operator: binary_operation.operator,
                })
            }
            AirExpressionKind::UnaryOperator(unary_operation) => {
                AirExpressionKind::UnaryOperator(UnaryOperation {
                    expression: Box::new(self.transform_expression(&unary_operation.expression)),
                    operator: unary_operation.operator,
                })
            }
            AirExpressionKind::Variable(air_value_id) => AirExpressionKind::Variable(*air_value_id),
            AirExpressionKind::Type(air_type) => AirExpressionKind::Type(self.resolve(air_type)),
            AirExpressionKind::Accessor(accessor) => AirExpressionKind::Accessor(Accessor {
                value: Box::new(self.transform_expression(&accessor.value)),
                ident: accessor.ident.clone(),
                ident_id: accessor.ident_id,
            }),
            AirExpressionKind::Call(call) => AirExpressionKind::Call(self.transform_call(call)),
            AirExpressionKind::Synthetic => AirExpressionKind::Synthetic,
            AirExpressionKind::Error(location) => AirExpressionKind::Error(location),
        };
        AirExpression {
            id: expression.id,
            r#type: self.resolve(&expression.r#type),
            kind,
        }
    }

    fn transform_constant(&mut self, constant: &AirConstant) -> AirConstant {
        match constant {
            AirConstant::Number(num) => AirConstant::Number(*num),
            AirConstant::Boolean(boolean) => AirConstant::Boolean(*boolean),
            AirConstant::String(small_string) => AirConstant::String(small_string.clone()),
            AirConstant::StructLiteral {
                struct_type,
                fields,
            } => AirConstant::StructLiteral {
                struct_type: struct_type.as_ref().map(|(id, ty)| (*id, self.resolve(ty))),
                fields: fields
                    .iter()
                    .map(|(label, expr)| (label.clone(), self.transform_expression(expr)))
                    .collect(),
            },
        }
    }

    /// This requires some special handling, because calls can cause new monomorphizations
    fn transform_call(&mut self, call: &Call) -> Call {
        let generic_args = call
            .generic_arguments
            .computed()
            .iter()
            .map(|ty| self.resolve_type(*ty))
            .collect::<Vec<_>>();
        let arguments = call
            .arguments
            .iter()
            .map(|arg| self.transform_expression(arg))
            .collect();

        let mut function = Box::new(self.transform_expression(&call.function));
        if let Type::FunctionItem(fn_ty) = function.r#type.computed() {
            let prev = std::mem::replace(
                &mut self.inference,
                GenericInference::from_already_inferred(generic_args.clone()),
            );
            function.r#type =
                AirType::Computed(Type::FunctionItem(self.resolve_function_type(fn_ty)));
            self.inference = prev;
        }

        let transformed_call = Call {
            function,
            arguments,
            generic_arguments: AirGenericArguments::Computed(generic_args.clone()),
        };

        match transformed_call.function_type(self.ty_ctx) {
            CallKind::FunctionItem(type_view_kind) => match &type_view_kind.value.reference {
                FunctionReference::UserDefined(air_function_id) => {
                    self.queue.add(*air_function_id, generic_args)
                }
                FunctionReference::Extern { .. } => {}
            },
            CallKind::Intrinsic(_) => {
                // Nothing needs to be done about intrinsics, they are evaluate in place anyways
            }
        }

        transformed_call
    }
}
