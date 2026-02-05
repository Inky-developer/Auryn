use stdx::{FastIndexSet, FastMap, FastSet, SmallString, default};

use crate::{
    auryn::{
        air::{
            data::{
                Accessor, AirBlock, AirBlockFinalizer, AirBlockId, AirConstant, AirExpression,
                AirExpressionKind, AirFunction, AirFunctionId, AirLocalValueId, AirNode,
                AirNodeKind, AirPlace, AirPlaceKind, AirValueId, Assignment, BinaryOperation, Call,
                CallKind, ExternFunctionKind, FunctionReference, Intrinsic, UnaryOperator, Update,
            },
            typecheck::{type_context::TypeContext, types::TypeView},
        },
        codegen_java::{
            class_generator::{ClassGenerator, GeneratedMethodData},
            print_utils::make_value_printable,
            representation::{
                FieldDescriptor, ImplicitArgs, MethodDescriptor, PrimitiveOrObject, Representation,
                RepresentationCtx, ReturnDescriptor,
            },
        },
        tokenizer::BinaryOperatorToken,
    },
    java::{
        class::{self, Comparison, TypeCategory},
        function_assembler::{ConstantValue, FunctionAssembler, Instruction, VariableId},
        source_graph::{BasicBlockId, BlockFinalizer},
    },
};

pub struct FunctionGenerator<'a> {
    assembler: FunctionAssembler<'a>,
    variable_map: FastMap<AirLocalValueId, VariableId>,
    block_translation: FastMap<AirBlockId, BasicBlockId>,
    generated_blocks: FastSet<AirBlockId>,
    pending_blocks: FastIndexSet<AirBlockId>,
    function_infos: &'a FastMap<AirFunctionId, GeneratedMethodData>,
    ty_ctx: &'a TypeContext,
    repr_ctx: &'a mut RepresentationCtx,
    class_name: &'a SmallString,
}

impl<'a> FunctionGenerator<'a> {
    pub fn new(
        name: SmallString,
        function: &AirFunction,
        method_descriptor: MethodDescriptor,
        parent: &'a mut ClassGenerator,
    ) -> Self {
        let TypeView::FunctionItem(function_type) =
            function.r#type.computed().as_view(&parent.air.ty_ctx)
        else {
            panic!("Invalid function type");
        };
        let mut block_translation = FastMap::default();
        block_translation.insert(AirBlockId::ROOT, BasicBlockId(0));

        let mut variable_map = FastMap::default();

        let mut variable_index = 0;
        for (parameter, argument_id) in function_type
            .parameters()
            .iter()
            .zip(function.argument_ids())
        {
            if let Some(primitive) = parent
                .repr_ctx
                .get_representation(parameter.as_view(&parent.air.ty_ctx))
            {
                let size = primitive.stack_size();
                let variable_id = VariableId {
                    index: variable_index,
                    r#type: primitive,
                };
                variable_index += size;
                variable_map.insert(argument_id, variable_id);
            }
        }

        let assembler = FunctionAssembler::new(
            parent.constant_pool.add_class(parent.class_name.clone()),
            name,
            method_descriptor,
            ImplicitArgs::None,
            &mut parent.constant_pool,
        );

        Self {
            class_name: &parent.class_name,
            function_infos: &parent.generated_methods,
            assembler,
            block_translation,
            variable_map,
            ty_ctx: &parent.air.ty_ctx,
            repr_ctx: &mut parent.repr_ctx,
            generated_blocks: default(),
            pending_blocks: default(),
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
                        Representation::Integer => BlockFinalizer::ReturnInteger,
                        Representation::Long => BlockFinalizer::ReturnLong,
                        Representation::Boolean => BlockFinalizer::ReturnBoolean,
                        Representation::Object { .. } | Representation::Array(_) => {
                            BlockFinalizer::ReturnObject
                        }
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
                assert_eq!(result, Some(Representation::Boolean));
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
            AirNodeKind::Update(update) => self.generate_update(update),
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
            Some(self.variable_map[&assignment.target].clone())
        } else {
            let air_type = assignment.expression.r#type.computed().as_view(self.ty_ctx);
            if let Some(primitive) = self.repr_ctx.get_representation(air_type) {
                let variable_id = self.assembler.alloc_variable(primitive);
                self.variable_map
                    .insert(assignment.target, variable_id.clone());
                Some(variable_id)
            } else {
                None
            }
        };

        if let Some(variable_id) = variable_id {
            self.assembler.add(Instruction::Store(variable_id));
        }
    }

    fn generate_update(&mut self, update: &Update) {
        let place = self.generate_place(&update.target);

        let repr = if let Some(op) = update.operator.to_binary_operator() {
            self.generate_binary_operation_inner(
                |this| this.generate_place_load(place.clone()),
                |this| this.generate_expression(&update.expression),
                op,
            )
        } else {
            self.generate_expression(&update.expression)
        };

        self.generate_place_store(place, repr);
    }

    fn generate_place(&mut self, place: &AirPlace) -> Place {
        match &place.kind {
            AirPlaceKind::Variable(var) => {
                if let Some(id) = self.variable_map.get(var) {
                    Place::Variable(id.clone())
                } else {
                    Place::Empty
                }
            }
            AirPlaceKind::Accessor(accessor) => {
                let object_repr = self.generate_expression(&accessor.value);
                let target_repr = self
                    .repr_ctx
                    .get_representation(place.r#type.as_view(self.ty_ctx));
                if let (Some(object_repr), Some(target_repr)) = (object_repr, target_repr) {
                    match object_repr {
                        Representation::Object(name) => Place::Stack {
                            name,
                            field: accessor.ident.clone(),
                            field_repr: target_repr,
                        },
                        other => panic!("Cannot access field on {other:?}"),
                    }
                } else {
                    Place::Empty
                }
            }
        }
    }

    /// Loads a place, without consuming its stack value
    /// Stack: `.., place -> .., place, value`
    fn generate_place_load(&mut self, place: Place) -> Option<Representation> {
        match place {
            Place::Variable(var) => {
                let repr = var.r#type.clone();
                self.assembler.add(Instruction::Load(var));
                Some(repr)
            }
            Place::Stack {
                name,
                field,
                field_repr,
            } => {
                self.assembler.add(Instruction::Dup(TypeCategory::Normal));
                let field_descriptor = field_repr.clone().into_field_descriptor();
                self.assembler.add(Instruction::GetField {
                    class_name: name,
                    name: field,
                    field_descriptor,
                });
                Some(field_repr)
            }
            Place::Empty => None,
        }
    }

    /// Stores a value into the place.
    /// Stack: `.., place, value -> ..`
    fn generate_place_store(&mut self, place: Place, value_repr: Option<Representation>) {
        match place {
            Place::Variable(var) => self.assembler.add(Instruction::Store(var)),
            Place::Stack {
                name,
                field,
                field_repr,
            } => {
                let field_descriptor = field_repr.into_field_descriptor();
                self.assembler.add(Instruction::PutField {
                    class_name: name,
                    name: field,
                    field_descriptor,
                });
            }
            Place::Empty => {
                if let Some(repr) = value_repr {
                    self.assembler.add(Instruction::Pop(repr.category()));
                }
            }
        }
    }

    /// The return value indicates the stack usage
    fn generate_expression(&mut self, expression: &AirExpression) -> Option<Representation> {
        let result_type = expression.r#type.as_view(self.ty_ctx);
        let repr = self.repr_ctx.get_representation(result_type);

        match &expression.kind {
            AirExpressionKind::Constant(constant) => {
                self.generate_constant(result_type, &repr, constant)
            }
            AirExpressionKind::BinaryOperator(binary_operator) => {
                self.generate_binary_operation(binary_operator)
            }
            AirExpressionKind::UnaryOperator(unary_operator) => {
                self.generate_unary_operation(&unary_operator.expression, unary_operator.operator)
            }
            AirExpressionKind::Variable(variable) => {
                self.generate_variable(repr.clone(), variable);
            }
            AirExpressionKind::Type(_) => {
                // Nothing needs to be done, since types are compile time constructs
            }
            AirExpressionKind::Accessor(accessor) => self.generate_accessor(accessor, repr.clone()),
            AirExpressionKind::Call(call) => {
                self.generate_call(call, expression.r#type.computed().as_view(self.ty_ctx))
            }
            expr @ (AirExpressionKind::Synthetic | AirExpressionKind::Error(_)) => {
                unreachable!("Codegen was started with invalid air: {expr:?}")
            }
        };
        repr
    }

    fn generate_constant(
        &mut self,
        result_type: TypeView,
        result_repr: &Option<Representation>,
        constant: &AirConstant,
    ) {
        match constant {
            AirConstant::Number(number) => match result_repr {
                None => {}
                Some(Representation::Integer) => self.assembler.add(Instruction::LoadConstant {
                    value: ConstantValue::Integer(
                        (*number)
                            .try_into()
                            .expect("Number should fit into target type"),
                    ),
                }),
                Some(Representation::Long) => self.assembler.add(Instruction::LoadConstant {
                    value: ConstantValue::Long(
                        (*number)
                            .try_into()
                            .expect("Number should fit into target type"),
                    ),
                }),
                Some(other) => panic!("Cannot load number as {other:?}"),
            },
            AirConstant::Boolean(boolean) => {
                self.assembler.add(Instruction::LoadConstant {
                    value: ConstantValue::Boolean(*boolean),
                });
            }
            AirConstant::String(text) => {
                let value = ConstantValue::String(text.clone());
                self.assembler.add(Instruction::LoadConstant { value });
            }
            AirConstant::StructLiteral {
                struct_type,
                fields,
            } => {
                let info = match struct_type.as_ref() {
                    Some((_, ty)) => {
                        let TypeView::Struct(ty) = ty.as_view(self.ty_ctx) else {
                            unreachable!("Invalid struct type");
                        };
                        self.repr_ctx.get_struct_repr(ty).clone()
                    }
                    None => {
                        let TypeView::Structural(structural) = result_type else {
                            panic!("Struct literal should result in a structural type");
                        };
                        self.repr_ctx.get_structural_repr(structural).clone()
                    }
                };

                if info.is_zero_sized {
                    return;
                }

                self.assembler.add_all([
                    Instruction::New(info.class_name.clone()),
                    Instruction::Dup(TypeCategory::Normal),
                    Instruction::InvokeSpecial {
                        method_descriptor: MethodDescriptor::VOID,
                        class_name: info.class_name.clone(),
                        name: "<init>".into(),
                    },
                ]);

                for (name, expr) in fields {
                    self.assembler.add(Instruction::Dup(TypeCategory::Normal));
                    let repr = self.generate_expression(expr);

                    if let Some(repr) = repr {
                        self.assembler.add_all([Instruction::PutField {
                            class_name: info.class_name.clone(),
                            name: name.value.clone(),
                            field_descriptor: repr.into_field_descriptor(),
                        }]);
                    } else {
                        self.assembler.add(Instruction::Pop(TypeCategory::Normal));
                    }
                }
            }
        }
    }

    fn generate_binary_operation(&mut self, operation: &BinaryOperation) {
        self.generate_binary_operation_inner(
            |this| this.generate_expression(&operation.lhs),
            |this| this.generate_expression(&operation.rhs),
            operation.operator,
        );
    }

    fn generate_binary_operation_inner(
        &mut self,
        lhs: impl FnOnce(&mut Self) -> Option<Representation>,
        rhs: impl FnOnce(&mut Self) -> Option<Representation>,
        operator: BinaryOperatorToken,
    ) -> Option<Representation> {
        match operator {
            BinaryOperatorToken::Plus
            | BinaryOperatorToken::Minus
            | BinaryOperatorToken::Times
            | BinaryOperatorToken::Divide
            | BinaryOperatorToken::Remainder => {
                let repr = lhs(self);
                let PrimitiveOrObject::Primitive(primitive) =
                    repr.clone().unwrap().into_primitive_type_or_object()
                else {
                    panic!("Can only perform operation on primitives");
                };
                rhs(self);
                let instruction = match operator {
                    BinaryOperatorToken::Plus => Instruction::Add(primitive),
                    BinaryOperatorToken::Minus => Instruction::Sub(primitive),
                    BinaryOperatorToken::Times => Instruction::Mul(primitive),
                    BinaryOperatorToken::Divide => Instruction::Div(primitive),
                    BinaryOperatorToken::Remainder => Instruction::Rem(primitive),
                    _ => unreachable!(),
                };
                self.assembler.add(instruction);
                repr
            }
            BinaryOperatorToken::Equal => self.generate_comparison(Comparison::Equal, lhs, rhs),
            BinaryOperatorToken::NotEqual => {
                self.generate_comparison(Comparison::NotEqual, lhs, rhs)
            }
            BinaryOperatorToken::Greater => self.generate_comparison(Comparison::Greater, lhs, rhs),
            BinaryOperatorToken::GreaterOrEqual => {
                self.generate_comparison(Comparison::GreaterOrEqual, lhs, rhs)
            }
            BinaryOperatorToken::Less => self.generate_comparison(Comparison::Less, lhs, rhs),
            BinaryOperatorToken::LessOrEqual => {
                self.generate_comparison(Comparison::LessOrEqual, lhs, rhs)
            }
            BinaryOperatorToken::And => self.generate_and(lhs, rhs),
            BinaryOperatorToken::Or => self.generate_or(lhs, rhs),
        }
    }

    fn generate_comparison(
        &mut self,
        comparison: Comparison,
        lhs: impl FnOnce(&mut Self) -> Option<Representation>,
        rhs: impl FnOnce(&mut Self) -> Option<Representation>,
    ) -> Option<Representation> {
        let repr = lhs(self);
        let PrimitiveOrObject::Primitive(primitive) =
            repr.clone().unwrap().into_primitive_type_or_object()
        else {
            panic!("Can only compare primitives")
        };
        rhs(self);
        let pos_block_id = self.assembler.add_block();
        let neg_block_id = self.assembler.add_block();
        let next_block_id = self.assembler.add_block();
        self.assembler.current_block_mut().finalizer = BlockFinalizer::BranchValueCmp {
            value_type: primitive,
            comparison,
            positive_block: pos_block_id,
            negative_block: neg_block_id,
        };

        self.assembler.set_current_block_id(pos_block_id);
        self.assembler.add(Instruction::LoadConstant {
            value: ConstantValue::Boolean(true),
        });
        self.assembler.current_block_mut().finalizer = BlockFinalizer::Goto(next_block_id);

        self.assembler.set_current_block_id(neg_block_id);
        self.assembler.add(Instruction::LoadConstant {
            value: ConstantValue::Boolean(false),
        });
        self.assembler.current_block_mut().finalizer = BlockFinalizer::Goto(next_block_id);

        self.assembler.set_current_block_id(next_block_id);

        repr
    }

    fn generate_and(
        &mut self,
        lhs: impl FnOnce(&mut Self) -> Option<Representation>,
        rhs: impl FnOnce(&mut Self) -> Option<Representation>,
    ) -> Option<Representation> {
        let first_true_block = self.assembler.add_block();
        let false_block = self.assembler.add_block();
        let next_block = self.assembler.add_block();

        let repr = lhs(self);
        self.assembler.current_block_mut().finalizer = BlockFinalizer::BranchInteger {
            comparison: Comparison::NotEqual,
            positive_block: first_true_block,
            negative_block: false_block,
        };

        self.assembler.set_current_block_id(first_true_block);
        rhs(self);
        self.assembler.current_block_mut().finalizer = BlockFinalizer::Goto(next_block);

        self.assembler.set_current_block_id(false_block);
        self.assembler.add(Instruction::LoadConstant {
            value: ConstantValue::Boolean(false),
        });
        self.assembler.current_block_mut().finalizer = BlockFinalizer::Goto(next_block);

        self.assembler.set_current_block_id(next_block);

        repr
    }

    fn generate_or(
        &mut self,
        lhs: impl FnOnce(&mut Self) -> Option<Representation>,
        rhs: impl FnOnce(&mut Self) -> Option<Representation>,
    ) -> Option<Representation> {
        let first_false_block = self.assembler.add_block();
        let true_block = self.assembler.add_block();
        let next_block = self.assembler.add_block();

        let repr = lhs(self);
        self.assembler.current_block_mut().finalizer = BlockFinalizer::BranchInteger {
            comparison: Comparison::Equal,
            positive_block: first_false_block,
            negative_block: true_block,
        };

        self.assembler.set_current_block_id(first_false_block);
        rhs(self);
        self.assembler.current_block_mut().finalizer = BlockFinalizer::Goto(next_block);

        self.assembler.set_current_block_id(true_block);
        self.assembler.add(Instruction::LoadConstant {
            value: ConstantValue::Boolean(true),
        });
        self.assembler.current_block_mut().finalizer = BlockFinalizer::Goto(next_block);

        self.assembler.set_current_block_id(next_block);

        repr
    }

    fn branch_bool(&mut self, on_true: impl FnOnce(&mut Self), on_false: impl FnOnce(&mut Self)) {
        let true_block = self.assembler.add_block();
        let false_block = self.assembler.add_block();
        let next_block = self.assembler.add_block();
        self.assembler.current_block_mut().finalizer = BlockFinalizer::BranchInteger {
            comparison: Comparison::Equal,
            positive_block: true_block,
            negative_block: false_block,
        };

        self.assembler.set_current_block_id(false_block);
        self.assembler.current_block_mut().finalizer = BlockFinalizer::Goto(next_block);
        on_true(self);

        self.assembler.set_current_block_id(true_block);
        self.assembler.current_block_mut().finalizer = BlockFinalizer::Goto(next_block);
        on_false(self);

        self.assembler.set_current_block_id(next_block);
    }

    fn generate_unary_operation(&mut self, expression: &AirExpression, op: UnaryOperator) {
        let UnaryOperator::Not = op;

        self.generate_expression(expression);
        self.branch_bool(
            |this| {
                this.assembler.add(Instruction::LoadConstant {
                    value: ConstantValue::Boolean(false),
                })
            },
            |this| {
                this.assembler.add(Instruction::LoadConstant {
                    value: ConstantValue::Boolean(true),
                })
            },
        );
    }

    fn generate_variable(
        &mut self,
        result_repr: Option<Representation>,
        variable: &AirValueId,
    ) -> Option<Representation> {
        match variable {
            AirValueId::Local(local_variable_id) => {
                if result_repr.is_some() {
                    let variable_id = self.variable_map[local_variable_id].clone();
                    self.assembler.add(Instruction::Load(variable_id));
                }
            }
            AirValueId::Intrinsic(_) | AirValueId::Global(_) => {
                // Intrinsic functions and globals have no run time representation
                // so nothing needs to be loaded
            }
        };
        result_repr
    }

    fn generate_accessor(&mut self, accessor: &Accessor, result_repr: Option<Representation>) {
        let value_repr = self.generate_expression(&accessor.value);

        match accessor.value.r#type.as_view(self.ty_ctx) {
            TypeView::Structural(_) | TypeView::Struct(_) => {
                let Some(result_repr) = result_repr else {
                    if let Some(value_repr) = value_repr {
                        self.assembler.add(Instruction::Pop(value_repr.category()));
                    }
                    return;
                };

                let Some(Representation::Object(class_name)) = value_repr else {
                    panic!("Structural type should be represented as object");
                };

                self.assembler.add(Instruction::GetField {
                    class_name,
                    name: accessor.ident.clone(),
                    field_descriptor: result_repr.into_field_descriptor(),
                });
            }
            TypeView::Meta(meta) => {
                let Some(result_repr) = result_repr else {
                    return;
                };
                let repr = self
                    .repr_ctx
                    .get_representation(meta.inner())
                    .expect("Type should be representable");
                let Representation::Object(class_name) = repr else {
                    panic!("Cannot get a static value from non-object type {repr:?}");
                };
                let name = accessor.ident.clone();
                let field_descriptor = result_repr.into_field_descriptor();
                self.assembler.add(Instruction::GetStatic {
                    class_name,
                    name,
                    field_descriptor,
                });
            }
            TypeView::Extern(_) => {
                if result_repr.is_some() {
                    todo!("Add support for loading runtime values from externs");
                }
            }
            other => panic!("Cannot get field on type {other}"),
        }
    }

    fn generate_call(&mut self, call: &Call, expression_type: TypeView) {
        self.generate_expression(&call.function);

        let function_type = call.function_type(self.ty_ctx);
        match function_type {
            CallKind::Intrinsic(intrinsic) => {
                self.generate_intrinsic_call(expression_type, intrinsic.intrinsic, &call.arguments)
            }
            CallKind::FunctionItem(function_item) => match &function_item.reference {
                FunctionReference::UserDefined(function_id) => {
                    for argument in &call.arguments {
                        self.generate_expression(argument);
                    }

                    let GeneratedMethodData {
                        generated_name,
                        method_descriptor,
                    } = &self.function_infos[function_id];
                    self.assembler.add(Instruction::InvokeStatic {
                        class_name: self.class_name.clone(),
                        name: generated_name.clone(),
                        method_descriptor: method_descriptor.clone(),
                    });
                }
                FunctionReference::Extern {
                    parent,
                    extern_name,
                    kind,
                    ..
                } => {
                    for argument in &call.arguments {
                        self.generate_expression(argument);
                    }

                    let method_descriptor =
                        self.repr_ctx.get_function_representation(function_item);
                    let Some(Representation::Object(class_name)) = self
                        .repr_ctx
                        .get_representation(parent.as_view(self.ty_ctx))
                    else {
                        panic!("Extern function should be member of an object");
                    };

                    match kind {
                        ExternFunctionKind::Method => {
                            self.assembler.add(Instruction::InvokeVirtual {
                                class_name,
                                name: extern_name.clone(),
                                method_descriptor,
                            });
                        }
                        ExternFunctionKind::Static => {
                            self.assembler.add(Instruction::InvokeStatic {
                                class_name,
                                name: extern_name.clone(),
                                method_descriptor,
                            })
                        }
                    }
                }
            },
        }
    }

    fn generate_intrinsic_call(
        &mut self,
        r#type: TypeView,
        intrinsic: Intrinsic,
        arguments: &[AirExpression],
    ) {
        match intrinsic {
            Intrinsic::Print => self.generate_intrinsic_print(arguments),
            Intrinsic::UnsafeTransmute => {
                self.generate_intrinsic_unsafe_transmute(r#type, arguments)
            }
            Intrinsic::Cast => self.generate_intrinsic_cast(r#type, arguments),
            Intrinsic::ArrayOf => self.generate_intrinsic_array_of(r#type, arguments),
            Intrinsic::ArrayOfZeros => self.generate_intrinsic_array_of_zeros(arguments, r#type),
            Intrinsic::ArrayGet => self.generate_intrinsic_array_get(arguments),
            Intrinsic::ArraySet => self.generate_intrinsic_array_set(arguments),
            Intrinsic::ArrayLen => self.generate_intrinsic_array_len(arguments),
        }
    }

    fn generate_intrinsic_print(&mut self, arguments: &[AirExpression]) {
        self.assembler.add(Instruction::GetStatic {
            class_name: "java/lang/System".into(),
            name: "out".into(),
            field_descriptor: FieldDescriptor::print_stream(),
        });
        let repr = self.generate_expression(&arguments[0]);
        let field_descriptor = make_value_printable(&mut self.assembler, repr, |_| {})
            .unwrap_or_else(|repr| {
                if let Some(other) = repr {
                    self.assembler.add(Instruction::Pop(other.category()));
                }
                let ty = arguments[0].r#type.computed().as_view(self.ty_ctx);
                self.assembler.add(Instruction::LoadConstant {
                    value: ConstantValue::String(ty.to_string().into()),
                });
                FieldDescriptor::string()
            });
        self.assembler.add(Instruction::InvokeVirtual {
            class_name: "java/io/PrintStream".into(),
            name: "println".into(),
            // method_type: MethodDescriptor("(I)V".to_string()),
            method_descriptor: MethodDescriptor {
                parameters: vec![field_descriptor],
                return_type: ReturnDescriptor::Void,
            },
        });
    }

    fn generate_intrinsic_unsafe_transmute(
        &mut self,
        target_type: TypeView,
        arguments: &[AirExpression],
    ) {
        let [expression] = arguments else {
            panic!("Invalid transmute call");
        };
        let repr = self.generate_expression(expression);

        if let Some(Representation::Object(_)) = repr
            && let Some(Representation::Object(to)) = self.repr_ctx.get_representation(target_type)
        {
            self.assembler.add(Instruction::Transmute(to));
        } else {
            panic!(
                "Transmute can never succeed: Tried to transmute from {repr:?} to {target_type}",
            );
        };
    }

    fn generate_intrinsic_cast(&mut self, target_type: TypeView, argument: &[AirExpression]) {
        let [value] = argument else {
            panic!("Invalid cast call");
        };

        self.generate_expression(value);

        let from_type = value.r#type.computed().as_view(self.ty_ctx);

        let from = self.repr_ctx.get_representation(from_type).unwrap();
        let to = self.repr_ctx.get_representation(target_type).unwrap();

        match (from, to) {
            (Representation::Integer | Representation::Boolean, Representation::Long) => {
                self.assembler.add(Instruction::IntToLong)
            }
            (Representation::Long, Representation::Integer) => {
                self.assembler.add(Instruction::LongToInt)
            }
            (Representation::Boolean, Representation::Integer) => {}
            (Representation::Integer | Representation::Long, Representation::Boolean) => todo!(),
            other => unreachable!("Invalid cast {other:?}"),
        }
    }

    fn generate_intrinsic_array_of(&mut self, r#type: TypeView, arguments: &[AirExpression]) {
        let TypeView::Array(array_type) = r#type else {
            unreachable!("Return type should be an array type");
        };
        let repr = self.repr_ctx.get_representation(array_type.element());

        match repr {
            Some(repr) => {
                self.assembler.add(Instruction::LoadConstant {
                    value: ConstantValue::Integer(arguments.len().try_into().unwrap()),
                });
                self.assembler.add(Instruction::NewArray(repr.clone()));

                for (index, argument) in arguments.iter().enumerate() {
                    self.assembler
                        .add(Instruction::Dup(class::TypeCategory::Normal));
                    self.assembler.add(Instruction::LoadConstant {
                        value: ConstantValue::Integer(index.try_into().unwrap()),
                    });
                    let actual_repr = self.generate_expression(argument);
                    assert_eq!(actual_repr.as_ref(), Some(&repr));
                    self.assembler.add(Instruction::ArrayStore(repr.clone()));
                }
            }
            None => unimplemented!(
                "Decide how to represent arrays of zero sized types. Maybe just use an int?"
            ),
        }
    }

    fn generate_intrinsic_array_of_zeros(
        &mut self,
        arguments: &[AirExpression],
        result_type: TypeView,
    ) {
        let [count] = arguments else {
            unreachable!("Should be a valid call");
        };

        let count = self.generate_expression(count);
        assert_eq!(count, Some(Representation::Integer));

        let TypeView::Array(element_type) = result_type else {
            panic!("arrayOf should return an array");
        };
        let element_repr = self
            .repr_ctx
            .get_representation(element_type.element())
            .expect("Cannot represent arrays of zero sized types yet");
        self.assembler.add(Instruction::NewArray(element_repr));
    }

    fn generate_intrinsic_array_get(&mut self, arguments: &[AirExpression]) {
        let [array, index] = arguments else {
            unreachable!("Should be valid call");
        };

        let TypeView::Array(array_type) = array.r#type.computed().as_view(self.ty_ctx) else {
            unreachable!("Should be an array");
        };
        let primitive = self
            .repr_ctx
            .get_representation(array_type.element())
            .expect("Zero sized element types not supported");

        self.generate_expression(array);
        self.generate_expression(index);
        self.assembler
            .add(Instruction::ArrayLoad(primitive.clone()));
    }

    fn generate_intrinsic_array_set(&mut self, arguments: &[AirExpression]) {
        let [array, index, value] = arguments else {
            unreachable!("Should be valid call");
        };

        let TypeView::Array(array_type) = array.r#type.computed().as_view(self.ty_ctx) else {
            unreachable!("Should be an array");
        };
        let primitive = self
            .repr_ctx
            .get_representation(array_type.element())
            .expect("Zero sized element types not supported");

        self.generate_expression(array);
        self.generate_expression(index);
        self.generate_expression(value);
        self.assembler.add(Instruction::ArrayStore(primitive));
    }

    fn generate_intrinsic_array_len(&mut self, arguments: &[AirExpression]) {
        let [array] = arguments else {
            unreachable!("Should be a valid call");
        };

        self.generate_expression(array);
        self.assembler.add(Instruction::ArrayLength);
    }
}

#[derive(Debug, Clone)]
enum Place {
    Variable(VariableId),
    Stack {
        name: SmallString,
        field: SmallString,
        field_repr: Representation,
    },
    Empty,
}
