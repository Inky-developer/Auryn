use stdx::FastMap;

use crate::auryn::{
    air::{
        data::{
            AirModuleId, AirStaticValue, AirStaticValueId, AirType, FunctionReference, TypeAliasId,
            UnresolvedExternMember,
        },
        namespace::UserDefinedTypeId,
        typecheck::{
            type_context::{TypeContext, TypeId},
            types::{
                ExternType, ExternTypeMember, FunctionItemType, FunctionParameters, GenericId,
                GenericType, ModuleType, StructType, StructuralType, Type,
            },
        },
        unresolved_type::{UnresolvedFunction, UnresolvedType, UnresolvedTypeProducer},
    },
    diagnostics::{diagnostic::Diagnostics, errors::UndefinedVariable},
    syntax_id::SyntaxId,
};

#[derive(Debug, Clone, Copy)]
pub enum ResolverError {
    CircularTypeAlias(SyntaxId),
}

pub type ResolverResult = Result<Type, ResolverError>;

pub struct Context<'a> {
    pub ty_ctx: &'a mut TypeContext,
    pub diagnostics: &'a mut Diagnostics,
    pub statics: &'a FastMap<AirStaticValueId, AirStaticValue>,
    pub type_aliasses: &'a FastMap<TypeAliasId, AirType>,
    pub type_parameters: &'a Vec<GenericType>,
}

#[derive(Debug, Default)]
pub struct Resolver {
    current_generic_parameters: Vec<GenericType>,
}

impl Resolver {
    pub fn resolve_type(
        &mut self,
        ctx: &mut Context,
        unresolved: &UnresolvedType,
    ) -> ResolverResult {
        self.current_generic_parameters.clear();
        self.resolve(ctx, unresolved)
    }

    fn resolve(&mut self, ctx: &mut Context, unresolved: &UnresolvedType) -> ResolverResult {
        Ok(match unresolved {
            UnresolvedType::DefinedType(user_defined_type_id) => match user_defined_type_id {
                UserDefinedTypeId::Extern(type_id) => Type::Extern(*type_id),
                UserDefinedTypeId::Module(type_id) => Type::Module(*type_id),
                UserDefinedTypeId::Struct(struct_id) => {
                    // TODO: Verify that the number of generic arguments (And I guess their bounds) are correct
                    ctx.ty_ctx.applied_of(*struct_id, Vec::new())
                }
                UserDefinedTypeId::TypeAlias(id) => match &ctx.type_aliasses[id] {
                    AirType::Inferred => unreachable!(),
                    AirType::Unresolved(_) => {
                        return Err(ResolverError::CircularTypeAlias(id.0));
                    }
                    AirType::Computed(ty) => *ty,
                },
                UserDefinedTypeId::Generic(id) => {
                    // Two cases:
                    // - Either we are currently resolving a type signature
                    //   Then the `current_generic_parameters` field is set
                    // - Or we are currently inside a cotext where this generic is defined
                    //   Then we can use the already calculated generic of the ctx scope
                    let generic = if self.current_generic_parameters.is_empty() {
                        ctx.type_parameters[id.0].clone()
                    } else {
                        self.current_generic_parameters[id.0].clone()
                    };
                    ctx.ty_ctx.generic_of(generic)
                }
            },
            UnresolvedType::Unit => ctx.ty_ctx.unit_type(),
            UnresolvedType::Ident(id, ident) => match ident.parse() {
                Ok(r#type) => r#type,
                Err(_) => {
                    ctx.diagnostics.add(
                        *id,
                        UndefinedVariable {
                            ident: ident.clone(),
                        },
                    );
                    Type::Error
                }
            },
            UnresolvedType::Function(UnresolvedFunction {
                parameters_reference,
                type_parameters,
                parameters,
                return_type,
                reference,
            }) => {
                let type_parameters = type_parameters
                    .iter()
                    .enumerate()
                    .map(|(index, param)| GenericType {
                        id: GenericId(index),
                        ident: param.clone(),
                    })
                    .collect::<Vec<_>>();

                // Make the generic parameters known so that they can be used in nested resolve calls
                self.current_generic_parameters.clear();
                self.current_generic_parameters
                    .extend(type_parameters.iter().cloned());

                let parameters = parameters
                    .iter()
                    .map(|param| self.resolve(ctx, param))
                    .collect::<Result<_, _>>()?;
                let return_type = return_type
                    .as_ref()
                    .map_or(Ok(ctx.ty_ctx.unit_type()), |ty| self.resolve(ctx, ty))?;
                let reference = self.resolve_function_reference(ctx, reference)?;
                Type::FunctionItem(ctx.ty_ctx.add(
                    Some(reference.syntax_id()),
                    FunctionItemType {
                        type_parameters,
                        parameters: FunctionParameters {
                            parameters,
                            parameters_reference: *parameters_reference,
                        },
                        return_type,
                        reference,
                    },
                ))
            }
            UnresolvedType::Array(_id, inner) => {
                let element_type = self.resolve(ctx, inner)?;
                ctx.ty_ctx.array_of(element_type)
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
                                r#type: self.resolve(ctx, r#type)?,
                            },
                            UnresolvedExternMember::Function {
                                unresolved_type,
                                extern_name,
                                ..
                            } => ExternTypeMember {
                                r#type: self.resolve(ctx, unresolved_type)?,
                                extern_name: extern_name.clone(),
                            },
                        };
                        Ok((ident.clone(), member))
                    })
                    .collect::<Result<_, _>>()?;
                Type::Extern(ctx.ty_ctx.add(
                    *id,
                    ExternType {
                        extern_name: extern_name.clone(),
                        members,
                    },
                ))
            }
            UnresolvedType::Module {
                name,
                id,
                namespace,
            } => {
                let mut members = namespace
                    .types
                    .iter()
                    .map(|(k, v)| {
                        let ty = self.resolve_user_defined(ctx, *v)?;
                        Ok((k.clone(), ctx.ty_ctx.meta_of(ty)))
                    })
                    .collect::<Result<FastMap<_, _>, _>>()?;
                members.extend(
                    namespace
                        .statics
                        .iter()
                        .flat_map(|(k, v)| ctx.statics.get(v).map(|it| (k, it)))
                        .map(|(k, value)| {
                            let ty = match value {
                                AirStaticValue::Function(id) => Type::FunctionItem((*id).into()),
                            };
                            (k.clone(), ty)
                        }),
                );
                let module = ModuleType {
                    name: name.clone(),
                    members,
                };
                let id: TypeId<_> = AirModuleId(id.file_id().unwrap()).into();
                Type::Module(ctx.ty_ctx.add(id.syntax_id(), module))
            }
            UnresolvedType::Structural(structural_type) => {
                let ty = StructuralType {
                    fields: structural_type
                        .iter()
                        .map(|(ident, field)| Ok((ident.clone(), self.resolve(ctx, field)?)))
                        .collect::<Result<_, _>>()?,
                };
                ctx.ty_ctx.structural_of(ty)
            }
            UnresolvedType::Application {
                id: _,
                r#type,
                generic_arguments,
            } => {
                let generic_type = self.resolve_type_producer(ctx, r#type)?;
                let generic_arguments = generic_arguments
                    .iter()
                    .map(|arg| self.resolve(ctx, arg))
                    .collect::<Result<Vec<_>, _>>()?;
                // TODO: Add error message if the type cannot accept the generic arguments
                ctx.ty_ctx.applied_of(generic_type, generic_arguments)
            }
        })
    }

    pub fn resolve_type_producer(
        &mut self,
        ctx: &mut Context,
        unresolved: &UnresolvedTypeProducer,
    ) -> Result<TypeId<StructType>, ResolverError> {
        match unresolved {
            UnresolvedTypeProducer::DefinedType(type_id) => Ok(*type_id),
            UnresolvedTypeProducer::Struct {
                id,
                ident,
                generics,
                fields,
            } => {
                let generics = generics
                    .iter()
                    .enumerate()
                    .map(|(index, it)| GenericType {
                        id: GenericId(index),
                        ident: it.clone(),
                    })
                    .collect::<Vec<_>>();

                self.current_generic_parameters.clear();
                self.current_generic_parameters
                    .extend(generics.iter().cloned());
                let structural = StructuralType {
                    fields: fields
                        .iter()
                        .map(|(ident, field)| Ok((ident.clone(), self.resolve(ctx, field)?)))
                        .collect::<Result<_, _>>()?,
                };

                let r#struct = StructType {
                    type_parameters: generics,
                    ident: ident.clone(),
                    structural,
                };
                Ok(ctx.ty_ctx.add(*id, r#struct))
            }
        }
    }

    fn resolve_user_defined(
        &mut self,
        ctx: &mut Context,
        user_defined_id: UserDefinedTypeId,
    ) -> ResolverResult {
        self.resolve(ctx, &UnresolvedType::DefinedType(user_defined_id))
    }

    fn resolve_function_reference(
        &mut self,
        ctx: &mut Context,
        reference: &FunctionReference,
    ) -> Result<FunctionReference, ResolverError> {
        Ok(match reference {
            FunctionReference::UserDefined(id) => FunctionReference::UserDefined(*id),
            FunctionReference::Extern {
                parent,
                kind,
                extern_name,
                syntax_id,
            } => {
                let parent = match parent.as_ref() {
                    AirType::Inferred => unreachable!(),
                    AirType::Computed(ty) => *ty,
                    AirType::Unresolved(unresolved) => self.resolve(ctx, unresolved)?,
                };
                FunctionReference::Extern {
                    parent: Box::new(AirType::Computed(parent)),
                    kind: *kind,
                    extern_name: extern_name.clone(),
                    syntax_id: *syntax_id,
                }
            }
        })
    }
}
