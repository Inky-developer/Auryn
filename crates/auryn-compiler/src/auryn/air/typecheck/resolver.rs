use stdx::FastMap;

use crate::auryn::{
    air::{
        data::{
            AirModuleId, AirStaticValue, AirStaticValueId, AirType, FunctionReference, TypeAliasId,
            UnresolvedExternMember,
        },
        namespace::UserDefinedTypeId,
        typecheck::{
            type_context::TypeContext,
            types::{
                ExternType, ExternTypeMember, FunctionItemType, FunctionParameters, ModuleType,
                StructType, StructuralType, Type,
            },
        },
        unresolved_type::UnresolvedType,
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
}

#[derive(Debug, Default)]
pub struct Resolver;

impl Resolver {
    pub fn resolve_type(
        &mut self,
        ctx: &mut Context,
        unresolved: &UnresolvedType,
    ) -> ResolverResult {
        Ok(match unresolved {
            UnresolvedType::DefinedType(user_defined_type_id) => match user_defined_type_id {
                UserDefinedTypeId::Extern(type_id) => Type::Extern(*type_id),
                UserDefinedTypeId::Module(type_id) => Type::Module(*type_id),
                UserDefinedTypeId::Struct(type_id) => Type::Struct(*type_id),
                UserDefinedTypeId::TypeAlias(id) => match &ctx.type_aliasses[id] {
                    AirType::Inferred => unreachable!(),
                    AirType::Unresolved(_) => {
                        return Err(ResolverError::CircularTypeAlias(id.0));
                    }
                    AirType::Computed(ty) => *ty,
                },
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
            UnresolvedType::Function {
                parameters_reference,
                parameters,
                return_type,
                reference,
            } => {
                let parameters = parameters
                    .iter()
                    .map(|param| self.resolve_type(ctx, param))
                    .collect::<Result<_, _>>()?;
                let return_type = return_type
                    .as_ref()
                    .map_or(Ok(ctx.ty_ctx.unit_type()), |ty| self.resolve_type(ctx, ty))?;
                let reference = self.resolve_function_reference(ctx, reference)?;
                Type::FunctionItem(ctx.ty_ctx.add_function_item(
                    reference.syntax_id(),
                    FunctionItemType {
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
                let element_type = self.resolve_type(ctx, inner)?;
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
                                r#type: self.resolve_type(ctx, r#type)?,
                            },
                            UnresolvedExternMember::Function {
                                unresolved_type,
                                extern_name,
                                ..
                            } => ExternTypeMember {
                                r#type: self.resolve_type(ctx, unresolved_type)?,
                                extern_name: extern_name.clone(),
                            },
                        };
                        Ok((ident.clone(), member))
                    })
                    .collect::<Result<_, _>>()?;
                Type::Extern(ctx.ty_ctx.add_extern(
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
                Type::Module(
                    ctx.ty_ctx
                        .add_module(AirModuleId(id.file_id().unwrap()), module),
                )
            }
            UnresolvedType::Structural(structural_type) => {
                let ty = StructuralType {
                    fields: structural_type
                        .iter()
                        .map(|(ident, field)| Ok((ident.clone(), self.resolve_type(ctx, field)?)))
                        .collect::<Result<_, _>>()?,
                };
                ctx.ty_ctx.structural_of(ty)
            }
            UnresolvedType::Struct { id, ident, fields } => {
                let structural = StructuralType {
                    fields: fields
                        .iter()
                        .map(|(ident, field)| Ok((ident.clone(), self.resolve_type(ctx, field)?)))
                        .collect::<Result<_, _>>()?,
                };
                let r#struct = StructType {
                    ident: ident.clone(),
                    structural,
                };
                Type::Struct(ctx.ty_ctx.add_struct(*id, r#struct))
            }
        })
    }

    fn resolve_user_defined(
        &mut self,
        ctx: &mut Context,
        user_defined_id: UserDefinedTypeId,
    ) -> ResolverResult {
        self.resolve_type(ctx, &UnresolvedType::DefinedType(user_defined_id))
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
                    AirType::Unresolved(unresolved) => self.resolve_type(ctx, unresolved)?,
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
