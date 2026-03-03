use stdx::FastMap;

use crate::auryn::{
    air::{
        data::{
            AirModuleId, AirStaticValue, AirStaticValueId, FunctionReference, TypeAliasId,
            UnresolvedExternMember, UnresolvedFunctionReference,
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
    diagnostics::{
        diagnostic::Diagnostics,
        errors::{MismatchedTypeArgumentCount, UndefinedVariable},
    },
    syntax_id::SyntaxId,
};

#[derive(Debug, Clone, Copy)]
pub enum ResolverError {
    CircularTypeAlias(SyntaxId),
}

pub type ResolverResult = Result<Type, ResolverError>;

#[derive(Debug)]
pub struct TypeProducerInfo {
    pub parameter_count: usize,
    pub definition_id: SyntaxId,
}

/// Active resolver for a single resolution pass. Created fresh at each call site
/// by borrowing individual fields from the `Typechecker`.
pub struct Resolver<'a> {
    pub ty_ctx: &'a mut TypeContext,
    pub diagnostics: &'a mut Diagnostics,
    pub statics: &'a FastMap<AirStaticValueId, AirStaticValue>,
    /// Aliases that have already been resolved. An alias absent from this map
    /// is either still being resolved (i.e. circular) or does not exist.
    pub resolved_type_aliases: &'a FastMap<TypeAliasId, Type>,
    pub type_parameters: &'a Vec<GenericType>,
    pub type_producer_info: &'a FastMap<TypeId<StructType>, TypeProducerInfo>,
    pub current_generic_parameters: Vec<GenericType>,
}

impl<'a> Resolver<'a> {
    pub fn resolve_type(&mut self, unresolved: &UnresolvedType) -> ResolverResult {
        self.current_generic_parameters.clear();
        self.resolve(unresolved)
    }

    fn resolve(&mut self, unresolved: &UnresolvedType) -> ResolverResult {
        Ok(match unresolved {
            UnresolvedType::DefinedType(user_defined_type_id) => match user_defined_type_id {
                UserDefinedTypeId::Extern(type_id) => Type::Extern(*type_id),
                UserDefinedTypeId::Module(type_id) => Type::Module(*type_id),
                UserDefinedTypeId::Struct(struct_id) => {
                    // TODO: Verify that the number of generic arguments (And I guess their bounds) are correct
                    self.ty_ctx.applied_of(*struct_id, Vec::new())
                }
                UserDefinedTypeId::TypeAlias(id) => match self.resolved_type_aliases.get(id) {
                    Some(ty) => *ty,
                    None => return Err(ResolverError::CircularTypeAlias(id.0)),
                },
                UserDefinedTypeId::Generic(id) => {
                    // Two cases:
                    // - Either we are currently resolving a type signature
                    //   Then the `current_generic_parameters` field is set
                    // - Or we are currently inside a context where this generic is defined
                    //   Then we can use the already calculated generic of the ctx scope
                    let generic = if self.current_generic_parameters.is_empty() {
                        self.type_parameters[id.0].clone()
                    } else {
                        self.current_generic_parameters[id.0].clone()
                    };
                    self.ty_ctx.generic_of(generic)
                }
            },
            UnresolvedType::Unit => self.ty_ctx.unit_type(),
            UnresolvedType::Ident(id, ident) => match ident.parse() {
                Ok(r#type) => r#type,
                Err(_) => {
                    self.diagnostics.add(
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
                    .map(|param| self.resolve(param))
                    .collect::<Result<_, _>>()?;
                let return_type = return_type
                    .as_ref()
                    .map_or(Ok(self.ty_ctx.unit_type()), |ty| self.resolve(ty))?;
                let reference = self.resolve_function_reference(reference)?;
                Type::FunctionItem(self.ty_ctx.add(
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
                let element_type = self.resolve(inner)?;
                self.ty_ctx.array_of(element_type)
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
                                r#type: self.resolve(r#type)?,
                            },
                            UnresolvedExternMember::Function {
                                unresolved_type,
                                extern_name,
                                ..
                            } => ExternTypeMember {
                                r#type: self.resolve(unresolved_type)?,
                                extern_name: extern_name.clone(),
                            },
                        };
                        Ok((ident.clone(), member))
                    })
                    .collect::<Result<_, _>>()?;
                Type::Extern(self.ty_ctx.add(
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
                        let ty = self.resolve_user_defined(*v)?;
                        Ok((k.clone(), self.ty_ctx.meta_of(ty)))
                    })
                    .collect::<Result<FastMap<_, _>, _>>()?;
                members.extend(
                    namespace
                        .statics
                        .iter()
                        .flat_map(|(k, v)| self.statics.get(v).map(|it| (k, it)))
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
                Type::Module(self.ty_ctx.add(id.syntax_id(), module))
            }
            UnresolvedType::Structural(structural_type) => {
                let ty = StructuralType {
                    fields: structural_type
                        .iter()
                        .map(|(ident, field)| Ok((ident.clone(), self.resolve(field)?)))
                        .collect::<Result<_, _>>()?,
                };
                self.ty_ctx.structural_of(ty)
            }
            UnresolvedType::Application {
                id,
                r#type,
                generic_arguments,
            } => {
                let generic_type = self.resolve_type_producer(r#type)?;
                let info = &self.type_producer_info[&generic_type];
                let provided = generic_arguments.len();
                if provided != info.parameter_count {
                    self.diagnostics.add(
                        *id,
                        MismatchedTypeArgumentCount {
                            definition_id: info.definition_id,
                            expected: info.parameter_count,
                            got: provided,
                        },
                    );
                    return Ok(Type::Error);
                }
                let generic_arguments = generic_arguments
                    .iter()
                    .map(|arg| self.resolve(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                self.ty_ctx.applied_of(generic_type, generic_arguments)
            }
        })
    }

    pub fn resolve_type_producer(
        &mut self,
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
                        .map(|(ident, field)| Ok((ident.clone(), self.resolve(field)?)))
                        .collect::<Result<_, _>>()?,
                };

                let r#struct = StructType {
                    type_parameters: generics,
                    ident: ident.clone(),
                    structural,
                };
                Ok(self.ty_ctx.add(*id, r#struct))
            }
        }
    }

    fn resolve_user_defined(&mut self, user_defined_id: UserDefinedTypeId) -> ResolverResult {
        self.resolve(&UnresolvedType::DefinedType(user_defined_id))
    }

    fn resolve_function_reference(
        &mut self,
        reference: &UnresolvedFunctionReference,
    ) -> Result<FunctionReference, ResolverError> {
        Ok(match reference {
            UnresolvedFunctionReference::UserDefined(id) => FunctionReference::UserDefined(*id),
            UnresolvedFunctionReference::Extern {
                parent,
                kind,
                extern_name,
                syntax_id,
            } => {
                let parent = self.resolve(parent)?;
                FunctionReference::Extern {
                    parent,
                    kind: *kind,
                    extern_name: extern_name.clone(),
                    syntax_id: *syntax_id,
                }
            }
        })
    }
}
