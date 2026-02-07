use stdx::{FastMap, SmallString};

use crate::auryn::{
    air::{
        data::{FunctionReference, UnresolvedExternMember},
        namespace::{Namespace, UserDefinedTypeId},
    },
    syntax_id::{Spanned, SyntaxId},
};

#[derive(Debug)]
pub struct UnresolvedFunction {
    pub parameters_reference: SyntaxId,
    pub type_parameters: Vec<Spanned<SmallString>>,
    pub parameters: Vec<UnresolvedType>,
    pub return_type: Option<Box<UnresolvedType>>,
    pub reference: FunctionReference,
}

/// Reprsents a type that was written by the user but not resolved yet.
#[derive(Debug)]
pub enum UnresolvedType {
    /// A type that was defined by the user, identified by its `syntax_id`
    DefinedType(UserDefinedTypeId),
    /// A type not defined by the user (so probably built-in, like `String`)
    Ident(SyntaxId, SmallString),
    Array(SyntaxId, Box<UnresolvedType>),
    /// A structural type like {a: I32, b: I64}
    Structural(Vec<(Spanned<SmallString>, UnresolvedType)>),
    /// A nominal (named) structural type
    Struct {
        id: SyntaxId,
        ident: SmallString,
        fields: Vec<(Spanned<SmallString>, UnresolvedType)>,
    },
    Unit,
    /// A function type
    Function(UnresolvedFunction),
    Extern {
        id: SyntaxId,
        extern_name: SmallString,
        members: FastMap<SmallString, UnresolvedExternMember>,
    },
    Module {
        name: SmallString,
        id: SyntaxId,
        namespace: Namespace,
    },
}

impl UnresolvedType {
    pub fn visit_contained_types(&self, visitor: &mut impl FnMut(&UnresolvedType)) {
        use UnresolvedType::*;

        visitor(self);

        match self {
            DefinedType(_)
            | Ident(_, _)
            | Unit
            | Module {
                name: _,
                id: _,
                namespace: _,
            } => {}
            Array(_, unresolved_type) => unresolved_type.visit_contained_types(visitor),
            Struct {
                fields,
                id: _,
                ident: _,
            }
            | Structural(fields) => {
                for (_, ty) in fields {
                    ty.visit_contained_types(visitor);
                }
            }
            Function(UnresolvedFunction {
                parameters_reference: _,
                type_parameters: _,
                parameters,
                return_type,
                reference: _,
            }) => {
                for param in parameters {
                    param.visit_contained_types(visitor);
                }
                if let Some(ty) = return_type.as_ref() {
                    ty.visit_contained_types(visitor);
                }
            }
            Extern {
                id: _,
                extern_name: _,
                members,
            } => {
                for member in members.values() {
                    match member {
                        UnresolvedExternMember::StaticLet {
                            r#type,
                            extern_name: _,
                        } => r#type.visit_contained_types(visitor),
                        UnresolvedExternMember::Function {
                            unresolved_type,
                            extern_name: _,
                        } => {
                            unresolved_type.visit_contained_types(visitor);
                        }
                    }
                }
            }
        }
    }
}
