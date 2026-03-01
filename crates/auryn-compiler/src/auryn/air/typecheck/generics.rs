use stdx::{FastIndexMap, FastMap, SmallString};

use crate::auryn::{
    air::typecheck::{
        bounds::{Bound, MaybeBounded, StructuralBound},
        type_context::TypeContext,
        types::{GenericId, GenericType, StructuralType, Type, TypeView},
    },
    diagnostics::{diagnostic::Diagnostics, errors::MismatchedTypeInference},
    syntax_id::{Spanned, SyntaxId},
};

#[derive(Debug)]
pub enum GenericInferenceError {
    MultipleInference {
        generic_name: Spanned<SmallString>,
        first_inferred: Type,
        second_inferred: Type,
    },
}

impl GenericInferenceError {
    pub fn write(self, syntax_id: SyntaxId, diagnostics: &mut Diagnostics) {
        match self {
            GenericInferenceError::MultipleInference {
                generic_name,
                first_inferred,
                second_inferred,
            } => {
                diagnostics.add(
                    syntax_id,
                    MismatchedTypeInference {
                        generic_name,
                        first_inferred,
                        second_inferred,
                    },
                );
            }
        }
    }
}

pub type InferenceResult<T = ()> = Result<T, GenericInferenceError>;

/// Implements inference for generic types
#[derive(Debug, Default)]
pub struct GenericInference {
    inferred: FastMap<GenericId, Type>,
}

impl GenericInference {
    pub fn new(monomorphization: Vec<Type>) -> Self {
        Self {
            inferred: monomorphization
                .into_iter()
                .enumerate()
                .map(|(index, ty)| (GenericId(index), ty))
                .collect(),
        }
    }

    /// Resolves a generic type if its generic type parameters are already known.
    /// Otherwise, returns the tightest bound it can infer
    pub fn resolve_generic_type(&self, ty_ctx: &mut TypeContext, ty: Type) -> MaybeBounded {
        match ty.as_view(ty_ctx) {
            TypeView::Generic(generic) => {
                let id = generic.value.id;
                self.inferred
                    .get(&id)
                    .copied()
                    .map_or(MaybeBounded::Bounded(Bound::Top), Type::as_bounded)
            }
            TypeView::Array(array) => {
                let inner = array.value.element_type;
                let resolved_inner = self.resolve_generic_type(ty_ctx, inner);
                match resolved_inner {
                    MaybeBounded::Type(ty) => MaybeBounded::Type(ty_ctx.array_of(ty)),
                    MaybeBounded::Bounded(bound) => {
                        MaybeBounded::Bounded(ty_ctx.array_bound_of(bound))
                    }
                }
            }
            TypeView::Structural(structural) => {
                let fields = structural.fields.clone();
                let resolved_fields = fields
                    .into_iter()
                    .map(|(ident, ty)| (ident, self.resolve_generic_type(ty_ctx, ty)))
                    .collect::<FastIndexMap<_, _>>();
                if resolved_fields.values().all(|val| val.as_type().is_some()) {
                    let resolved_fields = resolved_fields
                        .into_iter()
                        .map(|(ident, bound)| (ident, bound.as_type().unwrap()))
                        .collect();
                    MaybeBounded::Type(ty_ctx.structural_of(StructuralType {
                        fields: resolved_fields,
                    }))
                } else {
                    MaybeBounded::Bounded(ty_ctx.structural_bound_of(StructuralBound {
                        fields: resolved_fields,
                    }))
                }
            }
            TypeView::Application(application) => {
                let r#type = application.r#type;
                let arguments = application.arguments.clone();
                let resolved_args = arguments
                    .into_iter()
                    .map(|ty| self.resolve_generic_type(ty_ctx, ty))
                    .collect::<Vec<_>>();
                if resolved_args.iter().all(|arg| arg.as_type().is_some()) {
                    MaybeBounded::Type(
                        ty_ctx.applied_of(
                            r#type,
                            resolved_args
                                .into_iter()
                                .map(|arg| arg.as_type().unwrap())
                                .collect(),
                        ),
                    )
                } else {
                    // TODO: Use tighter bound here
                    Bound::Top.as_bounded()
                }
            }
            _ => MaybeBounded::Type(ty),
        }
    }

    /// Adds new information discovered about generic parameters to be used later.
    /// Performs inference recursively, e.g. `Array<T>` with `Array<I32>` infers `T=I32`.
    pub fn infer_generics(&mut self, expected: TypeView, received: TypeView) -> InferenceResult {
        // println!("Inferring: {expected} {received}");
        match (expected, received) {
            (TypeView::Array(expected_inner), TypeView::Array(received_inner)) => {
                self.infer_generics(expected_inner.element(), received_inner.element())?;
            }
            (TypeView::Structural(expected_inner), TypeView::Structural(received_inner)) => {
                let inner_fields = expected_inner
                    .fields
                    .iter()
                    .map(|(ident, ty)| (ident.value.clone(), *ty))
                    .collect::<FastMap<_, _>>();
                for (received_name, received_ty) in &received_inner.fields {
                    if let Some(expected_ty) = inner_fields.get(&received_name.value) {
                        self.infer_generics(
                            expected_ty.as_view(expected_inner.ctx),
                            received_ty.as_view(expected_inner.ctx),
                        )?;
                    }
                }
            }
            (TypeView::Generic(generic), _) => {
                self.add_generic(generic.value, received.as_type())?;
            }
            _ => {}
        };
        Ok(())
    }

    fn add_generic(&mut self, generic: &GenericType, received: Type) -> InferenceResult {
        let existing_inference = self.inferred.insert(generic.id, received);
        if let Some(existing_inference) = existing_inference
            && existing_inference != received
        {
            return Err(GenericInferenceError::MultipleInference {
                generic_name: generic.ident.clone(),
                first_inferred: existing_inference,
                second_inferred: received,
            });
        }

        Ok(())
    }

    pub fn into_inferred(self) -> FastMap<GenericId, Type> {
        self.inferred
    }
}
