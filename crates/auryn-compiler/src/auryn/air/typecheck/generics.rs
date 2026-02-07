use stdx::{FastMap, SmallString};

use crate::auryn::{
    air::typecheck::{
        type_context::TypeContext,
        types::{GenericId, GenericType, Type, TypeView},
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
    /// Resolves a generic type if its generic type parameters are already known.
    /// Otherwise, returns None
    pub fn resolve_generic_type(&self, ty_ctx: &mut TypeContext, ty: Type) -> Option<Type> {
        match ty.as_view(ty_ctx) {
            TypeView::Generic(generic) => {
                let id = generic.value.id;
                self.inferred.get(&id).copied()
            }
            TypeView::Array(array) => {
                let inner = array.value.element_type;
                let resolved_inner = self.resolve_generic_type(ty_ctx, inner)?;
                Some(ty_ctx.array_of(resolved_inner))
            }
            _ => Some(ty),
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

    pub fn get_resolved(&self, ty_ctx: &TypeContext, ty: Type) -> Type {
        match ty {
            Type::Generic(generic) => *self
                .inferred
                .get(&ty_ctx.get(generic).id)
                .expect("TODO Add error messsage for non-inferred type"),
            _ => ty,
        }
    }

    pub fn into_inferred(self) -> FastMap<GenericId, Type> {
        self.inferred
    }
}
