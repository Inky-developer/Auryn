use stdx::{FastMap, default};

use crate::auryn::air::typecheck::{
    bounds::{Bound, MaybeBounded},
    type_context::TypeContext,
    types::{GenericId, GenericType, Type, TypeView},
};

/// Implements inference for generic types
#[derive(Debug, Default)]
pub struct GenericInference {
    inferred: FastMap<GenericId, Type>,
}

impl GenericInference {
    /// Returns the type that should be used when infererring a parameter of the given expected type
    /// The bound is either the type or the resolved generic
    pub fn get_bound_for_inference(&self, ty_ctx: &TypeContext, ty: Type) -> MaybeBounded {
        match ty {
            Type::Generic(generic) => {
                let id = ty_ctx.get(generic).id;
                self.inferred
                    .get(&id)
                    .map_or(MaybeBounded::Bounded(Bound::Top), |ty| ty.as_bounded())
            }
            _ => {
                // If we don't receive a generic type here we cannot simply return the actual type as a bound,
                // because e.g. Array<T> can never be satisfied. So instead we only return the type as bound,
                // if the type does not contain any inner generics.
                // TODO: It is possible to compute much tighter bounds here, e.g. `Array<Top` for `Array<T>`
                let mut visited_types = default();
                ty.as_view(ty_ctx).visit(&mut visited_types);
                if visited_types
                    .iter()
                    .any(|ty| matches!(ty, Type::Generic(_)))
                {
                    MaybeBounded::Bounded(Bound::Top)
                } else {
                    ty.as_bounded()
                }
            }
        }
    }

    /// Adds new information discovered about generic parameters to be used later.
    /// Performs inference recursively, e.g. `Array<T>` with `Array<I32>` infers `T=I32`.
    pub fn add_inferred(&mut self, expected: TypeView, received: TypeView) {
        match (expected, received) {
            (TypeView::Array(expected_inner), TypeView::Array(received_inner)) => {
                self.add_inferred(expected_inner.element(), received_inner.element())
            }
            (TypeView::Generic(generic), _) => self.add_generic(generic.value, received.as_type()),
            _ => {}
        }
    }

    fn add_generic(&mut self, generic: &GenericType, received: Type) {
        let existing_inference = self.inferred.insert(generic.id, received);
        if let Some(existing_inference) = existing_inference {
            assert_eq!(existing_inference, received, "TODO: Add error message");
        }
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

    pub fn into_inferred(self) -> Vec<Type> {
        let mut inferred = self.inferred.into_iter().collect::<Vec<_>>();
        inferred.sort_by_key(|(id, _)| id.0);
        inferred.into_iter().map(|(_, ty)| ty).collect()
    }
}
