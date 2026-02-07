use stdx::FastMap;

use crate::auryn::air::typecheck::{
    bounds::{Bound, MaybeBounded},
    type_context::TypeContext,
    types::{GenericId, Type},
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
            _ => ty.as_bounded(),
        }
    }

    /// Adds new information discovered about generic parameters to be used later.
    pub fn add_inferred(&mut self, ty_ctx: &TypeContext, expected: Type, received: Type) {
        if let Type::Generic(generic) = expected {
            let generic = ty_ctx.get(generic);
            let existing_inference = self.inferred.insert(generic.id, received);
            if let Some(existing_inference) = existing_inference {
                assert_eq!(existing_inference, received, "TODO: Add error message");
            }
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
