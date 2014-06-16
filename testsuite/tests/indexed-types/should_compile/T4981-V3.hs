{-# LANGUAGE CPP, TypeFamilies, FlexibleContexts #-}
module Class ( cleverNamedResolve ) where

data FL p = FL p

class PatchInspect p where
instance PatchInspect p => PatchInspect (FL p) where

type family PrimOf p
type instance PrimOf (FL p) = PrimOf p

data WithName prim = WithName prim

instance PatchInspect prim => PatchInspect (WithName prim) where

class (PatchInspect (PrimOf p)) => Conflict p where
    resolveConflicts :: p -> PrimOf p

instance Conflict p => Conflict (FL p) where
    resolveConflicts = undefined

type family OnPrim p

joinPatches :: p -> p

joinPatches = id

cleverNamedResolve :: (Conflict (OnPrim p)
                      ,PrimOf (OnPrim p) ~ WithName (PrimOf p))
                   => p -> FL (OnPrim p) -> WithName (PrimOf p)
cleverNamedResolve x = resolveConflicts . joinPatches
-- I added the parameter 'x' to make the signature unambiguous
-- I don't think that ambiguity is essential to the original problem

{-
resolveConflicts :: q -> PrimOf q
 (w) FL (OnPrim p) ~ q
 (w) WithName (PrimOf p) ~ PrimOf q
==>
 (w) PrimOf (OnPrim p) ~ PrimOf (FL (OnPrim p))
==>
 (w) PrimOf (OnPrim p) ~ PrimOf (OnPrim p)

-}