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

class FromPrims p where

instance FromPrims (FL p) where

joinPatches :: FromPrims p => p -> p
joinPatches = id 

cleverNamedResolve :: (Conflict (OnPrim p)
                      ,PrimOf (OnPrim p) ~ WithName (PrimOf p))
                   => p -> FL (OnPrim p) -> WithName (PrimOf p)
cleverNamedResolve x = resolveConflicts . joinPatches
