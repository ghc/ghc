{-# LANGUAGE DataKinds, GHCForeignImportPrim #-}

module T21305_fail where

import GHC.Exts

foreign import prim "g" g :: forall (l :: Levity). Any @(TYPE (BoxedRep l)) -> Any

foreign import prim "f" f :: Any @(TYPE IntRep) -> Any
