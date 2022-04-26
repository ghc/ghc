{-# LANGUAGE DataKinds, GHCForeignImportPrim #-}

module T21305_fail where

import GHC.Exts

foreign import prim "f" f :: Any @(TYPE IntRep) -> Any
