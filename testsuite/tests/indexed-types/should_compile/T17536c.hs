{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module T17536c where

import Data.Kind
import GHC.Exts

type R :: forall (r :: RuntimeRep) -> TYPE r -> Type
type family R r a where
  R _ _ = Int

r :: R FloatRep Float# -> Int
r x = x

-- make sure wildcard and non-wildcard type variables are treated the same
type R1 :: forall (r :: RuntimeRep) -> TYPE r -> Type
type family R1 r a where
  R1 r a = Int

r1 :: R1 FloatRep Float# -> Int
r1 x = x
