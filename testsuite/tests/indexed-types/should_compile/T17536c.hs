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
