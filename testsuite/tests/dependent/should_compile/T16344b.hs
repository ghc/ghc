{-# LANGUAGE TypeInType, GADTs, KindSignatures #-}

module T16344 where

import Data.Kind

-- This one is accepted, even though it is polymorphic-recursive.
-- See Note [No polymorphic recursion] in GHC.Tc.Gen.HsType

data T3 ka (a::ka) = forall b. MkT3 (T3 Type b)
