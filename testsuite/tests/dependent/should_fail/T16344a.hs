{-# LANGUAGE TypeInType, KindSignatures #-}

module T16344 where

import Data.Kind

-- This one is rejected, but in the typechecking phase
-- which is a bit nasty.
-- See Note [No polymorphic recursion] in GHC.Tc.Gen.HsType

data T2 ka (a::ka) = MkT2 (T2 Type a)
