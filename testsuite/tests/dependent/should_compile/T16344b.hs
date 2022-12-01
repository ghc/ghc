{-# LANGUAGE PolyKinds, GADTs, KindSignatures #-}

module T16344 where

import Data.Kind

-- This one is accepted, even though it is polymorphic-recursive.
-- See Note [No polymorphic recursion in type decls] in GHC.Tc.TyCl

data T3 ka (a::ka) = forall b. MkT3 (T3 Type b)
