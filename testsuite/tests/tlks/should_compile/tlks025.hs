{-# LANGUAGE TopLevelKindSignatures #-}
{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module TLKS_025 where

import Data.Kind

data P (a :: k) = MkP

type C :: j -> Constraint
class C a where
  type T a b (c :: P p)
