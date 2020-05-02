{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
module T16245 where

import Data.Kind

type Const a b = a
type SameKind (a :: k) (b :: k) = (() :: Constraint)
class (forall (b :: k). SameKind a b) => C (k :: Const Type a)
