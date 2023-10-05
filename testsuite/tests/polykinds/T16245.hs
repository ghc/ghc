{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeAbstractions #-}
module T16245 where

import Data.Kind

type Const a b = a

type SameKind :: k -> k -> Constraint
type SameKind a b = ()

type C :: forall a. Const Type a -> Constraint
class (forall (b :: k). SameKind a b) => C @a k
