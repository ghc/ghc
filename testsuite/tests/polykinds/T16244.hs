{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeAbstractions #-}
module T16244 where

import Data.Kind

type Const a b = a

type SameKind :: k -> k -> Constraint
type SameKind a b = ()

type C :: forall a. forall (k :: Const Type a) -> k -> Constraint
class SameKind a b => C @a k b
