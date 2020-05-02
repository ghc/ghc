{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
module T16244 where

import Data.Kind

type Const a b = a
type SameKind (a :: k) (b :: k) = (() :: Constraint)
class SameKind a b => C (k :: Const Type a) (b :: k)
