{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T14048c where

import Data.Kind

data family   Foo :: k
data instance Foo :: Constraint
