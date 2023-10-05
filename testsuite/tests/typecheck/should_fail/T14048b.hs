{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T14048b where

import Data.Kind

data family Foo :: Constraint
