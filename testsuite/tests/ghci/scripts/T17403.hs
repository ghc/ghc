{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
module T17403 where

import Data.Kind

f :: (() :: Constraint) => String
f = "hello world"
