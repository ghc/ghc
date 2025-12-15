{-# LANGUAGE TypeFamilies #-}
module T7404 where

import Data.Kind (Type)

type family Foo (x :: Type) (y :: x)
