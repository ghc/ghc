-- Alternative.empty is not visible
module BugReproduce where

import Control.Applicative (Alternative)

newtype Foo a = MkFoo [a] deriving (Functor, Applicative)

instance Alternative Foo where
  empty = undefined
