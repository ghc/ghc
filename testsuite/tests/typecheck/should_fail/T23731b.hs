module T23731b where

import T23731b_aux (Foo) -- NB: not importing the constructor

import Data.Coerce (coerce)
import Data.Monoid (Sum(..))

foo :: Foo Int -> Foo (Sum Int)
foo = coerce
