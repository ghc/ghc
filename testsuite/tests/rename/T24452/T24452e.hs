-- Multiple unrelated errors related to empty.
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}

module T24452e where

import Control.Applicative (Alternative)
import qualified Data.Map as Map
import qualified Data.Set as Set

data A = A {
    empty :: ()
}
data B = B {
    empty :: ()
}

foo = empty

newtype Foo a = MkFoo [a] deriving (Functor, Applicative)

instance Alternative Foo where
  empty = undefined