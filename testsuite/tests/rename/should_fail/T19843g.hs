{-# LANGUAGE NoFieldSelectors #-}

module T19843g where

import Prelude (Int, map)

data A = A {mop :: ()}
data Mup = Mup

foo = foo {mup = 1}
