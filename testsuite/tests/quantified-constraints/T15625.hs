{-# Language GADTs, MultiParamTypeClasses, QuantifiedConstraints #-}

module T15625 where

import Data.Coerce

class a ~ b => Equal a b

test1 :: (forall b. a ~ b) => a
test1 = False

test2 :: (forall b. Equal a b) => a
test2 = False

test3 :: (forall b. Coercible a b) => a
test3 = coerce False
