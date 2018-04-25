{-# LANGUAGE TypeFamilies, ConstraintKinds  #-}

module Foo where

import GHC.Exts

f :: F [a] => a -> Bool
f x = x == x

type family F a :: Constraint
type instance F [a] = (Show a, (Show a, (Show a, (Show a, (Show a,
                       Show a, (Show a, (Show a, (Show a, (Show a, Eq a)))))))))
