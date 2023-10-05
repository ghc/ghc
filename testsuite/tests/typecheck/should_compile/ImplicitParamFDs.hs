{-# LANGUAGE ImplicitParams #-}

module ImplicitParamFDs where

import Data.Char

bar :: (?x::Bool) => Bool
bar = ?x

foo :: (?x::Int) => Bool
foo = let ?x = True in bar

quux :: (?x :: Char) => (Int, Bool)
quux = (ord ?x, let ?x = True in ?x)

flub :: (?x :: Int) => (Int, Integer)
flub = (?x, let ?x = 5 in ?x)
