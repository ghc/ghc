{-# LANGUAGE PartialTypeSignatures #-}
module PprInfixHole where

f1 a b = a `_` b
f2 a b = a ` _ ` b

t1 :: Int `_` Bool
t2 :: Int ` _ ` Bool
t1 = Left 0
t2 = Left 0
