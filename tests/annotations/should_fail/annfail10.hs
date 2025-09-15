module Annfail10 where
-- Testing ambiguity in annotations

{-# ANN module 1 #-}

{-# ANN type Foo 1 #-}
data Foo = Bar

{-# ANN f Nothing #-}
f x = x
