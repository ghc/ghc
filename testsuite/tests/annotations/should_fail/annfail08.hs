module Annfail08 where
-- Testing instance type errors in annotations

{-# ANN module (id + 1) #-}

{-# ANN type Foo (id + 1) #-}
data Foo = Bar

{-# ANN f (id + 1) #-}
f x = x