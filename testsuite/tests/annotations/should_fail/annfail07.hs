module Annfail07 where
-- Testing normal type errors in annotations

{-# ANN module (head True) #-}

{-# ANN type Foo (head True) #-}
data Foo = Bar

{-# ANN f (head True) #-}
f x = x