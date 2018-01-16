module Annfail09 where
-- Testing that we detect references to ids defined in module being compiled in annotations

g = 10

{-# ANN module g #-}

{-# ANN type Foo g #-}
data Foo = Bar

{-# ANN f g #-}
f x = x