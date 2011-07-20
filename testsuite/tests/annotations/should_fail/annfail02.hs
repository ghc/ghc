module Annfail02 where
-- Testing annotating things that don't exist in the right namespace

data Foo = Bar

{-# ANN Foo (1 :: Int) #-}
{-# ANN type Bar (2 :: Int) #-}