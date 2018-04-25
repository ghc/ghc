module Annfail01 where
-- Testing annotating things that don't exist

{-# ANN type Foo (1 :: Int) #-}
{-# ANN f (1 :: Int) #-}