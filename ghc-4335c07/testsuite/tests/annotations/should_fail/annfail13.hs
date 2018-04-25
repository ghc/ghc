module Annfail13 where
-- Testing that brackets are mandatory in the ANN syntax

{-# ANN f id 1 #-}
{-# ANN f 1 :: Int #-}
f x = x