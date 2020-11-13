{-# OPTIONS_GHC -O2 -fforce-recomp #-}

module T18870 where

import GHC.Exts

-- This function should not lead to an "Exciting arity" DEBUG message.
-- It should only do one round of fixed-point iteration to conclude that it has
-- arity 2.
f :: [a] -> a -> a
f []     = id
f (x:xs) = oneShot (\_ -> f xs x)
