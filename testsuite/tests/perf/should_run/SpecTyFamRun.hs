{-# OPTIONS_GHC -fspecialise-aggressively #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
module Main(main) where

import SpecTyFam_Import (specMe, MaybeShowNum)
import GHC.Exts

-- We want to see a specialization of `specMe` which doesn't take a dictionary at runtime.

{-# NOINLINE foo #-}
foo :: Int -> (String,Int)
-- We want specMe to be specialized, but not inlined
foo x = specMe True x

main = print $ sum $ map (snd . foo) [1..1000 :: Int]
