{-# OPTIONS_GHC -fspecialise-aggressively #-}
{-# OPTIONS_GHC -fno-spec-constr #-}

module SpecTyFam(main, foo) where

import SpecTyFam_Import (specMe, MaybeShowNum)
import GHC.Exts

-- We want to see a specialization of `specMe` which doesn't take a dictionary at runtime.

{-# OPAQUE foo #-}
foo :: Int -> (String,Int)
foo x = specMe True x

main = print $ sum $ map (snd . foo) [1..1000 :: Int]
