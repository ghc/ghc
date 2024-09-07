{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
-- Test case for #5681

{-# OPTIONS_GHC -Wno-deprecations #-}
  -- The use of the (now-deprecated) primop par# is an
  -- essential part of the issue that this is meant to test.
module Main where

import GHC.Exts

work :: Int -> Int
work n = work (n-1)

main :: IO ()
main = case spark# (work 2) realWorld# of
           (# _, _ #) -> case par# (work 1) of
                             _ -> return ()

