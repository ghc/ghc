{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
-- Test case for #5681
module Main where

import GHC.Exts

work :: Int -> Int
work n = work (n-1)

main :: IO ()
main = case spark# (work 2) realWorld# of
           (# _, _ #) -> case par# (work 1) of
                             _ -> return ()

