{-# LANGUAGE MagicHash, UnboxedTuples #-}
-- Test case for Trac #5681
module Main where

import GHC.Prim

work :: Int -> Int
work n = work (n-1)

main :: IO ()
main = case spark# (work 2) realWorld# of
           (# _, _ #) -> case par# (work 1) of
                             _ -> return ()

