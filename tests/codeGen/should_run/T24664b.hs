-- This is a variant of T24664a that could reproduce
-- the compiler crash originally observed in #24664.

{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts (Int64#, intToInt64#, uncheckedIShiftRL64#)

takesInt64a :: String -> Int64# -> String -> IO ()
{-# OPAQUE takesInt64a #-}
-- Idea: This function takes an Int64# but doesn't use it,
-- so that its argument might be turned into a rubbish literal.
-- We don't want WW to remove the argument entirely, so OPAQUE
takesInt64a str1 _ str2 = putStrLn str1 >> putStrLn str2

takesInt64b :: String -> Int64# -> String -> IO ()
{-# NOINLINE takesInt64b #-}
-- Idea: This function will get a worker that doesn't take an
-- Int64# at all, and the body of that worker will pass a
-- rubbish literal to takesInt64a since no real arg exists.
takesInt64b s1 x s2
  = takesInt64a (s1 ++ t) (x `uncheckedIShiftRL64#` 13#) (s2 ++ t)
  where t = " string to print"

takesInt64c :: Int64# -> IO ()
takesInt64c x = takesInt64b "first" x "second"

main :: IO ()
main = do
  takesInt64c (intToInt64# 12345#)
