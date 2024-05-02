-- This program tests the passing of RUBBISH values
-- with the Int64 representation, which were found
-- to by mis-handled by the JS backend in #24664.

{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts (Int64#, intToInt64#)

takesInt64a :: String -> Int64# -> String -> IO ()
{-# OPAQUE takesInt64a #-}
-- Idea: This function takes an Int64# but doesn't use it,
-- so that its argument might be turned into a rubbish literal.
-- We don't want WW to remove the argument entirely, so OPAQUE
takesInt64a str1 _ str2 = putStrLn str1 >> putStrLn str2

takesInt64b :: Int64# -> IO ()
{-# NOINLINE takesInt64b #-}
-- Idea: This function will get a worker that doesn't take an
-- Int64# at all, and the body of that worker will pass a
-- rubbish literal to takesInt64a since no real arg exists.
takesInt64b x = takesInt64a "first string to print" x "second string to print"

main :: IO ()
main = do
  takesInt64b (intToInt64# 12345#)
