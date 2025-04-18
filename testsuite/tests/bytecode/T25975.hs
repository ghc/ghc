-- Tests bytecode generation for tagToEnum# applied to literals
{-# LANGUAGE MagicHash #-}
module Main (main) where

import GHC.Exts

f1 :: Int# -> Bool
{-# OPAQUE f1 #-}
f1 v = case v of
  4# -> tagToEnum# v
  _  -> False

f2 :: Int# -> Bool
{-# OPAQUE f2 #-}
f2 v = case v of
  5# -> tagToEnum# 6#
  _  -> True

f3 :: Ordering
f3 = tagToEnum# (noinline runRW# (\_ -> 1#))


main :: IO ()
main = do
  print $ f1 2#
  print $ f2 3#
  print f3
