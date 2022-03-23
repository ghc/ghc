{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import System.Environment
import GHC.Exts
import Data.Foldable

divInt# :: Int# -> Int#
divInt# y# = case y# <# 0# of
    0# -> 4#
    1# -> 0#

-- unI# :: Int -> Int#
-- unI# (I# i#) = i#
-- {-# INLINE unI# #-}

main = do
  [n] <- map (read @Int) <$> getArgs
  -- let r = foldl' (\acc v -> acc + (I# (divInt# (unI# v)))) 0 [negate n..n]
  -- print r
  print "======="
  -- forM_ [negate n .. n] $ \x -> do
  --   putStr ("For " ++ show x ++ " ")
  --   print (I# (divInt# (unI# x)))
