{-# LANGUAGE MagicHash #-}

module T25976 where

import GHC.PrimOps (tagToEnum#)

-- Spoiler - it's all dead code since tagToEnum 3# is undefined
main = case (tagToEnum# 4# :: Bool) of
  True -> print "Dead Code"
  False -> print "Dead Code"
