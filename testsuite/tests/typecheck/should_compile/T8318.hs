{-# LANGUAGE MagicHash #-}

module T8318 where

import GHC.Exts

f :: String
f = case tagToEnum# 1# of
  True  -> "True"
  False -> "False"
