{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeData #-}
module TDTagToEnum where

import GHC.Exts (tagToEnum#)

type data Letter = A | B | C

f :: Letter
f = tagToEnum# 0#
