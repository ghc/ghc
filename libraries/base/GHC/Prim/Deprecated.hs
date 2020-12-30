{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Provide some functions with the same names and interfaces as removed
-- primops.
module GHC.Prim.Deprecated
  (
  -- narrowing ops
    narrow8Int#
  , narrow16Int#
  , narrow32Int#
  , narrow8Word#
  , narrow16Word#
  , narrow32Word#
  ) where

import GHC.Prim
import GHC.Types () -- Make implicit dependency known to build system

default () -- Double and Integer aren't available yet

narrow8Int#   :: Int# -> Int#
narrow8Int# i = i `andI#` 0xFF#

narrow16Int#  :: Int# -> Int#
narrow16Int# i = i `andI#` 0xFFFF#

narrow32Int#  :: Int# -> Int#
narrow32Int# i = i `andI#` 0xFFFFFFFF#

narrow8Word#  :: Word# -> Word#
narrow8Word# i = i `and#` 0xFF##

narrow16Word# :: Word# -> Word#
narrow16Word# i = i `and#` 0xFFFF##

narrow32Word# :: Word# -> Word#
narrow32Word# i = i `and#` 0xFFFFFFFF##
