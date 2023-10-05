{-# LANGUAGE GADTs, MagicHash #-}

module T12729 where

import GHC.Exts

newtype A where
   MkA :: Int# -> A

newtype B = MkB Int#
