{-# LANGUAGE MagicHash #-}
module T17940 where

import GHC.Exts

index# :: ByteArray# -> Int# -> Word8#
index# a i = _ (indexWord8Array# a i)
