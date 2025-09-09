{-# LANGUAGE MagicHash #-}
module T25718a where

import GHC.Exts

-- Test that value range analysis is used to remove unreachable alternatives

foo :: Word8# -> Bool
foo x = case word8ToWord# x of
  123456## -> False
  456789## -> False
  42##     -> False
  _        -> True

bar :: Word# -> Bool
bar x = case x `and#` 0xF## of
  123456## -> False
  456789## -> False
  0xF0##   -> False
  0x05##   -> False
  _        -> True
