{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash, UnliftedFFITypes #-}
module T7575 where

import GHC.Prim
import GHC.Word
import GHC.Types

foreign import ccall unsafe "hs_eqWord64" dummy_eqWord64# :: Word64# -> Word64# -> Bool

check :: Word64 -> Word64 -> Bool
check (W64# x#) (W64# y#) = dummy_eqWord64# x# y#

check2 :: Word64 -> Bool
check2 x = check x 0

