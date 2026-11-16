{-# LANGUAGE MagicHash #-}
module T28105b where

import GHC.Exts

-- I8[R2 + ((R3 << 2) + 4294967296)]
f :: Addr# -> Int# -> Word8#
f a i = indexWord8OffAddr# a (i *# 4# +# 4294967296#)

-- I8[(R2 + 4294967296) + (R3 << 2)]
g :: Addr# -> Int# -> Word8#
g a i = indexWord8OffAddr# (plusAddr# a 4294967296#) (i *# 4#)
