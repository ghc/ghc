{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}

import GHC.Int
import GHC.Exts

foreign import prim "byte_switch" byteSwitch:: Int8# -> Int8#

main = traverse (\(I8# x#) -> print $ I8# (byteSwitch x#)) [-4..4]
