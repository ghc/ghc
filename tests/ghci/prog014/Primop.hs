{-# LANGUAGE GHCForeignImportPrim, MagicHash,
             UnliftedFFITypes, UnboxedTuples #-}

import GHC.Exts

foreign import prim "dummy"
   dummy :: Word# -> Word#

foreign import prim "dummy2"
   dummy2 :: Any -> State# RealWorld -> (# State# RealWorld, Word# #)
