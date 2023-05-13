{-# LANGUAGE
    DerivingStrategies
  , MagicHash
  , UnliftedFFITypes
 #-}

module DCo_Array_aux
  ( memcpy_thaw ) where

import Data.Word
  ( Word32 )
import GHC.Exts
  ( MutableByteArray#, ByteArray#
  , Ptr
  )

newtype CSize = CSize Word32
  deriving newtype Num

foreign import ccall unsafe "memcpy"
    memcpy_thaw :: MutableByteArray# s -> ByteArray# -> CSize -> IO (Ptr a)
