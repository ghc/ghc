{-# LANGUAGE
    MagicHash
  , UnboxedTuples
  , UnliftedFFITypes
 #-}

module DCo_Array where

import DCo_Array_aux
  ( memcpy_thaw )

import GHC.Exts
  ( Ptr, Int(I#), RealWorld
  , MutableByteArray#, ByteArray#
  , newByteArray#
  )
import GHC.IO ( IO(..) )
import GHC.ST ( ST(..) )

data UArray e = UArray !Int ByteArray#
data STUArray s e = STUArray !Int (MutableByteArray# s)

thawSTUArray :: UArray e -> ST RealWorld (STUArray RealWorld e)
thawSTUArray (UArray n@(I# n#) arr#) = ST $ \s1# ->
  case newByteArray# n# s1# of
    (# s2#, marr# #) ->
      case memcpy_thaw marr# arr# (fromIntegral n) of
        IO m ->
          case m s2# of
            (# s3#, _ #) ->
              (# s3#, STUArray n marr# #)
