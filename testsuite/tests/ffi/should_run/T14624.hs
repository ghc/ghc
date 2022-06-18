{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import GHC.Exts
import GHC.Float
import GHC.Int
import GHC.IO
import GHC.Word
import Foreign.C.Types

foreign import capi "T14624_c.h f_char"       c_f_char       :: Char# -> Int#
foreign import capi "T14624_c.h f_int"        c_f_int        :: Int# -> Int#
foreign import capi "T14624_c.h f_int8"       c_f_int8       :: Int8# -> Int8#
foreign import capi "T14624_c.h f_int16"      c_f_int16      :: Int16# -> Int16#
foreign import capi "T14624_c.h f_int32"      c_f_int32      :: Int32# -> Int32#
foreign import capi "T14624_c.h f_int64"      c_f_int64      :: Int64# -> Int64#
foreign import capi "T14624_c.h f_word"       c_f_word       :: Word# -> Word#
foreign import capi "T14624_c.h f_word8"      c_f_word8      :: Word8# -> Word8#
foreign import capi "T14624_c.h f_word16"     c_f_word16     :: Word16# -> Word16#
foreign import capi "T14624_c.h f_word32"     c_f_word32     :: Word32# -> Word32#
foreign import capi "T14624_c.h f_word64"     c_f_word64     :: Word64# -> Word64#
foreign import capi "T14624_c.h f_float"      c_f_float      :: Float# -> Float#
foreign import capi "T14624_c.h f_double"     c_f_double     :: Double# -> Double# -> Double#
foreign import capi "T14624_c.h f_addr"       c_f_addr       :: Addr# -> Addr#
foreign import capi "T14624_c.h f_stable_ptr" c_f_stable_ptr :: StablePtr# a -> StablePtr# a

main :: IO ()
main = do
    print (I#   (c_f_char  '\0'#))
    print (I#   (c_f_int   (case maxBound @Int of (I# i) -> i)))
    print (I8#  (c_f_int8  (intToInt8# 127#)))
    print (I16# (c_f_int16 (intToInt16# 32767#)))
    print (I32# (c_f_int32 (intToInt32# 2147483647#)))
    print (I64# (c_f_int64 (case maxBound @Int64 of (I64# i) -> i)))

    print (W#   (c_f_word                   (case maxBound @Word of (W# i) -> i)))
    print (W8#  (c_f_word8  (int8ToWord8#   (intToInt8# 255#))))
    print (W16# (c_f_word16 (int16ToWord16# (intToInt16# 65535#))))
    print (W32# (c_f_word32 (int32ToWord32# (intToInt32# 4294967295#))))
    print (W64# (c_f_word64 (int64ToWord64# (intToInt64# 18446744073709551615#))))

    print (F# (c_f_float 3.0#))
    print (D# (c_f_double 909.0## 909.0##))

    print (I# (eqAddr# (c_f_addr nullAddr#) "909"#))
    let io = IO $ \s0 -> case makeStablePtr# () s0 of
            (# s1, sp #) -> (# s1, I# ((eqStablePtr# (c_f_stable_ptr sp) sp) -# 1#) #)
    io >>= print
