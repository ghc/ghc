{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- | This test ensures that sub-word signed and unsigned parameters are correctly
-- handed over to C functions. I.e. it asserts the calling-convention.
--
-- The number of parameters is currently shaped for the RISCV64 calling-convention.
-- You may need to add more parameters to the C functions in case there are more
-- registers reserved for parameters in your architecture.
module Main where

import Data.Word
import GHC.Exts
import GHC.Int

foreign import ccall "fun8"
  fun8 ::
    Int8# -> -- a0
    Word8# -> -- a1
    Int8# -> -- a2
    Int8# -> -- a3
    Int8# -> -- a4
    Int8# -> -- a5
    Int8# -> -- a6
    Int8# -> -- a7
    Word8# -> -- s0
    Int8# -> -- s1
    Int64# -- result

foreign import ccall "fun16"
  fun16 ::
    Int16# -> -- a0
    Word16# -> -- a1
    Int16# -> -- a2
    Int16# -> -- a3
    Int16# -> -- a4
    Int16# -> -- a5
    Int16# -> -- a6
    Int16# -> -- a7
    Word16# -> -- s0
    Int16# -> -- s1
    Int64# -- result

foreign import ccall "fun32"
  fun32 ::
    Int32# -> -- a0
    Word32# -> -- a1
    Int32# -> -- a2
    Int32# -> -- a3
    Int32# -> -- a4
    Int32# -> -- a5
    Int32# -> -- a6
    Int32# -> -- a7
    Word32# -> -- s0
    Int32# -> -- s1
    Int64# -- result

main :: IO ()
main =
  -- N.B. the values here aren't choosen by accident: -1 means all bits one in
  -- twos-complement, which is the same as the max word value.
  let i8 :: Int8# = intToInt8# (-1#)
      w8 :: Word8# = wordToWord8# (255##)
      res8 :: Int64# = fun8 i8 w8 i8 i8 i8 i8 i8 i8 w8 i8
      expected_res8 :: Int64 = 2 * (fromInteger . fromIntegral) (maxBound :: Word8) + 8 * (-1)
      i16 :: Int16# = intToInt16# (-1#)
      w16 :: Word16# = wordToWord16# (65535##)
      res16 :: Int64# = fun16 i16 w16 i16 i16 i16 i16 i16 i16 w16 i16
      expected_res16 :: Int64 = 2 * (fromInteger . fromIntegral) (maxBound :: Word16) + 8 * (-1)
      i32 :: Int32# = intToInt32# (-1#)
      w32 :: Word32# = wordToWord32# (4294967295##)
      res32 :: Int64# = fun32 i32 w32 i32 i32 i32 i32 i32 i32 w32 i32
      expected_res32 :: Int64 = 2 * (fromInteger . fromIntegral) (maxBound :: Word32) + 8 * (-1)
   in do
        print $ "fun8 result:" ++ show (I64# res8)
        assertEqual expected_res8 (I64# res8)
        print $ "fun16 result:" ++ show (I64# res16)
        assertEqual expected_res16 (I64# res16)
        print $ "fun32 result:" ++ show (I64# res32)
        assertEqual expected_res32 (I64# res32)

assertEqual :: (Integral a, Integral b, Show a, Show b) => a -> b -> IO ()
assertEqual a b =
  if (fromIntegral a) == (fromIntegral b)
    then pure ()
    else error $ show a ++ " =/= " ++ show b
