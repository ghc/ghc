{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language UnliftedFFITypes #-}
{-# language ForeignFunctionInterface #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

-- Test for shims when passing a ByteArray# to a foreign function.
-- The bad behavior here was initially observed in the MR
-- https://gitlab.haskell.org/ghc/ghc/merge_requests/939,
-- but this test has been named after issue #16650 since it
-- is closely related to the unexpected behavior there.

import GHC.Exts
import GHC.Word
import GHC.IO
import Data.Kind (Type)

main :: IO ()
main = do
  mb0 <- luckySingleton
  print =<< readByteArray mb0 0
  case box mb0 of
    Box x -> print =<< c_head_bytearray (unsafeCoerce# x)

foreign import ccall unsafe "head_bytearray"
  c_head_bytearray :: MutableByteArray# RealWorld -> IO Word8

data Box :: Type where
  Box :: (Any :: TYPE 'UnliftedRep) -> Box

data MutableByteArray :: Type where
  MutableByteArray :: MutableByteArray# RealWorld -> MutableByteArray

box :: MutableByteArray -> Box
{-# noinline box #-}
box (MutableByteArray x) = Box (unsafeCoerce# x)

luckySingleton :: IO MutableByteArray
luckySingleton = IO $ \s0 -> case newByteArray# 1# s0 of
  (# s1, marr# #) -> case writeWord8Array# marr# 0# 42## s1 of
    s2 -> (# s2, MutableByteArray marr# #)

readByteArray :: MutableByteArray -> Int -> IO Word8
readByteArray (MutableByteArray b#) (I# i#) = IO $ \s0 ->
  case readWord8Array# b# i# s0 of
    (# s1, w #) -> (# s1, W8# w #)
