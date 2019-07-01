{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language TypeApplications #-}

import GHC.Exts
import GHC.IO (IO(..))
import Control.Exception (SomeException,toException,try)
import GHC.IO.Exception (BoundsCheckException(BoundsCheckException))
import System.IO.Error (userError)
import Foreign.Storable (sizeOf)

data IntArray = IntArray ByteArray#
data MutableIntArray = MutableIntArray (MutableByteArray# RealWorld)

main :: IO ()
main = do
  arr <- newIntArray 5
  writeIntArray arr 0 47
  writeIntArray arr 1 49
  writeIntArray arr 2 51
  writeIntArray arr 3 53
  writeIntArray arr 4 55
  demandException =<< try (readIntArray arr (-1))
  demandException =<< try (readIntArray arr 5)
  demandValue 47 =<< readIntArray arr 0
  demandValue 55 =<< readIntArray arr 4

demandException :: Either BoundsCheckException a -> IO ()
demandException (Left BoundsCheckException) = pure ()
demandException (Right _) = fail "expected exception"

demandValue :: Int -> Int -> IO ()
demandValue expected actual = if expected == actual
  then pure ()
  else fail $ "expected " ++ show expected ++ " but got " ++ show actual

sizeOfInt :: Int
sizeOfInt = sizeOf @Int undefined

unInt :: Int -> Int#
unInt (I# i) = i

-- This implementation is only correct on a 64-bit platform
newIntArray :: Int -> IO MutableIntArray
newIntArray (I# n#)
  = IO (\s# ->
      case newByteArray# (n# *# unInt sizeOfInt) s# of
        (# s'#, arr# #) -> (# s'#, MutableIntArray arr# #)
    )

-- | Read a primitive value from the primitive array.
indexIntArray :: IntArray -> Int -> Int
indexIntArray (IntArray arr#) (I# i#) = I# (indexIntArray# arr# i#)

readIntArray :: MutableIntArray -> Int -> IO Int
readIntArray (MutableIntArray arr#) (I# i#) = IO $ \s0 ->
  case readIntArray# arr# i# s0 of
    (# s1, r #) -> (# s1, I# r #)

-- | Write an element to the given index.
writeIntArray ::
     MutableIntArray -- ^ array
  -> Int -- ^ index
  -> Int -- ^ element
  -> IO ()
writeIntArray (MutableIntArray arr#) (I# i#) (I# x) = IO $ \s0 ->
  case writeIntArray# arr# i# x s0 of
    s1 -> (# s1, () #)

-- | Convert a mutable byte array to an immutable one without copying. The
-- array should not be modified after the conversion.
unsafeFreezeIntArray :: MutableIntArray -> IO IntArray
unsafeFreezeIntArray (MutableIntArray arr#) = IO $ \s# ->
  case unsafeFreezeByteArray# arr# s# of
    (# s'#, arr'# #) -> (# s'#, IntArray arr'# #)
