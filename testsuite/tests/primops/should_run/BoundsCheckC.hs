{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

import Control.Monad (when)
import GHC.Exts
import GHC.IO (IO(..))
import GHC.Word (Word16(W16#))
import Control.Exception (SomeException,toException,try)
import GHC.IO.Exception (BoundsCheckException(BoundsCheckException))
import System.IO.Error (userError)
import Foreign.Storable (sizeOf)

data SmallMutableArray a = SmallMutableArray (SmallMutableArray# RealWorld a)

main :: IO ()
main = do
  arr <- newArray 10 (42 :: Integer)
  writeArray arr 0 (5 :: Integer)
  writeArray arr 9 (13 :: Integer)
  r0 <- readArray arr 0
  r1 <- readArray arr 8
  r2 <- readArray arr 9
  when (r0 + r1 + r2 /= 60) (fail "bad")

writeArray ::
     SmallMutableArray a -- ^ array
  -> Int -- ^ index
  -> a -- ^ element
  -> IO ()
writeArray (SmallMutableArray arr#) (I# i#) x = IO $ \s0 ->
  case writeSmallArray# arr# i# x s0 of
    s1 -> (# s1, () #)

readArray :: SmallMutableArray a -> Int -> IO a
readArray (SmallMutableArray arr#) (I# i#) = IO $ \s0 ->
  case readSmallArray# arr# i# s0 of
    (# s1, r #) -> (# s1, r #)

newArray :: Int -> a -> IO (SmallMutableArray a)
newArray (I# n#) a = IO $ \s# ->
  case newSmallArray# n# a s# of
    (# s'#, arr# #) -> (# s'#, SmallMutableArray arr# #)
