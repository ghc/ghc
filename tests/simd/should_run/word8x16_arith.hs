{-# LANGUAGE MagicHash, UnboxedTuples #-}
import Control.Monad
import Data.Array.Base
import Data.Word
import Foreign.Marshal.Array
import GHC.Int
import GHC.IO
import GHC.Prim
import GHC.Ptr

data Word8X16 = Word8X16 Word8X16#

indexAsWord8X16 :: UArray Int Word8 -> Int -> Word8X16#
indexAsWord8X16 (UArray l _ _ ba) i = case i - l of
  I# i# -> indexWord8ArrayAsWord8X16# ba i#

readAsWord8X16 :: Ptr Word8 -> Int -> IO Word8X16
readAsWord8X16 (Ptr addr) (I# i) = IO $ \s ->
  case readWord8OffAddrAsWord8X16# addr i s of
    (# s', v #) -> (# s', Word8X16 v #)

writeAsWord8X16 :: Ptr Word8 -> Int -> Word8X16# -> IO ()
writeAsWord8X16 (Ptr addr) (I# i) v = IO $ \s ->
  (# writeWord8OffAddrAsWord8X16# addr i v s, () #)

arr1 :: UArray Int Word8
arr1 = listArray (0,63) [5,1,2,2,2,92,0,12,246,2,5,127,1,4,5,3,2,3,2,3,235,2,137,3,3,1,0,3,63,1,149,0,253,1,1,2,127,1,0,2,3,7,2,3,4,3,127,0,185,0,1,2,167,0,3,1,71,4,25,223,5,1,1,208]

input2 :: [Word8]
input2 = [49,39,3,1,205,231,1,3,254,4,247,1,127,2,2,2,4,1,1,1,5,1,3,1,12,4,3,1,191,2,83,231,33,1,3,5,5,3,249,3,1,184,3,85,1,142,2,152,11,164,3,3,1,127,2,4,84,4,3,1,41,1,102,68]

run :: (Word8X16# -> Word8X16# -> Word8X16#) -> UArray Int Word8 -> Ptr Word8 -> IO [Word8]
run f a b = allocaArray 64 $ \result -> do
  forM_ [0,16..63] $ \i -> do
    let v = indexAsWord8X16 a i
    Word8X16 w <- readAsWord8X16 b i
    writeAsWord8X16 result i (f v w)
  peekArray 64 result
{-# INLINE run #-}

runN :: (Word8X16# -> Word8X16# -> Word8X16#) -> UArray Int Word8 -> Ptr Word8 -> IO [Word8]
runN f a b = allocaArray 64 $ \result -> do
  forM_ [0,16..63] $ \i -> do
    let v = indexAsWord8X16 a i
    Word8X16 w <- readAsWord8X16 b i
    writeAsWord8X16 result i (f v w)
  peekArray 64 result
{-# NOINLINE runN #-}

main :: IO ()
main = do
  withArray input2 $ \arr2 -> do
    run plusWord8X16# arr1 arr2 >>= print
    run minusWord8X16# arr1 arr2 >>= print
    run timesWord8X16# arr1 arr2 >>= print
    run quotWord8X16# arr1 arr2 >>= print
    run remWord8X16# arr1 arr2 >>= print
    run minWord8X16# arr1 arr2 >>= print
    run maxWord8X16# arr1 arr2 >>= print
    runN plusWord8X16# arr1 arr2 >>= print
    runN minusWord8X16# arr1 arr2 >>= print
    runN timesWord8X16# arr1 arr2 >>= print
    runN quotWord8X16# arr1 arr2 >>= print
    runN remWord8X16# arr1 arr2 >>= print
    runN minWord8X16# arr1 arr2 >>= print
    runN maxWord8X16# arr1 arr2 >>= print
    runN (\x y -> plusWord8X16# y x) arr1 arr2 >>= print
    runN (\x y -> minusWord8X16# y x) arr1 arr2 >>= print
    runN (\x y -> timesWord8X16# y x) arr1 arr2 >>= print
    runN (\x y -> minWord8X16# y x) arr1 arr2 >>= print
    runN (\x y -> maxWord8X16# y x) arr1 arr2 >>= print
