{-# LANGUAGE MagicHash, UnboxedTuples #-}
import Control.Monad
import Data.Array.Base
import Data.Word
import Foreign.Marshal.Array
import GHC.Int
import GHC.IO
import GHC.Prim
import GHC.Ptr

data Word32X4 = Word32X4 Word32X4#

indexAsWord32X4 :: UArray Int Word32 -> Int -> Word32X4#
indexAsWord32X4 (UArray l _ _ ba) i = case i - l of
  I# i# -> indexWord32ArrayAsWord32X4# ba i#

readAsWord32X4 :: Ptr Word32 -> Int -> IO Word32X4
readAsWord32X4 (Ptr addr) (I# i) = IO $ \s ->
  case readWord32OffAddrAsWord32X4# addr i s of
    (# s', v #) -> (# s', Word32X4 v #)

writeAsWord32X4 :: Ptr Word32 -> Int -> Word32X4# -> IO ()
writeAsWord32X4 (Ptr addr) (I# i) v = IO $ \s ->
  (# writeWord32OffAddrAsWord32X4# addr i v s, () #)

arr1 :: UArray Int Word32
arr1 = listArray (0,63) [232,0,5,515,1367438610,0,4211631663,2,351,2147483647,758,2,5,2,4,1,40,4,1,810,4,3108178225,5,1319199926,3,143,2147483647,2,3458651811,2,0,261,741,1,3,2,902769101,364643313,3,5,2,2147483647,4,2147483647,1494758316,1,216,3,597,3158013076,776,0,4,4,0,1,5,1,1021662415,0,3516158976,2,222,0]

input2 :: [Word32]
input2 = [4,1231395530,1,1014,4286336388,661,5,2,2,243,3,440,2147483647,2,600,4,2611184182,3,2472420436,462,67,5,227,644,3719807932,2712747774,5,4,533,5,1757908327,4,387,2,2,1,913,2382395048,1325017807,534,1,383,161,3614956473,1,2016887931,1,931,459096861,5,1,3,4,4,4,231,3971703300,5,454561961,2147483647,2,1,712,597]

run :: (Word32X4# -> Word32X4# -> Word32X4#) -> UArray Int Word32 -> Ptr Word32 -> IO [Word32]
run f a b = allocaArray 64 $ \result -> do
  forM_ [0,4..63] $ \i -> do
    let v = indexAsWord32X4 a i
    Word32X4 w <- readAsWord32X4 b i
    writeAsWord32X4 result i (f v w)
  peekArray 64 result
{-# INLINE run #-}

runN :: (Word32X4# -> Word32X4# -> Word32X4#) -> UArray Int Word32 -> Ptr Word32 -> IO [Word32]
runN f a b = allocaArray 64 $ \result -> do
  forM_ [0,4..63] $ \i -> do
    let v = indexAsWord32X4 a i
    Word32X4 w <- readAsWord32X4 b i
    writeAsWord32X4 result i (f v w)
  peekArray 64 result
{-# NOINLINE runN #-}

main :: IO ()
main = do
  withArray input2 $ \arr2 -> do
    run plusWord32X4# arr1 arr2 >>= print
    run minusWord32X4# arr1 arr2 >>= print
    run timesWord32X4# arr1 arr2 >>= print
    run quotWord32X4# arr1 arr2 >>= print
    run remWord32X4# arr1 arr2 >>= print
    run minWord32X4# arr1 arr2 >>= print
    run maxWord32X4# arr1 arr2 >>= print
    runN plusWord32X4# arr1 arr2 >>= print
    runN minusWord32X4# arr1 arr2 >>= print
    runN timesWord32X4# arr1 arr2 >>= print
    runN quotWord32X4# arr1 arr2 >>= print
    runN remWord32X4# arr1 arr2 >>= print
    runN minWord32X4# arr1 arr2 >>= print
    runN maxWord32X4# arr1 arr2 >>= print
    runN (\x y -> plusWord32X4# y x) arr1 arr2 >>= print
    runN (\x y -> minusWord32X4# y x) arr1 arr2 >>= print
    runN (\x y -> timesWord32X4# y x) arr1 arr2 >>= print
    runN (\x y -> minWord32X4# y x) arr1 arr2 >>= print
    runN (\x y -> maxWord32X4# y x) arr1 arr2 >>= print
