{-# LANGUAGE MagicHash, UnboxedTuples #-}
import Control.Monad
import Data.Array.Base
import Data.Word
import Foreign.Marshal.Array
import GHC.Int
import GHC.IO
import GHC.Prim
import GHC.Ptr

data Word16X8 = Word16X8 Word16X8#

indexAsWord16X8 :: UArray Int Word16 -> Int -> Word16X8#
indexAsWord16X8 (UArray l _ _ ba) i = case i - l of
  I# i# -> indexWord16ArrayAsWord16X8# ba i#

readAsWord16X8 :: Ptr Word16 -> Int -> IO Word16X8
readAsWord16X8 (Ptr addr) (I# i) = IO $ \s ->
  case readWord16OffAddrAsWord16X8# addr i s of
    (# s', v #) -> (# s', Word16X8 v #)

writeAsWord16X8 :: Ptr Word16 -> Int -> Word16X8# -> IO ()
writeAsWord16X8 (Ptr addr) (I# i) v = IO $ \s ->
  (# writeWord16OffAddrAsWord16X8# addr i v s, () #)

arr1 :: UArray Int Word16
arr1 = listArray (0,63) [5,27476,32767,6894,0,22953,5,14293,55254,10813,2,1,3,48047,1,26,40522,2,34173,1,32767,64647,0,32767,3,1,8,5,4,4,49789,2,0,0,3,4,1,62246,23,3,0,1,8,0,5,0,2,0,4,1,7,1,28,42969,20,4,4,2,2,16,1,19,2,60671]

input2 :: [Word16]
input2 = [5,29,4,17110,20,12,13,30889,11,32767,1,31332,5,1,7199,5,26,2,3,4,2,18865,5,3,13,5,2,5,3,31589,24,37027,5,55547,4,7,5,2,1,51255,4,2,32749,4,2,43220,24,14,25583,5,17,4,32767,57741,22,27745,1,2,6,1,22,2,1,18]

run :: (Word16X8# -> Word16X8# -> Word16X8#) -> UArray Int Word16 -> Ptr Word16 -> IO [Word16]
run f a b = allocaArray 64 $ \result -> do
  forM_ [0,8..63] $ \i -> do
    let v = indexAsWord16X8 a i
    Word16X8 w <- readAsWord16X8 b i
    writeAsWord16X8 result i (f v w)
  peekArray 64 result
{-# INLINE run #-}

runN :: (Word16X8# -> Word16X8# -> Word16X8#) -> UArray Int Word16 -> Ptr Word16 -> IO [Word16]
runN f a b = allocaArray 64 $ \result -> do
  forM_ [0,8..63] $ \i -> do
    let v = indexAsWord16X8 a i
    Word16X8 w <- readAsWord16X8 b i
    writeAsWord16X8 result i (f v w)
  peekArray 64 result
{-# NOINLINE runN #-}

main :: IO ()
main = do
  withArray input2 $ \arr2 -> do
    run plusWord16X8# arr1 arr2 >>= print
    run minusWord16X8# arr1 arr2 >>= print
    run timesWord16X8# arr1 arr2 >>= print
    run quotWord16X8# arr1 arr2 >>= print
    run remWord16X8# arr1 arr2 >>= print
    run minWord16X8# arr1 arr2 >>= print
    run maxWord16X8# arr1 arr2 >>= print
    runN plusWord16X8# arr1 arr2 >>= print
    runN minusWord16X8# arr1 arr2 >>= print
    runN timesWord16X8# arr1 arr2 >>= print
    runN quotWord16X8# arr1 arr2 >>= print
    runN remWord16X8# arr1 arr2 >>= print
    runN minWord16X8# arr1 arr2 >>= print
    runN maxWord16X8# arr1 arr2 >>= print
    runN (\x y -> plusWord16X8# y x) arr1 arr2 >>= print
    runN (\x y -> minusWord16X8# y x) arr1 arr2 >>= print
    runN (\x y -> timesWord16X8# y x) arr1 arr2 >>= print
    runN (\x y -> minWord16X8# y x) arr1 arr2 >>= print
    runN (\x y -> maxWord16X8# y x) arr1 arr2 >>= print
