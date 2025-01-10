{-# LANGUAGE MagicHash, UnboxedTuples #-}
import Control.Monad
import Data.Array.Base
import Foreign.Marshal.Array
import GHC.Int
import GHC.IO
import GHC.Prim
import GHC.Ptr

data Int16X8 = Int16X8 Int16X8#

indexAsInt16X8 :: UArray Int Int16 -> Int -> Int16X8#
indexAsInt16X8 (UArray l _ _ ba) i = case i - l of
  I# i# -> indexInt16ArrayAsInt16X8# ba i#

readAsInt16X8 :: Ptr Int16 -> Int -> IO Int16X8
readAsInt16X8 (Ptr addr) (I# i) = IO $ \s ->
  case readInt16OffAddrAsInt16X8# addr i s of
    (# s', v #) -> (# s', Int16X8 v #)

writeAsInt16X8 :: Ptr Int16 -> Int -> Int16X8# -> IO ()
writeAsInt16X8 (Ptr addr) (I# i) v = IO $ \s ->
  (# writeInt16OffAddrAsInt16X8# addr i v s, () #)

arr1 :: UArray Int Int16
arr1 = listArray (0,63) [0,2,-1,-3,-31372,2,-4,-1,0,-2,-4,-20536,-29853,-2,20798,5,-32768,-7,1,-11,0,-4,1,5,-12390,-20387,-32768,-32768,32767,-4,-32768,-32768,12,0,-30367,22602,-4,-5,-14886,0,12,-16,-3,-3,-2,10,-4,1286,1,-1,1,-4,1,3,32767,-2,5,-1,-4,2,5,-5,3,0]

input2 :: [Int16]
input2 = [2,15,3,-2,32767,-3,-32768,-2,-4,-10,-3,-32768,-4,-2,-15,13046,2,23604,1,14133,-4,29472,2,-28561,15,-6,-5,2,-2,-5,5,-9022,11350,-10,26129,-2,3,7,-7,1,-32768,-6598,31084,-24530,2,-14,-2,1,11,-6,-1,-32768,-17935,3565,3,17748,-2,-2,3,28092,2,2,3,-3]

run :: (Int16X8# -> Int16X8# -> Int16X8#) -> UArray Int Int16 -> Ptr Int16 -> IO [Int16]
run f a b = allocaArray 64 $ \result -> do
  forM_ [0,8..63] $ \i -> do
    let v = indexAsInt16X8 a i
    Int16X8 w <- readAsInt16X8 b i
    writeAsInt16X8 result i (f v w)
  peekArray 64 result
{-# INLINE run #-}

runN :: (Int16X8# -> Int16X8# -> Int16X8#) -> UArray Int Int16 -> Ptr Int16 -> IO [Int16]
runN f a b = allocaArray 64 $ \result -> do
  forM_ [0,8..63] $ \i -> do
    let v = indexAsInt16X8 a i
    Int16X8 w <- readAsInt16X8 b i
    writeAsInt16X8 result i (f v w)
  peekArray 64 result
{-# NOINLINE runN #-}

main :: IO ()
main = do
  withArray input2 $ \arr2 -> do
    run (\x _ -> negateInt16X8# x) arr1 arr2 >>= print
    run plusInt16X8# arr1 arr2 >>= print
    run minusInt16X8# arr1 arr2 >>= print
    run timesInt16X8# arr1 arr2 >>= print
    run quotInt16X8# arr1 arr2 >>= print
    run remInt16X8# arr1 arr2 >>= print
    run minInt16X8# arr1 arr2 >>= print
    run maxInt16X8# arr1 arr2 >>= print
    runN (\x _ -> negateInt16X8# x) arr1 arr2 >>= print
    runN plusInt16X8# arr1 arr2 >>= print
    runN minusInt16X8# arr1 arr2 >>= print
    runN timesInt16X8# arr1 arr2 >>= print
    runN quotInt16X8# arr1 arr2 >>= print
    runN remInt16X8# arr1 arr2 >>= print
    runN minInt16X8# arr1 arr2 >>= print
    runN maxInt16X8# arr1 arr2 >>= print
    runN (\_ y -> negateInt16X8# y) arr1 arr2 >>= print
    runN (\x y -> plusInt16X8# y x) arr1 arr2 >>= print
    runN (\x y -> minusInt16X8# y x) arr1 arr2 >>= print
    runN (\x y -> timesInt16X8# y x) arr1 arr2 >>= print
    runN (\x y -> minInt16X8# y x) arr1 arr2 >>= print
    runN (\x y -> maxInt16X8# y x) arr1 arr2 >>= print
