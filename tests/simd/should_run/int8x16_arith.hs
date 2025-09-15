{-# LANGUAGE MagicHash, UnboxedTuples #-}
import Control.Monad
import Data.Array.Base
import Foreign.Marshal.Array
import GHC.Int
import GHC.IO
import GHC.Prim
import GHC.Ptr

data Int8X16 = Int8X16 Int8X16#

indexAsInt8X16 :: UArray Int Int8 -> Int -> Int8X16#
indexAsInt8X16 (UArray l _ _ ba) i = case i - l of
  I# i# -> indexInt8ArrayAsInt8X16# ba i#

readAsInt8X16 :: Ptr Int8 -> Int -> IO Int8X16
readAsInt8X16 (Ptr addr) (I# i) = IO $ \s ->
  case readInt8OffAddrAsInt8X16# addr i s of
    (# s', v #) -> (# s', Int8X16 v #)

writeAsInt8X16 :: Ptr Int8 -> Int -> Int8X16# -> IO ()
writeAsInt8X16 (Ptr addr) (I# i) v = IO $ \s ->
  (# writeInt8OffAddrAsInt8X16# addr i v s, () #)

arr1 :: UArray Int Int8
arr1 = listArray (0,63) [-2,-1,2,-1,-5,0,-1,1,0,-2,1,3,0,4,0,-5,-1,-53,-2,-2,-128,-128,127,-1,127,3,-1,4,69,91,-2,-53,-19,0,5,1,30,-2,-1,-32,-128,-70,2,-109,127,-1,5,0,4,-2,0,2,0,-1,89,2,3,0,3,-112,-74,5,0,80]

input2 :: [Int8]
input2 = [25,-2,88,38,-2,-3,-3,-88,1,-5,3,-3,-1,1,-3,3,4,-1,-55,102,-2,1,5,-1,5,-5,-102,2,-1,-4,2,-5,2,-2,-1,-4,-64,-1,-126,-3,-4,-2,-2,-56,3,34,1,-5,-83,3,-1,-121,2,3,126,1,-107,28,1,2,-128,-2,-5,4]

run :: (Int8X16# -> Int8X16# -> Int8X16#) -> UArray Int Int8 -> Ptr Int8 -> IO [Int8]
run f a b = allocaArray 64 $ \result -> do
  forM_ [0,16..63] $ \i -> do
    let v = indexAsInt8X16 a i
    Int8X16 w <- readAsInt8X16 b i
    writeAsInt8X16 result i (f v w)
  peekArray 64 result
{-# INLINE run #-}

runN :: (Int8X16# -> Int8X16# -> Int8X16#) -> UArray Int Int8 -> Ptr Int8 -> IO [Int8]
runN f a b = allocaArray 64 $ \result -> do
  forM_ [0,16..63] $ \i -> do
    let v = indexAsInt8X16 a i
    Int8X16 w <- readAsInt8X16 b i
    writeAsInt8X16 result i (f v w)
  peekArray 64 result
{-# NOINLINE runN #-}

main :: IO ()
main = do
  withArray input2 $ \arr2 -> do
    run (\x _ -> negateInt8X16# x) arr1 arr2 >>= print
    run plusInt8X16# arr1 arr2 >>= print
    run minusInt8X16# arr1 arr2 >>= print
    run timesInt8X16# arr1 arr2 >>= print
    run quotInt8X16# arr1 arr2 >>= print
    run remInt8X16# arr1 arr2 >>= print
    run minInt8X16# arr1 arr2 >>= print
    run maxInt8X16# arr1 arr2 >>= print
    runN (\x _ -> negateInt8X16# x) arr1 arr2 >>= print
    runN plusInt8X16# arr1 arr2 >>= print
    runN minusInt8X16# arr1 arr2 >>= print
    runN timesInt8X16# arr1 arr2 >>= print
    runN quotInt8X16# arr1 arr2 >>= print
    runN remInt8X16# arr1 arr2 >>= print
    runN minInt8X16# arr1 arr2 >>= print
    runN maxInt8X16# arr1 arr2 >>= print
    runN (\_ y -> negateInt8X16# y) arr1 arr2 >>= print
    runN (\x y -> plusInt8X16# y x) arr1 arr2 >>= print
    runN (\x y -> minusInt8X16# y x) arr1 arr2 >>= print
    runN (\x y -> timesInt8X16# y x) arr1 arr2 >>= print
    runN (\x y -> minInt8X16# y x) arr1 arr2 >>= print
    runN (\x y -> maxInt8X16# y x) arr1 arr2 >>= print
