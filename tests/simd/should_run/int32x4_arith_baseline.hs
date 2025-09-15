{-# LANGUAGE MagicHash, UnboxedTuples #-}
import Control.Monad
import Data.Array.Base
import Foreign.Marshal.Array
import GHC.Int
import GHC.IO
import GHC.Prim
import GHC.Ptr

data Int32X4 = Int32X4 Int32X4#

indexAsInt32X4 :: UArray Int Int32 -> Int -> Int32X4#
indexAsInt32X4 (UArray l _ _ ba) i = case i - l of
  I# i# -> indexInt32ArrayAsInt32X4# ba i#

readAsInt32X4 :: Ptr Int32 -> Int -> IO Int32X4
readAsInt32X4 (Ptr addr) (I# i) = IO $ \s ->
  case readInt32OffAddrAsInt32X4# addr i s of
    (# s', v #) -> (# s', Int32X4 v #)

writeAsInt32X4 :: Ptr Int32 -> Int -> Int32X4# -> IO ()
writeAsInt32X4 (Ptr addr) (I# i) v = IO $ \s ->
  (# writeInt32OffAddrAsInt32X4# addr i v s, () #)

arr1 :: UArray Int Int32
arr1 = listArray (0,63) [-2,3,369500337,892734283,-1580732846,5,-574423864,0,409,3,2,-2147483648,2,-484,-19351995,-427,2147483647,3,84,1859701485,3,597261830,-444,0,-1,1,-9,2147483647,-1,-5,-2,-357,3,389,5,1548818645,-2,-210,-23,-2147483648,-5,2,-329,1250168429,109,1533460149,4,-447,411,-463,277,313,-46,5,-452,-401035643,-4,-1,-312,-51,3,-3,-3,-1]

input2 :: [Int32]
input2 = [5,-5,-118,-565830507,-1623863447,5,5,67,2147483647,-2147483648,-1157321724,283,55,2147483647,1,122,2147483647,2147483647,1324933262,1,1,5,146,2,1,-4,407242045,-4,-445,-5,3,-5,-454,-1810623436,4,5,-2,-1112764839,-199,3,1080156242,1,-2,-2,-2,81142576,3,-1,-460939274,4,-3,-51,61092842,1154639686,-111,5,4,-2147483648,184,1618943765,-1,-1600034815,-408441945,-203]

run :: (Int32X4# -> Int32X4# -> Int32X4#) -> UArray Int Int32 -> Ptr Int32 -> IO [Int32]
run f a b = allocaArray 64 $ \result -> do
  forM_ [0,4..63] $ \i -> do
    let v = indexAsInt32X4 a i
    Int32X4 w <- readAsInt32X4 b i
    writeAsInt32X4 result i (f v w)
  peekArray 64 result
{-# INLINE run #-}

runN :: (Int32X4# -> Int32X4# -> Int32X4#) -> UArray Int Int32 -> Ptr Int32 -> IO [Int32]
runN f a b = allocaArray 64 $ \result -> do
  forM_ [0,4..63] $ \i -> do
    let v = indexAsInt32X4 a i
    Int32X4 w <- readAsInt32X4 b i
    writeAsInt32X4 result i (f v w)
  peekArray 64 result
{-# NOINLINE runN #-}

main :: IO ()
main = do
  withArray input2 $ \arr2 -> do
    run (\x _ -> negateInt32X4# x) arr1 arr2 >>= print
    run plusInt32X4# arr1 arr2 >>= print
    run minusInt32X4# arr1 arr2 >>= print
    run timesInt32X4# arr1 arr2 >>= print
    run quotInt32X4# arr1 arr2 >>= print
    run remInt32X4# arr1 arr2 >>= print
    run minInt32X4# arr1 arr2 >>= print
    run maxInt32X4# arr1 arr2 >>= print
    runN (\x _ -> negateInt32X4# x) arr1 arr2 >>= print
    runN plusInt32X4# arr1 arr2 >>= print
    runN minusInt32X4# arr1 arr2 >>= print
    runN timesInt32X4# arr1 arr2 >>= print
    runN quotInt32X4# arr1 arr2 >>= print
    runN remInt32X4# arr1 arr2 >>= print
    runN minInt32X4# arr1 arr2 >>= print
    runN maxInt32X4# arr1 arr2 >>= print
    runN (\_ y -> negateInt32X4# y) arr1 arr2 >>= print
    runN (\x y -> plusInt32X4# y x) arr1 arr2 >>= print
    runN (\x y -> minusInt32X4# y x) arr1 arr2 >>= print
    runN (\x y -> timesInt32X4# y x) arr1 arr2 >>= print
    runN (\x y -> minInt32X4# y x) arr1 arr2 >>= print
    runN (\x y -> maxInt32X4# y x) arr1 arr2 >>= print
