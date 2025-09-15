{-# LANGUAGE MagicHash, UnboxedTuples #-}
import Control.Monad
import Data.Array.Base
import Data.Word
import Foreign.Marshal.Array
import GHC.Int
import GHC.IO
import GHC.Prim
import GHC.Ptr

data Word64X2 = Word64X2 Word64X2#

indexAsWord64X2 :: UArray Int Word64 -> Int -> Word64X2#
indexAsWord64X2 (UArray l _ _ ba) i = case i - l of
  I# i# -> indexWord64ArrayAsWord64X2# ba i#

readAsWord64X2 :: Ptr Word64 -> Int -> IO Word64X2
readAsWord64X2 (Ptr addr) (I# i) = IO $ \s ->
  case readWord64OffAddrAsWord64X2# addr i s of
    (# s', v #) -> (# s', Word64X2 v #)

writeAsWord64X2 :: Ptr Word64 -> Int -> Word64X2# -> IO ()
writeAsWord64X2 (Ptr addr) (I# i) v = IO $ \s ->
  (# writeWord64OffAddrAsWord64X2# addr i v s, () #)

arr1 :: UArray Int Word64
arr1 = listArray (0,63) [2,2,15481519882551836963,9223372036854775807,2,3,407056,665089,2626167761364572160,1,643138,9406164418870014025,9223372036854775807,2,1240668,9223372036854775807,1,11980999067584065094,3,4,1,1772456,3,1,1278861,2,567456,0,3,2,6248578581083150360,9223372036854775807,15959578855020885346,3,9223372036854775807,0,1641497,1208732,4,0,11364797772522142807,5,1,9223372036854775807,336667,252015,0,2,8221295439747667121,63622,2865852449280047921,0,543296297765177379,12719600901600025739,13775442247737726971,1,3,1823523,11055793002891365930,2,5151504871350152858,1,2,2137183130682150978]

input2 :: [Word64]
input2 = [915525,1224409,170768,2,12663971951943136152,1327342,476585,5,5,1,4,6556228363689256081,9133433368493456205,2,1554245,2881741784134950439,11372332786521801323,5704931795657208118,5,3,76089,15754615427018365069,9223372036854775807,9223372036854775807,1535757,1,3,5,155818,5,3,5,1211574,4,1,9223372036854775807,1440217,972910,3,4,3,15705338785248207129,1156620,447523,4,14804021770340513224,9118955793658285303,1,3,12353682228586877005,1864275,593173494286384281,1692130,542770,1072366,1,789882,269051,1,2,2,2842614861215614863,1,903765]

run :: (Word64X2# -> Word64X2# -> Word64X2#) -> UArray Int Word64 -> Ptr Word64 -> IO [Word64]
run f a b = allocaArray 64 $ \result -> do
  forM_ [0,2..63] $ \i -> do
    let v = indexAsWord64X2 a i
    Word64X2 w <- readAsWord64X2 b i
    writeAsWord64X2 result i (f v w)
  peekArray 64 result
{-# INLINE run #-}

runN :: (Word64X2# -> Word64X2# -> Word64X2#) -> UArray Int Word64 -> Ptr Word64 -> IO [Word64]
runN f a b = allocaArray 64 $ \result -> do
  forM_ [0,2..63] $ \i -> do
    let v = indexAsWord64X2 a i
    Word64X2 w <- readAsWord64X2 b i
    writeAsWord64X2 result i (f v w)
  peekArray 64 result
{-# NOINLINE runN #-}

main :: IO ()
main = do
  withArray input2 $ \arr2 -> do
    run plusWord64X2# arr1 arr2 >>= print
    run minusWord64X2# arr1 arr2 >>= print
    run timesWord64X2# arr1 arr2 >>= print
    run quotWord64X2# arr1 arr2 >>= print
    run remWord64X2# arr1 arr2 >>= print
    run minWord64X2# arr1 arr2 >>= print
    run maxWord64X2# arr1 arr2 >>= print
    runN plusWord64X2# arr1 arr2 >>= print
    runN minusWord64X2# arr1 arr2 >>= print
    runN timesWord64X2# arr1 arr2 >>= print
    runN quotWord64X2# arr1 arr2 >>= print
    runN remWord64X2# arr1 arr2 >>= print
    runN minWord64X2# arr1 arr2 >>= print
    runN maxWord64X2# arr1 arr2 >>= print
    runN (\x y -> plusWord64X2# y x) arr1 arr2 >>= print
    runN (\x y -> minusWord64X2# y x) arr1 arr2 >>= print
    runN (\x y -> timesWord64X2# y x) arr1 arr2 >>= print
    runN (\x y -> minWord64X2# y x) arr1 arr2 >>= print
    runN (\x y -> maxWord64X2# y x) arr1 arr2 >>= print
