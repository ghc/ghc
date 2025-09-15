{-# LANGUAGE MagicHash, UnboxedTuples #-}
import Control.Monad
import Data.Array.Base
import Foreign.Marshal.Array
import GHC.Int
import GHC.IO
import GHC.Prim
import GHC.Ptr

data Int64X2 = Int64X2 Int64X2#

indexAsInt64X2 :: UArray Int Int64 -> Int -> Int64X2#
indexAsInt64X2 (UArray l _ _ ba) i = case i - l of
  I# i# -> indexInt64ArrayAsInt64X2# ba i#

readAsInt64X2 :: Ptr Int64 -> Int -> IO Int64X2
readAsInt64X2 (Ptr addr) (I# i) = IO $ \s ->
  case readInt64OffAddrAsInt64X2# addr i s of
    (# s', v #) -> (# s', Int64X2 v #)

writeAsInt64X2 :: Ptr Int64 -> Int -> Int64X2# -> IO ()
writeAsInt64X2 (Ptr addr) (I# i) v = IO $ \s ->
  (# writeInt64OffAddrAsInt64X2# addr i v s, () #)

arr1 :: UArray Int Int64
arr1 = listArray (0,63) [-1042426,2,-2,-4,-1700443345246692480,490020,-6716270029806626616,-706961767874934979,1002704,4,-9223372036854775808,-1,-9223372036854775808,3,-6031921233268698664,-893527192986168709,2,-5877034375857300039,4,2,-7560692769380516127,-3124474509400657264,-6137967229602520990,-518225,5,-31805,1,9223372036854775807,4149823380638508175,83619,-603690,-904082,4930545353109937751,-1,-9223372036854775808,-8909596730947385503,-3,0,-2,-4279191490187396572,4,-2,0,-600286,1,4648950446138986124,-3917243402580280937,-3,8921165820682016928,3791689277915302570,-7002282506837472216,-129143652674572623,-2,-3,-4,-9223372036854775808,-104224,2,55180,3,-3816958014040616232,2,7299493622876926863,9223372036854775807]

input2 :: [Int64]
input2 = [733395,587201,1948536803153063036,-1,4,8003369379847901540,-4,596314,2,-8383710331524659188,169643179719315924,451764,-4,5,2799380952236214728,7093160715028222089,-28068443521774034,-9223372036854775808,456906,-1,986941,-3,9223372036854775807,3,-4150396939621010586,-5,-4,5757342999303575951,-189543703399644579,31564,2084401421510354528,-9223372036854775808,-3,-5455887619476870070,-988745,-153137,496042,-7108528277894588439,-2,7847253659263211859,149570036841400979,-3,-177358,-3,-35857,-5628330811888516790,-5,-6884305275477408766,-5,-260366,-1018131700086200747,4,3,9223372036854775807,61140724734263263,2,557677,-3,4,-534465,-597783,9223372036854775807,-504454,-736123]

run :: (Int64X2# -> Int64X2# -> Int64X2#) -> UArray Int Int64 -> Ptr Int64 -> IO [Int64]
run f a b = allocaArray 64 $ \result -> do
  forM_ [0,2..63] $ \i -> do
    let v = indexAsInt64X2 a i
    Int64X2 w <- readAsInt64X2 b i
    writeAsInt64X2 result i (f v w)
  peekArray 64 result
{-# INLINE run #-}

runN :: (Int64X2# -> Int64X2# -> Int64X2#) -> UArray Int Int64 -> Ptr Int64 -> IO [Int64]
runN f a b = allocaArray 64 $ \result -> do
  forM_ [0,2..63] $ \i -> do
    let v = indexAsInt64X2 a i
    Int64X2 w <- readAsInt64X2 b i
    writeAsInt64X2 result i (f v w)
  peekArray 64 result
{-# NOINLINE runN #-}

main :: IO ()
main = do
  withArray input2 $ \arr2 -> do
    run (\x _ -> negateInt64X2# x) arr1 arr2 >>= print
    run plusInt64X2# arr1 arr2 >>= print
    run minusInt64X2# arr1 arr2 >>= print
    run timesInt64X2# arr1 arr2 >>= print
    run quotInt64X2# arr1 arr2 >>= print
    run remInt64X2# arr1 arr2 >>= print
    run minInt64X2# arr1 arr2 >>= print
    run maxInt64X2# arr1 arr2 >>= print
    runN (\x _ -> negateInt64X2# x) arr1 arr2 >>= print
    runN plusInt64X2# arr1 arr2 >>= print
    runN minusInt64X2# arr1 arr2 >>= print
    runN timesInt64X2# arr1 arr2 >>= print
    runN quotInt64X2# arr1 arr2 >>= print
    runN remInt64X2# arr1 arr2 >>= print
    runN minInt64X2# arr1 arr2 >>= print
    runN maxInt64X2# arr1 arr2 >>= print
    runN (\_ y -> negateInt64X2# y) arr1 arr2 >>= print
    runN (\x y -> plusInt64X2# y x) arr1 arr2 >>= print
    runN (\x y -> minusInt64X2# y x) arr1 arr2 >>= print
    runN (\x y -> timesInt64X2# y x) arr1 arr2 >>= print
    runN (\x y -> minInt64X2# y x) arr1 arr2 >>= print
    runN (\x y -> maxInt64X2# y x) arr1 arr2 >>= print
