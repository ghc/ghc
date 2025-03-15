{-# LANGUAGE MagicHash, UnboxedTuples #-}
import Control.Monad
import Data.Array.Base
import Foreign.Marshal.Array
import GHC.Int
import GHC.IO
import GHC.Prim
import GHC.Ptr

data FloatX4 = FloatX4 FloatX4#

indexAsFloatX4 :: UArray Int Float -> Int -> FloatX4#
indexAsFloatX4 (UArray l _ _ ba) i = case i - l of
  I# i# -> indexFloatArrayAsFloatX4# ba i#

readAsFloatX4 :: Ptr Float -> Int -> IO FloatX4
readAsFloatX4 (Ptr addr) (I# i) = IO $ \s ->
  case readFloatOffAddrAsFloatX4# addr i s of
    (# s', v #) -> (# s', FloatX4 v #)

writeAsFloatX4 :: Ptr Float -> Int -> FloatX4# -> IO ()
writeAsFloatX4 (Ptr addr) (I# i) v = IO $ \s ->
  (# writeFloatOffAddrAsFloatX4# addr i v s, () #)

arr1 :: UArray Int Float
arr1 = listArray (0,63) [-46.6892,89.12732,-77.78206,25.62262,7.794174,38.453793,-39.720543,47.347664,39.21601,-97.32084,-91.673874,-93.06639,-24.45533,-82.11456,-50.65983,47.034466,-62.318623,-1.7945938,66.2823,0.43374634,-72.966705,2.8155289,-98.03614,57.937504,7.237129,-73.059814,39.52617,34.392334,-60.721226,-57.35563,93.037544,48.76241,-71.80935,-23.923409,-69.97397,54.472355,94.4757,-70.334885,12.157753,21.06411,95.6841,-61.641663,13.519524,-48.503525,18.204895,3.8409805,76.24074,81.23582,-51.674843,5.475235,-3.6810303,40.684856,-5.465164,-82.67741,-81.206024,25.460907,16.86615,36.823112,-31.761482,-86.52606,73.76491,94.99991,22.627441,-45.88064]

input2 :: [Float]
input2 = [-87.38458,-61.88547,0.7465515,-88.43077,-94.30282,-23.171593,85.92421,49.86781,-76.373604,87.17505,-75.796295,-17.862892,43.495815,-53.17978,7.1279907,-8.282211,65.43089,21.971985,-4.4562683,-57.797848,-0.100914,66.46458,0.43808746,87.80351,40.871555,-1.7138519,-88.45551,87.25368,-40.668373,-10.477974,-3.1042175,34.199455,-63.186687,-12.722816,63.30073,-9.216278,62.15632,-68.041595,-15.712379,10.544235,36.857437,-42.99411,69.461914,-73.757645,-33.06685,-80.0233,-14.392632,72.19195,43.64637,57.709164,-41.887623,7.433571,10.958084,-51.711353,-89.607704,98.22998,-87.74089,-82.14257,56.985752,-33.83731,-59.704292,-48.397453,-90.9834,-72.2701]

arr3 :: UArray Int Float
arr3 = listArray (0,63) [-82.874825,24.663322,80.977585,-5.539955,30.870918,-66.322014,68.622406,-32.070335,33.606728,52.395947,-28.537193,87.26526,-3.6302185,40.552147,-72.941605,-46.680695,37.59773,-26.715553,-50.429577,57.255318,3.264801,56.19945,-37.02984,92.190384,89.34591,49.41381,-2.5357513,-86.19853,68.70002,30.182632,-74.67398,11.763702,89.85512,71.93959,37.59725,-26.887596,-63.618843,12.511604,-21.1969,18.45224,88.10219,27.665588,-76.59616,87.72276,30.83065,-5.2282104,-78.12404,-8.904892,-57.82519,57.57774,62.44629,79.00699,9.82901,88.902176,91.8949,-13.740692,-61.520546,87.340866,38.71158,-84.03725,20.15123,68.01758,-89.39148,85.45143]

run :: (FloatX4# -> FloatX4# -> FloatX4# -> FloatX4#) -> UArray Int Float -> Ptr Float -> UArray Int Float -> IO [Float]
run f a b c = allocaArray 64 $ \result -> do
  forM_ [0,4..63] $ \i -> do
    let v = indexAsFloatX4 a i
    FloatX4 w <- readAsFloatX4 b i
    let x = indexAsFloatX4 c i
    writeAsFloatX4 result i (f v w x)
  peekArray 64 result
{-# INLINE run #-}

runN :: (FloatX4# -> FloatX4# -> FloatX4# -> FloatX4#) -> UArray Int Float -> Ptr Float -> UArray Int Float -> IO [Float]
runN f a b c = allocaArray 64 $ \result -> do
  forM_ [0,4..63] $ \i -> do
    let v = indexAsFloatX4 a i
    FloatX4 w <- readAsFloatX4 b i
    let x = indexAsFloatX4 c i
    writeAsFloatX4 result i (f v w x)
  peekArray 64 result
{-# NOINLINE runN #-}

main :: IO ()
main = do
  withArray input2 $ \arr2 -> do
    run (\x y z -> fmaddFloatX4# x y z) arr1 arr2 arr3 >>= print
    run (\x y z -> fmaddFloatX4# y x z) arr1 arr2 arr3 >>= print
    run (\x y z -> fmaddFloatX4# z y x) arr1 arr2 arr3 >>= print
    run (\x y z -> fmaddFloatX4# y z x) arr1 arr2 arr3 >>= print
    run (\x y z -> fmaddFloatX4# z x y) arr1 arr2 arr3 >>= print
    run (\x y z -> fmaddFloatX4# x z y) arr1 arr2 arr3 >>= print
    run (\x y z -> fmsubFloatX4# x y z) arr1 arr2 arr3 >>= print
    run (\x y z -> fmsubFloatX4# y x z) arr1 arr2 arr3 >>= print
    run (\x y z -> fmsubFloatX4# z y x) arr1 arr2 arr3 >>= print
    run (\x y z -> fmsubFloatX4# y z x) arr1 arr2 arr3 >>= print
    run (\x y z -> fmsubFloatX4# z x y) arr1 arr2 arr3 >>= print
    run (\x y z -> fmsubFloatX4# x z y) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmaddFloatX4# x y z) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmaddFloatX4# y x z) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmaddFloatX4# z y x) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmaddFloatX4# y z x) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmaddFloatX4# z x y) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmaddFloatX4# x z y) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmsubFloatX4# x y z) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmsubFloatX4# y x z) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmsubFloatX4# z y x) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmsubFloatX4# y z x) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmsubFloatX4# z x y) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmsubFloatX4# x z y) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmaddFloatX4# x y z) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmaddFloatX4# y x z) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmaddFloatX4# z y x) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmaddFloatX4# y z x) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmaddFloatX4# z x y) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmaddFloatX4# x z y) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmsubFloatX4# x y z) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmsubFloatX4# y x z) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmsubFloatX4# z y x) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmsubFloatX4# y z x) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmsubFloatX4# z x y) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmsubFloatX4# x z y) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmaddFloatX4# x y z) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmaddFloatX4# y x z) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmaddFloatX4# z y x) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmaddFloatX4# y z x) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmaddFloatX4# z x y) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmaddFloatX4# x z y) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmsubFloatX4# x y z) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmsubFloatX4# y x z) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmsubFloatX4# z y x) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmsubFloatX4# y z x) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmsubFloatX4# z x y) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmsubFloatX4# x z y) arr1 arr2 arr3 >>= print

{-
The values was generated by:
{- cabal:
build-depends: base, random >= 1.3.0
-}
import System.Random.Stateful
import qualified Data.List as List
import Control.Monad

main :: IO ()
main = do
  let xs, ys, zs :: [Float]
      (xs, ys, zs) = runStateGen_ (mkStdGen 42) $ \g -> do
        a <- replicateM 64 (uniformRM (-100.0, 100.0) g)
        b <- replicateM 64 (uniformRM (-100.0, 100.0) g)
        c <- replicateM 64 (uniformRM (-100.0, 100.0) g)
        pure (a, b, c)
  print xs
  print ys
  print zs
-}

{-
The code was generated by:
:m + Data.List
putStr $ unlines (["    run (\\x y z -> " ++ intercalate " " [f,a,b,c] ++ ") arr1 arr2 arr3 >>= print" | f <- ["fmaddFloatX4#","fmsubFloatX4#","fnmaddFloatX4#","fnmsubFloatX4#"], [a,b,c] <- permutations ["x", "y", "z"]])
putStr $ unlines (["    runN (\\x y z -> " ++ intercalate " " [f,a,b,c] ++ ") arr1 arr2 arr3 >>= print" | f <- ["fmaddFloatX4#","fmsubFloatX4#","fnmaddFloatX4#","fnmsubFloatX4#"], [a,b,c] <- permutations ["x", "y", "z"]])
-}
