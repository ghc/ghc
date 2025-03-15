{-# LANGUAGE MagicHash, UnboxedTuples #-}
import Control.Monad
import Data.Array.Base
import Foreign.Marshal.Array
import GHC.Int
import GHC.IO
import GHC.Prim
import GHC.Ptr

data DoubleX2 = DoubleX2 DoubleX2#

indexAsDoubleX2 :: UArray Int Double -> Int -> DoubleX2#
indexAsDoubleX2 (UArray l _ _ ba) i = case i - l of
  I# i# -> indexDoubleArrayAsDoubleX2# ba i#

readAsDoubleX2 :: Ptr Double -> Int -> IO DoubleX2
readAsDoubleX2 (Ptr addr) (I# i) = IO $ \s ->
  case readDoubleOffAddrAsDoubleX2# addr i s of
    (# s', v #) -> (# s', DoubleX2 v #)

writeAsDoubleX2 :: Ptr Double -> Int -> DoubleX2# -> IO ()
writeAsDoubleX2 (Ptr addr) (I# i) v = IO $ \s ->
  (# writeDoubleOffAddrAsDoubleX2# addr i v s, () #)

arr1 :: UArray Int Double
arr1 = listArray (0,63) [86.1704805043268,-87.055309160875,77.09385349363602,70.43981517796,50.02907568304664,-79.93253267799825,-94.4734175782,30.062138137255715,12.350686811659486,-23.78391700268743,-96.44925698909766,-67.13139854675774,77.84360517385772,-32.84892092508137,-58.02803492421746,-50.79004702787717,-11.908674906547787,-92.38400254440369,36.41222449165823,-68.6479119817805,87.23932115197337,-37.42852497445781,-64.82933137985788,84.16399940800329,-55.35002818965286,8.630163006731266,73.63479275711025,14.555315332112912,-59.0235004342909,27.431362647878856,36.75482002856153,30.991841701036876,-22.897831600115012,-93.89256411789064,17.639582620461923,94.24572581296609,46.564865390415235,-23.14689831583175,44.286969376764674,-61.7495951058958,37.602886696272556,-47.99709400665984,44.97168988769863,87.51005084451836,-57.245772080078,-74.15908041965238,-22.163672693129797,-36.26569085317567,81.28128900464503,-33.53676733222679,29.57596290010983,0.6835164364159141,-61.13183256365945,5.712281466153783,52.13503127014405,-75.17328181431184,-80.3499374345099,69.19728646446558,63.7669473494779,27.17066306772783,-91.12320723332579,65.78107094275762,-29.222084470705,55.14981850187309]

input2 :: [Double]
input2 = [-2.1274469530639664,-7.177109378829002,-98.24323405063399,-26.03855917426543,-82.35787006428066,86.84756206888044,66.51501361884274,-69.12618497150666,12.336044450377486,-0.9895575202943405,-10.826450417702162,-23.706389595256866,12.884863406302358,85.91465494902334,-10.791922059638424,83.5855338741751,68.54940218977418,-51.043267529916946,74.89746292225463,74.48290415522592,8.172249887162096,-6.426143126204735,-71.09523683330931,-67.16904290667816,-66.06993212850568,86.03428946847885,25.702893242346008,98.90487021469829,-51.28337546267276,-53.05672041384456,-39.24117195492243,91.65414155791501,87.26058482732314,68.14308311155132,95.0497956861045,-52.545861855432456,-17.050384860987847,-39.54129448813424,94.0049004311935,-87.37548839080712,-15.707343289828259,49.289832041364654,34.873616921124494,-7.712670991606458,-46.75167972060545,-67.53197682090646,-94.04146452769466,-53.28374327444284,-88.55039606421403,-20.010295314180937,16.90312028906486,-70.34859808800593,41.88668735114813,-56.73476319101437,23.34349297210754,97.33003908727085,65.43278416798937,26.006050520144072,4.234841735331756,21.139356688092775,79.81443499673723,-89.92184005873219,-36.90383017318868,-94.20848066738576]

run :: (DoubleX2# -> DoubleX2# -> DoubleX2#) -> UArray Int Double -> Ptr Double -> IO [Double]
run f a b = allocaArray 64 $ \result -> do
  forM_ [0,4..63] $ \i -> do
    let v = indexAsDoubleX2 a i
    DoubleX2 w <- readAsDoubleX2 b i
    writeAsDoubleX2 result i (f v w)
  peekArray 64 result
{-# INLINE run #-}

runN :: (DoubleX2# -> DoubleX2# -> DoubleX2#) -> UArray Int Double -> Ptr Double -> IO [Double]
runN f a b = allocaArray 64 $ \result -> do
  forM_ [0,4..63] $ \i -> do
    let v = indexAsDoubleX2 a i
    DoubleX2 w <- readAsDoubleX2 b i
    writeAsDoubleX2 result i (f v w)
  peekArray 64 result
{-# NOINLINE runN #-}

main :: IO ()
main = do
  withArray input2 $ \arr2 -> do
    run (\x _ -> negateDoubleX2# x) arr1 arr2 >>= print
    run plusDoubleX2# arr1 arr2 >>= print
    run minusDoubleX2# arr1 arr2 >>= print
    run timesDoubleX2# arr1 arr2 >>= print
    run divideDoubleX2# arr1 arr2 >>= print
    -- minDoubleX2# and maxDoubleX2# are not well-defined if the arguments are signed zeros or NaNs.
    -- This test case doesn't contain such cases.
    run minDoubleX2# arr1 arr2 >>= print
    run maxDoubleX2# arr1 arr2 >>= print
    runN (\x _ -> negateDoubleX2# x) arr1 arr2 >>= print
    runN plusDoubleX2# arr1 arr2 >>= print
    runN minusDoubleX2# arr1 arr2 >>= print
    runN timesDoubleX2# arr1 arr2 >>= print
    runN divideDoubleX2# arr1 arr2 >>= print
    runN minDoubleX2# arr1 arr2 >>= print
    runN maxDoubleX2# arr1 arr2 >>= print
    runN (\_ y -> negateDoubleX2# y) arr1 arr2 >>= print
    runN (\x y -> plusDoubleX2# y x) arr1 arr2 >>= print
    runN (\x y -> minusDoubleX2# y x) arr1 arr2 >>= print
    runN (\x y -> timesDoubleX2# y x) arr1 arr2 >>= print
    runN (\x y -> divideDoubleX2# y x) arr1 arr2 >>= print
    runN (\x y -> minDoubleX2# y x) arr1 arr2 >>= print
    runN (\x y -> maxDoubleX2# y x) arr1 arr2 >>= print

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
  let xs, ys :: [Double]
      (xs, ys) = runStateGen_ (mkStdGen 42) $ \g -> do
        a <- replicateM 64 (uniformRM (-100.0, 100.0) g)
        b <- replicateM 64 (uniformRM (-100.0, 100.0) g)
        pure (a, b)
  print $ or $ zipWith (\x y -> isNaN x || isNaN y || (x == 0 && y == 0 && isNegativeZero x /= isNegativeZero y)) xs ys -- should be False
  print xs
  print ys
-}
