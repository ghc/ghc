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

arr3 :: UArray Int Double
arr3 = listArray (0,63) [-59.65425141222654,69.64349283945359,83.9284747794684,12.817033474692124,62.27335504509297,-18.237252362613106,-27.440844934578607,86.99583598227946,60.27511044722207,99.105767009872,-17.302109168036182,-52.97040384897619,-40.87827233682159,-12.965680691542133,-7.085664028521066,-24.62739386751332,-46.726152845099044,96.32406472850488,-74.91671402250174,68.79700002196601,4.924046272240773,-95.05505737760092,-15.860493489780893,44.16404678304456,-78.88924777259722,96.26040871939944,10.733429436230978,-96.82282239343006,4.807453036471273,94.98957580192541,-81.10639072830648,60.06291064241987,33.176918791107624,15.812814601682973,20.525838187336277,74.97849506952045,-32.05634249924189,-33.75972083595386,-87.97178483130338,64.4790977515402,22.619625038491137,50.005386892628295,-39.866736828488335,-45.632596031698114,14.569082560676776,32.74340801994042,-57.011231518845086,-1.1165315306058403,76.29399105390189,91.78812120984722,-5.161503833356846,-35.47834180738009,-29.87208820642647,-0.34845820369714886,39.65825554726061,-52.50503328017441,-99.11169685924439,56.229972696709645,-87.58597819570255,11.270880346195128,-52.398349186440754,83.16647409648112,-27.946311069976417,-45.94266833398757]

run :: (DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2#) -> UArray Int Double -> Ptr Double -> UArray Int Double -> IO [Double]
run f a b c = allocaArray 64 $ \result -> do
  forM_ [0,4..63] $ \i -> do
    let v = indexAsDoubleX2 a i
    DoubleX2 w <- readAsDoubleX2 b i
    let x = indexAsDoubleX2 c i
    writeAsDoubleX2 result i (f v w x)
  peekArray 64 result
{-# INLINE run #-}

runN :: (DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2#) -> UArray Int Double -> Ptr Double -> UArray Int Double -> IO [Double]
runN f a b c = allocaArray 64 $ \result -> do
  forM_ [0,4..63] $ \i -> do
    let v = indexAsDoubleX2 a i
    DoubleX2 w <- readAsDoubleX2 b i
    let x = indexAsDoubleX2 c i
    writeAsDoubleX2 result i (f v w x)
  peekArray 64 result
{-# NOINLINE runN #-}

main :: IO ()
main = do
  withArray input2 $ \arr2 -> do
    run (\x y z -> fmaddDoubleX2# x y z) arr1 arr2 arr3 >>= print
    run (\x y z -> fmaddDoubleX2# y x z) arr1 arr2 arr3 >>= print
    run (\x y z -> fmaddDoubleX2# z y x) arr1 arr2 arr3 >>= print
    run (\x y z -> fmaddDoubleX2# y z x) arr1 arr2 arr3 >>= print
    run (\x y z -> fmaddDoubleX2# z x y) arr1 arr2 arr3 >>= print
    run (\x y z -> fmaddDoubleX2# x z y) arr1 arr2 arr3 >>= print
    run (\x y z -> fmsubDoubleX2# x y z) arr1 arr2 arr3 >>= print
    run (\x y z -> fmsubDoubleX2# y x z) arr1 arr2 arr3 >>= print
    run (\x y z -> fmsubDoubleX2# z y x) arr1 arr2 arr3 >>= print
    run (\x y z -> fmsubDoubleX2# y z x) arr1 arr2 arr3 >>= print
    run (\x y z -> fmsubDoubleX2# z x y) arr1 arr2 arr3 >>= print
    run (\x y z -> fmsubDoubleX2# x z y) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmaddDoubleX2# x y z) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmaddDoubleX2# y x z) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmaddDoubleX2# z y x) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmaddDoubleX2# y z x) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmaddDoubleX2# z x y) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmaddDoubleX2# x z y) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmsubDoubleX2# x y z) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmsubDoubleX2# y x z) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmsubDoubleX2# z y x) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmsubDoubleX2# y z x) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmsubDoubleX2# z x y) arr1 arr2 arr3 >>= print
    run (\x y z -> fnmsubDoubleX2# x z y) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmaddDoubleX2# x y z) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmaddDoubleX2# y x z) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmaddDoubleX2# z y x) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmaddDoubleX2# y z x) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmaddDoubleX2# z x y) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmaddDoubleX2# x z y) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmsubDoubleX2# x y z) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmsubDoubleX2# y x z) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmsubDoubleX2# z y x) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmsubDoubleX2# y z x) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmsubDoubleX2# z x y) arr1 arr2 arr3 >>= print
    runN (\x y z -> fmsubDoubleX2# x z y) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmaddDoubleX2# x y z) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmaddDoubleX2# y x z) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmaddDoubleX2# z y x) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmaddDoubleX2# y z x) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmaddDoubleX2# z x y) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmaddDoubleX2# x z y) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmsubDoubleX2# x y z) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmsubDoubleX2# y x z) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmsubDoubleX2# z y x) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmsubDoubleX2# y z x) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmsubDoubleX2# z x y) arr1 arr2 arr3 >>= print
    runN (\x y z -> fnmsubDoubleX2# x z y) arr1 arr2 arr3 >>= print

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
  let xs, ys, zs :: [Double]
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
putStr $ unlines (["    run (\\x y z -> " ++ intercalate " " [f,a,b,c] ++ ") arr1 arr2 arr3 >>= print" | f <- ["fmaddDoubleX2#","fmsubDoubleX2#","fnmaddDoubleX2#","fnmsubDoubleX2#"], [a,b,c] <- permutations ["x", "y", "z"]])
putStr $ unlines (["    runN (\\x y z -> " ++ intercalate " " [f,a,b,c] ++ ") arr1 arr2 arr3 >>= print" | f <- ["fmaddDoubleX2#","fmsubDoubleX2#","fnmaddDoubleX2#","fnmsubDoubleX2#"], [a,b,c] <- permutations ["x", "y", "z"]])
-}
