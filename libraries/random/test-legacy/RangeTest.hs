module RangeTest (main) where

import Control.Monad
import System.Random
import Data.Int
import Data.Word
import Foreign.C.Types

-- Take many measurements and record the max/min/average random values.
approxBounds ::
  (RandomGen g, Random a, Ord a, Num a) =>
  (g -> (a,g)) -> Int -> a -> (a,a) -> g -> ((a,a,a),g)
-- Here we do a little hack to essentiall pass in the type in the last argument:
approxBounds nxt iters unused (explo,exphi) initrng =
   if False
   then ((unused,unused,unused),undefined)
--   else loop initrng iters 100 (-100) 0 -- Oops, can't use minBound/maxBound here.
   else loop initrng iters exphi explo 0
 where
  loop rng 0 mn mx sum' = ((mn,mx,sum'),rng)
  loop rng  n mn mx sum' =
    case nxt rng of
      (x, rng') -> loop rng' (n-1) (min x mn) (max x mx) (x+sum')


-- We check that:
--     (1) all generated numbers are in bounds
--     (2) we get "close" to the bounds
-- The with (2) is that we do enough trials to ensure that we can at
-- least hit the 90% mark.
checkBounds ::
  (Real a, Show a, Ord a) =>
  String -> (Bool, a, a) -> ((a,a) -> StdGen -> ((a, a, t), StdGen)) -> IO ()
checkBounds msg (exclusive,lo,hi) fun = do
  -- (lo,hi) is [inclusive,exclusive)
  putStr $ msg ++ ":  "
  (mn,mx,_) <- getStdRandom (fun (lo,hi))
  when (mn < lo) $ error $ "broke lower bound: " ++ show mn
  when (mx > hi) $ error $ "broke upper bound: " ++ show mx
  when (exclusive && mx >= hi)$ error$ "hit upper bound: " ++ show mx

  let epsilon = 0.1 * (toRational hi - toRational lo)

  when (toRational (hi - mx) > epsilon) $ error $ "didn't get close enough to upper bound: "++ show mx
  when (toRational (mn - lo) > epsilon) $ error $ "didn't get close enough to lower bound: "++ show mn
  putStrLn "Passed"

boundedRange :: (Num a, Bounded a) => (Bool, a, a)
boundedRange  = ( False, minBound, maxBound )

trials :: Int
trials = 5000

-- Keep in mind here that on some architectures (e.g. ARM) CChar, CWchar, and CSigAtomic
-- are unsigned
main :: IO ()
main =
 do
    checkBounds "Int"     boundedRange   (approxBounds random trials (undefined::Int))
    checkBounds "Integer" (False, fromIntegral (minBound::Int), fromIntegral (maxBound::Int))
                                         (approxBounds random trials (undefined::Integer))
    checkBounds "Int8"    boundedRange   (approxBounds random trials (undefined::Int8))
    checkBounds "Int16"   boundedRange   (approxBounds random trials (undefined::Int16))
    checkBounds "Int32"   boundedRange   (approxBounds random trials (undefined::Int32))
    checkBounds "Int64"   boundedRange   (approxBounds random trials (undefined::Int64))
    checkBounds "Word"    boundedRange   (approxBounds random trials (undefined::Word))
    checkBounds "Word8"   boundedRange   (approxBounds random trials (undefined::Word8))
    checkBounds "Word16"  boundedRange   (approxBounds random trials (undefined::Word16))
    checkBounds "Word32"  boundedRange   (approxBounds random trials (undefined::Word32))
    checkBounds "Word64"  boundedRange   (approxBounds random trials (undefined::Word64))
    checkBounds "Double"  (False,0.0,1.0) (approxBounds random trials (undefined::Double))
    checkBounds "Float"   (False,0.0,1.0) (approxBounds random trials (undefined::Float))

    checkBounds "CChar"      boundedRange (approxBounds random trials (undefined:: CChar))
    checkBounds "CSChar"     boundedRange (approxBounds random trials (undefined:: CSChar))
    checkBounds "CUChar"     boundedRange (approxBounds random trials (undefined:: CUChar))
    checkBounds "CShort"     boundedRange (approxBounds random trials (undefined:: CShort))
    checkBounds "CUShort"    boundedRange (approxBounds random trials (undefined:: CUShort))
    checkBounds "CInt"       boundedRange (approxBounds random trials (undefined:: CInt))
    checkBounds "CUInt"      boundedRange (approxBounds random trials (undefined:: CUInt))
    checkBounds "CLong"      boundedRange (approxBounds random trials (undefined:: CLong))
    checkBounds "CULong"     boundedRange (approxBounds random trials (undefined:: CULong))
    checkBounds "CPtrdiff"   boundedRange (approxBounds random trials (undefined:: CPtrdiff))
    checkBounds "CSize"      boundedRange (approxBounds random trials (undefined:: CSize))
    checkBounds "CWchar"     boundedRange (approxBounds random trials (undefined:: CWchar))
    checkBounds "CSigAtomic" boundedRange (approxBounds random trials (undefined:: CSigAtomic))
    checkBounds "CLLong"     boundedRange (approxBounds random trials (undefined:: CLLong))
    checkBounds "CULLong"    boundedRange (approxBounds random trials (undefined:: CULLong))
    checkBounds "CIntPtr"    boundedRange (approxBounds random trials (undefined:: CIntPtr))
    checkBounds "CUIntPtr"   boundedRange (approxBounds random trials (undefined:: CUIntPtr))
    checkBounds "CIntMax"    boundedRange (approxBounds random trials (undefined:: CIntMax))
    checkBounds "CUIntMax"   boundedRange (approxBounds random trials (undefined:: CUIntMax))

  -- Then check all the range-restricted versions:
    checkBounds "Int R"     (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined::Int))
    checkBounds "Integer R"
      (False,-100000000000000000000,100000000000000000000)
      (approxBounds (randomR (-100000000000000000000,100000000000000000000)) trials (undefined::Integer))
    checkBounds "Int8 R"    (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined::Int8))
    checkBounds "Int8 Rsmall" (False,-50,50)  (approxBounds (randomR (-50,50))   trials (undefined::Int8))
    checkBounds "Int8 Rmini"    (False,3,4)   (approxBounds (randomR (3,4))      trials (undefined::Int8))
    checkBounds "Int8 Rtrivial" (False,3,3)   (approxBounds (randomR (3,3))      trials (undefined::Int8))

    checkBounds "Int16 R"   (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined::Int16))
    checkBounds "Int32 R"   (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined::Int32))
    checkBounds "Int64 R"   (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined::Int64))
    checkBounds "Word R"    (False,0,200)     (approxBounds (randomR (0,200))    trials (undefined::Word))
    checkBounds "Word8 R"   (False,0,200)     (approxBounds (randomR (0,200))    trials (undefined::Word8))
    checkBounds "Word16 R"  (False,0,200)     (approxBounds (randomR (0,200))    trials (undefined::Word16))
    checkBounds "Word32 R"  (False,0,200)     (approxBounds (randomR (0,200))    trials (undefined::Word32))
    checkBounds "Word64 R"  (False,0,200)     (approxBounds (randomR (0,200))    trials (undefined::Word64))
    checkBounds "Double R" (False,10.0,77.0)  (approxBounds (randomR (10,77)) trials (undefined::Double))
    checkBounds "Float R"  (False,10.0,77.0)  (approxBounds (randomR (10,77)) trials (undefined::Float))

    checkBounds "CChar R"   (False,0,100)        (approxBounds (randomR (0,100))    trials (undefined:: CChar))
    checkBounds "CSChar R"  (False,-100,100)     (approxBounds (randomR (-100,100)) trials (undefined:: CSChar))
    checkBounds "CUChar R"  (False,0,200)        (approxBounds (randomR (0,200))    trials (undefined:: CUChar))
    checkBounds "CShort R"  (False,-100,100)     (approxBounds (randomR (-100,100)) trials (undefined:: CShort))
    checkBounds "CUShort R" (False,0,200)        (approxBounds (randomR (0,200))    trials (undefined:: CUShort))
    checkBounds "CInt R"    (False,-100,100)     (approxBounds (randomR (-100,100)) trials (undefined:: CInt))
    checkBounds "CUInt R"   (False,0,200)        (approxBounds (randomR (0,200))    trials (undefined:: CUInt))
    checkBounds "CLong R"   (False,-100,100)     (approxBounds (randomR (-100,100)) trials (undefined:: CLong))
    checkBounds "CULong R"     (False,0,200)     (approxBounds (randomR (0,200))    trials (undefined:: CULong))
    checkBounds "CPtrdiff R"   (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined:: CPtrdiff))
    checkBounds "CSize R"      (False,0,200)     (approxBounds (randomR (0,200))    trials (undefined:: CSize))
    checkBounds "CWchar R"     (False,0,100)     (approxBounds (randomR (0,100))    trials (undefined:: CWchar))
    checkBounds "CSigAtomic R" (False,0,100)     (approxBounds (randomR (0,100))    trials (undefined:: CSigAtomic))
    checkBounds "CLLong R"     (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined:: CLLong))
    checkBounds "CULLong R"    (False,0,200)     (approxBounds (randomR (0,200))    trials (undefined:: CULLong))
    checkBounds "CIntPtr R"    (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined:: CIntPtr))
    checkBounds "CUIntPtr R"   (False,0,200)     (approxBounds (randomR (0,200))    trials (undefined:: CUIntPtr))
    checkBounds "CIntMax R"    (False,-100,100)  (approxBounds (randomR (-100,100)) trials (undefined:: CIntMax))
    checkBounds "CUIntMax R"   (False,0,200)     (approxBounds (randomR (0,200))    trials (undefined:: CUIntMax))

-- Untested:
-- instance Random Char where
-- instance Random Bool where
-- instance Random Integer where
