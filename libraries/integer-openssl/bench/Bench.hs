{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import           Criterion.Main
import           GHC.Prim
import           GHC.Types

import qualified GHC.Integer              as Y
import qualified OpenSSL.GHC.Integer.Type as X

main :: IO ()
main = defaultMain
  [ bgroup "mkInteger"
    [ bgroup "128bit"
      [ bench "library" $ whnf (mkIntegerBench X.mkInteger) big128
      , bench "builtin" $ whnf (mkIntegerBench Y.mkInteger) big128
      ]
    , bgroup "4096bit"
      [ bench "library" $ whnf (mkIntegerBench Y.mkInteger) big4096
      , bench "builtin" $ whnf (mkIntegerBench Y.mkInteger) big4096
      ]
    ]
  , bgroup "encodeDoubleInteger"
    [ bgroup "128bit"
      [ bench "library" $ whnf encodeDoubleIntegerX (big128, 0)
      , bench "builtin" $ whnf encodeDoubleIntegerY (big128, 0)
      ]
    , bgroup "4096bit"
      [ bench "library" $ whnf encodeDoubleIntegerX (big4096, 0)
      , bench "builtin" $ whnf encodeDoubleIntegerY (big4096, 0)
      ]
    ]
  , bgroup "plusInteger"
    [ bgroup "small"
      [ bench "library" $ whnf plusIntegerX (small, small_)
      , bench "builtin" $ whnf plusIntegerY (small, small_)
      ]
    , bgroup "128bit"
      [ bench "library" $ whnf plusIntegerX (big128, big128_)
      , bench "builtin" $ whnf plusIntegerY (big128, big128_)
      ]
    , bgroup "4096bit"
      [ bench "library" $ whnf plusIntegerX (big4096, big4096_)
      , bench "builtin" $ whnf plusIntegerY (big4096, big4096_)
      ]
    ]
  , bgroup "minusInteger"
    [ bgroup "small"
      [ bench "library" $ whnf minusIntegerX (small, small_)
      , bench "builtin" $ whnf minusIntegerY (small, small_)
      ]
    , bgroup "128bit"
      [ bench "library" $ whnf minusIntegerX (big128, big128_)
      , bench "builtin" $ whnf minusIntegerY (big128, big128_)
      ]
    , bgroup "4096bit"
      [ bench "library" $ whnf minusIntegerX (big4096, big4096_)
      , bench "builtin" $ whnf minusIntegerY (big4096, big4096_)
      ]
    ]
  , bgroup "timesInteger"
    [ bgroup "small"
      [ bench "library" $ whnf timesIntegerX (small, small_)
      , bench "builtin" $ whnf timesIntegerY (small, small_)
      ]
    , bgroup "128bit"
      [ bench "library" $ whnf timesIntegerX (big128, big128_)
      , bench "builtin" $ whnf timesIntegerY (big128, big128_)
      ]
    , bgroup "4096bit"
      [ bench "library" $ whnf timesIntegerX (big4096, big4096_)
      , bench "builtin" $ whnf timesIntegerY (big4096, big4096_)
      ]
    ]
  , bgroup "quotRemInteger"
    [ bgroup "small"
      [ bench "library" $ whnf quotRemIntegerX (small, small_)
      , bench "builtin" $ whnf quotRemIntegerY (small, small_)
      ]
    , bgroup "128bit"
      [ bench "library" $ whnf quotRemIntegerX (big128_, big128)
      , bench "builtin" $ whnf quotRemIntegerY (big128_, big128)
      ]
    , bgroup "4096bit"
      [ bench "library" $ whnf quotRemIntegerX (big4096_, big4096)
      , bench "builtin" $ whnf quotRemIntegerY (big4096_, big4096)
      ]
    ]
  , bgroup "divModInteger"
    [ bgroup "small"
      [ bench "library" $ whnf divModIntegerX (small, small_)
      , bench "builtin" $ whnf divModIntegerY (small, small_)
      ]
    , bgroup "128bit"
      [ bench "library" $ whnf divModIntegerX (big128_, big128)
      , bench "builtin" $ whnf divModIntegerY (big128_, big128)
      ]
    , bgroup "4096bit"
      [ bench "library" $ whnf divModIntegerX (big4096_, big4096)
      , bench "builtin" $ whnf divModIntegerY (big4096_, big4096)
      ]
    ]
  ]
 where
  small = (True, [123])
  small_ = (True, [42])
  big128 = (False, [0x1, 0x1, 0x1, 0x1, 0xf]) -- 4*31+4 = 128
  big128_ = (True, [0x2, 0x1, 0x1, 0x1, 0xf])
  big4096 = (True, [0x0..0x84] ++ [0xf]) -- 132*31+4 = 4096
  big4096_ = (False, [0x1..0x85] ++ [0xf])

  encodeDoubleIntegerX = encodeDoubleIntegerBench X.mkInteger X.encodeDoubleInteger
  encodeDoubleIntegerY = encodeDoubleIntegerBench Y.mkInteger Y.encodeDoubleInteger

  plusIntegerX = integer2Bench X.mkInteger X.plusInteger
  plusIntegerY = integer2Bench Y.mkInteger Y.plusInteger

  minusIntegerX = integer2Bench X.mkInteger X.minusInteger
  minusIntegerY = integer2Bench Y.mkInteger Y.minusInteger

  timesIntegerX = integer2Bench X.mkInteger X.timesInteger
  timesIntegerY = integer2Bench Y.mkInteger Y.timesInteger

  quotRemIntegerX = integer2TupleBench X.mkInteger X.quotRemInteger
  quotRemIntegerY = integer2TupleBench Y.mkInteger Y.quotRemInteger

  divModIntegerX = integer2TupleBench X.mkInteger X.divModInteger
  divModIntegerY = integer2TupleBench Y.mkInteger Y.divModInteger

type IntegerBench = (Bool, [Int])

-- | Benchmark big integer creation.
mkIntegerBench :: (Bool -> [Int] -> a) -> (Bool, [Int]) -> a
mkIntegerBench mkInteger (p, is) = mkInteger p is

integer2Bench :: (Bool -> [Int] -> a) -> (a -> a -> a)
                  -> (IntegerBench, IntegerBench) -> a
integer2Bench mkInteger f ((p, is), (p2, is2)) = f (mkInteger p is) (mkInteger p2 is2)

integer2TupleBench :: (Bool -> [Int] -> a) -> (a -> a -> (# a, a #))
                  -> (IntegerBench, IntegerBench) -> (a, a)
integer2TupleBench mkInteger quotRemInteger ((p, is), (p2, is2)) =
  case quotRemInteger (mkInteger p is) (mkInteger p2 is2) of (# q, r #) -> (q, r)

encodeDoubleIntegerBench :: (Bool -> [Int] -> a) -> (a -> Int# -> Double#)
                         -> (IntegerBench, Int) -> Double
encodeDoubleIntegerBench mkInteger f ((p, is), (I# i#)) = D# (f (mkInteger p is) i#)
