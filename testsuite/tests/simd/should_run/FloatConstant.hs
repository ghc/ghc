{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
import GHC.Exts
import Control.Monad

-- The FMOV instruction of AArch64 can encode some floating-point constants.
-- This file checks that the encoding works correctly.

data DoubleX2 = DX2 DoubleX2#
data FloatX4 = FX4 FloatX4#

floatConstants :: [FloatX4]
floatConstants =
  [ FX4 (broadcastFloatX4# -0.125#)
  , FX4 (packFloatX4# (# -0.1328125#, -0.1328125#, -0.1328125#, -0.1328125# #))
  , FX4 (broadcastFloatX4# 0.140625#)
  , FX4 (packFloatX4# (# -0.1484375#, -0.1484375#, -0.1484375#, -0.1484375# #))
  , FX4 (packFloatX4# (# -0.15625#, -0.15625#, -0.15625#, -0.15625# #))
  , FX4 (broadcastFloatX4# 0.1640625#)
  , FX4 (packFloatX4# (# -0.171875#, -0.171875#, -0.171875#, -0.171875# #))
  , FX4 (broadcastFloatX4# -0.1796875#)
  , FX4 (broadcastFloatX4# 0.1875#)
  , FX4 (broadcastFloatX4# -0.1953125#)
  , FX4 (packFloatX4# (# -0.203125#, -0.203125#, -0.203125#, -0.203125# #))
  , FX4 (broadcastFloatX4# -0.2109375#)
  , FX4 (packFloatX4# (# -0.21875#, -0.21875#, -0.21875#, -0.21875# #))
  , FX4 (broadcastFloatX4# 0.2265625#)
  , FX4 (packFloatX4# (# 0.234375#, 0.234375#, 0.234375#, 0.234375# #))
  , FX4 (broadcastFloatX4# 0.2421875#)
  , FX4 (broadcastFloatX4# -0.25#)
  , FX4 (packFloatX4# (# 0.265625#, 0.265625#, 0.265625#, 0.265625# #))
  , FX4 (packFloatX4# (# 0.28125#, 0.28125#, 0.28125#, 0.28125# #))
  , FX4 (broadcastFloatX4# -0.296875#)
  , FX4 (broadcastFloatX4# -0.3125#)
  , FX4 (broadcastFloatX4# -0.328125#)
  , FX4 (broadcastFloatX4# 0.34375#)
  , FX4 (packFloatX4# (# -0.359375#, -0.359375#, -0.359375#, -0.359375# #))
  , FX4 (packFloatX4# (# 0.375#, 0.375#, 0.375#, 0.375# #))
  , FX4 (broadcastFloatX4# -0.390625#)
  , FX4 (packFloatX4# (# 0.40625#, 0.40625#, 0.40625#, 0.40625# #))
  , FX4 (broadcastFloatX4# 0.421875#)
  , FX4 (packFloatX4# (# -0.4375#, -0.4375#, -0.4375#, -0.4375# #))
  , FX4 (packFloatX4# (# -0.453125#, -0.453125#, -0.453125#, -0.453125# #))
  , FX4 (broadcastFloatX4# 0.46875#)
  , FX4 (packFloatX4# (# 0.484375#, 0.484375#, 0.484375#, 0.484375# #))
  , FX4 (packFloatX4# (# -0.5#, -0.5#, -0.5#, -0.5# #))
  , FX4 (broadcastFloatX4# -0.53125#)
  , FX4 (packFloatX4# (# 0.5625#, 0.5625#, 0.5625#, 0.5625# #))
  , FX4 (broadcastFloatX4# -0.59375#)
  , FX4 (broadcastFloatX4# 0.625#)
  , FX4 (broadcastFloatX4# -0.65625#)
  , FX4 (packFloatX4# (# -0.6875#, -0.6875#, -0.6875#, -0.6875# #))
  , FX4 (broadcastFloatX4# 0.71875#)
  , FX4 (packFloatX4# (# 0.75#, 0.75#, 0.75#, 0.75# #))
  , FX4 (broadcastFloatX4# 0.78125#)
  , FX4 (broadcastFloatX4# -0.8125#)
  , FX4 (packFloatX4# (# -0.84375#, -0.84375#, -0.84375#, -0.84375# #))
  , FX4 (broadcastFloatX4# 0.875#)
  , FX4 (packFloatX4# (# 0.90625#, 0.90625#, 0.90625#, 0.90625# #))
  , FX4 (packFloatX4# (# -0.9375#, -0.9375#, -0.9375#, -0.9375# #))
  , FX4 (packFloatX4# (# -0.96875#, -0.96875#, -0.96875#, -0.96875# #))
  , FX4 (broadcastFloatX4# 1.0#)
  , FX4 (packFloatX4# (# 1.0625#, 1.0625#, 1.0625#, 1.0625# #))
  , FX4 (packFloatX4# (# 1.125#, 1.125#, 1.125#, 1.125# #))
  , FX4 (broadcastFloatX4# 1.1875#)
  , FX4 (packFloatX4# (# 1.25#, 1.25#, 1.25#, 1.25# #))
  , FX4 (broadcastFloatX4# 1.3125#)
  , FX4 (packFloatX4# (# -1.375#, -1.375#, -1.375#, -1.375# #))
  , FX4 (broadcastFloatX4# 1.4375#)
  , FX4 (broadcastFloatX4# -1.5#)
  , FX4 (packFloatX4# (# -1.5625#, -1.5625#, -1.5625#, -1.5625# #))
  , FX4 (packFloatX4# (# -1.625#, -1.625#, -1.625#, -1.625# #))
  , FX4 (packFloatX4# (# -1.6875#, -1.6875#, -1.6875#, -1.6875# #))
  , FX4 (packFloatX4# (# 1.75#, 1.75#, 1.75#, 1.75# #))
  , FX4 (packFloatX4# (# -1.8125#, -1.8125#, -1.8125#, -1.8125# #))
  , FX4 (packFloatX4# (# 1.875#, 1.875#, 1.875#, 1.875# #))
  , FX4 (packFloatX4# (# -1.9375#, -1.9375#, -1.9375#, -1.9375# #))
  , FX4 (broadcastFloatX4# 2.0#)
  , FX4 (packFloatX4# (# 2.125#, 2.125#, 2.125#, 2.125# #))
  , FX4 (broadcastFloatX4# 2.25#)
  , FX4 (packFloatX4# (# 2.375#, 2.375#, 2.375#, 2.375# #))
  , FX4 (broadcastFloatX4# -2.5#)
  , FX4 (packFloatX4# (# -2.625#, -2.625#, -2.625#, -2.625# #))
  , FX4 (broadcastFloatX4# -2.75#)
  , FX4 (broadcastFloatX4# -2.875#)
  , FX4 (packFloatX4# (# -3.0#, -3.0#, -3.0#, -3.0# #))
  , FX4 (broadcastFloatX4# -3.125#)
  , FX4 (packFloatX4# (# 3.25#, 3.25#, 3.25#, 3.25# #))
  , FX4 (broadcastFloatX4# -3.375#)
  , FX4 (broadcastFloatX4# -3.5#)
  , FX4 (packFloatX4# (# 3.625#, 3.625#, 3.625#, 3.625# #))
  , FX4 (packFloatX4# (# -3.75#, -3.75#, -3.75#, -3.75# #))
  , FX4 (packFloatX4# (# -3.875#, -3.875#, -3.875#, -3.875# #))
  , FX4 (packFloatX4# (# 4.0#, 4.0#, 4.0#, 4.0# #))
  , FX4 (packFloatX4# (# -4.25#, -4.25#, -4.25#, -4.25# #))
  , FX4 (broadcastFloatX4# -4.5#)
  , FX4 (broadcastFloatX4# 4.75#)
  , FX4 (broadcastFloatX4# -5.0#)
  , FX4 (broadcastFloatX4# -5.25#)
  , FX4 (packFloatX4# (# -5.5#, -5.5#, -5.5#, -5.5# #))
  , FX4 (broadcastFloatX4# 5.75#)
  , FX4 (broadcastFloatX4# 6.0#)
  , FX4 (packFloatX4# (# -6.25#, -6.25#, -6.25#, -6.25# #))
  , FX4 (broadcastFloatX4# -6.5#)
  , FX4 (broadcastFloatX4# -6.75#)
  , FX4 (broadcastFloatX4# 7.0#)
  , FX4 (packFloatX4# (# 7.25#, 7.25#, 7.25#, 7.25# #))
  , FX4 (packFloatX4# (# -7.5#, -7.5#, -7.5#, -7.5# #))
  , FX4 (packFloatX4# (# -7.75#, -7.75#, -7.75#, -7.75# #))
  , FX4 (packFloatX4# (# 8.0#, 8.0#, 8.0#, 8.0# #))
  , FX4 (packFloatX4# (# -8.5#, -8.5#, -8.5#, -8.5# #))
  , FX4 (packFloatX4# (# -9.0#, -9.0#, -9.0#, -9.0# #))
  , FX4 (broadcastFloatX4# -9.5#)
  , FX4 (packFloatX4# (# 10.0#, 10.0#, 10.0#, 10.0# #))
  , FX4 (packFloatX4# (# 10.5#, 10.5#, 10.5#, 10.5# #))
  , FX4 (broadcastFloatX4# 11.0#)
  , FX4 (packFloatX4# (# -11.5#, -11.5#, -11.5#, -11.5# #))
  , FX4 (broadcastFloatX4# -12.0#)
  , FX4 (packFloatX4# (# -12.5#, -12.5#, -12.5#, -12.5# #))
  , FX4 (broadcastFloatX4# -13.0#)
  , FX4 (packFloatX4# (# 13.5#, 13.5#, 13.5#, 13.5# #))
  , FX4 (broadcastFloatX4# 14.0#)
  , FX4 (packFloatX4# (# 14.5#, 14.5#, 14.5#, 14.5# #))
  , FX4 (broadcastFloatX4# -15.0#)
  , FX4 (packFloatX4# (# -15.5#, -15.5#, -15.5#, -15.5# #))
  , FX4 (broadcastFloatX4# 16.0#)
  , FX4 (broadcastFloatX4# -17.0#)
  , FX4 (broadcastFloatX4# -18.0#)
  , FX4 (broadcastFloatX4# -19.0#)
  , FX4 (broadcastFloatX4# -20.0#)
  , FX4 (packFloatX4# (# 21.0#, 21.0#, 21.0#, 21.0# #))
  , FX4 (packFloatX4# (# 22.0#, 22.0#, 22.0#, 22.0# #))
  , FX4 (broadcastFloatX4# -23.0#)
  , FX4 (broadcastFloatX4# 24.0#)
  , FX4 (packFloatX4# (# -25.0#, -25.0#, -25.0#, -25.0# #))
  , FX4 (packFloatX4# (# -26.0#, -26.0#, -26.0#, -26.0# #))
  , FX4 (packFloatX4# (# -27.0#, -27.0#, -27.0#, -27.0# #))
  , FX4 (broadcastFloatX4# -28.0#)
  , FX4 (broadcastFloatX4# -29.0#)
  , FX4 (broadcastFloatX4# 30.0#)
  , FX4 (packFloatX4# (# -31.0#, -31.0#, -31.0#, -31.0# #))
  , FX4 (packFloatX4# (# 0.0#, 0.0#, 0.0#, 0.0# #))
  , FX4 (broadcastFloatX4# (negateFloat# 0.0#))
  , FX4 (broadcastFloatX4# 32.0#)
  , FX4 (packFloatX4# (# -30.5#, -30.5#, -30.5#, -30.5# #))
  ]
{-# OPAQUE floatConstants #-}

doubleConstants :: [DoubleX2]
doubleConstants =
  [ DX2 (packDoubleX2# (# -0.125##, -0.125## #))
  , DX2 (broadcastDoubleX2# 0.1328125##)
  , DX2 (packDoubleX2# (# -0.140625##, -0.140625## #))
  , DX2 (broadcastDoubleX2# 0.1484375##)
  , DX2 (packDoubleX2# (# -0.15625##, -0.15625## #))
  , DX2 (broadcastDoubleX2# -0.1640625##)
  , DX2 (packDoubleX2# (# 0.171875##, 0.171875## #))
  , DX2 (packDoubleX2# (# -0.1796875##, -0.1796875## #))
  , DX2 (packDoubleX2# (# 0.1875##, 0.1875## #))
  , DX2 (packDoubleX2# (# -0.1953125##, -0.1953125## #))
  , DX2 (broadcastDoubleX2# -0.203125##)
  , DX2 (packDoubleX2# (# -0.2109375##, -0.2109375## #))
  , DX2 (packDoubleX2# (# 0.21875##, 0.21875## #))
  , DX2 (packDoubleX2# (# 0.2265625##, 0.2265625## #))
  , DX2 (broadcastDoubleX2# 0.234375##)
  , DX2 (broadcastDoubleX2# 0.2421875##)
  , DX2 (broadcastDoubleX2# 0.25##)
  , DX2 (broadcastDoubleX2# -0.265625##)
  , DX2 (broadcastDoubleX2# -0.28125##)
  , DX2 (broadcastDoubleX2# 0.296875##)
  , DX2 (broadcastDoubleX2# 0.3125##)
  , DX2 (broadcastDoubleX2# 0.328125##)
  , DX2 (packDoubleX2# (# 0.34375##, 0.34375## #))
  , DX2 (packDoubleX2# (# 0.359375##, 0.359375## #))
  , DX2 (broadcastDoubleX2# -0.375##)
  , DX2 (packDoubleX2# (# 0.390625##, 0.390625## #))
  , DX2 (broadcastDoubleX2# -0.40625##)
  , DX2 (broadcastDoubleX2# 0.421875##)
  , DX2 (packDoubleX2# (# -0.4375##, -0.4375## #))
  , DX2 (broadcastDoubleX2# 0.453125##)
  , DX2 (packDoubleX2# (# -0.46875##, -0.46875## #))
  , DX2 (packDoubleX2# (# -0.484375##, -0.484375## #))
  , DX2 (broadcastDoubleX2# 0.5##)
  , DX2 (packDoubleX2# (# -0.53125##, -0.53125## #))
  , DX2 (packDoubleX2# (# 0.5625##, 0.5625## #))
  , DX2 (packDoubleX2# (# 0.59375##, 0.59375## #))
  , DX2 (packDoubleX2# (# 0.625##, 0.625## #))
  , DX2 (broadcastDoubleX2# 0.65625##)
  , DX2 (broadcastDoubleX2# -0.6875##)
  , DX2 (broadcastDoubleX2# -0.71875##)
  , DX2 (packDoubleX2# (# 0.75##, 0.75## #))
  , DX2 (broadcastDoubleX2# 0.78125##)
  , DX2 (packDoubleX2# (# 0.8125##, 0.8125## #))
  , DX2 (broadcastDoubleX2# -0.84375##)
  , DX2 (packDoubleX2# (# -0.875##, -0.875## #))
  , DX2 (broadcastDoubleX2# 0.90625##)
  , DX2 (packDoubleX2# (# -0.9375##, -0.9375## #))
  , DX2 (broadcastDoubleX2# -0.96875##)
  , DX2 (packDoubleX2# (# -1.0##, -1.0## #))
  , DX2 (broadcastDoubleX2# -1.0625##)
  , DX2 (broadcastDoubleX2# 1.125##)
  , DX2 (broadcastDoubleX2# 1.1875##)
  , DX2 (broadcastDoubleX2# 1.25##)
  , DX2 (packDoubleX2# (# -1.3125##, -1.3125## #))
  , DX2 (broadcastDoubleX2# 1.375##)
  , DX2 (broadcastDoubleX2# -1.4375##)
  , DX2 (broadcastDoubleX2# -1.5##)
  , DX2 (broadcastDoubleX2# 1.5625##)
  , DX2 (packDoubleX2# (# -1.625##, -1.625## #))
  , DX2 (broadcastDoubleX2# 1.6875##)
  , DX2 (broadcastDoubleX2# -1.75##)
  , DX2 (broadcastDoubleX2# -1.8125##)
  , DX2 (broadcastDoubleX2# -1.875##)
  , DX2 (broadcastDoubleX2# 1.9375##)
  , DX2 (broadcastDoubleX2# 2.0##)
  , DX2 (packDoubleX2# (# -2.125##, -2.125## #))
  , DX2 (broadcastDoubleX2# 2.25##)
  , DX2 (packDoubleX2# (# -2.375##, -2.375## #))
  , DX2 (packDoubleX2# (# 2.5##, 2.5## #))
  , DX2 (broadcastDoubleX2# -2.625##)
  , DX2 (broadcastDoubleX2# -2.75##)
  , DX2 (packDoubleX2# (# 2.875##, 2.875## #))
  , DX2 (packDoubleX2# (# -3.0##, -3.0## #))
  , DX2 (packDoubleX2# (# 3.125##, 3.125## #))
  , DX2 (broadcastDoubleX2# 3.25##)
  , DX2 (broadcastDoubleX2# 3.375##)
  , DX2 (packDoubleX2# (# 3.5##, 3.5## #))
  , DX2 (packDoubleX2# (# 3.625##, 3.625## #))
  , DX2 (packDoubleX2# (# -3.75##, -3.75## #))
  , DX2 (packDoubleX2# (# -3.875##, -3.875## #))
  , DX2 (packDoubleX2# (# 4.0##, 4.0## #))
  , DX2 (packDoubleX2# (# -4.25##, -4.25## #))
  , DX2 (packDoubleX2# (# 4.5##, 4.5## #))
  , DX2 (packDoubleX2# (# 4.75##, 4.75## #))
  , DX2 (packDoubleX2# (# 5.0##, 5.0## #))
  , DX2 (broadcastDoubleX2# 5.25##)
  , DX2 (packDoubleX2# (# 5.5##, 5.5## #))
  , DX2 (packDoubleX2# (# -5.75##, -5.75## #))
  , DX2 (broadcastDoubleX2# -6.0##)
  , DX2 (packDoubleX2# (# -6.25##, -6.25## #))
  , DX2 (packDoubleX2# (# 6.5##, 6.5## #))
  , DX2 (broadcastDoubleX2# -6.75##)
  , DX2 (broadcastDoubleX2# -7.0##)
  , DX2 (broadcastDoubleX2# -7.25##)
  , DX2 (packDoubleX2# (# -7.5##, -7.5## #))
  , DX2 (packDoubleX2# (# -7.75##, -7.75## #))
  , DX2 (packDoubleX2# (# -8.0##, -8.0## #))
  , DX2 (broadcastDoubleX2# 8.5##)
  , DX2 (broadcastDoubleX2# -9.0##)
  , DX2 (broadcastDoubleX2# 9.5##)
  , DX2 (broadcastDoubleX2# -10.0##)
  , DX2 (broadcastDoubleX2# -10.5##)
  , DX2 (broadcastDoubleX2# 11.0##)
  , DX2 (broadcastDoubleX2# -11.5##)
  , DX2 (broadcastDoubleX2# -12.0##)
  , DX2 (broadcastDoubleX2# 12.5##)
  , DX2 (packDoubleX2# (# 13.0##, 13.0## #))
  , DX2 (broadcastDoubleX2# -13.5##)
  , DX2 (broadcastDoubleX2# 14.0##)
  , DX2 (packDoubleX2# (# -14.5##, -14.5## #))
  , DX2 (packDoubleX2# (# 15.0##, 15.0## #))
  , DX2 (broadcastDoubleX2# -15.5##)
  , DX2 (broadcastDoubleX2# -16.0##)
  , DX2 (broadcastDoubleX2# 17.0##)
  , DX2 (broadcastDoubleX2# 18.0##)
  , DX2 (packDoubleX2# (# 19.0##, 19.0## #))
  , DX2 (packDoubleX2# (# 20.0##, 20.0## #))
  , DX2 (packDoubleX2# (# 21.0##, 21.0## #))
  , DX2 (packDoubleX2# (# -22.0##, -22.0## #))
  , DX2 (broadcastDoubleX2# 23.0##)
  , DX2 (broadcastDoubleX2# -24.0##)
  , DX2 (packDoubleX2# (# -25.0##, -25.0## #))
  , DX2 (broadcastDoubleX2# -26.0##)
  , DX2 (broadcastDoubleX2# 27.0##)
  , DX2 (broadcastDoubleX2# 28.0##)
  , DX2 (packDoubleX2# (# 29.0##, 29.0## #))
  , DX2 (broadcastDoubleX2# -30.0##)
  , DX2 (broadcastDoubleX2# -31.0##)
  , DX2 (broadcastDoubleX2# 0.0##)
  , DX2 (broadcastDoubleX2# (negateDouble# 0.0##))
  , DX2 (broadcastDoubleX2# -32.0##)
  , DX2 (broadcastDoubleX2# -30.5##)
  ]
{-# OPAQUE doubleConstants #-}

main :: IO ()
main = do
  putStrLn "Float:"
  forM_ floatConstants $ \(FX4 v) ->
    case unpackFloatX4# v of
      (# x0, x1, x2, x3 #) ->
        print (F# x0, F# x1, F# x2, F# x3)
  putStrLn "Double:"
  forM_ doubleConstants $ \(DX2 v) ->
    case unpackDoubleX2# v of
      (# x0, x1 #) ->
        print (D# x0, D# x1)

{- Generated by:
{- cabal:
build-depends: base, random
-}
import System.Random.Stateful
import qualified Data.List as List
import Control.Monad

values :: Fractional a => [a]
values = [fromRational (m / 16 * 2^^(e :: Int)) | e <- [-3..4], m <- [16..31]]

extra :: Fractional a => [a]
extra = [0.0,-0.0,32.0,-30.5]

main :: IO ()
main = do
  -- print $ (values :: [Float])
  gen <- newIOGenM (mkStdGen 42)
  let float :: Double -> IO String
      float x = do
        usePack <- randomM gen
        neg <- randomM gen
        let k = if isNegativeZero x then
                  "(negateFloat# 0.0#)"
                else
                  (if neg && x > 0.0 then "-" else "") ++ show x ++ "#"
        let e = if usePack
                then "FX4 (packFloatX4# (# " ++ k ++ ", " ++ k ++ ", " ++ k ++ ", " ++ k ++ " #))"
                else "FX4 (broadcastFloatX4# " ++ k ++ ")"
        pure e
  let double :: Double -> IO String
      double x = do
        usePack <- randomM gen
        neg <- randomM gen
        let k = if isNegativeZero x then
                  "(negateDouble# 0.0##)"
                else
                  (if neg && x > 0.0 then "-" else "") ++ show x ++ "##"
        let e = if usePack
                then "DX2 (packDoubleX2# (# " ++ k ++ ", " ++ k ++ " #))"
                else "DX2 (broadcastDoubleX2# " ++ k ++ ")"
        pure e
  ds <- mapM float (values ++ extra)
  putStrLn "floatConstants :: [FloatX4]"
  putStrLn "floatConstants ="
  putStrLn $ "  [ " ++ List.intercalate "\n  , " ds ++ "\n  ]"
  putStrLn "{-# OPAQUE floatConstants #-}"
  putStrLn ""
  ds <- mapM double (values ++ extra)
  putStrLn "doubleConstants :: [DoubleX2]"
  putStrLn "doubleConstants ="
  putStrLn $ "  [ " ++ List.intercalate "\n  , " ds ++ "\n  ]"
  putStrLn "{-# OPAQUE doubleConstants #-}"
-}
