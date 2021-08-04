{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
module Basement.Floating
    ( integerToDouble
    , naturalToDouble
    , doubleExponant
    , integerToFloat
    , naturalToFloat
    , wordToFloat
    , floatToWord
    , wordToDouble
    , doubleToWord
    ) where

import           GHC.Types
import           GHC.Prim
import           GHC.Float
import           GHC.Word
import           GHC.ST
import           Basement.Compat.Base
import           Basement.Compat.Natural
import qualified Prelude (fromInteger, toInteger, (^^))

integerToDouble :: Integer -> Double
integerToDouble = Prelude.fromInteger
-- this depends on integer-gmp
--integerToDouble i = D# (doubleFromInteger i)

naturalToDouble :: Natural -> Double
naturalToDouble = integerToDouble . Prelude.toInteger

doubleExponant :: Double -> Int -> Double
doubleExponant = (Prelude.^^)

integerToFloat :: Integer -> Float
integerToFloat = Prelude.fromInteger

naturalToFloat :: Natural -> Float
naturalToFloat = integerToFloat . Prelude.toInteger

wordToFloat :: Word32 -> Float
wordToFloat (W32# x) = runST $ ST $ \s1 ->
    case newByteArray# 4# s1             of { (# s2, mbarr #) ->
    case writeWord32Array# mbarr 0# x s2 of { s3              ->
    case readFloatArray# mbarr 0# s3     of { (# s4, f #)     ->
        (# s4, F# f #) }}}
{-# INLINE wordToFloat #-}

floatToWord :: Float -> Word32
floatToWord (F# x) = runST $ ST $ \s1 ->
    case newByteArray# 4# s1            of { (# s2, mbarr #) ->
    case writeFloatArray# mbarr 0# x s2 of { s3              ->
    case readWord32Array# mbarr 0# s3   of { (# s4, w #)     ->
        (# s4, W32# w #) }}}
{-# INLINE floatToWord #-}

wordToDouble :: Word64 -> Double
wordToDouble (W64# x) = runST $ ST $ \s1 ->
    case newByteArray# 8# s1             of { (# s2, mbarr #) ->
    case writeWord64Array# mbarr 0# x s2 of { s3              ->
    case readDoubleArray# mbarr 0# s3    of { (# s4, f #)     ->
        (# s4, D# f #) }}}
{-# INLINE wordToDouble #-}

doubleToWord :: Double -> Word64
doubleToWord (D# x) = runST $ ST $ \s1 ->
    case newByteArray# 8# s1             of { (# s2, mbarr #) ->
    case writeDoubleArray# mbarr 0# x s2 of { s3              ->
    case readWord64Array# mbarr 0# s3    of { (# s4, w #)     ->
        (# s4, W64# w #) }}}
{-# INLINE doubleToWord #-}
