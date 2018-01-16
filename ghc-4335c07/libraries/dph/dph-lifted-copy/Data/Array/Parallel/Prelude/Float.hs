{-# OPTIONS_GHC -fvectorise #-}

module Data.Array.Parallel.Prelude.Float (
  Float,
  
  -- Ord
  (==), (/=), (<), (<=), (>), (>=), min, max,

  minimumP, maximumP, minIndexP, maxIndexP,

  -- Num
  (+), (-), (*), negate, abs,

  sumP, productP,

  -- Fractional
  (/), recip,

  -- Floating
  pi, exp, sqrt, log, (**), logBase,
  sin, tan, cos, asin, atan, acos,
  sinh, tanh, cosh, asinh, atanh, acosh,

  -- RealFrac and similar
  fromInt,

  truncate, round, ceiling, floor,
) where

import Data.Array.Parallel.Prim ()       -- dependency required by the vectoriser

import Data.Array.Parallel.Prelude.Base

import Data.Array.Parallel.PArr
import Data.Array.Parallel.Lifted.Scalar
import Data.Array.Parallel.Lifted.Closure

import qualified Prelude as P
import Prelude (Int, Float)


infixr 8 **
infixl 7 *, /
infixl 6 +, -
infix 4 ==, /=, <, <=, >, >=


(==), (/=), (<), (<=), (>), (>=) :: Float -> Float -> Bool
(==) = (P.==)
(/=) = (P./=)
(<=) = (P.<=)
(<)  = (P.<)
(>=) = (P.>=)
(>)  = (P.>)

min, max :: Float -> Float -> Float
min = P.min
max = P.max

minimumP, maximumP :: PArr Float -> Float
{-# NOINLINE minimumP #-}
minimumP a = a `indexPArr` 0
{-# VECTORISE minimumP = minimumP_v #-}
{-# NOINLINE maximumP #-}
maximumP a = a `indexPArr` 0
{-# VECTORISE maximumP = maximumP_v #-}

minimumP_v, maximumP_v:: PArray Float :-> Float
{-# INLINE minimumP_v #-}
minimumP_v = closure1 (scalar_fold1 P.min) (scalar_fold1s P.min)
{-# NOVECTORISE minimumP_v #-}
{-# INLINE maximumP_v #-}
maximumP_v = closure1 (scalar_fold1 P.max) (scalar_fold1s P.max)
{-# NOVECTORISE maximumP_v #-}

minIndexP :: PArr Float -> Int
{-# NOINLINE minIndexP #-}
minIndexP _ = 0   -- FIXME: add proper implementation
{-# VECTORISE minIndexP = minIndexPA #-}

minIndexPA :: PArray Float :-> Int
{-# INLINE minIndexPA #-}
minIndexPA = closure1 (scalar_fold1Index min') (scalar_fold1sIndex min')
{-# NOVECTORISE minIndexPA #-}

min' (i,x) (j,y) | x P.<= y    = (i,x)
                 | P.otherwise = (j,y)
{-# NOVECTORISE min' #-}

maxIndexP :: PArr Float -> Int
{-# NOINLINE maxIndexP #-}
maxIndexP _ = 0   -- FIXME: add proper implementation
{-# VECTORISE maxIndexP = maxIndexPA #-}

maxIndexPA :: PArray Float :-> Int
{-# INLINE maxIndexPA #-}
maxIndexPA = closure1 (scalar_fold1Index max') (scalar_fold1sIndex max')
{-# NOVECTORISE maxIndexPA #-}

max' (i,x) (j,y) | x P.>= y    = (i,x)
                 | P.otherwise = (j,y)
{-# NOVECTORISE max' #-}

(+), (-), (*) :: Float -> Float -> Float
(+) = (P.+)
(-) = (P.-)
(*) = (P.*)

negate, abs :: Float -> Float
negate = P.negate
abs = P.abs

sumP, productP :: PArr Float -> Float
{-# NOINLINE sumP #-}
sumP a = a `indexPArr` 0
{-# VECTORISE sumP = sumP_v #-}
{-# NOINLINE productP #-}
productP a = a `indexPArr` 0
{-# VECTORISE productP = productP_v #-}

sumP_v, productP_v:: PArray Float :-> Float
{-# INLINE sumP_v #-}
sumP_v     = closure1 (scalar_fold (+) 0) (scalar_folds (+) 0)
{-# NOVECTORISE sumP_v #-}
{-# INLINE productP_v #-}
productP_v = closure1 (scalar_fold (*) 1) (scalar_folds (*) 1)
{-# NOVECTORISE productP_v #-}

(/) :: Float -> Float -> Float
(/) = (P./)

recip :: Float -> Float
recip = P.recip

pi :: Float
pi = P.pi
{-# NOVECTORISE pi #-}

exp, sqrt, log, sin, tan, cos, asin, atan, acos, sinh, tanh, cosh,
  asinh, atanh, acosh :: Float -> Float
exp = P.exp
sqrt = P.sqrt
log = P.log
sin = P.sin
tan = P.tan
cos = P.cos
asin = P.asin
atan = P.atan
acos = P.acos
sinh = P.sinh
tanh = P.tanh
cosh = P.cosh
asinh = P.asinh
atanh = P.atanh
acosh = P.acosh

(**), logBase :: Float -> Float -> Float
(**) = (P.**)
logBase = P.logBase

fromInt :: Int -> Float
fromInt = P.fromIntegral

truncate, round, ceiling, floor :: Float -> Int
truncate = P.truncate
round = P.round
ceiling = P.ceiling
floor = P.floor
