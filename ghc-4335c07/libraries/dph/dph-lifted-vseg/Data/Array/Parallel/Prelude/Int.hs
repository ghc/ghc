{-# OPTIONS_GHC -fvectorise #-}

module Data.Array.Parallel.Prelude.Int 
        ( Int
          
          -- * Ord
        , (==), (/=), (<), (<=), (>), (>=), min, max
        , maximumP,  minimumP
        , maxIndexP, minIndexP
     
          -- * Num
        , (+), (-), (*)
        , negate, abs
        , sumP, productP
        
          -- * Integral
        , div, mod, sqrt
        
          -- * Enum
        , enumFromToP)
where
-- Primitives needed by the vectoriser.
import Data.Array.Parallel.Prim                         ()      
import Data.Array.Parallel.Prelude.Base                 (Bool)
import Data.Array.Parallel.PArr
import Data.Array.Parallel.PArray
import Data.Array.Parallel.Lifted                       ((:->)(..))
import qualified Data.Array.Parallel.Lifted             as L
import qualified Data.Array.Parallel.PArray.Scalar      as SC
import qualified Prelude as P
import Prelude (Int)

infixl 7 *
infixl 6 +, -
infix  4 ==, /=, <, <=, >, >=
infixl 7 `div`, `mod`

-- Ord ------------------------------------------------------------------------
(==), (/=), (<), (<=), (>), (>=) :: Int -> Int -> Bool
(==) = (P.==)
(/=) = (P./=)
(<=) = (P.<=)
(<)  = (P.<)
(>=) = (P.>=)
(>)  = (P.>)


-- min/max ----------------------------
min, max :: Int -> Int -> Int
min = P.min
max = P.max


-- minimum/maximum --------------------
minimumP, maximumP :: PArr Int -> Int

minimumP arr    = headPArr arr
{-# NOINLINE  minimumP #-}
{-# VECTORISE minimumP = minimumPP #-}

maximumP arr    = headPArr arr
{-# NOINLINE  maximumP #-}
{-# VECTORISE maximumP = maximumPP #-}

minimumPP, maximumPP :: PArray Int :-> Int
minimumPP      = L.closure1' (SC.fold1 P.min) (SC.fold1s P.min)
{-# INLINE      minimumPP #-}
{-# NOVECTORISE minimumPP #-}

maximumPP      = L.closure1' (SC.fold1 P.max) (SC.fold1s P.max)
{-# INLINE      maximumPP #-}
{-# NOVECTORISE maximumPP #-}


-- minIndex/maxIndex ------------------
minIndexP :: PArr Int -> Int
minIndexP !_    = 0 
{-# NOINLINE  minIndexP #-}
{-# VECTORISE minIndexP = minIndexPP #-}

minIndexPP :: PArray Int :-> Int
minIndexPP      = L.closure1' (SC.fold1Index min') (SC.fold1sIndex min')
{-# INLINE      minIndexPP #-}
{-# NOVECTORISE minIndexPP #-}

min' :: P.Ord b => (a, b) -> (a, b) -> (a, b)
min' (i,x) (j,y) | x P.<= y    = (i,x)
                 | P.otherwise = (j,y)
{-# NOVECTORISE min' #-}


maxIndexP :: PArr Int -> Int
maxIndexP _     = 0
{-# NOINLINE  maxIndexP #-}
{-# VECTORISE maxIndexP = maxIndexPP #-}

maxIndexPP :: PArray Int :-> Int
maxIndexPP      = L.closure1' (SC.fold1Index max') (SC.fold1sIndex max')
{-# INLINE      maxIndexPP #-}
{-# NOVECTORISE maxIndexPP #-}

max' :: P.Ord b => (a, b) -> (a, b) -> (a, b)
max' (i,x) (j,y) | x P.>= y    = (i,x)
                 | P.otherwise = (j,y)
{-# NOVECTORISE max' #-}


-- Num ------------------------------------------------------------------------
(+), (-), (*) :: Int -> Int -> Int
(+) = (P.+)
(-) = (P.-)
(*) = (P.*)


-- negate/abs -------------------------
negate, abs :: Int -> Int
negate  = P.negate
abs     = P.abs

-- sum/product ------------------------
sumP, productP :: PArr Int -> Int

sumP arr        = headPArr arr
{-# NOINLINE  sumP #-}
{-# VECTORISE sumP      = sumPP #-}

productP arr    = headPArr arr
{-# NOINLINE  productP #-}
{-# VECTORISE productP  = productPP #-}

sumPP, productPP :: PArray Int :-> Int
sumPP          = L.closure1' (SC.fold (+) 0) (SC.folds (+) 0)
{-# INLINE      sumPP #-}
{-# NOVECTORISE sumPP #-}

productPP      = L.closure1' (SC.fold (*) 1) (SC.folds (*) 1)
{-# INLINE      productPP #-}
{-# NOVECTORISE productPP #-}


-- Integral -------------------------------------------------------------------
div, mod :: Int -> Int -> Int
div = P.div
mod = P.mod

sqrt :: Int -> Int 
sqrt n = P.floor (P.sqrt (P.fromIntegral n) :: P.Double)

-- Enum -----------------------------------------------------------------------
enumFromToP :: Int -> Int -> PArr Int
enumFromToP !_ !_       = emptyPArr
{-# NOINLINE  enumFromToP #-}
{-# VECTORISE enumFromToP = enumFromToPP #-}

enumFromToPP :: Int :-> Int :-> PArray Int
enumFromToPP    = L.closure2' SC.enumFromTo SC.enumFromTol
{-# INLINE      enumFromToPP #-}
{-# NOVECTORISE enumFromToPP #-}
