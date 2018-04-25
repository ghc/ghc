{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Enum-related parallel operations on unlifted arrays
module Data.Array.Parallel.Unlifted.Parallel.Enum 
        ( enumFromToUP
        , enumFromThenToUP
        , enumFromStepLenUP
        , enumFromStepLenEachUP)
where
import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Distributed.What
import Data.Array.Parallel.Unlifted.Parallel.Combinators (mapUP)
import GHC.Base                                          (divInt)


delay_inline :: a -> a
delay_inline x = x
{-# INLINE [0] delay_inline #-}


enumFromToUP :: (Unbox a, Enum a) => a -> a -> Vector a
enumFromToUP start end 
 = mapUP toEnum (enumFromStepLenUP start' 1 len)
 where  start' = fromEnum start
        end'   = fromEnum end
        len    = delay_inline max (end' - start' + 1) 0
{-# INLINE_UP enumFromToUP #-}


enumFromThenToUP :: (Unbox a, Enum a) => a -> a -> a -> Vector a
enumFromThenToUP start next end 
 = mapUP toEnum (enumFromStepLenUP start' delta len)
 where  start' = fromEnum start
        next'  = fromEnum next
        end'   = fromEnum end
        delta  = next' - start'

        -- distance between start' and end' expressed in deltas
        dist   = (end' - start' + delta) `divInt` delta
        len    = max dist 0
{-# INLINE_UP enumFromThenToUP #-}


enumFromStepLenUP :: Int -> Int -> Int -> Vector Int
enumFromStepLenUP start delta len =
  joinD theGang balanced
  (mapD (What "enumFromStepLenUP/gen") theGang gen
  (splitLenIdxD theGang len))
  where
    gen (n,i) = Seq.enumFromStepLen (i * delta + start) delta n
{-# INLINE_UP enumFromStepLenUP #-}


enumFromStepLenEachUP 
        :: Int -> Vector Int -> Vector Int -> Vector Int -> Vector Int
enumFromStepLenEachUP _n starts steps lens
  = joinD theGang unbalanced
  $ mapD  (What "enumFromStepLenEachUP/enum") theGang enum
  $ splitD theGang unbalanced (Seq.zip (Seq.zip starts steps) lens)
  where
    enum ps = let (qs, llens) = Seq.unzip ps
                  (lstarts, lsteps) = Seq.unzip qs
              in Seq.enumFromStepLenEach (Seq.sum llens) lstarts lsteps llens
{-# INLINE_UP enumFromStepLenEachUP #-}

