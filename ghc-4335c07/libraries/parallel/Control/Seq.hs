{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Parallel.SeqStrategies
-- Copyright   :  (c) The University of Glasgow 2001-2009
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Sequential strategies provide ways to compositionally specify
-- the degree of evaluation of a data type between the extremes of
-- no evaluation and full evaluation.
-- Sequential strategies may be viewed as complimentary to the parallel
-- ones (see module "Control.Parallel.Strategies").
--

module Control.Seq
       (
         -- * The sequential strategy type
         Strategy

         -- * Application of sequential strategies
       , using            -- :: a -> Strategy a -> a
       , withStrategy     -- :: Strategy a -> a -> a

         -- * Basic sequential strategies
       , r0               -- :: Strategy a
       , rseq
       , rdeepseq         -- :: NFData a => Strategy a

         -- * Sequential strategies for lists
       , seqList          -- :: Strategy a -> Strategy [a]
       , seqListN         -- :: Int -> Strategy a -> Strategy [a]
       , seqListNth

         -- * Sequential strategies for foldable data types
       , seqFoldable      -- :: Foldable t => Strategy a -> Strategy (t a)
       , seqMap           -- :: Strategy k -> Strategy v -> Strategy (Map k v)
       , seqArray         -- :: Ix i => Strategy a -> Strategy (Array i a)
       , seqArrayBounds   -- :: Ix i => Strategy i -> Strategy (Array i a)

         -- * Sequential strategies for tuples

         -- | Evaluate the components of a tuple according to the given strategies.
         -- No guarantee is given as to the order of evaluation.

       , seqTuple2        -- :: Strategy a -> ... -> Strategy (a,...)
       , seqTuple3
       , seqTuple4
       , seqTuple5
       , seqTuple6
       , seqTuple7
       , seqTuple8
       , seqTuple9
       ) where

import Control.DeepSeq (NFData, deepseq)
#if MIN_VERSION_base(4,8,0)
import Data.Foldable (toList)
#else
import Data.Foldable (Foldable, toList)
#endif
import Data.Map (Map)
import qualified Data.Map (toList)
#if !((__GLASGOW_HASKELL__ >= 711) && MIN_VERSION_array(0,5,1))
import Data.Ix (Ix)
#endif
import Data.Array (Array)
import qualified Data.Array (bounds, elems)

infixl 0 `using`   -- lowest precedence and associate to the left

-- --------------------------------------------------------------------------
-- Sequential strategies

-- | The type @'Strategy' a@ is @a -> ()@.
-- Thus, a strategy is a function whose sole purpose it is to evaluate
-- its argument (either in full or in part).
type Strategy a = a -> ()

-- | Evaluate a value using the given strategy.
using :: a -> Strategy a -> a
x `using` strat = strat x `seq` x

-- | Evaluate a value using the given strategy.
-- This is simply 'using' with arguments reversed.
withStrategy :: Strategy a -> a -> a
withStrategy = flip using

-- --------------------------------------------------------------------------
-- Basic sequential strategies

-- | 'r0' performs *no* evaluation.
r0 :: Strategy a
r0 _ = ()

-- | 'rseq' evaluates its argument to weak head normal form.
rseq :: Strategy a
rseq x = x `seq` ()

-- | 'rdeepseq' fully evaluates its argument.
-- Relies on class 'NFData' from module "Control.DeepSeq".
rdeepseq :: NFData a => Strategy a
rdeepseq x = x `deepseq` ()


-- --------------------------------------------------------------------------
-- Sequential strategies for lists

-- | Evaluate each element of a list according to the given strategy.
-- This function is a specialisation of 'seqFoldable' to lists.
seqList :: Strategy a -> Strategy [a]
seqList _strat []    = ()
seqList strat (x:xs) = strat x `seq` seqList strat xs
-- Alternative definition via seqFoldable:
-- seqList = seqFoldable

-- | Evaluate the first n elements of a list according to the given strategy.
seqListN :: Int -> Strategy a -> Strategy [a]
seqListN 0  _strat _     = ()
seqListN !_ _strat []    = ()
seqListN !n strat (x:xs) = strat x `seq` seqListN (n-1) strat xs

-- | Evaluate the nth element of a list (if there is such) according to
-- the given strategy.
-- The spine of the list up to the nth element is evaluated as a side effect.
seqListNth :: Int -> Strategy a -> Strategy [a]
seqListNth 0  strat  (x:_)  = strat x
seqListNth !_ _strat []     = ()
seqListNth !n strat  (_:xs) = seqListNth (n-1) strat xs


-- --------------------------------------------------------------------------
-- Sequential strategies for foldable data types

-- | Evaluate the elements of a foldable data structure according to
-- the given strategy.
seqFoldable :: Foldable t => Strategy a -> Strategy (t a)
seqFoldable strat = seqList strat . toList
-- Alternative definition via foldl':
-- seqFoldable strat = foldl' (const strat) ()

{-# SPECIALISE seqFoldable :: Strategy a -> Strategy [a] #-}

-- | Evaluate the elements of an array according to the given strategy.
-- Evaluation of the array bounds may be triggered as a side effect.
#if (__GLASGOW_HASKELL__ >= 711) && MIN_VERSION_array(0,5,1)
seqArray :: Strategy a -> Strategy (Array i a)
#else
seqArray :: Ix i => Strategy a -> Strategy (Array i a)
#endif
seqArray strat = seqList strat . Data.Array.elems

-- | Evaluate the bounds of an array according to the given strategy.
#if (__GLASGOW_HASKELL__ >= 711) && MIN_VERSION_array(0,5,1)
seqArrayBounds :: Strategy i -> Strategy (Array i a)
#else
seqArrayBounds :: Ix i => Strategy i -> Strategy (Array i a)
#endif
seqArrayBounds strat = seqTuple2 strat strat . Data.Array.bounds

-- | Evaluate the keys and values of a map according to the given strategies.
seqMap :: Strategy k -> Strategy v -> Strategy (Map k v)
seqMap stratK stratV = seqList (seqTuple2 stratK stratV) . Data.Map.toList


-- --------------------------------------------------------------------------
-- Sequential strategies for tuples

seqTuple2 :: Strategy a -> Strategy b -> Strategy (a,b)
seqTuple2 strat1 strat2 (x1,x2) =
  strat1 x1 `seq` strat2 x2

seqTuple3 :: Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)
seqTuple3 strat1 strat2 strat3 (x1,x2,x3) =
  strat1 x1 `seq` strat2 x2 `seq` strat3 x3

seqTuple4 :: Strategy a -> Strategy b -> Strategy c -> Strategy d -> Strategy (a,b,c,d)
seqTuple4 strat1 strat2 strat3 strat4 (x1,x2,x3,x4) =
  strat1 x1 `seq` strat2 x2 `seq` strat3 x3 `seq` strat4 x4

seqTuple5 :: Strategy a -> Strategy b -> Strategy c -> Strategy d -> Strategy e -> Strategy (a,b,c,d,e)
seqTuple5 strat1 strat2 strat3 strat4 strat5 (x1,x2,x3,x4,x5) =
  strat1 x1 `seq` strat2 x2 `seq` strat3 x3 `seq` strat4 x4 `seq` strat5 x5

seqTuple6 :: Strategy a -> Strategy b -> Strategy c -> Strategy d -> Strategy e -> Strategy f -> Strategy (a,b,c,d,e,f)
seqTuple6 strat1 strat2 strat3 strat4 strat5 strat6 (x1,x2,x3,x4,x5,x6) =
  strat1 x1 `seq` strat2 x2 `seq` strat3 x3 `seq` strat4 x4 `seq` strat5 x5 `seq` strat6 x6

seqTuple7 :: Strategy a -> Strategy b -> Strategy c -> Strategy d -> Strategy e -> Strategy f -> Strategy g -> Strategy (a,b,c,d,e,f,g)
seqTuple7 strat1 strat2 strat3 strat4 strat5 strat6 strat7 (x1,x2,x3,x4,x5,x6,x7) =
  strat1 x1 `seq` strat2 x2 `seq` strat3 x3 `seq` strat4 x4 `seq` strat5 x5 `seq` strat6 x6 `seq` strat7 x7

seqTuple8 :: Strategy a -> Strategy b -> Strategy c -> Strategy d -> Strategy e -> Strategy f -> Strategy g -> Strategy h -> Strategy (a,b,c,d,e,f,g,h)
seqTuple8 strat1 strat2 strat3 strat4 strat5 strat6 strat7 strat8 (x1,x2,x3,x4,x5,x6,x7,x8) =
  strat1 x1 `seq` strat2 x2 `seq` strat3 x3 `seq` strat4 x4 `seq` strat5 x5 `seq` strat6 x6 `seq` strat7 x7 `seq` strat8 x8

seqTuple9 :: Strategy a -> Strategy b -> Strategy c -> Strategy d -> Strategy e -> Strategy f -> Strategy g -> Strategy h -> Strategy i -> Strategy (a,b,c,d,e,f,g,h,i)
seqTuple9 strat1 strat2 strat3 strat4 strat5 strat6 strat7 strat8 strat9 (x1,x2,x3,x4,x5,x6,x7,x8,x9) =
  strat1 x1 `seq` strat2 x2 `seq` strat3 x3 `seq` strat4 x4 `seq` strat5 x5 `seq` strat6 x6 `seq` strat7 x7 `seq` strat8 x8 `seq` strat9 x9
