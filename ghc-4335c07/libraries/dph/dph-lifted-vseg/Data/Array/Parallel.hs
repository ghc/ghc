{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}

-- | User level interface to vectorised parallel arrays.
--
--  /WARNING:/ In the current implementation, the functionality provided in
--  this module is tied to the vectoriser pass of GHC, invoked by `-fvectorise`.
--  These functions will not work at all in unvectorised code. To operate on
--  parallel arrays in unvectorised code, use the functions in
--  "Data.Array.Parallel.PArray" and convert between array representations by
--  using `fromPArrayP` and `toPArrayP` from /vectorised/ code.
---
--  The semantic difference between standard Haskell arrays (aka "lazy
--  arrays") and parallel arrays (aka "strict arrays") is that the evaluation
--  of two different elements of a lazy array is independent, whereas in a
--  strict array either non or all elements are evaluated.
--  In other words, when a parallel array is evaluated to WHNF, all its elements
--  will be evaluated to WHNF. The name parallel array indicates that all array
--  elements may, in general, be evaluated to WHNF in parallel without any
--  need to resort to speculative evaluation.  This parallel evaluation
--  semantics is also beneficial in the sequential case, as it facilitates
--  loop-based array processing as known from classic array-based languages,
--  such as Fortran.
--
--  The interface of this module is essentially a variant of the list
--  component of the Prelude, but also includes some functions (such as
--  permutations) that are not provided for lists.  The following list of
--  operations are not supported on parallel arrays, as they would require the
--  infinite parallel arrays: `iterate', `repeat', and `cycle'.
--
--  UGLY HACK ALERT: 
--   Same ugly hack as in 'base:GHC.PArr'!  We could do without in this module by
--   using the type synonym 'PArr' instead of '[::]', but that would lead to
--   significantly worse error message for end users.
--
module Data.Array.Parallel 
        ( module Data.Array.Parallel.Prelude

        -- * Conversions
        , PArray
        , fromPArrayP
        , toPArrayP
        , fromNestedPArrayP
        
        -- * Constructors
        , emptyP
        , singletonP
        , replicateP
        , appendP, (+:+)
        , concatP
        
        -- * Projections
        , lengthP
        , indexP,  (!:)
        , sliceP
        
        -- * Traversals
        , mapP
        , zipWithP
        , crossMapP

        -- * Filtering
        , filterP
        
        -- * Ziping and Unzipping
        , zipP
        , unzipP)
where
-- Primitives needed by the vectoriser.
import Data.Array.Parallel.Prim                 ()      

import Data.Array.Parallel.PArr
import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Lifted
import Data.Array.Parallel.PArray.PData.Base    (PArray(..))


-------------------------------------------------------------------------------
-- IMPORTANT:
--  We only define the signatures of operations on parallel arrays, and give
--  and bodies that convince GHC that these functions don't just diverge.
--  The vectoriser rewrites them to entirely the code given in the VECTORISE
--  pragmas.
--
--  The functions must be eta-expanded, so the right of the binding is
--  something of the final return type. The vectoriser takes the type of the
--  body to determine what PA dictionary to pass.
--
--  We also put bangs (!) on the arguments to indicate to the GHC strictness
--  analyser that these paramters will really be used in the vectorised code.
--
--     This won't work:   mapP       = undefined
--     You need this:     mapP !_ !_ = [::]
--
--  The bindings have NOINLINE pragmas because we never want to use the
--  actual body code (because it's fake anyway).
--

-- Conversions ----------------------------------------------------------------
-- | O(1). Convert between `PArray` and [::] array representations.
fromPArrayP :: PArray a -> [:a:]
fromPArrayP !_  = emptyP
{-# NOINLINE  fromPArrayP #-}
{-# VECTORISE fromPArrayP = fromPArrayPP #-}


-- | O(1). Convert between `PArray` and [::] array representations.
toPArrayP :: [:a:] -> PArray a
toPArrayP !_    = PArray 0# (error "toPArrayP: unvectorised")
{-# NOINLINE  toPArrayP #-}
{-# VECTORISE toPArrayP = toPArrayPP #-}


-- | O(1). Convert between `PArray` and [::] array representations.
fromNestedPArrayP :: PArray (PArray a) -> [:[:a:]:]
fromNestedPArrayP !_ = emptyP
{-# NOINLINE  fromNestedPArrayP #-}
{-# VECTORISE fromNestedPArrayP = fromNestedPArrayPP #-}


-- Constructors ---------------------------------------------------------------
-- | Construct an empty array, with no elements.
emptyP :: [:a:]
emptyP          = emptyPArr
{-# NOINLINE  emptyP #-}
{-# VECTORISE emptyP = emptyPP #-}


-- | Construct an array with a single element.
singletonP :: a -> [:a:]
singletonP      = singletonPArr
{-# NOINLINE  singletonP #-}
{-# VECTORISE singletonP = singletonPP #-}


-- | Construct an array by replicating the given element some number of times.
replicateP :: Int -> a -> [:a:]
replicateP      = replicatePArr
{-# NOINLINE  replicateP #-}
{-# VECTORISE replicateP = replicatePP #-}


-- | Append two arrays.
appendP, (+:+) :: [:a:] -> [:a:] -> [:a:]
(+:+) !_ !_     = emptyP
{-# NOINLINE  (+:+) #-}
{-# VECTORISE (+:+)     = appendPP #-}

appendP !_ !_   = emptyP
{-# NOINLINE  appendP #-}
{-# VECTORISE appendP   = appendPP #-}


-- | Concatenate an array of arrays.
concatP :: [:[:a:]:] -> [:a:]
concatP !_      = emptyP
{-# NOINLINE  concatP #-}
{-# VECTORISE concatP = concatPP #-}


-- Projections ----------------------------------------------------------------
-- | Take the length of an array.
lengthP :: [:a:] -> Int
lengthP = lengthPArr
{-# NOINLINE  lengthP #-}
{-# VECTORISE lengthP   = lengthPP #-}

-- | Lookup a single element from the source array.
indexP, (!:) :: [:a:] -> Int -> a
(!:)            = indexPArr
{-# NOINLINE  (!:) #-}
{-# VECTORISE (!:)      = indexPP #-}

indexP            = indexPArr
{-# NOINLINE  indexP #-}
{-# VECTORISE indexP    = indexPP #-}


-- | Extract a slice from an array.
sliceP :: Int -> Int -> [:a:] -> [:a:]
sliceP !_ !_ !_ = emptyP
{-# NOINLINE sliceP #-}
{-# VECTORISE sliceP    = slicePP #-}


-- Traversals -----------------------------------------------------------------
-- | Apply a worker function to every element of an array.
mapP :: (a -> b) -> [:a:] -> [:b:]
mapP !_ !_              = emptyP
{-# NOINLINE  mapP #-}
{-# VECTORISE mapP      = mapPP #-}

-- | Apply a worker function to every pair of two arrays.
zipWithP :: (a -> b -> c) -> [:a:] -> [:b:] -> [:c:]
zipWithP !_ !_ !_       = emptyP
{-# NOINLINE  zipWithP #-}
{-# VECTORISE zipWithP  = zipWithPP #-}

-- | For every element 'a' apply the function to get an array of 'b' then,
--   and return an array of all the 'a's and 'b's.
crossMapP :: [:a:] -> (a -> [:b:]) -> [:(a, b):]
{-# NOINLINE crossMapP #-}
crossMapP !_ !_ = emptyP
{-# VECTORISE crossMapP = crossMapPP #-}


-- Filtering -----------------------------------------------------------------
-- | Filter an array, keeping only those elements that match the given predicate.
filterP :: (a -> Bool) -> [:a:] -> [:a:]
filterP !_ !_   = emptyP
{-# NOINLINE  filterP #-}
{-# VECTORISE filterP = filterPP #-}


-- Zipping and Unzipping ------------------------------------------------------
-- | Zip a pair of arrays into an array of pairs.
zipP :: [:a:] -> [:b:] -> [:(a, b):]
zipP !_ !_      = emptyP
{-# NOINLINE  zipP #-}
{-# VECTORISE zipP      = zipPP #-}


-- | Unzip an array of pairs into a pair of arrays.
unzipP :: [:(a, b):] -> ([:a:], [:b:])
unzipP !_       = (emptyP, emptyP)
{-# NOINLINE  unzipP #-}
{-# VECTORISE unzipP    = unzipPP #-}
