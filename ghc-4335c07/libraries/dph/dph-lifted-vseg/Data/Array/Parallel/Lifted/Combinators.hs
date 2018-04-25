{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-spec-constr #-}
#include "fusion-phases.h"

--   NOTE NOTE NOTE
--   This file is IDENTICAL to the one in dph-lifted-boxed.
--   If you update one then update the other as well.

-- | Closure converted lifted array combinators.
--   The vectoriser produces code that uses these combinators directly.
-- 
--   All of the combinators in this module are polymorphic, work on `PArray`, and
--   take `PA` dictionaries. Combinators that are specific to a certain element type,
--   like `Int`, are defined in the corresponding prelude module, 
--   eg "Data.Array.Parallel.Prelude.Int".
--
module Data.Array.Parallel.Lifted.Combinators 
        ( -- * Conversions
          fromPArrayPP
        , toPArrayPP
        , fromNestedPArrayPP
        
        -- * Constructors
        , emptyPP
        , singletonPP
        , replicatePP
        , appendPP

        -- * Projections
        , lengthPP
        , indexPP
        , slicePP

        -- * Traversals
        , mapPP
        , zipWithPP
        , crossMapPP

        -- * Filtering
        , filterPP

        -- * Concatenation
        , concatPP

        -- * Tuple functions
        , zipPP
        , unzipPP)
where
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.PArray.PData         as PA
import Data.Array.Parallel.PArray.PRepr         as PA
import Data.Array.Parallel.PArray               as PA


-- Conversions ================================================================
-- The following identity functions are used as the vectorised versions of the
-- functions that convert between the source level array type [:a:] and the 
-- PArray type which is used in the library. 

-- | Identity function, used as the vectorised version of fromPArrayP.
fromPArrayPP :: PA a => PArray a :-> PArray a
fromPArrayPP         = closure1 (\x -> x) (\_ xs -> xs)
{-# INLINE fromPArrayPP #-}


-- | Identity function, used as the vectorised version of toPArrayP.
toPArrayPP :: PA a => PArray a :-> PArray a
toPArrayPP         = closure1 (\x -> x) (\_ xs -> xs)
{-# INLINE toPArrayPP #-}


-- | Identity function, used as the vectorised version of fromNestedPArrayP
fromNestedPArrayPP :: PA a => (PArray (PArray a) :-> PArray (PArray a))
fromNestedPArrayPP = closure1 (\xs -> xs) (\_ xss -> xss)
{-# INLINE fromNestedPArrayPP #-}


-- Combinators ================================================================
--   For each combinator:
--    The *PP_v version is the "vectorised" version that has had its parameters
--    closure converted. For first-order functions, the *PP_v version is
--    identical to the standard *PA version from D.A.P.PArray, so we can 
--    just use that directly.
--
--    The *PP_l version is the "lifted" version that works on arrays of arrays.
--    Each of these functions also takes an integer as its first argument. 
--    This is the "lifting context" that says now many element to expect in 
--    each of the argument arrays. 
--
--    The *PP version contains both the vectorised and lifted versions wrapped
--    up in a closure. The code produced by the vectoriser uses the *PP
--    versions directly.


-- Constructors ---------------------------------------------------------------
-- | O(1). Construct an empty array.
emptyPP :: PA a => PArray a
emptyPP         = PA.empty
{-# INLINE_PA emptyPP #-}


-- | O(1). Construct an array containing a single element.
singletonPP :: PA a => a :-> PArray a
singletonPP     = closure1' PA.singleton PA.singletonl
{-# INLINE_PA singletonPP #-}


-- | O(n). Construct an array of the given size, that maps all elements to the same value.
replicatePP     :: PA a => Int :-> a :-> PArray a
replicatePP     = closure2' PA.replicate PA.replicatel
{-# INLINE_PA replicatePP #-}


-- | O(len result). Append two arrays.
appendPP :: PA a => PArray a :-> PArray a :-> PArray a
appendPP        = closure2' PA.append PA.appendl
{-# INLINE_PA appendPP #-}


-- | O(len result). Concatenate a nested array.
concatPP :: PA a => PArray (PArray a) :-> PArray a
concatPP        = closure1' PA.concat PA.concatl
{-# INLINE_PA concatPP #-}


-- Projections ----------------------------------------------------------------
-- | O(1). Take the number of elements in an array.
lengthPP   :: PA a => PArray a :-> Int
lengthPP        = closure1' PA.length PA.lengthl
{-# INLINE_PA lengthPP #-}


-- | O(1). Lookup a single element from the source array.
indexPP :: PA a => PArray a :-> Int :-> a
indexPP         = closure2' PA.index PA.indexl
{-# INLINE_PA indexPP #-}


-- | O(len slice). Extract a range of elements from an array.
slicePP :: PA a => Int :-> Int :-> PArray a :-> PArray a
slicePP         = closure3' PA.slice PA.slicel
{-# INLINE_PA slicePP #-}


-- Traversals -----------------------------------------------------------------
-- | Apply a worker function to every element of an array.
mapPP   :: (PA a, PA b) 
        => (a :-> b) :-> PArray a :-> PArray b

mapPP   = closure2' mapPP_v mapPP_l
{-# INLINE_PA mapPP #-}


mapPP_v :: (PA a, PA b)
        => (a :-> b) -> PArray a -> PArray b
mapPP_v f as
        =   PA.replicate (PA.length as) f $:^ as
{-# INLINE mapPP_v #-}


mapPP_l :: (PA a, PA b)
        => (PArray (a :-> b)) -> PArray (PArray a) -> PArray (PArray b)
mapPP_l fs ass
        =   PA.unconcat ass 
        $   PA.replicates (PA.takeUSegd ass) fs
        $:^ PA.concat ass
{-# INLINE mapPP_l #-}


-- | Apply a worker function to every pair of two arrays.
zipWithPP 
        :: (PA a, PA b, PA c)
        => (a :-> b :-> c) :-> PArray a :-> PArray b :-> PArray c

zipWithPP = closure3' zipWithPP_v zipWithPP_l
 where
        {-# INLINE zipWithPP_v #-}
        zipWithPP_v f as bs
                = PA.replicate (PA.length as) f $:^ as $:^ bs

        {-# INLINE zipWithPP_l #-}
        zipWithPP_l fs ass bss
                =   PA.unconcat ass
                $   PA.replicates (PA.takeUSegd ass) fs
                $:^ PA.concat ass
                $:^ PA.concat bss
{-# INLINE_PA zipWithPP #-}


-- | 
crossMapPP
        :: (PA a, PA b)
        => PArray a :-> (a :-> PArray b) :-> PArray (a, b)

crossMapPP = closure2' crossMapPP_v crossMapPP_l
 where
        {-# INLINE crossMapPP_v #-}
        crossMapPP_v as f
                = let bss = mapPP_v f as
                  in  PA.zip (PA.replicates (PA.takeUSegd bss) as) (PA.concat bss)

        {-# INLINE crossMapPP_l #-}
        crossMapPP_l ass fs
                = let bsss = mapPP_l fs ass
                      bss  = PA.concat bsss
                      as'  = PA.replicates (PA.takeUSegd bss) (PA.concat ass)
                  in  PA.unconcat bss (PA.zip as' (PA.concat bss))

{-# INLINE_PA crossMapPP #-}


-- Filtering ------------------------------------------------------------------
-- | Extract the elements from an array that match the given predicate.
filterPP :: PA a => (a :-> Bool) :-> PArray a :-> PArray a
{-# INLINE filterPP #-}
filterPP = closure2' filterPP_v filterPP_l
 where
        {-# INLINE filterPP_v #-}
        filterPP_v p xs    = PA.pack xs   (mapPP_v p xs)
        
        {-# INLINE filterPP_l #-}
        filterPP_l ps xss  = PA.packl xss (mapPP_l ps xss)


-- Tuple Functions ------------------------------------------------------------
-- | Zip a pair of arrays into an array of pairs.
zipPP :: (PA a, PA b) => PArray a :-> PArray b :-> PArray (a, b)
zipPP           = closure2' PA.zip PA.zipl
{-# INLINE_PA zipPP #-}


-- | Unzip an array of pairs into a pair of arrays.
unzipPP :: (PA a, PA b) => PArray (a, b) :-> (PArray a, PArray b)
unzipPP         = closure1' PA.unzip PA.unzipl
{-# INLINE_PA unzipPP #-}

