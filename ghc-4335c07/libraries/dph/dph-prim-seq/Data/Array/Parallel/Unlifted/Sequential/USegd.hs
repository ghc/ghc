{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-orphans #-}
#include "fusion-phases.h"

-- | Segment Descriptors.
--
--   See "Data.Array.Parallel.Unlifted" for how this works.
--
module Data.Array.Parallel.Unlifted.Sequential.USegd 
        ( -- * Types
          USegd(..)

          -- * Constructors
        , mkUSegd, valid
        , empty, singleton
        , fromLengths

          -- * Projections
        , length
        , takeLengths, takeIndices, takeElements
        , getSeg

          -- * Operations
        , append, slice, extract)
where
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as U
import Data.Array.Parallel.Unlifted.Sequential.Vector           (Vector)
import Data.Array.Parallel.Pretty                               hiding (empty)
import Prelude                                                  hiding (length)
import Debug.Trace

here :: String -> String 
here s = "Data.Array.Parallel.Unlifted.Sequential.USegd." ++ s


-- | Segment descriptor. 
data USegd 
        = USegd 
        { usegd_lengths  :: !(Vector Int)  -- ^ Length of each segment.
        , usegd_indices  :: !(Vector Int)  -- ^ Starting index of each segment.
        , usegd_elements :: !Int           -- ^ Total number of elements in the flat array.
        } deriving (Show, Eq)


instance PprPhysical USegd where
 pprp (USegd lengths indices elements)
  =   text "USegd" 
  $$  (nest 7 $ vcat
        [ text "lengths: " <+> (text $ show $ U.toList lengths)
        , text "indices: " <+> (text $ show $ U.toList indices)
        , text "elements:" <+> (text $ show elements)])


-- Constructors ---------------------------------------------------------------
-- | O(1). Construct a new segment descriptor.
mkUSegd 
        :: Vector Int   -- ^ Length of each segment.
        -> Vector Int   -- ^ Starting index of each segment.
        -> Int          -- ^ Total number of elements in the flat array.
        -> USegd

mkUSegd = USegd
{-# INLINE_U mkUSegd #-}


-- | O(1). Check the internal consistency of a segment descriptor.
--
--   As the indices and elemens field can be generated based on the segment
--   lengths, we check the consistency by rebuilding these fields and 
--   comparing the rebuilt ones against the originals.
valid :: USegd -> Bool
valid usegd@(USegd lengths _ _)
        = usegd == fromLengths lengths
{-# NOINLINE valid #-}
--  NOINLINE because it's only enabled during debugging anyway.


-- | O(1). Construct an empty segment descriptor, with no elements or segments.
empty :: USegd
empty   = USegd U.empty U.empty 0
{-# INLINE_U empty #-}


-- | O(1). Construct a singleton segment descriptor.
--   The single segment covers the given number of elements.
singleton :: Int -> USegd
singleton n
        = USegd (U.singleton n) (U.singleton 0) n
{-# INLINE_U singleton #-}


-- | O(segs). Convert an array of segment lengths into a segment descriptor.
-- 
--   The array contains the length of each segment, and we compute the 
--   indices from that.
fromLengths :: Vector Int -> USegd
fromLengths lens
        = USegd lens (U.scanl (+) 0 lens) (U.sum lens)
{-# INLINE_U fromLengths #-}


-- Projections ----------------------------------------------------------------
-- INLINE trivial projections as they'll expand to a single record selector.

-- | O(1). Yield the overall number of segments.
length :: USegd -> Int
length          = U.length . usegd_lengths
{-# INLINE length #-}


-- | O(1). Yield the lengths of the individual segments.
takeLengths :: USegd -> Vector Int
takeLengths     = usegd_lengths
{-# INLINE takeLengths #-}


-- | O(1). Yield the segment indices of a segment descriptor.
takeIndices :: USegd -> Vector Int
takeIndices     = usegd_indices
{-# INLINE takeIndices #-}


-- | O(1). Yield the number of data elements.
takeElements :: USegd -> Int
takeElements    = usegd_elements
{-# INLINE takeElements #-}


-- | O(1). Get the length and segment index of a segment
getSeg :: USegd -> Int -> (Int, Int)
getSeg (USegd lengths indices _ ) ix
 =      ( U.index (here "getSeg") lengths ix
        , U.index (here "getSeg") indices ix)
{-# INLINE_U getSeg #-}


-- Operators ------------------------------------------------------------------
-- | O(segs). Produce a segment descriptor that describes the result of appending 
--   two arrays.
append :: USegd -> USegd -> USegd
append (USegd lengths1 indices1 elems1)
       (USegd lengths2 indices2 elems2)
 = traceEvent ("dph-prim-seq: USegd.append")
 $ USegd (lengths1 U.++ lengths2)
         (indices1 U.++ U.map (+ elems1) indices2)
         (elems1 + elems2)
{-# NOINLINE append #-}


-- | O(segs) Extract a slice of a segment descriptor, avoiding copying where possible.
--
--   We can share the segment lengths with the original segment descriptor, 
--   but still need to recompute the starting indices of each. Hence
--   runtime is O(segs) in the number of segments sliced out.

--   NOTE: In the new segment descriptor, the starting index of the first
--         segment will be 0.
slice
        :: USegd        -- ^ Source segment descriptor.
        -> Int          -- ^ Index of first segment.
        -> Int          -- ^ Number of segments to slice out.
        -> USegd
slice segd i n
        = fromLengths $ U.unsafeSlice (takeLengths segd) i n
{-# INLINE_U slice #-}


-- | Extract a slice of a segment descriptor, copying everything.
--
--   In contrast to `slice`, this function copies the array of 
--   segment lengths as well as recomputing the starting indices of each.

--   NOTE: In the new segment descriptor, the starting index of the first
--         segment will be 0.
extract
        :: USegd        -- ^ Source segment desciptor.
        -> Int          -- ^ Undex of the first segment.
        -> Int          -- ^ Number of segments to extract out.
        -> USegd
extract segd i n 
        = fromLengths $ U.extract (takeLengths segd) i n
{-# INLINE_U extract #-}

