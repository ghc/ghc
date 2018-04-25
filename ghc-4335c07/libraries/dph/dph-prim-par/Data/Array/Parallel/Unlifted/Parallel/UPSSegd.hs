{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Parallel Scattered Segment descriptors.
--
--   See "Data.Array.Parallel.Unlifted" for how this works.
--
module Data.Array.Parallel.Unlifted.Parallel.UPSSegd 
        ( -- * Types
          UPSSegd, valid

          -- * Constructors
        , mkUPSSegd, fromUSSegd, fromUPSegd
        , empty, singleton
  
          -- * Predicates
        , isContiguous

          -- * Projections
        , length
        , takeUSSegd
        , takeDistributed
        , takeLengths
        , takeIndices
        , takeElements
        , takeStarts
        , takeSources
        , getSeg
  
          -- * Append
        , appendWith  

          -- * Segmented Folds
        , foldWithP
        , fold1WithP
        , sumWithP
        , foldSegsWithP)
where
import Data.Array.Parallel.Pretty                                 hiding (empty)
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Distributed.What
import Data.Array.Parallel.Unlifted.Parallel.UPSegd               (UPSegd)
import Data.Array.Parallel.Unlifted.Sequential.USSegd             (USSegd)
import Data.Array.Parallel.Unlifted.Sequential.Vector             (Vector,  MVector, Unbox)
import Data.Array.Parallel.Unlifted.Vectors                       (Vectors, Unboxes)
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSegd     as UPSegd
import qualified Data.Array.Parallel.Unlifted.Distributed.Data.USSegd  as DUSSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd   as USSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector   as US
import qualified Data.Array.Parallel.Unlifted.Sequential          as Seq
import Control.Monad.ST
import Prelude hiding (length)

here :: String -> String
here s = "Data.Array.Parallel.Unlifted.Parallel.UPSSegd." ++ s


-- | Parallel Scattered Segment sescriptor
data UPSSegd 
        = UPSSegd 
        { upssegd_ussegd        :: !USSegd
          -- ^ Segment descriptor that describes the whole array.

        , upssegd_dssegd        :: Dist ((USSegd,Int),Int)
          -- ^ Segment descriptor for each chunk, 
          --   along with segment id of first slice in the chunk,
          --   and the offset of that slice in its segment.
          --   See docs of `splitSegdOfElemsD` for an example.
        }
        deriving Show


instance PprPhysical UPSSegd where
 pprp (UPSSegd ussegd dssegd)
  =  text "UPSSegd"
  $$ (nest 7 $ vcat
        [ text "ussegd:  " <+> pprp ussegd
        , text "dssegd:  " <+> pprp dssegd])


-- | O(1).
--   Check the internal consistency of a scattered segment descriptor.
--- 
--   * TODO: this doesn't do any checks yet
valid :: UPSSegd -> Bool
valid _ = True
{-# NOINLINE valid #-}
--  NOINLINE because it's only used during debugging anyway.


-- Constructors ---------------------------------------------------------------
-- | Construct a new segment descriptor.
mkUPSSegd 
        :: Vector Int   -- ^ Starting index of each segment in its flat array.
        -> Vector Int   -- ^ Source id of the flat array to tach each segment from.
        -> UPSegd       -- ^ Contiguous (unscattered) segment descriptor.
        -> UPSSegd

mkUPSSegd starts sources upsegd
        = fromUSSegd (USSegd.mkUSSegd starts sources (UPSegd.takeUSegd upsegd))
{-# INLINE_UP mkUPSSegd #-}


-- | Promote a global `USSegd` to a parallel `UPSSegd` by distributing
--   it across the gang.
fromUSSegd :: USSegd -> UPSSegd
fromUSSegd ssegd 
        = UPSSegd ssegd (DUSSegd.splitSSegdOnElemsD theGang ssegd)
{-# INLINE_UP fromUSSegd #-}


-- | Promote a plain `UPSegd` to a `UPSSegd`, by assuming that all segments
--   come from a single flat array with source id 0.
---
--   * TODO:
--     This sequentially constructs the indices and source fields, and we
--     throw out the existing distributed `USegd`. We could probably keep
--     some of the existing fields and save reconstructing them.
--
fromUPSegd :: UPSegd -> UPSSegd
fromUPSegd upsegd
        = fromUSSegd $ USSegd.fromUSegd $ UPSegd.takeUSegd upsegd
{-# INLINE_UP fromUPSegd #-}


-- | O(1). Yield an empty segment descriptor, with no elements or segments.
empty :: UPSSegd
empty   = fromUSSegd USSegd.empty
{-# INLINE_UP empty #-}


-- | O(1).
--   Yield a singleton segment descriptor.
--   The single segment covers the given number of elements.
singleton :: Int -> UPSSegd
singleton n = fromUSSegd $ USSegd.singleton n
{-# INLINE_UP singleton #-}


-- Predicates -----------------------------------------------------------------
-- INLINE trivial predicates as they'll expand to a simple calls.

-- | O(1). True when the starts are identical to the usegd indices field and
--   the sources are all 0's. 
--
--   In this case all the data elements are in one contiguous flat
--   array, and consumers can avoid looking at the real starts and
--   sources fields.
--
isContiguous :: UPSSegd -> Bool
isContiguous    = USSegd.isContiguous . upssegd_ussegd
{-# INLINE isContiguous #-}


-- Projections ----------------------------------------------------------------
-- INLINE trivial projections as they'll expand to a single record selector.

-- | O(1). Yield the overall number of segments.
length :: UPSSegd -> Int
length          = USSegd.length . upssegd_ussegd
{-# INLINE length #-}

-- | O(1). Yield the global `USegd` of a `UPSegd`
takeUSSegd :: UPSSegd -> USSegd
takeUSSegd      = upssegd_ussegd
{-# INLINE takeUSSegd #-}


-- | O(1). Yield the distributed `USegd` of a `UPSegd`
takeDistributed :: UPSSegd -> Dist ((USSegd, Int), Int)
takeDistributed = upssegd_dssegd
{-# INLINE takeDistributed #-}


-- | O(1). Yield the lengths of the individual segments.
takeLengths :: UPSSegd -> Vector Int
takeLengths     = USSegd.takeLengths . upssegd_ussegd
{-# INLINE takeLengths #-}


-- | O(1). Yield the segment indices.
takeIndices :: UPSSegd -> Vector Int
takeIndices     = USSegd.takeIndices . upssegd_ussegd
{-# INLINE takeIndices #-}


-- | O(1). Yield the total number of data elements.
--
--  @takeElements upssegd = sum (takeLengths upssegd)@
--
takeElements :: UPSSegd -> Int
takeElements    = USSegd.takeElements . upssegd_ussegd
{-# INLINE takeElements #-}


-- | O(1). Yield the starting indices.
takeStarts :: UPSSegd -> Vector Int
takeStarts      = USSegd.takeStarts . upssegd_ussegd
{-# INLINE takeStarts #-}


-- | O(1). Yield the source ids.
takeSources :: UPSSegd -> Vector Int
takeSources     = USSegd.takeSources . upssegd_ussegd 
{-# INLINE takeSources #-}


-- | O(1). Get the length, segment index, starting index, and source id of a segment.
getSeg :: UPSSegd -> Int -> (Int, Int, Int, Int)
getSeg upssegd ix
        = USSegd.getSeg (upssegd_ussegd upssegd) ix
{-# INLINE_UP getSeg #-}


-- Append ---------------------------------------------------------------------
-- | O(n)
--   Produce a segment descriptor that describes the result of appending two
--   segmented arrays.
--
--   Appending two nested arrays is an index space transformation. Because
--   a `UPSSegd` can contain segments from multiple flat data arrays, we can
--   represent the result of the append without copying elements from the
--   underlying flat data arrays.
---
--   * TODO: This calls out to the sequential version.
--
appendWith
        :: UPSSegd              -- ^ Segment descriptor of first nested array.
        -> Int                  -- ^ Number of flat data arrays used to represent first nested array.
        -> UPSSegd              -- ^ Segment descriptor of second nested array. 
        -> Int                  -- ^ Number of flat data arrays used to represent second nested array.
        -> UPSSegd
appendWith upssegd1 pdatas1
           upssegd2 pdatas2
 = fromUSSegd 
 $ USSegd.appendWith
        (upssegd_ussegd upssegd1) pdatas1
        (upssegd_ussegd upssegd2) pdatas2
{-# NOINLINE appendWith #-}
--  NOINLINE because we're not using it yet.


-- Fold -----------------------------------------------------------------------
-- | Fold segments specified by a `UPSSegd`.
foldWithP :: (Unbox a, Unboxes a)
         => (a -> a -> a) -> a -> UPSSegd -> Vectors a -> Vector a
foldWithP f !z  = foldSegsWithP f (Seq.foldlSSU f z)
{-# INLINE_UP foldWithP #-}


-- | Fold segments specified by a `UPSSegd`, with a non-empty vector.
fold1WithP :: (Unbox a, Unboxes a)
           => (a -> a -> a) -> UPSSegd -> Vectors a -> Vector a
fold1WithP f    = foldSegsWithP f (Seq.fold1SSU f)
{-# INLINE_UP fold1WithP #-}


-- | Sum up segments specified by a `UPSSegd`.
sumWithP :: (Num a, Unbox a, Unboxes a)
        => UPSSegd -> Vectors a -> Vector a
sumWithP = foldWithP (+) 0
{-# INLINE_UP sumWithP #-}


-- | Fold the segments specified by a `UPSSegd`.
--
--   Low level function takes a per-element worker and a per-segment worker.
--   It folds all the segments with the per-segment worker, then uses the
--   per-element worker to fixup the partial results when a segment 
--   is split across multiple threads.
--   
foldSegsWithP
        :: (Unbox a, Unboxes a)
        => (a -> a -> a)
        -> (USSegd -> Vectors a -> Vector a)
        -> UPSSegd -> Vectors a -> Vector a

foldSegsWithP fElem fSeg segd xss 
 = dcarry `seq` drs `seq` 
   runST (do
        mrs <- joinDM theGang drs
        fixupFold fElem mrs dcarry
        US.unsafeFreeze mrs)

 where  (dcarry,drs)
          = unzipD
          $ mapD (What "UPSSegd.foldSegsWithP/partial") theGang 
                partial (takeDistributed segd)

        partial ((ssegd, k), off)
         = let rs = fSeg ssegd xss
               {-# INLINE [0] n #-}
               n | off == 0  = 0
                 | otherwise = 1

           in  ((k, US.take n rs), US.drop n rs)
{-# INLINE_UP foldSegsWithP #-}


fixupFold
        :: Unbox a
        => (a -> a -> a)
        -> MVector s a
        -> Dist (Int,Vector a)
        -> ST s ()

fixupFold f !mrs !dcarry = go 1
  where
    !p = gangSize theGang

    go i | i >= p    = return ()
         | US.null c = go (i+1)
         | otherwise   
         = do   x <- US.read mrs k
                US.write mrs k (f x (US.index (here "fixupFold") c 0))
                go (i + 1)
      where
        (k,c) = indexD (here "fixupFold") dcarry i
{-# NOINLINE fixupFold #-}

