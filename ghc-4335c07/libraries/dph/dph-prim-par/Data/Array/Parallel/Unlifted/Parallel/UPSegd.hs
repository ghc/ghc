{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Parallel segment descriptors.
--
--   See "Data.Array.Parallel.Unlifted" for how this works.
--
module Data.Array.Parallel.Unlifted.Parallel.UPSegd 
        ( -- * Types
          UPSegd(..)
        , valid

          -- * Constructors
        , mkUPSegd, fromUSegd
        , empty, singleton, fromLengths

          -- * Projections
        , length
        , takeUSegd
        , takeDistributed
        , takeLengths
        , takeIndices
        , takeElements

          -- * Indices
        , indicesP
  
          -- * Replicate
        , replicateWithP
    
          -- * Segmented Folds
        , foldWithP
        , fold1WithP
        , sumWithP
        , foldSegsWithP)
where
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Distributed.What
import Data.Array.Parallel.Unlifted.Sequential.USegd                    (USegd)
import qualified Data.Array.Parallel.Unlifted.Distributed.Data.USegd  as USegd
import qualified Data.Array.Parallel.Unlifted.Sequential         as Seq
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector  as US
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd   as USegd
import Data.Array.Parallel.Pretty                                hiding (empty)
import Data.Array.Parallel.Unlifted.Sequential.Vector  (Vector, MVector, Unbox)
import Control.Monad.ST
import Prelude  hiding (length)

here :: String -> String
here s = "Data.Array.Parallel.Unlifted.Parallel.UPSegd." ++ s


-- | A parallel segment descriptor holds a global (undistributed) segment
--   desciptor, as well as a distributed version. The distributed version
--   describes how to split work on the segmented array over the gang. 
data UPSegd 
        = UPSegd 
        { upsegd_usegd :: !USegd
          -- ^ Segment descriptor that describes the whole array.

        , upsegd_dsegd :: Dist ((USegd,Int),Int)
          -- ^ Segment descriptor for each chunk, 
          --   along with segment id of first slice in the chunk,
          --   and the offset of that slice in its segment.
          --   See docs of `splitSegdOfElemsD` for an example.
        }


-- Pretty ---------------------------------------------------------------------
instance PprPhysical UPSegd where
 pprp (UPSegd usegd dsegd)
  =  text "UPSegd"
  $$ (nest 7 $ vcat
        [ text "usegd:  "  <+> pprp usegd
        , text "dsegd:  "  <+> pprp dsegd])


-- Valid ----------------------------------------------------------------------
-- | O(1).
--   Check the internal consistency of a parallel segment descriptor.
--- 
--   * TODO: this doesn't do any checks yet
valid :: UPSegd -> Bool
valid _ = True
{-# NOINLINE valid #-}
--  NOINLINE because it's only used during debugging anyway.


-- Constructors ---------------------------------------------------------------
-- | O(1). Construct a new parallel segment descriptor.
mkUPSegd 
        :: Vector Int   -- ^ Length of each segment.
        -> Vector Int   -- ^ Starting index of each segment.
        -> Int          -- ^ Total number of elements in the flat array.
        -> UPSegd

mkUPSegd lens idxs n
        = fromUSegd (USegd.mkUSegd lens idxs n)
{-# INLINE_UP mkUPSegd #-}


-- | Convert a global `USegd` to a parallel `UPSegd` by distributing 
--   it across the gang.
fromUSegd :: USegd -> UPSegd
fromUSegd segd   = UPSegd segd (USegd.splitSegdOnElemsD theGang segd)
{-# INLINE_UP fromUSegd #-}


-- | O(1). Construct an empty segment descriptor, with no elements or segments.
empty :: UPSegd
empty           = fromUSegd USegd.empty
{-# INLINE_UP empty #-}


-- | O(1). Construct a singleton segment descriptor.
--   The single segment covers the given number of elements.
singleton :: Int -> UPSegd
singleton n     = fromUSegd $ USegd.singleton n
{-# INLINE_UP singleton #-}


-- | O(n). Convert an array of segment lengths into a parallel segment descriptor.
-- 
--   The array contains the length of each segment, and we compute the 
--   indices from that. Runtime is O(n) in the number of segments.
--
fromLengths :: Vector Int -> UPSegd
fromLengths     = fromUSegd . USegd.fromLengths
{-# INLINE_UP fromLengths #-}


-- Projections ----------------------------------------------------------------
-- INLINE trivial projections as they'll expand to a single record selector.

-- | O(1). Yield the overall number of segments.
length :: UPSegd -> Int
length          = USegd.length . upsegd_usegd
{-# INLINE length #-}


-- | O(1). Yield the global `USegd` of a `UPSegd`.
takeUSegd :: UPSegd -> USegd
takeUSegd       = upsegd_usegd
{-# INLINE takeUSegd #-}


-- | O(1). Yield the distributed `USegd` of a `UPSegd`.
--   
--  We get a plain `USegd` for each chunk, the segment id of the first
--  slice in the chunk, and the starting offset of that slice in its segment.
-- 
takeDistributed :: UPSegd -> Dist ((USegd,Int),Int)
takeDistributed = upsegd_dsegd
{-# INLINE takeDistributed #-}


-- | O(1). Yield the lengths of the individual segments.
takeLengths :: UPSegd -> Vector Int
takeLengths     = USegd.takeLengths . upsegd_usegd
{-# INLINE takeLengths #-}


-- | O(1). Yield the segment indices.
takeIndices :: UPSegd -> Vector Int
takeIndices     = USegd.takeIndices . upsegd_usegd
{-# INLINE takeIndices #-}


-- | O(1). Yield the total number of array elements.
-- 
--  @takeElements upsegd = sum (takeLengths upsegd)@
--
takeElements :: UPSegd -> Int
takeElements    = USegd.takeElements . upsegd_usegd
{-# INLINE takeElements #-}


-- Indices --------------------------------------------------------------------
-- | O(n). Yield a vector containing indicies that give the position of each 
--         member of the flat array in its corresponding segment.
--
--  @indicesP (fromLengths [5, 2, 3]) = [0,1,2,3,4,0,1,0,1,2]@
--
indicesP :: UPSegd -> Vector Int
indicesP
        = joinD theGang balanced
        . mapD  (What "UPSegd.indicesP/indices") theGang indices
        . takeDistributed
  where
    indices ((segd,_k),off) = Seq.indicesSU' off segd
{-# NOINLINE indicesP #-}
--  NOINLINE because we're not using it yet.


-- Replicate ------------------------------------------------------------------
-- | Copying segmented replication. Each element of the vector is physically 
--   copied according to the length of each segment in the segment descriptor.
--
--   @replicateWith (fromLengths [3, 1, 2]) [5, 6, 7] = [5, 5, 5, 6, 7, 7]@
--
replicateWithP :: Unbox a => UPSegd -> Vector a -> Vector a
replicateWithP segd !xs 
  = joinD theGang balanced
  . mapD  (What "UPSegd.replicateWithP/replicateSU") theGang rep
  $ takeDistributed segd
  where
    rep ((dsegd,di),_)
      = Seq.replicateSU dsegd 
      $ US.slice (here "replicateWithP")
                xs di (USegd.length dsegd)
{-# INLINE_UP replicateWithP #-}


-- Fold -----------------------------------------------------------------------
-- | Fold segments specified by a `UPSegd`.
foldWithP :: Unbox a
         => (a -> a -> a) -> a -> UPSegd -> Vector a -> Vector a
foldWithP f !z  = foldSegsWithP f (Seq.foldlSU f z)
{-# INLINE_UP foldWithP #-}


-- | Fold segments specified by a `UPSegd`, with a non-empty vector.
fold1WithP :: Unbox a
         => (a -> a -> a) -> UPSegd -> Vector a -> Vector a
fold1WithP f    = foldSegsWithP f (Seq.fold1SU f)
{-# INLINE_UP fold1WithP #-}


-- | Sum up segments specified by a `UPSegd`.
sumWithP :: (Num e, Unbox e) => UPSegd -> Vector e -> Vector e
sumWithP = foldWithP (+) 0
{-# INLINE_UP sumWithP #-}


-- | Fold the segments specified by a `UPSegd`.
--
--   This low level function takes a per-element worker and a per-segment worker.
--   It folds all the segments with the per-segment worker, then uses the
--   per-element worker to fixup the partial results when a segment 
--   is split across multiple threads.
--   
foldSegsWithP
        :: Unbox a
        => (a -> a -> a)
        -> (USegd -> Vector a -> Vector a)
        -> UPSegd -> Vector a -> Vector a

{-# INLINE_UP foldSegsWithP #-}
foldSegsWithP fElem fSeg segd xs 
 = dcarry `seq` drs `seq` 
   runST (do
        mrs <- joinDM theGang drs
        fixupFold fElem mrs dcarry
        US.unsafeFreeze mrs)

 where  (dcarry,drs)
          = unzipD
          $ mapD (What "UPSegd.foldSegsWithP/partial") theGang partial
          $ zipD (takeDistributed segd)
                 (splitD theGang balanced xs)

        partial (((segd', k), off), as)
         = let rs = fSeg segd' as
               {-# INLINE [0] n #-}
               n | off == 0  = 0
                 | otherwise = 1

           in  ((k, US.take n rs), US.drop n rs)


fixupFold
        :: Unbox a
        => (a -> a -> a)
        -> MVector s a
        -> Dist (Int,Vector a)
        -> ST s ()
{-# NOINLINE fixupFold #-}
fixupFold f !mrs !dcarry = go 1
  where
    !p = gangSize theGang

    go i | i >= p = return ()
         | US.null c = go (i+1)
         | otherwise   = do
                           x <- US.read mrs k
                           US.write mrs k (f x (US.index (here "fixupFold") c 0))
                           go (i + 1)
      where
        (k,c) = indexD (here "fixupFold") dcarry i
