{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Operations on Distributed Segment Descriptors
module Data.Array.Parallel.Unlifted.Distributed.Data.USSegd.Split 
        (splitSSegdOnElemsD)
where
import Data.Array.Parallel.Unlifted.Distributed.Arrays
import Data.Array.Parallel.Unlifted.Distributed.Combinators
import Data.Array.Parallel.Unlifted.Distributed.Primitive
import Data.Array.Parallel.Unlifted.Sequential.USSegd                   (USSegd)
import Data.Array.Parallel.Unlifted.Sequential.Vector                   (Vector)
import Data.Array.Parallel.Base
import Data.Bits                                                        (shiftR)
import Control.Monad                                                    (when)
import Data.Array.Parallel.Unlifted.Distributed.Data.USSegd.DT          ()
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd          as USegd
import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd         as USSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector         as Seq
import Debug.Trace

here :: String -> String
here s = "Data.Array.Parallel.Unlifted.Distributed.USSegd." ++ s

-------------------------------------------------------------------------------
-- | Split a segment descriptor across the gang, element wise.
--   We try to put the same number of elements on each thread, which means
--   that segments are sometimes split across threads.
--
--   Each thread gets a slice of segment descriptor, the segid of the first 
--   slice, and the offset of the first slice in its segment.
--   
--   Example:
--    In this picture each X represents 5 elements, and we have 5 segements in total.
--
-- @   segs:    ----------------------- --- ------- --------------- -------------------
--    elems:  |X X X X X X X X X|X X X X X X X X X|X X X X X X X X X|X X X X X X X X X|
--            |     thread1     |     thread2     |     thread3     |     thread4     |
--    segid:  0                 0                 3                 4
--    offset: 0                 45                0                 5
--
--    pprp $ splitSegdOnElemsD theGang 
--          $ lengthsToUSegd $ fromList [60, 10, 20, 40, 50 :: Int]
--
--     segd:    DUSegd lengths:  DVector lengths: [1,3,2,1]
--                                        chunks:  [[45],[15,10,20],[40,5],[45]]
--                     indices:  DVector lengths: [1,3,2,1]
--                                        chunks:  [[0], [0,15,25], [0,40],[0]]
--                    elements:  DInt [45,45,45,45]
--
--     segids: DInt [0,0,3,4]     (segment id of first slice on thread)
--    offsets: DInt [0,45,0,5]    (offset of that slice in its segment)
-- @
--
splitSSegdOnElemsD :: Gang -> USSegd -> Dist ((USSegd,Int),Int)
splitSSegdOnElemsD g !segd 
  = {-# SCC "splitSSegdOnElemsD" #-}
    traceEvent ("dph-prim-par: USSegd.splitSSegdOnElems")
  $ imapD (What "UPSSegd.splitSSegdOnElems/splitLenIx") g mk 
          (splitLenIdxD g (USegd.takeElements $ USSegd.takeUSegd segd))
  where 
        -- Number of threads in gang.
        !nThreads = gangSize g


        -- Build a USSegd from just the lengths, starts and sources fields.
        --   The indices and elems fields of the contained USegd are 
        --   generated from the lengths.
        buildUSSegd :: Vector Int -> Vector Int -> Vector Int -> USSegd
        buildUSSegd lengths starts sources
                = USSegd.mkUSSegd starts sources
                $ USegd.fromLengths lengths

        -- Determine what elements go on a thread
        mk :: Int                  -- Thread index.
           -> (Int, Int)           -- Number of elements on this thread,
                                   --   and starting offset into the flat array.
           -> ((USSegd, Int), Int) -- Segd for this thread, segid of first slice,
                                   --   and offset of first slice.

        mk i (nElems, ixStart) 
         = case chunk segd ixStart nElems (i == nThreads - 1) of
            (# lengths, starts, sources, l, o #) 
             -> ((buildUSSegd lengths starts sources, l), o)

{-# NOINLINE splitSSegdOnElemsD #-}
--  NOINLINE because it's complicated and won't fuse with anything.
--  This function has a large body of code and we don't want to blow up
--  the client modules by inlining it everywhere.


-------------------------------------------------------------------------------
-- | Determine what elements go on a thread.
--   The 'chunk' refers to the a chunk of the flat array, and is defined
--   by a set of segment slices. 
--
--   Example:
--    In this picture each X represents 5 elements, and we have 5 segements in total.
--
-- @  segs:    ----------------------- --- ------- --------------- -------------------
--    elems:  |X X X X X X X X X|X X X X X X X X X|X X X X X X X X X|X X X X X X X X X|
--            |     thread1     |     thread2     |     thread3     |     thread4     |
--    segid:  0                 0                 3                 4
--    offset: 0                 45                0                 5
--    k:               0                 1                 3                 5
--    k':              1                 3                 5                 5
--    left:            0                 15                0                 45
--    right:           45                20                5                 0
--    left_len:        0                 1                 0                 1
--    left_off:        0                 45                0                 5
--    n':              1                 3                 2                 1
-- @
chunk   :: USSegd          -- ^ Segment descriptor of entire array.
        -> Int            -- ^ Starting offset into the flat array for the first
                          --    slice on this thread.
        -> Int            -- ^ Number of elements in this thread.
        -> Bool           -- ^ Whether this is the last thread in the gang.
        -> (# Vector Int  --    Lengths of segment slices, 
            , Vector Int  --    Starting index of data in its vector
            , Vector Int  --    Source id
            , Int         --    segid of first slice
            , Int #)      --    offset of first slice.

chunk !ussegd !nStart !nElems is_last
  = (# lengths', starts', sources', k-left_len, left_off #)
  where
    -- Lengths of all segments.
    -- eg: [60, 10, 20, 40, 50]
    lengths     = USSegd.takeLengths ussegd

    -- Indices indices of all segments.
    -- eg: [0, 60, 70, 90, 130]
    indices     = USSegd.takeIndices ussegd

    -- Starting indices for all segments.
    starts      = USSegd.takeStarts ussegd

    -- Source ids for all segments.
    sources     = USSegd.takeSources ussegd
    
    -- Total number of segments defined by segment descriptor.
    -- eg: 5
    n    = Seq.length lengths

    -- Segid of the first seg that starts after the left of this chunk.
    k    = search nStart indices

    -- Segid of the first seg that starts after the right of this chunk.
    k'       | is_last     = n
             | otherwise   = search (nStart + nElems) indices

    -- The length of the left-most slice of this chunk.
    left     | k == n      = nElems
             | otherwise   = min ((Seq.index (here "chunk") indices k) - nStart) nElems

    -- The length of the right-most slice of this chunk.
    length_right   
             | k' == k     = 0
             | otherwise   = nStart + nElems - (Seq.index (here "chunk") indices (k'-1))

    -- Whether the first element in this chunk is an internal element of
    -- of a segment. Alternatively, indicates that the first element of 
    -- the chunk is not the first element of a segment.            
    left_len | left == 0   = 0
             | otherwise   = 1

    -- If the first element of the chunk starts within a segment, 
    -- then gives the index within that segment, otherwise 0.
    left_off | left == 0   = 0
             | otherwise   = nStart - (Seq.index (here "chunk") indices (k-1))

    -- How many segments this chunk straddles.
    n' = left_len + (k'-k)

    -- Create the lengths for this chunk by first copying out the lengths
    -- from the original segment descriptor. If the slices on the left
    -- and right cover partial segments, then we update the corresponding
    -- lengths.
    (!lengths', !starts', !sources')
     = runST (do
            -- Create a new array big enough to hold all the lengths for this chunk.
            mlengths' <- Seq.newM n'
            msources' <- Seq.newM n'
            mstarts'  <- Seq.newM n'

            -- If the first element is inside a segment, 
            --   then update the length to be the length of the slice.
            when (left /= 0) 
             $ do Seq.write mlengths' 0 left
                  Seq.write mstarts'  0 (Seq.index (here "chunk") starts  (k - left_len) + left_off)
                  Seq.write msources' 0 (Seq.index (here "chunk") sources (k - left_len))

            -- Copy out array lengths for this chunk.
            Seq.copy (Seq.mdrop left_len mlengths') (Seq.slice (here "chunk") lengths k (k'-k))
            Seq.copy (Seq.mdrop left_len mstarts')  (Seq.slice (here "chunk")  starts k (k'-k))
            Seq.copy (Seq.mdrop left_len msources') (Seq.slice (here "chunk") sources k (k'-k))

            -- If the last element is inside a segment, 
            --   then update the length to be the length of the slice.
            when (length_right /= 0)
             $ do Seq.write mlengths' (n' - 1) length_right

            clengths' <- Seq.unsafeFreeze mlengths'
            cstarts'  <- Seq.unsafeFreeze mstarts'
            csources' <- Seq.unsafeFreeze msources'
            return (clengths', cstarts', csources'))

{-      = trace 
        (render $ vcat
                [ text "CHUNK"
                , pprp segd
                , text "nStart:  " <+> int nStart
                , text "nElems:  " <+> int nElems
                , text "k:       " <+> int k
                , text "k':      " <+> int k'
                , text "left:    " <+> int left
                , text "right:   " <+> int right
                , text "left_len:" <+> int left_len
                , text "left_off:" <+> int left_off
                , text "n':      " <+> int n'
                , text ""]) lens'
-}

{-# INLINE_DIST chunk #-}
--  INLINE_DIST even though it should be inlined into splitSSegdOnElemsD anyway
--  because that function contains the only use.


-------------------------------------------------------------------------------
-- O(log n).
-- Given a monotonically increasing vector of `Int`s,
-- find the first element that is larger than the given value.
-- 
-- eg  search 75 [0, 60, 70, 90, 130] = 90
--     search 43 [0, 60, 70, 90, 130] = 60
--
search :: Int -> Vector Int -> Int
search !x ys = go 0 (Seq.length ys)
  where
    go i n | n <= 0        = i
           | Seq.index (here "search") ys mid < x
           = go (mid + 1) (n - half - 1)
           | otherwise     = go i half
      where
        half = n `shiftR` 1
        mid  = i + half
{-# INLINE_DIST search #-}
--  INLINE_DIST because we want it inlined into both uses in 'chunk' above.

