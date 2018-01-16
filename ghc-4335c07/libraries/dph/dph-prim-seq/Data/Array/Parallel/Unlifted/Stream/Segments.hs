{-# LANGUAGE CPP, NoMonomorphismRestriction #-}
#include "fusion-phases.h"
module Data.Array.Parallel.Unlifted.Stream.Segments
        ( streamSegsFromNestedUSSegd
        , streamSegsFromVectorsUSSegd
        , streamSegsFromVectorsUVSegd
        , streamSegsFromVectorsUSSegdSegmap
        , streamSegsFromVectorsUSSegd_split)
where
import Data.Vector.Fusion.Bundle.Monadic                        (Bundle(..), fromStream)
import Data.Vector.Fusion.Bundle.Size
import Data.Vector.Fusion.Stream.Monadic                        (Stream(..), Step(..))
import Data.Array.Parallel.Unlifted.Sequential.Vector           (Unbox,   Vector, index)
import Data.Array.Parallel.Unlifted.Vectors                     (Unboxes, Vectors)
import Data.Array.Parallel.Unlifted.Sequential.USegd            (USegd(..))
import Data.Array.Parallel.Unlifted.Sequential.USSegd           (USSegd(..))
import Data.Array.Parallel.Unlifted.Sequential.UVSegd           (UVSegd(..))
import qualified Data.Array.Parallel.Unlifted.Vectors           as US
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd  as USegd
import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd as USSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as U
import qualified Data.Vector                                    as V
import qualified Data.Primitive.ByteArray                       as P
import System.IO.Unsafe


-- Nested -----------------------------------------------------------------------------------------
-- | Stream some physical segments from many data arrays.
--- 
--   * TODO: make this more efficient, and fix fusion.
--           We should be able to eliminate a lot of the indexing happening in the 
--           inner loop by being cleverer about the loop state.
--
--   * TODO: If this is contiguous then we can stream the lot without worrying 
--           about jumping between segments. EXCEPT that this information must be
--           statically visible else streamSegs won't fuse, so we can't have an 
--           ifThenElse checking the manifest flag.
streamSegsFromNestedUSSegd
        :: (Unbox a, Monad m)
        => V.Vector (Vector a)  -- ^ Source arrays.
        -> USSegd               -- ^ Segment descriptor defining segments base on source vectors.
        -> Bundle m v a

streamSegsFromNestedUSSegd
        pdatas
        ussegd@(USSegd _ starts sources usegd)
 = let  
        here            = "streamSegsFromNestedUSSegd"

        -- length of each segment
        pseglens        = USegd.takeLengths usegd
 
        -- We've finished streaming this pseg
        {-# INLINE_INNER fn #-}
        fn (pseg, ix)
         -- All psegs are done.
         | pseg >= USSegd.length ussegd
         = return $ Done
         
         -- Current pseg is done
         | ix   >= U.index here pseglens pseg 
         = return $ Skip (pseg + 1, 0)

         -- Stream an element from this pseg
         | otherwise
         = let  !srcid   = index here sources pseg
                !pdata   = pdatas  `V.unsafeIndex` srcid
                !start   = index here starts pseg
                !result  = index here pdata  (start + ix)
           in   return $ Yield result (pseg, ix + 1)

   in   fromStream (Stream fn (0, 0)) Unknown
{-# INLINE_STREAM streamSegsFromNestedUSSegd #-}


-- Vectors ----------------------------------------------------------------------------------------
-- | Stream segments from a `Vectors`.
-- 
--   * There must be at least one segment in the `USSegd`, but this is not checked.
-- 
--   * No bounds checking is done for the `USSegd`.
--
streamSegsFromVectorsUSSegd
        :: (Unboxes a, Monad m)
        => Vectors a            -- ^ Vectors holding source data.
        -> USSegd               -- ^ Scattered segment descriptor
        -> Bundle m v a

streamSegsFromVectorsUSSegd
        vectors
        ussegd@(USSegd _ segStarts segSources usegd) 
 = segStarts `seq` segSources `seq` usegd `seq` vectors `seq`
   let  here            = "stremSegsFromVectorsUSSegd"

        -- Length of each segment
        !segLens        = USegd.takeLengths usegd

        -- Total number of segments.
        !segsTotal      = USSegd.length ussegd
 
        -- Total number of elements to stream.
        !elements       = USegd.takeElements usegd

        -- seg, ix of that seg in usegd, length of seg, elem in seg
        {-# INLINE_INNER fnSeg #-}
        fnSeg (ixSeg, baSeg, ixEnd, ixElem)
         = ixSeg `seq` baSeg `seq`
           if ixElem >= ixEnd                   -- Was that the last elem in the current seg?
            then if ixSeg + 1 >= segsTotal      -- Was that last seg?

                       -- That was the last seg, we're done.
                  then return $ Done
                  
                       -- Move to the next seg.
                  else let ixSeg'       = ixSeg + 1
                           sourceSeg    = index here segSources ixSeg'
                           startSeg     = index here segStarts  ixSeg'
                           lenSeg       = index here segLens    ixSeg'
                           (arr, startArr, _) 
                                        = US.unsafeIndexUnpack vectors sourceSeg
                       in  return $ Skip
                                  ( ixSeg'
                                  , arr
                                  , startArr + startSeg + lenSeg
                                  , startArr + startSeg)

                 -- Stream the next element from the segment.
            else let !result  = P.indexByteArray baSeg ixElem
                 in  return   $ Yield result (ixSeg, baSeg, ixEnd, ixElem + 1)
                                 
        -- Starting state of the stream.
        -- CAREFUL:
        --  The ussegd might not contain any segments, so we can't initialise the state
        --  just by taking the first segment length etc from the ussegd.
        --  On the other hand, we don't want to use an extra case expression to test for
        --  this sitution, as that could break fusion.
        --  Instead, start with a dummy state which forces the loop to grab the first 
        --  segment, if there are any.
        !dummy  = unsafePerformIO 
                $ P.newByteArray 0 >>= P.unsafeFreezeByteArray

        !initState
         =      ( -1    -- force fnSeg loop to load first seg
                , dummy -- dummy array data to start with
                , 0     -- force fnSeg loop to load first seg
                , 0)           

        -- It's important that we set the result stream size, so Data.Vector
        -- doesn't need to add code to grow the result when it overflows.
   in   fromStream (Stream fnSeg initState) (Exact elements)
{-# INLINE_STREAM streamSegsFromVectorsUSSegd #-}



-- Vectors ----------------------------------------------------------------------------------------
-- | Stream segments from a `Vectors`.
-- 
--   * There must be at least one segment in the `USSegd`, but this is not checked.
-- 
--   * No bounds checking is done for the `USSegd`.
--
streamSegsFromVectorsUVSegd
        :: (Unboxes a, Monad m)
        => Vectors a            -- ^ Vectors holding source data.
        -> UVSegd               -- ^ Scattered segment descriptor
        -> Bundle m v a

streamSegsFromVectorsUVSegd
        vectors
        (UVSegd _ _ segmap _ ussegd)
 = streamSegsFromVectorsUSSegdSegmap vectors ussegd segmap
{-# INLINE_STREAM streamSegsFromVectorsUVSegd #-}


streamSegsFromVectorsUSSegdSegmap
        :: (Unboxes a, Monad m)
        => Vectors a            -- ^ Vectors holding source data.
        -> USSegd               -- ^ Scattered segment descriptor
        -> Vector Int           -- ^ Segmap
        -> Bundle m v a

streamSegsFromVectorsUSSegdSegmap
        vectors ussegd@(USSegd _ segStarts segSources usegd) segmap
 = segStarts `seq` segSources `seq` usegd `seq` segmap `seq`
   let  here            = "stremSegsFromVectorsUVSegd"

        -- Total number of elements to be streamed
        !lengths        = USSegd.takeLengths ussegd
        !elemsTotal     = U.sum $ U.map (U.index here lengths) segmap

        -- Total number of segments.
        !segsTotal      = U.length segmap
 
        -- Length of each physical segment.
        !segLens        = USegd.takeLengths usegd
        
        -- seg, ix of that seg in usegd, length of seg, elem in seg
        {-# INLINE_INNER fnSeg #-}
        fnSeg (ixSeg, baSeg, ixEnd, ixElem)
         = ixSeg `seq` baSeg `seq`
           if ixElem >= ixEnd                   -- Was that the last elem in the current seg?
            then if ixSeg + 1 >= segsTotal      -- Was that last seg?

                       -- That was the last seg, we're done.
                  then return $ Done
                  
                       -- Move to the next seg.
                  else let ixSeg'       = ixSeg + 1
                           ixPSeg       = index here segmap     ixSeg'
                           sourceSeg    = index here segSources ixPSeg
                           startSeg     = index here segStarts  ixPSeg
                           lenSeg       = index here segLens    ixPSeg
                           (arr, startArr, _) 
                                        = US.unsafeIndexUnpack vectors sourceSeg
                       in  return $ Skip
                                  ( ixSeg'
                                  , arr
                                  , startArr + startSeg + lenSeg
                                  , startArr + startSeg)

                 -- Stream the next element from the segment.
            else let !result  = P.indexByteArray baSeg ixElem
                 in  return   $ Yield result (ixSeg, baSeg, ixEnd, ixElem + 1)
                                 
        -- Starting state of the stream.
        !dummy  = unsafePerformIO 
                $ P.newByteArray 0 >>= P.unsafeFreezeByteArray

        !initState
         =      ( -1    -- force fnSeg loop to load first seg
                , dummy -- dummy array data to start with
                , 0     -- force fnSeg loop to load first seg
                , 0)           

        -- It's important that we set the result stream size, so Data.Vector
        -- doesn't need to add code to grow the result when it overflows.
   in   fromStream (Stream fnSeg initState) (Exact elemsTotal)
{-# INLINE_STREAM streamSegsFromVectorsUSSegdSegmap #-}




streamSegsFromVectorsUSSegd_split
        :: (Unboxes a, Monad m)
        => Vectors a            -- ^ Vectors holding source data.
        -> USSegd               -- ^ Scattered segment descriptor
        -> Vector Int           -- ^ Virtual segment ids
        -> ((USegd,Int),Int)    -- ^ Segmap
        -> Bundle m v a

streamSegsFromVectorsUSSegd_split
        !vectors !ussegd
        !vsegids ((!segd,!seg_off),!el_off)
 = let  here            = "streamSegsFromVectorsUSSegd_split"

        -- Total number of elements to be streamed
        !lengths        = USegd.takeLengths segd
        !elemsTotal     = U.sum lengths

        -- Total number of segments.
        !segsTotal      = U.length lengths

        !segStarts      = USSegd.takeStarts  ussegd
        !segSources     = USSegd.takeSources ussegd

        vsegid seg      = index here vsegids    (seg + seg_off)
        {-# INLINE vsegid #-}
        source pseg     = index here segSources  pseg
        {-# INLINE source #-}
        start  pseg     = index here segStarts   pseg
        {-# INLINE start #-}
        len    seg      = index here lengths     seg
        {-# INLINE len #-}

        -- seg, ix of that seg in usegd, length of seg, elem in seg
        {-# INLINE_INNER fnSeg #-}
        fnSeg (!ixSeg, !baSeg, !ixEnd, !ixElem)
         = if ixElem >= ixEnd                   -- Was that the last elem in the current seg?
            then if ixSeg + 1 >= segsTotal      -- Was that last seg?

                  -- That was the last seg, we're done.
                  then return $ Done

                  -- Move to the next seg.
                  else let ixSeg'       = ixSeg + 1
                           ixPSeg       = vsegid ixSeg'
                           sourceSeg    = source ixPSeg
                           startSeg     = start  ixPSeg
                           lenSeg       = len    ixSeg'
                           el_off'      = if ixSeg' == 0 then el_off else 0
                           (arr, startArr, _) 
                                        = US.unsafeIndexUnpack vectors sourceSeg
                       in  return $ Skip
                                  ( ixSeg'
                                  , arr
                                  , startArr + startSeg + el_off' + lenSeg
                                  , startArr + startSeg + el_off')

                 -- Stream the next element from the segment.
            else let !result  = P.indexByteArray baSeg ixElem
                 in  return   $ Yield result (ixSeg, baSeg, ixEnd, ixElem + 1)

        -- Starting state of the stream.
        !dummy  = unsafePerformIO 
                $ P.newByteArray 0 >>= P.unsafeFreezeByteArray

        !initState
         =      ( -1    -- force fnSeg loop to load first seg
                , dummy -- dummy array data to start with
                , 0     -- force fnSeg loop to load first seg
                , 0)

        -- It's important that we set the result stream size, so Data.Vector
        -- doesn't need to add code to grow the result when it overflows.
   in   fromStream (Stream fnSeg initState) (Exact elemsTotal)
{-# INLINE_STREAM streamSegsFromVectorsUSSegd_split #-}

