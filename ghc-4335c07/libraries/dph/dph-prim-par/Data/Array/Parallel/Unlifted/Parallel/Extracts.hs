{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Parallel combinators for segmented unboxed arrays
module Data.Array.Parallel.Unlifted.Parallel.Extracts 
        ( -- * Scattered indexing
          indexsFromVector
        , indexsFromVectorsUPVSegdP
        , indexsFromVectorsUPVSegd

          -- * Scattered extracts
        , extractsFromNestedUPSSegd
        , extractsFromVectorsUPSSegd

        , extractsFromVectorsUPVSegdP
        , extractsFromVectorsUPVSegd
        , extractsFromVectorsUPSSegdSegmap)
where
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Distributed.What
import Data.Array.Parallel.Unlifted.Parallel.UPSSegd                    (UPSSegd)
import Data.Array.Parallel.Unlifted.Parallel.UPVSegd                    (UPVSegd)
import Data.Array.Parallel.Unlifted.Vectors                             (Vectors)
import Data.Array.Parallel.Unlifted.Sequential.Vector                   as Seq
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSSegd          as UPSSegd
import qualified Data.Array.Parallel.Unlifted.Parallel.UPVSegd          as UPVSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.UVSegd         as UVSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd          as USegd
import qualified Data.Array.Parallel.Unlifted.Distributed.Data.USegd    as USegd
import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd         as USSegd
import qualified Data.Array.Parallel.Unlifted.Vectors                   as US
import qualified Data.Array.Parallel.Unlifted.Stream                    as US
import qualified Data.Array.Parallel.Unlifted.Sequential                as Seq
import qualified Data.Vector                                            as V
import Debug.Trace
import Prelude  as P

-- Indexvs --------------------------------------------------------------------
-- | Lookup elements from a `Vector`.
--
--   TODO: make this parallel.
--
indexsFromVector
        :: Unbox a
        => Vector a -> Vector Int -> Vector a

indexsFromVector = Seq.indexsFromVector


-- | Lookup elements from some `Vectors` through a `UPVSegd`.
--
--   TODO: make this parallel.
--
indexsFromVectorsUPVSegdP 
        :: (Unbox a, US.Unboxes a)
        => Vectors a -> UPVSegd -> Vector (Int, Int) -> Vector a

indexsFromVectorsUPVSegdP vectors upvsegd vsrcixs
 = splitJoinD theGang 
        (mapD   (What "indexsFromVectorsUPVSegdP") theGang
                (indexsFromVectorsUPVSegd vectors upvsegd))
        vsrcixs
{-# INLINE_UP indexsFromVectorsUPVSegdP #-}


-- | Lookup elements from some Vectors through a `UPVSegd`
indexsFromVectorsUPVSegd 
        :: (Unbox a, US.Unboxes a)
        => Vectors a -> UPVSegd -> Vector (Int, Int) -> Vector a

indexsFromVectorsUPVSegd vectors upvsegd vsrcixs
 = let  -- Because we're just doing indexing here, we don't need the culled
        -- vsegids or ussegd, and can just use the redundant version.
        !vsegids  = UPVSegd.takeVSegidsRedundant upvsegd
        !upssegd  = UPVSegd.takeUPSSegdRedundant upvsegd
        !ussegd   = UPSSegd.takeUSSegd upssegd
   in  Seq.unstream
         $ US.streamElemsFromVectors     vectors
         $ US.streamSrcIxsThroughUSSegd  ussegd
         $ US.streamSrcIxsThroughVSegids vsegids
         $ Seq.stream vsrcixs
{-# INLINE_U indexsFromVectorsUPVSegd #-}


-- Extracts -------------------------------------------------------------------
-- | Copy segments from a nested vectors and concatenate them into a new array.
extractsFromNestedUPSSegd
        :: Unbox a
        => UPSSegd -> V.Vector (Vector a) -> Vector a

extractsFromNestedUPSSegd upssegd vectors
        = Seq.unstream 
        $ US.streamSegsFromNestedUSSegd
                vectors
                (UPSSegd.takeUSSegd upssegd)
{-# INLINE_U extractsFromNestedUPSSegd #-}

-- | TODO: make this parallel.
extractsFromVectorsUPSSegd
        :: (Unbox a, US.Unboxes a)
        => UPSSegd
        -> Vectors a
        -> Vector a

extractsFromVectorsUPSSegd upssegd vectors
        = Seq.extractsFromVectorsUSSegd
                (UPSSegd.takeUSSegd upssegd) 
                vectors
{-# INLINE_UP extractsFromVectorsUPSSegd #-}



-- From UPVSegd ---------------------------------------------------------------
-- | Parallel extracts from UPVSegd and Segmap
--   TODO: This just distributes the segmap over the gang, and will be unbalanced
--         if there aren't many segments, or they have varying sizes.
extractsFromVectorsUPVSegdP
        :: (Unbox a, US.Unboxes a)
        => UPVSegd
        -> Vectors a
        -> Vector a

extractsFromVectorsUPVSegdP !upvsegd !vectors
 = let !segs    = UPVSegd.takeDistributed       upvsegd
       !vsegids = UPVSegd.takeVSegidsRedundant  upvsegd
       !ussegd  = UPSSegd.takeUSSegd
                $ UPVSegd.takeUPSSegdRedundant  upvsegd
   in joinD theGang balanced
    $ mapD (What "extractsFromVectorsUPVSegdP")
        theGang
        (extractsFromVectorsUPSSegd_split
                ussegd
                vsegids
                vectors)
        segs

{-# INLINE_UP extractsFromVectorsUPVSegdP #-}

-- | Sequential extracts from USSegd and Segmap
extractsFromVectorsUPSSegd_split
        :: (Unbox a, US.Unboxes a)
        => USSegd.USSegd
        -> Vector Int
        -> Vectors a
        -> ((USegd.USegd,Int),Int)
        -> Vector a

extractsFromVectorsUPSSegd_split !ussegd !vsegids !vectors !which
        = Seq.unstream 
        $ US.streamSegsFromVectorsUSSegd_split vectors
                ussegd vsegids which
{-# INLINE_UP extractsFromVectorsUPSSegd_split #-}


-- | Sequential extracts from UPVSegd.
extractsFromVectorsUPVSegd
        :: (Unbox a, US.Unboxes a)
        => UPVSegd
        -> Vectors a
        -> Vector a

extractsFromVectorsUPVSegd upvsegd vectors
        = Seq.unstream 
        $ US.streamSegsFromVectorsUVSegd vectors
        $ UVSegd.mkUVSegd 
                (UPVSegd.takeVSegidsRedundant upvsegd)
                (UPSSegd.takeUSSegd $ UPVSegd.takeUPSSegdRedundant upvsegd)
{-# INLINE_UP extractsFromVectorsUPVSegd #-}


-- | Sequential extracts from USSegd and Segmap
extractsFromVectorsUPSSegdSegmap
        :: (Unbox a, US.Unboxes a)
        => UPSSegd
        -> Vectors a
        -> Vector Int
        -> Vector a

extractsFromVectorsUPSSegdSegmap upssegd vectors segmap
        = Seq.unstream 
        $ US.streamSegsFromVectorsUSSegdSegmap vectors
                (UPSSegd.takeUSSegd upssegd)
                segmap
{-# INLINE_UP extractsFromVectorsUPSSegdSegmap #-}
