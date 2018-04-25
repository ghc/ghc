{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Distribution of Segment Descriptors
module Data.Array.Parallel.Unlifted.Distributed.Data.USegd.Base
        ( mkDUSegdD
        , lengthD
        , takeLengthsD
        , takeIndicesD
        , takeElementsD)
where
import Data.Array.Parallel.Unlifted.Distributed.Data.USegd.DT
import Data.Array.Parallel.Unlifted.Distributed.Primitive
import Data.Array.Parallel.Unlifted.Sequential.USegd                    (USegd)
import Data.Array.Parallel.Unlifted.Sequential.Vector                   (Vector)
import qualified Data.Array.Parallel.Unlifted.Distributed.Data.Vector  as DV


-- | O(1). Construct a distributed segment descriptor
mkDUSegdD 
        :: Dist (Vector Int)    -- ^ segment lengths
        -> Dist (Vector Int)    -- ^ segment indices
        -> Dist Int             -- ^ number of elements in each chunk
        -> Dist USegd

mkDUSegdD = DUSegd
{-# INLINE_DIST mkDUSegdD #-}


-- | O(1). Yield the overall number of segments.
lengthD :: Dist USegd -> Dist Int
lengthD (DUSegd lens _ _) 
        = DV.lengthD lens
{-# INLINE_DIST lengthD #-}


-- | O(1). Yield the lengths of the individual segments.
takeLengthsD :: Dist USegd -> Dist (Vector Int)
takeLengthsD (DUSegd lens _ _ )
        = lens
{-# INLINE_DIST takeLengthsD #-}


-- | O(1). Yield the segment indices of a segment descriptor.
takeIndicesD :: Dist USegd -> Dist (Vector Int)
takeIndicesD (DUSegd _ idxs _)
        = idxs
{-# INLINE_DIST takeIndicesD #-}


-- | O(1). Yield the number of data elements.
takeElementsD :: Dist USegd -> Dist Int
takeElementsD (DUSegd _ _ dns)
        = dns
{-# INLINE_DIST takeElementsD #-}
