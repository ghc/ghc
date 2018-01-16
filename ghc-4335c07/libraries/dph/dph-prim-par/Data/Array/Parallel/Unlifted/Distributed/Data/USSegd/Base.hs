{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Distribution of Segment Descriptors
module Data.Array.Parallel.Unlifted.Distributed.Data.USSegd.Base
        ( lengthD
        , takeLengthsD
        , takeIndicesD
        , takeElementsD
        , takeStartsD
        , takeSourcesD
        , takeUSegdD)
where
import Data.Array.Parallel.Unlifted.Distributed.Data.USSegd.DT
import Data.Array.Parallel.Unlifted.Distributed.Primitive.DT
import Data.Array.Parallel.Unlifted.Sequential.USSegd                   (USSegd)
import Data.Array.Parallel.Unlifted.Sequential.USegd                    (USegd)
import Data.Array.Parallel.Unlifted.Sequential.Vector                   (Vector)
import Prelude                                                          as P
import qualified Data.Array.Parallel.Unlifted.Distributed.Data.USegd    as DUSegd
import qualified Data.Array.Parallel.Unlifted.Distributed.Data.Vector   as DV


-- | O(1). Yield the overall number of segments.
lengthD :: Dist USSegd -> Dist Int
lengthD (DUSSegd starts _ _) 
        = DV.lengthD starts
{-# INLINE_DIST lengthD #-}


-- | O(1). Yield the lengths of the individual segments.
takeLengthsD :: Dist USSegd -> Dist (Vector Int)
takeLengthsD (DUSSegd _ _ usegds)
        = DUSegd.takeLengthsD usegds
{-# INLINE_DIST takeLengthsD #-}


-- | O(1). Yield the segment indices.
takeIndicesD :: Dist USSegd -> Dist (Vector Int)
takeIndicesD (DUSSegd _ _ usegds)
        = DUSegd.takeIndicesD usegds
{-# INLINE_DIST takeIndicesD #-}


-- | O(1). Yield the number of data elements.
takeElementsD :: Dist USSegd -> Dist Int
takeElementsD (DUSSegd _ _ usegds)
        = DUSegd.takeElementsD usegds
{-# INLINE_DIST takeElementsD #-}


-- | O(1). Yield the starting indices.
takeStartsD :: Dist USSegd -> Dist (Vector Int)
takeStartsD (DUSSegd starts _ _)
        = starts
{-# INLINE_DIST takeStartsD #-}
        

-- | O(1). Yield the source ids
takeSourcesD :: Dist USSegd -> Dist (Vector Int)
takeSourcesD (DUSSegd _ sources _)
        = sources
{-# INLINE_DIST takeSourcesD #-}


-- | O(1). Yield the USegd
takeUSegdD :: Dist USSegd -> Dist USegd
takeUSegdD (DUSSegd _ _ usegd)
        = usegd
{-# INLINE_DIST takeUSegdD #-}
