{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Distribution of Segment Descriptors
module Data.Array.Parallel.Unlifted.Distributed.Data.USSegd 
        ( lengthD
        , takeLengthsD
        , takeIndicesD
        , takeElementsD
        , takeStartsD
        , takeSourcesD
        , takeUSegdD
        , splitSSegdOnElemsD)
where
import Data.Array.Parallel.Unlifted.Distributed.Data.USSegd.DT          ()
import Data.Array.Parallel.Unlifted.Distributed.Data.USSegd.Base
import Data.Array.Parallel.Unlifted.Distributed.Data.USSegd.Split
