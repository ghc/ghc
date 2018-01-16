{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Distribution of Segment Descriptors
module Data.Array.Parallel.Unlifted.Distributed.Data.USegd 
        ( mkDUSegdD
        , lengthD
        , takeLengthsD
        , takeIndicesD
        , takeElementsD
        , splitSegdOnSegsD
        , splitSegdOnElemsD
        , splitSD
        , joinSegdD
        , glueSegdD)
where
import Data.Array.Parallel.Unlifted.Distributed.Data.USegd.DT           ()
import Data.Array.Parallel.Unlifted.Distributed.Data.USegd.Base
import Data.Array.Parallel.Unlifted.Distributed.Data.USegd.Split
