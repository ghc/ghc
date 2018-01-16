{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Standard combinators for distributed types.
module Data.Array.Parallel.Unlifted.Distributed.Primitive
        ( -- * The Gang
          Gang
        , gangSize
        , seqGang
        , forkGang
        , theGang

          -- * Distributed Types
        , DT (..)
        , newD
        , debugD
        , checkGangD

          -- * Primitive Distributed Operators.
        , generateD
        , generateD_cheap
        , imapD'
        , foldD
        , scanD)
where
import Data.Array.Parallel.Unlifted.Distributed.Primitive.DT
import Data.Array.Parallel.Unlifted.Distributed.Primitive.Gang
import Data.Array.Parallel.Unlifted.Distributed.Primitive.Operators 
import Control.Concurrent       (getNumCapabilities)
import System.IO.Unsafe         (unsafePerformIO)


-- | DPH programs use this single, shared gang of threads.
--   The gang exists at top level, and is initialised at program start.
-- 
--   The vectoriser guarantees that the gang is only used by a single
--   computation at a time. This is true because the program produced
--   by the vector only uses flat parallelism, so parallel computations
--   don't invoke further parallel computations. If the vectorised program
--   tries to use nested parallelism then there is a bug in the vectoriser,
--   and the code will run sequentially.
--
theGang :: Gang
theGang = unsafePerformIO (getNumCapabilities >>= forkGang)
{-# NOINLINE theGang #-}

