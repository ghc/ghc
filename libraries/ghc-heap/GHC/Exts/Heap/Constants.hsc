{-# LANGUAGE CPP #-}

module GHC.Exts.Heap.Constants
    ( wORD_SIZE
    , tAG_MASK
    , wORD_SIZE_IN_BITS
    ) where

#include "MachDeps.h"

import Prelude -- See note [Why do we import Prelude here?]
import Data.Bits

wORD_SIZE, tAG_MASK, wORD_SIZE_IN_BITS :: Int
wORD_SIZE = #const SIZEOF_HSWORD
wORD_SIZE_IN_BITS = #const WORD_SIZE_IN_BITS
tAG_MASK = (1 `shift` #const TAG_BITS) - 1
