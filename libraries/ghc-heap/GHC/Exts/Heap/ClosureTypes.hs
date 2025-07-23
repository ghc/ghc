{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

module GHC.Exts.Heap.ClosureTypes
    ( ClosureType(..)
    , closureTypeHeaderSize
    ) where

import Prelude -- See note [Why do we import Prelude here?]
import GHC.Internal.ClosureTypes

-- | Return the size of the closures header in words
closureTypeHeaderSize :: ClosureType -> Int
closureTypeHeaderSize closType =
    case closType of
        ct | THUNK <= ct && ct <= THUNK_0_2 -> thunkHeader
        ct | ct == THUNK_SELECTOR -> thunkHeader
        ct | ct == AP -> thunkHeader
        ct | ct == AP_STACK -> thunkHeader
        _ -> header
  where
    header = 1 + prof
    thunkHeader = 2 + prof
#if defined(PROFILING)
    prof = 2
#else
    prof = 0
#endif
