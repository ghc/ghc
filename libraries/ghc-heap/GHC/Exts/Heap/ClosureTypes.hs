{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

module GHC.Exts.Heap.ClosureTypes
    ( ClosureType(..)
    , closureTypeHeaderSize
    ) where

import Prelude -- See note [Why do we import Prelude here?]
import GHC.Generics

{- ---------------------------------------------
-- Enum representing closure types
-- This is a mirror of:
-- includes/rts/storage/ClosureTypes.h
-- ---------------------------------------------}

data ClosureType
    = INVALID_OBJECT
    | CONSTR
    | CONSTR_1_0
    | CONSTR_0_1
    | CONSTR_2_0
    | CONSTR_1_1
    | CONSTR_0_2
    | CONSTR_NOCAF
    | FUN
    | FUN_1_0
    | FUN_0_1
    | FUN_2_0
    | FUN_1_1
    | FUN_0_2
    | FUN_STATIC
    | THUNK
    | THUNK_1_0
    | THUNK_0_1
    | THUNK_2_0
    | THUNK_1_1
    | THUNK_0_2
    | THUNK_STATIC
    | THUNK_SELECTOR
    | BCO
    | AP
    | PAP
    | AP_STACK
    | IND
    | IND_STATIC
    | RET_BCO
    | RET_SMALL
    | RET_BIG
    | RET_FUN
    | UPDATE_FRAME
    | CATCH_FRAME
    | UNDERFLOW_FRAME
    | STOP_FRAME
    | BLOCKING_QUEUE
    | BLACKHOLE
    | MVAR_CLEAN
    | MVAR_DIRTY
    | TVAR
    | ARR_WORDS
    | MUT_ARR_PTRS_CLEAN
    | MUT_ARR_PTRS_DIRTY
    | MUT_ARR_PTRS_FROZEN_DIRTY
    | MUT_ARR_PTRS_FROZEN_CLEAN
    | MUT_VAR_CLEAN
    | MUT_VAR_DIRTY
    | WEAK
    | PRIM
    | MUT_PRIM
    | TSO
    | STACK
    | TREC_CHUNK
    | ATOMICALLY_FRAME
    | CATCH_RETRY_FRAME
    | CATCH_STM_FRAME
    | WHITEHOLE
    | SMALL_MUT_ARR_PTRS_CLEAN
    | SMALL_MUT_ARR_PTRS_DIRTY
    | SMALL_MUT_ARR_PTRS_FROZEN_DIRTY
    | SMALL_MUT_ARR_PTRS_FROZEN_CLEAN
    | COMPACT_NFDATA
    | N_CLOSURE_TYPES
 deriving (Enum, Eq, Ord, Show, Generic)

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
