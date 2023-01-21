{-# LANGUAGE CPP #-}
module GHC.Exts.StackConstants where

-- TODO: Better expression to allow is only for the latest (this branch) GHC?
#if MIN_TOOL_VERSION_ghc(9,5,0)

import           Prelude

#include "Rts.h"
#undef BLOCK_SIZE
#undef MBLOCK_SIZE
#undef BLOCKS_PER_MBLOCK
#include "DerivedConstants.h"

offsetStgCatchFrameHandler :: Int
offsetStgCatchFrameHandler = (#const OFFSET_StgCatchFrame_handler) + (#size StgHeader)

offsetStgCatchSTMFrameCode :: Int
offsetStgCatchSTMFrameCode = (#const OFFSET_StgCatchSTMFrame_code) + (#size StgHeader)

offsetStgCatchSTMFrameHandler :: Int
offsetStgCatchSTMFrameHandler = (#const OFFSET_StgCatchSTMFrame_handler) + (#size StgHeader)

offsetStgUpdateFrameUpdatee :: Int
offsetStgUpdateFrameUpdatee = (#const OFFSET_StgUpdateFrame_updatee) + (#size StgHeader)

offsetStgAtomicallyFrameCode :: Int
offsetStgAtomicallyFrameCode = (#const OFFSET_StgAtomicallyFrame_code) + (#size StgHeader)

offsetStgAtomicallyFrameResult :: Int
offsetStgAtomicallyFrameResult = (#const OFFSET_StgAtomicallyFrame_result) + (#size StgHeader)

offsetStgCatchRetryFrameRunningAltCode :: Int
offsetStgCatchRetryFrameRunningAltCode = (#const OFFSET_StgCatchRetryFrame_running_alt_code) + (#size StgHeader)

offsetStgCatchRetryFrameRunningFirstCode :: Int
offsetStgCatchRetryFrameRunningFirstCode = (#const OFFSET_StgCatchRetryFrame_first_code) + (#size StgHeader)

offsetStgCatchRetryFrameAltCode :: Int
offsetStgCatchRetryFrameAltCode = (#const OFFSET_StgCatchRetryFrame_alt_code) + (#size StgHeader)

offsetStgRetFunFrameSize :: Int
-- StgRetFun has no header, but only a pointer to the info table at the beginning.
offsetStgRetFunFrameSize = (#const OFFSET_StgRetFun_size)

offsetStgRetFunFrameFun :: Int
offsetStgRetFunFrameFun = (#const OFFSET_StgRetFun_fun)

offsetStgRetFunFramePayload :: Int
offsetStgRetFunFramePayload = (#const OFFSET_StgRetFun_payload)

offsetStgBCOFrameInstrs :: Int
offsetStgBCOFrameInstrs = (#const OFFSET_StgBCO_instrs) + (#size StgHeader)

offsetStgBCOFrameLiterals :: Int
offsetStgBCOFrameLiterals = (#const OFFSET_StgBCO_literals) + (#size StgHeader)

offsetStgBCOFramePtrs :: Int
offsetStgBCOFramePtrs = (#const OFFSET_StgBCO_ptrs) + (#size StgHeader)

offsetStgBCOFrameArity :: Int
offsetStgBCOFrameArity = (#const OFFSET_StgBCO_arity) + (#size StgHeader)

offsetStgBCOFrameSize :: Int
offsetStgBCOFrameSize = (#const OFFSET_StgBCO_size) + (#size StgHeader)

offsetStgClosurePayload :: Int
offsetStgClosurePayload = (#const OFFSET_StgClosure_payload) + (#size StgHeader)

bytesInWord :: Int
bytesInWord = (#const SIZEOF_UNSIGNED_LONG)
#endif
