{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GHC.Exts.StackConstants where

-- TODO: Better expression to allow is only for the latest (this branch) GHC?
#if MIN_TOOL_VERSION_ghc(9,5,0)

import           Prelude

#include "Rts.h"
#undef BLOCK_SIZE
#undef MBLOCK_SIZE
#undef BLOCKS_PER_MBLOCK
#include "DerivedConstants.h"

newtype ByteOffset = ByteOffset { offsetInBytes :: Int }
  deriving newtype (Eq, Show, Integral, Real, Num, Enum, Ord)

newtype WordOffset = WordOffset { offsetInWords :: Int }
  deriving newtype (Eq, Show, Integral, Real, Num, Enum, Ord)

offsetStgCatchFrameHandler :: ByteOffset
offsetStgCatchFrameHandler = (#const OFFSET_StgCatchFrame_handler) + (#size StgHeader)

offsetStgCatchSTMFrameCode :: ByteOffset
offsetStgCatchSTMFrameCode = (#const OFFSET_StgCatchSTMFrame_code) + (#size StgHeader)

offsetStgCatchSTMFrameHandler :: ByteOffset
offsetStgCatchSTMFrameHandler = (#const OFFSET_StgCatchSTMFrame_handler) + (#size StgHeader)

offsetStgUpdateFrameUpdatee :: ByteOffset
offsetStgUpdateFrameUpdatee = (#const OFFSET_StgUpdateFrame_updatee) + (#size StgHeader)

offsetStgAtomicallyFrameCode :: ByteOffset
offsetStgAtomicallyFrameCode = (#const OFFSET_StgAtomicallyFrame_code) + (#size StgHeader)

offsetStgAtomicallyFrameResult :: ByteOffset
offsetStgAtomicallyFrameResult = (#const OFFSET_StgAtomicallyFrame_result) + (#size StgHeader)

offsetStgCatchRetryFrameRunningAltCode :: ByteOffset
offsetStgCatchRetryFrameRunningAltCode = (#const OFFSET_StgCatchRetryFrame_running_alt_code) + (#size StgHeader)

offsetStgCatchRetryFrameRunningFirstCode :: ByteOffset
offsetStgCatchRetryFrameRunningFirstCode = (#const OFFSET_StgCatchRetryFrame_first_code) + (#size StgHeader)

offsetStgCatchRetryFrameAltCode :: ByteOffset
offsetStgCatchRetryFrameAltCode = (#const OFFSET_StgCatchRetryFrame_alt_code) + (#size StgHeader)

offsetStgRetFunFrameSize :: ByteOffset
-- StgRetFun has no header, but only a pointer to the info table at the beginning.
offsetStgRetFunFrameSize = (#const OFFSET_StgRetFun_size)

offsetStgRetFunFrameFun :: ByteOffset
offsetStgRetFunFrameFun = (#const OFFSET_StgRetFun_fun)

offsetStgRetFunFramePayload :: ByteOffset
offsetStgRetFunFramePayload = (#const OFFSET_StgRetFun_payload)

offsetStgBCOFrameInstrs :: ByteOffset
offsetStgBCOFrameInstrs = (#const OFFSET_StgBCO_instrs) + (#size StgHeader)

offsetStgBCOFrameLiterals :: ByteOffset
offsetStgBCOFrameLiterals = (#const OFFSET_StgBCO_literals) + (#size StgHeader)

offsetStgBCOFramePtrs :: ByteOffset
offsetStgBCOFramePtrs = (#const OFFSET_StgBCO_ptrs) + (#size StgHeader)

offsetStgBCOFrameArity :: ByteOffset
offsetStgBCOFrameArity = (#const OFFSET_StgBCO_arity) + (#size StgHeader)

offsetStgBCOFrameSize :: ByteOffset
offsetStgBCOFrameSize = (#const OFFSET_StgBCO_size) + (#size StgHeader)

offsetStgClosurePayload :: ByteOffset
offsetStgClosurePayload = (#const OFFSET_StgClosure_payload) + (#size StgHeader)

-- TODO: Should be SIZEOF_VOID_P
bytesInWord :: Int
bytesInWord = (#const SIZEOF_UNSIGNED_LONG)
#endif
