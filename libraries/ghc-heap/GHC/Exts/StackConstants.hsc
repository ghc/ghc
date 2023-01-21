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

offsetStgCatchFrameHandler :: WordOffset
offsetStgCatchFrameHandler = byteOffsetToWordOffset $ (#const OFFSET_StgCatchFrame_handler) + (#size StgHeader)

offsetStgCatchFrameExceptionsBlocked :: WordOffset
offsetStgCatchFrameExceptionsBlocked = byteOffsetToWordOffset $ (#const OFFSET_StgCatchFrame_exceptions_blocked) + (#size StgHeader)

offsetStgCatchSTMFrameCode :: WordOffset
offsetStgCatchSTMFrameCode = byteOffsetToWordOffset $ (#const OFFSET_StgCatchSTMFrame_code) + (#size StgHeader)

offsetStgCatchSTMFrameHandler :: WordOffset
offsetStgCatchSTMFrameHandler = byteOffsetToWordOffset $ (#const OFFSET_StgCatchSTMFrame_handler) + (#size StgHeader)

offsetStgUpdateFrameUpdatee :: WordOffset
offsetStgUpdateFrameUpdatee = byteOffsetToWordOffset $ (#const OFFSET_StgUpdateFrame_updatee) + (#size StgHeader)

offsetStgAtomicallyFrameCode :: WordOffset
offsetStgAtomicallyFrameCode = byteOffsetToWordOffset $ (#const OFFSET_StgAtomicallyFrame_code) + (#size StgHeader)

offsetStgAtomicallyFrameResult :: WordOffset
offsetStgAtomicallyFrameResult = byteOffsetToWordOffset $ (#const OFFSET_StgAtomicallyFrame_result) + (#size StgHeader)

offsetStgCatchRetryFrameRunningAltCode :: WordOffset
offsetStgCatchRetryFrameRunningAltCode = byteOffsetToWordOffset $ (#const OFFSET_StgCatchRetryFrame_running_alt_code) + (#size StgHeader)

offsetStgCatchRetryFrameRunningFirstCode :: WordOffset
offsetStgCatchRetryFrameRunningFirstCode = byteOffsetToWordOffset $ (#const OFFSET_StgCatchRetryFrame_first_code) + (#size StgHeader)

offsetStgCatchRetryFrameAltCode :: WordOffset
offsetStgCatchRetryFrameAltCode = byteOffsetToWordOffset $ (#const OFFSET_StgCatchRetryFrame_alt_code) + (#size StgHeader)

offsetStgRetFunFrameSize :: WordOffset
-- StgRetFun has no header, but only a pointer to the info table at the beginning.
offsetStgRetFunFrameSize = byteOffsetToWordOffset $ (#const OFFSET_StgRetFun_size)

offsetStgRetFunFrameFun :: WordOffset
offsetStgRetFunFrameFun = byteOffsetToWordOffset $ (#const OFFSET_StgRetFun_fun)

offsetStgRetFunFramePayload :: WordOffset
offsetStgRetFunFramePayload = byteOffsetToWordOffset $ (#const OFFSET_StgRetFun_payload)

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

offsetStgClosurePayload :: WordOffset
offsetStgClosurePayload = byteOffsetToWordOffset $ (#const OFFSET_StgClosure_payload) + (#size StgHeader)

byteOffsetToWordOffset :: ByteOffset -> WordOffset
byteOffsetToWordOffset bo = if bo `mod` bytesInWord == 0 then
                              fromIntegral $ bo `div` bytesInWord
                            else
                              error "Unexpected struct alignment!"
  where
        bytesInWord = (#const SIZEOF_VOID_P)

#endif
