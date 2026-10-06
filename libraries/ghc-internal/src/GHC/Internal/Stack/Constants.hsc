{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GHC.Internal.Stack.Constants where

import GHC.Internal.Base
import GHC.Internal.Enum
import GHC.Internal.Err (error)
import GHC.Internal.Num
import GHC.Internal.Show
import GHC.Internal.Real
import GHC.Internal.Stack.Types as Rebindable

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
offsetStgCatchFrameHandler = byteOffsetToWordOffset $
  (#const OFFSET_StgCatchFrame_handler) + (#size StgFrameHeader)

sizeStgCatchFrame :: Int
sizeStgCatchFrame = bytesToWords $
  (#const SIZEOF_StgCatchFrame_NoHdr) + (#size StgFrameHeader)

offsetStgCatchSTMFrameCode :: WordOffset
offsetStgCatchSTMFrameCode = byteOffsetToWordOffset $
  (#const OFFSET_StgCatchSTMFrame_code) + (#size StgFrameHeader)

offsetStgCatchSTMFrameHandler :: WordOffset
offsetStgCatchSTMFrameHandler = byteOffsetToWordOffset $
  (#const OFFSET_StgCatchSTMFrame_handler) + (#size StgFrameHeader)

sizeStgCatchSTMFrame :: Int
sizeStgCatchSTMFrame = bytesToWords $
  (#const SIZEOF_StgCatchSTMFrame_NoHdr) + (#size StgFrameHeader)

offsetStgUpdateFrameUpdatee :: WordOffset
offsetStgUpdateFrameUpdatee = byteOffsetToWordOffset $
  (#const OFFSET_StgUpdateFrame_updatee) + (#size StgFrameHeader)

sizeStgUpdateFrame :: Int
sizeStgUpdateFrame = bytesToWords $
  (#const SIZEOF_StgUpdateFrame_NoHdr) + (#size StgFrameHeader)

offsetStgAtomicallyFrameCode :: WordOffset
offsetStgAtomicallyFrameCode = byteOffsetToWordOffset $
  (#const OFFSET_StgAtomicallyFrame_code) + (#size StgFrameHeader)

offsetStgAtomicallyFrameResult :: WordOffset
offsetStgAtomicallyFrameResult = byteOffsetToWordOffset $
  (#const OFFSET_StgAtomicallyFrame_result) + (#size StgFrameHeader)

sizeStgAtomicallyFrame :: Int
sizeStgAtomicallyFrame = bytesToWords $
  (#const SIZEOF_StgAtomicallyFrame_NoHdr) + (#size StgFrameHeader)

offsetStgCatchRetryFrameRunningAltCode :: WordOffset
offsetStgCatchRetryFrameRunningAltCode = byteOffsetToWordOffset $
  (#const OFFSET_StgCatchRetryFrame_running_alt_code) + (#size StgFrameHeader)

offsetStgCatchRetryFrameRunningFirstCode :: WordOffset
offsetStgCatchRetryFrameRunningFirstCode = byteOffsetToWordOffset $
  (#const OFFSET_StgCatchRetryFrame_first_code) + (#size StgFrameHeader)

offsetStgCatchRetryFrameAltCode :: WordOffset
offsetStgCatchRetryFrameAltCode = byteOffsetToWordOffset $
  (#const OFFSET_StgCatchRetryFrame_alt_code) + (#size StgFrameHeader)

sizeStgCatchRetryFrame :: Int
sizeStgCatchRetryFrame = bytesToWords $
  (#const SIZEOF_StgCatchRetryFrame_NoHdr) + (#size StgFrameHeader)

offsetStgRetFunFrameSize :: WordOffset
-- StgRetFun has no header, but only a pointer to the info table at the beginning
-- (preceded by the return-code address without tables-next-to-code); these
-- offsets are from the start of the struct.
offsetStgRetFunFrameSize = byteOffsetToWordOffset (#const OFFSET_StgRetFun_size)

offsetStgRetFunFrameFun :: WordOffset
offsetStgRetFunFrameFun = byteOffsetToWordOffset (#const OFFSET_StgRetFun_fun)

offsetStgRetFunFramePayload :: WordOffset
offsetStgRetFunFramePayload = byteOffsetToWordOffset (#const OFFSET_StgRetFun_payload)

sizeStgRetFunFrame :: Int
sizeStgRetFunFrame = bytesToWords (#const SIZEOF_StgRetFun)

sizeStgAnnFrame :: Int
sizeStgAnnFrame = bytesToWords $
  (#const SIZEOF_StgAnnFrame_NoHdr) + (#size StgFrameHeader)

offsetStgAnnFrameAnn :: WordOffset
offsetStgAnnFrameAnn = byteOffsetToWordOffset $
  (#const OFFSET_StgAnnFrame_ann) + (#size StgFrameHeader)

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

-- | Words from the start of a stack frame to its payload: the frame header
-- (two words, code and info pointer, without tables-next-to-code; see
-- StgFrameHeader in rts/include/rts/storage/Closures.h) plus the profiling
-- header if any. Use this, not 'offsetStgClosurePayload', for frames.
offsetStgFramePayload :: WordOffset
offsetStgFramePayload = byteOffsetToWordOffset $
  (#const OFFSET_StgClosure_payload) + (#size StgFrameHeader)

-- | Words in a stack frame header ('StgFrameHeader').
sizeStgFrameHeader :: Int
sizeStgFrameHeader = bytesToWords (#size StgFrameHeader)

offsetStgClosurePayload :: WordOffset
offsetStgClosurePayload = byteOffsetToWordOffset $
  (#const OFFSET_StgClosure_payload) + (#size StgHeader)

sizeStgClosure :: Int
sizeStgClosure = bytesToWords (#size StgHeader)

byteOffsetToWordOffset :: ByteOffset -> WordOffset
byteOffsetToWordOffset = WordOffset . bytesToWords . fromInteger . toInteger

bytesToWords :: Int -> Int
bytesToWords b =
  if b `mod` bytesInWord == 0 then
      fromIntegral $ b `div` bytesInWord
    else
      error "Unexpected struct alignment!"

bytesInWord :: Int
bytesInWord = (#const SIZEOF_VOID_P)

