{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module GHC.Exts.Heap.FFIClosures_ProfilingEnabled where

-- See [hsc and CPP workaround]

#define PROFILING
#include "Rts.h"

import Prelude
import Foreign
import GHC.Exts
import GHC.Exts.Heap.ProfInfo.PeekProfInfo
import GHC.Exts.Heap.ProfInfo.Types
import GHC.Exts.Heap.Closures(WhatNext(..), WhyBlocked(..), TsoFlags(..))

data TSOFields = TSOFields {
    tso_what_next :: WhatNext,
    tso_why_blocked :: WhyBlocked,
    tso_flags :: [TsoFlags],
-- Unfortunately block_info is a union without clear discriminator.
--    block_info :: TDB,
    tso_threadId :: Word64,
    tso_saved_errno :: Word32,
    tso_dirty:: Word32,
    tso_alloc_limit :: Int64,
    tso_tot_stack_size :: Word32,
    tso_prof :: Maybe StgTSOProfInfo
}

-- | Get non-pointer fields from @StgTSO_@ (@TSO.h@)
peekTSOFields :: (Ptr a -> IO (Maybe CostCentreStack)) -> Ptr tsoPtr -> IO TSOFields
peekTSOFields decodeCCS ptr = do
    what_next' <- (#peek struct StgTSO_, what_next) ptr
    why_blocked' <- (#peek struct StgTSO_, why_blocked) ptr
    flags' <- (#peek struct StgTSO_, flags) ptr
    threadId' <- (#peek struct StgTSO_, id) ptr
    saved_errno' <- (#peek struct StgTSO_, saved_errno) ptr
    dirty' <- (#peek struct StgTSO_, dirty) ptr
    alloc_limit' <- (#peek struct StgTSO_, alloc_limit) ptr
    tot_stack_size' <- (#peek struct StgTSO_, tot_stack_size) ptr
    tso_prof' <- peekStgTSOProfInfo decodeCCS ptr

    return TSOFields {
        tso_what_next = parseWhatNext what_next',
        tso_why_blocked = parseWhyBlocked why_blocked',
        tso_flags = parseTsoFlags flags',
        tso_threadId = threadId',
        tso_saved_errno = saved_errno',
        tso_dirty = dirty',
        tso_alloc_limit = alloc_limit',
        tso_tot_stack_size = tot_stack_size',
        tso_prof = tso_prof'
    }

parseWhatNext :: Word16 -> WhatNext
parseWhatNext w = case w of
                    (#const ThreadRunGHC) -> ThreadRunGHC
                    (#const ThreadInterpret) -> ThreadInterpret
                    (#const ThreadKilled) -> ThreadKilled
                    (#const ThreadComplete) -> ThreadComplete
                    _ -> WhatNextUnknownValue w

parseWhyBlocked :: Word16 -> WhyBlocked
parseWhyBlocked w = case w of
                        (#const NotBlocked) -> NotBlocked
                        (#const BlockedOnMVar) -> BlockedOnMVar
                        (#const BlockedOnMVarRead) -> BlockedOnMVarRead
                        (#const BlockedOnBlackHole) -> BlockedOnBlackHole
                        (#const BlockedOnRead) -> BlockedOnRead
                        (#const BlockedOnWrite) -> BlockedOnWrite
                        (#const BlockedOnDelay) -> BlockedOnDelay
                        (#const BlockedOnSTM) -> BlockedOnSTM
                        (#const BlockedOnDoProc) -> BlockedOnDoProc
                        (#const BlockedOnCCall) -> BlockedOnCCall
                        (#const BlockedOnCCall_Interruptible) -> BlockedOnCCall_Interruptible
                        (#const BlockedOnMsgThrowTo) -> BlockedOnMsgThrowTo
                        (#const ThreadMigrating) -> ThreadMigrating
                        _ -> WhyBlockedUnknownValue w

parseTsoFlags :: Word32 -> [TsoFlags]
parseTsoFlags w | isSet (#const TSO_LOCKED) w = TsoLocked : parseTsoFlags (unset (#const TSO_LOCKED) w)
                | isSet (#const TSO_BLOCKEX) w = TsoBlockx : parseTsoFlags (unset (#const TSO_BLOCKEX) w)
                | isSet (#const TSO_INTERRUPTIBLE) w = TsoInterruptible : parseTsoFlags (unset (#const TSO_INTERRUPTIBLE) w)
                | isSet (#const TSO_STOPPED_ON_BREAKPOINT) w = TsoStoppedOnBreakpoint : parseTsoFlags (unset (#const TSO_STOPPED_ON_BREAKPOINT) w)
                | isSet (#const TSO_MARKED) w = TsoMarked : parseTsoFlags (unset (#const TSO_MARKED) w)
                | isSet (#const TSO_SQUEEZED) w = TsoSqueezed : parseTsoFlags (unset (#const TSO_SQUEEZED) w)
                | isSet (#const TSO_ALLOC_LIMIT) w = TsoAllocLimit : parseTsoFlags (unset (#const TSO_ALLOC_LIMIT) w)
parseTsoFlags 0 = []
parseTsoFlags w = [TsoFlagsUnknownValue w]

isSet :: Word32 -> Word32 -> Bool
isSet bitMask w = w .&. bitMask /= 0

unset :: Word32 -> Word32 -> Word32
unset bitMask w = w `xor` bitMask

data StackFields = StackFields {
    stack_size :: Word32,
    stack_dirty :: Word8,
#if __GLASGOW_HASKELL__ >= 811
    stack_marking :: Word8,
#endif
    stack_sp :: Addr##
}

-- | Get non-closure fields from @StgStack_@ (@TSO.h@)
peekStackFields :: Ptr a -> IO StackFields
peekStackFields ptr = do
    stack_size' <- (#peek struct StgStack_, stack_size) ptr ::IO Word32
    dirty' <- (#peek struct StgStack_, dirty) ptr
#if __GLASGOW_HASKELL__ >= 811
    marking' <- (#peek struct StgStack_, marking) ptr
#endif
    Ptr sp' <- (#peek struct StgStack_, sp) ptr

    -- TODO decode the stack.

    return StackFields {
        stack_size = stack_size',
        stack_dirty = dirty',
#if __GLASGOW_HASKELL__ >= 811
        stack_marking = marking',
#endif
        stack_sp = sp'
    }
