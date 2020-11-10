{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module GHC.Exts.Heap.FFIClosures_ProfilingDisabled where

-- Manually undefining PROFILING gives the #peek and #poke macros an accurate
-- representation of the C structures when hsc2hs runs. This is valid because
-- a non-profiling build would use
-- GHC.Exts.Heap.FFIClosures_ProfilingEnabled.
#undef PROFILING
#include "Rts.h"

import Prelude
import Foreign
import GHC.Exts
import GHC.Exts.Heap.ProfInfo.PeekProfInfo_ProfilingEnabled
import GHC.Exts.Heap.ProfInfo.Types
import GHC.Exts.Heap.Closures(WhatNext(..), WhyBlocked(..), TsoFlags(..), Box(..))
import GHC.Exts.Heap.ClosureTypes
import GHC.Exts.Heap.InfoTable
import GHC.Exts.Heap.Constants

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
peekTSOFields :: Ptr tsoPtr -> IO TSOFields
peekTSOFields ptr = do
    what_next' <- (#peek struct StgTSO_, what_next) ptr
    why_blocked' <- (#peek struct StgTSO_, why_blocked) ptr
    flags' <- (#peek struct StgTSO_, flags) ptr
    threadId' <- (#peek struct StgTSO_, id) ptr
    saved_errno' <- (#peek struct StgTSO_, saved_errno) ptr
    dirty' <- (#peek struct StgTSO_, dirty) ptr
    alloc_limit' <- (#peek struct StgTSO_, alloc_limit) ptr
    tot_stack_size' <- (#peek struct StgTSO_, tot_stack_size) ptr
    tso_prof' <- peekStgTSOProfInfo ptr

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
#if __GLASGOW_HASKELL__ >= 810
                        (#const BlockedOnIOCompletion) -> BlockedOnIOCompletion
#endif
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
#if __GLASGOW_HASKELL__ >= 810
    stack_marking :: Word8,
#endif
    stack_sp :: Ptr (),
    stack_stack :: Ptr ()
} deriving Show

-- | Get non-closure fields from @StgStack_@ (@TSO.h@)
peekStackFields :: Ptr a -> IO StackFields
peekStackFields ptr = do
    stack_size' <- (#peek struct StgStack_, stack_size) ptr ::IO Word32
    dirty' <- (#peek struct StgStack_, dirty) ptr
#if __GLASGOW_HASKELL__ >= 810
    marking' <- (#peek struct StgStack_, marking) ptr
#endif
    Ptr sp' <- (#peek struct StgStack_, sp) ptr
    Ptr stack <- (#peek struct StgStack_, stack) ptr

    -- TODO decode the stack.

    return StackFields {
        stack_size = stack_size',
        stack_dirty = dirty',
#if __GLASGOW_HASKELL__ >= 810
        stack_marking = marking',
#endif
        stack_sp = Ptr sp',
        stack_stack = Ptr stack
    }


data GenStackFrame c = StackFrame StgStackInfoTable (GenStackPayload c) deriving Show

type StackFrame = GenStackFrame Box

data GenStackPayload c = StackPayload [PointerOrData c] deriving Show

peekStack :: Ptr a -> IO [StackFrame]
peekStack p = do
  print p
  fields <- peekStackFields p
  print fields
  print (startStack p)
  let end = (startStack p `plusPtr` (8 * (fromIntegral $ stack_size fields)))
  print end
  print (stack_sp fields >= end)
  print (stack_sp fields `minusPtr` end)
  stackWorker (castPtr p) (stack_sp fields) end

startStack = (#ptr StgStack, stack)

--peekStackFrame :: Ptr a -> IO StackFrame
--peekStackFrame = _

stackWorker :: Ptr () -> Ptr a -> Ptr a -> IO [StackFrame]
stackWorker c stackStart stackEnd
  | stackStart >= stackEnd = return []
  | otherwise = do
      print ("start", stackStart)
      print ("togo", stackStart `minusPtr` stackEnd)
      -- StgRetInfoTable
      itblPtr <- (#peek struct StgClosure_, header.info) stackStart
      print ("itblPtr", itblPtr `plusPtr` (2 * negate wORD_SIZE))

      let itblPtr' = itblPtr `plusPtr` (2 * negate wORD_SIZE)
      (ty :: HalfWord) <- ((#peek StgRetInfoTable, i.type) itblPtr')
      print ty
      itbl <- peekStackItbl ((#ptr StgRetInfoTable, i) itblPtr')
      print ("itbl", itbl)
      (ps, next) <-
        case tipe itbl of
          STOP_FRAME -> small_bitmap stackStart itbl
          CATCH_FRAME -> small_bitmap stackStart itbl
          CATCH_STM_FRAME -> small_bitmap stackStart itbl
          CATCH_RETRY_FRAME -> small_bitmap stackStart itbl
          ATOMICALLY_FRAME -> small_bitmap stackStart itbl
          RET_SMALL -> small_bitmap stackStart itbl
          _ -> getLine >> undefined
      print ps
      print next
      more_frames <- stackWorker c next stackEnd
      return $ (StackFrame itbl (StackPayload ps)) : more_frames
  where
    small_bitmap start itbl = do
      let BM pords = layout itbl
      collectPointers (start `plusPtr` wORD_SIZE) pords

    collectPointers :: Ptr a -> [PointerOrData ()] -> IO ([PointerOrData Box], Ptr a)
    collectPointers p [] = return ([], p)
    collectPointers p (pord:pords) = do
      (pord', p') <- collectPointer p pord
      (xs, p'') <- collectPointers p' pords
      return $ (pord' : xs, p'')

    collectPointer :: Ptr a -> PointerOrData () -> IO (PointerOrData Box, Ptr a)
    collectPointer p pord = do
      pord' <- traverse (const (pointerPtrToBox (castPtr p))) pord
      return (pord', p `plusPtr` (wORD_SIZE))


    pointerPtrToBox :: Ptr (Ptr a) -> IO Box
    pointerPtrToBox p = do
      Ptr p' <- peek p
      return (Box (unsafeCoerce## p'))

