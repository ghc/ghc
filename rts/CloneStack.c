/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2020-2021
 *
 * Stack snapshotting and decoding. (Cloning and unwinding.)
 *
 *---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsFlags.h"
#include "rts/Messages.h"
#include "Messages.h"
#include "rts/Types.h"
#include "rts/storage/TSO.h"
#include "stg/Types.h"
#include "CloneStack.h"
#include "StablePtr.h"
#include "Threads.h"
#include "Prelude.h"

#if defined(DEBUG)
#include "sm/Sanity.h"
#include "Printer.h"
#endif

#include <string.h>


static StgWord getStackFrameCount(StgStack* stack);
static StgWord getStackChunkClosureCount(StgStack* stack);
static StgArrBytes* allocateByteArray(Capability *cap, StgWord bytes);
static void copyPtrsToArray(StgArrBytes* arr, StgStack* stack);

static StgStack* cloneStackChunk(Capability* capability, const StgStack* stack)
{
  StgWord spOffset = stack->sp - stack->stack;
  StgWord closureSizeBytes = sizeof(StgStack) + (stack->stack_size * sizeof(StgWord));

  StgStack* newStackClosure = (StgStack*) allocate(capability, ROUNDUP_BYTES_TO_WDS(closureSizeBytes));

  memcpy(newStackClosure, stack, closureSizeBytes);

  newStackClosure->sp = newStackClosure->stack + spOffset;
  // The new stack is not on the mutable list; clear the dirty flag such that
  // we don't claim that it is.
  newStackClosure->dirty = 0;

#if defined(DEBUG)
  checkClosure((StgClosure*) newStackClosure);
#endif

  return newStackClosure;
}

StgStack* cloneStack(Capability* capability, const StgStack* stack)
{
  StgStack *top_stack = cloneStackChunk(capability, stack);
  StgStack *last_stack = top_stack;
  while (true) {
    // check whether the stack ends in an underflow frame
    StgUnderflowFrame *frame = (StgUnderflowFrame *) (last_stack->stack
      + last_stack->stack_size - sizeofW(StgUnderflowFrame));
    if (frame->info == &stg_stack_underflow_frame_d_info
      ||frame->info == &stg_stack_underflow_frame_v16_info
      ||frame->info == &stg_stack_underflow_frame_v32_info
      ||frame->info == &stg_stack_underflow_frame_v64_info) {
      StgStack *s = cloneStackChunk(capability, frame->next_chunk);
      frame->next_chunk = s;
      last_stack = s;
    } else {
      break;
    }
  }
  return top_stack;
}

#if defined(THREADED_RTS)

// ThreadId# in Haskell is a StgTSO* in RTS.
void sendCloneStackMessage(StgTSO *tso, HsStablePtr mvar) {
  Capability *srcCapability = rts_unsafeGetMyCapability();

  MessageCloneStack *msg;
  msg = (MessageCloneStack *)allocate(srcCapability, sizeofW(MessageCloneStack));
  msg->tso = tso;
  msg->result = (StgMVar*)deRefStablePtr(mvar);
  SET_HDR_RELEASE(msg, &stg_MSG_CLONE_STACK_info, CCS_SYSTEM);

  sendMessage(srcCapability, tso->cap, (Message *)msg);
}

void handleCloneStackMessage(MessageCloneStack *msg){
  StgStack* newStackClosure = cloneStack(msg->tso->cap, msg->tso->stackobj);

  // Lift StackSnapshot# to StackSnapshot by applying it's constructor.
  // This is necessary because performTryPutMVar() puts the closure onto the
  // stack for evaluation and stacks can not be evaluated (entered).
  HaskellObj result = rts_apply(msg->tso->cap, StackSnapshot_constructor_closure, (HaskellObj) newStackClosure);

  bool putMVarWasSuccessful = performTryPutMVar(msg->tso->cap, msg->result, result);

  if(!putMVarWasSuccessful) {
    barf("Can't put stack cloning result into MVar.");
  }
}

#else // !defined(THREADED_RTS)

STG_NORETURN
void sendCloneStackMessage(StgTSO *tso STG_UNUSED, HsStablePtr mvar STG_UNUSED) {
  barf("Sending CloneStackMessages is only available in threaded RTS!");
}

#endif // end !defined(THREADED_RTS)

// Creates a MutableArray# (Haskell representation) that contains a
// InfoProvEnt* for every stack frame on the given stack. Thus, the size of the
// array is the count of stack frames.
// Each InfoProvEnt* is looked up by lookupIPE(). If there's no IPE for a stack
// frame it's represented by null.
StgArrBytes* decodeClonedStack(Capability *cap, StgStack* stack) {
  StgWord closureCount = getStackFrameCount(stack);

  StgArrBytes* array = allocateByteArray(cap, sizeof(StgInfoTable*) * closureCount);

  copyPtrsToArray(array, stack);

  return array;
}

// Count the stack frames that are on the given stack.
// This is the sum of all stack frames in all stack chunks of this stack.
StgWord getStackFrameCount(StgStack* stack) {
  StgWord closureCount = 0;
  StgStack *last_stack = stack;
  while (true) {
    closureCount += getStackChunkClosureCount(last_stack);

    // check whether the stack ends in an underflow frame
    StgUnderflowFrame *frame = (StgUnderflowFrame *) (last_stack->stack
      + last_stack->stack_size - sizeofW(StgUnderflowFrame));
    if (frame->info == &stg_stack_underflow_frame_d_info
      ||frame->info == &stg_stack_underflow_frame_v16_info
      ||frame->info == &stg_stack_underflow_frame_v32_info
      ||frame->info == &stg_stack_underflow_frame_v64_info) {
      last_stack = frame->next_chunk;
    } else {
      break;
    }
  }
  return closureCount;
}

StgWord getStackChunkClosureCount(StgStack* stack) {
    StgWord closureCount = 0;
    StgPtr sp = stack->sp;
    StgPtr spBottom = stack->stack + stack->stack_size;
    for (; sp < spBottom; sp += stack_frame_sizeW((StgClosure *)sp)) {
      closureCount++;
    }

    return closureCount;
}

// Allocate and initialize memory for a ByteArray# (Haskell representation).
StgArrBytes* allocateByteArray(Capability *cap, StgWord bytes) {
  // Idea stolen from PrimOps.cmm:stg_newArrayzh()
  StgWord words = sizeofW(StgArrBytes) + bytes;

  StgArrBytes* array = (StgArrBytes*) allocate(cap, words);

  SET_HDR(array, &stg_ARR_WORDS_info, CCS_SYSTEM);
  array->bytes  = bytes;
  return array;
}

static void copyPtrsToArray(StgArrBytes* arr, StgStack* stack) {
  StgWord index = 0;
  StgStack *last_stack = stack;
  const StgInfoTable **result = (const StgInfoTable **) arr->payload;
  while (true) {
    StgPtr sp = last_stack->sp;
    StgPtr spBottom = last_stack->stack + last_stack->stack_size;
    for (; sp < spBottom; sp += stack_frame_sizeW((StgClosure *)sp)) {
      const StgInfoTable* infoTable = ((StgClosure *)sp)->header.info;
      result[index] = infoTable;
      index++;
    }

    // Ensure that we didn't overflow the result array
    ASSERT(index-1 < arr->bytes / sizeof(StgInfoTable*));

    // check whether the stack ends in an underflow frame
    StgUnderflowFrame *frame = (StgUnderflowFrame *) (last_stack->stack
      + last_stack->stack_size - sizeofW(StgUnderflowFrame));
    if (frame->info == &stg_stack_underflow_frame_d_info
      ||frame->info == &stg_stack_underflow_frame_v16_info
      ||frame->info == &stg_stack_underflow_frame_v32_info
      ||frame->info == &stg_stack_underflow_frame_v64_info) {
      last_stack = frame->next_chunk;
    } else {
      break;
    }
  }
}
