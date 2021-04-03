/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2020-2021
 *
 * Stack snapshotting and decoding. (Cloning and unwinding.)
 *
 *---------------------------------------------------------------------------*/

#include <string.h>

#include "Rts.h"
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
    if (frame->info == &stg_stack_underflow_frame_info) {
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
  SET_HDR(msg, &stg_MSG_CLONE_STACK_info, CCS_SYSTEM);
  // Ensure that writes constructing Message are committed before sending.
  write_barrier();

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

GNU_ATTRIBUTE(__noreturn__)
void sendCloneStackMessage(StgTSO *tso STG_UNUSED, HsStablePtr mvar STG_UNUSED) {
  barf("Sending CloneStackMessages is only available in threaded RTS!");
}

#endif // end !defined(THREADED_RTS)

// Creates a MutableArray# (Haskell representation) that contains a
// InfoProvEnt* for every stack frame on the given stack. Thus, the size of the
// array is the count of stack frames.
// Each InfoProvEnt* is looked up by lookupIPE(). If there's no IPE for a stack
// frame it's represented by null.
StgMutArrPtrs* decodeClonedStack(Capability *cap, StgStack* stack) {
  StgWord closureCount = getStackFrameCount(stack);

  StgMutArrPtrs* array = allocateMutableArray(closureCount);

  copyPtrsToArray(cap, array, stack);

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
    if (frame->info == &stg_stack_underflow_frame_info) {
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

// Allocate and initialize memory for a MutableArray# (Haskell representation).
StgMutArrPtrs* allocateMutableArray(StgWord closureCount) {
  // Idea stolen from PrimOps.cmm:stg_newArrayzh()
  StgWord size = closureCount + mutArrPtrsCardTableSize(closureCount);
  StgWord words = sizeofW(StgMutArrPtrs) + size;

  StgMutArrPtrs* array = (StgMutArrPtrs*) allocate(myTask()->cap, words);

  SET_HDR(array, &stg_MUT_ARR_PTRS_DIRTY_info, CCS_SYSTEM);
  array->ptrs  = closureCount;
  array->size = size;

  return array;
}


void copyPtrsToArray(Capability *cap, StgMutArrPtrs* arr, StgStack* stack) {
  StgWord index = 0;
  StgStack *last_stack = stack;
  while (true) {
    StgPtr sp = last_stack->sp;
    StgPtr spBottom = last_stack->stack + last_stack->stack_size;
    for (; sp < spBottom; sp += stack_frame_sizeW((StgClosure *)sp)) {
      const StgInfoTable* infoTable = get_itbl((StgClosure *)sp);

      // Add the IPE that was looked up by lookupIPE() to the MutableArray#.
      // The "Info Table Provernance Entry Map" (IPE) idea is to use a pointer
      // (address) to the info table to lookup entries, this is fulfilled in
      // non-"Tables Next to Code" builds.
      // When "Tables Next to Code" is used, the assembly label of the info table
      // is between the info table and it's code. There's no other label in the
      // assembly code which could be used instead, thus lookupIPE() is actually
      // called with the code pointer of the info table.
      // (As long as it's used consistently, this doesn't really matter - IPE uses
      // the pointer only to connect an info table to it's provenance entry in the
      // IPE map.)
#if defined(TABLES_NEXT_TO_CODE)
      InfoProvEnt* ipe = lookupIPE((StgInfoTable*) infoTable->code);
#else
      InfoProvEnt* ipe = lookupIPE(infoTable);
#endif
      arr->payload[index] = createPtrClosure(cap, ipe);

      index++;
    }

    // check whether the stack ends in an underflow frame
    StgUnderflowFrame *frame = (StgUnderflowFrame *) (last_stack->stack
      + last_stack->stack_size - sizeofW(StgUnderflowFrame));
    if (frame->info == &stg_stack_underflow_frame_info) {
      last_stack = frame->next_chunk;
    } else {
      break;
    }
  }
}

// Create a GHC.Ptr (Haskell constructor: `Ptr InfoProvEnt`) pointing to the
// IPE.
StgClosure* createPtrClosure(Capability *cap, InfoProvEnt* ipe) {
  StgClosure *p = (StgClosure *) allocate(cap, CONSTR_sizeW(0,1));
  SET_HDR(p, &base_GHCziPtr_Ptr_con_info, CCS_SYSTEM);
  p->payload[0] = (StgClosure*) ipe;
  return TAG_CLOSURE(1, p);
}
