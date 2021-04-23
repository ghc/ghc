/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001-2021
 *
 * Stack snapshotting.
 */

#include <string.h>

#include "Rts.h"
#include "rts/Messages.h"
#include "Messages.h"
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
    StgPtr top = last_stack->stack + last_stack->stack_size;
    StgUnderflowFrame *underFlowFrame = ((StgUnderflowFrame *) top);
    StgUnderflowFrame *frame = underFlowFrame--;
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
  freeStablePtr(mvar);
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

// From Cmm.h
#define mutArrCardMask ((1 << MUT_ARR_PTRS_CARD_BITS) - 1)
#define mutArrPtrCardDown(i) ((i) >> MUT_ARR_PTRS_CARD_BITS)
#define mutArrPtrCardUp(i)   (((i) + mutArrCardMask) >> MUT_ARR_PTRS_CARD_BITS)
#define mutArrPtrsCardWords(n) ROUNDUP_BYTES_TO_WDS(mutArrPtrCardUp(n))
#define BYTES_TO_WDS(n) ((n) / sizeof(StgWord))

StgMutArrPtrs* decodeClonedStack(StgStack* stack){
#if defined(DEBUG)
  printStack(stack);
#endif

  StgWord closureCount = getStackClosureCount(stack);

  // Stolen from PrimOps.cmm:stg_newArrayzh()
  StgWord size = closureCount + mutArrPtrsCardWords(closureCount);
  StgWord words = BYTES_TO_WDS(sizeof(StgMutArrPtrs)) + size;

  StgMutArrPtrs* arr = (StgMutArrPtrs*) allocate(myTask()->cap, words);

  SET_HDR(arr, &stg_MUT_ARR_PTRS_DIRTY_info, CCS_SYSTEM);
  arr->ptrs  = closureCount;
  arr->size = size;

  copyPtrsToArray(arr, stack);

  return arr;
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

StgWord getStackClosureCount(StgStack* stack) {
  StgWord closureCount = 0;
  StgStack *last_stack = stack;
  while (true) {
    closureCount += getStackChunkClosureCount(last_stack);

    // check whether the stack ends in an underflow frame
    StgPtr top = last_stack->stack + last_stack->stack_size;
    StgUnderflowFrame *underFlowFrame = ((StgUnderflowFrame *) top);
    StgUnderflowFrame *frame = underFlowFrame--;
    if (frame->info == &stg_stack_underflow_frame_info) {
      last_stack = frame->next_chunk;
    } else {
      break;
    }
  }
  return closureCount;
}

void copyPtrsToArray(StgMutArrPtrs* arr, StgStack* stack) {
  StgWord index = 0;
  StgStack *last_stack = stack;
  while (true) {
    StgPtr sp = last_stack->sp;
    StgPtr spBottom = last_stack->stack + last_stack->stack_size;
    for (; sp < spBottom; sp += stack_frame_sizeW((StgClosure *)sp)) {
      const StgInfoTable* infoTable = get_itbl((StgClosure *)sp);
      // TODO: Explain why it's infoTable->code
      arr->payload[index] = createWordClosure(myTask()->cap, (StgAddr) infoTable->code);
      index++;
    }

    // check whether the stack ends in an underflow frame
    StgPtr top = last_stack->stack + last_stack->stack_size;
    StgUnderflowFrame *underFlowFrame = ((StgUnderflowFrame *) top);
    StgUnderflowFrame *frame = underFlowFrame--;
    if (frame->info == &stg_stack_underflow_frame_info) {
      last_stack = frame->next_chunk;
    } else {
      break;
    }
  }
}

// TODO: Should use something better than a word for pointers
StgClosure* createWordClosure (Capability *cap, StgAddr i) {
  StgClosure *p = (StgClosure *) allocate(cap, CONSTR_sizeW(0,1));
  SET_HDR(p, &base_GHCziPtr_Ptr_con_info, CCS_SYSTEM);
  *(StgWord *) p->payload = i;
  return TAG_CLOSURE(1, p);
}
