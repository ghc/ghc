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

#if defined(DEBUG)
#include "sm/Sanity.h"
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
