#include <string.h>

#include "Rts.h"
#include "rts/Messages.h"
#include "rts/storage/TSO.h"
#include "stg/Types.h"
#include "CloneStack.h"

#if defined(THREADED_RTS)

// ThreadId# in Haskell is a StgTSO* in RTS.
void sendCloneStackMessage(StgTSO *tso, HsStablePtr mvar) {
  Capability *srcCapability = rts_unsafeGetMyCapability();

  MessageCloneStack *msg;
  msg = (MessageCloneStack *)allocate(srcCapability, sizeofW(MessageCloneStack));
  msg->tso = tso;
  msg->result = (StgMVar*)deRefStablePtr(mvar);
  // TODO: Free? See RtsAPI.c: hs_try_putmvar()
  SET_HDR(msg, &stg_MSG_CLONE_STACK_info, CCS_SYSTEM);
  // Ensure that writes constructing Message are committed before sending.
  write_barrier();

  sendMessage(srcCapability, tso->cap, (Message *)msg);
}

void handleCloneStackMessage(MessageCloneStack *msg){
  StgStack* newStackClosure = cloneStack(msg->tso->cap, msg->tso->stackobj);

  bool putMVarWasSuccessful = performTryPutMVar(msg->tso->cap, msg->result, newStackClosure);

  if(!putMVarWasSuccessful) {
    barf("Can't put stack cloning result into MVar.");
  }
}

#else // !defined(THREADED_RTS)

void sendCloneStackMessage(StgTSO *tso, HsStablePtr mvar) {
  barf("Sending CloneStackMessages is only available in threaded RTS!");
}

#endif // end !defined(THREADED_RTS)

StgStack* cloneStack(Capability* capability, StgStack* stack){
#if defined(DEBUG)
  debugBelch("Stack to clone\n");
  printStack(stack);
#endif

  StgWord spOffset = stack->sp - stack->stack;
  StgWord closureSizeBytes = sizeof(StgStack) + (stack->stack_size * sizeof(StgWord));

  StgStack* newStackClosure = (StgStack*) allocate(capability, ROUNDUP_BYTES_TO_WDS(closureSizeBytes));

  memcpy(newStackClosure, stack, closureSizeBytes);

  newStackClosure->sp = newStackClosure->stack + spOffset;

#if defined(DEBUG)
  debugBelch("Cloned stack\n");
  printStack(newStackClosure);
  // TODO: Check sanity
#endif

  return newStackClosure;
}
