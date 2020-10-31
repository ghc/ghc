/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001-2021
 *
 * Stack snapshotting.
 */

#include <string.h>

#include "Rts.h"
#include "rts/storage/TSO.h"
#include "stg/Types.h"
#include "CloneStack.h"
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
