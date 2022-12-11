#include "Rts.h"
#include "RtsAPI.h"
#include "rts/Messages.h"
#include "rts/Types.h"
#include "rts/storage/ClosureMacros.h"
#include "rts/storage/Closures.h"
#include "stg/Types.h"
#include <stdlib.h>

StgStack *update_frame() {
  Capability *cap = rts_lock();
  StgWord closureSizeBytes = sizeof(StgStack) + sizeof(StgStopFrame) + sizeof(StgUpdateFrame);
  StgStack *stack = (StgStack*) allocate(cap, ROUNDUP_BYTES_TO_WDS(closureSizeBytes));
  SET_HDR(stack, &, CCS_SYSTEM);
  stack->stack_size = closureSizeBytes;
  stack->dirty = 0;
  stack->marking = 0;
  rts_unlock(cap);
  return stack;
}
