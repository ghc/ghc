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

StgStack* cloneStack(Capability* capability, StgStack* stack){
  StgWord spOffset = stack->sp - stack->stack;
  StgWord closureSizeBytes = sizeof(StgStack) + (stack->stack_size * sizeof(StgWord));

  StgStack* newStackClosure = (StgStack*) allocate(capability, ROUNDUP_BYTES_TO_WDS(closureSizeBytes));

  memcpy(newStackClosure, stack, closureSizeBytes);

  newStackClosure->sp = newStackClosure->stack + spOffset;

#if defined(DEBUG)
  checkClosure((StgClosure*) newStackClosure);
#endif

  return newStackClosure;
}
