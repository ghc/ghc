#include "MachDeps.h"
#include "Rts.h"
#include "RtsAPI.h"
#include "rts/Messages.h"
#include "rts/Types.h"
#include "rts/storage/ClosureMacros.h"
#include "rts/storage/Closures.h"
#include "stg/Types.h"
#include <stdlib.h>

extern void printStack(StgStack *stack);

// See rts/Threads.c
#define MIN_STACK_WORDS (RESERVED_STACK_WORDS + sizeofW(StgStopFrame) + 3)

// Copied from Cmm.h
#define SIZEOF_W SIZEOF_VOID_P
#define WDS(n) ((n)*SIZEOF_W)

StgStack *any_update_frame() {
  Capability *cap = rts_lock();
  StgWord closureSizeWords =
      sizeofW(StgStack) + sizeofW(StgUpdateFrame) + MIN_STACK_WORDS;
  StgStack *stack = (StgStack *)allocate(cap, closureSizeWords);
  StgWord closureSizeBytes = WDS(closureSizeWords);
  SET_HDR(stack, &stg_upd_frame_info, CCS_SYSTEM);
  stack->stack_size = closureSizeBytes;
  stack->dirty = 0;
  stack->marking = 0;

  StgPtr spBottom = stack->stack + stack->stack_size;
  stack->sp = spBottom;
  stack->sp -= sizeofW(StgStopFrame);
  SET_HDR((StgClosure *)stack->sp, &stg_stop_thread_info, CCS_SYSTEM);

  stack->sp -= sizeofW(StgUpdateFrame);
  StgUpdateFrame *updF = (StgUpdateFrame *)stack->sp;
  SET_HDR(updF, &stg_upd_frame_info, CCS_SYSTEM);
  StgClosure *payload = UNTAG_CLOSURE(rts_mkWord(cap, 42));
  updF->updatee = payload;
  rts_unlock(cap);
  printStack(stack);
  return stack;
}
