#include "MachDeps.h"
#include "Rts.h"
#include "RtsAPI.h"
#include "rts/Messages.h"
#include "rts/Types.h"
#include "rts/storage/ClosureMacros.h"
#include "rts/storage/Closures.h"
#include "rts/storage/TSO.h"
#include "stg/Types.h"
#include <stdlib.h>

extern void printStack(StgStack *stack);

// See rts/Threads.c
#define MIN_STACK_WORDS (RESERVED_STACK_WORDS + sizeofW(StgStopFrame) + 3)

// Copied from Cmm.h
#define SIZEOF_W SIZEOF_VOID_P
#define WDS(n) ((n)*SIZEOF_W)

// TODO: Try to remove UNTAG_CLOSURE. This should happen in the decoding logic.
void create_any_update_frame(Capability *cap, StgStack *stack, StgWord w) {
  StgUpdateFrame *updF = (StgUpdateFrame *)stack->sp;
  SET_HDR(updF, &stg_upd_frame_info, CCS_SYSTEM);
  StgClosure *payload = UNTAG_CLOSURE(rts_mkWord(cap, w));
  updF->updatee = payload;
}

void create_any_catch_frame(Capability *cap, StgStack *stack, StgWord w) {
  StgCatchFrame *catchF = (StgCatchFrame *)stack->sp;
  SET_HDR(catchF, &stg_catch_frame_info, CCS_SYSTEM);
  StgClosure *payload = UNTAG_CLOSURE(rts_mkWord(cap, w));
  catchF->exceptions_blocked = 1;
  catchF->handler = payload;
}

void create_any_catch_stm_frame(Capability *cap, StgStack *stack, StgWord w) {
  StgCatchSTMFrame *catchF = (StgCatchSTMFrame *)stack->sp;
  SET_HDR(catchF, &stg_catch_stm_frame_info, CCS_SYSTEM);
  StgClosure *payload1 = UNTAG_CLOSURE(rts_mkWord(cap, w));
  StgClosure *payload2 = UNTAG_CLOSURE(rts_mkWord(cap, w + 1));
  catchF->code = payload1;
  catchF->handler = payload2;
}

StgStack *setup(StgWord closureSizeWords, StgWord w,
                void (*f)(Capability *, StgStack *, StgWord)) {
  Capability *cap = rts_lock();
  StgWord totalSizeWords =
      sizeofW(StgStack) + closureSizeWords + MIN_STACK_WORDS;
  StgStack *stack = (StgStack *)allocate(cap, totalSizeWords);
  StgWord totalSizeBytes = WDS(totalSizeWords);
  SET_HDR(stack, &stg_upd_frame_info, CCS_SYSTEM);
  stack->stack_size = totalSizeBytes;
  stack->dirty = 0;
  stack->marking = 0;

  StgPtr spBottom = stack->stack + stack->stack_size;
  stack->sp = spBottom;
  stack->sp -= sizeofW(StgStopFrame);
  SET_HDR((StgClosure *)stack->sp, &stg_stop_thread_info, CCS_SYSTEM);
  stack->sp -= closureSizeWords;

  f(cap, stack, w);

  rts_unlock(cap);
  return stack;
}

StgStack *any_update_frame(StgWord w) {
  return setup(sizeofW(StgUpdateFrame), w, &create_any_update_frame);
}

StgStack *any_catch_frame(StgWord w) {
  return setup(sizeofW(StgCatchFrame), w, &create_any_catch_frame);
}

StgStack *any_catch_stm_frame(StgWord w) {
  return setup(sizeofW(StgCatchSTMFrame), w, &create_any_catch_stm_frame);
}
