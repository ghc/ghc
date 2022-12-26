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

// TODO: Use `w` for running_alt_code, too.
void create_any_catch_retry_frame(Capability *cap, StgStack *stack, StgWord w) {
  StgCatchRetryFrame *catchRF = (StgCatchRetryFrame *)stack->sp;
  SET_HDR(catchRF, &stg_catch_retry_frame_info, CCS_SYSTEM);
  StgClosure *payload1 = UNTAG_CLOSURE(rts_mkWord(cap, w));
  StgClosure *payload2 = UNTAG_CLOSURE(rts_mkWord(cap, w + 1));
  catchRF->running_alt_code = 1;
  catchRF->first_code = payload1;
  catchRF->alt_code = payload2;
}

void create_any_atomically_frame(Capability *cap, StgStack *stack, StgWord w) {
  StgAtomicallyFrame *aF = (StgAtomicallyFrame *)stack->sp;
  SET_HDR(aF, &stg_atomically_frame_info, CCS_SYSTEM);
  StgClosure *payload1 = UNTAG_CLOSURE(rts_mkWord(cap, w));
  StgClosure *payload2 = UNTAG_CLOSURE(rts_mkWord(cap, w + 1));
  aF->code = payload1;
  aF->result = payload2;
}

void create_any_ret_small_prim_frame(Capability *cap, StgStack *stack, StgWord w) {
  StgClosure *c = (StgClosure *)stack->sp;
  SET_HDR(c, &stg_ret_n_info, CCS_SYSTEM);
  // The cast is a lie (w is interpreted as plain Word, not as pointer), but the
  // memory layout fits.
  c->payload[0] = (StgClosure*) w;
}

void create_any_ret_small_closure_frame(Capability *cap, StgStack *stack, StgWord w) {
  StgClosure *c = (StgClosure *)stack->sp;
  SET_HDR(c, &stg_ret_p_info, CCS_SYSTEM);
  StgClosure *payload = UNTAG_CLOSURE(rts_mkWord(cap, w));
  c->payload[0] = payload;
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

StgStack *any_catch_retry_frame(StgWord w) {
  return setup(sizeofW(StgCatchRetryFrame), w, &create_any_catch_retry_frame);
}

StgStack *any_atomically_frame(StgWord w) {
  return setup(sizeofW(StgAtomicallyFrame), w, &create_any_atomically_frame);
}

StgStack *any_ret_small_prim_frame(StgWord w) {
  return setup(sizeofW(StgClosure) + sizeofW(StgWord), w, &create_any_ret_small_prim_frame);
}

StgStack *any_ret_small_closure_frame(StgWord w) {
  return setup(sizeofW(StgClosure) + sizeofW(StgClosurePtr), w, &create_any_ret_small_closure_frame);
}
