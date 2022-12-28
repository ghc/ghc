#include "MachDeps.h"
#include "Rts.h"
#include "RtsAPI.h"
#include "alloca.h"
#include "rts/Messages.h"
#include "rts/Types.h"
#include "rts/storage/ClosureMacros.h"
#include "rts/storage/Closures.h"
#include "rts/storage/InfoTables.h"
#include "rts/storage/TSO.h"
#include "stg/Types.h"

extern void printStack(StgStack *stack);

// See rts/Threads.c
#define MIN_STACK_WORDS (RESERVED_STACK_WORDS + sizeofW(StgStopFrame) + 3)

// Copied from Cmm.h
#define SIZEOF_W SIZEOF_VOID_P
#define WDS(n) ((n)*SIZEOF_W)

// Update frames are interpreted by the garbage collector. We play it some
// tricks here with a fake blackhole.
RTS_RET(test_fake_blackhole);
void create_any_update_frame(Capability *cap, StgStack *stack, StgWord w) {
  StgUpdateFrame *updF = (StgUpdateFrame *)stack->sp;
  SET_HDR(updF, &stg_upd_frame_info, CCS_SYSTEM);
  // StgInd and a BLACKHOLE have the same structure
  StgInd *blackhole = allocate(cap, sizeofW(StgInd));
  SET_HDR(blackhole, &test_fake_blackhole_info, CCS_SYSTEM);
  StgClosure *payload = rts_mkWord(cap, w);
  blackhole->indirectee = payload;
  updF->updatee = (StgClosure *)blackhole;
}

void create_any_catch_frame(Capability *cap, StgStack *stack, StgWord w) {
  StgCatchFrame *catchF = (StgCatchFrame *)stack->sp;
  SET_HDR(catchF, &stg_catch_frame_info, CCS_SYSTEM);
  StgClosure *payload = rts_mkWord(cap, w);
  catchF->exceptions_blocked = 1;
  catchF->handler = payload;
}

void create_any_catch_stm_frame(Capability *cap, StgStack *stack, StgWord w) {
  StgCatchSTMFrame *catchF = (StgCatchSTMFrame *)stack->sp;
  SET_HDR(catchF, &stg_catch_stm_frame_info, CCS_SYSTEM);
  StgClosure *payload1 = rts_mkWord(cap, w);
  StgClosure *payload2 = rts_mkWord(cap, w + 1);
  catchF->code = payload1;
  catchF->handler = payload2;
}

// TODO: Use `w` for running_alt_code, too.
void create_any_catch_retry_frame(Capability *cap, StgStack *stack, StgWord w) {
  StgCatchRetryFrame *catchRF = (StgCatchRetryFrame *)stack->sp;
  SET_HDR(catchRF, &stg_catch_retry_frame_info, CCS_SYSTEM);
  StgClosure *payload1 = rts_mkWord(cap, w);
  StgClosure *payload2 = rts_mkWord(cap, w + 1);
  catchRF->running_alt_code = 1;
  catchRF->first_code = payload1;
  catchRF->alt_code = payload2;
}

void create_any_atomically_frame(Capability *cap, StgStack *stack, StgWord w) {
  StgAtomicallyFrame *aF = (StgAtomicallyFrame *)stack->sp;
  SET_HDR(aF, &stg_atomically_frame_info, CCS_SYSTEM);
  StgClosure *payload1 = rts_mkWord(cap, w);
  StgClosure *payload2 = rts_mkWord(cap, w + 1);
  aF->code = payload1;
  aF->result = payload2;
}

void create_any_ret_small_prim_frame(Capability *cap, StgStack *stack,
                                     StgWord w) {
  StgClosure *c = (StgClosure *)stack->sp;
  SET_HDR(c, &stg_ret_n_info, CCS_SYSTEM);
  // The cast is a lie (w is interpreted as plain Word, not as pointer), but the
  // memory layout fits.
  c->payload[0] = (StgClosure *)w;
}

void create_any_ret_small_closure_frame(Capability *cap, StgStack *stack,
                                        StgWord w) {
  StgClosure *c = (StgClosure *)stack->sp;
  SET_HDR(c, &stg_ret_p_info, CCS_SYSTEM);
  StgClosure *payload = rts_mkWord(cap, w);
  c->payload[0] = payload;
}

#define MAX_SMALL_BITMAP_BITS (BITS_IN(W_) - BITMAP_BITS_SHIFT)

StgWord maxSmallBitmapBits() { return MAX_SMALL_BITMAP_BITS; }

RTS_RET(test_small_ret_full_p);
void create_any_ret_small_closures_frame(Capability *cap, StgStack *stack,
                                         StgWord w) {
  StgClosure *c = (StgClosure *)stack->sp;
  SET_HDR(c, &test_small_ret_full_p_info, CCS_SYSTEM);
  for (int i = 0; i < MAX_SMALL_BITMAP_BITS; i++) {
    StgClosure *payload1 = UNTAG_CLOSURE(rts_mkWord(cap, w));
    w++;
    c->payload[i] = payload1;
  }
}

RTS_RET(test_small_ret_full_n);
void create_any_ret_small_prims_frame(Capability *cap, StgStack *stack,
                                      StgWord w) {
  StgClosure *c = (StgClosure *)stack->sp;
  SET_HDR(c, &test_small_ret_full_n_info, CCS_SYSTEM);
  for (int i = 0; i < MAX_SMALL_BITMAP_BITS; i++) {
    c->payload[i] = (StgClosure *)w;
    w++;
  }
}

#define MIN_LARGE_BITMAP_BITS (MAX_SMALL_BITMAP_BITS + 1)

RTS_RET(test_big_ret_min_n);
void create_any_ret_big_prims_min_frame(Capability *cap, StgStack *stack,
                                        StgWord w) {
  StgClosure *c = (StgClosure *)stack->sp;
  SET_HDR(c, &test_big_ret_min_n_info, CCS_SYSTEM);

  for (int i = 0; i < MIN_LARGE_BITMAP_BITS; i++) {
    c->payload[i] = (StgClosure *)w;
    w++;
  }
}

RTS_RET(test_big_ret_min_p);
void create_any_ret_big_closures_min_frame(Capability *cap, StgStack *stack,
                                           StgWord w) {
  StgClosure *c = (StgClosure *)stack->sp;
  SET_HDR(c, &test_big_ret_min_p_info, CCS_SYSTEM);

  for (int i = 0; i < MIN_LARGE_BITMAP_BITS; i++) {
    c->payload[i] = UNTAG_CLOSURE(rts_mkWord(cap, w));
    w++;
  }
}

void checkSTACK(StgStack *stack);
StgStack *setup(Capability *cap, StgWord closureSizeWords, StgWord w,
                void (*f)(Capability *, StgStack *, StgWord)) {
  StgWord totalSizeWords =
      sizeofW(StgStack) + closureSizeWords + MIN_STACK_WORDS;
  StgStack *stack = (StgStack *)allocate(cap, totalSizeWords);
  StgWord totalSizeBytes = WDS(totalSizeWords);
  SET_HDR(stack, &stg_STACK_info, CCS_SYSTEM);
  stack->stack_size = totalSizeBytes;
  stack->dirty = 0;
  stack->marking = 0;

  StgPtr spBottom = stack->stack + stack->stack_size;
  stack->sp = spBottom;
  stack->sp -= sizeofW(StgStopFrame);
  SET_HDR((StgClosure *)stack->sp, &stg_stop_thread_info, CCS_SYSTEM);
  stack->sp -= closureSizeWords;

  f(cap, stack, w);

  checkSTACK(stack);
  return stack;
}

StgStack *any_update_frame(Capability *cap, StgWord w) {
  return setup(cap, sizeofW(StgUpdateFrame), w, &create_any_update_frame);
}

StgStack *any_catch_frame(Capability *cap, StgWord w) {
  return setup(cap, sizeofW(StgCatchFrame), w, &create_any_catch_frame);
}

StgStack *any_catch_stm_frame(Capability *cap, StgWord w) {
  return setup(cap, sizeofW(StgCatchSTMFrame), w, &create_any_catch_stm_frame);
}

StgStack *any_catch_retry_frame(Capability *cap, StgWord w) {
  return setup(cap, sizeofW(StgCatchRetryFrame), w,
               &create_any_catch_retry_frame);
}

StgStack *any_atomically_frame(Capability *cap, StgWord w) {
  return setup(cap, sizeofW(StgAtomicallyFrame), w,
               &create_any_atomically_frame);
}

StgStack *any_ret_small_prim_frame(Capability *cap, StgWord w) {
  return setup(cap, sizeofW(StgClosure) + sizeofW(StgWord), w,
               &create_any_ret_small_prim_frame);
}

StgStack *any_ret_small_closure_frame(Capability *cap, StgWord w) {
  return setup(cap, sizeofW(StgClosure) + sizeofW(StgClosurePtr), w,
               &create_any_ret_small_closure_frame);
}

StgStack *any_ret_small_closures_frame(Capability *cap, StgWord w) {
  return setup(
      cap, sizeofW(StgClosure) + MAX_SMALL_BITMAP_BITS * sizeofW(StgClosurePtr),
      w, &create_any_ret_small_closures_frame);
}

StgStack *any_ret_small_prims_frame(Capability *cap, StgWord w) {
  return setup(cap,
               sizeofW(StgClosure) + MAX_SMALL_BITMAP_BITS * sizeofW(StgWord),
               w, &create_any_ret_small_prims_frame);
}

StgStack *any_ret_big_closures_min_frame(Capability *cap, StgWord w) {
  return setup(cap, sizeofW(StgClosure) + MIN_LARGE_BITMAP_BITS * sizeofW(StgClosure), w,
               &create_any_ret_big_closures_min_frame);
}

StgStack *any_ret_big_prims_min_frame(Capability *cap, StgWord w) {
  return setup(cap, sizeofW(StgClosure) + MIN_LARGE_BITMAP_BITS * sizeofW(StgWord), w,
               &create_any_ret_big_prims_min_frame);
}

void belchStack(StgStack *stack) { printStack(stack); }
