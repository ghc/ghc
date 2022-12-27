#include "MachDeps.h"
#include "Rts.h"
#include "RtsAPI.h"
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

void create_any_update_frame(Capability *cap, StgStack *stack, StgWord w) {
  StgUpdateFrame *updF = (StgUpdateFrame *)stack->sp;
  SET_HDR(updF, &stg_upd_frame_info, CCS_SYSTEM);
  StgClosure *payload = rts_mkWord(cap, w);
  updF->updatee = payload;
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
    StgClosure *payload1 = rts_mkWord(cap, w);
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

void create_any_ret_big_prims_frame(Capability *cap, StgStack *stack,
                                    StgWord w) {
  StgClosure *c = (StgClosure *)stack->sp;
  StgWord bitmapCount = 1;
  StgWord memSizeInfo = sizeofW(StgRetInfoTable);
  StgWord memSizeBitmap =
      sizeofW(StgLargeBitmap) + bitmapCount * sizeofW(StgWord);
  StgRetInfoTable *info = allocate(cap, memSizeInfo);
  memset(info, 0, WDS(memSizeInfo));
  StgLargeBitmap *largeBitmap = allocate(cap, memSizeBitmap);
  memset(largeBitmap, 0, WDS(memSizeBitmap));
  info->i.type = RET_BIG;
#if !defined(TABLES_NEXT_TO_CODE)
  info->i.layout.large_bitmap =
      largeBitmap; /* pointer to large bitmap structure */
  SET_HDR(c, info, CCS_SYSTEM);
#else
  info->i.layout.large_bitmap_offset =
      ((StgWord)largeBitmap) - ((StgWord)(info + 1));
  SET_HDR(c, (StgInfoTable *)info + 1, CCS_SYSTEM);
#endif
  largeBitmap->size = 1;
  largeBitmap->bitmap[0] = 1;
  StgClosure *payload = UNTAG_CLOSURE(rts_mkWord(cap, w));
  c->payload[0] = (StgClosure *)w;

  debugBelch("Yooo itbl : %us\n", get_itbl(c)->type);
  debugBelch("Yooo bitmap size : %ul\n", GET_LARGE_BITMAP(get_itbl(c))->size);
  printStack(stack);
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
  return setup(sizeofW(StgClosure) + sizeofW(StgWord), w,
               &create_any_ret_small_prim_frame);
}

StgStack *any_ret_small_closure_frame(StgWord w) {
  return setup(sizeofW(StgClosure) + sizeofW(StgClosurePtr), w,
               &create_any_ret_small_closure_frame);
}

StgStack *any_ret_small_closures_frame(StgWord w) {
  return setup(sizeofW(StgClosure) +
                   MAX_SMALL_BITMAP_BITS * sizeofW(StgClosurePtr),
               w, &create_any_ret_small_closures_frame);
}

StgStack *any_ret_small_prims_frame(StgWord w) {
  return setup(sizeofW(StgClosure) +
                   MAX_SMALL_BITMAP_BITS * sizeofW(StgWord),
               w, &create_any_ret_small_prims_frame);
}

StgStack *any_ret_big_closures_frame(StgWord w) {
  return NULL; // TODO: Implement
  //  return setup(sizeofW(StgClosure) + sizeofW(StgClosurePtr), w,
  //               &create_any_ret_closures_closure_frame);
}

StgStack *any_ret_big_prims_frame(StgWord w) {
  return setup(sizeofW(StgClosure) + sizeofW(StgWord), w,
               &create_any_ret_big_prims_frame);
}
