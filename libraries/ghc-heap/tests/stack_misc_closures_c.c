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
#include "stg/MiscClosures.h"
#include "stg/Types.h"

// TODO: Delete when development finished
extern void printStack(StgStack *stack);
extern void printObj(StgClosure *obj);

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

#define TWO_WORDS_LARGE_BITMAP_BITS (BITS_IN(W_) + 1)

RTS_RET(test_big_ret_two_words_p);
void create_any_ret_big_closures_two_words_frame(Capability *cap,
                                                 StgStack *stack, StgWord w) {
  StgClosure *c = (StgClosure *)stack->sp;
  SET_HDR(c, &test_big_ret_two_words_p_info, CCS_SYSTEM);

  for (int i = 0; i < TWO_WORDS_LARGE_BITMAP_BITS; i++) {
    c->payload[i] = UNTAG_CLOSURE(rts_mkWord(cap, w));
    w++;
  }
}

RTS_RET(test_ret_fun);
RTS_RET(test_arg_n_fun_0_1);
void create_any_ret_fun_arg_n_prim_frame(Capability *cap, StgStack *stack,
                                         StgWord w) {
  StgRetFun *c = (StgRetFun *)stack->sp;
  c->info = &test_ret_fun_info;
  StgClosure *f =
      (StgClosure *)allocate(cap, sizeofW(StgClosure) + sizeofW(StgWord));
  SET_HDR(f, &test_arg_n_fun_0_1_info, ccs)
  c->fun = f;
  const StgFunInfoTable *fun_info = get_fun_itbl(UNTAG_CLOSURE(c->fun));
  c->size = BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]);
  // The cast is a lie (w is interpreted as plain Word, not as pointer), but the
  // memory layout fits.
  c->payload[0] = (StgClosure *)w;
  f->payload[0] = (StgClosure *)w;
}

RTS_CLOSURE(base_GHCziIOziEncodingziLatin1_zdwasciizuencode_closure);
void create_any_ret_fun_arg_gen_frame(Capability *cap, StgStack *stack,
                                      StgWord w) {
  StgRetFun *c = (StgRetFun *)stack->sp;
  c->info = &test_ret_fun_info;
  // The selection of this closure was a bit arbitrary: There aren't many
  // ARG_GEN closures around and I found this one first. N.B.: The payload
  // values (and their types) are non-sense. But, this should be okay as we're
  // only testing de-serialization.
  c->fun = &base_GHCziIOziEncodingziLatin1_zdwasciizuencode_closure;
  const StgFunInfoTable *fun_info = get_fun_itbl(UNTAG_CLOSURE(c->fun));
  c->size = BITMAP_SIZE(fun_info->f.b.bitmap);
  c->payload[0] = (StgClosure *)w;
  c->payload[1] = rts_mkWord(cap, ++w);
  c->payload[2] = rts_mkWord(cap, ++w);
  c->payload[3] = (StgClosure *)++w;
  c->payload[4] = (StgClosure *)++w;
  c->payload[5] = (StgClosure *)++w;
  c->payload[6] = (StgClosure *)++w;
  c->payload[7] = rts_mkWord(cap, ++w);

  // TODO: Is this really needed? ghc-heap does not need it. Does the GC need
  // it?
  for (int i = 0; i < 8; i++) {
    c->fun->payload[i] = c->payload[i];
  }
}

// Import from Sanity.c
extern void checkSTACK(StgStack *stack);

StgStack *setup(Capability *cap, StgWord closureSizeWords,
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

  // Pointers can easíly be confused with each other. Provide a start value for
  // values (1) in closures and increment it after every usage. The goal is to
  // have distinct values in the closure to ensure nothing gets mixed up.
  f(cap, stack, 1);

  // Make a sanitiy check to find unsound closures before the GC and the decode
  // code.
  checkSTACK(stack);
  return stack;
}

StgStack *any_update_frame(Capability *cap) {
  return setup(cap, sizeofW(StgUpdateFrame), &create_any_update_frame);
}

StgStack *any_catch_frame(Capability *cap) {
  return setup(cap, sizeofW(StgCatchFrame), &create_any_catch_frame);
}

StgStack *any_catch_stm_frame(Capability *cap) {
  return setup(cap, sizeofW(StgCatchSTMFrame), &create_any_catch_stm_frame);
}

StgStack *any_catch_retry_frame(Capability *cap) {
  return setup(cap, sizeofW(StgCatchRetryFrame), &create_any_catch_retry_frame);
}

StgStack *any_atomically_frame(Capability *cap) {
  return setup(cap, sizeofW(StgAtomicallyFrame), &create_any_atomically_frame);
}

StgStack *any_ret_small_prim_frame(Capability *cap) {
  return setup(cap, sizeofW(StgClosure) + sizeofW(StgWord),
               &create_any_ret_small_prim_frame);
}

StgStack *any_ret_small_closure_frame(Capability *cap) {
  return setup(cap, sizeofW(StgClosure) + sizeofW(StgClosurePtr),
               &create_any_ret_small_closure_frame);
}

StgStack *any_ret_small_closures_frame(Capability *cap) {
  return setup(
      cap, sizeofW(StgClosure) + MAX_SMALL_BITMAP_BITS * sizeofW(StgClosurePtr),
      &create_any_ret_small_closures_frame);
}

StgStack *any_ret_small_prims_frame(Capability *cap) {
  return setup(cap,
               sizeofW(StgClosure) + MAX_SMALL_BITMAP_BITS * sizeofW(StgWord),
               &create_any_ret_small_prims_frame);
}

StgStack *any_ret_big_closures_min_frame(Capability *cap) {
  return setup(
      cap, sizeofW(StgClosure) + MIN_LARGE_BITMAP_BITS * sizeofW(StgClosure),
      &create_any_ret_big_closures_min_frame);
}

StgStack *any_ret_big_closures_two_words_frame(Capability *cap) {
  return setup(cap,
               sizeofW(StgClosure) +
                   TWO_WORDS_LARGE_BITMAP_BITS * sizeofW(StgClosure),
               &create_any_ret_big_closures_two_words_frame);
}

StgStack *any_ret_big_prims_min_frame(Capability *cap) {
  return setup(cap,
               sizeofW(StgClosure) + MIN_LARGE_BITMAP_BITS * sizeofW(StgWord),
               &create_any_ret_big_prims_min_frame);
}

StgStack *any_ret_fun_arg_n_prim_frame(Capability *cap) {
  return setup(cap, sizeofW(StgRetFun) + sizeofW(StgWord),
               &create_any_ret_fun_arg_n_prim_frame);
}

StgStack *any_ret_fun_arg_gen_frame(Capability *cap) {
  return setup(
      cap, sizeofW(StgRetFun) + 5 * sizeofW(StgWord) + 3 * sizeofW(StgClosure),
      &create_any_ret_fun_arg_gen_frame);
}

void belchStack(StgStack *stack) { printStack(stack); }
