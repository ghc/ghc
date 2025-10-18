#include "MachDeps.h"
#include "Rts.h"
#include "RtsAPI.h"
#include "rts/Messages.h"
#include "rts/Types.h"
#include "rts/storage/ClosureTypes.h"
#include "rts/storage/Closures.h"
#include "rts/storage/FunTypes.h"
#include "rts/storage/InfoTables.h"

StgWord stackFrameSize(StgStack *stack, StgWord offset) {
  StgClosure *c = (StgClosure *)stack->sp + offset;
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(c));
  return stack_frame_sizeW(c);
}

StgStack *getUnderflowFrameStack(StgStack *stack, StgWord offset) {
  StgClosure *frame = (StgClosure *)stack->sp + offset;
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(frame));
  const StgRetInfoTable *info = get_ret_itbl((StgClosure *)frame);

  if (info->i.type == UNDERFLOW_FRAME) {
    return ((StgUnderflowFrame *)frame)->next_chunk;
  } else {
    return NULL;
  }
}

// Only exists to make the get_itbl macro available in Haskell code (via FFI).
const StgInfoTable *getItbl(StgClosure *closure) {
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(closure));
  return get_itbl(closure);
};

StgWord getBitmapSize(StgClosure *c) {
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(c));

  const StgInfoTable *info = get_itbl(c);
  StgWord bitmap = info->layout.bitmap;
  return BITMAP_SIZE(bitmap);
}

StgWord getRetFunBitmapSize(StgRetFun *ret_fun) {
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(ret_fun));

  const StgFunInfoTable *fun_info = get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));
  switch (fun_info->f.fun_type) {
  case ARG_GEN:
    return BITMAP_SIZE(fun_info->f.b.bitmap);
  case ARG_GEN_BIG:
    return GET_FUN_LARGE_BITMAP(fun_info)->size;
  default:
    return BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]);
  }
}

StgWord getBitmapWord(StgClosure *c) {
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(c));

  const StgInfoTable *info = get_itbl(c);
  StgWord bitmap = info->layout.bitmap;
  StgWord bitmapWord = BITMAP_BITS(bitmap);
  return bitmapWord;
}

StgWord getRetFunBitmapWord(StgRetFun *ret_fun) {
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(ret_fun));

  const StgFunInfoTable *fun_info = get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));
  fun_info = get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));
  switch (fun_info->f.fun_type) {
  case ARG_GEN:
    return BITMAP_BITS(fun_info->f.b.bitmap);
  case ARG_GEN_BIG:
    // Cannot do more than warn and exit.
    errorBelch("Unexpected ARG_GEN_BIG StgRetFun closure %p", ret_fun);
    stg_exit(EXIT_INTERNAL_ERROR);
  default:
    return BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
  }
}

StgWord getLargeBitmapSize(StgClosure *c) {
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(c));

  const StgInfoTable *info = get_itbl(c);
  StgLargeBitmap *bitmap = GET_LARGE_BITMAP(info);
  return bitmap->size;
}

StgWord getRetFunSize(StgRetFun *ret_fun) {
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(ret_fun));

  const StgFunInfoTable *fun_info = get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));
  fun_info = get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));
  switch (fun_info->f.fun_type) {
  case ARG_GEN:
    return BITMAP_SIZE(fun_info->f.b.bitmap);
  case ARG_GEN_BIG:
    return GET_FUN_LARGE_BITMAP(fun_info)->size;
  default:
    return BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]);
  }
}

StgWord getBCOLargeBitmapSize(StgClosure *c) {
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(c));

  StgBCO *bco = (StgBCO *)*c->payload;

  return BCO_BITMAP_SIZE(bco);
}

StgWord *getLargeBitmap(Capability *cap, StgClosure *c) {
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(c));
  const StgInfoTable *info = get_itbl(c);
  StgLargeBitmap *bitmap = GET_LARGE_BITMAP(info);

  return bitmap->bitmap;
}

StgWord *getRetFunLargeBitmap(Capability *cap, StgRetFun *ret_fun) {
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(ret_fun));

  const StgFunInfoTable *fun_info = get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));
  StgLargeBitmap *bitmap = GET_FUN_LARGE_BITMAP(fun_info);

  return bitmap->bitmap;
}

StgWord *getBCOLargeBitmap(Capability *cap, StgClosure *c) {
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(c));

  StgBCO *bco = (StgBCO *)*c->payload;
  StgLargeBitmap *bitmap = BCO_BITMAP(bco);

  return bitmap->bitmap;
}

StgStack *getUnderflowFrameNextChunk(StgUnderflowFrame *frame) {
  return frame->next_chunk;
}

StgWord isArgGenBigRetFunType(StgRetFun *ret_fun) {
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(ret_fun));

  const StgFunInfoTable *fun_info = get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));
  return fun_info->f.fun_type == ARG_GEN_BIG;
}

StgClosure *getStackClosure(StgClosure **c) { return *c; }
