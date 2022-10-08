#include "MachDeps.h"
#include "Rts.h"
#include "rts/Messages.h"
#include "rts/Types.h"
#include "rts/storage/ClosureTypes.h"
#include "rts/storage/Closures.h"
#include "rts/storage/InfoTables.h"

StgWord stackFrameSize(StgStack* stack, StgWord index){
  StgClosure* c = (StgClosure *) stack->sp + index;
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(c));
  return stack_frame_sizeW(c);
}

StgStack* getUnderflowFrameStack(StgStack* stack, StgWord index){
  StgClosure* frame = (StgClosure *) stack->sp + index;
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(frame));
  const StgRetInfoTable *info  = get_ret_itbl((StgClosure *)frame);

  if(info->i.type == UNDERFLOW_FRAME) {
    return ((StgUnderflowFrame*) frame)->next_chunk;
  } else {
    return NULL;
  }
}

// Only exists to make the get_itbl macro available in Haskell code (via FFI).
const StgInfoTable *getItbl(StgClosure *closure) {
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(closure));
  // printObj(closure);
  return get_itbl(closure);
};

StgWord getSpecialRetSmall(StgPtr sp) {
  StgWord c = *sp;
  if (c == (StgWord)&stg_ap_v_info) {
    return 1;
  } else if (c == (StgWord)&stg_ap_f_info) {
    return 2;
  } else if (c == (StgWord)&stg_ap_d_info) {
    return 3;
  } else if (c == (StgWord)&stg_ap_l_info) {
    return 4;
  } else if (c == (StgWord)&stg_ap_n_info) {
    return 5;
  } else if (c == (StgWord)&stg_ap_p_info) {
    return 6;
  } else if (c == (StgWord)&stg_ap_pp_info) {
    return 7;
  } else if (c == (StgWord)&stg_ap_ppp_info) {
    return 8;
  } else if (c == (StgWord)&stg_ap_pppp_info) {
    return 9;
  } else if (c == (StgWord)&stg_ap_ppppp_info) {
    return 10;
  } else if (c == (StgWord)&stg_ap_pppppp_info) {
    return 11;
  } else if (c == (StgWord)&stg_ret_v_info) {
    return 12;
  } else if (c == (StgWord)&stg_ret_p_info) {
    return 13;
  } else if (c == (StgWord)&stg_ret_n_info) {
    return 14;
  } else if (c == (StgWord)&stg_ret_f_info) {
    return 15;
  } else if (c == (StgWord)&stg_ret_d_info) {
    return 16;
  } else if (c == (StgWord)&stg_ret_l_info) {
    return 17;
#if defined(PROFILING)
  } else if (c == (StgWord)&stg_restore_cccs_info) {
    return 18;
  } else if (c == (StgWord)&stg_restore_cccs_eval_info) {
    return 19;
#endif
  } else {
    return 0;
  }
}

StgWord getBitmapSize(StgClosure *c){
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(c));

  const StgInfoTable* info = get_itbl(c);
  StgWord bitmap = info->layout.bitmap;
  return BITMAP_SIZE(bitmap);
}

StgWord getBitmapWord(StgClosure *c){
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(c));

  const StgInfoTable* info = get_itbl(c);
  StgWord bitmap = info->layout.bitmap;
  // debugBelch("getBitmapWord - bitmap : %lu \n", bitmap);
  StgWord bitmapWord = BITMAP_BITS(bitmap);
  // debugBelch("getBitmapWord - bitmapWord : %lu \n", bitmapWord);
  return bitmapWord;
}

StgWord getLargeBitmapSize(StgClosure *c){
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(c));

  const StgInfoTable* info = get_itbl(c);
  StgLargeBitmap* bitmap = GET_LARGE_BITMAP(info);
  return bitmap->size;
}

#define ROUNDUP_BITS_TO_WDS(n) (((n) + WORD_SIZE_IN_BITS - 1) / WORD_SIZE_IN_BITS )

// Copied from Cmm.h
#define SIZEOF_W  SIZEOF_VOID_P
#define WDS(n) ((n)*SIZEOF_W)

StgArrBytes* getLargeBitmaps(Capability *cap, StgClosure *c){
  ASSERT(LOOKS_LIKE_CLOSURE_PTR(c));

  const StgInfoTable* info = get_itbl(c);
  StgLargeBitmap* bitmap = GET_LARGE_BITMAP(info);
  StgWord neededWords = ROUNDUP_BITS_TO_WDS(bitmap->size);
  StgArrBytes* array = (StgArrBytes *) allocate(cap, sizeofW(StgArrBytes) + neededWords);
  SET_HDR(array, &stg_ARR_WORDS_info, CCCS);
  array->bytes = WDS(ROUNDUP_BITS_TO_WDS(bitmap->size));

  for(int i = 0; i < neededWords; i++) {
    array->payload[i] = bitmap->bitmap[i];
  }

  return array;
}

#if defined(DEBUG)
extern void        printStack ( StgStack *stack );
void belchStack(StgStack* stack){
  printStack(stack);
}
#endif
