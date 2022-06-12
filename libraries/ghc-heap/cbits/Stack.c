#include "Rts.h"

// Only exists to make the stack_frame_sizeW macro available in Haskell code
// (via FFI).
StgWord stackFrameSizeW(StgClosure *frame) { return stack_frame_sizeW(frame); }

// Only exists to make the get_itbl macro available in Haskell code (via FFI).
const StgInfoTable *getItbl(StgClosure *closure) {
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

// TODO: Consider to use HSC
StgWord getBitmapSize(StgInfoTable *info){
  StgWord bitmap = info->layout.bitmap;
  return BITMAP_SIZE(bitmap);
}

// TODO: Consider to use HSC
StgWord getBitmapWord(StgInfoTable *info){
  StgWord bitmap = info->layout.bitmap;
  return BITMAP_BITS(bitmap);
}
