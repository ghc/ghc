
// The Jumps.h file allows us to compile certain hand-written functions in
// the RTS with different levels of support for vector registers.
//
// See Note [realArgRegsCover] in GHC.Cmm.CallConv for more details.

#if defined(ARG_REGS_V64)
  #define MK_FUN_NM(f) f##_v64
  #define ALL_ARG_REGS V64_ARG_REGS
  #define SAVE_ARG_REGS SAVE_V64_ARG_REGS()
  #define RESTORE_ARG_REGS RESTORE_V64_ARG_REGS()
  #if defined(REG_ZMM1) || defined(UnregisterisedCompiler)
    #define REGS_ALLOWED 1
  #else
    #define REGS_ALLOWED 0
  #endif
#elif defined(ARG_REGS_V32)
  #define MK_FUN_NM(f) f##_v32
  #define ALL_ARG_REGS V32_ARG_REGS
  #define SAVE_ARG_REGS SAVE_V32_ARG_REGS()
  #define RESTORE_ARG_REGS RESTORE_V32_ARG_REGS()
  #if defined(REG_YMM1) || defined(UnregisterisedCompiler)
    #define REGS_ALLOWED 1
  #else
    #define REGS_ALLOWED 0
  #endif
#elif defined(ARG_REGS_V16)
  #define MK_FUN_NM(f) f##_v16
  #define ALL_ARG_REGS V16_ARG_REGS
  #define SAVE_ARG_REGS SAVE_V16_ARG_REGS()
  #define RESTORE_ARG_REGS RESTORE_V16_ARG_REGS()
  #if defined(REG_XMM1) || defined(UnregisterisedCompiler)
    #define REGS_ALLOWED 1
  #else
    #define REGS_ALLOWED 0
  #endif
#else
  #define MK_FUN_NM(f) f##_d
  #define ALL_ARG_REGS SCALAR_ARG_REGS
  #define SAVE_ARG_REGS SAVE_SCALAR_ARG_REGS()
  #define RESTORE_ARG_REGS RESTORE_SCALAR_ARG_REGS()
  #define REGS_ALLOWED 1
#endif

#if !defined(UnregisterisedCompiler)
// See Note [import CLOSURE annotations] in rts/Apply.cmm
import CLOSURE RtsFlags;
#endif

/* ----------------------------------------------------------------------------
   Stack underflow
   ------------------------------------------------------------------------- */

INFO_TABLE_RET (MK_FUN_NM(stg_stack_underflow_frame), UNDERFLOW_FRAME,
                W_ info_ptr, P_ unused)
    /* no args => explicit stack */
{
#if REGS_ALLOWED
    unwind Sp = W_[Sp + WDS(2)];

    W_ new_tso;
    W_ ret_off;

    SAVE_ARG_REGS;
    SAVE_THREAD_STATE();
    (ret_off) = foreign "C" threadStackUnderflow(MyCapability() "ptr",
                                                 CurrentTSO);
    LOAD_THREAD_STATE();
    RESTORE_ARG_REGS;

    jump %ENTRY_CODE(Sp(ret_off)) ALL_ARG_REGS;
#else
    ccall barf("stg_stack_underflow_frame: unsupported register", NULL) never returns;
#endif
}

/* ----------------------------------------------------------------------------
   Restore a saved cost centre
   ------------------------------------------------------------------------- */

INFO_TABLE_RET (MK_FUN_NM(stg_restore_cccs), RET_SMALL, W_ info_ptr, W_ cccs)
{
#if REGS_ALLOWED
    W_ _unused;

    unwind Sp = Sp + WDS(2);
#if defined(PROFILING)
    CCCS = Sp(1);
#endif
    Sp_adj(2);

    IF_DEBUG(sanity,
      SAVE_ARG_REGS;
      (_unused) = ccall checkStackFrame(Sp "ptr");
      RESTORE_ARG_REGS);

    jump %ENTRY_CODE(Sp(0)) ALL_ARG_REGS;
#else
    ccall barf("stg_restore_cccs: unsupported register", NULL) never returns;
#endif
}
