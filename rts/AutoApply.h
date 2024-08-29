/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2002-2004
 *
 * Helper bits for the generic apply code (AutoApply.cmm)
 *
 * -------------------------------------------------------------------------- */

#pragma once

// Build a new PAP: function is in R1
// ret addr and m arguments taking up n words are on the stack.
// NB. x is a dummy argument attached to the 'for' label so that
// BUILD_PAP can be used multiple times in the same function.
// m: number of arguments provided
// n: words on the stack for arguments (could be > m e.g. double args on x32)
// f: function
#define BUILD_PAP(m,n,f,x)                              \
    W_ pap;                                             \
    W_ size;                                            \
    W_ i;                                               \
    size = SIZEOF_StgPAP + WDS(n);                      \
    HP_CHK_NP_ASSIGN_SP0(size,f);                       \
    TICK_ALLOC_PAP(size, 0);                            \
    CCCS_ALLOC(size);                                   \
    pap = Hp + WDS(1) - size;                           \
    SET_HDR(pap, stg_PAP_info, CCCS);                   \
    StgPAP_arity(pap) = HALF_W_(arity - m);             \
    StgPAP_fun(pap)   = R1;                             \
    StgPAP_n_args(pap) = HALF_W_(n);                    \
    i = 0;                                              \
  for##x:                                               \
    if (i < n) {                                        \
        StgPAP_payload(pap,i) = Sp(1+i);                \
        i = i + 1;                                      \
        goto for##x;                                    \
    }                                                   \
    R1 = pap;                                           \
    Sp_adj(1 + n);                                      \
    jump %ENTRY_CODE(Sp(0)) [R1];

// Just like when we enter a PAP, if we're building a new PAP by applying more
// arguments to an existing PAP, we must construct the CCS for the new PAP as if
// we had entered the existing PAP from the current CCS.  Otherwise, we lose any
// stack information in the existing PAP.  See #5654, and the test T5654b-O0.
#if defined(PROFILING)
#define ENTER_FUN_CCS_NEW_PAP(pap) \
  ccall enterFunCCS(BaseReg "ptr", StgHeader_ccs(pap) "ptr");
#else
#define ENTER_FUN_CCS_NEW_PAP(pap) /* empty */
#endif

// Copy the old PAP, build a new one with the extra arg(s)
// ret addr and m arguments taking up n words are on the stack.
// NB. x is a dummy argument attached to the 'for' label so that
// BUILD_PAP can be used multiple times in the same function.
#define NEW_PAP(m,n,f,x)                                        \
     W_ pap;                                                    \
     W_ new_pap;                                                \
     W_ size;                                                   \
     W_ i;                                                      \
     pap = R1;                                                  \
     size = SIZEOF_StgPAP + WDS(TO_W_(StgPAP_n_args(pap))) + WDS(n);    \
     HP_CHK_NP_ASSIGN_SP0(size,f);                              \
     TICK_ALLOC_PAP(size, 0);                                   \
     CCCS_ALLOC(size);                                          \
     ENTER_FUN_CCS_NEW_PAP(pap);                                \
     new_pap = Hp + WDS(1) - size;                              \
     SET_HDR(new_pap, stg_PAP_info, CCCS);                      \
     StgPAP_arity(new_pap) = HALF_W_(arity - m);                \
     W_ n_args;                                                 \
     n_args = TO_W_(StgPAP_n_args(pap));                        \
     StgPAP_n_args(new_pap) = HALF_W_(n_args + n);              \
     StgPAP_fun(new_pap) = StgPAP_fun(pap);                     \
     i = 0;                                                     \
   for1##x:                                                     \
     if (i < n_args) {                                          \
         StgPAP_payload(new_pap,i) = StgPAP_payload(pap,i);     \
         i = i + 1;                                             \
         goto for1##x;                                          \
     }                                                          \
     i = 0;                                                     \
   for2##x:                                                     \
     if (i < n) {                                               \
         StgPAP_payload(new_pap,n_args+i) = Sp(1+i);            \
         i = i + 1;                                             \
         goto for2##x;                                          \
     }                                                          \
     R1 = new_pap;                                              \
     Sp_adj(n+1);                                               \
     jump %ENTRY_CODE(Sp(0)) [R1];

// Jump to target, saving CCCS and restoring it on return
#if defined(PROFILING)
#define jump_SAVE_CCCS(restore_fun, target,...) \
    Sp(-1) = CCCS;                              \
    Sp(-2) = (restore_fun);                     \
    Sp_adj(-2);                                 \
    jump (target) [__VA_ARGS__]
#else
#define jump_SAVE_CCCS(restore_fun, target,...) jump (target) [__VA_ARGS__]
#endif
