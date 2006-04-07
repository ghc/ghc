/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2002-2004
 *
 * Helper bits for the generic apply code (AutoApply.hc)
 *
 * -------------------------------------------------------------------------- */

#ifndef AUTOAPPLY_H
#define AUTOAPPLY_H

// Build a new PAP: function is in R1
// ret addr and m arguments taking up n words are on the stack.
// NB. x is a dummy argument attached to the 'for' label so that
// BUILD_PAP can be used multiple times in the same function.
#define BUILD_PAP(m,n,f,x)				\
    W_ pap;						\
    W_ size;						\
    W_ i;						\
    size = SIZEOF_StgPAP + WDS(n);			\
    HP_CHK_NP_ASSIGN_SP0(size,f);			\
    TICK_ALLOC_HEAP_NOCTR(BYTES_TO_WDS(size));		\
    TICK_ALLOC_PAP(n+1 /* +1 for the FUN */, 0);	\
    pap = Hp + WDS(1) - size;				\
    SET_HDR(pap, stg_PAP_info, W_[CCCS]);		\
    StgPAP_arity(pap) = HALF_W_(arity - m);		\
    StgPAP_fun(pap)   = R1;				\
    StgPAP_n_args(pap) = HALF_W_(n);			\
    i = 0;						\
  for##x:						\
    if (i < n) {					\
	StgPAP_payload(pap,i) = Sp(1+i);		\
	i = i + 1;					\
	goto for##x;					\
    }							\
    R1 = pap;						\
    Sp_adj(1 + n);					\
    jump %ENTRY_CODE(Sp(0));

// Copy the old PAP, build a new one with the extra arg(s)
// ret addr and m arguments taking up n words are on the stack.
// NB. x is a dummy argument attached to the 'for' label so that
// BUILD_PAP can be used multiple times in the same function.
#define NEW_PAP(m,n,f,x)					\
     W_ pap;							\
     W_ new_pap;						\
     W_ size;							\
     W_ i;							\
     pap = R1;							\
     size = SIZEOF_StgPAP + WDS(TO_W_(StgPAP_n_args(pap))) + WDS(n);	\
     HP_CHK_NP_ASSIGN_SP0(size,f);				\
     TICK_ALLOC_HEAP_NOCTR(BYTES_TO_WDS(size));			\
     TICK_ALLOC_PAP(n+1 /* +1 for the FUN */, 0);		\
     new_pap = Hp + WDS(1) - size;				\
     SET_HDR(new_pap, stg_PAP_info, W_[CCCS]);			\
     StgPAP_arity(new_pap) = HALF_W_(arity - m);		\
     W_ n_args;							\
     n_args = TO_W_(StgPAP_n_args(pap));			\
     StgPAP_n_args(new_pap) = HALF_W_(n_args + n);		\
     StgPAP_fun(new_pap) = StgPAP_fun(pap);			\
     i = 0;							\
   for1##x:							\
     if (i < n_args) {						\
         StgPAP_payload(new_pap,i) = StgPAP_payload(pap,i);	\
	 i = i + 1;						\
	 goto for1##x;						\
     }								\
     i = 0;							\
   for2##x:							\
     if (i < n) {						\
	 StgPAP_payload(new_pap,n_args+i) = Sp(1+i);		\
         i = i + 1;						\
         goto for2##x;						\
     }								\
     R1 = new_pap;						\
     Sp_adj(n+1);						\
     jump %ENTRY_CODE(Sp(0));

#endif /* APPLY_H */

