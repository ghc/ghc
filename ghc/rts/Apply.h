// -----------------------------------------------------------------------------
// Apply.h
//
// (c) The University of Glasgow 2002
//
// Helper bits for the generic apply code (AutoApply.hc)
// -----------------------------------------------------------------------------

#ifndef APPLY_H
#define APPLY_H

// Build a new PAP: function is in R1,p
// ret addr and m arguments taking up n words are on the stack.
#define BUILD_PAP(m,n,f)			\
 {						\
    StgPAP *pap;				\
    nat size, i;				\
    TICK_SLOW_CALL_BUILT_PAP();			\
    size = PAP_sizeW(n);			\
    HP_CHK_NP(size, Sp[0] = f;);		\
    TICK_ALLOC_PAP(n, 0);			\
    pap = (StgPAP *) (Hp + 1 - size);		\
    SET_HDR(pap, &stg_PAP_info, CCCS);		\
    pap->arity = arity - m;			\
    pap->fun = R1.cl;				\
    pap->n_args = n;				\
    for (i = 0; i < n; i++) {			\
      pap->payload[i] = (StgClosure *)Sp[1+i];	\
    }						\
    R1.p = (P_)pap;				\
    Sp += 1 + n;				\
    JMP_(ENTRY_CODE(Sp[0]));			\
 }

// Copy the old PAP, build a new one with the extra arg(s)
// ret addr and m arguments taking up n words are on the stack.
#define NEW_PAP(m,n,f)					\
 {							\
     StgPAP *pap, *new_pap;				\
     nat size, i;					\
     TICK_SLOW_CALL_NEW_PAP();				\
     pap = (StgPAP *)R1.p;				\
     size = PAP_sizeW(pap->n_args + n);			\
     HP_CHK_NP(size, Sp[0] = f;);			\
     TICK_ALLOC_PAP(n, 0);				\
     new_pap = (StgPAP *) (Hp + 1 - size);		\
     SET_HDR(new_pap, &stg_PAP_info, CCCS);		\
     new_pap->arity = arity - m;			\
     new_pap->n_args = pap->n_args + n;			\
     new_pap->fun = pap->fun;				\
     for (i = 0; i < pap->n_args; i++) {		\
	 new_pap->payload[i] = pap->payload[i];		\
     }							\
     for (i = 0; i < n; i++) {				\
        new_pap->payload[pap->n_args+i] = (StgClosure *)Sp[1+i];	\
     }							\
     R1.p = (P_)new_pap;				\
     Sp += n+1;						\
     JMP_(ENTRY_CODE(Sp[0]));				\
 }

// canned slow entry points, indexed by arg type (ARG_P, ARG_PP, etc.)
extern StgFun * stg_ap_stack_entries[];

// canned register save code for heap check failure in a function
extern StgFun * stg_stack_save_entries[];

// canned bitmap for each arg type
extern StgWord stg_arg_bitmaps[];

#endif // APPLY_H

