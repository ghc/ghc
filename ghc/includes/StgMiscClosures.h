/* -----------------------------------------------------------------------------
 * $Id: StgMiscClosures.h,v 1.4 1999/01/15 12:47:19 sewardj Exp $
 *
 * Entry code for various built-in closure types.
 *
 * ---------------------------------------------------------------------------*/

/* The naming scheme here follows the naming scheme for closure types
 * defined in InfoTables.h.  The actual info tables and entry code for
 * these objects can be found in StgMiscClosures.hc.
 */

/* entry code */

STGFUN(IND_entry);
STGFUN(IND_STATIC_entry);
STGFUN(IND_PERM_entry);
STGFUN(IND_OLDGEN_entry);
STGFUN(IND_OLDGEN_PERM_entry);
STGFUN(CAF_UNENTERED_entry);
STGFUN(CAF_ENTERED_entry);
STGFUN(CAF_BLACKHOLE_entry);
STGFUN(BLACKHOLE_entry);
STGFUN(BCO_entry);
STGFUN(EVACUATED_entry);
STGFUN(FOREIGN_entry);
STGFUN(WEAK_entry);
STGFUN(DEAD_WEAK_entry);
STGFUN(TSO_entry);
STGFUN(FULL_MVAR_entry);
STGFUN(EMPTY_MVAR_entry);
STGFUN(ARR_WORDS_entry);
STGFUN(MUT_ARR_WORDS_entry);
STGFUN(MUT_ARR_PTRS_entry);
STGFUN(MUT_ARR_PTRS_FROZEN_entry);
STGFUN(MUT_VAR_entry);
STGFUN(END_TSO_QUEUE_entry);
STGFUN(MUT_CONS_entry);
STGFUN(END_MUT_LIST_entry);
STGFUN(dummy_ret_entry);

/* info tables */

extern const StgInfoTable IND_info;
extern const StgInfoTable IND_STATIC_info;
extern const StgInfoTable IND_PERM_info;
extern const StgInfoTable IND_OLDGEN_info;
extern const StgInfoTable IND_OLDGEN_PERM_info;
extern const StgInfoTable CAF_UNENTERED_info;
extern const StgInfoTable CAF_ENTERED_info;
extern const StgInfoTable CAF_BLACKHOLE_info;
extern const StgInfoTable BLACKHOLE_info;
extern const StgInfoTable BCO_info;
extern const StgInfoTable EVACUATED_info;
extern const StgInfoTable FOREIGN_info;
extern const StgInfoTable WEAK_info;
extern const StgInfoTable DEAD_WEAK_info;
extern const StgInfoTable FULL_MVAR_info;
extern const StgInfoTable EMPTY_MVAR_info;
extern const StgInfoTable TSO_info;
extern const StgInfoTable ARR_WORDS_info;
extern const StgInfoTable MUT_ARR_WORDS_info;
extern const StgInfoTable MUT_ARR_PTRS_info;
extern const StgInfoTable MUT_ARR_PTRS_FROZEN_info;
extern const StgInfoTable MUT_VAR_info;
extern const StgInfoTable END_TSO_QUEUE_info;
extern const StgInfoTable MUT_CONS_info;
extern const StgInfoTable END_MUT_LIST_info;
extern const StgInfoTable catch_info;
extern const StgInfoTable seq_info;
extern const StgInfoTable dummy_ret_info;

#ifdef INTERPRETER

EXTFUN(Hugs_CONSTR_entry);
extern const StgInfoTable ret_bco_info;

#endif /* INTERPRETER */

/* closures */

extern StgClosure END_TSO_QUEUE_closure;
extern StgClosure END_MUT_LIST_closure;
extern StgClosure dummy_ret_closure;

extern StgIntCharlikeClosure CHARLIKE_closure[];
extern StgIntCharlikeClosure INTLIKE_closure[];

/* standard entry points */

extern StgFun stg_error_entry;

/* standard selector thunks */

EI_ __sel_0_upd_info;
EI_ __sel_1_upd_info;
EI_ __sel_2_upd_info;
EI_ __sel_3_upd_info;
EI_ __sel_4_upd_info;
EI_ __sel_5_upd_info;
EI_ __sel_6_upd_info;
EI_ __sel_7_upd_info;
EI_ __sel_8_upd_info;
EI_ __sel_8_upd_info;
EI_ __sel_9_upd_info;
EI_ __sel_10_upd_info;
EI_ __sel_11_upd_info;
EI_ __sel_12_upd_info;
EI_ __sel_13_upd_info;
EI_ __sel_14_upd_info;
EI_ __sel_15_upd_info;

EI_ __sel_0_noupd_info;
EI_ __sel_1_noupd_info;
EI_ __sel_2_noupd_info;
EI_ __sel_3_noupd_info;
EI_ __sel_4_noupd_info;
EI_ __sel_5_noupd_info;
EI_ __sel_6_noupd_info;
EI_ __sel_7_noupd_info;
EI_ __sel_8_noupd_info;
EI_ __sel_9_noupd_info;
EI_ __sel_10_noupd_info;
EI_ __sel_11_noupd_info;
EI_ __sel_12_noupd_info;
EI_ __sel_13_noupd_info;
EI_ __sel_14_noupd_info;
EI_ __sel_15_noupd_info;

/* standard ap thunks */

EI_ __ap_1_upd_info;
EI_ __ap_2_upd_info;
EI_ __ap_3_upd_info;
EI_ __ap_4_upd_info;
EI_ __ap_5_upd_info;
EI_ __ap_6_upd_info;
EI_ __ap_7_upd_info;
EI_ __ap_8_upd_info;

