/* -----------------------------------------------------------------------------
 * $Id: StgMiscClosures.h,v 1.37 2001/02/15 14:27:36 sewardj Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Entry code for various built-in closure types.
 *
 * ---------------------------------------------------------------------------*/

/* The naming scheme here follows the naming scheme for closure types
 * defined in InfoTables.h.  The actual info tables and entry code for
 * these objects can be found in StgMiscClosures.hc.
 */

/* entry code */

STGFUN(stg_IND_entry);
STGFUN(stg_IND_STATIC_entry);
STGFUN(stg_IND_PERM_entry);
STGFUN(stg_IND_OLDGEN_entry);
STGFUN(stg_IND_OLDGEN_PERM_entry);
STGFUN(stg_CAF_UNENTERED_entry);
STGFUN(stg_CAF_ENTERED_entry);
STGFUN(stg_BLACKHOLE_entry);
STGFUN(stg_CAF_BLACKHOLE_entry);
STGFUN(stg_BLACKHOLE_BQ_entry);
#ifdef SMP
STGFUN(stg_WHITEHOLE_entry);
#endif
#ifdef TICKY_TICKY
STGFUN(stg_SE_BLACKHOLE_entry);
STGFUN(stg_SE_CAF_BLACKHOLE_entry);
#endif
#if defined(PAR) || defined(GRAN)
STGFUN(stg_RBH_entry);
#endif
STGFUN(stg_BCO_entry);
STGFUN(stg_EVACUATED_entry);
STGFUN(stg_FOREIGN_entry);
STGFUN(stg_WEAK_entry);
STGFUN(stg_NO_FINALIZER_entry);
STGFUN(stg_DEAD_WEAK_entry);
STGFUN(stg_STABLE_NAME_entry);
STGFUN(stg_TSO_entry);
STGFUN(stg_FULL_MVAR_entry);
STGFUN(stg_EMPTY_MVAR_entry);
STGFUN(stg_ARR_WORDS_entry);
STGFUN(stg_MUT_ARR_PTRS_entry);
STGFUN(stg_MUT_ARR_PTRS_FROZEN_entry);
STGFUN(stg_MUT_VAR_entry);
STGFUN(stg_END_TSO_QUEUE_entry);
STGFUN(stg_MUT_CONS_entry);
STGFUN(stg_END_MUT_LIST_entry);
STGFUN(stg_dummy_ret_entry);

/* entry code for constructors created by the bytecode interpreter */
STGFUN(stg_interp_constr_entry);
STGFUN(stg_interp_constr1_entry);
STGFUN(stg_interp_constr2_entry);
STGFUN(stg_interp_constr3_entry);
STGFUN(stg_interp_constr4_entry);
STGFUN(stg_interp_constr5_entry);
STGFUN(stg_interp_constr6_entry);
STGFUN(stg_interp_constr7_entry);
STGFUN(stg_interp_constr8_entry);

/* Magic glue code for when compiled code returns a value in R1/F1/D1
   or a VoidRep to the interpreter. */
extern DLL_IMPORT_RTS const vec_info_8 stg_ctoi_ret_R1p_info;
extern DLL_IMPORT_RTS const vec_info_8 stg_ctoi_ret_R1n_info;
extern DLL_IMPORT_RTS const vec_info_8 stg_ctoi_ret_F1_info;
extern DLL_IMPORT_RTS const vec_info_8 stg_ctoi_ret_D1_info;
extern DLL_IMPORT_RTS const vec_info_8 stg_ctoi_ret_V_info;

/* Used by the interpreter to return an unboxed value on the stack to
   compiled code. */
extern DLL_IMPORT_RTS const StgInfoTable stg_gc_unbx_r1_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_gc_f1_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_gc_d1_info;

#if defined(PAR) || defined(GRAN)
/* this is the NIL ptr for a blocking queue */
# define END_BQ_QUEUE  ((StgBlockingQueueElement *)(void*)&END_TSO_QUEUE_closure)
/* this is the NIL ptr for a blocked fetch queue (as in PendingFetches in GUM) */
# define END_BF_QUEUE  ((StgBlockedFetch *)(void*)&END_TSO_QUEUE_closure)
#endif

/* info tables */

extern DLL_IMPORT_RTS const StgInfoTable stg_IND_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_IND_STATIC_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_IND_PERM_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_IND_OLDGEN_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_IND_OLDGEN_PERM_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_CAF_UNENTERED_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_CAF_ENTERED_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_BLACKHOLE_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_CAF_BLACKHOLE_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_BLACKHOLE_BQ_info;
#ifdef SMP
extern DLL_IMPORT_RTS const StgInfoTable stg_WHITEHOLE_info;
#endif
#ifdef TICKY_TICKY
extern DLL_IMPORT_RTS const StgInfoTable stg_SE_BLACKHOLE_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_SE_CAF_BLACKHOLE_info;
#endif
#if defined(PAR) || defined(GRAN)
extern DLL_IMPORT_RTS const StgInfoTable stg_RBH_info;
#endif
extern DLL_IMPORT_RTS const StgInfoTable stg_BCO_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_EVACUATED_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_FOREIGN_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_WEAK_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_DEAD_WEAK_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_STABLE_NAME_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_FULL_MVAR_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_EMPTY_MVAR_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_TSO_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_ARR_WORDS_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_MUT_ARR_WORDS_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_MUT_ARR_PTRS_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_MUT_ARR_PTRS_FROZEN_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_MUT_VAR_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_END_TSO_QUEUE_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_MUT_CONS_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_END_MUT_LIST_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_catch_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_seq_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_dummy_ret_info;

/* closures */

extern DLL_IMPORT_RTS StgClosure stg_END_TSO_QUEUE_closure;
extern DLL_IMPORT_RTS StgClosure stg_END_MUT_LIST_closure;
extern DLL_IMPORT_RTS StgClosure stg_NO_FINALIZER_closure;
extern DLL_IMPORT_RTS StgClosure stg_dummy_ret_closure;
extern DLL_IMPORT_RTS StgClosure stg_forceIO_closure;

extern DLL_IMPORT_RTS StgIntCharlikeClosure stg_CHARLIKE_closure[];
extern DLL_IMPORT_RTS StgIntCharlikeClosure stg_INTLIKE_closure[];

/* standard entry points */

/* EXTFUN_RTS(stg_error_entry); No longer used */

  /* (see also below  -- KSW 1998-12) */

/* standard selector thunks */

EXTINFO_RTS stg_sel_0_upd_info;
EXTINFO_RTS stg_sel_1_upd_info;
EXTINFO_RTS stg_sel_2_upd_info;
EXTINFO_RTS stg_sel_3_upd_info;
EXTINFO_RTS stg_sel_4_upd_info;
EXTINFO_RTS stg_sel_5_upd_info;
EXTINFO_RTS stg_sel_6_upd_info;
EXTINFO_RTS stg_sel_7_upd_info;
EXTINFO_RTS stg_sel_8_upd_info;
EXTINFO_RTS stg_sel_8_upd_info;
EXTINFO_RTS stg_sel_9_upd_info;
EXTINFO_RTS stg_sel_10_upd_info;
EXTINFO_RTS stg_sel_11_upd_info;
EXTINFO_RTS stg_sel_12_upd_info;
EXTINFO_RTS stg_sel_13_upd_info;
EXTINFO_RTS stg_sel_14_upd_info;
EXTINFO_RTS stg_sel_15_upd_info;

EXTINFO_RTS stg_sel_0_noupd_info;
EXTINFO_RTS stg_sel_1_noupd_info;
EXTINFO_RTS stg_sel_2_noupd_info;
EXTINFO_RTS stg_sel_3_noupd_info;
EXTINFO_RTS stg_sel_4_noupd_info;
EXTINFO_RTS stg_sel_5_noupd_info;
EXTINFO_RTS stg_sel_6_noupd_info;
EXTINFO_RTS stg_sel_7_noupd_info;
EXTINFO_RTS stg_sel_8_noupd_info;
EXTINFO_RTS stg_sel_9_noupd_info;
EXTINFO_RTS stg_sel_10_noupd_info;
EXTINFO_RTS stg_sel_11_noupd_info;
EXTINFO_RTS stg_sel_12_noupd_info;
EXTINFO_RTS stg_sel_13_noupd_info;
EXTINFO_RTS stg_sel_14_noupd_info;
EXTINFO_RTS stg_sel_15_noupd_info;

  /* and their standard entry points  -- KSW 1998-12 */

EXTFUN_RTS(stg_sel_0_upd_entry);
EXTFUN_RTS(stg_sel_1_upd_entry);
EXTFUN_RTS(stg_sel_2_upd_entry);
EXTFUN_RTS(stg_sel_3_upd_entry);
EXTFUN_RTS(stg_sel_4_upd_entry);
EXTFUN_RTS(stg_sel_5_upd_entry);
EXTFUN_RTS(stg_sel_6_upd_entry);
EXTFUN_RTS(stg_sel_7_upd_entry);
EXTFUN_RTS(stg_sel_8_upd_entry);
EXTFUN_RTS(stg_sel_8_upd_entry);
EXTFUN_RTS(stg_sel_9_upd_entry);
EXTFUN_RTS(stg_sel_10_upd_entry);
EXTFUN_RTS(stg_sel_11_upd_entry);
EXTFUN_RTS(stg_sel_12_upd_entry);
EXTFUN_RTS(stg_sel_13_upd_entry);
EXTFUN_RTS(stg_sel_14_upd_entry);
EXTFUN_RTS(stg_sel_15_upd_entry);

EXTFUN_RTS(stg_sel_0_noupd_entry);
EXTFUN_RTS(stg_sel_1_noupd_entry);
EXTFUN_RTS(stg_sel_2_noupd_entry);
EXTFUN_RTS(stg_sel_3_noupd_entry);
EXTFUN_RTS(stg_sel_4_noupd_entry);
EXTFUN_RTS(stg_sel_5_noupd_entry);
EXTFUN_RTS(stg_sel_6_noupd_entry);
EXTFUN_RTS(stg_sel_7_noupd_entry);
EXTFUN_RTS(stg_sel_8_noupd_entry);
EXTFUN_RTS(stg_sel_9_noupd_entry);
EXTFUN_RTS(stg_sel_10_noupd_entry);
EXTFUN_RTS(stg_sel_11_noupd_entry);
EXTFUN_RTS(stg_sel_12_noupd_entry);
EXTFUN_RTS(stg_sel_13_noupd_entry);
EXTFUN_RTS(stg_sel_14_noupd_entry);
EXTFUN_RTS(stg_sel_15_noupd_entry);

/* standard ap thunks */

EXTINFO_RTS stg_ap_1_upd_info;
EXTINFO_RTS stg_ap_2_upd_info;
EXTINFO_RTS stg_ap_3_upd_info;
EXTINFO_RTS stg_ap_4_upd_info;
EXTINFO_RTS stg_ap_5_upd_info;
EXTINFO_RTS stg_ap_6_upd_info;
EXTINFO_RTS stg_ap_7_upd_info;
EXTINFO_RTS stg_ap_8_upd_info;

