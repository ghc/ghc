/* -----------------------------------------------------------------------------
 * $Id: StgMiscClosures.h,v 1.42 2002/01/26 18:02:05 rje Exp $
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
extern DLL_IMPORT_RTS const StgInfoTable stg_gc_l1_info;


/* this is the NIL ptr for a TSO queue (e.g. runnable queue) */
#define END_TSO_QUEUE  ((StgTSO *)(void*)&stg_END_TSO_QUEUE_closure)
/* this is the NIL ptr for a list CAFs */
#define END_ECAF_LIST   ((StgCAF *)(void*)&stg_END_TSO_QUEUE_closure)
#if defined(PAR) || defined(GRAN)
/* this is the NIL ptr for a blocking queue */
# define END_BQ_QUEUE  ((StgBlockingQueueElement *)(void*)&stg_END_TSO_QUEUE_closure)
/* this is the NIL ptr for a blocked fetch queue (as in PendingFetches in GUM) */
# define END_BF_QUEUE  ((StgBlockedFetch *)(void*)&stg_END_TSO_QUEUE_closure)
#endif
/* ToDo?: different name for end of sleeping queue ? -- HWL */

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
#if defined(PAR)
extern DLL_IMPORT_RTS const StgInfoTable stg_FETCH_ME_BQ_info;
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

/* standard GC & stack check entry points */

EXTFUN(stg_gc_entertop);
EXTFUN(stg_gc_enter_1_hponly);
EXTFUN(__stg_gc_enter_1);
EXTFUN(stg_gc_enter_2);
EXTFUN(stg_gc_enter_3);
EXTFUN(stg_gc_enter_4);
EXTFUN(stg_gc_enter_5);
EXTFUN(stg_gc_enter_6);
EXTFUN(stg_gc_enter_7);
EXTFUN(stg_gc_enter_8);
EXTFUN(stg_gc_seq_1);

EI_(stg_gc_noregs_info);
EF_(stg_gc_noregs);

EI_(stg_gc_unpt_r1_info);
EF_(stg_gc_unpt_r1);

EI_(stg_gc_unbx_r1_info);
EF_(stg_gc_unbx_r1);

EI_(stg_gc_f1_info);
EF_(stg_gc_f1);

EI_(stg_gc_d1_info);
EF_(stg_gc_d1);

EI_(stg_gc_ut_1_0_info);
EI_(stg_gc_l1_info);
EF_(stg_gc_l1);
EF_(stg_gc_ut_1_0);

EI_(stg_gc_ut_0_1_info);
EF_(stg_gc_ut_0_1);

EXTFUN(__stg_chk_0);
EXTFUN(__stg_chk_1);
EXTFUN(stg_chk_1n);
EXTFUN(stg_chk_2);
EXTFUN(stg_chk_3);
EXTFUN(stg_chk_4);
EXTFUN(stg_chk_5);
EXTFUN(stg_chk_6);
EXTFUN(stg_chk_7);
EXTFUN(stg_chk_8);
EXTFUN(stg_gen_chk_ret);
EXTFUN(stg_gen_chk);
EXTFUN(stg_gen_hp);
EXTFUN(stg_gen_yield);
EXTFUN(stg_yield_noregs);
EXTFUN(stg_yield_to_interpreter);
EXTFUN(stg_gen_block);
EXTFUN(stg_block_noregs);
EXTFUN(stg_block_1);
EXTFUN(stg_block_takemvar);
EXTFUN(stg_block_putmvar);

