/* -----------------------------------------------------------------------------
 * $Id: StgMiscClosures.h,v 1.47 2003/03/27 13:54:31 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2002
 *
 * Entry code for various built-in closure types.
 *
 * ---------------------------------------------------------------------------*/

/* The naming scheme here follows the naming scheme for closure types
 * defined in InfoTables.h.  The actual info tables and entry code for
 * these objects can be found in StgMiscClosures.hc.
 */

/* Various entry points */
STGFUN(stg_PAP_entry);
STGFUN(stg_BCO_entry);

/* Entry code for constructors created by the bytecode interpreter */
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
extern DLL_IMPORT_RTS const StgPolyInfoTable stg_ctoi_ret_R1p_info;
extern DLL_IMPORT_RTS const StgRetInfoTable stg_ctoi_ret_R1unpt_info;
extern DLL_IMPORT_RTS const StgRetInfoTable stg_ctoi_ret_R1n_info;
extern DLL_IMPORT_RTS const StgRetInfoTable stg_ctoi_ret_F1_info;
extern DLL_IMPORT_RTS const StgRetInfoTable stg_ctoi_ret_D1_info;
extern DLL_IMPORT_RTS const StgRetInfoTable stg_ctoi_ret_L1_info;
extern DLL_IMPORT_RTS const StgRetInfoTable stg_ctoi_ret_V_info;

extern DLL_IMPORT_RTS const StgRetInfoTable stg_apply_interp_info;

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
extern DLL_IMPORT_RTS const StgInfoTable stg_IND_direct_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_IND_0_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_IND_1_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_IND_2_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_IND_3_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_IND_4_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_IND_5_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_IND_6_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_IND_7_info;
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
extern DLL_IMPORT_RTS const StgFunInfoTable stg_BCO_info;
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
extern DLL_IMPORT_RTS const StgPolyInfoTable stg_seq_frame_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_PAP_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_AP_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_AP_STACK_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_dummy_ret_info;
extern DLL_IMPORT_RTS const StgInfoTable stg_raise_info;
extern DLL_IMPORT_RTS const StgRetInfoTable stg_forceIO_info;
extern DLL_IMPORT_RTS const StgRetInfoTable stg_noforceIO_info;
/* closures */

extern DLL_IMPORT_RTS StgClosure stg_END_TSO_QUEUE_closure;
extern DLL_IMPORT_RTS StgClosure stg_END_MUT_LIST_closure;
extern DLL_IMPORT_RTS StgClosure stg_NO_FINALIZER_closure;
extern DLL_IMPORT_RTS StgClosure stg_dummy_ret_closure;
extern DLL_IMPORT_RTS StgClosure stg_forceIO_closure;

extern DLL_IMPORT_RTS StgIntCharlikeClosure stg_CHARLIKE_closure[];
extern DLL_IMPORT_RTS StgIntCharlikeClosure stg_INTLIKE_closure[];


/* standard entry points */

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

// standard ap thunks

ETI_RTS stg_ap_1_upd_info;
ETI_RTS stg_ap_2_upd_info;
ETI_RTS stg_ap_3_upd_info;
ETI_RTS stg_ap_4_upd_info;
ETI_RTS stg_ap_5_upd_info;
ETI_RTS stg_ap_6_upd_info;
ETI_RTS stg_ap_7_upd_info;
ETI_RTS stg_ap_8_upd_info;

// standard application routines (see also rts/gen_apply.py, 
// and compiler/codeGen/CgStackery.lhs).

extern DLL_IMPORT_RTS const StgPolyInfoTable stg_ap_0_info;
ERI_(stg_ap_v_info);
ERI_(stg_ap_f_info);
ERI_(stg_ap_d_info);
ERI_(stg_ap_l_info);
ERI_(stg_ap_n_info);
ERI_(stg_ap_p_info);
ERI_(stg_ap_pv_info);
ERI_(stg_ap_pp_info);
ERI_(stg_ap_ppv_info);
ERI_(stg_ap_ppp_info);
ERI_(stg_ap_pppp_info);
ERI_(stg_ap_ppppp_info);
ERI_(stg_ap_pppppp_info);
ERI_(stg_ap_ppppppp_info);

EXTFUN(stg_ap_0_ret);
EXTFUN(stg_ap_v_ret);
EXTFUN(stg_ap_f_ret);
EXTFUN(stg_ap_d_ret);
EXTFUN(stg_ap_l_ret);
EXTFUN(stg_ap_n_ret);
EXTFUN(stg_ap_p_ret);
EXTFUN(stg_ap_pv_ret);
EXTFUN(stg_ap_pp_ret);
EXTFUN(stg_ap_ppv_ret);
EXTFUN(stg_ap_ppp_ret);
EXTFUN(stg_ap_pppp_ret);
EXTFUN(stg_ap_ppppp_ret);
EXTFUN(stg_ap_pppppp_ret);
EXTFUN(stg_ap_ppppppp_ret);

/* standard GC & stack check entry points, all defined in HeapStackCheck.hc */

ERI_(stg_enter_info);
EF_(stg_enter_ret);

ERI_(stg_gc_void_info);

EF_(__stg_gc_enter_1);

EF_(stg_gc_noregs);

ERI_(stg_gc_unpt_r1_info);
EF_(stg_gc_unpt_r1);

ERI_(stg_gc_unbx_r1_info);
EF_(stg_gc_unbx_r1);

ERI_(stg_gc_f1_info);
EF_(stg_gc_f1);

ERI_(stg_gc_d1_info);
EF_(stg_gc_d1);

ERI_(stg_gc_l1_info);
EF_(stg_gc_l1);

EF_(__stg_gc_fun);
ERI_(stg_gc_fun_info);
EF_(stg_gc_fun_ret);

EF_(stg_gc_gen);
ERI_(stg_gc_gen_info);

EF_(stg_ut_1_0_unreg_ret);
ERI_(stg_ut_1_0_unreg_info);

EF_(stg_gc_gen_hp);
EF_(stg_gc_ut);
EF_(stg_gen_yield);
EF_(stg_yield_noregs);
EF_(stg_yield_to_interpreter);
EF_(stg_gen_block);
EF_(stg_block_noregs);
EF_(stg_block_1);
EF_(stg_block_takemvar);
EF_(stg_block_putmvar);
#ifdef mingw32_TARGET_OS
EF_(stg_block_async);
#endif
