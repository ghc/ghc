/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Declarations for various symbols exported by the RTS.
 *
 * ToDo: many of the symbols in here don't need to be exported, but
 * our Cmm code generator doesn't know how to generate local symbols
 * for the RTS bits (it assumes all RTS symbols are external).
 *
 * --------------------------------------------------------------------------*/

#ifndef STGMISCCLOSURES_H
#define STGMISCCLOSURES_H

#if IN_STG_CODE
#  define RTS_RET_INFO(i)   extern W_(i)[]
#  define RTS_FUN_INFO(i)   extern W_(i)[]
#  define RTS_THUNK_INFO(i) extern W_(i)[]
#  define RTS_INFO(i)       extern W_(i)[]
#  define RTS_CLOSURE(i)    extern W_(i)[]
#  define RTS_FUN(f)	    extern DLL_IMPORT_RTS StgFunPtr f(void)
#else
#  define RTS_RET_INFO(i)   extern DLL_IMPORT_RTS const StgRetInfoTable i
#  define RTS_FUN_INFO(i)   extern DLL_IMPORT_RTS const StgFunInfoTable i
#  define RTS_THUNK_INFO(i) extern DLL_IMPORT_RTS const StgThunkInfoTable i
#  define RTS_INFO(i)       extern DLL_IMPORT_RTS const StgInfoTable i
#  define RTS_CLOSURE(i)    extern DLL_IMPORT_RTS StgClosure i
#  define RTS_FUN(f)	    extern DLL_IMPORT_RTS StgFunPtr f(void)
#endif

#ifdef TABLES_NEXT_TO_CODE
#  define RTS_ENTRY(f)    /* nothing */
#else
#  define RTS_ENTRY(f)    RTS_FUN(f)
#endif

/* Stack frames */
RTS_RET_INFO(stg_upd_frame_info);
RTS_RET_INFO(stg_marked_upd_frame_info);
RTS_RET_INFO(stg_noupd_frame_info);
RTS_RET_INFO(stg_seq_frame_info);
RTS_RET_INFO(stg_catch_frame_info);
RTS_RET_INFO(stg_catch_retry_frame_info);
RTS_RET_INFO(stg_atomically_frame_info);
RTS_RET_INFO(stg_atomically_waiting_frame_info);
RTS_RET_INFO(stg_catch_stm_frame_info);

RTS_ENTRY(stg_upd_frame_ret);
RTS_ENTRY(stg_marked_upd_frame_ret);
RTS_ENTRY(stg_seq_frame_ret);

/* Entry code for constructors created by the bytecode interpreter */
RTS_FUN(stg_interp_constr_entry);
RTS_FUN(stg_interp_constr1_entry);
RTS_FUN(stg_interp_constr2_entry);
RTS_FUN(stg_interp_constr3_entry);
RTS_FUN(stg_interp_constr4_entry);
RTS_FUN(stg_interp_constr5_entry);
RTS_FUN(stg_interp_constr6_entry);
RTS_FUN(stg_interp_constr7_entry);
RTS_FUN(stg_interp_constr8_entry);

/* Magic glue code for when compiled code returns a value in R1/F1/D1
   or a VoidRep to the interpreter. */
RTS_RET_INFO(stg_ctoi_R1p_info);
RTS_RET_INFO(stg_ctoi_R1unpt_info);
RTS_RET_INFO(stg_ctoi_R1n_info);
RTS_RET_INFO(stg_ctoi_F1_info);
RTS_RET_INFO(stg_ctoi_D1_info);
RTS_RET_INFO(stg_ctoi_L1_info);
RTS_RET_INFO(stg_ctoi_V_info);

RTS_ENTRY(stg_ctoi_R1p_ret);
RTS_ENTRY(stg_ctoi_R1unpt_ret);
RTS_ENTRY(stg_ctoi_R1n_ret);
RTS_ENTRY(stg_ctoi_F1_ret);
RTS_ENTRY(stg_ctoi_D1_ret);
RTS_ENTRY(stg_ctoi_L1_ret);
RTS_ENTRY(stg_ctoi_V_ret);

RTS_RET_INFO(stg_apply_interp_info);
RTS_ENTRY(stg_apply_interp_ret);

RTS_INFO(stg_IND_info);
RTS_INFO(stg_IND_direct_info);
RTS_INFO(stg_IND_0_info);
RTS_INFO(stg_IND_1_info);
RTS_INFO(stg_IND_2_info);
RTS_INFO(stg_IND_3_info);
RTS_INFO(stg_IND_4_info);
RTS_INFO(stg_IND_5_info);
RTS_INFO(stg_IND_6_info);
RTS_INFO(stg_IND_7_info);
RTS_INFO(stg_IND_STATIC_info);
RTS_INFO(stg_IND_PERM_info);
RTS_INFO(stg_IND_OLDGEN_info);
RTS_INFO(stg_IND_OLDGEN_PERM_info);
RTS_INFO(stg_CAF_UNENTERED_info);
RTS_INFO(stg_CAF_ENTERED_info);
RTS_INFO(stg_WHITEHOLE_info);
RTS_INFO(stg_BLACKHOLE_info);
RTS_INFO(stg_CAF_BLACKHOLE_info);
#ifdef TICKY_TICKY
RTS_INFO(stg_SE_BLACKHOLE_info);
RTS_INFO(stg_SE_CAF_BLACKHOLE_info);
#endif

#if defined(PAR) || defined(GRAN)
RTS_INFO(stg_RBH_info);
#endif
#if defined(PAR)
RTS_INFO(stg_FETCH_ME_BQ_info);
#endif
RTS_FUN_INFO(stg_BCO_info);
RTS_INFO(stg_EVACUATED_info);
RTS_INFO(stg_WEAK_info);
RTS_INFO(stg_DEAD_WEAK_info);
RTS_INFO(stg_STABLE_NAME_info);
RTS_INFO(stg_FULL_MVAR_info);
RTS_INFO(stg_EMPTY_MVAR_info);
RTS_INFO(stg_TSO_info);
RTS_INFO(stg_ARR_WORDS_info);
RTS_INFO(stg_MUT_ARR_WORDS_info);
RTS_INFO(stg_MUT_ARR_PTRS_CLEAN_info);
RTS_INFO(stg_MUT_ARR_PTRS_DIRTY_info);
RTS_INFO(stg_MUT_ARR_PTRS_FROZEN_info);
RTS_INFO(stg_MUT_ARR_PTRS_FROZEN0_info);
RTS_INFO(stg_MUT_VAR_CLEAN_info);
RTS_INFO(stg_MUT_VAR_DIRTY_info);
RTS_INFO(stg_END_TSO_QUEUE_info);
RTS_INFO(stg_MUT_CONS_info);
RTS_INFO(stg_catch_info);
RTS_INFO(stg_PAP_info);
RTS_INFO(stg_AP_info);
RTS_INFO(stg_AP_STACK_info);
RTS_INFO(stg_dummy_ret_info);
RTS_INFO(stg_raise_info);
RTS_INFO(stg_TVAR_WAIT_QUEUE_info);
RTS_INFO(stg_TVAR_info);
RTS_INFO(stg_TREC_CHUNK_info);
RTS_INFO(stg_TREC_HEADER_info);
RTS_INFO(stg_END_STM_WAIT_QUEUE_info);
RTS_INFO(stg_END_STM_CHUNK_LIST_info);
RTS_INFO(stg_NO_TREC_info);

RTS_ENTRY(stg_IND_entry);
RTS_ENTRY(stg_IND_direct_entry);
RTS_ENTRY(stg_IND_0_entry);
RTS_ENTRY(stg_IND_1_entry);
RTS_ENTRY(stg_IND_2_entry);
RTS_ENTRY(stg_IND_3_entry);
RTS_ENTRY(stg_IND_4_entry);
RTS_ENTRY(stg_IND_5_entry);
RTS_ENTRY(stg_IND_6_entry);
RTS_ENTRY(stg_IND_7_entry);
RTS_ENTRY(stg_IND_STATIC_entry);
RTS_ENTRY(stg_IND_PERM_entry);
RTS_ENTRY(stg_IND_OLDGEN_entry);
RTS_ENTRY(stg_IND_OLDGEN_PERM_entry);
RTS_ENTRY(stg_CAF_UNENTERED_entry);
RTS_ENTRY(stg_CAF_ENTERED_entry);
RTS_ENTRY(stg_WHITEHOLE_entry);
RTS_ENTRY(stg_BLACKHOLE_entry);
RTS_ENTRY(stg_CAF_BLACKHOLE_entry);
#ifdef TICKY_TICKY
RTS_ENTRY(stg_SE_BLACKHOLE_entry);
RTS_ENTRY(stg_SE_CAF_BLACKHOLE_entry);
#endif
#if defined(PAR) || defined(GRAN)
RTS_ENTRY(stg_RBH_entry);
#endif
#if defined(PAR)
RTS_ENTRY(stg_FETCH_ME_BQ_entry);
#endif
RTS_ENTRY(stg_BCO_entry);
RTS_ENTRY(stg_EVACUATED_entry);
RTS_ENTRY(stg_WEAK_entry);
RTS_ENTRY(stg_DEAD_WEAK_entry);
RTS_ENTRY(stg_STABLE_NAME_entry);
RTS_ENTRY(stg_FULL_MVAR_entry);
RTS_ENTRY(stg_EMPTY_MVAR_entry);
RTS_ENTRY(stg_TSO_entry);
RTS_ENTRY(stg_ARR_WORDS_entry);
RTS_ENTRY(stg_MUT_ARR_WORDS_entry);
RTS_ENTRY(stg_MUT_ARR_PTRS_CLEAN_entry);
RTS_ENTRY(stg_MUT_ARR_PTRS_DIRTY_entry);
RTS_ENTRY(stg_MUT_ARR_PTRS_FROZEN_entry);
RTS_ENTRY(stg_MUT_ARR_PTRS_FROZEN0_entry);
RTS_ENTRY(stg_MUT_VAR_CLEAN_entry);
RTS_ENTRY(stg_MUT_VAR_DIRTY_entry);
RTS_ENTRY(stg_END_TSO_QUEUE_entry);
RTS_ENTRY(stg_MUT_CONS_entry);
RTS_ENTRY(stg_catch_entry);
RTS_ENTRY(stg_PAP_entry);
RTS_ENTRY(stg_AP_entry);
RTS_ENTRY(stg_AP_STACK_entry);
RTS_ENTRY(stg_dummy_ret_entry);
RTS_ENTRY(stg_raise_entry);
RTS_ENTRY(stg_END_STM_WAIT_QUEUE_entry);
RTS_ENTRY(stg_END_STM_CHUNK_LIST_entry);
RTS_ENTRY(stg_NO_TREC_entry);
RTS_ENTRY(stg_TVAR_entry);
RTS_ENTRY(stg_TVAR_WAIT_QUEUE_entry);
RTS_ENTRY(stg_TREC_CHUNK_entry);
RTS_ENTRY(stg_TREC_HEADER_entry);


RTS_ENTRY(stg_unblockAsyncExceptionszh_ret_ret);
RTS_ENTRY(stg_blockAsyncExceptionszh_ret_ret);
RTS_ENTRY(stg_catch_frame_ret);
RTS_ENTRY(stg_catch_retry_frame_ret);
RTS_ENTRY(stg_atomically_frame_ret);
RTS_ENTRY(stg_atomically_waiting_frame_ret);
RTS_ENTRY(stg_catch_stm_frame_ret);
RTS_ENTRY(stg_catch_frame_ret);
RTS_ENTRY(stg_catch_entry);
RTS_ENTRY(stg_raise_entry);

/* closures */

RTS_CLOSURE(stg_END_TSO_QUEUE_closure);
RTS_CLOSURE(stg_NO_FINALIZER_closure);
RTS_CLOSURE(stg_dummy_ret_closure);
RTS_CLOSURE(stg_forceIO_closure);

RTS_CLOSURE(stg_END_STM_WAIT_QUEUE_closure);
RTS_CLOSURE(stg_END_STM_CHUNK_LIST_closure);
RTS_CLOSURE(stg_NO_TREC_closure);

RTS_ENTRY(stg_NO_FINALIZER_entry);
RTS_ENTRY(stg_END_EXCEPTION_LIST_entry);
RTS_ENTRY(stg_EXCEPTION_CONS_entry);

#if IN_STG_CODE
extern DLL_IMPORT_RTS StgWordArray stg_CHARLIKE_closure;
extern DLL_IMPORT_RTS StgWordArray stg_INTLIKE_closure;
#else
extern DLL_IMPORT_RTS StgIntCharlikeClosure stg_CHARLIKE_closure[];
extern DLL_IMPORT_RTS StgIntCharlikeClosure stg_INTLIKE_closure[];
#endif

/* StgStartup */

RTS_RET_INFO(stg_forceIO_info);
RTS_ENTRY(stg_forceIO_ret);

RTS_RET_INFO(stg_noforceIO_info);
RTS_ENTRY(stg_noforceIO_ret);

/* standard entry points */

/* standard selector thunks */

RTS_ENTRY(stg_sel_ret_0_upd_ret);
RTS_ENTRY(stg_sel_ret_1_upd_ret);
RTS_ENTRY(stg_sel_ret_2_upd_ret);
RTS_ENTRY(stg_sel_ret_3_upd_ret);
RTS_ENTRY(stg_sel_ret_4_upd_ret);
RTS_ENTRY(stg_sel_ret_5_upd_ret);
RTS_ENTRY(stg_sel_ret_6_upd_ret);
RTS_ENTRY(stg_sel_ret_7_upd_ret);
RTS_ENTRY(stg_sel_ret_8_upd_ret);
RTS_ENTRY(stg_sel_ret_8_upd_ret);
RTS_ENTRY(stg_sel_ret_9_upd_ret);
RTS_ENTRY(stg_sel_ret_10_upd_ret);
RTS_ENTRY(stg_sel_ret_11_upd_ret);
RTS_ENTRY(stg_sel_ret_12_upd_ret);
RTS_ENTRY(stg_sel_ret_13_upd_ret);
RTS_ENTRY(stg_sel_ret_14_upd_ret);
RTS_ENTRY(stg_sel_ret_15_upd_ret);

RTS_INFO(stg_sel_0_upd_info);
RTS_INFO(stg_sel_1_upd_info);
RTS_INFO(stg_sel_2_upd_info);
RTS_INFO(stg_sel_3_upd_info);
RTS_INFO(stg_sel_4_upd_info);
RTS_INFO(stg_sel_5_upd_info);
RTS_INFO(stg_sel_6_upd_info);
RTS_INFO(stg_sel_7_upd_info);
RTS_INFO(stg_sel_8_upd_info);
RTS_INFO(stg_sel_8_upd_info);
RTS_INFO(stg_sel_9_upd_info);
RTS_INFO(stg_sel_10_upd_info);
RTS_INFO(stg_sel_11_upd_info);
RTS_INFO(stg_sel_12_upd_info);
RTS_INFO(stg_sel_13_upd_info);
RTS_INFO(stg_sel_14_upd_info);
RTS_INFO(stg_sel_15_upd_info);

RTS_ENTRY(stg_sel_0_upd_entry);
RTS_ENTRY(stg_sel_1_upd_entry);
RTS_ENTRY(stg_sel_2_upd_entry);
RTS_ENTRY(stg_sel_3_upd_entry);
RTS_ENTRY(stg_sel_4_upd_entry);
RTS_ENTRY(stg_sel_5_upd_entry);
RTS_ENTRY(stg_sel_6_upd_entry);
RTS_ENTRY(stg_sel_7_upd_entry);
RTS_ENTRY(stg_sel_8_upd_entry);
RTS_ENTRY(stg_sel_8_upd_entry);
RTS_ENTRY(stg_sel_9_upd_entry);
RTS_ENTRY(stg_sel_10_upd_entry);
RTS_ENTRY(stg_sel_11_upd_entry);
RTS_ENTRY(stg_sel_12_upd_entry);
RTS_ENTRY(stg_sel_13_upd_entry);
RTS_ENTRY(stg_sel_14_upd_entry);
RTS_ENTRY(stg_sel_15_upd_entry);

RTS_ENTRY(stg_sel_ret_0_noupd_ret);
RTS_ENTRY(stg_sel_ret_1_noupd_ret);
RTS_ENTRY(stg_sel_ret_2_noupd_ret);
RTS_ENTRY(stg_sel_ret_3_noupd_ret);
RTS_ENTRY(stg_sel_ret_4_noupd_ret);
RTS_ENTRY(stg_sel_ret_5_noupd_ret);
RTS_ENTRY(stg_sel_ret_6_noupd_ret);
RTS_ENTRY(stg_sel_ret_7_noupd_ret);
RTS_ENTRY(stg_sel_ret_8_noupd_ret);
RTS_ENTRY(stg_sel_ret_8_noupd_ret);
RTS_ENTRY(stg_sel_ret_9_noupd_ret);
RTS_ENTRY(stg_sel_ret_10_noupd_ret);
RTS_ENTRY(stg_sel_ret_11_noupd_ret);
RTS_ENTRY(stg_sel_ret_12_noupd_ret);
RTS_ENTRY(stg_sel_ret_13_noupd_ret);
RTS_ENTRY(stg_sel_ret_14_noupd_ret);
RTS_ENTRY(stg_sel_ret_15_noupd_ret);

RTS_INFO(stg_sel_0_noupd_info);
RTS_INFO(stg_sel_1_noupd_info);
RTS_INFO(stg_sel_2_noupd_info);
RTS_INFO(stg_sel_3_noupd_info);
RTS_INFO(stg_sel_4_noupd_info);
RTS_INFO(stg_sel_5_noupd_info);
RTS_INFO(stg_sel_6_noupd_info);
RTS_INFO(stg_sel_7_noupd_info);
RTS_INFO(stg_sel_8_noupd_info);
RTS_INFO(stg_sel_9_noupd_info);
RTS_INFO(stg_sel_10_noupd_info);
RTS_INFO(stg_sel_11_noupd_info);
RTS_INFO(stg_sel_12_noupd_info);
RTS_INFO(stg_sel_13_noupd_info);
RTS_INFO(stg_sel_14_noupd_info);
RTS_INFO(stg_sel_15_noupd_info);

RTS_ENTRY(stg_sel_0_noupd_entry);
RTS_ENTRY(stg_sel_1_noupd_entry);
RTS_ENTRY(stg_sel_2_noupd_entry);
RTS_ENTRY(stg_sel_3_noupd_entry);
RTS_ENTRY(stg_sel_4_noupd_entry);
RTS_ENTRY(stg_sel_5_noupd_entry);
RTS_ENTRY(stg_sel_6_noupd_entry);
RTS_ENTRY(stg_sel_7_noupd_entry);
RTS_ENTRY(stg_sel_8_noupd_entry);
RTS_ENTRY(stg_sel_9_noupd_entry);
RTS_ENTRY(stg_sel_10_noupd_entry);
RTS_ENTRY(stg_sel_11_noupd_entry);
RTS_ENTRY(stg_sel_12_noupd_entry);
RTS_ENTRY(stg_sel_13_noupd_entry);
RTS_ENTRY(stg_sel_14_noupd_entry);
RTS_ENTRY(stg_sel_15_noupd_entry);

/* standard ap thunks */

RTS_THUNK_INFO(stg_ap_1_upd_info);
RTS_THUNK_INFO(stg_ap_2_upd_info);
RTS_THUNK_INFO(stg_ap_3_upd_info);
RTS_THUNK_INFO(stg_ap_4_upd_info);
RTS_THUNK_INFO(stg_ap_5_upd_info);
RTS_THUNK_INFO(stg_ap_6_upd_info);
RTS_THUNK_INFO(stg_ap_7_upd_info);

RTS_ENTRY(stg_ap_1_upd_entry);
RTS_ENTRY(stg_ap_2_upd_entry);
RTS_ENTRY(stg_ap_3_upd_entry);
RTS_ENTRY(stg_ap_4_upd_entry);
RTS_ENTRY(stg_ap_5_upd_entry);
RTS_ENTRY(stg_ap_6_upd_entry);
RTS_ENTRY(stg_ap_7_upd_entry);

/* standard application routines (see also rts/gen_apply.py, 
 * and compiler/codeGen/CgStackery.lhs).
 */
RTS_RET_INFO(stg_ap_v_info);
RTS_RET_INFO(stg_ap_f_info);
RTS_RET_INFO(stg_ap_d_info);
RTS_RET_INFO(stg_ap_l_info);
RTS_RET_INFO(stg_ap_n_info);
RTS_RET_INFO(stg_ap_p_info);
RTS_RET_INFO(stg_ap_pv_info);
RTS_RET_INFO(stg_ap_pp_info);
RTS_RET_INFO(stg_ap_ppv_info);
RTS_RET_INFO(stg_ap_ppp_info);
RTS_RET_INFO(stg_ap_pppv_info);
RTS_RET_INFO(stg_ap_pppp_info);
RTS_RET_INFO(stg_ap_ppppp_info);
RTS_RET_INFO(stg_ap_pppppp_info);

RTS_ENTRY(stg_ap_v_ret);
RTS_ENTRY(stg_ap_f_ret);
RTS_ENTRY(stg_ap_d_ret);
RTS_ENTRY(stg_ap_l_ret);
RTS_ENTRY(stg_ap_n_ret);
RTS_ENTRY(stg_ap_p_ret);
RTS_ENTRY(stg_ap_pv_ret);
RTS_ENTRY(stg_ap_pp_ret);
RTS_ENTRY(stg_ap_ppv_ret);
RTS_ENTRY(stg_ap_ppp_ret);
RTS_ENTRY(stg_ap_pppv_ret);
RTS_ENTRY(stg_ap_pppp_ret);
RTS_ENTRY(stg_ap_ppppp_ret);
RTS_ENTRY(stg_ap_pppppp_ret);

RTS_FUN(stg_ap_0_fast);
RTS_FUN(stg_ap_v_fast);
RTS_FUN(stg_ap_f_fast);
RTS_FUN(stg_ap_d_fast);
RTS_FUN(stg_ap_l_fast);
RTS_FUN(stg_ap_n_fast);
RTS_FUN(stg_ap_p_fast);
RTS_FUN(stg_ap_pv_fast);
RTS_FUN(stg_ap_pp_fast);
RTS_FUN(stg_ap_ppv_fast);
RTS_FUN(stg_ap_ppp_fast);
RTS_FUN(stg_ap_pppv_fast);
RTS_FUN(stg_ap_pppp_fast);
RTS_FUN(stg_ap_ppppp_fast);
RTS_FUN(stg_ap_pppppp_fast);

/* standard GC & stack check entry points, all defined in HeapStackCheck.hc */

RTS_RET_INFO(stg_enter_info);
RTS_ENTRY(stg_enter_ret);

RTS_RET_INFO(stg_gc_void_info);
RTS_ENTRY(stg_gc_void_ret);

RTS_FUN(__stg_gc_enter_1);

RTS_FUN(stg_gc_noregs);

RTS_RET_INFO(stg_gc_unpt_r1_info);
RTS_ENTRY(stg_gc_unpt_r1_ret);
RTS_FUN(stg_gc_unpt_r1);

RTS_RET_INFO(stg_gc_unbx_r1_info);
RTS_ENTRY(stg_gc_unbx_r1_ret);
RTS_FUN(stg_gc_unbx_r1);

RTS_RET_INFO(stg_gc_f1_info);
RTS_ENTRY(stg_gc_f1_ret);
RTS_FUN(stg_gc_f1);

RTS_RET_INFO(stg_gc_d1_info);
RTS_ENTRY(stg_gc_d1_ret);
RTS_FUN(stg_gc_d1);

RTS_RET_INFO(stg_gc_l1_info);
RTS_ENTRY(stg_gc_l1_ret);
RTS_FUN(stg_gc_l1);

RTS_FUN(__stg_gc_fun);
RTS_RET_INFO(stg_gc_fun_info);
RTS_ENTRY(stg_gc_fun_ret);

RTS_RET_INFO(stg_gc_gen_info);
RTS_ENTRY(stg_gc_gen_ret);
RTS_FUN(stg_gc_gen);

RTS_ENTRY(stg_ut_1_0_unreg_ret);
RTS_RET_INFO(stg_ut_1_0_unreg_info);

RTS_FUN(stg_gc_gen_hp);
RTS_FUN(stg_gc_ut);
RTS_FUN(stg_gen_yield);
RTS_FUN(stg_yield_noregs);
RTS_FUN(stg_yield_to_interpreter);
RTS_FUN(stg_gen_block);
RTS_FUN(stg_block_noregs);
RTS_FUN(stg_block_1);
RTS_FUN(stg_block_blackhole);
RTS_FUN(stg_block_blackhole_finally);
RTS_FUN(stg_block_takemvar);
RTS_ENTRY(stg_block_takemvar_ret);
RTS_FUN(stg_block_putmvar);
RTS_ENTRY(stg_block_putmvar_ret);
#ifdef mingw32_HOST_OS
RTS_FUN(stg_block_async);
RTS_ENTRY(stg_block_async_ret);
RTS_FUN(stg_block_async_void);
RTS_ENTRY(stg_block_async_void_ret);
#endif
RTS_FUN(stg_block_stmwait);

/* Entry/exit points from StgStartup.cmm */

RTS_RET_INFO(stg_stop_thread_info);
RTS_ENTRY(stg_stop_thread_ret);

RTS_FUN(stg_returnToStackTop);
RTS_FUN(stg_returnToSched);
RTS_FUN(stg_returnToSchedNotPaused);
RTS_FUN(stg_returnToSchedButFirst);

RTS_FUN(stg_init_finish);
RTS_FUN(stg_init);

/* -----------------------------------------------------------------------------
   PrimOps
   -------------------------------------------------------------------------- */

RTS_FUN(plusIntegerzh_fast);
RTS_FUN(minusIntegerzh_fast);
RTS_FUN(timesIntegerzh_fast);
RTS_FUN(gcdIntegerzh_fast);
RTS_FUN(quotRemIntegerzh_fast);
RTS_FUN(quotIntegerzh_fast);
RTS_FUN(remIntegerzh_fast);
RTS_FUN(divExactIntegerzh_fast);
RTS_FUN(divModIntegerzh_fast);

RTS_FUN(cmpIntegerIntzh_fast);
RTS_FUN(cmpIntegerzh_fast);
RTS_FUN(integer2Intzh_fast);
RTS_FUN(integer2Wordzh_fast);
RTS_FUN(gcdIntegerIntzh_fast);
RTS_FUN(gcdIntzh_fast);

RTS_FUN(int2Integerzh_fast);
RTS_FUN(word2Integerzh_fast);

RTS_FUN(decodeFloatzh_fast);
RTS_FUN(decodeDoublezh_fast);

RTS_FUN(andIntegerzh_fast);
RTS_FUN(orIntegerzh_fast);
RTS_FUN(xorIntegerzh_fast);
RTS_FUN(complementIntegerzh_fast);

#ifdef SUPPORT_LONG_LONGS

RTS_FUN(int64ToIntegerzh_fast);
RTS_FUN(word64ToIntegerzh_fast);

#endif

RTS_FUN(unsafeThawArrayzh_fast);
RTS_FUN(newByteArrayzh_fast);
RTS_FUN(newPinnedByteArrayzh_fast);
RTS_FUN(newArrayzh_fast);

RTS_FUN(decodeFloatzh_fast);
RTS_FUN(decodeDoublezh_fast);

RTS_FUN(newMutVarzh_fast);
RTS_FUN(atomicModifyMutVarzh_fast);

RTS_FUN(isEmptyMVarzh_fast);
RTS_FUN(newMVarzh_fast);
RTS_FUN(takeMVarzh_fast);
RTS_FUN(putMVarzh_fast);
RTS_FUN(tryTakeMVarzh_fast);
RTS_FUN(tryPutMVarzh_fast);

RTS_FUN(waitReadzh_fast);
RTS_FUN(waitWritezh_fast);
RTS_FUN(delayzh_fast);
#ifdef mingw32_HOST_OS
RTS_FUN(asyncReadzh_fast);
RTS_FUN(asyncWritezh_fast);
RTS_FUN(asyncDoProczh_fast);
#endif

RTS_FUN(catchzh_fast);
RTS_FUN(raisezh_fast);
RTS_FUN(raiseIOzh_fast);

RTS_FUN(makeStableNamezh_fast);
RTS_FUN(makeStablePtrzh_fast);
RTS_FUN(deRefStablePtrzh_fast);

RTS_FUN(forkzh_fast);
RTS_FUN(yieldzh_fast);
RTS_FUN(killThreadzh_fast);
RTS_FUN(blockAsyncExceptionszh_fast);
RTS_FUN(unblockAsyncExceptionszh_fast);
RTS_FUN(myThreadIdzh_fast);
RTS_FUN(labelThreadzh_fast);
RTS_FUN(isCurrentThreadBoundzh_fast);

RTS_FUN(mkWeakzh_fast);
RTS_FUN(finalizzeWeakzh_fast);
RTS_FUN(deRefWeakzh_fast);

RTS_FUN(newBCOzh_fast);
RTS_FUN(mkApUpd0zh_fast);

RTS_FUN(retryzh_fast);
RTS_FUN(catchRetryzh_fast);
RTS_FUN(catchSTMzh_fast);
RTS_FUN(atomicallyzh_fast);
RTS_FUN(newTVarzh_fast);
RTS_FUN(readTVarzh_fast);
RTS_FUN(writeTVarzh_fast);

#endif /* STGMISCCLOSURES_H */
