/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Declarations for various symbols exported by the RTS.
 *
 * ToDo: many of the symbols in here don't need to be exported, but
 * our Cmm code generator doesn't know how to generate local symbols
 * for the RTS bits (it assumes all RTS symbols are external).
 *
 * See wiki:commentary/compiler/backends/ppr-c#prototypes
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * --------------------------------------------------------------------------*/

#pragma once

#if IN_STG_CODE
#  define RTS_RET_INFO(i)   extern const W_(i)[]
#  define RTS_FUN_INFO(i)   extern const W_(i)[]
#  define RTS_THUNK_INFO(i) extern const W_(i)[]
#  define RTS_INFO(i)       extern const W_(i)[]
#  define RTS_CLOSURE(i)    extern W_(i)[]
#  define RTS_FUN_DECL(f)   extern DLL_IMPORT_RTS StgFunPtr f(void)
#else
#  define RTS_RET_INFO(i)   extern DLL_IMPORT_RTS const StgRetInfoTable i
#  define RTS_FUN_INFO(i)   extern DLL_IMPORT_RTS const StgFunInfoTable i
#  define RTS_THUNK_INFO(i) extern DLL_IMPORT_RTS const StgThunkInfoTable i
#  define RTS_INFO(i)       extern DLL_IMPORT_RTS const StgInfoTable i
#  define RTS_CLOSURE(i)    extern DLL_IMPORT_RTS StgClosure i
#  define RTS_FUN_DECL(f)   extern DLL_IMPORT_RTS StgFunPtr f(void)
#endif

#if defined(TABLES_NEXT_TO_CODE)
#  define RTS_RET(f)      RTS_INFO(f##_info)
#  define RTS_ENTRY(f)    RTS_INFO(f##_info)
#  define RTS_FUN(f)      RTS_FUN_INFO(f##_info)
#  define RTS_THUNK(f)    RTS_THUNK_INFO(f##_info)
#else
#  define RTS_RET(f)      RTS_INFO(f##_info);  RTS_FUN_DECL(f##_ret)
#  define RTS_ENTRY(f)    RTS_INFO(f##_info);  RTS_FUN_DECL(f##_entry)
#  define RTS_FUN(f)      RTS_FUN_INFO(f##_info); RTS_FUN_DECL(f##_entry)
#  define RTS_THUNK(f)    RTS_THUNK_INFO(f##_info); RTS_FUN_DECL(f##_entry)
#endif

/* Stack frames */
RTS_RET(stg_upd_frame);
RTS_RET(stg_bh_upd_frame);
RTS_RET(stg_marked_upd_frame);
RTS_RET(stg_noupd_frame);
RTS_RET(stg_orig_thunk_info_frame);
RTS_RET(stg_catch_frame);
RTS_RET(stg_catch_retry_frame);
RTS_RET(stg_atomically_frame);
RTS_RET(stg_atomically_waiting_frame);
RTS_RET(stg_catch_stm_frame);
RTS_RET(stg_unmaskAsyncExceptionszh_ret);
RTS_RET(stg_maskUninterruptiblezh_ret);
RTS_RET(stg_maskAsyncExceptionszh_ret);
RTS_RET(stg_stack_underflow_frame);
RTS_RET(stg_keepAlive_frame);
RTS_RET(stg_restore_cccs);
RTS_RET(stg_restore_cccs_eval);
RTS_RET(stg_prompt_frame);

// RTS_FUN(stg_interp_constr1_entry);
// RTS_FUN(stg_interp_constr2_entry);
// RTS_FUN(stg_interp_constr3_entry);
// RTS_FUN(stg_interp_constr4_entry);
// RTS_FUN(stg_interp_constr5_entry);
// RTS_FUN(stg_interp_constr6_entry);
// RTS_FUN(stg_interp_constr7_entry);
//
// This is referenced using the FFI in the compiler (GHC.ByteCode.InfoTable),
// so we can't give it the correct type here because the prototypes
// would clash (FFI references are always declared with type StgWord[]
// in the generated C code).

/* Magic glue code for when compiled code returns a value in R1/F1/D1
   or a VoidRep to the interpreter. */
RTS_RET(stg_ctoi_R1p);
RTS_RET(stg_ctoi_R1n);
RTS_RET(stg_ctoi_F1);
RTS_RET(stg_ctoi_D1);
RTS_RET(stg_ctoi_L1);
RTS_RET(stg_ctoi_V);

RTS_FUN_DECL(stg_ctoi_t);
RTS_RET(stg_ctoi_t0);
RTS_RET(stg_ctoi_t1);
RTS_RET(stg_ctoi_t2);
RTS_RET(stg_ctoi_t3);
RTS_RET(stg_ctoi_t4);
RTS_RET(stg_ctoi_t5);
RTS_RET(stg_ctoi_t6);
RTS_RET(stg_ctoi_t7);
RTS_RET(stg_ctoi_t8);
RTS_RET(stg_ctoi_t9);

RTS_RET(stg_ctoi_t10);
RTS_RET(stg_ctoi_t11);
RTS_RET(stg_ctoi_t12);
RTS_RET(stg_ctoi_t13);
RTS_RET(stg_ctoi_t14);
RTS_RET(stg_ctoi_t15);
RTS_RET(stg_ctoi_t16);
RTS_RET(stg_ctoi_t17);
RTS_RET(stg_ctoi_t18);
RTS_RET(stg_ctoi_t19);

RTS_RET(stg_ctoi_t20);
RTS_RET(stg_ctoi_t21);
RTS_RET(stg_ctoi_t22);
RTS_RET(stg_ctoi_t23);
RTS_RET(stg_ctoi_t24);
RTS_RET(stg_ctoi_t25);
RTS_RET(stg_ctoi_t26);
RTS_RET(stg_ctoi_t27);
RTS_RET(stg_ctoi_t28);
RTS_RET(stg_ctoi_t29);

RTS_RET(stg_ctoi_t30);
RTS_RET(stg_ctoi_t31);
RTS_RET(stg_ctoi_t32);
RTS_RET(stg_ctoi_t33);
RTS_RET(stg_ctoi_t34);
RTS_RET(stg_ctoi_t35);
RTS_RET(stg_ctoi_t36);
RTS_RET(stg_ctoi_t37);
RTS_RET(stg_ctoi_t38);
RTS_RET(stg_ctoi_t39);

RTS_RET(stg_ctoi_t40);
RTS_RET(stg_ctoi_t41);
RTS_RET(stg_ctoi_t42);
RTS_RET(stg_ctoi_t43);
RTS_RET(stg_ctoi_t44);
RTS_RET(stg_ctoi_t45);
RTS_RET(stg_ctoi_t46);
RTS_RET(stg_ctoi_t47);
RTS_RET(stg_ctoi_t48);
RTS_RET(stg_ctoi_t49);

RTS_RET(stg_ctoi_t50);
RTS_RET(stg_ctoi_t51);
RTS_RET(stg_ctoi_t52);
RTS_RET(stg_ctoi_t53);
RTS_RET(stg_ctoi_t54);
RTS_RET(stg_ctoi_t55);
RTS_RET(stg_ctoi_t56);
RTS_RET(stg_ctoi_t57);
RTS_RET(stg_ctoi_t58);
RTS_RET(stg_ctoi_t59);

RTS_RET(stg_ctoi_t60);
RTS_RET(stg_ctoi_t61);
RTS_RET(stg_ctoi_t62);

RTS_RET(stg_primcall);
RTS_RET(stg_apply_interp);
RTS_RET(stg_dead_thread);

RTS_ENTRY(stg_IND);
RTS_ENTRY(stg_IND_STATIC);
RTS_ENTRY(stg_BLACKHOLE);
RTS_ENTRY(stg_CAF_BLACKHOLE);
RTS_ENTRY(__stg_EAGER_BLACKHOLE);
RTS_ENTRY(stg_WHITEHOLE);
RTS_ENTRY(stg_BLOCKING_QUEUE_CLEAN);
RTS_ENTRY(stg_BLOCKING_QUEUE_DIRTY);

RTS_FUN(stg_BCO);
RTS_ENTRY(stg_EVACUATED);
RTS_ENTRY(stg_WEAK);
RTS_ENTRY(stg_DEAD_WEAK);
RTS_ENTRY(stg_C_FINALIZER_LIST);
RTS_ENTRY(stg_STABLE_NAME);
RTS_ENTRY(stg_MVAR_CLEAN);
RTS_ENTRY(stg_MVAR_DIRTY);
RTS_ENTRY(stg_TVAR_CLEAN);
RTS_ENTRY(stg_TVAR_DIRTY);
RTS_ENTRY(stg_TSO);
RTS_ENTRY(stg_STACK);
RTS_ENTRY(stg_RUBBISH_ENTRY);
RTS_ENTRY(stg_ARR_WORDS);
RTS_ENTRY(stg_MUT_ARR_WORDS);
RTS_ENTRY(stg_MUT_ARR_PTRS_CLEAN);
RTS_ENTRY(stg_MUT_ARR_PTRS_DIRTY);
RTS_ENTRY(stg_MUT_ARR_PTRS_FROZEN_CLEAN);
RTS_ENTRY(stg_MUT_ARR_PTRS_FROZEN_DIRTY);
RTS_ENTRY(stg_SMALL_MUT_ARR_PTRS_CLEAN);
RTS_ENTRY(stg_SMALL_MUT_ARR_PTRS_DIRTY);
RTS_ENTRY(stg_SMALL_MUT_ARR_PTRS_FROZEN_CLEAN);
RTS_ENTRY(stg_SMALL_MUT_ARR_PTRS_FROZEN_DIRTY);
RTS_ENTRY(stg_MUT_VAR_CLEAN);
RTS_ENTRY(stg_MUT_VAR_DIRTY);
RTS_ENTRY(stg_END_TSO_QUEUE);
RTS_ENTRY(stg_GCD_CAF);
RTS_ENTRY(stg_STM_AWOKEN);
RTS_ENTRY(stg_MSG_TRY_WAKEUP);
RTS_ENTRY(stg_MSG_THROWTO);
RTS_ENTRY(stg_MSG_BLACKHOLE);
RTS_ENTRY(stg_MSG_CLONE_STACK);
RTS_ENTRY(stg_MSG_NULL);
RTS_ENTRY(stg_MVAR_TSO_QUEUE);
RTS_ENTRY(stg_catch);
RTS_ENTRY(stg_PAP);
RTS_ENTRY(stg_AP);
RTS_ENTRY(stg_AP_NOUPD);
RTS_ENTRY(stg_AP_STACK);
RTS_ENTRY(stg_AP_STACK_NOUPD);
RTS_ENTRY(stg_CONTINUATION);
RTS_ENTRY(stg_PROMPT_TAG);
RTS_ENTRY(stg_dummy_ret);
RTS_ENTRY(stg_raise);
RTS_ENTRY(stg_raise_ret);
RTS_ENTRY(stg_atomically);
RTS_ENTRY(stg_TVAR_WATCH_QUEUE);
RTS_ENTRY(stg_TREC_CHUNK);
RTS_ENTRY(stg_TREC_HEADER);
RTS_ENTRY(stg_END_STM_WATCH_QUEUE);
RTS_ENTRY(stg_END_STM_CHUNK_LIST);
RTS_ENTRY(stg_NO_TREC);
RTS_ENTRY(stg_COMPACT_NFDATA_CLEAN);
RTS_ENTRY(stg_COMPACT_NFDATA_DIRTY);
RTS_ENTRY(stg_SRT_1);
RTS_ENTRY(stg_SRT_2);
RTS_ENTRY(stg_SRT_3);
RTS_ENTRY(stg_SRT_4);
RTS_ENTRY(stg_SRT_5);
RTS_ENTRY(stg_SRT_6);
RTS_ENTRY(stg_SRT_7);
RTS_ENTRY(stg_SRT_8);
RTS_ENTRY(stg_SRT_9);
RTS_ENTRY(stg_SRT_10);
RTS_ENTRY(stg_SRT_11);
RTS_ENTRY(stg_SRT_12);
RTS_ENTRY(stg_SRT_13);
RTS_ENTRY(stg_SRT_14);
RTS_ENTRY(stg_SRT_15);
RTS_ENTRY(stg_SRT_16);

/* closures */

RTS_CLOSURE(stg_END_TSO_QUEUE_closure);
RTS_CLOSURE(stg_STM_AWOKEN_closure);
RTS_CLOSURE(stg_NO_FINALIZER_closure);
RTS_CLOSURE(stg_dummy_ret_closure);
RTS_CLOSURE(stg_forceIO_closure);

RTS_CLOSURE(stg_END_STM_WATCH_QUEUE_closure);
RTS_CLOSURE(stg_END_STM_CHUNK_LIST_closure);
RTS_CLOSURE(stg_NO_TREC_closure);

RTS_ENTRY(stg_NO_FINALIZER);

#if IN_STG_CODE
extern DLL_IMPORT_RTS StgWordArray stg_CHARLIKE_closure;
extern DLL_IMPORT_RTS StgWordArray stg_INTLIKE_closure;
#else
extern DLL_IMPORT_RTS StgIntCharlikeClosure stg_CHARLIKE_closure[];
extern DLL_IMPORT_RTS StgIntCharlikeClosure stg_INTLIKE_closure[];
#endif

/* StgStartup */

RTS_RET(stg_forceIO);
RTS_RET(stg_noforceIO);

/* standard entry points */

/* standard selector thunks */

RTS_ENTRY(stg_sel_0_upd);
RTS_ENTRY(stg_sel_1_upd);
RTS_ENTRY(stg_sel_2_upd);
RTS_ENTRY(stg_sel_3_upd);
RTS_ENTRY(stg_sel_4_upd);
RTS_ENTRY(stg_sel_5_upd);
RTS_ENTRY(stg_sel_6_upd);
RTS_ENTRY(stg_sel_7_upd);
RTS_ENTRY(stg_sel_8_upd);
RTS_ENTRY(stg_sel_9_upd);
RTS_ENTRY(stg_sel_10_upd);
RTS_ENTRY(stg_sel_11_upd);
RTS_ENTRY(stg_sel_12_upd);
RTS_ENTRY(stg_sel_13_upd);
RTS_ENTRY(stg_sel_14_upd);
RTS_ENTRY(stg_sel_15_upd);

RTS_ENTRY(stg_sel_0_noupd);
RTS_ENTRY(stg_sel_1_noupd);
RTS_ENTRY(stg_sel_2_noupd);
RTS_ENTRY(stg_sel_3_noupd);
RTS_ENTRY(stg_sel_4_noupd);
RTS_ENTRY(stg_sel_5_noupd);
RTS_ENTRY(stg_sel_6_noupd);
RTS_ENTRY(stg_sel_7_noupd);
RTS_ENTRY(stg_sel_8_noupd);
RTS_ENTRY(stg_sel_9_noupd);
RTS_ENTRY(stg_sel_10_noupd);
RTS_ENTRY(stg_sel_11_noupd);
RTS_ENTRY(stg_sel_12_noupd);
RTS_ENTRY(stg_sel_13_noupd);
RTS_ENTRY(stg_sel_14_noupd);
RTS_ENTRY(stg_sel_15_noupd);

/* standard ap thunks */

RTS_THUNK(stg_ap_1_upd);
RTS_THUNK(stg_ap_2_upd);
RTS_THUNK(stg_ap_3_upd);
RTS_THUNK(stg_ap_4_upd);
RTS_THUNK(stg_ap_5_upd);
RTS_THUNK(stg_ap_6_upd);
RTS_THUNK(stg_ap_7_upd);

// Standard entry for `unpackCString# str` thunks
RTS_ENTRY(stg_unpack_cstring);
RTS_ENTRY(stg_unpack_cstring_utf8);

/* standard application routines (see also utils/genapply,
 * and GHC.StgToCmm.ArgRep).
 */
RTS_RET(stg_ap_v);
RTS_RET(stg_ap_f);
RTS_RET(stg_ap_d);
RTS_RET(stg_ap_l);
RTS_RET(stg_ap_v16);
RTS_RET(stg_ap_v32);
RTS_RET(stg_ap_v64);
RTS_RET(stg_ap_n);
RTS_RET(stg_ap_p);
RTS_RET(stg_ap_pv);
RTS_RET(stg_ap_pp);
RTS_RET(stg_ap_ppv);
RTS_RET(stg_ap_ppp);
RTS_RET(stg_ap_pppv);
RTS_RET(stg_ap_pppp);
RTS_RET(stg_ap_ppppp);
RTS_RET(stg_ap_pppppp);

RTS_FUN_DECL(stg_ap_0_fast);
RTS_FUN_DECL(stg_ap_v_fast);
RTS_FUN_DECL(stg_ap_f_fast);
RTS_FUN_DECL(stg_ap_d_fast);
RTS_FUN_DECL(stg_ap_l_fast);
RTS_FUN_DECL(stg_ap_v16_fast);
RTS_FUN_DECL(stg_ap_v32_fast);
RTS_FUN_DECL(stg_ap_v64_fast);
RTS_FUN_DECL(stg_ap_n_fast);
RTS_FUN_DECL(stg_ap_p_fast);
RTS_FUN_DECL(stg_ap_pv_fast);
RTS_FUN_DECL(stg_ap_pp_fast);
RTS_FUN_DECL(stg_ap_ppv_fast);
RTS_FUN_DECL(stg_ap_ppp_fast);
RTS_FUN_DECL(stg_ap_pppv_fast);
RTS_FUN_DECL(stg_ap_pppp_fast);
RTS_FUN_DECL(stg_ap_ppppp_fast);
RTS_FUN_DECL(stg_ap_pppppp_fast);
RTS_FUN_DECL(stg_PAP_apply);
RTS_FUN_DECL(stg_CONTINUATION_apply);

/* Defined in separate AutoApply_{V16,V32,V64}.cmm files and imported into AutoApply.cmm */
RTS_FUN_DECL(stg_ap_stk_v16);
RTS_FUN_DECL(stg_ap_stk_v32);
RTS_FUN_DECL(stg_ap_stk_v64);
RTS_FUN_DECL(stg_stk_save_v16);
RTS_FUN_DECL(stg_stk_save_v32);
RTS_FUN_DECL(stg_stk_save_v64);

/* standard GC & stack check entry points, all defined in HeapStackCheck.cmm */

RTS_FUN_DECL(stg_gc_noregs);

RTS_RET(stg_ret_v);
RTS_RET(stg_ret_p);
RTS_RET(stg_ret_n);
RTS_RET(stg_ret_f);
RTS_RET(stg_ret_d);
RTS_RET(stg_ret_l);
RTS_RET(stg_ret_t);

RTS_FUN_DECL(stg_gc_prim);
RTS_FUN_DECL(stg_gc_prim_p);
RTS_FUN_DECL(stg_gc_prim_pp);
RTS_FUN_DECL(stg_gc_prim_n);

RTS_RET(stg_gc_prim_p_ll_ret);
RTS_RET(stg_gc_prim_pp_ll_ret);
RTS_FUN_DECL(stg_gc_prim_p_ll);
RTS_FUN_DECL(stg_gc_prim_pp_ll);

RTS_RET(stg_enter);
RTS_FUN_DECL(__stg_gc_enter_1);

RTS_FUN_DECL(stg_gc_unpt_r1);
RTS_FUN_DECL(stg_gc_unbx_r1);
RTS_FUN_DECL(stg_gc_f1);
RTS_FUN_DECL(stg_gc_d1);
RTS_FUN_DECL(stg_gc_l1);
RTS_FUN_DECL(stg_gc_pp);
RTS_FUN_DECL(stg_gc_ppp);
RTS_FUN_DECL(stg_gc_pppp);

RTS_RET(stg_gc_fun);
RTS_FUN_DECL(__stg_gc_fun);

RTS_FUN_DECL(stg_yield_noregs);
RTS_FUN_DECL(stg_yield_to_interpreter);
RTS_FUN_DECL(stg_block_noregs);
RTS_FUN_DECL(stg_block_blackhole);
RTS_FUN_DECL(stg_block_blackhole_finally);
RTS_FUN_DECL(stg_block_takemvar);
RTS_FUN_DECL(stg_block_readmvar);
RTS_RET(stg_block_takemvar);
RTS_RET(stg_block_readmvar);
RTS_FUN_DECL(stg_block_putmvar);
RTS_RET(stg_block_putmvar);
#if defined(mingw32_HOST_OS)
RTS_FUN_DECL(stg_block_async);
RTS_RET(stg_block_async);
RTS_FUN_DECL(stg_block_async_void);
RTS_RET(stg_block_async_void);
#endif
RTS_FUN_DECL(stg_block_stmwait);
RTS_FUN_DECL(stg_block_throwto);
RTS_RET(stg_block_throwto);

RTS_FUN_DECL(stg_readIOPortzh);
RTS_FUN_DECL(stg_writeIOPortzh);
RTS_FUN_DECL(stg_newIOPortzh);

/* Entry/exit points from StgStartup.cmm */

RTS_RET(stg_stop_thread);

RTS_FUN_DECL(stg_returnToStackTop);
RTS_FUN_DECL(stg_returnToSched);
RTS_FUN_DECL(stg_returnToSchedNotPaused);
RTS_FUN_DECL(stg_returnToSchedButFirst);
RTS_FUN_DECL(stg_threadFinished);

RTS_FUN_DECL(StgReturn);

/* -----------------------------------------------------------------------------
   PrimOps
   -------------------------------------------------------------------------- */

RTS_FUN_DECL(stg_decodeFloatzuIntzh);
RTS_FUN_DECL(stg_decodeDoublezu2Intzh);
RTS_FUN_DECL(stg_decodeDoublezuInt64zh);

RTS_FUN_DECL(stg_unsafeThawArrayzh);
RTS_FUN_DECL(stg_casArrayzh);
RTS_FUN_DECL(stg_newByteArrayzh);
RTS_FUN_DECL(stg_newPinnedByteArrayzh);
RTS_FUN_DECL(stg_newAlignedPinnedByteArrayzh);
RTS_FUN_DECL(stg_isByteArrayPinnedzh);
RTS_FUN_DECL(stg_isMutableByteArrayPinnedzh);
RTS_FUN_DECL(stg_isByteArrayWeaklyPinnedzh);
RTS_FUN_DECL(stg_isMutableByteArrayWeaklyPinnedzh);
RTS_FUN_DECL(stg_shrinkMutableByteArrayzh);
RTS_FUN_DECL(stg_resizzeMutableByteArrayzh);
RTS_FUN_DECL(stg_shrinkSmallMutableArrayzh);
RTS_FUN_DECL(stg_casIntArrayzh);
RTS_FUN_DECL(stg_casInt8Arrayzh);
RTS_FUN_DECL(stg_casInt16Arrayzh);
RTS_FUN_DECL(stg_casInt32Arrayzh);
RTS_FUN_DECL(stg_casInt64Arrayzh);
RTS_FUN_DECL(stg_newArrayzh);
RTS_FUN_DECL(stg_copyArrayzh);
RTS_FUN_DECL(stg_copyMutableArrayzh);
RTS_FUN_DECL(stg_cloneArrayzh);
RTS_FUN_DECL(stg_cloneMutableArrayzh);
RTS_FUN_DECL(stg_freezzeArrayzh);
RTS_FUN_DECL(stg_thawArrayzh);

RTS_FUN_DECL(stg_newSmallArrayzh);
RTS_FUN_DECL(stg_unsafeThawSmallArrayzh);
RTS_FUN_DECL(stg_cloneSmallArrayzh);
RTS_FUN_DECL(stg_cloneSmallMutableArrayzh);
RTS_FUN_DECL(stg_freezzeSmallArrayzh);
RTS_FUN_DECL(stg_thawSmallArrayzh);
RTS_FUN_DECL(stg_copySmallArrayzh);
RTS_FUN_DECL(stg_copySmallMutableArrayzh);
RTS_FUN_DECL(stg_casSmallArrayzh);

RTS_FUN_DECL(stg_newMutVarzh);
RTS_FUN_DECL(stg_atomicModifyMutVar2zh);
RTS_FUN_DECL(stg_atomicModifyMutVarzuzh);
RTS_FUN_DECL(stg_casMutVarzh);

RTS_FUN_DECL(stg_isEmptyMVarzh);
RTS_FUN_DECL(stg_newMVarzh);
RTS_FUN_DECL(stg_takeMVarzh);
RTS_FUN_DECL(stg_putMVarzh);
RTS_FUN_DECL(stg_readMVarzh);
RTS_FUN_DECL(stg_tryTakeMVarzh);
RTS_FUN_DECL(stg_tryPutMVarzh);
RTS_FUN_DECL(stg_tryReadMVarzh);

RTS_FUN_DECL(stg_waitReadzh);
RTS_FUN_DECL(stg_waitWritezh);
RTS_FUN_DECL(stg_delayzh);
#if defined(mingw32_HOST_OS)
RTS_FUN_DECL(stg_asyncReadzh);
RTS_FUN_DECL(stg_asyncWritezh);
RTS_FUN_DECL(stg_asyncDoProczh);
#endif

RTS_FUN_DECL(stg_catchzh);
RTS_FUN_DECL(stg_raisezh);
RTS_FUN_DECL(stg_raiseDivZZerozh);
RTS_FUN_DECL(stg_raiseUnderflowzh);
RTS_FUN_DECL(stg_raiseOverflowzh);
RTS_FUN_DECL(stg_raiseIOzh);
RTS_FUN_DECL(stg_paniczh);
RTS_FUN_DECL(stg_keepAlivezh);
RTS_FUN_DECL(stg_absentErrorzh);

RTS_FUN_DECL(stg_newPromptTagzh);
RTS_FUN_DECL(stg_promptzh);
RTS_FUN_DECL(stg_control0zh);
RTS_FUN_DECL(stg_control0zh_ll);

RTS_FUN_DECL(stg_makeStableNamezh);
RTS_FUN_DECL(stg_makeStablePtrzh);
RTS_FUN_DECL(stg_deRefStablePtrzh);

RTS_FUN_DECL(stg_compactAddzh);
RTS_FUN_DECL(stg_compactAddWithSharingzh);
RTS_FUN_DECL(stg_compactNewzh);
RTS_FUN_DECL(stg_compactAppendzh);
RTS_FUN_DECL(stg_compactResizzezh);
RTS_FUN_DECL(stg_compactGetRootzh);
RTS_FUN_DECL(stg_compactContainszh);
RTS_FUN_DECL(stg_compactContainsAnyzh);
RTS_FUN_DECL(stg_compactGetFirstBlockzh);
RTS_FUN_DECL(stg_compactGetNextBlockzh);
RTS_FUN_DECL(stg_compactAllocateBlockzh);
RTS_FUN_DECL(stg_compactFixupPointerszh);
RTS_FUN_DECL(stg_compactSizzezh);

RTS_FUN_DECL(stg_forkzh);
RTS_FUN_DECL(stg_forkOnzh);
RTS_FUN_DECL(stg_yieldzh);
RTS_FUN_DECL(stg_killMyself);
RTS_FUN_DECL(stg_killThreadzh);
RTS_FUN_DECL(stg_getMaskingStatezh);
RTS_FUN_DECL(stg_maskAsyncExceptionszh);
RTS_FUN_DECL(stg_maskUninterruptiblezh);
RTS_FUN_DECL(stg_unmaskAsyncExceptionszh);
RTS_FUN_DECL(stg_myThreadIdzh);
RTS_FUN_DECL(stg_labelThreadzh);
RTS_FUN_DECL(stg_isCurrentThreadBoundzh);
RTS_FUN_DECL(stg_threadLabelzh);
RTS_FUN_DECL(stg_threadStatuszh);
RTS_FUN_DECL(stg_listThreadszh);

RTS_FUN_DECL(stg_mkWeakzh);
RTS_FUN_DECL(stg_mkWeakNoFinalizzerzh);
RTS_FUN_DECL(stg_mkWeakForeignzh);
RTS_FUN_DECL(stg_addCFinalizzerToWeakzh);
RTS_FUN_DECL(stg_finalizzeWeakzh);
RTS_FUN_DECL(stg_deRefWeakzh);

RTS_FUN_DECL(stg_runRWzh);

RTS_FUN_DECL(stg_newBCOzh);
RTS_FUN_DECL(stg_mkApUpd0zh);

RTS_FUN_DECL(stg_retryzh);
RTS_FUN_DECL(stg_catchRetryzh);
RTS_FUN_DECL(stg_catchSTMzh);
RTS_FUN_DECL(stg_atomicallyzh);
RTS_FUN_DECL(stg_newTVarzh);
RTS_FUN_DECL(stg_readTVarzh);
RTS_FUN_DECL(stg_readTVarIOzh);
RTS_FUN_DECL(stg_writeTVarzh);

RTS_FUN_DECL(stg_unpackClosurezh);
RTS_FUN_DECL(stg_closureSizzezh);
RTS_FUN_DECL(stg_whereFromzh);
RTS_FUN_DECL(stg_getApStackValzh);
RTS_FUN_DECL(stg_getSparkzh);
RTS_FUN_DECL(stg_numSparkszh);

RTS_FUN_DECL(stg_noDuplicatezh);
RTS_FUN(stg_noDuplicate);

RTS_FUN_DECL(stg_clearCCSzh);
RTS_FUN_DECL(stg_traceEventzh);
RTS_FUN_DECL(stg_traceBinaryEventzh);
RTS_FUN_DECL(stg_traceMarkerzh);
RTS_FUN_DECL(stg_getThreadAllocationCounterzh);
RTS_FUN_DECL(stg_setThreadAllocationCounterzh);

RTS_FUN_DECL(stg_castWord64ToDoublezh);
RTS_FUN_DECL(stg_castDoubleToWord64zh);
RTS_FUN_DECL(stg_castWord32ToFloatzh);
RTS_FUN_DECL(stg_castFloatToWord32zh);

/* Other misc stuff */
// See wiki:commentary/compiler/backends/ppr-c#prototypes

#if IN_STG_CODE && !IN_STGCRUN

// Interpreter.c
extern StgWord rts_stop_next_breakpoint[];
extern StgWord rts_stop_on_exception[];
extern StgWord rts_breakpoint_io_action[];

// Schedule.c
extern StgWord RTS_VAR(blocked_queue_hd), RTS_VAR(blocked_queue_tl);
extern StgWord RTS_VAR(sleeping_queue);
extern StgWord RTS_VAR(sched_mutex);

// Apply.cmm
// canned bitmap for each arg type
extern const StgWord stg_arg_bitmaps[];
extern const StgWord stg_ap_stack_entries[];
extern const StgWord stg_stack_save_entries[];

// Storage.c
extern unsigned int RTS_VAR(g0);
extern unsigned int RTS_VAR(large_alloc_lim);
extern StgWord RTS_VAR(atomic_modify_mutvar_mutex);

// RtsFlags
extern StgWord RTS_VAR(RtsFlags); // bogus type

// StablePtr.c
extern StgWord RTS_VAR(stable_ptr_table);

// StableName.c
extern StgWord RTS_VAR(stable_name_table);

// Profiling.c
extern unsigned int RTS_VAR(era);
extern StgWord RTS_VAR(user_era);
extern unsigned int RTS_VAR(entering_PAP);
extern StgWord      CCS_OVERHEAD[];
extern StgWord      CCS_SYSTEM[];

// Calls to these rts functions are generated directly
// by codegen (see GHC.StgToCmm.Prof)
// and don't require (don't emit) forward declarations.
//
// In unregisterised mode (when building via .hc files)
// the calls are ordinary C calls. Functions must be in
// scope and must match prototype assumed by
//    'GHC.StgToCmm.Prof'
// as opposed to real prototype declared in
//    'rts/include/rts/prof/CCS.h'
void enterFunCCS (void *reg, void *ccsfn);
void * pushCostCentre (void *ccs, void *cc);

// Capability.c
extern unsigned int n_capabilities;

/* -----------------------------------------------------------------------------
   Nonmoving GC write barrier
   -------------------------------------------------------------------------- */

#include <rts/NonMoving.h>


#endif
