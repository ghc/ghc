/* -----------------------------------------------------------------------------
 * $Id: StgMiscClosures.h,v 1.15 1999/11/02 15:05:53 simonmar Exp $
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

STGFUN(IND_entry);
STGFUN(IND_STATIC_entry);
STGFUN(IND_PERM_entry);
STGFUN(IND_OLDGEN_entry);
STGFUN(IND_OLDGEN_PERM_entry);
STGFUN(CAF_UNENTERED_entry);
STGFUN(CAF_ENTERED_entry);
STGFUN(CAF_BLACKHOLE_entry);
STGFUN(BLACKHOLE_entry);
STGFUN(BLACKHOLE_BQ_entry);
#ifdef SMP
STGFUN(WHITEHOLE_entry);
#endif
#ifdef TICKY_TICKY
STGFUN(SE_BLACKHOLE_entry);
STGFUN(SE_CAF_BLACKHOLE_entry);
#endif
STGFUN(BCO_entry);
STGFUN(EVACUATED_entry);
STGFUN(FOREIGN_entry);
STGFUN(WEAK_entry);
STGFUN(NO_FINALIZER_entry);
STGFUN(DEAD_WEAK_entry);
STGFUN(STABLE_NAME_entry);
STGFUN(TSO_entry);
STGFUN(FULL_MVAR_entry);
STGFUN(EMPTY_MVAR_entry);
STGFUN(ARR_WORDS_entry);
STGFUN(MUT_ARR_PTRS_entry);
STGFUN(MUT_ARR_PTRS_FROZEN_entry);
STGFUN(MUT_VAR_entry);
STGFUN(END_TSO_QUEUE_entry);
STGFUN(MUT_CONS_entry);
STGFUN(END_MUT_LIST_entry);
STGFUN(dummy_ret_entry);

/* info tables */

extern DLL_IMPORT_RTS const StgInfoTable IND_info;
extern DLL_IMPORT_RTS const StgInfoTable IND_STATIC_info;
extern DLL_IMPORT_RTS const StgInfoTable IND_PERM_info;
extern DLL_IMPORT_RTS const StgInfoTable IND_OLDGEN_info;
extern DLL_IMPORT_RTS const StgInfoTable IND_OLDGEN_PERM_info;
extern DLL_IMPORT_RTS const StgInfoTable CAF_UNENTERED_info;
extern DLL_IMPORT_RTS const StgInfoTable CAF_ENTERED_info;
extern DLL_IMPORT_RTS const StgInfoTable CAF_BLACKHOLE_info;
extern DLL_IMPORT_RTS const StgInfoTable BLACKHOLE_info;
extern DLL_IMPORT_RTS const StgInfoTable BLACKHOLE_BQ_info;
#ifdef SMP
extern DLL_IMPORT_RTS const StgInfoTable WHITEHOLE_info;
#endif
#ifdef TICKY_TICKY
extern DLL_IMPORT_RTS const StgInfoTable SE_BLACKHOLE_info;
extern DLL_IMPORT_RTS const StgInfoTable SE_CAF_BLACKHOLE_info;
#endif
extern DLL_IMPORT_RTS const StgInfoTable BCO_info;
extern DLL_IMPORT_RTS const StgInfoTable EVACUATED_info;
extern DLL_IMPORT_RTS const StgInfoTable FOREIGN_info;
extern DLL_IMPORT_RTS const StgInfoTable WEAK_info;
extern DLL_IMPORT_RTS const StgInfoTable DEAD_WEAK_info;
extern DLL_IMPORT_RTS const StgInfoTable STABLE_NAME_info;
extern DLL_IMPORT_RTS const StgInfoTable FULL_MVAR_info;
extern DLL_IMPORT_RTS const StgInfoTable EMPTY_MVAR_info;
extern DLL_IMPORT_RTS const StgInfoTable TSO_info;
extern DLL_IMPORT_RTS const StgInfoTable ARR_WORDS_info;
extern DLL_IMPORT_RTS const StgInfoTable MUT_ARR_WORDS_info;
extern DLL_IMPORT_RTS const StgInfoTable MUT_ARR_PTRS_info;
extern DLL_IMPORT_RTS const StgInfoTable MUT_ARR_PTRS_FROZEN_info;
extern DLL_IMPORT_RTS const StgInfoTable MUT_VAR_info;
extern DLL_IMPORT_RTS const StgInfoTable END_TSO_QUEUE_info;
extern DLL_IMPORT_RTS const StgInfoTable MUT_CONS_info;
extern DLL_IMPORT_RTS const StgInfoTable END_MUT_LIST_info;
extern DLL_IMPORT_RTS const StgInfoTable catch_info;
extern DLL_IMPORT_RTS const StgInfoTable seq_info;
extern DLL_IMPORT_RTS const StgInfoTable dummy_ret_info;

#ifdef INTERPRETER

EXTFUN(Hugs_CONSTR_entry);

extern const vec_info_8 ret_bco_info;

#endif /* INTERPRETER */

/* closures */

extern DLL_IMPORT_DATA StgClosure END_TSO_QUEUE_closure;
extern DLL_IMPORT_DATA StgClosure END_MUT_LIST_closure;
extern DLL_IMPORT_DATA StgClosure NO_FINALIZER_closure;
extern DLL_IMPORT_DATA StgClosure dummy_ret_closure;
extern DLL_IMPORT_DATA StgClosure forceIO_closure;

extern DLL_IMPORT_DATA StgIntCharlikeClosure CHARLIKE_closure[];
extern DLL_IMPORT_DATA StgIntCharlikeClosure INTLIKE_closure[];

/* standard entry points */

extern StgFun stg_error_entry;

  /* (see also below  -- KSW 1998-12) */

/* standard selector thunks */

#ifdef COMPILING_RTS
#define EI__ EI_
#else
#define EI__ EDI_
#endif

EI__ __sel_0_upd_info;
EI__ __sel_1_upd_info;
EI__ __sel_2_upd_info;
EI__ __sel_3_upd_info;
EI__ __sel_4_upd_info;
EI__ __sel_5_upd_info;
EI__ __sel_6_upd_info;
EI__ __sel_7_upd_info;
EI__ __sel_8_upd_info;
EI__ __sel_8_upd_info;
EI__ __sel_9_upd_info;
EI__ __sel_10_upd_info;
EI__ __sel_11_upd_info;
EI__ __sel_12_upd_info;
EI__ __sel_13_upd_info;
EI__ __sel_14_upd_info;
EI__ __sel_15_upd_info;

EI__ __sel_0_noupd_info;
EI__ __sel_1_noupd_info;
EI__ __sel_2_noupd_info;
EI__ __sel_3_noupd_info;
EI__ __sel_4_noupd_info;
EI__ __sel_5_noupd_info;
EI__ __sel_6_noupd_info;
EI__ __sel_7_noupd_info;
EI__ __sel_8_noupd_info;
EI__ __sel_9_noupd_info;
EI__ __sel_10_noupd_info;
EI__ __sel_11_noupd_info;
EI__ __sel_12_noupd_info;
EI__ __sel_13_noupd_info;
EI__ __sel_14_noupd_info;
EI__ __sel_15_noupd_info;

  /* and their standard entry points  -- KSW 1998-12 */

EF_(__sel_0_upd_entry);
EF_(__sel_1_upd_entry);
EF_(__sel_2_upd_entry);
EF_(__sel_3_upd_entry);
EF_(__sel_4_upd_entry);
EF_(__sel_5_upd_entry);
EF_(__sel_6_upd_entry);
EF_(__sel_7_upd_entry);
EF_(__sel_8_upd_entry);
EF_(__sel_8_upd_entry);
EF_(__sel_9_upd_entry);
EF_(__sel_10_upd_entry);
EF_(__sel_11_upd_entry);
EF_(__sel_12_upd_entry);
EF_(__sel_13_upd_entry);
EF_(__sel_14_upd_entry);
EF_(__sel_15_upd_entry);

EF_(__sel_0_noupd_entry);
EF_(__sel_1_noupd_entry);
EF_(__sel_2_noupd_entry);
EF_(__sel_3_noupd_entry);
EF_(__sel_4_noupd_entry);
EF_(__sel_5_noupd_entry);
EF_(__sel_6_noupd_entry);
EF_(__sel_7_noupd_entry);
EF_(__sel_8_noupd_entry);
EF_(__sel_9_noupd_entry);
EF_(__sel_10_noupd_entry);
EF_(__sel_11_noupd_entry);
EF_(__sel_12_noupd_entry);
EF_(__sel_13_noupd_entry);
EF_(__sel_14_noupd_entry);
EF_(__sel_15_noupd_entry);

/* standard ap thunks */

EI__ __ap_1_upd_info;
EI__ __ap_2_upd_info;
EI__ __ap_3_upd_info;
EI__ __ap_4_upd_info;
EI__ __ap_5_upd_info;
EI__ __ap_6_upd_info;
EI__ __ap_7_upd_info;
EI__ __ap_8_upd_info;

