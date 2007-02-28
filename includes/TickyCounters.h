/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2007
 *
 * Declarations for counters used by ticky-ticky profiling.
 *----------------------------------------------------------------------------- */


#ifndef TICKYCOUNTERS_H
#define TICKYCOUNTERS_H

/* These should probably be automatically generated in order to
   keep them consistent with the macros that use them (which are
   defined in Cmm.h. */

#ifdef TICKY_TICKY
/* same trick as in the former StgTicky.h: recycle the same declarations
   for both extern decls (which are included everywhere)
   and initializations (which only happen once) */
#ifdef TICKY_C
#define INIT(ializer) = ializer
#define EXTERN
#else
#define INIT(ializer)
#define EXTERN extern
#endif


/* Here are all the counter declarations: */

EXTERN StgInt ENT_VIA_NODE_ctr INIT(0);
EXTERN StgInt ENT_STATIC_THK_ctr INIT(0);
EXTERN StgInt ENT_DYN_THK_ctr INIT(0);
EXTERN StgInt ENT_STATIC_FUN_DIRECT_ctr INIT(0);
EXTERN StgInt ENT_DYN_FUN_DIRECT_ctr INIT(0);
EXTERN StgInt ENT_STATIC_CON_ctr INIT(0);
EXTERN StgInt ENT_DYN_CON_ctr INIT(0);
EXTERN StgInt ENT_STATIC_IND_ctr INIT(0);
EXTERN StgInt ENT_DYN_IND_ctr INIT(0);
EXTERN StgInt ENT_PERM_IND_ctr INIT(0);
EXTERN StgInt ENT_PAP_ctr INIT(0);
EXTERN StgInt ENT_AP_ctr INIT(0);
EXTERN StgInt ENT_AP_STACK_ctr INIT(0);
EXTERN StgInt ENT_BH_ctr INIT(0);

EXTERN StgInt UNKNOWN_CALL_ctr INIT(0);

EXTERN StgInt SLOW_CALL_v_ctr INIT(0);
EXTERN StgInt SLOW_CALL_f_ctr INIT(0);
EXTERN StgInt SLOW_CALL_d_ctr INIT(0);
EXTERN StgInt SLOW_CALL_l_ctr INIT(0);
EXTERN StgInt SLOW_CALL_n_ctr INIT(0);
EXTERN StgInt SLOW_CALL_p_ctr INIT(0);
EXTERN StgInt SLOW_CALL_pv_ctr INIT(0);
EXTERN StgInt SLOW_CALL_pp_ctr INIT(0);
EXTERN StgInt SLOW_CALL_ppv_ctr INIT(0);
EXTERN StgInt SLOW_CALL_ppp_ctr INIT(0);
EXTERN StgInt SLOW_CALL_pppv_ctr INIT(0);
EXTERN StgInt SLOW_CALL_pppp_ctr INIT(0);
EXTERN StgInt SLOW_CALL_ppppp_ctr INIT(0);
EXTERN StgInt SLOW_CALL_pppppp_ctr INIT(0);
EXTERN StgInt SLOW_CALL_OTHER_ctr INIT(0);

EXTERN StgInt ticky_slow_call_unevald;
EXTERN StgInt SLOW_CALL_ctr INIT(0);
EXTERN StgInt MULTI_CHUNK_SLOW_CALL_ctr INIT(0);
EXTERN StgInt MULTI_CHUNK_SLOW_CALL_CHUNKS_ctr INIT(0);
EXTERN StgInt KNOWN_CALL_ctr INIT(0);
EXTERN StgInt KNOWN_CALL_TOO_FEW_ARGS_ctr INIT(0);
EXTERN StgInt KNOWN_CALL_EXTRA_ARGS_ctr INIT(0);
EXTERN StgInt SLOW_CALL_FUN_TOO_FEW_ctr INIT(0);
EXTERN StgInt SLOW_CALL_FUN_CORRECT_ctr INIT(0);
EXTERN StgInt SLOW_CALL_FUN_TOO_MANY_ctr INIT(0);
EXTERN StgInt SLOW_CALL_PAP_TOO_FEW_ctr INIT(0);
EXTERN StgInt SLOW_CALL_PAP_CORRECT_ctr INIT(0);
EXTERN StgInt SLOW_CALL_PAP_TOO_MANY_ctr INIT(0);
EXTERN StgInt SLOW_CALL_UNEVALD_ctr INIT(0);


EXTERN StgInt UPDF_OMITTED_ctr INIT(0);
EXTERN StgInt UPDF_PUSHED_ctr INIT(0);
EXTERN StgInt CATCHF_PUSHED_ctr INIT(0);
EXTERN StgInt UPDF_RCC_PUSHED_ctr INIT(0);
EXTERN StgInt UPDF_RCC_OMITTED_ctr INIT(0);

EXTERN StgInt UPD_SQUEEZED_ctr INIT(0);
EXTERN StgInt UPD_CON_IN_NEW_ctr INIT(0);
EXTERN StgInt UPD_CON_IN_PLACE_ctr INIT(0);
EXTERN StgInt UPD_PAP_IN_NEW_ctr INIT(0);
EXTERN StgInt UPD_PAP_IN_PLACE_ctr INIT(0);

EXTERN StgInt ALLOC_HEAP_ctr INIT(0);
EXTERN StgInt ALLOC_HEAP_tot;

EXTERN StgInt ALLOC_FUN_ctr INIT(0);
EXTERN StgInt ALLOC_FUN_adm;
EXTERN StgInt ALLOC_FUN_gds;
EXTERN StgInt ALLOC_FUN_slp;

EXTERN StgInt UPD_NEW_IND_ctr INIT(0);
EXTERN StgInt UPD_NEW_PERM_IND_ctr INIT(0);
EXTERN StgInt UPD_OLD_IND_ctr INIT(0);
EXTERN StgInt UPD_OLD_PERM_IND_ctr INIT(0);

EXTERN StgInt UPD_BH_UPDATABLE_ctr INIT(0);
EXTERN StgInt UPD_BH_SINGLE_ENTRY_ctr INIT(0);
EXTERN StgInt UPD_CAF_BH_UPDATABLE_ctr INIT(0);
EXTERN StgInt UPD_CAF_BH_SINGLE_ENTRY_ctr INIT(0);

EXTERN StgInt GC_SEL_ABANDONED_ctr INIT(0);
EXTERN StgInt GC_SEL_MINOR_ctr INIT(0);
EXTERN StgInt GC_SEL_MAJOR_ctr INIT(0);

EXTERN StgInt GC_FAILED_PROMOTION_ctr INIT(0);

EXTERN StgInt GC_WORDS_COPIED_ctr INIT(0);

EXTERN StgInt ALLOC_UP_THK_ctr INIT(0);
EXTERN StgInt ALLOC_SE_THK_ctr INIT(0);
EXTERN StgInt ALLOC_THK_adm INIT(0);
EXTERN StgInt ALLOC_THK_gds INIT(0);
EXTERN StgInt ALLOC_THK_slp INIT(0);

EXTERN StgInt ALLOC_CON_ctr INIT(0);
EXTERN StgInt ALLOC_CON_adm INIT(0);
EXTERN StgInt ALLOC_CON_gds INIT(0);
EXTERN StgInt ALLOC_CON_slp INIT(0);

EXTERN StgInt ALLOC_TUP_ctr INIT(0);
EXTERN StgInt ALLOC_TUP_adm INIT(0);
EXTERN StgInt ALLOC_TUP_gds INIT(0);
EXTERN StgInt ALLOC_TUP_slp INIT(0);

EXTERN StgInt ALLOC_BH_ctr INIT(0);
EXTERN StgInt ALLOC_BH_adm INIT(0);
EXTERN StgInt ALLOC_BH_gds INIT(0);
EXTERN StgInt ALLOC_BH_slp INIT(0);

EXTERN StgInt ALLOC_PRIM_ctr INIT(0);
EXTERN StgInt ALLOC_PRIM_adm INIT(0);
EXTERN StgInt ALLOC_PRIM_gds INIT(0);
EXTERN StgInt ALLOC_PRIM_slp INIT(0);

EXTERN StgInt ALLOC_PAP_ctr INIT(0);
EXTERN StgInt ALLOC_PAP_adm INIT(0);
EXTERN StgInt ALLOC_PAP_gds INIT(0);
EXTERN StgInt ALLOC_PAP_slp INIT(0);

EXTERN StgInt ALLOC_TSO_ctr INIT(0);
EXTERN StgInt ALLOC_TSO_adm INIT(0);
EXTERN StgInt ALLOC_TSO_gds INIT(0);
EXTERN StgInt ALLOC_TSO_slp INIT(0);

EXTERN StgInt RET_NEW_ctr INIT(0);
EXTERN StgInt RET_OLD_ctr INIT(0);
EXTERN StgInt RET_UNBOXED_TUP_ctr INIT(0);

EXTERN StgInt RET_SEMI_loads_avoided INIT(0);

/* End of counter declarations. */

/* Here are stubs for a bunch of macros that aren't 
   implemented yet. */

#define TICK_ALLOC_FUN(g,s)
#define TICK_ALLOC_CON(g,s)
#define TICK_ALLOC_TUP(g,s)
#define TICK_ALLOC_BH(g,s)
#define TICK_ALLOC_PAP(g,s)
#define TICK_ALLOC_FMBQ(a,g,s)
#define TICK_ALLOC_FME(a,g,s)
#define TICK_ALLOC_BF(a,g,s)
#define TICK_ALLOC_PRIM2(w)

#endif /* TICKY_TICKY */

/* This is ugly, but the story is:
   We got rid of StgTicky.h, which was previously
   defining these macros for the benefit of C code
   so, we define them here instead (to be no-ops).
   (since those macros are only defined in Cmm.h) 

   Note that these macros must be defined whether
   TICKY_TICKY is defined or not. */
  
#ifndef CMINUSMINUS
#define TICK_ALLOC_PRIM(x,y,z)
#define TICK_UPD_OLD_IND()
#define TICK_UPD_NEW_IND()
#define TICK_UPD_SQUEEZED()
#define TICK_ALLOC_HEAP_NOCTR(x)
#define TICK_GC_WORDS_COPIED(x)
#define TICK_GC_FAILED_PROMOTION()
#define TICK_ALLOC_TSO(g,s)
#define TICK_ALLOC_UP_THK(g,s)
#define TICK_ALLOC_SE_THK(g,s)

#endif


#endif /* TICKYCOUNTERS_H */
