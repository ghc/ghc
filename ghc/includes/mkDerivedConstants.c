/* --------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1992-2004
 *
 * mkDerivedConstants.c
 *
 * Basically this is a C program that extracts information from the C
 * declarations in the header files (primarily struct field offsets)
 * and generates a header file that can be #included into non-C source
 * containing this information.
 *
 * ------------------------------------------------------------------------*/

#define IN_STG_CODE 0

/*
 * We need offsets of profiled things... better be careful that this
 * doesn't affect the offsets of anything else.
 */
#define PROFILING

#include "Rts.h"
#include "RtsFlags.h"
#include "Storage.h"

#include <stdio.h>

#define str(a,b) #a "_" #b

#define OFFSET(s_type, field) ((unsigned int)&(((s_type*)0)->field))

#if defined(GEN_HASKELL)
#define def_offset(str, offset) \
    printf("oFFSET_" str " = %d::Int\n", offset);
#else
#define def_offset(str, offset) \
    printf("#define OFFSET_" str " %d\n", offset);
#endif

#if defined(GEN_HASKELL)
#define ctype(type) /* nothing */
#else
#define ctype(type) \
    printf("#define SIZEOF_" #type " %d\n", sizeof(type)); 
#endif

#if defined(GEN_HASKELL)
#define field_type_(str, s_type, field) /* nothing */
#else
#define field_type_(str, s_type, field) \
    printf("#define REP_" str " I"); \
    printf("%d\n", sizeof (__typeof__(((((s_type*)0)->field)))) * 8);
#endif

#define field_type(s_type, field) \
    field_type_(str(s_type,field),s_type,field);

#define field_offset_(str, s_type, field) \
    def_offset(str, OFFSET(s_type,field));

#define field_offset(s_type, field) \
    field_offset_(str(s_type,field),s_type,field);

/* An access macro for use in C-- sources. */
#define struct_field_macro(str) \
    printf("#define " str "(__ptr__)  REP_" str "[__ptr__+OFFSET_" str "]\n");

/* Outputs the byte offset and MachRep for a field */
#define struct_field(s_type, field)		\
    field_offset(s_type, field);		\
    field_type(s_type, field);			\
    struct_field_macro(str(s_type,field))

#define struct_field_(str, s_type, field)	\
    field_offset_(str, s_type, field);		\
    field_type_(str, s_type, field);		\
    struct_field_macro(str)

#if defined(GEN_HASKELL)
#define def_size(str, size) \
    printf("sIZEOF_" str " = %d::Int\n", size);
#else
#define def_size(str, size) \
    printf("#define SIZEOF_" str " %d\n", size);
#endif

#if defined(GEN_HASKELL)
#define def_closure_size(str, size) /* nothing */
#else
#define def_closure_size(str, size) \
    printf("#define SIZEOF_" str " (SIZEOF_StgHeader+%d)\n", size);
#endif

#if defined(GEN_HASKELL)
#define def_thunk_size(str, size) /* nothing */
#else
#define def_thunk_size(str, size) \
    printf("#define SIZEOF_" str " (SIZEOF_StgThunkHeader+%d)\n", size);
#endif

#define struct_size(s_type) \
    def_size(#s_type, sizeof(s_type));

/*
 * Size of a closure type, minus the header, named SIZEOF_<type>_NoHdr
 * Also, we #define SIZEOF_<type> to be the size of the whole closure for .cmm.
 */
#define closure_size(s_type) \
    def_size(#s_type "_NoHdr", sizeof(s_type) - sizeof(StgHeader)); \
    def_closure_size(#s_type, sizeof(s_type) - sizeof(StgHeader));

#define thunk_size(s_type) \
    def_size(#s_type "_NoHdr", sizeof(s_type) - sizeof(StgHeader)); \
    def_thunk_size(#s_type, sizeof(s_type) - sizeof(StgHeader));

/* An access macro for use in C-- sources. */
#define closure_field_macro(str) \
    printf("#define " str "(__ptr__)  REP_" str "[__ptr__+SIZEOF_StgHeader+OFFSET_" str "]\n");

#define thunk_field_macro(str) \
    printf("#define " str "(__ptr__)  REP_" str "[__ptr__+SIZEOF_StgThunkHeader+OFFSET_" str "]\n");

#define closure_field_offset_(str, s_type,field) \
    def_offset(str, OFFSET(s_type,field) - sizeof(StgHeader));

#define thunk_field_offset_(str, s_type, field) \
    closure_field_offset_(str, s_type, field)

#define closure_field_offset(s_type,field) \
    closure_field_offset_(str(s_type,field),s_type,field)

#define thunk_field_offset(s_type,field) \
    thunk_field_offset_(str(s_type,field),s_type,field)

#define closure_payload_macro(str) \
    printf("#define " str "(__ptr__,__ix__)  W_[__ptr__+SIZEOF_StgHeader+OFFSET_" str " + WDS(__ix__)]\n");

#define thunk_payload_macro(str) \
    printf("#define " str "(__ptr__,__ix__)  W_[__ptr__+SIZEOF_StgThunkHeader+OFFSET_" str " + WDS(__ix__)]\n");

#define closure_payload(s_type,field) \
    closure_field_offset_(str(s_type,field),s_type,field); \
    closure_payload_macro(str(s_type,field));

#define thunk_payload(s_type,field) \
    thunk_field_offset_(str(s_type,field),s_type,field); \
    thunk_payload_macro(str(s_type,field));

/* Byte offset and MachRep for a closure field, minus the header */
#define closure_field(s_type, field) \
    closure_field_offset(s_type,field) \
    field_type(s_type, field); \
    closure_field_macro(str(s_type,field))

#define thunk_field(s_type, field) \
    thunk_field_offset(s_type,field) \
    field_type(s_type, field); \
    thunk_field_macro(str(s_type,field))

/* Byte offset and MachRep for a closure field, minus the header */
#define closure_field_(str, s_type, field) \
    closure_field_offset_(str,s_type,field) \
    field_type_(str, s_type, field); \
    closure_field_macro(str)

#define thunk_field_(str, s_type, field) \
    thunk_field_offset_(str,s_type,field) \
    field_type_(str, s_type, field); \
    thunk_field_macro(str)
/*
 * Byte offset and MachRep for a TSO field, minus the header and
 * variable prof bit.
 */
#define tso_offset(s_type, field) \
    def_offset(str(s_type,field), OFFSET(s_type,field) - sizeof(StgHeader) - sizeof(StgTSOProfInfo));

#define tso_field_macro(str) \
    printf("#define " str "(__ptr__)  REP_" str "[__ptr__+SIZEOF_StgHeader+SIZEOF_OPT_StgTSOProfInfo+SIZEOF_OPT_StgTSOParInfo+SIZEOF_OPT_StgTSOGranInfo+SIZEOF_OPT_StgTSODistInfo+OFFSET_" str "]\n");

#define tso_field(s_type, field)		\
    tso_offset(s_type, field);			\
    field_type(s_type, field);			\
    tso_field_macro(str(s_type,field))

#define opt_struct_size(s_type, option)					\
    printf("#ifdef " #option "\n");					\
    printf("#define SIZEOF_OPT_" #s_type " SIZEOF_" #s_type "\n");	\
    printf("#else\n");							\
    printf("#define SIZEOF_OPT_" #s_type " 0\n");			\
    printf("#endif\n\n");

#define FUN_OFFSET(sym) (OFFSET(Capability,f.sym) - OFFSET(Capability,r))


int
main(int argc, char *argv[])
{
#ifndef GEN_HASKELL
    printf("/* This file is created automatically.  Do not edit by hand.*/\n\n");

    printf("#define STD_HDR_SIZE   %d\n", sizeofW(StgHeader) - sizeofW(StgProfHeader));
    /* grrr.. PROFILING is on so we need to subtract sizeofW(StgProfHeader) */
    printf("#define PROF_HDR_SIZE  %d\n", sizeofW(StgProfHeader));
    printf("#define GRAN_HDR_SIZE  %d\n", sizeofW(StgGranHeader));

    printf("#define STD_ITBL_SIZE   %d\n", sizeofW(StgInfoTable));
    printf("#define RET_ITBL_SIZE   %d\n", sizeofW(StgRetInfoTable) - sizeofW(StgInfoTable));
    printf("#define PROF_ITBL_SIZE  %d\n", sizeofW(StgProfInfo));

    printf("#define GRAN_ITBL_SIZE  %d\n", 0);
    printf("#define TICKY_ITBL_SIZE %d\n", 0);

    printf("#define BLOCK_SIZE   %d\n", BLOCK_SIZE);
    printf("#define MBLOCK_SIZE   %d\n", MBLOCK_SIZE);  

    printf("\n\n");
#endif

    field_offset(StgRegTable, rR1);
    field_offset(StgRegTable, rR2);
    field_offset(StgRegTable, rR3);
    field_offset(StgRegTable, rR4);
    field_offset(StgRegTable, rR5);
    field_offset(StgRegTable, rR6);
    field_offset(StgRegTable, rR7);
    field_offset(StgRegTable, rR8);
    field_offset(StgRegTable, rR9);
    field_offset(StgRegTable, rR10);
    field_offset(StgRegTable, rF1);
    field_offset(StgRegTable, rF2);
    field_offset(StgRegTable, rF3);
    field_offset(StgRegTable, rF4);
    field_offset(StgRegTable, rD1);
    field_offset(StgRegTable, rD2);
    field_offset(StgRegTable, rL1);
    field_offset(StgRegTable, rSp);
    field_offset(StgRegTable, rSpLim);
    field_offset(StgRegTable, rHp);
    field_offset(StgRegTable, rHpLim);
    field_offset(StgRegTable, rCurrentTSO);
    field_offset(StgRegTable, rCurrentNursery);
    field_offset(StgRegTable, rHpAlloc);

    // Needed for SMP builds
    field_offset(StgRegTable, rmp_tmp_w);
    field_offset(StgRegTable, rmp_tmp1);
    field_offset(StgRegTable, rmp_tmp2);
    field_offset(StgRegTable, rmp_result1);
    field_offset(StgRegTable, rmp_result2);

    def_offset("stgGCEnter1", FUN_OFFSET(stgGCEnter1));
    def_offset("stgGCFun", FUN_OFFSET(stgGCFun));

    field_offset(Capability, r);

    struct_field(bdescr, start);
    struct_field(bdescr, free);
    struct_field(bdescr, blocks);
    struct_field(bdescr, gen_no);
    struct_field(bdescr, link);

    struct_size(generation);
    struct_field(generation, mut_list);

    struct_size(CostCentreStack);
    struct_field(CostCentreStack, ccsID);
    struct_field(CostCentreStack, mem_alloc);
    struct_field(CostCentreStack, scc_count);
    struct_field(CostCentreStack, prevStack);

    struct_field(CostCentre, ccID);
    struct_field(CostCentre, link);

    struct_field(StgHeader, info);
    struct_field_("StgHeader_ccs",  StgHeader, prof.ccs);
    struct_field_("StgHeader_ldvw", StgHeader, prof.hp.ldvw);

    struct_size(StgSMPThunkHeader);

    closure_payload(StgClosure,payload);

    struct_field(StgEntCounter, allocs);
    struct_field(StgEntCounter, registeredp);
    struct_field(StgEntCounter, link);
    
    closure_size(StgUpdateFrame);
    closure_size(StgCatchFrame);
    closure_size(StgStopFrame);

    closure_size(StgMutArrPtrs);
    closure_field(StgMutArrPtrs, ptrs);

    closure_size(StgArrWords);
    closure_field(StgArrWords, words);
    closure_payload(StgArrWords, payload);

    closure_field(StgTSO, link);
    closure_field(StgTSO, global_link);
    closure_field(StgTSO, what_next);
    closure_field(StgTSO, why_blocked);
    closure_field(StgTSO, block_info);
    closure_field(StgTSO, blocked_exceptions);
    closure_field(StgTSO, id);
    closure_field(StgTSO, saved_errno);
    closure_field(StgTSO, trec);
    closure_field_("StgTSO_CCCS", StgTSO, prof.CCCS);
    tso_field(StgTSO, sp);
    tso_offset(StgTSO, stack);
    tso_field(StgTSO, stack_size);

    struct_size(StgTSOProfInfo);
    struct_size(StgTSOParInfo);
    struct_size(StgTSOGranInfo);
    struct_size(StgTSODistInfo);

    opt_struct_size(StgTSOProfInfo,PROFILING);
    opt_struct_size(StgTSOParInfo,PAR);
    opt_struct_size(StgTSOGranInfo,GRAN);
    opt_struct_size(StgTSODistInfo,DIST);

    closure_field(StgUpdateFrame, updatee);

    closure_field(StgCatchFrame, handler);
    closure_field(StgCatchFrame, exceptions_blocked);

    closure_size(StgPAP);
    closure_field(StgPAP, n_args);
    closure_field(StgPAP, fun);
    closure_field(StgPAP, arity);
    closure_payload(StgPAP, payload);

    closure_size(StgAP);
    closure_field(StgAP, n_args);
    closure_field(StgAP, fun);
    closure_payload(StgAP, payload);

    thunk_size(StgAP_STACK);
    thunk_field(StgAP_STACK, size);
    thunk_field(StgAP_STACK, fun);
    thunk_payload(StgAP_STACK, payload);

    closure_field(StgInd, indirectee);

    closure_size(StgMutVar);
    closure_field(StgMutVar, var);

    closure_size(StgAtomicallyFrame);
    closure_field(StgAtomicallyFrame, waiting);
    closure_field(StgAtomicallyFrame, code);

    closure_size(StgCatchSTMFrame);
    closure_field(StgCatchSTMFrame, handler);

    closure_size(StgCatchRetryFrame);
    closure_field(StgCatchRetryFrame, running_alt_code);
    closure_field(StgCatchRetryFrame, first_code);
    closure_field(StgCatchRetryFrame, alt_code);
    closure_field(StgCatchRetryFrame, first_code_trec);

    closure_size(StgForeignObj);
    closure_field(StgForeignObj,data);

    closure_size(StgWeak);
    closure_field(StgWeak,link);
    closure_field(StgWeak,key);
    closure_field(StgWeak,value);
    closure_field(StgWeak,finalizer);

    closure_size(StgDeadWeak);
    closure_field(StgDeadWeak,link);

    closure_size(StgMVar);
    closure_field(StgMVar,head);
    closure_field(StgMVar,tail);
    closure_field(StgMVar,value);

    closure_size(StgTVar);
    closure_field(StgTVar,current_value);
    closure_field(StgTVar,first_wait_queue_entry);
    closure_field(StgTVar,last_update_by);

    closure_size(StgBCO);
    closure_field(StgBCO, instrs);
    closure_field(StgBCO, literals);
    closure_field(StgBCO, ptrs);
    closure_field(StgBCO, itbls);
    closure_field(StgBCO, arity);
    closure_field(StgBCO, size);
    closure_payload(StgBCO, bitmap);

    closure_size(StgStableName);
    closure_field(StgStableName,sn);

    struct_field_("RtsFlags_ProfFlags_showCCSOnException",
		  RTS_FLAGS, ProfFlags.showCCSOnException);
    struct_field_("RtsFlags_DebugFlags_apply",
		  RTS_FLAGS, DebugFlags.apply);
    struct_field_("RtsFlags_DebugFlags_sanity",
		  RTS_FLAGS, DebugFlags.sanity);
    struct_field_("RtsFlags_DebugFlags_weak",
		  RTS_FLAGS, DebugFlags.weak);
    struct_field_("RtsFlags_GcFlags_initialStkSize",
		  RTS_FLAGS, GcFlags.initialStkSize);

    struct_size(StgFunInfoExtraFwd);
    struct_field(StgFunInfoExtraFwd, slow_apply);
    struct_field(StgFunInfoExtraFwd, fun_type);
    struct_field(StgFunInfoExtraFwd, arity);
    struct_field_("StgFunInfoExtraFwd_bitmap", StgFunInfoExtraFwd, b.bitmap);

    struct_size(StgFunInfoExtraRev);
    struct_field(StgFunInfoExtraRev, slow_apply_offset);
    struct_field(StgFunInfoExtraRev, fun_type);
    struct_field(StgFunInfoExtraRev, arity);
    struct_field_("StgFunInfoExtraRev_bitmap", StgFunInfoExtraRev, b.bitmap);

    struct_field(StgLargeBitmap, size);
    field_offset(StgLargeBitmap, bitmap);

    struct_size(snEntry);
    struct_field(snEntry,sn_obj);
    struct_field(snEntry,addr);

#ifdef mingw32_HOST_OS
    struct_size(StgAsyncIOResult);
    struct_field(StgAsyncIOResult, reqID);
    struct_field(StgAsyncIOResult, len);
    struct_field(StgAsyncIOResult, errCode);
#endif

    struct_size(MP_INT);
    struct_field(MP_INT,_mp_alloc);
    struct_field(MP_INT,_mp_size);
    struct_field(MP_INT,_mp_d);

    ctype(mp_limb_t);
    return 0;
}
