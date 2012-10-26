/* --------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1992-2012
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
#define THREADED_RTS

#include "PosixSource.h"
#include "Rts.h"
#include "Stable.h"
#include "Capability.h"

#include <inttypes.h>
#include <stdio.h>
#include <string.h>

enum Mode { Gen_Haskell_Type, Gen_Haskell_Value, Gen_Haskell_Wrappers, Gen_Haskell_Exports, Gen_Header } mode;

#define str(a,b) #a "_" #b

#define OFFSET(s_type, field) ((size_t)&(((s_type*)0)->field))
#define FIELD_SIZE(s_type, field) ((size_t)sizeof(((s_type*)0)->field))
#define TYPE_SIZE(type) (sizeof(type))

#pragma GCC poison sizeof

#define def_offset(str, offset)                                             \
    switch (mode) {                                                         \
    case Gen_Haskell_Type:                                                  \
        printf("    , pc_OFFSET_" str " :: Int\n");                         \
        break;                                                              \
    case Gen_Haskell_Value:                                                 \
        printf("    , pc_OFFSET_" str " = %" PRIdPTR "\n", (intptr_t)(offset)); \
        break;                                                              \
    case Gen_Haskell_Wrappers:                                              \
        printf("oFFSET_" str " :: DynFlags -> Int\n");                      \
        printf("oFFSET_" str " dflags = pc_OFFSET_" str " (sPlatformConstants (settings dflags))\n"); \
        break;                                                              \
    case Gen_Haskell_Exports:                                               \
        printf("    oFFSET_" str ",\n");                                    \
        break;                                                              \
    case Gen_Header:                                                        \
        printf("#define OFFSET_" str " %" PRIdPTR "\n", (intptr_t)(offset));  \
        break;                                                              \
    }

#define ctype(type)                                                         \
    switch (mode) {                                                         \
    case Gen_Haskell_Type:                                                  \
    case Gen_Haskell_Value:                                                 \
    case Gen_Haskell_Wrappers:                                              \
    case Gen_Haskell_Exports:                                               \
        break;                                                              \
    case Gen_Header:                                                        \
        printf("#define SIZEOF_" #type " %" FMT_SizeT "\n",                 \
               (size_t)TYPE_SIZE(type));                                    \
        break;                                                              \
    }

/* Defining REP_x to be b32 etc
   These are both the C-- types used in a load
      e.g.  b32[addr]
   and the names of the CmmTypes in the compiler
      b32 :: CmmType
*/
#define field_type_(want_haskell, str, s_type, field)                       \
    switch (mode) {                                                         \
    case Gen_Haskell_Type:                                                  \
        if (want_haskell) {                                                 \
            printf("    , pc_REP_" str " :: Int\n");                        \
            break;                                                          \
        }                                                                   \
    case Gen_Haskell_Value:                                                 \
        if (want_haskell) {                                                 \
            printf("    , pc_REP_" str " = %" PRIdPTR "\n", (intptr_t)(FIELD_SIZE(s_type, field))); \
            break;                                                          \
        }                                                                   \
    case Gen_Haskell_Wrappers:                                              \
    case Gen_Haskell_Exports:                                               \
        break;                                                              \
    case Gen_Header:                                                        \
        printf("#define REP_" str " b");                                    \
        printf("%" FMT_SizeT "\n", FIELD_SIZE(s_type, field) * 8);          \
        break;                                                              \
    }

#define field_type_gcptr_(str, s_type, field)                               \
    switch (mode) {                                                         \
    case Gen_Haskell_Type:                                                  \
    case Gen_Haskell_Value:                                                 \
    case Gen_Haskell_Wrappers:                                              \
    case Gen_Haskell_Exports:                                               \
        break;                                                              \
    case Gen_Header:                                                        \
        printf("#define REP_" str " gcptr\n");                              \
        break;                                                              \
    }

#define field_type(want_haskell, s_type, field) \
    field_type_(want_haskell,str(s_type,field),s_type,field);

#define field_offset_(str, s_type, field) \
    def_offset(str, OFFSET(s_type,field));

#define field_offset(s_type, field) \
    field_offset_(str(s_type,field),s_type,field);

/* An access macro for use in C-- sources. */
#define struct_field_macro(str)                                             \
    switch (mode) {                                                         \
    case Gen_Haskell_Type:                                                  \
    case Gen_Haskell_Value:                                                 \
    case Gen_Haskell_Wrappers:                                              \
    case Gen_Haskell_Exports:                                               \
        break;                                                              \
    case Gen_Header:                                                        \
        printf("#define " str "(__ptr__)  REP_" str "[__ptr__+OFFSET_" str "]\n"); \
        break;                                                              \
    }

/* Outputs the byte offset and MachRep for a field */
#define struct_field_helper(want_haskell, s_type, field)    \
    field_offset(s_type, field);                            \
    field_type(want_haskell, s_type, field);                \
    struct_field_macro(str(s_type,field))

#define struct_field(s_type, field)         \
    struct_field_helper(0, s_type, field)

#define struct_field_h(s_type, field)       \
    struct_field_helper(1, s_type, field)

#define struct_field_(str, s_type, field)	\
    field_offset_(str, s_type, field);		\
    field_type_(0,str, s_type, field);		\
    struct_field_macro(str)

#define def_size(str, size)                                                 \
    switch (mode) {                                                         \
    case Gen_Haskell_Type:                                                  \
        printf("    , pc_SIZEOF_" str " :: Int\n");                         \
        break;                                                              \
    case Gen_Haskell_Value:                                                 \
        printf("    , pc_SIZEOF_" str " = %" FMT_SizeT "\n", (size_t)size); \
        break;                                                              \
    case Gen_Haskell_Wrappers:                                              \
        printf("sIZEOF_" str " :: DynFlags -> Int\n");                      \
        printf("sIZEOF_" str " dflags = pc_SIZEOF_" str " (sPlatformConstants (settings dflags))\n"); \
        break;                                                              \
    case Gen_Haskell_Exports:                                               \
        printf("    sIZEOF_" str ",\n");                                    \
        break;                                                              \
    case Gen_Header:                                                        \
        printf("#define SIZEOF_" str " %" FMT_SizeT "\n", (size_t)size);    \
        break;                                                              \
    }

#define def_closure_size(str, size)                                         \
    switch (mode) {                                                         \
    case Gen_Haskell_Type:                                                  \
    case Gen_Haskell_Value:                                                 \
    case Gen_Haskell_Wrappers:                                              \
    case Gen_Haskell_Exports:                                               \
        break;                                                              \
    case Gen_Header:                                                        \
        printf("#define SIZEOF_" str " (SIZEOF_StgHeader+%" FMT_SizeT ")\n", (size_t)size); \
        break;                                                              \
    }

#define struct_size(s_type) \
    def_size(#s_type, TYPE_SIZE(s_type));

/*
 * Size of a closure type, minus the header, named SIZEOF_<type>_NoHdr
 * Also, we #define SIZEOF_<type> to be the size of the whole closure for .cmm.
 */
#define closure_size(s_type) \
    def_size(#s_type "_NoHdr", TYPE_SIZE(s_type) - TYPE_SIZE(StgHeader)); \
    def_closure_size(#s_type, TYPE_SIZE(s_type) - TYPE_SIZE(StgHeader));

#define thunk_size(s_type) \
    def_size(#s_type "_NoThunkHdr", TYPE_SIZE(s_type) - TYPE_SIZE(StgThunkHeader)); \
    closure_size(s_type)

/* An access macro for use in C-- sources. */
#define closure_field_macro(str)                                            \
    switch (mode) {                                                         \
    case Gen_Haskell_Type:                                                  \
    case Gen_Haskell_Value:                                                 \
    case Gen_Haskell_Wrappers:                                              \
    case Gen_Haskell_Exports:                                               \
        break;                                                              \
    case Gen_Header:                                                        \
        printf("#define " str "(__ptr__)  REP_" str "[__ptr__+SIZEOF_StgHeader+OFFSET_" str "]\n"); \
        break;                                                              \
    }

#define closure_field_offset_(str, s_type,field) \
    def_offset(str, OFFSET(s_type,field) - TYPE_SIZE(StgHeader));

#define closure_field_offset(s_type,field) \
    closure_field_offset_(str(s_type,field),s_type,field)

#define closure_payload_macro(str)                                          \
    switch (mode) {                                                         \
    case Gen_Haskell_Type:                                                  \
    case Gen_Haskell_Value:                                                 \
    case Gen_Haskell_Wrappers:                                              \
    case Gen_Haskell_Exports:                                               \
        break;                                                              \
    case Gen_Header:                                                        \
        printf("#define " str "(__ptr__,__ix__)  W_[__ptr__+SIZEOF_StgHeader+OFFSET_" str " + WDS(__ix__)]\n"); \
        break;                                                              \
    }

#define closure_payload(s_type,field) \
    closure_field_offset_(str(s_type,field),s_type,field); \
    closure_payload_macro(str(s_type,field));

/* Byte offset and MachRep for a closure field, minus the header */
#define closure_field_(str, s_type, field) \
    closure_field_offset_(str,s_type,field) \
    field_type_(0, str, s_type, field); \
    closure_field_macro(str)

#define closure_field(s_type, field) \
    closure_field_(str(s_type,field),s_type,field)

/* Byte offset and MachRep for a closure field, minus the header */
#define closure_field_gcptr_(str, s_type, field) \
    closure_field_offset_(str,s_type,field) \
    field_type_gcptr_(str, s_type, field); \
    closure_field_macro(str)

#define closure_field_gcptr(s_type, field) \
    closure_field_gcptr_(str(s_type,field),s_type,field)

/* Byte offset for a TSO field, minus the header and variable prof bit. */
#define tso_payload_offset(s_type, field) \
    def_offset(str(s_type,field), OFFSET(s_type,field) - TYPE_SIZE(StgHeader) - TYPE_SIZE(StgTSOProfInfo));

/* Full byte offset for a TSO field, for use from Cmm */
#define tso_field_offset_macro(str)                                         \
    switch (mode) {                                                         \
    case Gen_Haskell_Type:                                                  \
    case Gen_Haskell_Value:                                                 \
    case Gen_Haskell_Wrappers:                                              \
    case Gen_Haskell_Exports:                                               \
        break;                                                              \
    case Gen_Header:                                                        \
        printf("#define TSO_OFFSET_" str " (SIZEOF_StgHeader+SIZEOF_OPT_StgTSOProfInfo+OFFSET_" str ")\n"); \
        break;                                                              \
    }

#define tso_field_offset(s_type, field) \
    tso_payload_offset(s_type, field);  	\
    tso_field_offset_macro(str(s_type,field));

#define tso_field_macro(str)                                                \
    switch (mode) {                                                         \
    case Gen_Haskell_Type:                                                  \
    case Gen_Haskell_Value:                                                 \
    case Gen_Haskell_Wrappers:                                              \
    case Gen_Haskell_Exports:                                               \
        break;                                                              \
    case Gen_Header:                                                        \
    printf("#define " str "(__ptr__)  REP_" str "[__ptr__+TSO_OFFSET_" str "]\n") \
        break;                                                              \
    }

#define tso_field(s_type, field)        \
    field_type(0, s_type, field);       \
    tso_field_offset(s_type,field);     \
    tso_field_macro(str(s_type,field))
  
#define opt_struct_size(s_type, option)					                    \
    switch (mode) {                                                         \
    case Gen_Haskell_Type:                                                  \
    case Gen_Haskell_Value:                                                 \
    case Gen_Haskell_Wrappers:                                              \
    case Gen_Haskell_Exports:                                               \
        break;                                                              \
    case Gen_Header:                                                        \
        printf("#ifdef " #option "\n");					                    \
        printf("#define SIZEOF_OPT_" #s_type " SIZEOF_" #s_type "\n");	    \
        printf("#else\n");							                        \
        printf("#define SIZEOF_OPT_" #s_type " 0\n");			            \
        printf("#endif\n\n");                                               \
        break;                                                              \
    }

#define FUN_OFFSET(sym) (OFFSET(Capability,f.sym) - OFFSET(Capability,r))

void constantBool(char *haskellName, int val) {
    switch (mode) {
    case Gen_Haskell_Type:
        printf("    , pc_%s :: Bool\n", haskellName);
        break;
    case Gen_Haskell_Value:
        printf("    , pc_%s = %s\n", haskellName, val ? "True" : "False");
        break;
    case Gen_Haskell_Wrappers:
        printf("%s :: DynFlags -> Bool\n", haskellName);
        printf("%s dflags = pc_%s (sPlatformConstants (settings dflags))\n",
               haskellName, haskellName);
        break;
    case Gen_Haskell_Exports:
        printf("    %s,\n", haskellName);
        break;
    case Gen_Header:
        break;
    }
}

void constantIntegralC(char *haskellType, char *cName, char *haskellName,
                       intptr_t val) {
    switch (mode) {
    case Gen_Haskell_Type:
        printf("    , pc_%s :: %s\n", haskellName, haskellType);
        break;
    case Gen_Haskell_Value:
        printf("    , pc_%s = %" PRIdPTR "\n", haskellName, val);
        break;
    case Gen_Haskell_Wrappers:
        printf("%s :: DynFlags -> %s\n", haskellName, haskellType);
        printf("%s dflags = pc_%s (sPlatformConstants (settings dflags))\n",
               haskellName, haskellName);
        break;
    case Gen_Haskell_Exports:
        printf("    %s,\n", haskellName);
        break;
    case Gen_Header:
        if (cName != NULL) {
            printf("#define %s %" PRIdPTR "\n", cName, val);
        }
        break;
    }
}

void constantIntC(char *cName, char *haskellName, intptr_t val) {
    /* If the value is larger than 2^28 or smaller than -2^28, then fail.
       This test is a bit conservative, but if any constants are roughly
       maxBoun or minBound then we probably need them to be Integer
       rather than Int so that cross-compiling between 32bit and 64bit
       platforms works. */
    if (val > 268435456) {
        printf("Value too large for constantInt: %" PRIdPTR "\n", val);
        exit(1);
    }
    if (val < -268435456) {
        printf("Value too small for constantInt: %" PRIdPTR "\n", val);
        exit(1);
    }

    constantIntegralC("Int", cName, haskellName, val);
}

void constantInt(char *name, intptr_t val) {
    constantIntC(NULL, name, val);
}

void constantInteger(char *name, intptr_t val) {
    constantIntegralC("Integer", NULL, name, val);
}

int
main(int argc, char *argv[])
{
    if (argc == 1) {
        mode = Gen_Header;
    }
    else if (argc == 2) {
        if (0 == strcmp("--gen-haskell-type", argv[1])) {
            mode = Gen_Haskell_Type;
        }
        else if (0 == strcmp("--gen-haskell-value", argv[1])) {
            mode = Gen_Haskell_Value;
        }
        else if (0 == strcmp("--gen-haskell-wrappers", argv[1])) {
            mode = Gen_Haskell_Wrappers;
        }
        else if (0 == strcmp("--gen-haskell-exports", argv[1])) {
            mode = Gen_Haskell_Exports;
        }
        else {
            printf("Bad args\n");
            exit(1);
        }
    }
    else {
        printf("Bad args\n");
        exit(1);
    }

    switch (mode) {
    case Gen_Haskell_Type:
        printf("data PlatformConstants = PlatformConstants {\n");
        /* Now a kludge that allows the real entries to all start with a
           comma, which makes life a little easier */
        printf("    pc_platformConstants :: ()\n");
        break;
    case Gen_Haskell_Value:
        printf("PlatformConstants {\n");
        printf("    pc_platformConstants = ()\n");
        break;
    case Gen_Haskell_Wrappers:
    case Gen_Haskell_Exports:
        break;
    case Gen_Header:
        printf("/* This file is created automatically.  Do not edit by hand.*/\n\n");

        break;
    }

    // Closure header sizes.
    constantIntC("STD_HDR_SIZE", "sTD_HDR_SIZE",
                 sizeofW(StgHeader) - sizeofW(StgProfHeader));
    /* grrr.. PROFILING is on so we need to subtract sizeofW(StgProfHeader) */
    constantIntC("PROF_HDR_SIZE", "pROF_HDR_SIZE", sizeofW(StgProfHeader));

    // Size of a storage manager block (in bytes).
    constantIntC("BLOCK_SIZE", "bLOCK_SIZE", BLOCK_SIZE);
    if (mode == Gen_Header) {
        constantIntC("MBLOCK_SIZE", "mBLOCK_SIZE", MBLOCK_SIZE);
    }
    // blocks that fit in an MBlock, leaving space for the block descriptors
    constantIntC("BLOCKS_PER_MBLOCK", "bLOCKS_PER_MBLOCK", BLOCKS_PER_MBLOCK);
    // could be derived, but better to save doing the calculation twice


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
    field_offset(StgRegTable, rCCCS);
    field_offset(StgRegTable, rCurrentTSO);
    field_offset(StgRegTable, rCurrentNursery);
    field_offset(StgRegTable, rHpAlloc);
    if (mode == Gen_Header) {
        struct_field(StgRegTable, rRet);
        struct_field(StgRegTable, rNursery);
    }

    def_offset("stgEagerBlackholeInfo", FUN_OFFSET(stgEagerBlackholeInfo));
    def_offset("stgGCEnter1", FUN_OFFSET(stgGCEnter1));
    def_offset("stgGCFun", FUN_OFFSET(stgGCFun));

    field_offset(Capability, r);
    if (mode == Gen_Header) {
        field_offset(Capability, lock);
        struct_field(Capability, no);
        struct_field(Capability, mut_lists);
        struct_field(Capability, context_switch);
        struct_field(Capability, interrupt);
        struct_field(Capability, sparks);
    }

    struct_field(bdescr, start);
    struct_field(bdescr, free);
    struct_field(bdescr, blocks);
    if (mode == Gen_Header) {
        struct_field(bdescr, gen_no);
        struct_field(bdescr, link);

        struct_size(generation);
        struct_field(generation, n_new_large_words);
    }

    struct_size(CostCentreStack);
    if (mode == Gen_Header) {
        struct_field(CostCentreStack, ccsID);
    }
    struct_field_h(CostCentreStack, mem_alloc);
    struct_field_h(CostCentreStack, scc_count);
    if (mode == Gen_Header) {
        struct_field(CostCentreStack, prevStack);

        struct_field(CostCentre, ccID);
        struct_field(CostCentre, link);

        struct_field(StgHeader, info);
    }
    struct_field_("StgHeader_ccs",  StgHeader, prof.ccs);
    struct_field_("StgHeader_ldvw", StgHeader, prof.hp.ldvw);

    struct_size(StgSMPThunkHeader);

    if (mode == Gen_Header) {
        closure_payload(StgClosure,payload);
    }

    struct_field_h(StgEntCounter, allocs);
    struct_field(StgEntCounter, registeredp);
    struct_field(StgEntCounter, link);
    struct_field(StgEntCounter, entry_count);

    closure_size(StgUpdateFrame);
    if (mode == Gen_Header) {
        closure_size(StgCatchFrame);
        closure_size(StgStopFrame);
    }

    closure_size(StgMutArrPtrs);
    closure_field(StgMutArrPtrs, ptrs);
    closure_field(StgMutArrPtrs, size);

    closure_size(StgArrWords);
    if (mode == Gen_Header) {
        closure_field(StgArrWords, bytes);
        closure_payload(StgArrWords, payload);

        closure_field(StgTSO, _link);
        closure_field(StgTSO, global_link);
        closure_field(StgTSO, what_next);
        closure_field(StgTSO, why_blocked);
        closure_field(StgTSO, block_info);
        closure_field(StgTSO, blocked_exceptions);
        closure_field(StgTSO, id);
        closure_field(StgTSO, cap);
        closure_field(StgTSO, saved_errno);
        closure_field(StgTSO, trec);
        closure_field(StgTSO, flags);
        closure_field(StgTSO, dirty);
        closure_field(StgTSO, bq);
    }
    closure_field_("StgTSO_cccs", StgTSO, prof.cccs);
    closure_field(StgTSO, stackobj);

    closure_field(StgStack, sp);
    closure_field_offset(StgStack, stack);
    if (mode == Gen_Header) {
    closure_field(StgStack, stack_size);
        closure_field(StgStack, dirty);

        struct_size(StgTSOProfInfo);

        opt_struct_size(StgTSOProfInfo,PROFILING);
    }

    closure_field(StgUpdateFrame, updatee);

    if (mode == Gen_Header) {
        closure_field(StgCatchFrame, handler);
        closure_field(StgCatchFrame, exceptions_blocked);

        closure_size(StgPAP);
        closure_field(StgPAP, n_args);
        closure_field_gcptr(StgPAP, fun);
        closure_field(StgPAP, arity);
        closure_payload(StgPAP, payload);

        thunk_size(StgAP);
        closure_field(StgAP, n_args);
        closure_field_gcptr(StgAP, fun);
        closure_payload(StgAP, payload);

        thunk_size(StgAP_STACK);
        closure_field(StgAP_STACK, size);
        closure_field_gcptr(StgAP_STACK, fun);
        closure_payload(StgAP_STACK, payload);

        thunk_size(StgSelector);

        closure_field_gcptr(StgInd, indirectee);

        closure_size(StgMutVar);
        closure_field(StgMutVar, var);

        closure_size(StgAtomicallyFrame);
        closure_field(StgAtomicallyFrame, code);
        closure_field(StgAtomicallyFrame, next_invariant_to_check);
        closure_field(StgAtomicallyFrame, result);

        closure_field(StgInvariantCheckQueue, invariant);
        closure_field(StgInvariantCheckQueue, my_execution);
        closure_field(StgInvariantCheckQueue, next_queue_entry);

        closure_field(StgAtomicInvariant, code);

        closure_field(StgTRecHeader, enclosing_trec);

        closure_size(StgCatchSTMFrame);
        closure_field(StgCatchSTMFrame, handler);
        closure_field(StgCatchSTMFrame, code);

        closure_size(StgCatchRetryFrame);
        closure_field(StgCatchRetryFrame, running_alt_code);
        closure_field(StgCatchRetryFrame, first_code);
        closure_field(StgCatchRetryFrame, alt_code);

        closure_field(StgTVarWatchQueue, closure);
        closure_field(StgTVarWatchQueue, next_queue_entry);
        closure_field(StgTVarWatchQueue, prev_queue_entry);

        closure_field(StgTVar, current_value);

        closure_size(StgWeak);
        closure_field(StgWeak,link);
        closure_field(StgWeak,key);
        closure_field(StgWeak,value);
        closure_field(StgWeak,finalizer);
        closure_field(StgWeak,cfinalizer);

        closure_size(StgDeadWeak);
        closure_field(StgDeadWeak,link);

        closure_size(StgMVar);
        closure_field(StgMVar,head);
        closure_field(StgMVar,tail);
        closure_field(StgMVar,value);

        closure_size(StgMVarTSOQueue);
        closure_field(StgMVarTSOQueue, link);
        closure_field(StgMVarTSOQueue, tso);

        closure_size(StgBCO);
        closure_field(StgBCO, instrs);
        closure_field(StgBCO, literals);
        closure_field(StgBCO, ptrs);
        closure_field(StgBCO, arity);
        closure_field(StgBCO, size);
        closure_payload(StgBCO, bitmap);

        closure_size(StgStableName);
        closure_field(StgStableName,sn);

        closure_size(StgBlockingQueue);
        closure_field(StgBlockingQueue, bh);
        closure_field(StgBlockingQueue, owner);
        closure_field(StgBlockingQueue, queue);
        closure_field(StgBlockingQueue, link);

        closure_size(MessageBlackHole);
        closure_field(MessageBlackHole, link);
        closure_field(MessageBlackHole, tso);
        closure_field(MessageBlackHole, bh);

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
        struct_field_("RtsFlags_MiscFlags_tickInterval",
		      RTS_FLAGS, MiscFlags.tickInterval);

        struct_size(StgFunInfoExtraFwd);
        struct_field(StgFunInfoExtraFwd, slow_apply);
        struct_field(StgFunInfoExtraFwd, fun_type);
        struct_field(StgFunInfoExtraFwd, arity);
        struct_field_("StgFunInfoExtraFwd_bitmap", StgFunInfoExtraFwd, b.bitmap);
    }

    struct_size(StgFunInfoExtraRev);
    if (mode == Gen_Header) {
        struct_field(StgFunInfoExtraRev, slow_apply_offset);
        struct_field(StgFunInfoExtraRev, fun_type);
        struct_field(StgFunInfoExtraRev, arity);
        struct_field_("StgFunInfoExtraRev_bitmap", StgFunInfoExtraRev, b.bitmap);

        struct_field(StgLargeBitmap, size);
        field_offset(StgLargeBitmap, bitmap);

        struct_size(snEntry);
        struct_field(snEntry,sn_obj);
        struct_field(snEntry,addr);
    }

#ifdef mingw32_HOST_OS
    /* Note that this conditional part only affects the C headers.
       That's important, as it means we get the same PlatformConstants
       type on all platforms. */
    if (mode == Gen_Header) {
        struct_size(StgAsyncIOResult);
        struct_field(StgAsyncIOResult, reqID);
        struct_field(StgAsyncIOResult, len);
        struct_field(StgAsyncIOResult, errCode);
    }
#endif

    // pre-compiled thunk types
    constantInt("mAX_SPEC_SELECTEE_SIZE", MAX_SPEC_SELECTEE_SIZE);
    constantInt("mAX_SPEC_AP_SIZE", MAX_SPEC_AP_SIZE);

    // closure sizes: these do NOT include the header (see below for
    // header sizes)
    constantInt("mIN_PAYLOAD_SIZE", MIN_PAYLOAD_SIZE);

    constantInt("mIN_INTLIKE", MIN_INTLIKE);
    constantInt("mAX_INTLIKE", MAX_INTLIKE);

    constantInt("mIN_CHARLIKE", MIN_CHARLIKE);
    constantInt("mAX_CHARLIKE", MAX_CHARLIKE);

    constantInt("mUT_ARR_PTRS_CARD_BITS", MUT_ARR_PTRS_CARD_BITS);

    // A section of code-generator-related MAGIC CONSTANTS.
    constantInt("mAX_Vanilla_REG",      MAX_VANILLA_REG);
    constantInt("mAX_Float_REG",        MAX_FLOAT_REG);
    constantInt("mAX_Double_REG",       MAX_DOUBLE_REG);
    constantInt("mAX_Long_REG",         MAX_LONG_REG);
    constantInt("mAX_Real_Vanilla_REG", MAX_REAL_VANILLA_REG);
    constantInt("mAX_Real_Float_REG",   MAX_REAL_FLOAT_REG);
    constantInt("mAX_Real_Double_REG",  MAX_REAL_DOUBLE_REG);
    constantInt("mAX_Real_Long_REG",    MAX_REAL_LONG_REG);

    // This tells the native code generator the size of the spill
    // area is has available.
    constantInt("rESERVED_C_STACK_BYTES", RESERVED_C_STACK_BYTES);
    // The amount of (Haskell) stack to leave free for saving registers when
    // returning to the scheduler.
    constantInt("rESERVED_STACK_WORDS", RESERVED_STACK_WORDS);
    // Continuations that need more than this amount of stack should do their
    // own stack check (see bug #1466).
    constantInt("aP_STACK_SPLIM", AP_STACK_SPLIM);

    // Size of a word, in bytes
    constantInt("wORD_SIZE", SIZEOF_HSWORD);

    // Size of a double in StgWords.
    constantInt("dOUBLE_SIZE", SIZEOF_DOUBLE);

    // Size of a C int, in bytes. May be smaller than wORD_SIZE.
    constantInt("cINT_SIZE", SIZEOF_INT);
    constantInt("cLONG_SIZE", SIZEOF_LONG);
    constantInt("cLONG_LONG_SIZE", SIZEOF_LONG_LONG);

    // Number of bits to shift a bitfield left by in an info table.
    constantInt("bITMAP_BITS_SHIFT", BITMAP_BITS_SHIFT);

    // Amount of pointer bits used for semi-tagging constructor closures
    constantInt("tAG_BITS", TAG_BITS);

    constantBool("wORDS_BIGENDIAN",
#ifdef WORDS_BIGENDIAN
                                    1
#else
                                    0
#endif
                                         );

    constantBool("dYNAMIC_BY_DEFAULT",
#ifdef DYNAMIC_BY_DEFAULT
                                       1
#else
                                       0
#endif
                                         );

    constantInt("lDV_SHIFT", LDV_SHIFT);
    constantInteger("iLDV_CREATE_MASK",  LDV_CREATE_MASK);
    constantInteger("iLDV_STATE_CREATE", LDV_STATE_CREATE);
    constantInteger("iLDV_STATE_USE",    LDV_STATE_USE);

    switch (mode) {
    case Gen_Haskell_Type:
        printf("  } deriving Read\n");
        break;
    case Gen_Haskell_Value:
        printf("  }\n");
        break;
    case Gen_Haskell_Wrappers:
    case Gen_Haskell_Exports:
    case Gen_Header:
        break;
    }

    return 0;
}
