/* ----------------------------------------------------------------------------
 * 
 * (c) The GHC Team, 1998-2005
 *
 * Closure Type Constants: out here because the native code generator
 * needs to get at them.
 *
 * -------------------------------------------------------------------------- */

#ifndef RTS_STORAGE_CLOSURETYPES_H
#define RTS_STORAGE_CLOSURETYPES_H

/* 
 * WARNING WARNING WARNING
 *
 * If you add or delete any closure types, don't forget to update
 * the closure flags table in rts/ClosureFlags.c.
 */

/* Object tag 0 raises an internal error */
#define INVALID_OBJECT          0
#define CONSTR                  1
#define CONSTR_1_0              2
#define CONSTR_0_1              3
#define CONSTR_2_0              4
#define CONSTR_1_1              5
#define CONSTR_0_2              6
#define CONSTR_STATIC           7
#define CONSTR_NOCAF_STATIC     8
#define FUN                     9
#define FUN_1_0                 10
#define FUN_0_1                 11
#define FUN_2_0                 12
#define FUN_1_1                 13
#define FUN_0_2                 14
#define FUN_STATIC              15
#define THUNK                   16
#define THUNK_1_0               17
#define THUNK_0_1               18
#define THUNK_2_0               19
#define THUNK_1_1               20
#define THUNK_0_2               21
#define THUNK_STATIC            22
#define THUNK_SELECTOR          23
#define BCO                     24
#define AP                      25
#define PAP                     26
#define AP_STACK                27
#define IND                     28
#define IND_STATIC              29
#define RET_BCO                 30
#define RET_SMALL               31
#define RET_BIG                 32
#define RET_FUN                 33
#define UPDATE_FRAME            34
#define CATCH_FRAME             35
#define UNDERFLOW_FRAME         36
#define STOP_FRAME              37
#define BLOCKING_QUEUE          38
#define BLACKHOLE               39
#define MVAR_CLEAN              40
#define MVAR_DIRTY              41
#define TVAR                    42
#define ARR_WORDS               43
#define MUT_ARR_PTRS_CLEAN      44
#define MUT_ARR_PTRS_DIRTY      45
#define MUT_ARR_PTRS_FROZEN0    46
#define MUT_ARR_PTRS_FROZEN     47
#define MUT_VAR_CLEAN           48
#define MUT_VAR_DIRTY           49
#define WEAK                    50
#define PRIM                    51
#define MUT_PRIM                52
#define TSO                     53
#define STACK                   54
#define TREC_CHUNK              55
#define ATOMICALLY_FRAME        56
#define CATCH_RETRY_FRAME       57
#define CATCH_STM_FRAME         58
#define WHITEHOLE               59
#define SMALL_MUT_ARR_PTRS_CLEAN      60
#define SMALL_MUT_ARR_PTRS_DIRTY      61
#define SMALL_MUT_ARR_PTRS_FROZEN0    62
#define SMALL_MUT_ARR_PTRS_FROZEN     63
#define COMPACT_NFDATA          64
#define N_CLOSURE_TYPES         65

#endif /* RTS_STORAGE_CLOSURETYPES_H */
