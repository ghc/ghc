/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Closure Type Constants: out here because the native code generator
 * needs to get at them.
 *
 * -------------------------------------------------------------------------- */

#pragma once

/*
 * WARNING WARNING WARNING
 *
 * If you add or delete any closure types, don't forget to update the following,
 *   - the closure flags table in rts/ClosureFlags.c
 *   - isRetainer in rts/RetainerProfile.c
 *   - the closure_type_names list in rts/Printer.c
 */

/* CONSTR/THUNK/FUN_$A_$B mean they have $A pointers followed by $B
 * non-pointers in their payloads.
 */

/* Object tag 0 raises an internal error */
#define INVALID_OBJECT                0
#define CONSTR                        1
#define CONSTR_1_0                    2
#define CONSTR_0_1                    3
#define CONSTR_2_0                    4
#define CONSTR_1_1                    5
#define CONSTR_0_2                    6
#define CONSTR_NOCAF                  7
#define FUN                           8
#define FUN_1_0                       9
#define FUN_0_1                       10
#define FUN_2_0                       11
#define FUN_1_1                       12
#define FUN_0_2                       13
#define FUN_STATIC                    14
#define THUNK                         15
#define THUNK_1_0                     16
#define THUNK_0_1                     17
#define THUNK_2_0                     18
#define THUNK_1_1                     19
#define THUNK_0_2                     20
#define THUNK_STATIC                  21
#define THUNK_SELECTOR                22
#define BCO                           23
#define AP                            24
#define PAP                           25
#define AP_STACK                      26
#define IND                           27
#define IND_STATIC                    28
#define RET_BCO                       29
#define RET_SMALL                     30
#define RET_BIG                       31
#define RET_FUN                       32
#define UPDATE_FRAME                  33
#define CATCH_FRAME                   34
#define UNDERFLOW_FRAME               35
#define STOP_FRAME                    36
#define BLOCKING_QUEUE                37
#define BLACKHOLE                     38
#define MVAR_CLEAN                    39
#define MVAR_DIRTY                    40
#define TVAR                          41
#define ARR_WORDS                     42
#define MUT_ARR_PTRS_CLEAN            43
#define MUT_ARR_PTRS_DIRTY            44
#define MUT_ARR_PTRS_FROZEN_DIRTY     45
#define MUT_ARR_PTRS_FROZEN_CLEAN     46
#define MUT_VAR_CLEAN                 47
#define MUT_VAR_DIRTY                 48
#define WEAK                          49
#define PRIM                          50
#define MUT_PRIM                      51
#define TSO                           52
#define STACK                         53
#define TREC_CHUNK                    54
#define ATOMICALLY_FRAME              55
#define CATCH_RETRY_FRAME             56
#define CATCH_STM_FRAME               57
#define WHITEHOLE                     58
#define SMALL_MUT_ARR_PTRS_CLEAN      59
#define SMALL_MUT_ARR_PTRS_DIRTY      60
#define SMALL_MUT_ARR_PTRS_FROZEN_DIRTY 61
#define SMALL_MUT_ARR_PTRS_FROZEN_CLEAN 62
#define COMPACT_NFDATA                63
#define N_CLOSURE_TYPES               64
