/* ----------------------------------------------------------------------------
 * 
 * (c) The GHC Team, 1998-2005
 *
 * Closure Type Constants: out here because the native code generator
 * needs to get at them.
 *
 * -------------------------------------------------------------------------- */

#ifndef CLOSURETYPES_H
#define CLOSURETYPES_H

/* 
 * WARNING WARNING WARNING
 *
 * Keep the closure tags contiguous: rts/ClosureFlags.c relies on
 * this.
 *
 * If you add or delete any closure types, don't forget to update
 * the closure flags table in rts/ClosureFlags.c.
 */

/* Object tag 0 raises an internal error */
#define INVALID_OBJECT          0
#define CONSTR                  1
#define	CONSTR_1_0		2
#define	CONSTR_0_1		3
#define	CONSTR_2_0		4
#define	CONSTR_1_1		5
#define	CONSTR_0_2		6
#define CONSTR_STATIC	        7
#define CONSTR_NOCAF_STATIC     8
#define FUN		        9 
#define	FUN_1_0		  	10
#define	FUN_0_1		  	11
#define	FUN_2_0		  	12
#define	FUN_1_1		  	13
#define	FUN_0_2			14
#define FUN_STATIC	        15
#define THUNK		        16
#define	THUNK_1_0	  	17
#define	THUNK_0_1	  	18
#define	THUNK_2_0	  	19
#define	THUNK_1_1	  	20
#define	THUNK_0_2		21
#define THUNK_STATIC	        22
#define THUNK_SELECTOR	        23
#define BCO		        24
#define AP		        25
#define PAP			26
#define AP_STACK                27
#define IND		        28
#define IND_OLDGEN	        29
#define IND_PERM	        30
#define IND_OLDGEN_PERM	        31
#define IND_STATIC	        32
#define RET_BCO                 33
#define RET_SMALL	        34
#define RET_VEC_SMALL	        35
#define RET_BIG		        36
#define RET_VEC_BIG	        37
#define RET_DYN		        38
#define RET_FUN                 39
#define UPDATE_FRAME	        40
#define CATCH_FRAME	        41
#define STOP_FRAME	        42
#define CAF_BLACKHOLE		43
#define BLACKHOLE	        44
#define SE_BLACKHOLE		45
#define SE_CAF_BLACKHOLE	46
#define MVAR		        47
#define ARR_WORDS	        48
#define MUT_ARR_PTRS_CLEAN      49
#define MUT_ARR_PTRS_DIRTY      50
#define MUT_ARR_PTRS_FROZEN0    51
#define MUT_ARR_PTRS_FROZEN     52
#define MUT_VAR_CLEAN	        53
#define MUT_VAR_DIRTY	        54
#define WEAK		        55
#define STABLE_NAME	        56
#define TSO		        57
#define BLOCKED_FETCH	        58
#define FETCH_ME                59
#define FETCH_ME_BQ             60
#define RBH                     61
#define EVACUATED               62
#define REMOTE_REF              63
#define TVAR_WATCH_QUEUE        64
#define INVARIANT_CHECK_QUEUE   65
#define ATOMIC_INVARIANT        66
#define TVAR                    67
#define TREC_CHUNK              68
#define TREC_HEADER             69
#define ATOMICALLY_FRAME        70
#define CATCH_RETRY_FRAME       71
#define CATCH_STM_FRAME         72
#define N_CLOSURE_TYPES         73

#endif /* CLOSURETYPES_H */
