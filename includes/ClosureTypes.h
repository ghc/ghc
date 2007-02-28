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
#define RET_BIG		        35
#define RET_DYN		        36
#define RET_FUN                 37
#define UPDATE_FRAME	        38
#define CATCH_FRAME	        39
#define STOP_FRAME	        40
#define CAF_BLACKHOLE		41
#define BLACKHOLE	        42
#define SE_BLACKHOLE		43
#define SE_CAF_BLACKHOLE	44
#define MVAR		        45
#define ARR_WORDS	        46
#define MUT_ARR_PTRS_CLEAN      47
#define MUT_ARR_PTRS_DIRTY      48
#define MUT_ARR_PTRS_FROZEN0    49
#define MUT_ARR_PTRS_FROZEN     50
#define MUT_VAR_CLEAN	        51
#define MUT_VAR_DIRTY	        52
#define WEAK		        53
#define STABLE_NAME	        54
#define TSO		        55
#define BLOCKED_FETCH	        56
#define FETCH_ME                57
#define FETCH_ME_BQ             58
#define RBH                     59
#define EVACUATED               60
#define REMOTE_REF              61
#define TVAR_WATCH_QUEUE        62
#define INVARIANT_CHECK_QUEUE   63
#define ATOMIC_INVARIANT        64
#define TVAR                    65
#define TREC_CHUNK              66
#define TREC_HEADER             67
#define ATOMICALLY_FRAME        68
#define CATCH_RETRY_FRAME       69
#define CATCH_STM_FRAME         70
#define N_CLOSURE_TYPES         71

#endif /* CLOSURETYPES_H */
