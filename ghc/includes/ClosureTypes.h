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
#define CONSTR_INTLIKE	        7 
#define CONSTR_CHARLIKE	        8 
#define CONSTR_STATIC	        9 
#define CONSTR_NOCAF_STATIC     10
#define FUN		        11
#define	FUN_1_0		  	12
#define	FUN_0_1		  	13
#define	FUN_2_0		  	14
#define	FUN_1_1		  	15
#define	FUN_0_2			16
#define FUN_STATIC	        17
#define THUNK		        18
#define	THUNK_1_0	  	19
#define	THUNK_0_1	  	20
#define	THUNK_2_0	  	21
#define	THUNK_1_1	  	22
#define	THUNK_0_2		23
#define THUNK_STATIC	        24
#define THUNK_SELECTOR	        25
#define BCO		        26
#define AP		        27
#define PAP			28
#define AP_STACK                29
#define IND		        30
#define IND_OLDGEN	        31
#define IND_PERM	        32
#define IND_OLDGEN_PERM	        33
#define IND_STATIC	        34
#define RET_BCO                 35
#define RET_SMALL	        36
#define RET_VEC_SMALL	        37
#define RET_BIG		        38
#define RET_VEC_BIG	        39
#define RET_DYN		        40
#define RET_FUN                 41
#define UPDATE_FRAME	        42
#define CATCH_FRAME	        43
#define STOP_FRAME	        44
#define CAF_BLACKHOLE		45
#define BLACKHOLE	        46
#define SE_BLACKHOLE		47
#define SE_CAF_BLACKHOLE	48
#define MVAR		        59
#define ARR_WORDS	        50
#define MUT_ARR_PTRS	        51
#define MUT_ARR_PTRS_FROZEN0    52
#define MUT_ARR_PTRS_FROZEN     53
#define MUT_VAR		        54
#define WEAK		        55
#define FOREIGN		        56
#define STABLE_NAME	        57
#define TSO		        58
#define BLOCKED_FETCH	        69
#define FETCH_ME                60
#define FETCH_ME_BQ             61
#define RBH                     62
#define EVACUATED               63
#define REMOTE_REF              64
#define TVAR_WAIT_QUEUE         65
#define TVAR                    66
#define TREC_CHUNK              67
#define TREC_HEADER             68
#define ATOMICALLY_FRAME        79
#define CATCH_RETRY_FRAME       70
#define CATCH_STM_FRAME         71
#define N_CLOSURE_TYPES         72

#endif /* CLOSURETYPES_H */
