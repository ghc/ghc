/* ----------------------------------------------------------------------------
 * $Id: ClosureTypes.h,v 1.20 2005/02/10 13:02:02 simonmar Exp $
 * 
 * (c) The GHC Team, 1998-1999
 *
 * Closure Type Constants
 *
 * -------------------------------------------------------------------------- */

#ifndef CLOSURETYPES_H
#define CLOSURETYPES_H

/* Out here because the native code generator needs to get at them. */

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
#define BLACKHOLE_BQ	        47
#define SE_BLACKHOLE		48
#define SE_CAF_BLACKHOLE	49
#define MVAR		        50
#define ARR_WORDS	        51
#define MUT_ARR_PTRS	        52
#define MUT_ARR_PTRS_FROZEN0    53
#define MUT_ARR_PTRS_FROZEN     54
#define MUT_VAR		        55
#define WEAK		        56
#define FOREIGN		        57
#define STABLE_NAME	        58
#define TSO		        59
#define BLOCKED_FETCH	        60
#define FETCH_ME                61
#define FETCH_ME_BQ             62
#define RBH                     63
#define EVACUATED               64
#define REMOTE_REF              65
#define TVAR_WAIT_QUEUE         66
#define TVAR                    67
#define TREC_CHUNK              68
#define TREC_HEADER             69
#define ATOMICALLY_FRAME        70
#define CATCH_RETRY_FRAME       71
#define CATCH_STM_FRAME         72
#define N_CLOSURE_TYPES         73

#endif /* CLOSURETYPES_H */
