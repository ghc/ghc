/* ----------------------------------------------------------------------------
 * $Id: ClosureTypes.h,v 1.17 2002/03/26 11:09:34 simonmar Exp $
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
#define AP_UPD		        27
#define PAP			28
#define IND		        29
#define IND_OLDGEN	        30
#define IND_PERM	        31
#define IND_OLDGEN_PERM	        32
#define IND_STATIC	        33
#define RET_BCO                 34
#define RET_SMALL	        35
#define RET_VEC_SMALL	        36
#define RET_BIG		        37
#define RET_VEC_BIG	        38
#define RET_DYN		        39
#define UPDATE_FRAME	        40
#define CATCH_FRAME	        41
#define STOP_FRAME	        42
#define SEQ_FRAME	        43
#define CAF_BLACKHOLE		44
#define BLACKHOLE	        45
#define BLACKHOLE_BQ	        46
#define SE_BLACKHOLE		47
#define SE_CAF_BLACKHOLE	48
#define MVAR		        49
#define ARR_WORDS	        50
#define MUT_ARR_PTRS	        51
#define MUT_ARR_PTRS_FROZEN     52
#define MUT_VAR		        53
#define MUT_CONS                54
#define WEAK		        55
#define FOREIGN		        56
#define STABLE_NAME	        57
#define TSO		        58
#define BLOCKED_FETCH	        59
#define FETCH_ME                60
#define FETCH_ME_BQ             61
#define RBH                     62
#define EVACUATED               63
#define REMOTE_REF              64
#define N_CLOSURE_TYPES         65

#endif /* CLOSURETYPES_H */
