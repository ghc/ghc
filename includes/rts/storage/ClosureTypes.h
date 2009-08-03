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
#define MVAR_CLEAN	        43
#define MVAR_DIRTY	        44
#define ARR_WORDS	        45
#define MUT_ARR_PTRS_CLEAN      46
#define MUT_ARR_PTRS_DIRTY      47
#define MUT_ARR_PTRS_FROZEN0    48
#define MUT_ARR_PTRS_FROZEN     49
#define MUT_VAR_CLEAN	        50
#define MUT_VAR_DIRTY	        51
#define WEAK		        52
#define STABLE_NAME	        53
#define TSO		        54
#define TVAR_WATCH_QUEUE        55
#define INVARIANT_CHECK_QUEUE   56
#define ATOMIC_INVARIANT        57
#define TVAR                    58
#define TREC_CHUNK              59
#define TREC_HEADER             60
#define ATOMICALLY_FRAME        61
#define CATCH_RETRY_FRAME       62
#define CATCH_STM_FRAME         63
#define WHITEHOLE               64
#define N_CLOSURE_TYPES         65

#endif /* RTS_STORAGE_CLOSURETYPES_H */
