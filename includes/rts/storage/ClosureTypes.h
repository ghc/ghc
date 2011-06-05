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
#define IND_PERM	        29
#define IND_STATIC	        30
#define IND_LOCAL               31
#define RET_BCO                 32
#define RET_SMALL	        33
#define RET_BIG		        34
#define RET_DYN		        35
#define RET_FUN                 36
#define UPDATE_FRAME	        37
#define CATCH_FRAME	        38
#define UNDERFLOW_FRAME         39
#define STOP_FRAME              40
#define BLOCKING_QUEUE		41
#define BLACKHOLE	        42
#define MVAR_CLEAN	        43
#define MVAR_DIRTY	        44
#define ARR_WORDS	        45
#define MUT_ARR_PTRS_LOCAL      46
#define MUT_ARR_PTRS_GLOBAL     47
#define MUT_ARR_PTRS_FROZEN0    48
#define MUT_ARR_PTRS_FROZEN     49
#define MUT_VAR_LOCAL	        50
#define MUT_VAR_GLOBAL	        51
#define WEAK		        52
#define PRIM		        53
#define MUT_PRIM                54
#define TSO		        55
#define STACK                   56
#define TREC_CHUNK              57
#define ATOMICALLY_FRAME        58
#define CATCH_RETRY_FRAME       59
#define CATCH_STM_FRAME         60
#define WHITEHOLE               61
#define N_CLOSURE_TYPES         62

#endif /* RTS_STORAGE_CLOSURETYPES_H */
