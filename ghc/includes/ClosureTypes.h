/* ----------------------------------------------------------------------------
 * $Id: ClosureTypes.h,v 1.3 1999/01/13 17:25:52 simonm Exp $
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
/* #define CONSTR_p_np */       
#define CONSTR_INTLIKE	        2
#define CONSTR_CHARLIKE	        3
#define CONSTR_STATIC	        4
#define CONSTR_NOCAF_STATIC     5
#define FUN		        6
#define FUN_STATIC	        7
#define THUNK		        8
/* #define THUNK_p_np */        
#define THUNK_STATIC	        9
#define THUNK_SELECTOR	        10
#define BCO		        11
#define AP_UPD		        12
#define PAP			13
#define IND		        14
#define IND_OLDGEN	        15
#define IND_PERM	        16
#define IND_OLDGEN_PERM	        17
#define IND_STATIC	        18
#define CAF_UNENTERED           19
#define CAF_ENTERED		20
#define CAF_BLACKHOLE		21
#define RET_BCO                 22
#define RET_SMALL	        23
#define RET_VEC_SMALL	        24
#define RET_BIG		        25
#define RET_VEC_BIG	        26
#define RET_DYN		        27
#define UPDATE_FRAME	        28
#define UPDATE_STATIC_FRAME     29
#define CATCH_FRAME	        30
#define STOP_FRAME	        31
#define SEQ_FRAME	        32
#define BLACKHOLE	        33
#define BLACKHOLE_STATIC        34
#define MVAR		        35
#define ARR_WORDS	        36
#define MUT_ARR_WORDS	        37
#define MUT_ARR_PTRS	        38
#define MUT_ARR_PTRS_FROZEN     39
#define MUT_VAR		        40
#define WEAK		        41
#define FOREIGN		        42
#define TSO		        43
#define BLOCKED_FETCH	        44
#define FETCH_ME                45
#define EVACUATED               46

#endif CLOSURETYPES_H
