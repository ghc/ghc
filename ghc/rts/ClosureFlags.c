/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-1999
 *
 * Closure type flags
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

StgWord16 closure_flags[] = {

/* 
 * These *must* be in the same order as the closure types in
 * ClosureTypes.h.
 */

/* ToDo: some of these flags seem to be duplicated.
 *       - NS is the same as HNF, and the negation of THU
 * (however, we set NS for indirections, which is probably the
 *  right thing to do, since we never get indirections pointing
 *  to thunks.)
 */

/*                              0    1    2    3    4   5   6   7 */
/*			      HNF  BTM   NS  STA  THU MUT UPT SRT */

/* INVALID_OBJECT       = */ ( 0                                      	 ),
/* CONSTR  		= */ (_HNF|     _NS                           	 ),
/* CONSTR_1_0	   	= */ (_HNF|     _NS                           	 ),
/* CONSTR_0_1	   	= */ (_HNF|     _NS                           	 ),
/* CONSTR_2_0	   	= */ (_HNF|     _NS                           	 ),
/* CONSTR_1_1	   	= */ (_HNF|     _NS                           	 ),
/* CONSTR_0_2	   	= */ (_HNF|     _NS                           	 ),
/* CONSTR_INTLIKE 	= */ (_HNF|     _NS|_STA                      	 ),
/* CONSTR_CHARLIKE  	= */ (_HNF|     _NS|_STA                      	 ),
/* CONSTR_STATIC	= */ (_HNF|     _NS|_STA                      	 ),
/* CONSTR_NOCAF_STATIC  = */ (_HNF|     _NS|_STA                      	 ),
/* FUN		   	= */ (_HNF|     _NS|                  _SRT    	 ),
/* FUN_1_0		= */ (_HNF|     _NS|		      _SRT       ),
/* FUN_0_1		= */ (_HNF|     _NS|		      _SRT       ),
/* FUN_2_0		= */ (_HNF|     _NS|		      _SRT       ),
/* FUN_1_1		= */ (_HNF|     _NS|		      _SRT       ),
/* FUN_0_2		= */ (_HNF|     _NS|		      _SRT       ),
/* FUN_STATIC	   	= */ (_HNF|     _NS|_STA|             _SRT    	 ),
/* THUNK		= */ (     _BTM|         _THU|        _SRT    	 ),
/* THUNK_1_0		= */ (     _BTM|         _THU|        _SRT    	 ),
/* THUNK_0_1		= */ (     _BTM|         _THU|        _SRT    	 ),
/* THUNK_2_0		= */ (     _BTM|         _THU|        _SRT    	 ),
/* THUNK_1_1		= */ (     _BTM|         _THU|        _SRT    	 ),
/* THUNK_0_2		= */ (     _BTM|         _THU|        _SRT    	 ),
/* THUNK_STATIC	   	= */ (     _BTM|    _STA|_THU|        _SRT    	 ),
/* THUNK_SELECTOR	= */ (     _BTM|         _THU|        _SRT    	 ),
/* BCO		   	= */ (_HNF|     _NS                           	 ),
/* AP			= */ (                   _THU                 	 ),
/* PAP		   	= */ (_HNF|     _NS				 ),
/* AP_STACK	   	= */ (          	 _THU			 ),
/* IND		   	= */ (          _NS|			    _IND ),
/* IND_OLDGEN	   	= */ (          _NS|			    _IND ),
/* IND_PERM		= */ (          _NS|			    _IND ),
/* IND_OLDGEN_PERM	= */ (          _NS|			    _IND ),
/* IND_STATIC	   	= */ (          _NS|_STA|                   _IND ),
/* RET_BCO		= */ (     _BTM                                  ),
/* RET_SMALL		= */ (     _BTM|                       _SRT      ),
/* RET_VEC_SMALL	= */ (     _BTM|                       _SRT      ),
/* RET_BIG		= */ (                                 _SRT      ),
/* RET_VEC_BIG	   	= */ (                                 _SRT      ),
/* RET_DYN		= */ (                                 _SRT      ),
/* RET_FUN		= */ ( 0                                         ),
/* UPDATE_FRAME        	= */ (     _BTM                                  ),
/* CATCH_FRAME	   	= */ (     _BTM                                  ),
/* STOP_FRAME	   	= */ (     _BTM                                  ),
/* CAF_BLACKHOLE   	= */ ( 	   _BTM|_NS|              _UPT           ),
/* BLACKHOLE		= */ ( 	        _NS|              _UPT           ),
/* SE_BLACKHOLE		= */ ( 	        _NS|              _UPT           ),
/* SE_CAF_BLACKHOLE	= */ ( 	        _NS|              _UPT           ),
/* MVAR		   	= */ (_HNF|     _NS|         _MUT|_UPT           ),
/* ARR_WORDS		= */ (_HNF|     _NS|              _UPT           ),
/* MUT_ARR_PTRS_CLEAN  	= */ (_HNF|     _NS|         _MUT|_UPT           ),
/* MUT_ARR_PTRS_DIRTY  	= */ (_HNF|     _NS|         _MUT|_UPT           ),
/* MUT_ARR_PTRS_FROZEN0	= */ (_HNF|     _NS|         _MUT|_UPT           ),
/* MUT_ARR_PTRS_FROZEN 	= */ (_HNF|     _NS|              _UPT           ),
/* MUT_VAR		= */ (_HNF|     _NS|         _MUT|_UPT           ),
/* WEAK		   	= */ (_HNF|     _NS|              _UPT           ),
/* STABLE_NAME	   	= */ (_HNF|     _NS|              _UPT           ),
/* TSO                 	= */ (_HNF|     _NS|         _MUT|_UPT           ),
/* BLOCKED_FETCH	= */ (_HNF|     _NS|         _MUT|_UPT           ),
/* FETCH_ME		= */ (_HNF|     _NS|         _MUT|_UPT           ),
/* FETCH_ME_BQ          = */ ( 	        _NS|         _MUT|_UPT           ),
/* RBH                  = */ ( 	        _NS|         _MUT|_UPT           ),
/* EVACUATED		= */ ( 0                                         ),
/* REMOTE_REF		= */ (_HNF|     _NS|              _UPT           ),
/* TVAR_WAIT_QUEUE      = */ (          _NS|         _MUT|_UPT           ),
/* TVAR                 = */ (_HNF|     _NS|         _MUT|_UPT           ), 
/* TREC_CHUNK           = */ (          _NS|         _MUT|_UPT           ),
/* TREC_HEADER          = */ (          _NS|         _MUT|_UPT           ),
/* ATOMICALLY_FRAME     = */ (     _BTM                                  ),
/* CATCH_RETRY_FRAME    = */ (     _BTM                                  ),
/* CATCH_STM_FRAME      = */ (     _BTM                                  )
};

#if N_CLOSURE_TYPES != 72
#error Closure types changed: update ClosureFlags.c!
#endif

