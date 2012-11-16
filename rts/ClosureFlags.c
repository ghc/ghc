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

/* ToDo: some of these flags seem to be duplicated.
 *       - NS is the same as HNF, and the negation of THU
 * (however, we set NS for indirections, which is probably the
 *  right thing to do, since we never get indirections pointing
 *  to thunks.)
 */

/*                              0    1    2    3    4   5   6   7     8 */
/*			      HNF  BTM   NS  STA  THU MUT UPT SRT   IND */

 [INVALID_OBJECT]       =  ( 0                                      	 ),
 [CONSTR]  		=  (_HNF|     _NS                           	 ),
 [CONSTR_1_0]	   	=  (_HNF|     _NS                           	 ),
 [CONSTR_0_1]	   	=  (_HNF|     _NS                           	 ),
 [CONSTR_2_0]	   	=  (_HNF|     _NS                           	 ),
 [CONSTR_1_1]	   	=  (_HNF|     _NS                           	 ),
 [CONSTR_0_2]	   	=  (_HNF|     _NS                           	 ),
 [CONSTR_STATIC]	=  (_HNF|     _NS|_STA                      	 ),
 [CONSTR_NOCAF_STATIC]  =  (_HNF|     _NS|_STA                      	 ),
 [FUN]		   	=  (_HNF|     _NS|                  _SRT    	 ),
 [FUN_1_0]		=  (_HNF|     _NS|		    _SRT         ),
 [FUN_0_1]		=  (_HNF|     _NS|		    _SRT         ),
 [FUN_2_0]		=  (_HNF|     _NS|		    _SRT         ),
 [FUN_1_1]		=  (_HNF|     _NS|		    _SRT         ),
 [FUN_0_2]		=  (_HNF|     _NS|		    _SRT         ),
 [FUN_STATIC]	   	=  (_HNF|     _NS|_STA|             _SRT    	 ),
 [THUNK]                =  (                   _THU|        _SRT         ),
 [THUNK_1_0]            =  (                   _THU|        _SRT         ),
 [THUNK_0_1]            =  (                   _THU|        _SRT         ),
 [THUNK_2_0]            =  (                   _THU|        _SRT         ),
 [THUNK_1_1]            =  (                   _THU|        _SRT         ),
 [THUNK_0_2]            =  (                   _THU|        _SRT         ),
 [THUNK_STATIC]         =  (              _STA|_THU|        _SRT         ),
 [THUNK_SELECTOR]       =  (                   _THU|        _SRT         ),
 [BCO]		   	=  (_HNF|     _NS                           	 ),
 [AP]			=  (                   _THU                 	 ),
 [PAP]		   	=  (_HNF|     _NS				 ),
 [AP_STACK]	   	=  (          	       _THU			 ),
 [IND]		   	=  (          _NS|			  _IND ),
 [IND_PERM]		=  (          _NS|			  _IND ),
 [IND_STATIC]	   	=  (          _NS|_STA|                   _IND ),
 [RET_BCO]              =  ( 0                                         ),
 [RET_SMALL]		=  (     _BTM|                       _SRT      ),
 [RET_BIG]		=  (                                 _SRT      ),
 [RET_FUN]              =  ( 0                                         ),
 [UPDATE_FRAME]        	=  (     _BTM                                  ),
 [CATCH_FRAME]	   	=  (     _BTM                                  ),
 [UNDERFLOW_FRAME]      =  (     _BTM                                  ),
 [STOP_FRAME]           =  (     _BTM                                  ),
 [BLACKHOLE]            =  (          _NS|              _UPT           ),
 [BLOCKING_QUEUE]	=  ( 	      _NS|         _MUT|_UPT           ),
 [MVAR_CLEAN]	   	=  (_HNF|     _NS|         _MUT|_UPT           ),
 [MVAR_DIRTY]	   	=  (_HNF|     _NS|         _MUT|_UPT           ),
 [TVAR]                 =  (_HNF|     _NS|         _MUT|_UPT           ),
 [ARR_WORDS]            =  (_HNF|     _NS|              _UPT           ),
 [MUT_ARR_PTRS_CLEAN]  	=  (_HNF|     _NS|         _MUT|_UPT           ),
 [MUT_ARR_PTRS_DIRTY]  	=  (_HNF|     _NS|         _MUT|_UPT           ),
 [MUT_ARR_PTRS_FROZEN0]	=  (_HNF|     _NS|         _MUT|_UPT           ),
 [MUT_ARR_PTRS_FROZEN] 	=  (_HNF|     _NS|              _UPT           ),
 [MUT_VAR_CLEAN]	=  (_HNF|     _NS|         _MUT|_UPT           ),
 [MUT_VAR_DIRTY]	=  (_HNF|     _NS|         _MUT|_UPT           ),
 [WEAK]		   	=  (_HNF|     _NS|              _UPT           ),
 [PRIM]  	   	=  (_HNF|     _NS|              _UPT           ),
 [MUT_PRIM]  	   	=  (_HNF|     _NS|         _MUT|_UPT           ),
 [TSO]                  =  (_HNF|     _NS|         _MUT|_UPT           ),
 [STACK]                =  (_HNF|     _NS|         _MUT|_UPT           ),
 [TREC_CHUNK]           =  (          _NS|         _MUT|_UPT           ),
 [ATOMICALLY_FRAME]     =  (     _BTM                                  ),
 [CATCH_RETRY_FRAME]    =  (     _BTM                                  ),
 [CATCH_STM_FRAME]      =  (     _BTM                                  ),
 [WHITEHOLE]		=  ( 0                                         )
};

#if N_CLOSURE_TYPES != 61
#error Closure types changed: update ClosureFlags.c!
#endif
