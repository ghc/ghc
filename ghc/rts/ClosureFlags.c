/* -----------------------------------------------------------------------------
 * $Id: ClosureFlags.c,v 1.2 1999/05/11 16:47:49 keithw Exp $
 *
 * (c) The GHC Team 1998-1999
 *
 * Closure type flags
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

StgWord16 closure_flags[] = {

/* 
 * These *must* be in the same order as the closure types in
 * ClosureTypes.h.
 */

/*			     HNF  BTM   NS  STA  THU MUT UPT SRT */
				                                    
/* INVALID_OBJECT       */ ( 0                                   ),
/* CONSTR  		*/ (_HNF|     _NS                        ),
/* CONSTR_1_0	   	*/ (_HNF|     _NS                        ),
/* CONSTR_0_1	   	*/ (_HNF|     _NS                        ),
/* CONSTR_2_0	   	*/ (_HNF|     _NS                        ),
/* CONSTR_1_1	   	*/ (_HNF|     _NS                        ),
/* CONSTR_0_2	   	*/ (_HNF|     _NS                        ),
/* CONSTR_INTLIKE 	*/ (_HNF|     _NS|_STA                   ),
/* CONSTR_CHARLIKE  	*/ (_HNF|     _NS|_STA                   ),
/* CONSTR_STATIC	*/ (_HNF|     _NS|_STA                   ),
/* CONSTR_NOCAF_STATIC  */ (_HNF|     _NS|_STA                   ),
/* FUN		   	*/ (_HNF|     _NS|                  _SRT ),
/* FUN_1_0		*/ (_HNF|     _NS                        ),
/* FUN_0_1		*/ (_HNF|     _NS                        ),
/* FUN_2_0		*/ (_HNF|     _NS                        ),
/* FUN_1_1		*/ (_HNF|     _NS                        ),
/* FUN_0_2		*/ (_HNF|     _NS                        ),
/* FUN_STATIC	   	*/ (_HNF|     _NS|_STA|             _SRT ),
/* THUNK		*/ (     _BTM|         _THU|        _SRT ),
/* THUNK_1_0		*/ (     _BTM|         _THU|        _SRT ),
/* THUNK_0_1		*/ (     _BTM|         _THU|        _SRT ),
/* THUNK_2_0		*/ (     _BTM|         _THU|        _SRT ),
/* THUNK_1_1		*/ (     _BTM|         _THU|        _SRT ),
/* THUNK_0_2		*/ (     _BTM|         _THU|        _SRT ),
/* THUNK_STATIC	   	*/ (     _BTM|    _STA|_THU|        _SRT ),
/* THUNK_SELECTOR	*/ (     _BTM|         _THU|        _SRT ),
/* BCO		   	*/ (_HNF|     _NS                        ),
/* AP_UPD		*/ (     _BTM|         _THU              ),
/* PAP		   	*/ (_HNF|     _NS                        ),
/* IND		   	*/ ( 0                                   ),
/* IND_OLDGEN	   	*/ ( 0                                   ),
/* IND_PERM		*/ ( 0                                   ),
/* IND_OLDGEN_PERM	*/ ( 0                                   ),
/* IND_STATIC	   	*/ (              _STA                   ),
/* CAF_UNENTERED        */ ( 0                                   ),
/* CAF_ENTERED          */ ( 0                                   ),
/* CAF_BLACKHOLE        */ ( 	 _BTM|_NS|              _UPT     ),
/* RET_BCO		*/ (     _BTM                            ),
/* RET_SMALL		*/ (     _BTM|                       _SRT),
/* RET_VEC_SMALL	*/ (     _BTM|                       _SRT),
/* RET_BIG		*/ (                                 _SRT),
/* RET_VEC_BIG	   	*/ (                                 _SRT),
/* RET_DYN		*/ (                                 _SRT),
/* UPDATE_FRAME         */ (     _BTM                            ),
/* CATCH_FRAME	   	*/ (     _BTM                            ),
/* STOP_FRAME	   	*/ (     _BTM                            ),
/* SEQ_FRAME 	   	*/ (     _BTM                            ),
/* BLACKHOLE		*/ ( 	      _NS|              _UPT     ),
/* BLACKHOLE_BQ	   	*/ ( 	      _NS|         _MUT|_UPT     ),
/* SE_BLACKHOLE		*/ ( 	      _NS|              _UPT     ),
/* SE_CAF_BLACKHOLE	*/ ( 	      _NS|              _UPT     ),
/* MVAR		   	*/ (_HNF|     _NS|         _MUT|_UPT     ),
/* ARR_WORDS		*/ (_HNF|     _NS|              _UPT     ),
/* MUT_ARR_PTRS	   	*/ (_HNF|     _NS|         _MUT|_UPT     ),
/* MUT_ARR_PTRS_FROZEN  */ (_HNF|     _NS|         _MUT|_UPT     ),
/* MUT_VAR		*/ (_HNF|     _NS|         _MUT|_UPT     ),
/* WEAK		   	*/ (_HNF|     _NS|              _UPT     ),
/* FOREIGN		*/ (_HNF|     _NS|              _UPT     ),
/* STABLE_NAME	   	*/ (_HNF|     _NS|              _UPT     ),
/* TSO                  */ (_HNF|     _NS|         _MUT|_UPT     ),
/* BLOCKED_FETCH	*/ (_HNF|     _NS                        ),
/* FETCH_ME		*/ (_HNF|     _NS                        ),
/* EVACUATED		*/ ( 0                                   )
};
