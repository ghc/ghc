/* -----------------------------------------------------------------------------
 * $Id: Evaluator.h,v 1.2 1998/12/02 13:28:21 simonm Exp $
 *
 * Prototypes for functions in Evaluator.c
 *
 * ---------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Sizes of objects it constructs
 * (used by Assembler)
 * ------------------------------------------------------------------------*/

#define IZh_sizeW       CONSTR_sizeW(0,sizeofW(StgInt))
#define I64Zh_sizeW     CONSTR_sizeW(0,sizeofW(StgInt64))
#define WZh_sizeW       CONSTR_sizeW(0,sizeofW(StgWord))
#define AZh_sizeW       CONSTR_sizeW(0,sizeofW(StgAddr))
#define CZh_sizeW       CONSTR_sizeW(0,sizeofW(StgWord))
#define FZh_sizeW       CONSTR_sizeW(0,sizeofW(StgFloat))
#define DZh_sizeW       CONSTR_sizeW(0,sizeofW(StgDouble))
#define StableZh_sizeW  CONSTR_sizeW(0,sizeofW(StgStablePtr))
#define GenericZh_sizeW CONSTR_sizeW(1,0)

/* --------------------------------------------------------------------------
 * 
 * ------------------------------------------------------------------------*/

extern StgThreadReturnCode enter        ( StgClosurePtr obj );

extern nat marshall   ( char arg_ty, void* arg );
extern nat unmarshall ( char res_ty, void* res );
extern nat argSize    ( const char* ks );

