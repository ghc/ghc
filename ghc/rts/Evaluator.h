/* -----------------------------------------------------------------------------
 * $Id: Evaluator.h,v 1.5 1999/10/22 15:58:25 sewardj Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Prototypes for functions in Evaluator.c
 *
 * ---------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Sizes of objects it constructs
 * (used by Assembler)
 * ------------------------------------------------------------------------*/

#define Izh_sizeW       CONSTR_sizeW(0,sizeofW(StgInt))
#define I64zh_sizeW     CONSTR_sizeW(0,sizeofW(StgInt64))
#define Wzh_sizeW       CONSTR_sizeW(0,sizeofW(StgWord))
#define Azh_sizeW       CONSTR_sizeW(0,sizeofW(StgAddr))
#define Czh_sizeW       CONSTR_sizeW(0,sizeofW(StgWord))
#define Fzh_sizeW       CONSTR_sizeW(0,sizeofW(StgFloat))
#define Dzh_sizeW       CONSTR_sizeW(0,sizeofW(StgDouble))
#define Stablezh_sizeW  CONSTR_sizeW(0,sizeofW(StgStablePtr))
#define Genericzh_sizeW CONSTR_sizeW(1,0)

/* --------------------------------------------------------------------------
 * 
 * ------------------------------------------------------------------------*/

extern StgThreadReturnCode enter        ( StgClosurePtr obj );

extern nat marshall   ( char arg_ty, void* arg );
extern nat unmarshall ( char res_ty, void* res );
extern nat argSize    ( const char* ks );


extern StgInt          PopTaggedInt        ( void ) ;
extern StgWord         PopTaggedWord       ( void ) ;
extern StgAddr         PopTaggedAddr       ( void ) ;
extern StgStablePtr    PopTaggedStablePtr  ( void ) ;
extern StgChar         PopTaggedChar       ( void ) ;
extern StgFloat        PopTaggedFloat      ( void ) ;
extern StgDouble       PopTaggedDouble     ( void ) ;

extern void   PushTaggedInt        ( StgInt       );
extern void   PushTaggedWord       ( StgWord      );
extern void   PushTaggedAddr       ( StgAddr      );
extern void   PushTaggedStablePtr  ( StgStablePtr );
extern void   PushTaggedChar       ( StgChar      );
extern void   PushTaggedFloat      ( StgFloat     );
extern void   PushTaggedDouble     ( StgDouble    );

extern void   PushPtr        ( StgPtr );
extern StgPtr PopPtr         ( void );
