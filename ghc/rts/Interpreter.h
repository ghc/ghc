
/* -----------------------------------------------------------------------------
 * $Id: Interpreter.h,v 1.3 2001/02/12 04:55:33 chak Exp $
 *
 * (c) The GHC Team, 1998-2000.
 *
 * Prototypes for functions in Interpreter.c
 *
 * ---------------------------------------------------------------------------*/

extern StgThreadReturnCode interpretBCO ( Capability* cap );

typedef unsigned short UShort;

#if 0
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

extern int    numEnters;

/*-------------------------------------------------------------------------*/
#ifdef XMLAMBDA

#define MAX_CALL_VALUES  100

/* Self contained CallInfo structure for the i_ccall instruction */
typedef struct _CallInfo {
  unsigned int  argCount;
  unsigned int  resultCount;
  char          callConv;     /* 's'=stdcall, 'c'=ccall */
  
/* The strings arg_tys and result_tys reside here. 
   This allows us to put the complete CallInfo in the nonptrwords of a BCO */
  char          data[MAX_CALL_VALUES+2];  
} CallInfo;

#endif
#endif
