
/* -----------------------------------------------------------------------------
 * $Id: Assembler.h,v 1.5 1999/03/01 14:47:09 sewardj Exp $
 *
 * (c) The GHC Team 1994-1998.
 *
 * Bytecode assembler
 *
 * NB This is one of the few files shared between Hugs and the runtime system,
 * so it is very important that it not conflict with either and that it not
 * rely on either.  
 * (In fact, it might be fun to create a GreenCard interface to this file too.)
 * ---------------------------------------------------------------------------*/

/* ToDo: put this somewhere more sensible */
extern void DEBUG_LoadSymbols( char *name );

/* This file is supposed to be somewhat self-contained because it is one
 * of the major external interfaces to the runtime system.
 * Keeping it self-contained reduces the chance of conflict with Hugs
 * (or anything else that includes it).
 * The big disadvantage of being self-contained is that definitions
 * like AsmNat8, etc duplicate definitions in StgTypes.h.
 * I'm not sure what we can do about this but, if you try to fix it,
 * please remember why it was done this way in the first place.
 * -- ADR
 */

typedef unsigned char   AsmNat8;
typedef unsigned int    AsmNat;
typedef signed   int    AsmInt;
typedef signed long long int AsmInt64;  /* ToDo: not portable!  */
typedef unsigned int    AsmWord;
typedef void*           AsmAddr;
typedef unsigned char   AsmChar;
typedef float           AsmFloat;       /* ToDo: not on Alphas! */
typedef double          AsmDouble;
typedef char*           AsmString;

/* I want to #include this file into the file that defines the
 * functions but I don't want to expose the structures that
 * these types point to.
 * This hack is the best I could think of.  Surely there's a better way?
 */
#ifdef INSIDE_ASSEMBLER_C
typedef struct AsmObject_ *AsmObject;
typedef struct AsmBCO_    *AsmBCO;
typedef struct AsmCAF_    *AsmCAF;
typedef struct AsmCon_    *AsmCon;
typedef StgInfoTable      *AsmInfo;
typedef StgClosure        *AsmClosure;
typedef Instr              AsmInstr;
#else
/* the types we export are totally opaque */
typedef void              *AsmObject;
typedef void              *AsmBCO;
typedef void              *AsmCAF;
typedef void              *AsmCon;
typedef void              *AsmInfo;
typedef void              *AsmClosure;
typedef unsigned int       AsmInstr;
#endif

typedef int   AsmSp;   /* stack offset                  */
typedef int   AsmPc;   /* program counter		*/
typedef AsmSp AsmVar;  /* offset of a Var on the stack  */

/* --------------------------------------------------------------------------
 * "Types" used within the assembler
 *
 * Some of these types are synonyms for the same underlying representation
 * to let Hugs (or whoever) generate useful Haskell types from the type
 * of a primitive operation.
 *
 *  Extreme care should be taken if you change any of these - the
 *  same constants are hardwired into Hugs (ILLEGAL_REP) and into
 *  pieces of assembly language used to implement foreign import/export.
 *  And, of course, you'll have to change the primop table in Assembler.c
 * ------------------------------------------------------------------------*/

typedef enum {
  ILLEGAL_REP = 0,

  /* The following can be passed to C */
  CHAR_REP    = 'C',     
  INT_REP     = 'I',      
#ifdef PROVIDE_INT64
  INT64_REP   = 'z', 
#endif
#ifdef PROVIDE_INTEGER
  INTEGER_REP = 'Z',  
#endif
#ifdef PROVIDE_WORD
  WORD_REP    = 'W',     
#endif
#ifdef PROVIDE_ADDR
  ADDR_REP    = 'A',     
#endif
  FLOAT_REP   = 'F',    
  DOUBLE_REP  = 'D',   
#ifdef PROVIDE_STABLE
  STABLE_REP  = 's',   /* StablePtr a */
#endif
#ifdef PROVIDE_FOREIGN
  FOREIGN_REP = 'f',   /* ForeignObj  */
#endif
#ifdef PROVIDE_WEAK
  WEAK_REP    = 'w',   /* Weak a      */
#endif
#ifdef PROVIDE_ARRAY
  BARR_REP     = 'x',  /* PrimByteArray          a */
  MUTBARR_REP  = 'm',  /* PrimMutableByteArray s a */
#endif

  /* The following can't be passed to C */
  PTR_REP      = 'P',      
  ALPHA_REP    = 'a',  /* a                        */
  BETA_REP     = 'b',  /* b			   */
  GAMMA_REP    = 'c',  /* c                        */
  BOOL_REP     = 'B',  /* Bool			   */
  IO_REP       = 'i',  /* IO a	                   */
  HANDLER_REP  = 'H',  /* Exception -> IO a	   */
  ERROR_REP    = 'E',  /* Exception		   */
#ifdef PROVIDE_ARRAY		
  ARR_REP      = 'X',  /* PrimArray              a */
  REF_REP      = 'R',  /* Ref                  s a */
  MUTARR_REP   = 'M',  /* PrimMutableArray     s a */
#endif
#ifdef PROVIDE_CONCURRENT
  THREADID_REP = 'T',  /* ThreadId                 */
  MVAR_REP     = 'r',  /* MVar a                   */
#endif

  /* Allegedly used in the IO monad */
  VOID_REP     = 'v'      
} AsmRep;

/* --------------------------------------------------------------------------
 * Allocating (top level) heap objects
 * ------------------------------------------------------------------------*/

extern AsmBCO     asmBeginBCO        ( int /*StgExpr*/ e );
extern void       asmEndBCO          ( AsmBCO bco );

extern AsmBCO     asmBeginContinuation ( AsmSp sp, int /*List*/ alts );
extern void       asmEndContinuation   ( AsmBCO bco );

extern AsmObject  asmMkObject        ( AsmClosure c );

extern AsmCAF     asmBeginCAF        ( void );
extern void       asmEndCAF          ( AsmCAF caf, AsmBCO body );

extern AsmInfo    asmMkInfo          ( AsmNat tag, AsmNat ptrs );
extern AsmCon     asmBeginCon        ( AsmInfo info );
extern void       asmEndCon          ( AsmCon con );

/* NB: we add ptrs to other objects in left-to-right order.
 * This is different from pushing arguments on the stack which is done
 * in right to left order.
 */
extern void       asmAddPtr          ( AsmObject obj, AsmObject arg );

extern int        asmObjectHasClosure( AsmObject obj );
extern AsmClosure asmClosureOfObject ( AsmObject obj );
extern void       asmMarkObject      ( AsmObject obj );

/* --------------------------------------------------------------------------
 * Generating instruction streams
 * ------------------------------------------------------------------------*/
                               
extern AsmSp  asmBeginArgCheck ( AsmBCO bco );
extern void   asmEndArgCheck   ( AsmBCO bco, AsmSp last_arg );
                               
extern AsmSp  asmBeginEnter    ( AsmBCO bco );
extern void   asmEndEnter      ( AsmBCO bco, AsmSp sp1, AsmSp sp2 );
                               
extern AsmVar asmBind          ( AsmBCO bco, AsmRep rep );
extern void   asmVar           ( AsmBCO bco, AsmVar v, AsmRep rep );
                               
extern AsmSp  asmBeginCase     ( AsmBCO bco );
extern void   asmEndCase       ( AsmBCO bco );
extern AsmSp  asmContinuation  ( AsmBCO bco, AsmBCO ret_addr );

extern AsmSp  asmBeginAlt      ( AsmBCO bco );
extern void   asmEndAlt        ( AsmBCO bco, AsmSp  sp );
extern AsmPc  asmTest          ( AsmBCO bco, AsmWord tag );
extern AsmPc  asmTestInt       ( AsmBCO bco, AsmVar v, AsmInt x );
extern void   asmFixBranch     ( AsmBCO bco, AsmPc pc );
extern void   asmPanic         ( AsmBCO bco );
                               
extern AsmVar asmBox           ( AsmBCO bco, AsmRep rep );
extern AsmVar asmUnbox         ( AsmBCO bco, AsmRep rep );
extern void   asmReturnUnboxed ( AsmBCO bco, AsmRep rep );             

/* push unboxed Ints, Floats, etc */
extern void   asmConstInt      ( AsmBCO bco, AsmInt     x );
#ifdef PROVIDE_ADDR
extern void   asmConstAddr     ( AsmBCO bco, AsmAddr    x );
#endif
#ifdef PROVIDE_WORD
extern void   asmConstWord     ( AsmBCO bco, AsmWord    x );
#endif
extern void   asmConstChar     ( AsmBCO bco, AsmChar    x );
extern void   asmConstFloat    ( AsmBCO bco, AsmFloat   x );
extern void   asmConstDouble   ( AsmBCO bco, AsmDouble  x );
#ifdef PROVIDE_INT64
extern void   asmConstInt64    ( AsmBCO bco, AsmInt64   x );
#endif
#ifdef PROVIDE_INTEGER
extern void   asmConstInteger  ( AsmBCO bco, AsmString  x );
#endif
             
/* Which monad (if any) does the primop live in? */
typedef enum {
    MONAD_Id,  /* no monad (aka the identity monad) */
    MONAD_ST,
    MONAD_IO
} AsmMonad;

typedef struct {
    char*    name;
    char*    args;
    char*    results;
    AsmMonad monad;
    AsmNat8  prefix; /* should be StgInstr           */
    AsmNat8  opcode; /* should be Primop1 or Primop2 */
} AsmPrim;

extern const AsmPrim asmPrimOps[]; /* null terminated list */

extern const AsmPrim* asmFindPrim    ( char* s );
extern const AsmPrim* asmFindPrimop  ( AsmInstr prefix, AsmInstr op );
extern AsmSp          asmBeginPrim   ( AsmBCO bco );
extern void           asmEndPrim     ( AsmBCO bco, const AsmPrim* prim, AsmSp base );

extern AsmBCO asm_BCO_catch ( void );
extern AsmBCO asm_BCO_raise ( void );
extern AsmBCO asm_BCO_seq   ( void );


/* --------------------------------------------------------------------------
 * Heap manipulation
 * ------------------------------------------------------------------------*/

extern AsmVar asmClosure       ( AsmBCO bco, AsmObject p );

extern AsmVar asmAllocCONSTR   ( AsmBCO bco, AsmInfo info );

extern AsmSp  asmBeginPack     ( AsmBCO bco );
extern void   asmEndPack       ( AsmBCO bco, AsmVar v, AsmSp start, AsmInfo info );

extern void   asmBeginUnpack   ( AsmBCO bco );
extern void   asmEndUnpack     ( AsmBCO bco );

extern AsmVar asmAllocAP       ( AsmBCO bco, AsmNat size );
extern AsmSp  asmBeginMkAP     ( AsmBCO bco );
extern void   asmEndMkAP       ( AsmBCO bco, AsmVar v, AsmSp start );

extern AsmVar asmAllocPAP      ( AsmBCO bco, AsmNat size );
extern AsmSp  asmBeginMkPAP    ( AsmBCO bco );
extern void   asmEndMkPAP      ( AsmBCO bco, AsmVar v, AsmSp start );

/* --------------------------------------------------------------------------
 * C-call and H-call
 * ------------------------------------------------------------------------*/

extern const AsmPrim ccall_Id;
extern const AsmPrim ccall_IO;

typedef struct {
  char *        arg_tys;
  int           arg_size;
  char *        result_tys;
  int           result_size;
} CFunDescriptor;

typedef struct {
  char *        arg_tys;
  char *        result_tys;
} HFunDescriptor;

CFunDescriptor* mkDescriptor( char* as, char* rs );

/*-------------------------------------------------------------------------*/
