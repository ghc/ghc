
/* -----------------------------------------------------------------------------
 * $Id: Assembler.h,v 1.15 2000/06/15 13:18:08 daan Exp $
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

/* Make this compilable with Visual C++ */
#ifndef HAVE_INT64
#define HAVE_INT64
#ifdef _MSC_VER
typedef __int64            int64;
typedef unsigned __int64   nat64;
#else
typedef long long          int64;
typedef unsigned long long nat64;
#endif
#endif

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
typedef int64           AsmInt64;  
typedef unsigned int    AsmWord;
typedef void*           AsmAddr;
typedef unsigned char   AsmChar;
typedef float           AsmFloat;       /* ToDo: not on Alphas! */
typedef double          AsmDouble;
typedef char*           AsmString;

typedef int   AsmSp;   /* stack offset                  */
typedef int   AsmPc;   /* program counter		*/
typedef AsmSp AsmVar;  /* offset of a Var on the stack  */

/* I want to #include this file into the file that defines the
 * functions but I don't want to expose the structures that
 * these types point to.
 * This hack is the best I could think of.  Surely there's a better way?
 */
#ifdef INSIDE_ASSEMBLER_C
/* these types are defined in Assembler.c */
typedef 
   enum { 
     Asm_RefNoOp,    /* Pointer which needs no further messing with */
     Asm_RefObject,   /* Reference to malloc'd AsmCAF/AsmBCO/AsmCon */
     Asm_RefHugs,          /* Reference to Hugs name or tycon table */

     Asm_NonPtrWord,                          /* A non-pointer word */
     Asm_Insn8,                                /* One BCO insn byte */
   }
   Asm_Kind;

typedef
   struct {
      Asm_Kind  kind;
      StgWord   val;   /* StgWord is allegedly big enough to also hold
                          a pointer, on all platforms */
   }
   Asm_Entity;


   struct AsmObject_ {
      unsigned int magic;
      struct AsmObject_* next;
      enum { Asm_BCO, Asm_CAF, Asm_Con } kind;
      int           sizeEntities;
      int           usedEntities;
      Asm_Entity*   entities;
      StgClosure*   closure;

      int           n_refs;          /* number of ptr words  */
      int           n_words;         /* number of words      */
      int           n_insns;         /* number of insn BYTES */

      /* AsmCon specifics */
      StgInfoTable* itbl;

      /* AsmBCO specifics */
      int /*StgExpr*/ stgexpr;       /* stg tree for debugging */
      AsmSp           sp;            /* simulated sp */
      AsmSp           max_sp;        /* high-tide of sp */
      Instr           lastOpc;       /* last opcode, for peephole opt */
   };
   /* AsmObject_ is only mentioned in Assembler.c; clients use
      AsmObject/AsmBCO/AsmCAF/AsmCon. 
   */

typedef StgInfoTable*       AsmInfo;
typedef struct AsmObject_*  AsmBCO;
typedef struct AsmObject_*  AsmCAF;
typedef struct AsmObject_*  AsmCon;
typedef struct AsmObject_*  AsmObject;
typedef Instr               AsmInstr;
#else
/* the types we export are totally opaque */
typedef void*               AsmObject;
typedef void*               AsmBCO;
typedef void*               AsmCAF;
typedef void*               AsmCon;
typedef void*               AsmInfo;
typedef void*               AsmClosure;
typedef unsigned int        AsmInstr;
#endif



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
  INTEGER_REP = 'Z',  
  WORD_REP    = 'W',     
  ADDR_REP    = 'A',     
  FLOAT_REP   = 'F',    
  DOUBLE_REP  = 'D',   
  STABLE_REP  = 's',   /* StablePtr a */
#ifdef PROVIDE_FOREIGN
  FOREIGN_REP = 'f',   /* ForeignObj  */
#endif
#ifdef PROVIDE_WEAK
  WEAK_REP    = 'w',   /* Weak a      */
#endif
  BARR_REP     = 'x',  /* PrimByteArray          a */
  MUTBARR_REP  = 'm',  /* PrimMutableByteArray s a */

  /* The following can't be passed to C */
  PTR_REP      = 'P',      
  ALPHA_REP    = 'a',  /* a                        */
  BETA_REP     = 'b',  /* b			   */
  GAMMA_REP    = 'c',  /* c                        */
  DELTA_REP    = 'd',  /* d                        */
  BOOL_REP     = 'B',  /* Bool			   */
  IO_REP       = 'i',  /* IO a	                   */
  HANDLER_REP  = 'H',  /* Exception -> IO a	   */
  ERROR_REP    = 'E',  /* Exception		   */
  ARR_REP      = 'X',  /* PrimArray              a */
  REF_REP      = 'R',  /* Ref                  s a */
  MUTARR_REP   = 'M',  /* PrimMutableArray     s a */
  THREADID_REP = 'T',  /* ThreadId                 */
  MVAR_REP     = 'r',  /* MVar a                   */

  /* Allegedly used in the IO monad */
  VOID_REP     = 'v'      
} AsmRep;

/* --------------------------------------------------------------------------
 * Top-level control of the BCO generation + linking mechanism
 * ------------------------------------------------------------------------*/

extern void asmInitialise         ( void );
extern void asmAllocateHeapSpace  ( void );
extern void asmCopyAndLink        ( void );
extern void asmShutdown           ( void );

extern void* /* StgClosure* */ asmGetClosureOfObject ( AsmObject );

/* --------------------------------------------------------------------------
 * Allocating (top level) heap objects
 * ------------------------------------------------------------------------*/

extern AsmBCO     asmBeginBCO        ( int /*StgExpr*/ e );
extern void       asmEndBCO          ( AsmBCO bco );

extern AsmBCO     asmBeginContinuation ( AsmSp sp, int /*List*/ alts );
extern void       asmEndContinuation   ( AsmBCO bco );

extern AsmCAF     asmBeginCAF        ( void );
extern void       asmEndCAF          ( AsmCAF caf );

extern AsmInfo    asmMkInfo          ( AsmNat tag, AsmNat ptrs );
extern AsmCon     asmBeginCon        ( AsmInfo info );
extern void       asmEndCon          ( AsmCon con );

/* NB: we add ptrs to other objects in left-to-right order.
 * This is different from pushing arguments on the stack which is done
 * in right to left order.
 */
extern void       asmAddPtr          ( AsmObject obj, AsmObject arg );
extern int        asmRepSizeW        ( AsmRep rep );

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
extern void   asmConstAddr     ( AsmBCO bco, AsmAddr    x );
extern void   asmConstWord     ( AsmBCO bco, AsmWord    x );
extern void   asmConstChar     ( AsmBCO bco, AsmChar    x );
extern void   asmConstFloat    ( AsmBCO bco, AsmFloat   x );
extern void   asmConstDouble   ( AsmBCO bco, AsmDouble  x );
extern void   asmConstInteger  ( AsmBCO bco, AsmString  x );
             
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

extern AsmPrim asmPrimOps[]; /* null terminated list */

extern AsmPrim* asmFindPrim      ( char* s );
extern AsmPrim* asmFindPrimop    ( AsmInstr prefix, AsmInstr op );
extern AsmSp    asmBeginPrim     ( AsmBCO bco );
extern void     asmEndPrim       ( AsmBCO bco, const AsmPrim* prim, 
                                               AsmSp base );
extern char*    asmGetPrimopName ( AsmPrim* p );

extern void* /* StgBCO* */ asm_BCO_catch    ( void );
extern void* /* StgBCO* */ asm_BCO_raise    ( void );
extern void* /* StgBCO* */ asm_BCO_seq      ( void );
extern void* /* StgBCO* */ asm_BCO_takeMVar ( void );


/* --------------------------------------------------------------------------
 * Heap manipulation
 * ------------------------------------------------------------------------*/

extern AsmVar asmPushRefHugs   ( AsmBCO bco, int /*Name*/ n );
extern AsmVar asmPushRefObject ( AsmBCO bco, AsmObject p );
extern AsmVar asmPushRefNoOp   ( AsmBCO bco, StgPtr p );

extern void   asmAddRefObject  ( AsmObject obj, AsmObject p );
extern void   asmAddRefNoOp    ( AsmObject obj, StgPtr p );
extern void   asmAddRefHugs    ( AsmObject obj,int /*Name*/ n );

extern AsmVar asmAllocCONSTR   ( AsmBCO bco, AsmInfo info );

extern AsmSp  asmBeginPack     ( AsmBCO bco );
extern void   asmEndPack       ( AsmBCO bco, AsmVar v, AsmSp start, 
                                                       AsmInfo info );

extern void   asmBeginUnpack   ( AsmBCO bco );
extern void   asmEndUnpack     ( AsmBCO bco );

extern AsmVar asmAllocAP       ( AsmBCO bco, AsmNat size );
extern AsmSp  asmBeginMkAP     ( AsmBCO bco );
extern void   asmEndMkAP       ( AsmBCO bco, AsmVar v, AsmSp start );

extern AsmVar asmAllocPAP      ( AsmBCO bco, AsmNat size );
extern AsmSp  asmBeginMkPAP    ( AsmBCO bco );
extern void   asmEndMkPAP      ( AsmBCO bco, AsmVar v, AsmSp start );

#ifdef XMLAMBDA
/*------------------------------------------------------------------------
 XMlambda primitives.
------------------------------------------------------------------------*/
typedef AsmInt  AsmIndex;

/* Rows */
extern AsmVar asmAllocRow      ( AsmBCO bco, AsmNat /*number of fields*/ n );

extern AsmSp  asmBeginPackRow  ( AsmBCO bco );
extern void   asmEndPackRow    ( AsmBCO bco, AsmVar v, AsmSp start, 
                                             AsmNat /*number of fields*/ n );

extern void   asmBeginUnpackRow( AsmBCO bco );
extern void   asmEndUnpackRow  ( AsmBCO bco );

extern AsmPrim primRowRemoveAtN;
extern AsmPrim primRowIndexAtN;

/* Inj */
extern AsmVar asmInj( AsmBCO bco, AsmVar var );
extern AsmVar asmInjConst( AsmBCO bco, AsmIndex i );
extern AsmVar asmUnInj( AsmBCO bco );
extern AsmPc  asmTestInj( AsmBCO bco, AsmVar var );
extern AsmPc  asmTestInjConst( AsmBCO, AsmIndex i );
extern AsmVar asmConstIndex( AsmBCO bco, AsmIndex x );
#endif

/* --------------------------------------------------------------------------
 * C-call and H-call
 * ------------------------------------------------------------------------*/

extern AsmPrim ccall_ccall_Id;
extern AsmPrim ccall_ccall_IO;
extern AsmPrim ccall_stdcall_Id;
extern AsmPrim ccall_stdcall_IO;

typedef struct {
  unsigned int  num_args;
  char*         arg_tys;
  unsigned int  num_results;
  char*         result_tys;
} CFunDescriptor;

CFunDescriptor* mkDescriptor( char* as, char* rs );

/*-------------------------------------------------------------------------*/
