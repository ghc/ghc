
/* --------------------------------------------------------------------------
 * Bytecode assembler
 *
 * Copyright (c) 1994-1998.
 *
 * $RCSfile: Assembler.c,v $
 * $Revision: 1.8 $
 * $Date: 1999/04/27 10:07:15 $
 *
 * This module provides functions to construct BCOs and other closures
 * required by the bytecode compiler.
 *
 * It is supposed to shield the compiler from platform dependent information
 * such as:
 *
 * o sizeof(StgFloat)
 * o sizeof(I#)
 *
 * and from details of how the abstract machine is implemented such as:
 *
 * o what does a BCO look like?
 * o how many bytes does the "Push InfoTable" instruction require?
 *
 * Details of design:
 * o (To handle letrecs) We allocate Aps, Paps and Cons using number of
 *   heap allocated args to determine size.
 *   We can't handle unboxed args :-(
 * o All stack offsets are relative to position of Sp at start of
 *   function or thunk (not BCO - consider continuations)
 * o Active thunks must be roots during GC - how to achieve this?
 * o Each BCO contains its own stack and heap check
 *   We don't try to exploit the Hp check optimisation - easier to make
 *   each thunk stand on its own.
 * o asBind returns a "varid" (which is, in fact, a stack offset)
 *   asVar acts on a "varid" - combining it with the current stack size to
 *   determine actual position
 * o Assembler.h uses totally neutral types: strings, floats, ints, etc
 *   to minimise conflicts with other parts of the system.
 * Simulated Stack
 * ------------------------------------------------------------------------*/

#include "Rts.h"

#ifdef INTERPRETER

#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Bytecodes.h"
#include "Printer.h"
#include "Disassembler.h"
#include "Evaluator.h"
#include "StgMiscClosures.h"
#include "Storage.h"

#define INSIDE_ASSEMBLER_C
#include "Assembler.h"
#undef INSIDE_ASSEMBLER_C

/* --------------------------------------------------------------------------
 * References between BCOs
 *
 * These are necessary because there can be circular references between 
 * BCOs so we have to keep track of all the references to each object
 * and fill in all the references once we're done.
 *
 * ToDo: generalise to allow references between any objects
 * ------------------------------------------------------------------------*/

typedef struct {
    AsmObject ref;  /* who refers to it                       */
    AsmNat i;       /* index into some table held by referer  */
} AsmRef;

/* --------------------------------------------------------------------------
 * Queues (of instructions, ptrs, nonptrs)
 * ------------------------------------------------------------------------*/

#define Queue Instrs
#define Type  StgWord8
#include "QueueTemplate.h"
#undef Type
#undef Queue

#define Queue Ptrs
#define Type  AsmObject
#include "QueueTemplate.h"
#undef Type
#undef Queue

#define Queue Refs
#define Type  AsmRef
#include "QueueTemplate.h"
#undef Type
#undef Queue

#define Queue NonPtrs
#define Type  StgWord
#include "QueueTemplate.h"
#undef Type
#undef Queue

/* --------------------------------------------------------------------------
 * AsmObjects are used to build heap objects.
 *
 * AsmObjects can contain circular references to each other
 * so we have to keep track of all the references which can't be filled
 * in yet.
 *
 * When we finish building an AsmObject, we allocate an actual heap object and
 * fill in all the references to the asmObject with pointers to the heap object.
 *
 * We obtain a limited form of polymorphism through inheritance by putting 
 * the AsmObject first in every structure (as in C++ implementations).
 * We use the closure type of the allocated object to figure out
 * where the payload lives in the closure.
 * ------------------------------------------------------------------------*/
/* ToDo: clean up terminology: is Closure right or should it be object or ... */

struct AsmObject_ {
    Refs           refs;
    Ptrs           ptrs;
    AsmNat         num_unresolved; /* number of unfilled references */
    StgClosure*    closure;        /* where object was allocated    */
};
    
struct AsmCon_ {
    struct AsmObject_ object;  /* must be first in struct */

    AsmInfo info;
};
  
struct AsmCAF_ {
    struct AsmObject_ object;  /* must be first in struct */
};

struct AsmBCO_ {
    struct AsmObject_ object;  /* must be first in struct */

    Instrs   is;          
    NonPtrs  nps;

    int /*StgExpr*/  stgexpr;    

    /* abstract machine ("executed" during compilation) */
    AsmSp    sp;          /* stack ptr */
    AsmSp    max_sp;
    StgWord  hp;          /* heap ptr  */
    StgWord  max_hp;
    Instr    lastOpc;
};

static void asmResolveRef( AsmObject obj, AsmNat i, AsmClosure reference )
{
    ASSERT(obj->closure);
    switch (get_itbl(obj->closure)->type) {
    case BCO:
        {
            StgBCO* bco = stgCast(StgBCO*,obj->closure);
            ASSERT(i < bco->n_ptrs && bcoConstPtr(bco,i) == NULL);
            bcoConstCPtr(bco,i) = reference;
            break;
        }
    case CAF_UNENTERED:
        {
            StgCAF* caf = stgCast(StgCAF*,obj->closure);
            ASSERT(i == 0 && caf->body == NULL);
            caf->body = reference;
            break;
        }
    case CONSTR:
        {
            StgClosure* con = stgCast(StgClosure*,obj->closure);
            ASSERT(i < get_itbl(con)->layout.payload.nptrs && payloadCPtr(con,i) == NULL);
            payloadCPtr(con,i) = reference;
            break;
        }
    case AP_UPD:
        {
            StgAP_UPD* ap = stgCast(StgAP_UPD*,obj->closure);
            ASSERT(i < 1+ap->n_args);
            if (i==0) {
                ASSERT(ap->fun == NULL);
                ap->fun = reference;
            } else {
                ASSERT(payloadCPtr(ap,i-1) == NULL);
                payloadCPtr(ap,i-1) = reference;
            }
            break;
        }
    default:
            barf("asmResolveRef");
    }
    obj->num_unresolved -= 1;
}

static void asmAddRef( AsmObject referent, AsmObject referer, AsmNat i )
{
    if (referent->closure) {
        asmResolveRef(referer,i,(AsmClosure)referent->closure);
    } else {
        insertRefs(&(referent->refs),(AsmRef){referer,i});
    }
}

void asmAddPtr( AsmObject obj, AsmObject arg )
{
    ASSERT(obj->closure == 0); /* can't extend an object once it's allocated */
    insertPtrs( &obj->ptrs, arg );
}

static void asmBeginObject( AsmObject obj )
{
    obj->closure = NULL;
    obj->num_unresolved = 0;
    initRefs(&obj->refs);
    initPtrs(&obj->ptrs);
}

static void asmEndObject( AsmObject obj, StgClosure* c )
{
    obj->num_unresolved = obj->ptrs.len;
    obj->closure = c;
    mapQueue(Ptrs,    AsmObject, obj->ptrs, asmAddRef(x,obj,i));
    mapQueue(Refs,    AsmRef,    obj->refs, asmResolveRef(x.ref,x.i,c));

    if (obj->num_unresolved == 0) {
        freePtrs(&obj->ptrs);
        freeRefs(&obj->refs);
        /* we don't print until all ptrs are resolved */
        IF_DEBUG(codegen,printObj(obj->closure));
    }
}

int asmObjectHasClosure ( AsmObject obj )
{
    return (obj->num_unresolved == 0 && obj->closure);
}

AsmClosure asmClosureOfObject ( AsmObject obj )
{
    ASSERT(asmObjectHasClosure(obj));
    return obj->closure;
}

void asmMarkObject ( AsmObject obj )
{
    ASSERT(obj->num_unresolved == 0 && obj->closure);
    obj->closure = MarkRoot(obj->closure);
}

/* --------------------------------------------------------------------------
 * Heap allocation
 * ------------------------------------------------------------------------*/

static StgClosure* asmAlloc( nat size )
{
    StgClosure* o = stgCast(StgClosure*,allocate(size));
    ASSERT( size >= MIN_NONUPD_SIZE + sizeofW(StgHeader) );
    /* printf("Allocated %p .. %p\n", o, o+size-1); */
    return o;
}

static void grabHpUpd( AsmBCO bco, nat size )
{
    /* ToDo: sometimes we should test for MIN_UPD_SIZE instead */
    ASSERT( size >= MIN_UPD_SIZE + sizeofW(StgHeader) );
    bco->hp += size;
}

static void grabHpNonUpd( AsmBCO bco, nat size )
{
    /* ToDo: sometimes we should test for MIN_UPD_SIZE instead */
    ASSERT( size >= MIN_NONUPD_SIZE + sizeofW(StgHeader) );
    bco->hp += size;
}

static void resetHp( AsmBCO bco, nat hp )
{
    bco->max_hp = stg_max(bco->hp,bco->max_hp);
    bco->hp     = hp;
}

static void resetSp( AsmBCO bco, AsmSp sp )
{
    bco->max_sp = stg_max(bco->sp,bco->max_sp);
    bco->sp     = sp;
}

/* --------------------------------------------------------------------------
 * 
 * ------------------------------------------------------------------------*/

AsmObject asmMkObject( AsmClosure c )
{
    AsmObject obj = malloc(sizeof(struct AsmObject_));
    if (obj == NULL) {
        barf("Can't allocate AsmObject");
    }
    asmBeginObject(obj);
    asmEndObject(obj,c);
    return obj;
}

AsmCon asmBeginCon( AsmInfo info )
{
    AsmCon con = malloc(sizeof(struct AsmCon_));
    if (con == NULL) {
        barf("Can't allocate AsmCon");
    }
    asmBeginObject(&con->object);
    con->info = info;
    return con;
}

void asmEndCon( AsmCon con )
{
    nat p  = con->object.ptrs.len;
    nat np = stg_max(0,MIN_NONUPD_SIZE-p);

    StgClosure* c = asmAlloc(CONSTR_sizeW(p,np));
    StgClosure* o = stgCast(StgClosure*,c);
    SET_HDR(o,con->info,??);
    mapQueue(Ptrs,    AsmObject, con->object.ptrs, payloadCPtr(o,i) = NULL);
    { nat i; for( i=0; i<np; ++i ) { payloadWord(o,p+i) = 0xdeadbeef; } }
    asmEndObject(&con->object,c);
}

AsmCAF asmBeginCAF( void )
{
    AsmCAF caf = malloc(sizeof(struct AsmCAF_));
    if (caf == NULL) {
        barf("Can't allocate AsmCAF");
    }
    asmBeginObject(&caf->object);
    return caf;
}

void asmEndCAF( AsmCAF caf, AsmBCO body )
{
    StgClosure* c = asmAlloc(CAF_sizeW());
    StgCAF*     o = stgCast(StgCAF*,c);
    SET_HDR(o,&CAF_UNENTERED_info,??);
    o->body  = NULL;
    o->value = stgCast(StgClosure*,0xdeadbeef);
    o->link  = stgCast(StgCAF*,0xdeadbeef);
    o->mut_link = NULL;
    asmAddPtr(&caf->object,&body->object);
    asmEndObject(&caf->object,c);
}

AsmBCO asmBeginBCO( int /*StgExpr*/ e )
{
    AsmBCO bco = malloc(sizeof(struct AsmBCO_));
    if (bco == NULL) {
        barf("Can't allocate AsmBCO");
    }
    asmBeginObject(&bco->object);
    initInstrs(&bco->is);
    initNonPtrs(&bco->nps);

    bco->stgexpr = e;
    bco->max_sp = bco->sp = 0;
    bco->max_hp = bco->hp = 0;
    bco->lastOpc = i_INTERNAL_ERROR;
    return bco;
}

void asmEndBCO( AsmBCO bco )
{
    nat p  = bco->object.ptrs.len;
    nat np = bco->nps.len;
    nat is = bco->is.len + 2;  /* 2 for stack check */

    StgClosure* c = asmAlloc(BCO_sizeW(p,np,is));
    StgBCO*     o = stgCast(StgBCO*,c);
    SET_HDR(o,&BCO_info,??);
    o->n_ptrs   = p;
    o->n_words  = np;
    o->n_instrs = is;
    o->stgexpr  = bco->stgexpr;
    mapQueue(Ptrs,    AsmObject, bco->object.ptrs, bcoConstCPtr(o,i) = NULL);
    mapQueue(NonPtrs, StgWord,   bco->nps,  bcoConstWord(o,i) = x);
    {
        nat j = 0;
        bco->max_sp = stg_max(bco->sp,bco->max_sp);
        bco->max_hp = stg_max(bco->hp,bco->max_hp);
        bcoInstr(o,j++) = i_STK_CHECK;
        bcoInstr(o,j++) = bco->max_sp;
        mapQueue(Instrs,  StgWord8,   bco->is,   bcoInstr(o,j++) = x);
        ASSERT(j == is);
    }
    freeInstrs(&bco->is);
    freeNonPtrs(&bco->nps);
    asmEndObject(&bco->object,c);
}

/* --------------------------------------------------------------------------
 * 
 * ------------------------------------------------------------------------*/

static void asmInstrOp ( AsmBCO bco, StgWord i )
{
    ASSERT(i <= BIGGEST_OPCODE); /* must be a valid opcode */
    bco->lastOpc = i;
    insertInstrs(&(bco->is),i);
}

static void asmInstr8 ( AsmBCO bco, StgWord i )
{
  if (i >= 256) {
    ASSERT(i < 256); /* must be a byte */
  }
    insertInstrs(&(bco->is),i);
}

static void asmInstr16 ( AsmBCO bco, StgWord i )
{
    ASSERT(i < 65536); /* must be a short */
    insertInstrs(&(bco->is),i / 256);
    insertInstrs(&(bco->is),i % 256);
}

static Instr asmInstrBack ( AsmBCO bco, StgWord n )
{
   return bco->is.elems[bco->is.len - n];
}

static void asmInstrRecede ( AsmBCO bco, StgWord n )
{
   if (bco->is.len < n) barf("asmInstrRecede");
   bco->is.len -= n;
}

static void asmPtr( AsmBCO bco, AsmObject x )
{
    insertPtrs( &bco->object.ptrs, x );
}

static void asmWord( AsmBCO bco, StgWord i )
{
    insertNonPtrs( &bco->nps, i );
}

#define asmWords(bco,ty,x)                               \
    {                                                    \
        union { ty a; AsmWord b[sizeofW(ty)]; } p;       \
        nat i;                                           \
        if (sizeof(ty) < sizeof(AsmWord)) p.b[0]=0;      \
        p.a = x;                                         \
        for( i = 0; i < sizeofW(ty); i++ ) {             \
            asmWord(bco,p.b[i]);                         \
        }                                                \
    }

static StgWord repSizeW( AsmRep rep )
{
    switch (rep) {
    case CHAR_REP:    return sizeofW(StgWord) + sizeofW(StgChar);

    case BOOL_REP:
    case INT_REP:     return sizeofW(StgWord) + sizeofW(StgInt);
    case WORD_REP:    return sizeofW(StgWord) + sizeofW(StgWord);
    case ADDR_REP:    return sizeofW(StgWord) + sizeofW(StgAddr);
    case FLOAT_REP:   return sizeofW(StgWord) + sizeofW(StgFloat);
    case DOUBLE_REP:  return sizeofW(StgWord) + sizeofW(StgDouble);
#ifdef PROVIDE_STABLE
    case STABLE_REP:  return sizeofW(StgWord) + sizeofW(StgWord);
#endif

    case INTEGER_REP: 
#ifdef PROVIDE_WEAK
    case WEAK_REP: 
#endif
#ifdef PROVIDE_FOREIGN
    case FOREIGN_REP: 
#endif
    case ALPHA_REP:    /* a                        */ 
    case BETA_REP:     /* b			   */ 
    case GAMMA_REP:    /* c			   */ 
    case HANDLER_REP:  /* IOError -> IO a	   */ 
    case ERROR_REP:    /* IOError		   */ 
    case ARR_REP    :  /* PrimArray              a */ 
    case BARR_REP   :  /* PrimByteArray          a */ 
    case REF_REP    :  /* Ref                  s a */ 
    case MUTARR_REP :  /* PrimMutableArray     s a */ 
    case MUTBARR_REP:  /* PrimMutableByteArray s a */ 
#ifdef PROVIDE_CONCURRENT
    case THREADID_REP: /* ThreadId                 */ 
    case MVAR_REP:     /* MVar a                   */ 
#endif
    case PTR_REP:     return sizeofW(StgPtr);

    case VOID_REP:    return sizeofW(StgWord);
    default:          barf("repSizeW %d",rep);
    }
}


int asmRepSizeW ( AsmRep rep )
{
   return repSizeW ( rep );
}


/* --------------------------------------------------------------------------
 * Instruction emission.  All instructions should be routed through here
 * so that the peephole optimiser gets to see what's happening.
 * ------------------------------------------------------------------------*/

static void emiti_ ( AsmBCO bco, Instr opcode )
{
   StgInt x, y;
   if (bco->lastOpc == i_SLIDE && opcode == i_ENTER) {
      /* SLIDE x y ; ENTER   ===>  SE x y */
      x = asmInstrBack(bco,2);
      y = asmInstrBack(bco,1); 
      asmInstrRecede(bco,3);
      asmInstrOp(bco,i_SE); asmInstr8(bco,x); asmInstr8(bco,y);
   }
   else
   if (bco->lastOpc == i_RV && opcode == i_ENTER) {
      /* RV x y ; ENTER ===> RVE x (y-2)
         Because RETADDR pushes 2 words on the stack, y must be at least 2. */
      x = asmInstrBack(bco,2);
      y = asmInstrBack(bco,1);
      if (y < 2) barf("emiti_: RVE: impossible y value");
      asmInstrRecede(bco,3);
      asmInstrOp(bco, i_RVE); asmInstr8(bco,x); asmInstr8(bco,y-2);
   }
   else {
      asmInstrOp(bco,opcode);
   }
}

static void emiti_8 ( AsmBCO bco, Instr opcode, int arg1 )
{
   StgInt x;
   if (bco->lastOpc == i_VAR && opcode == i_VAR) {
      /* VAR x ; VAR y ===>  VV x y */
      x = asmInstrBack(bco,1);
      asmInstrRecede(bco,2);
      asmInstrOp(bco,i_VV); asmInstr8(bco,x); asmInstr8(bco,arg1);
   } 
   else 
   if (bco->lastOpc == i_RETADDR && opcode == i_VAR) {
      /* RETADDR x ; VAR y ===> RV x y */
      x = asmInstrBack(bco,1);
      asmInstrRecede(bco,2);
      asmInstrOp(bco, i_RV); asmInstr8(bco,x); asmInstr8(bco,arg1);
   }
   else {
      asmInstrOp(bco,opcode);
      asmInstr8(bco,arg1);
   }
}

static void emiti_16 ( AsmBCO bco, Instr opcode, int arg1 )
{
   asmInstrOp(bco,opcode);
   asmInstr16(bco,arg1);
}

static void emiti_8_8 ( AsmBCO bco, Instr opcode, int arg1, int arg2 )
{
   asmInstrOp(bco,opcode);
   asmInstr8(bco,arg1);
   asmInstr8(bco,arg2);
}

static void emiti_8_16 ( AsmBCO bco, Instr opcode, int arg1, int arg2 )
{
   asmInstrOp(bco,opcode);
   asmInstr8(bco,arg1);
   asmInstr16(bco,arg2);
}

static void emiti_16_16 ( AsmBCO bco, Instr opcode, int arg1, int arg2 )
{
   asmInstrOp(bco,opcode);
   asmInstr16(bco,arg1);
   asmInstr16(bco,arg2);
}


/* --------------------------------------------------------------------------
 * Wrappers around the above fns
 * ------------------------------------------------------------------------*/

static void emit_i_VAR_INT ( AsmBCO bco, int arg1 )
{
   ASSERT(arg1 >= 0);
   if (arg1 < 256)
      emiti_8 (bco,i_VAR_INT,    arg1); else
      emiti_16(bco,i_VAR_INT_big,arg1);
}

static void emit_i_VAR_WORD ( AsmBCO bco, int arg1 )
{
   ASSERT(arg1 >= 0);
   if (arg1 < 256)
      emiti_8 (bco,i_VAR_WORD,    arg1); else
      emiti_16(bco,i_VAR_WORD_big,arg1);
}

static void emit_i_VAR_ADDR ( AsmBCO bco, int arg1 )
{
   ASSERT(arg1 >= 0);
   if (arg1 < 256)
      emiti_8 (bco,i_VAR_ADDR,    arg1); else
      emiti_16(bco,i_VAR_ADDR_big,arg1);
}

static void emit_i_VAR_CHAR ( AsmBCO bco, int arg1 )
{
   ASSERT(arg1 >= 0);
   if (arg1 < 256)
      emiti_8 (bco,i_VAR_CHAR,    arg1); else
      emiti_16(bco,i_VAR_CHAR_big,arg1);
}

static void emit_i_VAR_FLOAT ( AsmBCO bco, int arg1 )
{
   ASSERT(arg1 >= 0);
   if (arg1 < 256)
      emiti_8 (bco,i_VAR_FLOAT,    arg1); else
      emiti_16(bco,i_VAR_FLOAT_big,arg1);
}

static void emit_i_VAR_DOUBLE ( AsmBCO bco, int arg1 )
{
   ASSERT(arg1 >= 0);
   if (arg1 < 256)
      emiti_8 (bco,i_VAR_DOUBLE,    arg1); else
      emiti_16(bco,i_VAR_DOUBLE_big,arg1);
}

static void emit_i_VAR ( AsmBCO bco, int arg1 )
{
   ASSERT(arg1 >= 0);
   if (arg1 < 256)
      emiti_8 (bco,i_VAR,    arg1); else
      emiti_16(bco,i_VAR_big,arg1);
}

static void emit_i_PACK ( AsmBCO bco, int arg1 )
{
   ASSERT(arg1 >= 0);
   if (arg1 < 256)
      emiti_8 (bco,i_PACK,    arg1); else
      emiti_16(bco,i_PACK_big,arg1);
}

static void emit_i_SLIDE ( AsmBCO bco, int arg1, int arg2 )
{
   ASSERT(arg1 >= 0);
   ASSERT(arg2 >= 0);
   if (arg1 < 256 && arg2 < 256)
      emiti_8_8  (bco,i_SLIDE,    arg1,arg2); else
      emiti_16_16(bco,i_SLIDE_big,arg1,arg2);
}

static void emit_i_MKAP ( AsmBCO bco, int arg1, int arg2 )
{
   ASSERT(arg1 >= 0);
   ASSERT(arg2 >= 0);
   if (arg1 < 256 && arg2 < 256)
      emiti_8_8  (bco,i_MKAP,    arg1,arg2); else
      emiti_16_16(bco,i_MKAP_big,arg1,arg2);
}


static void emit_i_CONST_INT ( AsmBCO bco, int arg1 )
{
   ASSERT(arg1 >= 0);
   if (arg1 < 256)
      emiti_8 (bco,i_CONST_INT,    arg1); else
      emiti_16(bco,i_CONST_INT_big,arg1);
}

static void emit_i_CONST_INTEGER ( AsmBCO bco, int arg1 )
{
   ASSERT(arg1 >= 0);
   if (arg1 < 256)
      emiti_8 (bco,i_CONST_INTEGER,    arg1); else
      emiti_16(bco,i_CONST_INTEGER_big,arg1);
}

static void emit_i_CONST_ADDR ( AsmBCO bco, int arg1 )
{
   ASSERT(arg1 >= 0);
   if (arg1 < 256)
      emiti_8 (bco,i_CONST_ADDR,    arg1); else
      emiti_16(bco,i_CONST_ADDR_big,arg1);
}

static void emit_i_CONST_CHAR ( AsmBCO bco, int arg1 )
{
   ASSERT(arg1 >= 0);
   if (arg1 < 256)
      emiti_8 (bco,i_CONST_CHAR,    arg1); else
      emiti_16(bco,i_CONST_CHAR_big,arg1);
}

static void emit_i_CONST_FLOAT ( AsmBCO bco, int arg1 )
{
   ASSERT(arg1 >= 0);
   if (arg1 < 256)
      emiti_8 (bco,i_CONST_FLOAT,    arg1); else
      emiti_16(bco,i_CONST_FLOAT_big,arg1);
}

static void emit_i_CONST_DOUBLE ( AsmBCO bco, int arg1 )
{
   ASSERT(arg1 >= 0);
   if (arg1 < 256)
      emiti_8 (bco,i_CONST_DOUBLE,    arg1); else
      emiti_16(bco,i_CONST_DOUBLE_big,arg1);
}

static void emit_i_CONST ( AsmBCO bco, int arg1 )
{
   ASSERT(arg1 >= 0);
   if (arg1 < 256)
      emiti_8 (bco,i_CONST,    arg1); else
      emiti_16(bco,i_CONST_big,arg1);
}

static void emit_i_RETADDR ( AsmBCO bco, int arg1 )
{
   ASSERT(arg1 >= 0);
   if (arg1 < 256)
      emiti_8 (bco,i_RETADDR,    arg1); else
      emiti_16(bco,i_RETADDR_big,arg1);
}


/* --------------------------------------------------------------------------
 * Arg checks.
 * ------------------------------------------------------------------------*/

AsmSp  asmBeginArgCheck ( AsmBCO bco )
{
    ASSERT(bco->sp == 0);
    return bco->sp;
}

void   asmEndArgCheck   ( AsmBCO bco, AsmSp last_arg )
{
    nat args = bco->sp - last_arg;
    if (args != 0) { /* optimisation */
        emiti_8(bco,i_ARG_CHECK,args);
        grabHpNonUpd(bco,PAP_sizeW(args-1));
        resetHp(bco,0);
    }
}

/* --------------------------------------------------------------------------
 * Creating and using "variables"
 * ------------------------------------------------------------------------*/

AsmVar asmBind          ( AsmBCO bco, AsmRep rep )
{
    bco->sp += repSizeW(rep);
    return bco->sp;
}

void   asmVar           ( AsmBCO bco, AsmVar v, AsmRep rep )
{
    int offset;

    if (rep == VOID_REP) {
        emiti_(bco,i_VOID);
        bco->sp += repSizeW(rep);
        return;
    }

    offset = bco->sp - v;
    switch (rep) {
    case BOOL_REP:
    case INT_REP:
            emit_i_VAR_INT(bco,offset);
            break;
    case WORD_REP:
            emit_i_VAR_WORD(bco,offset);
            break;
    case ADDR_REP:
            emit_i_VAR_ADDR(bco,offset);
            break;
    case CHAR_REP:
            emit_i_VAR_CHAR(bco,offset);
            break;
    case FLOAT_REP:
            emit_i_VAR_FLOAT(bco,offset);
            break;
    case DOUBLE_REP:
            emit_i_VAR_DOUBLE(bco,offset);
            break;
#ifdef PROVIDE_STABLE
    case STABLE_REP:
            emit_i_VAR_STABLE(bco,offset);
            break;
#endif

    case INTEGER_REP:
#ifdef PROVIDE_WEAK
    case WEAK_REP: 
#endif
#ifdef PROVIDE_FOREIGN
    case FOREIGN_REP:
#endif
    case ALPHA_REP:    /* a                        */ 
    case BETA_REP:     /* b			   */
    case GAMMA_REP:    /* c			   */ 
    case HANDLER_REP:  /* IOError -> IO a	   */
    case ERROR_REP:    /* IOError		   */
    case ARR_REP    :  /* PrimArray              a */
    case BARR_REP   :  /* PrimByteArray          a */
    case REF_REP    :  /* Ref                  s a */
    case MUTARR_REP :  /* PrimMutableArray     s a */
    case MUTBARR_REP:  /* PrimMutableByteArray s a */
#ifdef PROVIDE_CONCURRENT
    case THREADID_REP: /* ThreadId	           */
    case MVAR_REP:     /* MVar a	           */
#endif
    case PTR_REP:
            emit_i_VAR(bco,offset);
            break;
    default:
            barf("asmVar %d",rep);
    }
    bco->sp += repSizeW(rep);
}

/* --------------------------------------------------------------------------
 * Tail calls
 * ------------------------------------------------------------------------*/

AsmSp asmBeginEnter( AsmBCO bco )
{
    return bco->sp;
}

void asmEndEnter( AsmBCO bco, AsmSp sp1, AsmSp sp2 )
{
    int x = bco->sp - sp1;
    int y = sp1 - sp2;
    ASSERT(x >= 0 && y >= 0);
    if (y != 0) {
        emit_i_SLIDE(bco,x,y);
        bco->sp -= sp1 - sp2;
    }
    emiti_(bco,i_ENTER);
}

/* --------------------------------------------------------------------------
 * Build boxed Ints, Floats, etc
 * ------------------------------------------------------------------------*/

AsmVar asmBox( AsmBCO bco, AsmRep rep )
{
    switch (rep) {
    case CHAR_REP:
            emiti_(bco,i_PACK_CHAR);
            grabHpNonUpd(bco,Czh_sizeW);
            break;
    case INT_REP:
            emiti_(bco,i_PACK_INT);
            grabHpNonUpd(bco,Izh_sizeW);
            break;
    case WORD_REP:
            emiti_(bco,i_PACK_WORD);
            grabHpNonUpd(bco,Wzh_sizeW);
            break;
    case ADDR_REP:
            emiti_(bco,i_PACK_ADDR);
            grabHpNonUpd(bco,Azh_sizeW);
            break;
    case FLOAT_REP:
            emiti_(bco,i_PACK_FLOAT);
            grabHpNonUpd(bco,Fzh_sizeW);
            break;
    case DOUBLE_REP:
            emiti_(bco,i_PACK_DOUBLE);
            grabHpNonUpd(bco,Dzh_sizeW);
            break;
#ifdef PROVIDE_STABLE
    case STABLE_REP:
            emiti_(bco,i_PACK_STABLE);
            grabHpNonUpd(bco,Stablezh_sizeW);
            break;
#endif

    default:
            barf("asmBox %d",rep);
    }
    /* NB: these operations DO pop their arg       */
    bco->sp -= repSizeW(rep);   /* pop unboxed arg */
    bco->sp += sizeofW(StgPtr); /* push box        */
    return bco->sp;
}

/* --------------------------------------------------------------------------
 * Unbox Ints, Floats, etc
 * ------------------------------------------------------------------------*/

AsmVar asmUnbox( AsmBCO bco, AsmRep rep )
{
    switch (rep) {
    case INT_REP:
            emiti_(bco,i_UNPACK_INT);
            break;
    case WORD_REP:
            emiti_(bco,i_UNPACK_WORD);
            break;
    case ADDR_REP:
            emiti_(bco,i_UNPACK_ADDR);
            break;
    case CHAR_REP:
            emiti_(bco,i_UNPACK_CHAR);
            break;
    case FLOAT_REP:
            emiti_(bco,i_UNPACK_FLOAT);
            break;
    case DOUBLE_REP:
            emiti_(bco,i_UNPACK_DOUBLE);
            break;
#ifdef PROVIDE_STABLE
    case STABLE_REP:
            emiti_(bco,i_UNPACK_STABLE);
            break;
#endif
    default:
            barf("asmUnbox %d",rep);
    }
    /* NB: these operations DO NOT pop their arg  */
    bco->sp += repSizeW(rep); /* push unboxed arg */
    return bco->sp;
}


/* --------------------------------------------------------------------------
 * Push unboxed Ints, Floats, etc
 * ------------------------------------------------------------------------*/

void asmConstInt( AsmBCO bco, AsmInt x )
{
    emit_i_CONST_INT(bco,bco->nps.len);
    asmWords(bco,AsmInt,x);
    bco->sp += repSizeW(INT_REP);
}

void asmConstInteger( AsmBCO bco, AsmString x )
{
    emit_i_CONST_INTEGER(bco,bco->nps.len);
    asmWords(bco,AsmString,x);
    bco->sp += repSizeW(INTEGER_REP);
}

void asmConstAddr( AsmBCO bco, AsmAddr x )
{
    emit_i_CONST_ADDR(bco,bco->nps.len);
    asmWords(bco,AsmAddr,x);
    bco->sp += repSizeW(ADDR_REP);
}

void asmConstWord( AsmBCO bco, AsmWord x )
{
    emit_i_CONST_INT(bco,bco->nps.len);
    asmWords(bco,AsmWord,(AsmInt)x);
    bco->sp += repSizeW(WORD_REP);
}

void asmConstChar( AsmBCO bco, AsmChar x )
{
    emit_i_CONST_CHAR(bco,bco->nps.len);
    asmWords(bco,AsmChar,x);
    bco->sp += repSizeW(CHAR_REP);
}

void asmConstFloat( AsmBCO bco, AsmFloat x )
{
    emit_i_CONST_FLOAT(bco,bco->nps.len);
    asmWords(bco,AsmFloat,x);
    bco->sp += repSizeW(FLOAT_REP);
}

void asmConstDouble( AsmBCO bco, AsmDouble x )
{
    emit_i_CONST_DOUBLE(bco,bco->nps.len);
    asmWords(bco,AsmDouble,x);
    bco->sp += repSizeW(DOUBLE_REP);
}

/* --------------------------------------------------------------------------
 * Algebraic case helpers
 * ------------------------------------------------------------------------*/

/* a mildly bogus pair of functions... */
AsmSp asmBeginCase( AsmBCO bco )
{
    return bco->sp;
}

void asmEndCase( AsmBCO bco )
{
}

AsmSp asmContinuation( AsmBCO bco, AsmBCO ret_addr )
{
    emit_i_RETADDR(bco,bco->object.ptrs.len);
    asmPtr(bco,&(ret_addr->object));
    bco->sp += 2 * sizeofW(StgPtr);
    return bco->sp;
}

AsmBCO asmBeginContinuation ( AsmSp sp, int /*List*/ alts )
{
    AsmBCO bco = asmBeginBCO(alts);
    bco->sp = sp;
    return bco;
}

void asmEndContinuation ( AsmBCO bco )
{
    asmEndBCO(bco);
}


/* --------------------------------------------------------------------------
 * Branches
 * ------------------------------------------------------------------------*/

AsmSp asmBeginAlt( AsmBCO bco )
{
    return bco->sp;
}

void asmEndAlt( AsmBCO bco, AsmSp  sp )
{
    resetSp(bco,sp);
}

AsmPc asmTest( AsmBCO bco, AsmWord tag )
{
    emiti_8_16(bco,i_TEST,tag,0);
    return bco->is.len;
}

AsmPc asmTestInt( AsmBCO bco, AsmVar v, AsmInt x )
{
    asmVar(bco,v,INT_REP);
    asmConstInt(bco,x);
    emiti_16(bco,i_TEST_INT,0);
    bco->sp -= 2*repSizeW(INT_REP);
    return bco->is.len;
}

void asmFixBranch( AsmBCO bco, AsmPc from )
{
    int distance = bco->is.len - from;
    ASSERT(distance >= 0);
    ASSERT(distance < 65536);
    setInstrs(&(bco->is),from-2,distance/256);
    setInstrs(&(bco->is),from-1,distance%256);
}

void asmPanic( AsmBCO bco )
{
    emiti_(bco,i_PANIC); /* "irrefutable" pattern failed - oops! */
}

/* --------------------------------------------------------------------------
 * Primops
 * ------------------------------------------------------------------------*/

AsmSp asmBeginPrim( AsmBCO bco )
{
    return bco->sp;
}

void   asmEndPrim( AsmBCO bco, const AsmPrim* prim, AsmSp base )
{
    emiti_8(bco,prim->prefix,prim->opcode);
    bco->sp = base;
}

/* Hugs used to let you add arbitrary primops with arbitrary types
 * just by editing Prelude.hs or any other file you wanted.
 * We deliberately avoided that approach because we wanted more
 * control over which primops are provided.
 */
const AsmPrim asmPrimOps[] = {

    /* Char# operations */
      { "primGtChar",                "CC", "B",  MONAD_Id, i_PRIMOP1, i_gtChar }
    , { "primGeChar",                "CC", "B",  MONAD_Id, i_PRIMOP1, i_geChar }
    , { "primEqChar",                "CC", "B",  MONAD_Id, i_PRIMOP1, i_eqChar }
    , { "primNeChar",                "CC", "B",  MONAD_Id, i_PRIMOP1, i_neChar }
    , { "primLtChar",                "CC", "B",  MONAD_Id, i_PRIMOP1, i_ltChar }
    , { "primLeChar",                "CC", "B",  MONAD_Id, i_PRIMOP1, i_leChar }
    , { "primCharToInt",             "C",  "I",  MONAD_Id, i_PRIMOP1, i_charToInt }
    , { "primIntToChar",             "I",  "C",  MONAD_Id, i_PRIMOP1, i_intToChar }

    /* Int# operations */
    , { "primGtInt",                 "II", "B",  MONAD_Id, i_PRIMOP1, i_gtInt }
    , { "primGeInt",                 "II", "B",  MONAD_Id, i_PRIMOP1, i_geInt }
    , { "primEqInt",                 "II", "B",  MONAD_Id, i_PRIMOP1, i_eqInt }
    , { "primNeInt",                 "II", "B",  MONAD_Id, i_PRIMOP1, i_neInt }
    , { "primLtInt",                 "II", "B",  MONAD_Id, i_PRIMOP1, i_ltInt }
    , { "primLeInt",                 "II", "B",  MONAD_Id, i_PRIMOP1, i_leInt }
    , { "primMinInt",                "",   "I",  MONAD_Id, i_PRIMOP1, i_minInt }
    , { "primMaxInt",                "",   "I",  MONAD_Id, i_PRIMOP1, i_maxInt }
    , { "primPlusInt",               "II", "I",  MONAD_Id, i_PRIMOP1, i_plusInt }
    , { "primMinusInt",              "II", "I",  MONAD_Id, i_PRIMOP1, i_minusInt }
    , { "primTimesInt",              "II", "I",  MONAD_Id, i_PRIMOP1, i_timesInt }
    , { "primQuotInt",               "II", "I",  MONAD_Id, i_PRIMOP1, i_quotInt }
    , { "primRemInt",                "II", "I",  MONAD_Id, i_PRIMOP1, i_remInt }
    , { "primQuotRemInt",            "II", "II", MONAD_Id, i_PRIMOP1, i_quotRemInt }
    , { "primNegateInt",             "I",  "I",  MONAD_Id, i_PRIMOP1, i_negateInt }

    , { "primAndInt",                "II", "I",  MONAD_Id, i_PRIMOP1, i_andInt }
    , { "primOrInt",                 "II", "I",  MONAD_Id, i_PRIMOP1, i_orInt }
    , { "primXorInt",                "II", "I",  MONAD_Id, i_PRIMOP1, i_xorInt }
    , { "primNotInt",                "I",  "I",  MONAD_Id, i_PRIMOP1, i_notInt }
    , { "primShiftLInt",             "II", "I",  MONAD_Id, i_PRIMOP1, i_shiftLInt }
    , { "primShiftRAInt",            "II", "I",  MONAD_Id, i_PRIMOP1, i_shiftRAInt }
    , { "primShiftRLInt",            "II", "I",  MONAD_Id, i_PRIMOP1, i_shiftRLInt }

    /* Word# operations */
    , { "primGtWord",                "WW", "B",  MONAD_Id, i_PRIMOP1, i_gtWord }
    , { "primGeWord",                "WW", "B",  MONAD_Id, i_PRIMOP1, i_geWord }
    , { "primEqWord",                "WW", "B",  MONAD_Id, i_PRIMOP1, i_eqWord }
    , { "primNeWord",                "WW", "B",  MONAD_Id, i_PRIMOP1, i_neWord }
    , { "primLtWord",                "WW", "B",  MONAD_Id, i_PRIMOP1, i_ltWord }
    , { "primLeWord",                "WW", "B",  MONAD_Id, i_PRIMOP1, i_leWord }
    , { "primMinWord",               "",   "W",  MONAD_Id, i_PRIMOP1, i_minWord }
    , { "primMaxWord",               "",   "W",  MONAD_Id, i_PRIMOP1, i_maxWord }
    , { "primPlusWord",              "WW", "W",  MONAD_Id, i_PRIMOP1, i_plusWord }
    , { "primMinusWord",             "WW", "W",  MONAD_Id, i_PRIMOP1, i_minusWord }
    , { "primTimesWord",             "WW", "W",  MONAD_Id, i_PRIMOP1, i_timesWord }
    , { "primQuotWord",              "WW", "W",  MONAD_Id, i_PRIMOP1, i_quotWord }
    , { "primRemWord",               "WW", "W",  MONAD_Id, i_PRIMOP1, i_remWord }
    , { "primQuotRemWord",           "WW", "WW", MONAD_Id, i_PRIMOP1, i_quotRemWord }
    , { "primNegateWord",            "W",  "W",  MONAD_Id, i_PRIMOP1, i_negateWord }

    , { "primAndWord",               "WW", "W",  MONAD_Id, i_PRIMOP1, i_andWord }
    , { "primOrWord",                "WW", "W",  MONAD_Id, i_PRIMOP1, i_orWord }
    , { "primXorWord",               "WW", "W",  MONAD_Id, i_PRIMOP1, i_xorWord }
    , { "primNotWord",               "W",  "W",  MONAD_Id, i_PRIMOP1, i_notWord }
    , { "primShiftLWord",            "WW", "W",  MONAD_Id, i_PRIMOP1, i_shiftLWord }
    , { "primShiftRAWord",           "WW", "W",  MONAD_Id, i_PRIMOP1, i_shiftRAWord }
    , { "primShiftRLWord",           "WW", "W",  MONAD_Id, i_PRIMOP1, i_shiftRLWord }

    , { "primIntToWord",             "I",  "W",  MONAD_Id, i_PRIMOP1, i_intToWord }
    , { "primWordToInt",             "W",  "I",  MONAD_Id, i_PRIMOP1, i_wordToInt }

    /* Addr# operations */
    , { "primGtAddr",                "AA", "B",  MONAD_Id, i_PRIMOP1, i_gtAddr }
    , { "primGeAddr",                "AA", "B",  MONAD_Id, i_PRIMOP1, i_geAddr }
    , { "primEqAddr",                "AA", "B",  MONAD_Id, i_PRIMOP1, i_eqAddr }
    , { "primNeAddr",                "AA", "B",  MONAD_Id, i_PRIMOP1, i_neAddr }
    , { "primLtAddr",                "AA", "B",  MONAD_Id, i_PRIMOP1, i_ltAddr }
    , { "primLeAddr",                "AA", "B",  MONAD_Id, i_PRIMOP1, i_leAddr }
    , { "primIntToAddr",             "I",  "A",  MONAD_Id, i_PRIMOP1, i_intToAddr }
    , { "primAddrToInt",             "A",  "I",  MONAD_Id, i_PRIMOP1, i_addrToInt }

    , { "primIndexCharOffAddr",      "AI", "C",  MONAD_Id, i_PRIMOP1, i_indexCharOffAddr }
    , { "primIndexIntOffAddr",       "AI", "I",  MONAD_Id, i_PRIMOP1, i_indexIntOffAddr }
    , { "primIndexWordOffAddr",      "AI", "W",  MONAD_Id, i_PRIMOP1, i_indexWordOffAddr }
    , { "primIndexAddrOffAddr",      "AI", "A",  MONAD_Id, i_PRIMOP1, i_indexAddrOffAddr }
    , { "primIndexFloatOffAddr",     "AI", "F",  MONAD_Id, i_PRIMOP1, i_indexFloatOffAddr }
    , { "primIndexDoubleOffAddr",    "AI", "D",  MONAD_Id, i_PRIMOP1, i_indexDoubleOffAddr }
#ifdef PROVIDE_STABLE
    , { "primIndexStableOffAddr",    "AI", "s",  MONAD_Id, i_PRIMOP1, i_indexStableOffAddr }
#endif

    /* These ops really ought to be in the IO monad */
    , { "primReadCharOffAddr",       "AI", "C",  MONAD_ST, i_PRIMOP1, i_readCharOffAddr }
    , { "primReadIntOffAddr",        "AI", "I",  MONAD_ST, i_PRIMOP1, i_readIntOffAddr }
    , { "primReadWordOffAddr",       "AI", "W",  MONAD_ST, i_PRIMOP1, i_readWordOffAddr }
    , { "primReadAddrOffAddr",       "AI", "A",  MONAD_ST, i_PRIMOP1, i_readAddrOffAddr }
    , { "primReadFloatOffAddr",      "AI", "F",  MONAD_ST, i_PRIMOP1, i_readFloatOffAddr }
    , { "primReadDoubleOffAddr",     "AI", "D",  MONAD_ST, i_PRIMOP1, i_readDoubleOffAddr }
#ifdef PROVIDE_STABLE                
    , { "primReadStableOffAddr",     "AI", "s",  MONAD_ST, i_PRIMOP1, i_readStableOffAddr }
#endif

    /* These ops really ought to be in the IO monad */
    , { "primWriteCharOffAddr",      "AIC", "",  MONAD_ST, i_PRIMOP1, i_writeCharOffAddr }
    , { "primWriteIntOffAddr",       "AII", "",  MONAD_ST, i_PRIMOP1, i_writeIntOffAddr }
    , { "primWriteWordOffAddr",      "AIW", "",  MONAD_ST, i_PRIMOP1, i_writeWordOffAddr }
    , { "primWriteAddrOffAddr",      "AIA", "",  MONAD_ST, i_PRIMOP1, i_writeAddrOffAddr }
    , { "primWriteFloatOffAddr",     "AIF", "",  MONAD_ST, i_PRIMOP1, i_writeFloatOffAddr }
    , { "primWriteDoubleOffAddr",    "AID", "",  MONAD_ST, i_PRIMOP1, i_writeDoubleOffAddr }
#ifdef PROVIDE_STABLE
    , { "primWriteStableOffAddr",    "AIs", "",  MONAD_ST, i_PRIMOP1, i_writeStableOffAddr }
#endif

    /* Integer operations */
    , { "primCompareInteger",        "ZZ", "I",  MONAD_Id, i_PRIMOP1, i_compareInteger }
    , { "primNegateInteger",         "Z",  "Z",  MONAD_Id, i_PRIMOP1, i_negateInteger }
    , { "primPlusInteger",           "ZZ", "Z",  MONAD_Id, i_PRIMOP1, i_plusInteger }
    , { "primMinusInteger",          "ZZ", "Z",  MONAD_Id, i_PRIMOP1, i_minusInteger }
    , { "primTimesInteger",          "ZZ", "Z",  MONAD_Id, i_PRIMOP1, i_timesInteger }
    , { "primQuotRemInteger",        "ZZ", "ZZ", MONAD_Id, i_PRIMOP1, i_quotRemInteger }
    , { "primDivModInteger",         "ZZ", "ZZ", MONAD_Id, i_PRIMOP1, i_divModInteger }
    , { "primIntegerToInt",          "Z",  "I",  MONAD_Id, i_PRIMOP1, i_integerToInt }
    , { "primIntToInteger",          "I",  "Z",  MONAD_Id, i_PRIMOP1, i_intToInteger }
    , { "primIntegerToWord",         "Z",  "W",  MONAD_Id, i_PRIMOP1, i_integerToWord }
    , { "primWordToInteger",         "W",  "Z",  MONAD_Id, i_PRIMOP1, i_wordToInteger }
    , { "primIntegerToFloat",        "Z",  "F",  MONAD_Id, i_PRIMOP1, i_integerToFloat }
    , { "primFloatToInteger",        "F",  "Z",  MONAD_Id, i_PRIMOP1, i_floatToInteger }
    , { "primIntegerToDouble",       "Z",  "D",  MONAD_Id, i_PRIMOP1, i_integerToDouble }
    , { "primDoubleToInteger",       "D",  "Z",  MONAD_Id, i_PRIMOP1, i_doubleToInteger }

    /* Float# operations */
    , { "primGtFloat",               "FF", "B",  MONAD_Id, i_PRIMOP1, i_gtFloat }
    , { "primGeFloat",               "FF", "B",  MONAD_Id, i_PRIMOP1, i_geFloat }
    , { "primEqFloat",               "FF", "B",  MONAD_Id, i_PRIMOP1, i_eqFloat }
    , { "primNeFloat",               "FF", "B",  MONAD_Id, i_PRIMOP1, i_neFloat }
    , { "primLtFloat",               "FF", "B",  MONAD_Id, i_PRIMOP1, i_ltFloat }
    , { "primLeFloat",               "FF", "B",  MONAD_Id, i_PRIMOP1, i_leFloat }
    , { "primMinFloat",              "",   "F",  MONAD_Id, i_PRIMOP1, i_minFloat }
    , { "primMaxFloat",              "",   "F",  MONAD_Id, i_PRIMOP1, i_maxFloat }
    , { "primRadixFloat",            "",   "I",  MONAD_Id, i_PRIMOP1, i_radixFloat }
    , { "primDigitsFloat",           "",   "I",  MONAD_Id, i_PRIMOP1, i_digitsFloat }
    , { "primMinExpFloat",           "",   "I",  MONAD_Id, i_PRIMOP1, i_minExpFloat }
    , { "primMaxExpFloat",           "",   "I",  MONAD_Id, i_PRIMOP1, i_maxExpFloat }
    , { "primPlusFloat",             "FF", "F",  MONAD_Id, i_PRIMOP1, i_plusFloat }
    , { "primMinusFloat",            "FF", "F",  MONAD_Id, i_PRIMOP1, i_minusFloat }
    , { "primTimesFloat",            "FF", "F",  MONAD_Id, i_PRIMOP1, i_timesFloat }
    , { "primDivideFloat",           "FF", "F",  MONAD_Id, i_PRIMOP1, i_divideFloat }
    , { "primNegateFloat",           "F",  "F",  MONAD_Id, i_PRIMOP1, i_negateFloat }
    , { "primFloatToInt",            "F",  "I",  MONAD_Id, i_PRIMOP1, i_floatToInt }
    , { "primIntToFloat",            "I",  "F",  MONAD_Id, i_PRIMOP1, i_intToFloat }
    , { "primExpFloat",              "F",  "F",  MONAD_Id, i_PRIMOP1, i_expFloat }
    , { "primLogFloat",              "F",  "F",  MONAD_Id, i_PRIMOP1, i_logFloat }
    , { "primSqrtFloat",             "F",  "F",  MONAD_Id, i_PRIMOP1, i_sqrtFloat }
    , { "primSinFloat",              "F",  "F",  MONAD_Id, i_PRIMOP1, i_sinFloat }
    , { "primCosFloat",              "F",  "F",  MONAD_Id, i_PRIMOP1, i_cosFloat }
    , { "primTanFloat",              "F",  "F",  MONAD_Id, i_PRIMOP1, i_tanFloat }
    , { "primAsinFloat",             "F",  "F",  MONAD_Id, i_PRIMOP1, i_asinFloat }
    , { "primAcosFloat",             "F",  "F",  MONAD_Id, i_PRIMOP1, i_acosFloat }
    , { "primAtanFloat",             "F",  "F",  MONAD_Id, i_PRIMOP1, i_atanFloat }
    , { "primSinhFloat",             "F",  "F",  MONAD_Id, i_PRIMOP1, i_sinhFloat }
    , { "primCoshFloat",             "F",  "F",  MONAD_Id, i_PRIMOP1, i_coshFloat }
    , { "primTanhFloat",             "F",  "F",  MONAD_Id, i_PRIMOP1, i_tanhFloat }
    , { "primPowerFloat",            "FF", "F",  MONAD_Id, i_PRIMOP1, i_powerFloat }
    , { "primDecodeFloatZ",          "F",  "ZI", MONAD_Id, i_PRIMOP1, i_decodeFloatZ }
    , { "primEncodeFloatZ",          "ZI", "F",  MONAD_Id, i_PRIMOP1, i_encodeFloatZ }
    , { "primIsNaNFloat",            "F",  "B",  MONAD_Id, i_PRIMOP1, i_isNaNFloat }
    , { "primIsInfiniteFloat",       "F",  "B",  MONAD_Id, i_PRIMOP1, i_isInfiniteFloat }
    , { "primIsDenormalizedFloat",   "F",  "B",  MONAD_Id, i_PRIMOP1, i_isDenormalizedFloat }
    , { "primIsNegativeZeroFloat",   "F",  "B",  MONAD_Id, i_PRIMOP1, i_isNegativeZeroFloat }
    , { "primIsIEEEFloat",           "",   "B",  MONAD_Id, i_PRIMOP1, i_isIEEEFloat }

    /* Double# operations */
    , { "primGtDouble",              "DD", "B",  MONAD_Id, i_PRIMOP1, i_gtDouble }
    , { "primGeDouble",              "DD", "B",  MONAD_Id, i_PRIMOP1, i_geDouble }
    , { "primEqDouble",              "DD", "B",  MONAD_Id, i_PRIMOP1, i_eqDouble }
    , { "primNeDouble",              "DD", "B",  MONAD_Id, i_PRIMOP1, i_neDouble }
    , { "primLtDouble",              "DD", "B",  MONAD_Id, i_PRIMOP1, i_ltDouble }
    , { "primLeDouble",              "DD", "B",  MONAD_Id, i_PRIMOP1, i_leDouble }
    , { "primMinDouble",             "",   "D",  MONAD_Id, i_PRIMOP1, i_minDouble }
    , { "primMaxDouble",             "",   "D",  MONAD_Id, i_PRIMOP1, i_maxDouble }
    , { "primRadixDouble",           "",   "I",  MONAD_Id, i_PRIMOP1, i_radixDouble }
    , { "primDigitsDouble",          "",   "I",  MONAD_Id, i_PRIMOP1, i_digitsDouble }
    , { "primMinExpDouble",          "",   "I",  MONAD_Id, i_PRIMOP1, i_minExpDouble }
    , { "primMaxExpDouble",          "",   "I",  MONAD_Id, i_PRIMOP1, i_maxExpDouble }
    , { "primPlusDouble",            "DD", "D",  MONAD_Id, i_PRIMOP1, i_plusDouble }
    , { "primMinusDouble",           "DD", "D",  MONAD_Id, i_PRIMOP1, i_minusDouble }
    , { "primTimesDouble",           "DD", "D",  MONAD_Id, i_PRIMOP1, i_timesDouble }
    , { "primDivideDouble",          "DD", "D",  MONAD_Id, i_PRIMOP1, i_divideDouble }
    , { "primNegateDouble",          "D",  "D",  MONAD_Id, i_PRIMOP1, i_negateDouble }
    , { "primDoubleToInt",           "D",  "I",  MONAD_Id, i_PRIMOP1, i_doubleToInt }
    , { "primIntToDouble",           "I",  "D",  MONAD_Id, i_PRIMOP1, i_intToDouble }
    , { "primDoubleToFloat",         "D",  "F",  MONAD_Id, i_PRIMOP1, i_doubleToFloat }
    , { "primFloatToDouble",         "F",  "D",  MONAD_Id, i_PRIMOP1, i_floatToDouble }
    , { "primExpDouble",             "D",  "D",  MONAD_Id, i_PRIMOP1, i_expDouble }
    , { "primLogDouble",             "D",  "D",  MONAD_Id, i_PRIMOP1, i_logDouble }
    , { "primSqrtDouble",            "D",  "D",  MONAD_Id, i_PRIMOP1, i_sqrtDouble }
    , { "primSinDouble",             "D",  "D",  MONAD_Id, i_PRIMOP1, i_sinDouble }
    , { "primCosDouble",             "D",  "D",  MONAD_Id, i_PRIMOP1, i_cosDouble }
    , { "primTanDouble",             "D",  "D",  MONAD_Id, i_PRIMOP1, i_tanDouble }
    , { "primAsinDouble",            "D",  "D",  MONAD_Id, i_PRIMOP1, i_asinDouble }
    , { "primAcosDouble",            "D",  "D",  MONAD_Id, i_PRIMOP1, i_acosDouble }
    , { "primAtanDouble",            "D",  "D",  MONAD_Id, i_PRIMOP1, i_atanDouble }
    , { "primSinhDouble",            "D",  "D",  MONAD_Id, i_PRIMOP1, i_sinhDouble }
    , { "primCoshDouble",            "D",  "D",  MONAD_Id, i_PRIMOP1, i_coshDouble }
    , { "primTanhDouble",            "D",  "D",  MONAD_Id, i_PRIMOP1, i_tanhDouble }
    , { "primPowerDouble",           "DD", "D",  MONAD_Id, i_PRIMOP1, i_powerDouble }
    , { "primDecodeDoubleZ",         "D",  "ZI", MONAD_Id, i_PRIMOP1, i_decodeDoubleZ }
    , { "primEncodeDoubleZ",         "ZI",  "D", MONAD_Id, i_PRIMOP1, i_encodeDoubleZ }
    , { "primIsNaNDouble",           "D",  "B",  MONAD_Id, i_PRIMOP1, i_isNaNDouble }
    , { "primIsInfiniteDouble",      "D",  "B",  MONAD_Id, i_PRIMOP1, i_isInfiniteDouble }
    , { "primIsDenormalizedDouble",  "D",  "B",  MONAD_Id, i_PRIMOP1, i_isDenormalizedDouble }
    , { "primIsNegativeZeroDouble",  "D",  "B",  MONAD_Id, i_PRIMOP1, i_isNegativeZeroDouble }
    , { "primIsIEEEDouble",          "",   "B",  MONAD_Id, i_PRIMOP1, i_isIEEEDouble }

    /* Ref operations */
    , { "primNewRef",                "a",  "R",  MONAD_ST, i_PRIMOP2, i_newRef }
    , { "primWriteRef",              "Ra", "",   MONAD_ST, i_PRIMOP2, i_writeRef }
    , { "primReadRef",               "R",  "a",  MONAD_ST, i_PRIMOP2, i_readRef }
    , { "primSameRef",               "RR", "B",  MONAD_Id, i_PRIMOP2, i_sameRef }

    /* PrimArray operations */
    , { "primSameMutableArray",      "MM",  "B", MONAD_Id, i_PRIMOP2, i_sameMutableArray }
    , { "primUnsafeFreezeArray",     "M",   "X", MONAD_ST, i_PRIMOP2, i_unsafeFreezeArray }
    , { "primNewArray",              "Ia",  "M", MONAD_ST, i_PRIMOP2, i_newArray }
    , { "primWriteArray",            "MIa", "",  MONAD_ST, i_PRIMOP2, i_writeArray }
    , { "primReadArray",             "MI",  "a", MONAD_ST, i_PRIMOP2, i_readArray }
    , { "primIndexArray",            "XI",  "a", MONAD_Id, i_PRIMOP2, i_indexArray }
    , { "primSizeArray",             "X",   "I", MONAD_Id, i_PRIMOP2, i_sizeArray }
    , { "primSizeMutableArray",      "M",   "I", MONAD_Id, i_PRIMOP2, i_sizeMutableArray }

    /* Prim[Mutable]ByteArray operations */
    , { "primSameMutableByteArray",  "mm", "B", MONAD_Id, i_PRIMOP2, i_sameMutableByteArray }
    , { "primUnsafeFreezeByteArray", "m",  "x", MONAD_ST, i_PRIMOP2, i_unsafeFreezeByteArray }
    
    , { "primNewByteArray",          "I",  "m", MONAD_ST, i_PRIMOP2, i_newByteArray }

    , { "primWriteCharArray",        "mIC", "", MONAD_ST, i_PRIMOP2, i_writeCharArray }
    , { "primReadCharArray",         "mI", "C", MONAD_ST, i_PRIMOP2, i_readCharArray }
    , { "primIndexCharArray",        "xI", "C", MONAD_Id, i_PRIMOP2, i_indexCharArray }
    
    , { "primWriteIntArray",         "mII", "",  MONAD_ST, i_PRIMOP2, i_writeIntArray }
    , { "primReadIntArray",          "mI",  "I", MONAD_ST, i_PRIMOP2, i_readIntArray }
    , { "primIndexIntArray",         "xI",  "I", MONAD_Id, i_PRIMOP2, i_indexIntArray }

    /* {new,write,read,index}IntegerArray not provided */

    , { "primWriteWordArray",        "mIW", "",  MONAD_ST, i_PRIMOP2, i_writeWordArray }
    , { "primReadWordArray",         "mI",  "W", MONAD_ST, i_PRIMOP2, i_readWordArray }
    , { "primIndexWordArray",        "xI",  "W", MONAD_Id, i_PRIMOP2, i_indexWordArray }
    , { "primWriteAddrArray",        "mIA", "",  MONAD_ST, i_PRIMOP2, i_writeAddrArray }
    , { "primReadAddrArray",         "mI",  "A", MONAD_ST, i_PRIMOP2, i_readAddrArray }
    , { "primIndexAddrArray",        "xI",  "A", MONAD_Id, i_PRIMOP2, i_indexAddrArray }
    , { "primWriteFloatArray",       "mIF", "",  MONAD_ST, i_PRIMOP2, i_writeFloatArray }
    , { "primReadFloatArray",        "mI",  "F", MONAD_ST, i_PRIMOP2, i_readFloatArray }
    , { "primIndexFloatArray",       "xI",  "F", MONAD_Id, i_PRIMOP2, i_indexFloatArray }
    , { "primWriteDoubleArray" ,     "mID", "",  MONAD_ST, i_PRIMOP2, i_writeDoubleArray }
    , { "primReadDoubleArray",       "mI",  "D", MONAD_ST, i_PRIMOP2, i_readDoubleArray }
    , { "primIndexDoubleArray",      "xI",  "D", MONAD_Id, i_PRIMOP2, i_indexDoubleArray }

#ifdef PROVIDE_STABLE                
    , { "primWriteStableArray",      "mIs", "",  MONAD_ST, i_PRIMOP2, i_writeStableArray }
    , { "primReadStableArray",       "mI",  "s", MONAD_ST, i_PRIMOP2, i_readStableArray }
    , { "primIndexStableArray",      "xI",  "s", MONAD_Id, i_PRIMOP2, i_indexStableArray }
#endif

    /* {new,write,read,index}ForeignObjArray not provided */


#ifdef PROVIDE_FOREIGN
    /* ForeignObj# operations */
    , { "primMakeForeignObj",        "A",  "f",  MONAD_IO, i_PRIMOP2, i_makeForeignObj }
#endif
#ifdef PROVIDE_WEAK
    /* WeakPair# operations */
    , { "primMakeWeak",              "bac", "w",  MONAD_IO, i_PRIMOP2, i_makeWeak }
    , { "primDeRefWeak",             "w",   "Ia", MONAD_IO, i_PRIMOP2, i_deRefWeak }
#endif
#ifdef PROVIDE_STABLE
    /* StablePtr# operations */
    , { "primMakeStablePtr",         "a", "s",   MONAD_IO, i_PRIMOP2, i_makeStablePtr }
    , { "primDeRefStablePtr",        "s", "a",   MONAD_IO, i_PRIMOP2, i_deRefStablePtr }
    , { "primFreeStablePtr",         "s", "",    MONAD_IO, i_PRIMOP2, i_freeStablePtr }
#endif
#ifdef PROVIDE_PTREQUALITY
    , { "primReallyUnsafePtrEquality", "aa", "B",MONAD_Id, i_PRIMOP2, i_reallyUnsafePtrEquality }
#endif
#ifdef PROVIDE_COERCE
    , { "primUnsafeCoerce",          "a", "b",   MONAD_Id, i_PRIMOP2, i_unsafeCoerce }
#endif
#ifdef PROVIDE_CONCURRENT
    /* Concurrency operations */
    , { "primFork",                  "a", "T",   MONAD_IO, i_PRIMOP2, i_fork }
    , { "primKillThread",            "T", "",    MONAD_IO, i_PRIMOP2, i_killThread }
    , { "primSameMVar",              "rr", "B",  MONAD_Id, i_PRIMOP2, i_sameMVar }
    , { "primNewMVar",               "",  "r",   MONAD_IO, i_PRIMOP2, i_newMVar }
    , { "primTakeMVar",              "r", "a",   MONAD_IO, i_PRIMOP2, i_takeMVar }
    , { "primPutMVar",               "ra", "",   MONAD_IO, i_PRIMOP2, i_putMVar } 
    , { "primDelay",                 "I", "",    MONAD_IO, i_PRIMOP2, i_delay }
    , { "primWaitRead",              "I", "",    MONAD_IO, i_PRIMOP2, i_waitRead }
    , { "primWaitWrite",             "I", "",    MONAD_IO, i_PRIMOP2, i_waitWrite }
#endif

    /* Ccall is polyadic - so it's excluded from this table */

    , { 0,0,0,0 }
};

const AsmPrim ccall_Id = { "ccall", 0, 0, MONAD_IO, i_PRIMOP2, i_ccall_Id };
const AsmPrim ccall_IO = { "ccall", 0, 0, MONAD_IO, i_PRIMOP2, i_ccall_IO };


const AsmPrim* asmFindPrim( char* s )
{
    int i;
    for (i=0; asmPrimOps[i].name; ++i) {
        if (strcmp(s,asmPrimOps[i].name)==0) {
            return &asmPrimOps[i];
        }
    }
    return 0;
}

const AsmPrim* asmFindPrimop( AsmInstr prefix, AsmInstr op )
{
    nat i;
    for (i=0; asmPrimOps[i].name; ++i) {
        if (asmPrimOps[i].prefix == prefix && asmPrimOps[i].opcode == op) {
            return &asmPrimOps[i];
        }
    }
    return 0;
}

/* --------------------------------------------------------------------------
 * Handwritten primops
 * ------------------------------------------------------------------------*/

AsmBCO asm_BCO_catch ( void )
{
   AsmBCO bco = asmBeginBCO(0 /*NIL*/);
   emiti_8(bco,i_ARG_CHECK,2);
   emiti_8(bco,i_PRIMOP1,i_pushcatchframe);
   bco->sp += (1-2)*sizeofW(StgPtr) + sizeofW(StgCatchFrame);
   emiti_(bco,i_ENTER);
   asmEndBCO(bco);
   return bco;
}

AsmBCO asm_BCO_raise ( void )
{
   AsmBCO bco = asmBeginBCO(0 /*NIL*/);
   emiti_8(bco,i_ARG_CHECK,1);
   emiti_8(bco,i_PRIMOP2,i_raise);
   asmEndBCO(bco);
   return bco;
}

AsmBCO asm_BCO_seq ( void )
{
   AsmBCO eval, cont;

   cont = asmBeginBCO(0 /*NIL*/);
   emiti_8(cont,i_ARG_CHECK,2);
   emit_i_VAR(cont,1);
   emit_i_SLIDE(cont,1,2);
   emiti_(cont,i_ENTER);
   cont->sp += 3*sizeofW(StgPtr);
   asmEndBCO(cont);

   eval = asmBeginBCO(0 /*NIL*/);
   emiti_8(eval,i_ARG_CHECK,2);
   emit_i_RETADDR(eval,eval->object.ptrs.len);
   asmPtr(eval,&(cont->object));
   emit_i_VAR(eval,2);
   emit_i_SLIDE(eval,3,1);
   emiti_8(eval,i_PRIMOP1,i_pushseqframe);
   emiti_(eval,i_ENTER);
   eval->sp += sizeofW(StgSeqFrame) + 4*sizeofW(StgPtr);
   asmEndBCO(eval);

   return eval;
}

/* --------------------------------------------------------------------------
 * Heap manipulation
 * ------------------------------------------------------------------------*/

AsmVar asmAllocCONSTR   ( AsmBCO bco, AsmInfo info )
{
    ASSERT( sizeW_fromITBL(info) >= MIN_NONUPD_SIZE + sizeofW(StgHeader) );
    emiti_8(bco,i_ALLOC_CONSTR,bco->nps.len);
    asmWords(bco,AsmInfo,info);
    bco->sp += sizeofW(StgClosurePtr);
    grabHpNonUpd(bco,sizeW_fromITBL(info));
    return bco->sp;
}

AsmSp asmBeginPack( AsmBCO bco )
{
    return bco->sp;
}

void asmEndPack( AsmBCO bco, AsmVar v, AsmSp start, AsmInfo info )
{
    nat size = bco->sp - start;
    assert(bco->sp >= start);
    assert(start >= v);
    /* only reason to include info is for this assertion */
    assert(info->layout.payload.ptrs == size);
    emit_i_PACK(bco, bco->sp - v);
    bco->sp = start;
}

void asmBeginUnpack( AsmBCO bco )
{
    /* dummy to make it look prettier */
}

void asmEndUnpack( AsmBCO bco )
{
    emiti_(bco,i_UNPACK);
}

AsmVar asmAllocAP( AsmBCO bco, AsmNat words )
{
    emiti_8(bco,i_ALLOC_AP,words);
    bco->sp += sizeofW(StgPtr);
    grabHpUpd(bco,AP_sizeW(words));
    return bco->sp;
}

AsmSp asmBeginMkAP( AsmBCO bco )
{
    return bco->sp;
}

void asmEndMkAP( AsmBCO bco, AsmVar v, AsmSp start )
{
    emit_i_MKAP(bco,bco->sp-v,bco->sp-start-1);
            /* -1 because fun isn't counted */
    bco->sp = start;
}

AsmVar asmAllocPAP( AsmBCO bco, AsmNat size )
{
    emiti_8(bco,i_ALLOC_PAP,size);
    bco->sp += sizeofW(StgPtr);
    return bco->sp;
}

AsmSp asmBeginMkPAP( AsmBCO bco )
{
    return bco->sp;
}

void asmEndMkPAP( AsmBCO bco, AsmVar v, AsmSp start )
{
    emiti_8_8(bco,i_MKPAP,bco->sp-v,bco->sp-start-1);
            /* -1 because fun isn't counted */
    bco->sp = start;
}

AsmVar asmClosure( AsmBCO bco, AsmObject p )
{
    emit_i_CONST(bco,bco->object.ptrs.len);
    asmPtr(bco,p);
    bco->sp += sizeofW(StgPtr);
    return bco->sp;
}

/* --------------------------------------------------------------------------
 * Building InfoTables
 * ------------------------------------------------------------------------*/

AsmInfo asmMkInfo( AsmNat tag, AsmNat ptrs )
{
    StgInfoTable* info = stgMallocBytes( sizeof(StgInfoTable),"asmMkInfo");
    /* Note: the evaluator automatically pads objects with the right number
     * of non-ptrs to satisfy MIN_NONUPD_SIZE restrictions.
     */
    AsmNat nptrs = stg_max(0,MIN_NONUPD_SIZE-ptrs);

    /* initialisation code based on INFO_TABLE_CONSTR */
    info->layout.payload.ptrs  = ptrs;
    info->layout.payload.nptrs = nptrs;
    info->srt_len = tag;
    info->type    = CONSTR;
#ifdef USE_MINIINTERPRETER
    info->entry   = stgCast(StgFunPtr,&Hugs_CONSTR_entry);
#else
#warning asmMkInfo: Need to insert entry code in some cunning way
#endif
    ASSERT( sizeW_fromITBL(info) >= MIN_NONUPD_SIZE + sizeofW(StgHeader) );
    return info;
}

/*-------------------------------------------------------------------------*/

#endif /* INTERPRETER */

