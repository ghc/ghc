
/* --------------------------------------------------------------------------
 * Bytecode assembler
 *
 * Copyright (c) 1994-1998.
 *
 * $RCSfile: Assembler.c,v $
 * $Revision: 1.30 $
 * $Date: 2000/05/10 16:53:35 $
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
#include "StgMiscClosures.h"
#include "Storage.h"
#include "Schedule.h"
#include "Evaluator.h"

#define INSIDE_ASSEMBLER_C
#include "Assembler.h"
#undef INSIDE_ASSEMBLER_C

static StgClosure* asmAlloc ( nat size );
extern void* getNameOrTupleClosureCPtr ( int /*Cell*/ c );


/* Defined in this file ... */
AsmObject    asmNewObject      ( void );
void         asmAddEntity      ( AsmObject, Asm_Kind, StgWord );
int          asmCalcHeapSizeW  ( AsmObject );
StgClosure*  asmDerefEntity    ( Asm_Entity );

/* --------------------------------------------------------------------------
 * Initialising and managing objects and entities
 * ------------------------------------------------------------------------*/

static struct AsmObject_* objects;

#define INITIALISE_TABLE(Type,table,size,used)                       \
   size = used = 0;                                                  \
   table = NULL;

#define ENSURE_SPACE_IN_TABLE(Type,table,size,used)                  \
   if (used == size) {                                               \
      Type* new;                                                     \
      size = (size ? 2*size : 1);                                    \
      new = malloc ( size * sizeof(Type));                           \
      if (!new)                                                      \
         barf("bytecode assembler: can't expand table of type "      \
              #Type);                                                \
      memcpy ( new, table, used * sizeof(Type) );                    \
      if (table) free(table);                                        \
      table = new;                                                   \
   }

void asmInitialise ( void )
{
   objects = NULL;
}


AsmObject asmNewObject ( void )
{
   AsmObject obj = malloc(sizeof(struct AsmObject_));
   if (!obj)
      barf("bytecode assembler: can't malloc in asmNewObject");
   obj->next    = objects;
   objects      = obj;
   obj->n_refs  = obj->n_words = obj->n_insns = 0;
   obj->closure = NULL;
   obj->stgexpr = 0; /*NIL*/
   obj->magic   = 0x31415927;
   INITIALISE_TABLE(AsmEntity,obj->entities,
                              obj->sizeEntities,
                              obj->usedEntities);
   return obj;
}


void asmAddEntity ( AsmObject   obj, 
                    Asm_Kind    kind,
                    StgWord     val )
{
   ENSURE_SPACE_IN_TABLE(
      Asm_Entity,obj->entities,
      obj->sizeEntities,obj->usedEntities);
   obj->entities[obj->usedEntities].kind = kind;
   obj->entities[obj->usedEntities].val  = val;
   obj->usedEntities++;
   switch (kind) {
      case Asm_RefNoOp: case Asm_RefObject: case Asm_RefHugs: 
         obj->n_refs++; break;
      case Asm_NonPtrWord: 
         obj->n_words++; break;
      case Asm_Insn8:
         obj->n_insns++; break;
      default:
         barf("asmAddEntity");
   }
}

/* Support for the peephole optimiser.  Find the instruction
   byte n back, carefully stepping over any non Asm_Insn8 entities
   on the way.
*/
static Instr asmInstrBack ( AsmBCO bco, StgInt n )
{
   StgInt ue = bco->usedEntities;
   while (1) {
      if (ue < 0 || n <= 0) barf("asmInstrBack");
      ue--;
      if (bco->entities[ue].kind != Asm_Insn8) continue;
      n--;
      if (n == 0) return bco->entities[ue].val;
   }
}


/* Throw away n Asm_Insn8 bytes, and slide backwards any Asm_Insn8 entities
   as necessary.
*/
static void asmInstrRecede ( AsmBCO bco, StgInt n )
{
   StgInt ue = bco->usedEntities;
   StgInt wr;
   while (1) {
      if (ue < 0 || n <= 0) barf("asmInstrRecede");
      ue--;
      if (bco->entities[ue].kind != Asm_Insn8) continue;
      n--;
      bco->n_insns--;
      if (n == 0) break;
   }
   /* Now ue is the place where we would recede usedEntities to,
      except that there may be stuff to slide downwards.
   */
   wr = ue;
   for (; ue < bco->usedEntities; ue++) {
      if (bco->entities[ue].kind != Asm_Insn8) {
         bco->entities[wr] = bco->entities[ue];
         wr++;
      }
   }
   bco->usedEntities = wr;
}


static int asmFindInNonPtrs ( AsmBCO bco, StgWord w )
{
   int i, j = 0;
   for (i = 0; i < bco->usedEntities; i++) {
      if (bco->entities[i].kind == Asm_NonPtrWord) {
         if (bco->entities[i].val == w) return j;
         j++;
      }
   }
   return -1;
}

static void setInstrs ( AsmBCO bco, int instr_no, StgWord new_instr_byte )
{
   int i, j = 0;
   for (i = 0; i < bco->usedEntities; i++) {
      if (bco->entities[i].kind == Asm_Insn8) {
         if (j == instr_no) {
            bco->entities[i].val = new_instr_byte;
            return;
         }
         j++;
      }
   }
   barf("setInstrs");
}

void* asmGetClosureOfObject ( AsmObject obj )
{
   return obj->closure;
}


/* --------------------------------------------------------------------------
 * Top level assembler/BCO linker functions
 * ------------------------------------------------------------------------*/

int asmCalcHeapSizeW ( AsmObject obj )
{
   int p, np, is, ws;
   switch (obj->kind) {
      case Asm_BCO:
         p  = obj->n_refs;
         np = obj->n_words;
         is = obj->n_insns + (obj->max_sp <= 255 ? 2 : 3);
         ws = BCO_sizeW ( p, np, is );
         break;
      case Asm_CAF:
         ws = CAF_sizeW();
         break;
      case Asm_Con:
         p  = obj->n_refs;
         np = obj->n_words;
         ws = CONSTR_sizeW ( p, np );
         break;
      default:
         barf("asmCalcHeapSizeW");
   }
   if (ws - sizeofW(StgHeader) < MIN_NONUPD_SIZE)
      ws = sizeofW(StgHeader) + MIN_NONUPD_SIZE;
   return ws;
}


void asmAllocateHeapSpace ( void )
{
   AsmObject obj;
   for (obj = objects; obj; obj = obj->next) {
      StgClosure* c = asmAlloc ( asmCalcHeapSizeW ( obj ) );
      obj->closure = c;
   }
}

void asmShutdown ( void ) 
{
   AsmObject obj;
   AsmObject next = NULL;
   for (obj = objects; obj; obj = next) {
      next = obj->next;
      obj->magic = 0x27180828;
      if ( /*paranoia*/ obj->entities)
         free(obj->entities);
      free(obj);
   }
   objects = NULL;
}

StgClosure* asmDerefEntity ( Asm_Entity entity )
{
   switch (entity.kind) {
      case Asm_RefNoOp:
         return (StgClosure*)entity.val;
      case Asm_RefObject:
         ASSERT(entity.val);
         ASSERT( ((AsmObject)(entity.val))->magic == 0x31415927 );
         return ((AsmObject)(entity.val))->closure;
      case Asm_RefHugs:
         return getNameOrTupleClosureCPtr(entity.val);
      default:
         barf("asmDerefEntity");
   }
   return NULL; /*notreached*/
}


void asmCopyAndLink ( void )
{
   int       j, k;
   AsmObject obj;

   for (obj = objects; obj; obj = obj->next) {
      StgClosure** p   = (StgClosure**)(obj->closure);
      ASSERT(p);

      switch (obj->kind) {

         case Asm_BCO: {
            AsmBCO  abco  = (AsmBCO)obj;
            StgBCO* bco   = (StgBCO*)p;
            SET_HDR(bco,&BCO_info,??);
            bco->n_ptrs   = abco->n_refs;
            bco->n_words  = abco->n_words;
            bco->n_instrs = abco->n_insns + (obj->max_sp <= 255 ? 2 : 3);
            bco->stgexpr  = abco->stgexpr;
	    //ppStgExpr(bco->stgexpr);
            /* First copy in the ptrs. */
            k = 0;
            for (j = 0; j < obj->usedEntities; j++) {
               switch (obj->entities[j].kind) {
               case Asm_RefNoOp: 
               case Asm_RefObject:
               case Asm_RefHugs:
                  bcoConstCPtr(bco,k++) 
                     = (StgClosure*)asmDerefEntity(obj->entities[j]); break;
               default: 
                  break;
               }
            }

            /* Now the non-ptrs. */
            k = 0;
            for (j = 0; j < obj->usedEntities; j++) {
               switch (obj->entities[j].kind) {
               case Asm_NonPtrWord: 
                  bcoConstWord(bco,k++) = obj->entities[j].val; break;
               default: 
                  break;
               }
            }

            /* Finally the insns, adding a stack check at the start. */
            k = 0;
            abco->max_sp = stg_max(abco->sp,abco->max_sp);

            ASSERT(abco->max_sp <= 65535);
            if (abco->max_sp <= 255) {
               bcoInstr(bco,k++) = i_STK_CHECK;
               bcoInstr(bco,k++) = abco->max_sp;
            } else {
               bcoInstr(bco,k++) = i_STK_CHECK_big;
               bcoInstr(bco,k++) = abco->max_sp / 256;
               bcoInstr(bco,k++) = abco->max_sp % 256;
            }
            for (j = 0; j < obj->usedEntities; j++) {
               switch (obj->entities[j].kind) {
               case Asm_Insn8:
                  bcoInstr(bco,k++) = obj->entities[j].val; break;
               case Asm_RefNoOp: 
               case Asm_RefObject:
               case Asm_RefHugs:
               case Asm_NonPtrWord:
                  break;
               default: 
                  barf("asmCopyAndLink: strange stuff in AsmBCO");
               }
            }

            ASSERT((unsigned int)k == bco->n_instrs);
            break;
         }

         case Asm_CAF: {
            StgCAF* caf = (StgCAF*)p;
            SET_HDR(caf,&CAF_UNENTERED_info,??); 
            caf->link     = NULL;
            caf->mut_link = NULL;
            caf->value    = (StgClosure*)0xdeadbeef;
            ASSERT(obj->usedEntities == 1);
            switch (obj->entities[0].kind) {
               case Asm_RefNoOp:
               case Asm_RefObject:
               case Asm_RefHugs:
                  caf->body = (StgClosure*)asmDerefEntity(obj->entities[0]);
                  break;
               default:
                  barf("asmCopyAndLink: strange stuff in AsmCAF");
            }
            p += CAF_sizeW();
            break;
         }

         case Asm_Con: {            
            SET_HDR((StgClosure*)p,obj->itbl,??);
            p++;
            /* First put in the pointers, then the non-pointers. */
            for (j = 0; j < obj->usedEntities; j++) {
               switch (obj->entities[j].kind) {
               case Asm_RefNoOp: 
               case Asm_RefObject:
               case Asm_RefHugs:
                  *p++ = asmDerefEntity(obj->entities[j]); break;
               default: 
                  break;
               }
            }
            for (j = 0; j < obj->usedEntities; j++) {
               switch (obj->entities[j].kind) {
               case Asm_NonPtrWord: 
                 *p++ = (StgClosure*)(obj->entities[j].val); break;
               default: 
                 barf("asmCopyAndLink: strange stuff in AsmCon");
               }
            }
            break;
         }

         default:
            barf("asmCopyAndLink");
      }
   }
}


/* --------------------------------------------------------------------------
 * Keeping track of the simulated stack pointer
 * ------------------------------------------------------------------------*/

static StgClosure* asmAlloc( nat size )
{
    StgClosure* o = stgCast(StgClosure*,allocate(size));
    ASSERT( size >= MIN_NONUPD_SIZE + sizeofW(StgHeader) );
    /* printf("Allocated %p .. %p\n", o, o+size-1); */
    return o;
}

static void setSp( AsmBCO bco, AsmSp sp )
{
    bco->max_sp = stg_max(bco->sp,bco->max_sp);
    bco->sp     = sp;
    bco->max_sp = stg_max(bco->sp,bco->max_sp);
}

static void incSp ( AsmBCO bco, int sp_delta )
{
    bco->max_sp  = stg_max(bco->sp,bco->max_sp);
    bco->sp     += sp_delta;
    bco->max_sp  = stg_max(bco->sp,bco->max_sp);
}

static void decSp ( AsmBCO bco, int sp_delta )
{
    bco->max_sp  = stg_max(bco->sp,bco->max_sp);
    bco->sp     -= sp_delta;
    bco->max_sp  = stg_max(bco->sp,bco->max_sp);
}

/* --------------------------------------------------------------------------
 * 
 * ------------------------------------------------------------------------*/

AsmCon asmBeginCon( AsmInfo info )
{
   AsmCon con = asmNewObject();
   con->kind = Asm_Con;
   con->itbl = info;
   return con;
}

void asmEndCon( AsmCon con __attribute__ ((unused)) )
{
}

AsmCAF asmBeginCAF( void )
{
   AsmCAF caf = asmNewObject();
   caf->kind = Asm_CAF;
   return caf;
}

void asmEndCAF( AsmCAF caf __attribute__ ((unused)) )
{
}

AsmBCO asmBeginBCO( int /*StgExpr*/ e )
{
   AsmBCO bco   = asmNewObject();
   bco->kind    = Asm_BCO;
   bco->stgexpr = e;
   //ppStgExpr(bco->stgexpr);
   bco->sp      = 0;
   bco->max_sp  = 0;
   bco->lastOpc = i_INTERNAL_ERROR;
   return bco;
}

void asmEndBCO( AsmBCO bco __attribute__ ((unused)) )
{
}

/* --------------------------------------------------------------------------
 * 
 * ------------------------------------------------------------------------*/

static void asmAddInstr ( AsmBCO bco, StgWord i )
{
   asmAddEntity ( bco, Asm_Insn8, i );
}

static void asmAddNonPtrWord ( AsmObject obj, StgWord i )
{
   asmAddEntity ( obj, Asm_NonPtrWord, i );
}

void asmAddRefHugs ( AsmObject obj,int /*Name*/ n )
{
   asmAddEntity ( obj, Asm_RefHugs, n );
}

void asmAddRefObject ( AsmObject obj, AsmObject p )
{
   ASSERT(p->magic == 0x31415927);
   asmAddEntity ( obj, Asm_RefObject, (StgWord)p );
}

void asmAddRefNoOp ( AsmObject obj, StgPtr p )
{
   asmAddEntity ( obj, Asm_RefNoOp, (StgWord)p );
}



static void asmInstrOp ( AsmBCO bco, StgWord i )
{
    ASSERT(i <= BIGGEST_OPCODE); /* must be a valid opcode */
    bco->lastOpc = i;
    asmAddInstr(bco,i);
}

static void asmInstr8 ( AsmBCO bco, StgWord i )
{
  if (i >= 256) {
    ASSERT(i < 256); /* must be a byte */
  }
    asmAddInstr(bco,i);
}

static void asmInstr16 ( AsmBCO bco, StgWord i )
{
    ASSERT(i < 65536); /* must be a short */
    asmAddInstr(bco,i / 256);
    asmAddInstr(bco,i % 256);
}


#define asmAddNonPtrWords(bco,ty,x)                      \
    {                                                    \
        union { ty a; AsmWord b[sizeofW(ty)]; } p;       \
        nat i;                                           \
        if (sizeof(ty) < sizeof(AsmWord)) p.b[0]=0;      \
        p.a = x;                                         \
        for( i = 0; i < sizeofW(ty); i++ ) {             \
            asmAddNonPtrWord(bco,p.b[i]);                \
        }                                                \
    }

static StgWord repSizeW( AsmRep rep )
{
    switch (rep) {
    case CHAR_REP:    return sizeofW(StgWord) + sizeofW(StgChar);

    case BOOL_REP:
    case INT_REP:      return sizeofW(StgWord) + sizeofW(StgInt);
    case THREADID_REP:
    case WORD_REP:     return sizeofW(StgWord) + sizeofW(StgWord);
    case ADDR_REP:     return sizeofW(StgWord) + sizeofW(StgAddr);
    case FLOAT_REP:    return sizeofW(StgWord) + sizeofW(StgFloat);
    case DOUBLE_REP:   return sizeofW(StgWord) + sizeofW(StgDouble);
    case STABLE_REP:   return sizeofW(StgWord) + sizeofW(StgWord);

    case INTEGER_REP: 
#ifdef PROVIDE_WEAK
    case WEAK_REP: 
#endif
#ifdef PROVIDE_FOREIGN
    case FOREIGN_REP: 
#endif
    case ALPHA_REP:    /* a                        */ 
    case BETA_REP:     /* b                        */ 
    case GAMMA_REP:    /* c                	   */ 
    case DELTA_REP:    /* d			   */ 
    case HANDLER_REP:  /* IOError -> IO a	   */ 
    case ERROR_REP:    /* IOError		   */ 
    case ARR_REP    :  /* PrimArray              a */ 
    case BARR_REP   :  /* PrimByteArray          a */ 
    case REF_REP    :  /* Ref                  s a */ 
    case MUTARR_REP :  /* PrimMutableArray     s a */ 
    case MUTBARR_REP:  /* PrimMutableByteArray s a */ 
    case MVAR_REP:     /* MVar a                   */ 
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
#if 1
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
#else
   asmInstrOp(bco,opcode);
#endif
}

static void emiti_8 ( AsmBCO bco, Instr opcode, int arg1 )
{
#if 1
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
#else
   asmInstrOp(bco,opcode);
   asmInstr8(bco,arg1);
#endif
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

static void emit_i_VAR_STABLE ( AsmBCO bco, int arg1 )
{
   ASSERT(arg1 >= 0);
   if (arg1 < 256)
      emiti_8 (bco,i_VAR_STABLE,    arg1); else
      emiti_16(bco,i_VAR_STABLE_big,arg1);
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

static void emit_i_ALLOC_CONSTR ( AsmBCO bco, int arg1 )
{
   ASSERT(arg1 >= 0);
   if (arg1 < 256)
      emiti_8 (bco,i_ALLOC_CONSTR,    arg1); else
      emiti_16(bco,i_ALLOC_CONSTR_big,arg1);
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
    }
}

/* --------------------------------------------------------------------------
 * Creating and using "variables"
 * ------------------------------------------------------------------------*/

AsmVar asmBind          ( AsmBCO bco, AsmRep rep )
{
    incSp(bco,repSizeW(rep));
    return bco->sp;
}

void   asmVar           ( AsmBCO bco, AsmVar v, AsmRep rep )
{
    int offset;

    if (rep == VOID_REP) {
        emiti_(bco,i_VOID);
        incSp(bco,repSizeW(rep));
        return;
    }

    offset = bco->sp - v;
    switch (rep) {
    case BOOL_REP:
    case INT_REP:
            emit_i_VAR_INT(bco,offset);
            break;
    case THREADID_REP:
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
    case STABLE_REP:
            emit_i_VAR_STABLE(bco,offset);
            break;

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
    case DELTA_REP:    /* d			   */ 
    case HANDLER_REP:  /* IOError -> IO a	   */
    case ERROR_REP:    /* IOError		   */
    case ARR_REP    :  /* PrimArray              a */
    case BARR_REP   :  /* PrimByteArray          a */
    case REF_REP    :  /* Ref                  s a */
    case MUTARR_REP :  /* PrimMutableArray     s a */
    case MUTBARR_REP:  /* PrimMutableByteArray s a */
    case MVAR_REP:     /* MVar a	           */
    case PTR_REP:
            emit_i_VAR(bco,offset);
            break;
    default:
            barf("asmVar %d",rep);
    }
    incSp(bco,repSizeW(rep));
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
        decSp(bco,sp1 - sp2);
    }
    emiti_(bco,i_ENTER);
    decSp(bco,sizeofW(StgPtr));
}

/* --------------------------------------------------------------------------
 * Build boxed Ints, Floats, etc
 * ------------------------------------------------------------------------*/

AsmVar asmBox( AsmBCO bco, AsmRep rep )
{
    switch (rep) {
    case CHAR_REP:
            emiti_(bco,i_PACK_CHAR);
            break;
    case INT_REP:
            emiti_(bco,i_PACK_INT);
            break;
    case THREADID_REP:
    case WORD_REP:
            emiti_(bco,i_PACK_WORD);
            break;
    case ADDR_REP:
            emiti_(bco,i_PACK_ADDR);
            break;
    case FLOAT_REP:
            emiti_(bco,i_PACK_FLOAT);
            break;
    case DOUBLE_REP:
            emiti_(bco,i_PACK_DOUBLE);
            break;
    case STABLE_REP:
            emiti_(bco,i_PACK_STABLE);
            break;

    default:
            barf("asmBox %d",rep);
    }
    /* NB: these operations DO pop their arg       */
    decSp(bco, repSizeW(rep));   /* pop unboxed arg */
    incSp(bco, sizeofW(StgPtr)); /* push box        */
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
    case THREADID_REP:
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
    case STABLE_REP:
            emiti_(bco,i_UNPACK_STABLE);
            break;
    default:
            barf("asmUnbox %d",rep);
    }
    /* NB: these operations DO NOT pop their arg  */
    incSp(bco, repSizeW(rep)); /* push unboxed arg */
    return bco->sp;
}


/* --------------------------------------------------------------------------
 * Push unboxed Ints, Floats, etc
 * ------------------------------------------------------------------------*/

void asmConstInt( AsmBCO bco, AsmInt x )
{
    emit_i_CONST_INT(bco,bco->n_words);
    asmAddNonPtrWords(bco,AsmInt,x);
    incSp(bco, repSizeW(INT_REP));
}

void asmConstInteger( AsmBCO bco, AsmString x )
{
    emit_i_CONST_INTEGER(bco,bco->n_words);
    asmAddNonPtrWords(bco,AsmString,x);
    incSp(bco, repSizeW(INTEGER_REP));
}

void asmConstAddr( AsmBCO bco, AsmAddr x )
{
    emit_i_CONST_ADDR(bco,bco->n_words);
    asmAddNonPtrWords(bco,AsmAddr,x);
    incSp(bco, repSizeW(ADDR_REP));
}

void asmConstWord( AsmBCO bco, AsmWord x )
{
    emit_i_CONST_INT(bco,bco->n_words);
    asmAddNonPtrWords(bco,AsmWord,(AsmInt)x);
    incSp(bco, repSizeW(WORD_REP));
}

void asmConstChar( AsmBCO bco, AsmChar x )
{
    emit_i_CONST_CHAR(bco,bco->n_words);
    asmAddNonPtrWords(bco,AsmChar,x);
    incSp(bco, repSizeW(CHAR_REP));
}

void asmConstFloat( AsmBCO bco, AsmFloat x )
{
    emit_i_CONST_FLOAT(bco,bco->n_words);
    asmAddNonPtrWords(bco,AsmFloat,x);
    incSp(bco, repSizeW(FLOAT_REP));
}

void asmConstDouble( AsmBCO bco, AsmDouble x )
{
    emit_i_CONST_DOUBLE(bco,bco->n_words);
    asmAddNonPtrWords(bco,AsmDouble,x);
    incSp(bco, repSizeW(DOUBLE_REP));
}

/* --------------------------------------------------------------------------
 * Algebraic case helpers
 * ------------------------------------------------------------------------*/

/* a mildly bogus pair of functions... */
AsmSp asmBeginCase( AsmBCO bco )
{
    return bco->sp;
}

void asmEndCase( AsmBCO bco __attribute__ ((unused)) )
{
}

AsmSp asmContinuation( AsmBCO bco, AsmBCO ret_addr )
{
    emit_i_RETADDR(bco,bco->n_refs);
    asmAddRefObject(bco,ret_addr);
    incSp(bco, 2 * sizeofW(StgPtr));
    return bco->sp;
}

AsmBCO asmBeginContinuation ( AsmSp sp, int /*List*/ alts )
{
    AsmBCO bco = asmBeginBCO(alts);
    setSp(bco, sp);
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
    setSp(bco,sp);
}

AsmPc asmTest( AsmBCO bco, AsmWord tag )
{
    emiti_8_16(bco,i_TEST,tag,0);
    return bco->n_insns;
}

AsmPc asmTestInt ( AsmBCO bco, AsmVar v, AsmInt x )
{
    asmVar(bco,v,INT_REP);
    asmConstInt(bco,x);
    emiti_16(bco,i_TEST_INT,0);
    decSp(bco, 2*repSizeW(INT_REP));
    return bco->n_insns;
}

void asmFixBranch ( AsmBCO bco, AsmPc from )
{
    int distance = bco->n_insns - from;
    ASSERT(distance >= 0);
    ASSERT(distance < 65536);
    setInstrs(bco,from-2,distance/256);
    setInstrs(bco,from-1,distance%256);
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

void asmEndPrim( AsmBCO bco, const AsmPrim* prim, AsmSp base )
{
    emiti_8(bco,prim->prefix,prim->opcode);
    setSp(bco, base);
}

char* asmGetPrimopName ( AsmPrim* p )
{
   return p->name;
}

/* Hugs used to let you add arbitrary primops with arbitrary types
 * just by editing Prelude.hs or any other file you wanted.
 * We deliberately avoided that approach because we wanted more
 * control over which primops are provided.
 */
AsmPrim asmPrimOps[] = {

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
    , { "primIndexStableOffAddr",    "AI", "s",  MONAD_Id, i_PRIMOP1, i_indexStableOffAddr }

    /* Stable# operations */
    , { "primIntToStablePtr",        "I",  "s",  MONAD_Id, i_PRIMOP1, i_intToStable }
    , { "primStablePtrToInt",        "s",  "I",  MONAD_Id, i_PRIMOP1, i_stableToInt }

    /* These ops really ought to be in the IO monad */
    , { "primReadCharOffAddr",       "AI", "C",  MONAD_ST, i_PRIMOP1, i_readCharOffAddr }
    , { "primReadIntOffAddr",        "AI", "I",  MONAD_ST, i_PRIMOP1, i_readIntOffAddr }
    , { "primReadWordOffAddr",       "AI", "W",  MONAD_ST, i_PRIMOP1, i_readWordOffAddr }
    , { "primReadAddrOffAddr",       "AI", "A",  MONAD_ST, i_PRIMOP1, i_readAddrOffAddr }
    , { "primReadFloatOffAddr",      "AI", "F",  MONAD_ST, i_PRIMOP1, i_readFloatOffAddr }
    , { "primReadDoubleOffAddr",     "AI", "D",  MONAD_ST, i_PRIMOP1, i_readDoubleOffAddr }
    , { "primReadStableOffAddr",     "AI", "s",  MONAD_ST, i_PRIMOP1, i_readStableOffAddr }

    /* These ops really ought to be in the IO monad */
    , { "primWriteCharOffAddr",      "AIC", "",  MONAD_ST, i_PRIMOP1, i_writeCharOffAddr }
    , { "primWriteIntOffAddr",       "AII", "",  MONAD_ST, i_PRIMOP1, i_writeIntOffAddr }
    , { "primWriteWordOffAddr",      "AIW", "",  MONAD_ST, i_PRIMOP1, i_writeWordOffAddr }
    , { "primWriteAddrOffAddr",      "AIA", "",  MONAD_ST, i_PRIMOP1, i_writeAddrOffAddr }
    , { "primWriteFloatOffAddr",     "AIF", "",  MONAD_ST, i_PRIMOP1, i_writeFloatOffAddr }
    , { "primWriteDoubleOffAddr",    "AID", "",  MONAD_ST, i_PRIMOP1, i_writeDoubleOffAddr }
    , { "primWriteStableOffAddr",    "AIs", "",  MONAD_ST, i_PRIMOP1, i_writeStableOffAddr }

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

#if 0
#ifdef PROVIDE_STABLE
    , { "primWriteStableArray",      "mIs", "",  MONAD_ST, i_PRIMOP2, i_writeStableArray }
    , { "primReadStableArray",       "mI",  "s", MONAD_ST, i_PRIMOP2, i_readStableArray }
    , { "primIndexStableArray",      "xI",  "s", MONAD_Id, i_PRIMOP2, i_indexStableArray }
#endif
#endif
    /* {new,write,read,index}ForeignObjArray not provided */


#ifdef PROVIDE_FOREIGN
    /* ForeignObj# operations */
    , { "primMkForeignObj",          "A",  "f",  MONAD_IO, i_PRIMOP2, i_mkForeignObj }
#endif
#ifdef PROVIDE_WEAK
    /* WeakPair# operations */
    , { "primMakeWeak",              "bac", "w",  MONAD_IO, i_PRIMOP2, i_makeWeak }
    , { "primDeRefWeak",             "w",   "Ia", MONAD_IO, i_PRIMOP2, i_deRefWeak }
#endif
    /* StablePtr# operations */
    , { "primMakeStablePtr",         "a", "s",   MONAD_IO, i_PRIMOP2, i_makeStablePtr }
    , { "primDeRefStablePtr",        "s", "a",   MONAD_IO, i_PRIMOP2, i_deRefStablePtr }
    , { "primFreeStablePtr",         "s", "",    MONAD_IO, i_PRIMOP2, i_freeStablePtr }

    /* foreign export dynamic support */
    , { "primCreateAdjThunkARCH",    "sAC","A",  MONAD_IO, i_PRIMOP2, i_createAdjThunkARCH }

    /* misc handy hacks */
    , { "primGetArgc",               "",   "I",  MONAD_IO, i_PRIMOP2, i_getArgc }
    , { "primGetArgv",               "I",  "A",  MONAD_IO, i_PRIMOP2, i_getArgv }

#ifdef PROVIDE_PTREQUALITY
    , { "primReallyUnsafePtrEquality", "aa", "B",MONAD_Id, i_PRIMOP2, i_reallyUnsafePtrEquality }
#endif
#ifdef PROVIDE_COERCE
    , { "primUnsafeCoerce",          "a", "b",   MONAD_Id, i_PRIMOP2, i_unsafeCoerce }
#endif
#ifdef PROVIDE_CONCURRENT
    /* Concurrency operations */
    , { "primForkIO",                "a", "T",   MONAD_IO, i_PRIMOP2, i_forkIO }
    , { "primKillThread",            "T", "",    MONAD_IO, i_PRIMOP2, i_killThread }
    , { "primRaiseInThread",         "TE", "",   MONAD_IO, i_PRIMOP2, i_raiseInThread }

    , { "primWaitRead",              "I", "",    MONAD_IO, i_PRIMOP2, i_waitRead }
    , { "primWaitWrite",             "I", "",    MONAD_IO, i_PRIMOP2, i_waitWrite }
    , { "primYield",                 "", "",     MONAD_IO, i_PRIMOP2, i_yield }    , { "primDelay",                 "I", "",    MONAD_IO, i_PRIMOP2, i_delay }
    , { "primGetThreadId",           "",   "T",  MONAD_IO, i_PRIMOP2, i_getThreadId }
    , { "primCmpThreadIds",          "TT", "I",  MONAD_Id, i_PRIMOP2, i_cmpThreadIds }
#endif
    , { "primNewEmptyMVar",          "",  "r",   MONAD_IO, i_PRIMOP2, i_newMVar }
      /* primTakeMVar is handwritten bytecode */
    , { "primPutMVar",               "ra", "",   MONAD_IO, i_PRIMOP2, i_putMVar } 
    , { "primSameMVar",              "rr", "B",  MONAD_Id, i_PRIMOP2, i_sameMVar }

  
    /* Ccall is polyadic - so it's excluded from this table */

    , { 0,0,0,0,0,0 }
};

AsmPrim ccall_ccall_Id
   = { "ccall", 0, 0, MONAD_IO, i_PRIMOP2, i_ccall_ccall_Id };
AsmPrim ccall_ccall_IO
   = { "ccall", 0, 0, MONAD_IO, i_PRIMOP2, i_ccall_ccall_IO };
AsmPrim ccall_stdcall_Id 
   = { "ccall", 0, 0, MONAD_IO, i_PRIMOP2, i_ccall_stdcall_Id };
AsmPrim ccall_stdcall_IO 
   = { "ccall", 0, 0, MONAD_IO, i_PRIMOP2, i_ccall_stdcall_IO };

#ifdef DEBUG
void checkBytecodeCount( void );
void checkBytecodeCount( void ) 
{
  if (MAX_Primop1 >= 255) {
    printf("Too many Primop1 bytecodes (%d)\n",MAX_Primop1);
  }
  if (MAX_Primop2 >= 255) {
    printf("Too many Primop2 bytecodes (%d)\n",MAX_Primop2);
  }
}
#endif

AsmPrim* asmFindPrim( char* s )
{
    int i;
    for (i=0; asmPrimOps[i].name; ++i) {
        if (strcmp(s,asmPrimOps[i].name)==0) {
            return &asmPrimOps[i];
        }
    }
    return 0;
}

AsmPrim* asmFindPrimop( AsmInstr prefix, AsmInstr op )
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

void* /* StgBCO* */ asm_BCO_catch ( void )
{
   AsmBCO  bco;
   StgBCO* closure;
   asmInitialise();

   bco = asmBeginBCO(0 /*NIL*/);
   emiti_8(bco,i_ARG_CHECK,2);
   emiti_8(bco,i_PRIMOP1,i_pushcatchframe);
   incSp(bco, (1-2)*sizeofW(StgPtr) + sizeofW(StgCatchFrame));
   emiti_(bco,i_ENTER);
   decSp(bco, sizeofW(StgPtr));
   asmEndBCO(bco);

   asmAllocateHeapSpace();
   asmCopyAndLink();
   closure = (StgBCO*)(bco->closure);
   asmShutdown();
   return closure;
}

void* /* StgBCO* */ asm_BCO_raise ( void )
{
   AsmBCO bco;
   StgBCO* closure;
   asmInitialise();

   bco = asmBeginBCO(0 /*NIL*/);
   emiti_8(bco,i_ARG_CHECK,1);
   emiti_8(bco,i_PRIMOP2,i_raise);
   decSp(bco,sizeofW(StgPtr));
   asmEndBCO(bco);

   asmAllocateHeapSpace();
   asmCopyAndLink();
   closure = (StgBCO*)(bco->closure);
   asmShutdown();
   return closure;
}

void* /* StgBCO* */ asm_BCO_seq ( void )
{
   AsmBCO eval, cont;
   StgBCO* closure;
   asmInitialise();

   cont = asmBeginBCO(0 /*NIL*/);
   emiti_8(cont,i_ARG_CHECK,2);   /* should never fail */
   emit_i_VAR(cont,1);
   emit_i_SLIDE(cont,1,2);
   emiti_(cont,i_ENTER);
   incSp(cont, 3*sizeofW(StgPtr));
   asmEndBCO(cont);

   eval = asmBeginBCO(0 /*NIL*/);
   emiti_8(eval,i_ARG_CHECK,2);
   emit_i_RETADDR(eval,eval->n_refs);
   asmAddRefObject(eval,cont);
   emit_i_VAR(eval,2);
   emit_i_SLIDE(eval,3,1);
   emiti_8(eval,i_PRIMOP1,i_pushseqframe);
   emiti_(eval,i_ENTER);
   incSp(eval, sizeofW(StgSeqFrame) + 4*sizeofW(StgPtr));
   asmEndBCO(eval);

   asmAllocateHeapSpace();
   asmCopyAndLink();
   closure = (StgBCO*)(eval->closure);
   asmShutdown();
   return closure;
}

void* /* StgBCO* */ asm_BCO_takeMVar ( void )
{
   AsmBCO kase, casecont, take;
   StgBCO* closure;
   asmInitialise();

   take = asmBeginBCO(0 /*NIL*/);
   emit_i_VAR(take,0);
   emiti_8(take,i_PRIMOP2,i_takeMVar);
   emit_i_VAR(take,3);
   emit_i_VAR(take,1);
   emit_i_VAR(take,4);
   emit_i_SLIDE(take,3,4);
   emiti_(take,i_ENTER);
   incSp(take,20);
   asmEndBCO(take);

   casecont = asmBeginBCO(0 /*NIL*/);
   emiti_(casecont,i_UNPACK);
   emit_i_VAR(casecont,4);
   emit_i_VAR(casecont,4);
   emit_i_VAR(casecont,2);
   emit_i_CONST(casecont,casecont->n_refs);
   asmAddRefObject(casecont,take);
   emit_i_SLIDE(casecont,4,5);
   emiti_(casecont,i_ENTER);
   incSp(casecont,20);
   asmEndBCO(casecont);

   kase = asmBeginBCO(0 /*NIL*/);
   emiti_8(kase,i_ARG_CHECK,3);
   emit_i_RETADDR(kase,kase->n_refs);
   asmAddRefObject(kase,casecont);
   emit_i_VAR(kase,2);
   emiti_(kase,i_ENTER);
   incSp(kase,20);
   asmEndBCO(kase);

   asmAllocateHeapSpace();
   asmCopyAndLink();
   closure = (StgBCO*)(kase->closure);
   asmShutdown();
   return closure;
}


/* --------------------------------------------------------------------------
 * Heap manipulation
 * ------------------------------------------------------------------------*/

AsmVar asmAllocCONSTR   ( AsmBCO bco, AsmInfo info )
{
    int i;
    ASSERT( sizeW_fromITBL(info) >= MIN_NONUPD_SIZE + sizeofW(StgHeader) );

    /* Look in this bco's collection of nonpointers (literals)
       to see if the itbl pointer is already there.  If so, re-use it. */
    i = asmFindInNonPtrs ( bco, (StgWord)info );

    if (i == -1) {
       emit_i_ALLOC_CONSTR(bco,bco->n_words);
       asmAddNonPtrWords(bco,AsmInfo,info);
    } else {
       emit_i_ALLOC_CONSTR(bco,i);
    }

    incSp(bco, sizeofW(StgClosurePtr));
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
    setSp(bco, start);
}

void asmBeginUnpack( AsmBCO bco __attribute__ ((unused)) )
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
    incSp(bco, sizeofW(StgPtr));
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
    setSp(bco, start);
}

AsmVar asmAllocPAP( AsmBCO bco, AsmNat size )
{
    emiti_8(bco,i_ALLOC_PAP,size);
    incSp(bco, sizeofW(StgPtr));
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
    setSp(bco, start);
}

AsmVar asmPushRefHugs ( AsmBCO bco, int /*Name*/ n )
{
    emit_i_CONST(bco,bco->n_refs);
    asmAddRefHugs(bco,n);
    incSp(bco, sizeofW(StgPtr));
    return bco->sp;
}

AsmVar asmPushRefObject ( AsmBCO bco, AsmObject p )
{
    emit_i_CONST(bco,bco->n_refs);
    asmAddRefObject(bco,p);
    incSp(bco, sizeofW(StgPtr));
    return bco->sp;
}

AsmVar asmPushRefNoOp ( AsmBCO bco, StgPtr p )
{
    emit_i_CONST(bco,bco->n_refs);
    asmAddRefNoOp(bco,p);
    incSp(bco, sizeofW(StgPtr));
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
