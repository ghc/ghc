/* -*- mode: hugs-c; -*- */
/* -----------------------------------------------------------------------------
 * Bytecode evaluator
 *
 * Copyright (c) 1994-1998.
 *
 * $RCSfile: Evaluator.c,v $
 * $Revision: 1.2 $
 * $Date: 1998/12/02 13:28:17 $
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

#ifdef INTERPRETER

#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Updates.h"
#include "Storage.h"
#include "SchedAPI.h" /* for createGenThread */
#include "Schedule.h" /* for context_switch  */

#include "Bytecodes.h"
#include "Assembler.h" /* for CFun stuff */
#include "ForeignCall.h"
#include "StablePtr.h"
#include "PrimOps.h"   /* for __{encode,decode}{Float,Double} */
#include "Evaluator.h"

#ifdef DEBUG
#include "Printer.h"
#include "Disassembler.h"

#include "Sanity.h"
#include "StgRun.h"
#endif

#include <math.h>    /* These are for primops */
#include <limits.h>  /* These are for primops */
#include <float.h>   /* These are for primops */
#ifdef HAVE_IEEE754_H
#include <ieee754.h> /* These are for primops */
#endif
#ifdef PROVIDE_INTEGER
#include "gmp.h"     /* These are for primops */
#endif

/* An incredibly useful abbreviation.
 * Interestingly, there are some uses of END_TSO_QUEUE_closure that
 * can't use it because they use the closure at type StgClosure* or
 * even StgPtr*.  I suspect they should be changed.  -- ADR
 */
#define EndTSOQueue stgCast(StgTSO*,(void*)&END_TSO_QUEUE_closure)

/* These macros are rather delicate - read a good ANSI C book carefully
 * before meddling.
 */
#define mystr(x)      #x
#define mycat(x,y)    x##y
#define mycat2(x,y)   mycat(x,y)
#define mycat3(x,y,z) mycat2(x,mycat2(y,z))

/* --------------------------------------------------------------------------
 * Hugs Hooks - a bit of a hack
 * ------------------------------------------------------------------------*/

void setRtsFlags( int x );
void setRtsFlags( int x )
{
    *(int*)(&(RtsFlags.DebugFlags)) = x;
}

/* --------------------------------------------------------------------------
 * RTS Hooks
 *
 * ToDo: figure out why these are being used and crush them!
 * ------------------------------------------------------------------------*/

void OnExitHook (void)
{
}
void StackOverflowHook (unsigned long stack_size)
{
    fprintf(stderr,"Stack Overflow\n");
    exit(1);
}
void OutOfHeapHook (unsigned long request_size, unsigned long heap_size)
{
    fprintf(stderr,"Out Of Heap\n");
    exit(1);
}
void MallocFailHook (unsigned long request_size /* in bytes */, char *msg)
{
    fprintf(stderr,"Malloc Fail\n");
    exit(1);
}
void defaultsHook (void)
{
    /* do nothing */
}

/* --------------------------------------------------------------------------
 * MPZ helpers
 * ------------------------------------------------------------------------*/

#ifdef PROVIDE_INTEGER
static inline mpz_ptr mpz_alloc ( void );
static inline void    mpz_free  ( mpz_ptr );

static inline mpz_ptr mpz_alloc ( void )
{
    mpz_ptr r = stgCast(mpz_ptr,stgMallocBytes( sizeof(mpz_t),"mpz_alloc"));
    mpz_init(r);
    return r;
}

static inline void    mpz_free  ( mpz_ptr a )
{
    mpz_clear(a);
    free(a);
}
#endif

/* --------------------------------------------------------------------------
 * 
 * ------------------------------------------------------------------------*/

static inline void            PushTag            ( StackTag    t );
static inline void            PushPtr            ( StgPtr      x );
static inline void            PushCPtr           ( StgClosure* x );
static inline void            PushInt            ( StgInt      x );
static inline void            PushWord           ( StgWord     x );
                                                 
static inline void            PushTag            ( StackTag    t ) { *(--Sp) = t; }
static inline void            PushPtr            ( StgPtr      x ) { *(--stgCast(StgPtr*,Sp))  = x; }
static inline void            PushCPtr           ( StgClosure* x ) { *(--stgCast(StgClosure**,Sp)) = x; }
static inline void            PushInt            ( StgInt      x ) { *(--stgCast(StgInt*,Sp))  = x; }
static inline void            PushWord           ( StgWord     x ) { *(--stgCast(StgWord*,Sp)) = x; }
                                                     
static inline void            checkTag           ( StackTag t1, StackTag t2 );
static inline void            PopTag             ( StackTag t );
static inline StgPtr          PopPtr             ( void );
static inline StgClosure*     PopCPtr            ( void );
static inline StgInt          PopInt             ( void );
static inline StgWord         PopWord            ( void );
                                                 
static inline void            checkTag           ( StackTag t1, StackTag t2 ) { ASSERT(t1 == t2);}
static inline void            PopTag             ( StackTag t ) { checkTag(t,*(Sp++));    }
static inline StgPtr          PopPtr             ( void )       { return *stgCast(StgPtr*,Sp)++; }
static inline StgClosure*     PopCPtr            ( void )       { return *stgCast(StgClosure**,Sp)++; }
static inline StgInt          PopInt             ( void )       { return *stgCast(StgInt*,Sp)++;  }
static inline StgWord         PopWord            ( void )       { return *stgCast(StgWord*,Sp)++; }

static inline StgPtr          stackPtr           ( StgStackOffset i );
static inline StgInt          stackInt           ( StgStackOffset i );
static inline StgWord         stackWord          ( StgStackOffset i );

static inline StgPtr          stackPtr           ( StgStackOffset i ) { return *stgCast(StgPtr*, Sp+i); }
static inline StgInt          stackInt           ( StgStackOffset i ) { return *stgCast(StgInt*, Sp+i); }
static inline StgWord         stackWord          ( StgStackOffset i ) { return *stgCast(StgWord*,Sp+i); }
                              
static inline void            setStackWord       ( StgStackOffset i, StgWord w );

static inline void            setStackWord       ( StgStackOffset i, StgWord w ) { Sp[i] = w; }
                              
static inline void            PushTaggedRealWorld( void         );
static inline void            PushTaggedInt      ( StgInt     x );
#ifdef PROVIDE_INT64
static inline void            PushTaggedInt64    ( StgInt64   x );
#endif
#ifdef PROVIDE_INTEGER
static inline void            PushTaggedInteger  ( mpz_ptr    x );
#endif
#ifdef PROVIDE_WORD
static inline void            PushTaggedWord     ( StgWord    x );
#endif
#ifdef PROVIDE_ADDR
static inline void            PushTaggedAddr     ( StgAddr    x );
#endif
static inline void            PushTaggedChar     ( StgChar    x );
static inline void            PushTaggedFloat    ( StgFloat   x );
static inline void            PushTaggedDouble   ( StgDouble  x );
static inline void            PushTaggedStablePtr   ( StgStablePtr x );
static inline void            PushTaggedBool     ( int        x );

static inline void            PushTaggedRealWorld( void            ) { PushTag(REALWORLD_TAG);  }
static inline void            PushTaggedInt      ( StgInt        x ) { Sp -= sizeofW(StgInt);        *Sp = x;          PushTag(INT_TAG);    }
#ifdef PROVIDE_INT64
static inline void            PushTaggedInt64    ( StgInt64      x ) { Sp -= sizeofW(StgInt64);      ASSIGN_Int64(Sp,x); PushTag(INT64_TAG); }
#endif
#ifdef PROVIDE_INTEGER
static inline void            PushTaggedInteger  ( mpz_ptr    x )
{
    StgForeignObj *result;
    StgWeak *w;

    result = stgCast(StgForeignObj*,allocate(sizeofW(StgForeignObj)));
    SET_HDR(result,&FOREIGN_info,CCCS);
    result -> data      = x;

#if 0 /* For now we don't deallocate Integer's at all */
    w = stgCast(StgWeak*,allocate(sizeofW(StgWeak)));
    SET_HDR(w, &WEAK_info, CCCS);
    w->key        = stgCast(StgClosure*,result);
    w->value      = stgCast(StgClosure*,result); /* or any other closure you have handy */
    w->finaliser  = funPtrToIO(mpz_free);
    w->link       = weak_ptr_list;
    weak_ptr_list = w;
    IF_DEBUG(weak, fprintf(stderr,"New weak pointer watching Foreign MPZ at %p\n",w));
#endif

    PushPtr(stgCast(StgPtr,result));
}
#endif
#ifdef PROVIDE_WORD
static inline void            PushTaggedWord     ( StgWord       x ) { Sp -= sizeofW(StgWord);       *Sp = x;          PushTag(WORD_TAG);   }
#endif
#ifdef PROVIDE_ADDR
static inline void            PushTaggedAddr     ( StgAddr       x ) { Sp -= sizeofW(StgAddr);       *Sp = (W_)x;      PushTag(ADDR_TAG);   }
#endif
static inline void            PushTaggedChar     ( StgChar       x ) { Sp -= sizeofW(StgChar);       *Sp = x;          PushTag(CHAR_TAG);   }
static inline void            PushTaggedFloat    ( StgFloat      x ) { Sp -= sizeofW(StgFloat);      ASSIGN_FLT(Sp,x); PushTag(FLOAT_TAG);  }
static inline void            PushTaggedDouble   ( StgDouble     x ) { Sp -= sizeofW(StgDouble);     ASSIGN_DBL(Sp,x); PushTag(DOUBLE_TAG); }
static inline void            PushTaggedStablePtr   ( StgStablePtr  x ) { Sp -= sizeofW(StgStablePtr);  *Sp = x;          PushTag(STABLE_TAG); }
static inline void            PushTaggedBool     ( int           x ) { PushTaggedInt(x); }

static inline void            PopTaggedRealWorld ( void );
static inline StgInt          PopTaggedInt       ( void );
#ifdef PROVIDE_INT64
static inline StgInt64        PopTaggedInt64     ( void );
#endif
#ifdef PROVIDE_INTEGER
static inline mpz_ptr         PopTaggedInteger   ( void );
#endif
#ifdef PROVIDE_WORD
static inline StgWord         PopTaggedWord      ( void );
#endif
#ifdef PROVIDE_ADDR
static inline StgAddr         PopTaggedAddr      ( void );
#endif
static inline StgChar         PopTaggedChar      ( void );
static inline StgFloat        PopTaggedFloat     ( void );
static inline StgDouble       PopTaggedDouble    ( void );
static inline StgStablePtr    PopTaggedStablePtr    ( void );

static inline void            PopTaggedRealWorld ( void ) { PopTag(REALWORLD_TAG); }
static inline StgInt          PopTaggedInt       ( void ) { StgInt    r; PopTag(INT_TAG);     r = *stgCast(StgInt*,  Sp);      Sp += sizeofW(StgInt);        return r;}
#ifdef PROVIDE_INT64
static inline StgInt64        PopTaggedInt64     ( void ) { StgInt64  r; PopTag(INT64_TAG);   r = PK_Int64(Sp);                Sp += sizeofW(StgInt64);      return r;}
#endif
#ifdef PROVIDE_INTEGER
static inline mpz_ptr         PopTaggedInteger   ( void ) { StgForeignObj *r = *stgCast(StgForeignObj**,Sp); Sp += sizeofW(StgPtr); return stgCast(mpz_ptr,r->data);}
#endif
#ifdef PROVIDE_WORD
static inline StgWord         PopTaggedWord      ( void ) { StgWord   r; PopTag(WORD_TAG);    r = *stgCast(StgWord*, Sp);      Sp += sizeofW(StgWord);       return r;}
#endif
#ifdef PROVIDE_ADDR
static inline StgAddr         PopTaggedAddr      ( void ) { StgAddr   r; PopTag(ADDR_TAG);    r = *stgCast(StgAddr*, Sp);      Sp += sizeofW(StgAddr);       return r;}
#endif
static inline StgChar         PopTaggedChar      ( void ) { StgChar   r; PopTag(CHAR_TAG);    r = *stgCast(StgChar*, Sp);      Sp += sizeofW(StgChar);       return r;}
static inline StgFloat        PopTaggedFloat     ( void ) { StgFloat  r; PopTag(FLOAT_TAG);   r = PK_FLT(Sp);                  Sp += sizeofW(StgFloat);      return r;}
static inline StgDouble       PopTaggedDouble    ( void ) { StgDouble r; PopTag(DOUBLE_TAG);  r = PK_DBL(Sp);                  Sp += sizeofW(StgDouble);     return r;}
static inline StgStablePtr    PopTaggedStablePtr    ( void ) { StgInt    r; PopTag(STABLE_TAG);  r = *stgCast(StgStablePtr*, Sp); Sp += sizeofW(StgStablePtr);  return r;}

static inline StgInt          taggedStackInt     ( StgStackOffset i );
#ifdef PROVIDE_INT64
static inline StgInt64        taggedStackInt64   ( StgStackOffset i );
#endif
#ifdef PROVIDE_WORD
static inline StgWord         taggedStackWord    ( StgStackOffset i );
#endif
#ifdef PROVIDE_ADDR
static inline StgAddr         taggedStackAddr    ( StgStackOffset i );
#endif
static inline StgChar         taggedStackChar    ( StgStackOffset i );
static inline StgFloat        taggedStackFloat   ( StgStackOffset i );
static inline StgDouble       taggedStackDouble  ( StgStackOffset i );
static inline StgStablePtr    taggedStackStable  ( StgStackOffset i );

static inline StgInt          taggedStackInt     ( StgStackOffset i ) { checkTag(INT_TAG,Sp[i]);     return *stgCast(StgInt*,         Sp+1+i); }
#ifdef PROVIDE_INT64
static inline StgInt64        taggedStackInt64   ( StgStackOffset i ) { checkTag(INT64_TAG,Sp[i]);   return PK_Int64(Sp+1+i); }
#endif
#ifdef PROVIDE_WORD
static inline StgWord         taggedStackWord    ( StgStackOffset i ) { checkTag(WORD_TAG,Sp[i]);    return *stgCast(StgWord*,        Sp+1+i); }
#endif
#ifdef PROVIDE_ADDR
static inline StgAddr         taggedStackAddr    ( StgStackOffset i ) { checkTag(ADDR_TAG,Sp[i]);    return *stgCast(StgAddr*,        Sp+1+i); }
#endif
static inline StgChar         taggedStackChar    ( StgStackOffset i ) { checkTag(CHAR_TAG,Sp[i]);    return *stgCast(StgChar*,        Sp+1+i); }
static inline StgFloat        taggedStackFloat   ( StgStackOffset i ) { checkTag(FLOAT_TAG,Sp[i]);   return PK_FLT(Sp+1+i); }
static inline StgDouble       taggedStackDouble  ( StgStackOffset i ) { checkTag(DOUBLE_TAG,Sp[i]);  return PK_DBL(Sp+1+i); }
static inline StgStablePtr    taggedStackStable  ( StgStackOffset i ) { checkTag(STABLE_TAG,Sp[i]);  return *stgCast(StgStablePtr*,   Sp+1+i); }


/* --------------------------------------------------------------------------
 * Heap allocation
 *
 * Should we allocate from a nursery or use the
 * doYouWantToGC/allocate interface?  We'd already implemented a
 * nursery-style scheme when the doYouWantToGC/allocate interface
 * was implemented.
 * One reason to prefer the doYouWantToGC/allocate interface is to 
 * support operations which allocate an unknown amount in the heap
 * (array ops, gmp ops, etc)
 * ------------------------------------------------------------------------*/

static inline StgPtr grabHpUpd( nat size )
{
    ASSERT( size >= MIN_UPD_SIZE + sizeofW(StgHeader) );
    return allocate(size);
}

static inline StgPtr grabHpNonUpd( nat size )
{
    ASSERT( size >= MIN_NONUPD_SIZE + sizeofW(StgHeader) );
    return allocate(size);
}

/* --------------------------------------------------------------------------
 * Manipulate "update frame" list:
 * o Update frames           (based on stg_do_update and friends in Updates.hc)
 * o Error handling/catching (based on catchZh_fast and friends in Prims.hc)
 * o Seq frames              (based on seq_frame_entry in Prims.hc)
 * o Stop frames
 * ------------------------------------------------------------------------*/

static inline void PopUpdateFrame ( StgClosure* obj );
static inline void PushCatchFrame ( StgClosure* catcher );
static inline void PopCatchFrame  ( void );
static inline void PushSeqFrame   ( void );
static inline void PopSeqFrame    ( void );

static inline StgClosure* raiseAnError   ( StgClosure* errObj );

static inline void PopUpdateFrame( StgClosure* obj )
{
    /* NB: doesn't assume that Sp == Su */
    IF_DEBUG(evaluator,
             fprintf(stderr,  "Updating ");
             printPtr(stgCast(StgPtr,Su->updatee)); 
             fprintf(stderr,  " with ");
             printObj(obj);
             fprintf(stderr,"\nSp = %p\tSu = %p\n", Sp, Su);
             );
#ifndef LAZY_BLACKHOLING
    ASSERT(get_itbl(Su->updatee)->type == BLACKHOLE
           || get_itbl(Su->updatee)->type == CAF_BLACKHOLE
           );
#endif /* LAZY_BLACKHOLING */
    UPD_IND(Su->updatee,obj);
    Sp = stgCast(StgStackPtr,Su) + sizeofW(StgUpdateFrame);
    Su = Su->link;
}

static inline void PopStopFrame( StgClosure* obj )
{
    /* Move Su just off the end of the stack, we're about to spam the
     * STOP_FRAME with the return value.
     */
    Su = stgCast(StgUpdateFrame*,Sp+1);  
    *stgCast(StgClosure**,Sp) = obj;
}

static inline void PushCatchFrame( StgClosure* handler )
{
    StgCatchFrame* fp;
    /* ToDo: stack check! */
    Sp -= sizeofW(StgCatchFrame*);  /* ToDo: this can't be right */
    fp = stgCast(StgCatchFrame*,Sp);
    SET_HDR(fp,&catch_frame_info,CCCS);
    fp->handler         = handler;
    fp->link            = Su;
    Su = stgCast(StgUpdateFrame*,fp);
}

static inline void PopCatchFrame( void )
{
    /* NB: doesn't assume that Sp == Su */
    /* fprintf(stderr,"Popping catch frame\n"); */
    Sp = stgCast(StgStackPtr,Su) + sizeofW(StgCatchFrame);
    Su = stgCast(StgCatchFrame*,Su)->link;		
}

static inline void PushSeqFrame( void )
{
    StgSeqFrame* fp;
    /* ToDo: stack check! */
    Sp -= sizeofW(StgSeqFrame*);  /* ToDo: this can't be right */
    fp = stgCast(StgSeqFrame*,Sp);
    SET_HDR(fp,&seq_frame_info,CCCS);
    fp->link = Su;
    Su = stgCast(StgUpdateFrame*,fp);
}

static inline void PopSeqFrame( void )
{
    /* NB: doesn't assume that Sp == Su */
    Sp = stgCast(StgStackPtr,Su) + sizeofW(StgSeqFrame);
    Su = stgCast(StgSeqFrame*,Su)->link;		
}

static inline StgClosure* raiseAnError( StgClosure* errObj )
{
    while (1) {
        switch (get_itbl(Su)->type) {
        case UPDATE_FRAME:
                UPD_INPLACE1(Su->updatee,&raise_info,errObj);
                Sp = stgCast(StgStackPtr,Su) + sizeofW(StgUpdateFrame);
                Su = Su->link;
                break;
        case SEQ_FRAME:
                PopSeqFrame();
                break;
        case CATCH_FRAME:  /* found it! */
            {
                StgCatchFrame* fp = stgCast(StgCatchFrame*,Su);
                StgClosure *handler = fp->handler;
                Su = fp->link; 
                Sp += sizeofW(StgCatchFrame); /* Pop */
                PushCPtr(errObj);
                return handler;
            }
        case STOP_FRAME:
                barf("raiseError: STOP_FRAME");
        default:
                barf("raiseError: weird activation record");
        }
    }
}

static StgClosure* raisePrim(char* msg)
{
    /* ToDo: figure out some way to turn the msg into a Haskell Exception
     * Hack: we don't know how to build an Exception but we do know how
     * to build a (recursive!) error object.
     * The result isn't pretty but it's (slightly) better than nothing.
     */
    nat size = sizeof(StgClosure) + 1;
    StgClosure* errObj = stgCast(StgClosure*,grabHpNonUpd(size));
    SET_INFO(errObj,&raise_info);
    errObj->payload[0] = errObj;

#if 0
    belch(msg);
#else
    /* At the moment, I prefer to put it on stdout to make things as
     * close to Hugs' old behaviour as possible.
     */
    fprintf(stdout, "Program error: %s", msg);
    fflush(stdout);
#endif
    return raiseAnError(stgCast(StgClosure*,errObj));
}

#define raiseIndex(where) raisePrim("Array index out of range in " where)
#define raiseDiv0(where)  raisePrim("Division by 0 in " where)

/* --------------------------------------------------------------------------
 * Evaluator
 * ------------------------------------------------------------------------*/

#define OP_CC_B(e)            \
{                             \
    unsigned char x = PopTaggedChar(); \
    unsigned char y = PopTaggedChar(); \
    PushTaggedBool(e);        \
}

#define OP_C_I(e)             \
{                             \
    unsigned char x = PopTaggedChar(); \
    PushTaggedInt(e);         \
}

#define OP__I(e)             \
{                            \
    PushTaggedInt(e);        \
}

#define OP_IW_I(e)           \
{                            \
    StgInt  x = PopTaggedInt();  \
    StgWord y = PopTaggedWord();  \
    PushTaggedInt(e);        \
}

#define OP_II_I(e)           \
{                            \
    StgInt x = PopTaggedInt();  \
    StgInt y = PopTaggedInt();  \
    PushTaggedInt(e);        \
}

#define OP_II_B(e)           \
{                            \
    StgInt x = PopTaggedInt();  \
    StgInt y = PopTaggedInt();  \
    PushTaggedBool(e);       \
}

#define OP__A(e)             \
{                            \
    PushTaggedAddr(e);       \
}

#define OP_I_A(e)            \
{                            \
    StgInt x = PopTaggedInt();  \
    PushTaggedAddr(e);       \
}

#define OP_I_I(e)            \
{                            \
    StgInt x = PopTaggedInt();  \
    PushTaggedInt(e);        \
}

#define OP__C(e)             \
{                            \
    PushTaggedChar(e);       \
}

#define OP_I_C(e)            \
{                            \
    StgInt x = PopTaggedInt();  \
    PushTaggedChar(e);       \
}

#define OP__W(e)              \
{                             \
    PushTaggedWord(e);        \
}

#define OP_I_W(e)            \
{                            \
    StgInt x = PopTaggedInt();  \
    PushTaggedWord(e);       \
}

#define OP__F(e)             \
{                            \
    PushTaggedFloat(e);      \
}

#define OP_I_F(e)            \
{                            \
    StgInt x = PopTaggedInt();  \
    PushTaggedFloat(e);      \
}

#define OP__D(e)             \
{                            \
    PushTaggedDouble(e);     \
}

#define OP_I_D(e)            \
{                            \
    StgInt x = PopTaggedInt();  \
    PushTaggedDouble(e);     \
}

#ifdef PROVIDE_WORD
#define OP_WW_B(e)            \
{                             \
    StgWord x = PopTaggedWord(); \
    StgWord y = PopTaggedWord(); \
    PushTaggedBool(e);        \
}

#define OP_WW_W(e)            \
{                             \
    StgWord x = PopTaggedWord(); \
    StgWord y = PopTaggedWord(); \
    PushTaggedWord(e);        \
}

#define OP_W_I(e)             \
{                             \
    StgWord x = PopTaggedWord(); \
    PushTaggedInt(e);         \
}

#define OP_W_W(e)             \
{                             \
    StgWord x = PopTaggedWord(); \
    PushTaggedWord(e);        \
}
#endif

#ifdef PROVIDE_ADDR
#define OP_AA_B(e)            \
{                             \
    StgAddr x = PopTaggedAddr(); \
    StgAddr y = PopTaggedAddr(); \
    PushTaggedBool(e);        \
}
#define OP_A_I(e)             \
{                             \
    StgAddr x = PopTaggedAddr(); \
    PushTaggedInt(e);         \
}
#define OP_AI_C(s)            \
{                             \
    StgAddr x = PopTaggedAddr(); \
    int  y = PopTaggedInt();  \
    StgChar r;                \
    s;                        \
    PushTaggedChar(r);        \
}
#define OP_AI_I(s)            \
{                             \
    StgAddr x = PopTaggedAddr(); \
    int  y = PopTaggedInt();  \
    StgInt r;                 \
    s;                        \
    PushTaggedInt(r);         \
}
#define OP_AI_z(s)            \
{                             \
    StgAddr x = PopTaggedAddr(); \
    int  y = PopTaggedInt();  \
    StgInt64 r;               \
    s;                        \
    PushTaggedInt64(r);       \
}
#define OP_AI_A(s)            \
{                             \
    StgAddr x = PopTaggedAddr(); \
    int  y = PopTaggedInt();  \
    StgAddr r;                \
    s;                        \
    PushTaggedAddr(s);        \
}
#define OP_AI_F(s)            \
{                             \
    StgAddr x = PopTaggedAddr(); \
    int  y = PopTaggedInt();  \
    StgFloat r;               \
    s;                        \
    PushTaggedFloat(r);       \
}
#define OP_AI_D(s)            \
{                             \
    StgAddr x = PopTaggedAddr(); \
    int  y = PopTaggedInt();  \
    StgDouble r;              \
    s;                        \
    PushTaggedDouble(r);      \
}
#define OP_AI_s(s)            \
{                             \
    StgAddr x = PopTaggedAddr(); \
    int  y = PopTaggedInt();  \
    StgStablePtr r;           \
    s;                        \
    PushTaggedStablePtr(r);      \
}
#define OP_AIC_(s)            \
{                             \
    StgAddr x = PopTaggedAddr(); \
    int     y = PopTaggedInt();  \
    StgChar z = PopTaggedChar(); \
    s;                        \
}
#define OP_AII_(s)            \
{                             \
    StgAddr x = PopTaggedAddr(); \
    int     y = PopTaggedInt();  \
    StgInt  z = PopTaggedInt(); \
    s;                        \
}
#define OP_AIz_(s)            \
{                             \
    StgAddr x = PopTaggedAddr(); \
    int     y = PopTaggedInt();  \
    StgInt64 z = PopTaggedInt64(); \
    s;                        \
}
#define OP_AIA_(s)            \
{                             \
    StgAddr x = PopTaggedAddr(); \
    int     y = PopTaggedInt();  \
    StgAddr z = PopTaggedAddr(); \
    s;                        \
}
#define OP_AIF_(s)            \
{                             \
    StgAddr x = PopTaggedAddr(); \
    int     y = PopTaggedInt();  \
    StgFloat z = PopTaggedFloat(); \
    s;                        \
}
#define OP_AID_(s)            \
{                             \
    StgAddr x = PopTaggedAddr(); \
    int     y = PopTaggedInt();  \
    StgDouble z = PopTaggedDouble(); \
    s;                        \
}
#define OP_AIs_(s)            \
{                             \
    StgAddr x = PopTaggedAddr(); \
    int     y = PopTaggedInt();  \
    StgStablePtr z = PopTaggedStablePtr(); \
    s;                        \
}

#endif /* PROVIDE_ADDR */

#define OP_FF_B(e)              \
{                               \
    StgFloat x = PopTaggedFloat(); \
    StgFloat y = PopTaggedFloat(); \
    PushTaggedBool(e);          \
}

#define OP_FF_F(e)              \
{                               \
    StgFloat x = PopTaggedFloat(); \
    StgFloat y = PopTaggedFloat(); \
    PushTaggedFloat(e);         \
}

#define OP_F_F(e)               \
{                               \
    StgFloat x = PopTaggedFloat(); \
    PushTaggedFloat(e);         \
}

#define OP_F_B(e)               \
{                               \
    StgFloat x = PopTaggedFloat(); \
    PushTaggedBool(e);         \
}

#define OP_F_I(e)               \
{                               \
    StgFloat x = PopTaggedFloat(); \
    PushTaggedInt(e);           \
}

#define OP_F_D(e)               \
{                               \
    StgFloat x = PopTaggedFloat(); \
    PushTaggedDouble(e);        \
}

#define OP_DD_B(e)                \
{                                 \
    StgDouble x = PopTaggedDouble(); \
    StgDouble y = PopTaggedDouble(); \
    PushTaggedBool(e);            \
}

#define OP_DD_D(e)                \
{                                 \
    StgDouble x = PopTaggedDouble(); \
    StgDouble y = PopTaggedDouble(); \
    PushTaggedDouble(e);          \
}

#define OP_D_B(e)                 \
{                                 \
    StgDouble x = PopTaggedDouble(); \
    PushTaggedBool(e);          \
}

#define OP_D_D(e)                 \
{                                 \
    StgDouble x = PopTaggedDouble(); \
    PushTaggedDouble(e);          \
}

#define OP_D_I(e)                 \
{                                 \
    StgDouble x = PopTaggedDouble(); \
    PushTaggedInt(e);             \
}

#define OP_D_F(e)                 \
{                                 \
    StgDouble x = PopTaggedDouble(); \
    PushTaggedFloat(e);           \
}

#ifdef PROVIDE_INT64
#define OP_zI_F(e)                     \
{                                      \
    StgInt64 x = PopTaggedInt64(); \
    int        y = PopTaggedInt();     \
    PushTaggedFloat(e);                \
}
#define OP_zI_D(e)                     \
{                                      \
    StgInt64 x = PopTaggedInt64(); \
    int        y = PopTaggedInt();     \
    PushTaggedDouble(e);               \
}
#define OP_zz_I(e)                     \
{                                      \
    StgInt64 x = PopTaggedInt64(); \
    StgInt64 y = PopTaggedInt64(); \
    PushTaggedInt(e);                  \
}
#define OP_z_z(e)                      \
{                                      \
    StgInt64 x = PopTaggedInt64(); \
    PushTaggedInt64(e);              \
}
#define OP_zz_z(e)                     \
{                                      \
    StgInt64 x = PopTaggedInt64(); \
    StgInt64 y = PopTaggedInt64(); \
    PushTaggedInt64(e);              \
}
#define OP_zW_z(e)                     \
{                                      \
    StgInt64 x = PopTaggedInt64(); \
    StgWord  y = PopTaggedWord(); \
    PushTaggedInt64(e);              \
}
#define OP_zz_zZ(e1,e2)                \
{                                      \
    StgInt64 x = PopTaggedInt64(); \
    StgInt64 y = PopTaggedInt64(); \
    PushTaggedInt64(e1);             \
    PushTaggedInt64(e2);             \
}
#define OP_zz_B(e)           \
{                            \
    StgInt64 x = PopTaggedInt64();  \
    StgInt64 y = PopTaggedInt64();  \
    PushTaggedBool(e);       \
}
#define OP__z(e)             \
{                            \
    PushTaggedInt64(e);        \
}
#define OP_z_I(e)                      \
{                                      \
    StgInt64 x = PopTaggedInt64(); \
    PushTaggedInt(e);                  \
}
#define OP_I_z(e)                      \
{                                      \
    StgInt x = PopTaggedInt();            \
    PushTaggedInt64(e);              \
}
#ifdef PROVIDE_WORD
#define OP_z_W(e)                      \
{                                      \
    StgInt64 x = PopTaggedInt64(); \
    PushTaggedWord(e);                 \
}
#define OP_W_z(e)                      \
{                                      \
    StgWord x = PopTaggedWord();          \
    PushTaggedInt64(e);              \
}
#endif
#define OP_z_F(e)                      \
{                                      \
    StgInt64 x = PopTaggedInt64(); \
    printf("%lld = %f\n",x,(float)(e)); \
    PushTaggedFloat(e);                \
}
#define OP_F_z(e)                      \
{                                      \
    StgFloat x = PopTaggedFloat();        \
    PushTaggedInt64(e);              \
}
#define OP_z_D(e)                      \
{                                      \
    StgInt64 x = PopTaggedInt64(); \
    PushTaggedDouble(e);               \
}
#define OP_D_z(e)                      \
{                                      \
    StgDouble x = PopTaggedDouble();      \
    PushTaggedInt64(e);              \
}
#endif

#ifdef PROVIDE_INTEGER

#define OP_ZI_F(e)                     \
{                                      \
    mpz_ptr x = PopTaggedInteger();    \
    int   y = PopTaggedInt();          \
    PushTaggedFloat(e);                \
}
#define OP_F_ZI(s)                     \
{                                      \
    StgFloat x = PopTaggedFloat();     \
    mpz_ptr r1 = mpz_alloc();          \
    StgInt r2;                         \
    s;                                 \
    PushTaggedInt(r2);                 \
    PushTaggedInteger(r1);             \
}
#define OP_ZI_D(e)                     \
{                                      \
    mpz_ptr x = PopTaggedInteger();    \
    int   y = PopTaggedInt();          \
    PushTaggedDouble(e);               \
}
#define OP_D_ZI(s)                     \
{                                      \
    StgDouble x = PopTaggedDouble();   \
    mpz_ptr r1 = mpz_alloc();          \
    StgInt r2;                         \
    s;                                 \
    PushTaggedInt(r2);                 \
    PushTaggedInteger(r1);             \
}
#define OP_Z_Z(s)                      \
{                                      \
    mpz_ptr x = PopTaggedInteger();      \
    mpz_ptr r = mpz_alloc();           \
    s;                                 \
    PushTaggedInteger(r);              \
}
#define OP_ZZ_Z(s)                     \
{                                      \
    mpz_ptr x = PopTaggedInteger();    \
    mpz_ptr y = PopTaggedInteger();    \
    mpz_ptr r = mpz_alloc();           \
    s;                                 \
    PushTaggedInteger(r);              \
}
#define OP_ZZ_B(e)           \
{                            \
    mpz_ptr x = PopTaggedInteger();  \
    mpz_ptr y = PopTaggedInteger();  \
    PushTaggedBool(e);       \
}
#define OP_Z_I(e)                      \
{                                      \
    mpz_ptr x = PopTaggedInteger();      \
    PushTaggedInt(e);                  \
}
#define OP_I_Z(s)                      \
{                                      \
    StgInt x = PopTaggedInt();         \
    mpz_ptr r = mpz_alloc();           \
    s;                                 \
    PushTaggedInteger(r);              \
}
#ifdef PROVIDE_INT64
#define OP_Z_z(e)                      \
{                                      \
    mpz_ptr x = PopTaggedInteger(); \
    PushTaggedInt64(e);                  \
}
#define OP_z_Z(s)                      \
{                                      \
    StgInt64 x = PopTaggedInt64();     \
    mpz_ptr r = mpz_alloc();           \
    s;                                 \
    PushTaggedInteger(r);              \
}
#endif
#ifdef PROVIDE_WORD
#define OP_Z_W(e)                      \
{                                      \
    mpz_ptr x = PopTaggedInteger(); \
    PushTaggedWord(e);                 \
}
#define OP_W_Z(s)                      \
{                                      \
    StgWord x = PopTaggedWord();       \
    mpz_ptr r = mpz_alloc();           \
    s;                                 \
    PushTaggedInteger(r);              \
}
#endif
#define OP_Z_F(e)                      \
{                                      \
    mpz_ptr x = PopTaggedInteger(); \
    PushTaggedFloat(e);                \
}
#define OP_F_Z(s)                      \
{                                      \
    StgFloat x = PopTaggedFloat();        \
    mpz_ptr r = mpz_alloc();           \
    s;                                 \
    PushTaggedInteger(r);              \
}
#define OP_Z_D(e)                      \
{                                      \
    mpz_ptr x = PopTaggedInteger(); \
    PushTaggedDouble(e);               \
}
#define OP_D_Z(s)                      \
{                                      \
    StgDouble x = PopTaggedDouble();      \
    mpz_ptr r = mpz_alloc();           \
    s;                                 \
    PushTaggedInteger(r);              \
}

#endif /* ifdef PROVIDE_INTEGER */

#ifdef PROVIDE_ARRAY
#define HEADER_mI(ty,where)          \
    StgArrWords* x = stgCast(StgArrWords*,PopPtr()); \
    nat i = PopTaggedInt();   \
    if (i * sizeof(ty) + (sizeof(ty)) > sizeof(StgWord) * x->words) {        \
        obj = raiseIndex(where);  \
        goto enterLoop;           \
    }                             
#define OP_mI_ty(ty,where,s)        \
{                                   \
    HEADER_mI(mycat2(Stg,ty),where) \
    { mycat2(Stg,ty) r;             \
      s;                            \
      mycat2(PushTagged,ty)(r);     \
    }                               \
}
#define OP_mIty_(ty,where,s)        \
{                                   \
    HEADER_mI(mycat2(Stg,ty),where) \
    {                               \
      mycat2(Stg,ty) z = mycat2(PopTagged,ty)(); \
      s;                            \
    }                               \
}

#endif /* PROVIDE_ARRAY */


/* This is written as one giant function in the hope that gcc will do
 * a better job of register allocation.
 */
StgThreadReturnCode enter( StgClosure* obj )
{
    /* We use a char so that we'll do a context_switch check every 256
     * iterations.
     */
    char enterCount = 0;
enterLoop:
    /* ASSERT(StorageMgrInfo.hp_start <= Hp && Hp < HpLim && HpLim == StorageMgrInfo.hplim); */
    ASSERT(SpLim <= Sp && Sp <= stgCast(StgPtr,Su));
#if 0
    IF_DEBUG(evaluator,
             fprintf(stderr,"Sp = %p\tSu = %p\n", Sp, Su);
             printStack(Sp,CurrentTSO->stack+CurrentTSO->stack_size,Su);
             fprintf(stderr,"Entering: "); printObj(obj);
    );
#endif
#if 0
    IF_DEBUG(sanity,
             {
                 /*belch("Starting sanity check");
                  *SaveThreadState();
                  *checkTSO(CurrentTSO, heap_step);
                  * This check fails if we've done any updates because we
                  * whack into holes in the heap.
                  *checkHeap(?,?);
                  *belch("Ending sanity check");
	          */
             }
             );
#endif
#if 0
    IF_DEBUG(evaluator,
             fprintf(stderr,"Continue?\n");
             getchar()
             );
#endif
    if (++enterCount == 0 && context_switch) {
        PushCPtr(obj); /* code to restart with */
        return ThreadYielding;
    }
    switch ( get_itbl(obj)->type ) {
    case INVALID_OBJECT:
            barf("Invalid object %p",obj);
    case BCO:
        {
            StgBCO* bco = stgCast(StgBCO*,obj);
            InstrPtr pc = 0;
#if 1  /* We don't use an explicit HP_CHECK anymore */
            if (doYouWantToGC()) {
                PushCPtr(obj); /* code to restart with */
                return HeapOverflow;
            }
#endif
            while (1) {
                ASSERT(pc < bco->n_instrs);
                IF_DEBUG(evaluator,
                         fprintf(stderr,"Sp = %p\tSu = %p\tpc = %d\t", Sp, Su, pc);
                         disInstr(bco,pc);
                         /*fprintf(stderr,"\t"); printStackObj(Sp); */
                         fprintf(stderr,"\n");
                         );
                switch (bcoInstr(bco,pc++)) {
                case i_INTERNAL_ERROR:
                        barf("INTERNAL_ERROR at %p:%d",bco,pc-1);
                case i_PANIC:
                        barf("PANIC at %p:%d",bco,pc-1); 
#if 0
                case i_HP_CHECK:
                    {
                        int n = bcoInstr(bco,pc++);
                        /* ToDo: we could allocate the whole thing now and
                         * slice it up ourselves
	                 */
                        if (doYouWantToGC()) {
                            PushCPtr(obj); /* code to restart with */
                            return HeapOverflow;
                        }
                        break;
                    }
#endif
                case i_STK_CHECK:
                    {
                        int n = bcoInstr(bco,pc++);
                        if (Sp - n < SpLim) {
                            PushCPtr(obj); /* code to restart with */
                            return StackOverflow;
                        }
                        break;
                    }
                case i_ARG_CHECK:
                    {
                        /* ToDo: make sure that hp check allows for possible PAP */
                        nat n = bcoInstr(bco,pc++);
                        if (stgCast(StgPtr,Sp + n) > stgCast(StgPtr,Su)) {
                            StgWord words = (P_)Su - Sp;
                            
                            /* first build a PAP */
                            ASSERT((P_)Su >= Sp);  /* was (words >= 0) but that's always true */
                            if (words == 0) { /* optimisation */
                                /* Skip building the PAP and update with an indirection. */
                            } else { /* Build the PAP. */
                                /* In the evaluator, we avoid the need to do 
                                 * a heap check here by including the size of
                                 * the PAP in the heap check we performed
                                 * when we entered the BCO.
	                         */
                                StgInt  i;
                                StgPAP* pap = stgCast(StgPAP*,grabHpNonUpd(PAP_sizeW(words)));
                                SET_HDR(pap,&PAP_info,CC_pap);
                                pap->n_args = words;
                                pap->fun = obj;
                                for(i = 0; i < (I_)words; ++i) {
                                    payloadWord(pap,i) = Sp[i];
                                }
                                Sp += words;
                                obj = stgCast(StgClosure*,pap);
                            }

                            /* now deal with "update frame" */
                            /* as an optimisation, we process all on top of stack instead of just the top one */
                            ASSERT(Sp==(P_)Su);
                            do {
                                switch (get_itbl(Su)->type) {
                                case CATCH_FRAME:
                                        PopCatchFrame();
                                        break;
                                case UPDATE_FRAME:
                                        PopUpdateFrame(obj);
                                        break;
                                case STOP_FRAME:
                                        PopStopFrame(obj);
                                        return ThreadFinished;
                                case SEQ_FRAME:
                                        PopSeqFrame();
                                        break;
                                default:        
                                        barf("Invalid update frame during argcheck");
                                }
                            } while (Sp==(P_)Su);
	                    goto enterLoop;
                        }
                        break;
                    }
                case i_ALLOC_AP:
                    {
                        int words = bcoInstr(bco,pc++);
                        PushPtr(grabHpUpd(AP_sizeW(words)));
                        break;
                    }
                case i_ALLOC_CONSTR:
                    {
                        StgInfoTable* info = bcoConstAddr(bco,bcoInstr(bco,pc++));
                        StgClosure* c = stgCast(StgClosure*,grabHpNonUpd(sizeW_fromITBL(info)));
                        SET_HDR(c,info,??);
                        PushPtr(stgCast(StgPtr,c));
                        break;
                    }
                case i_MKAP:
                    {
                        int x = bcoInstr(bco,pc++);  /* ToDo: Word not Int! */
                        int y = bcoInstr(bco,pc++);
                        StgAP_UPD* o = stgCast(StgAP_UPD*,stackPtr(x));
                        SET_HDR(o,&AP_UPD_info,??);
                        o->n_args = y;
                        o->fun    = stgCast(StgClosure*,PopPtr());
                        for(x=0; x < y; ++x) {
                            payloadWord(o,x) = PopWord();
                        }
                        IF_DEBUG(evaluator,
                                 fprintf(stderr,"\tBuilt "); 
                                 printObj(stgCast(StgClosure*,o));
                        );
                        break;
                    }
                case i_MKPAP:
                    {
                        int x = bcoInstr(bco,pc++);
                        int y = bcoInstr(bco,pc++);
                        StgPAP* o = stgCast(StgPAP*,stackPtr(x));
                        SET_HDR(o,&PAP_info,??);
                        o->n_args = y;
                        o->fun    = stgCast(StgClosure*,PopPtr());
                        for(x=0; x < y; ++x) {
                            payloadWord(o,x) = PopWord();
                        }
                        IF_DEBUG(evaluator,
                                 fprintf(stderr,"\tBuilt "); 
                                 printObj(stgCast(StgClosure*,o));
                                 );
                        break;
                    }
                case i_PACK:
                    {
                        int offset = bcoInstr(bco,pc++);
                        StgClosure* o = stgCast(StgClosure*,stackPtr(offset));
                        const StgInfoTable* info = get_itbl(o);
                        nat p  = info->layout.payload.ptrs; 
                        nat np = info->layout.payload.nptrs; 
                        nat i;
                        for(i=0; i < p; ++i) {
                            payloadCPtr(o,i) = PopCPtr();
                        }
                        for(i=0; i < np; ++i) {
                            payloadWord(o,p+i) = 0xdeadbeef;
                        }
                        IF_DEBUG(evaluator,
                                 fprintf(stderr,"\tBuilt "); 
                                 printObj(stgCast(StgClosure*,o));
                                 );
                        break;
                    }
                case i_SLIDE:
                    {
                        int x = bcoInstr(bco,pc++);
                        int y = bcoInstr(bco,pc++);
                        ASSERT(Sp+x+y <= stgCast(StgPtr,Su));
                        /* a_1, .. a_x, b_1, .. b_y, s => a_1, .. a_x, s */
                        while(--x >= 0) {
                            setStackWord(x+y,stackWord(x));
                        }
                        Sp += y;
                        break;
                    }
                case i_ENTER:
                    {
                        obj = PopCPtr();
                        goto enterLoop;
                    }
                case i_RETADDR:
                    {
                        PushPtr(bcoConstPtr(bco,bcoInstr(bco,pc++)));
                        PushPtr(stgCast(StgPtr,&ret_bco_info));
                        break;
                    }
                case i_TEST:
                    {
                        int  tag       = bcoInstr(bco,pc++);
                        StgWord offset = bcoInstr(bco,pc++);
                        if (constrTag(stgCast(StgClosure*,stackPtr(0))) != tag) {
                            pc += offset;
                        }
                        break;
                    }
                case i_UNPACK:
                    {
                        StgClosure* o = stgCast(StgClosure*,stackPtr(0));
                        const StgInfoTable* itbl = get_itbl(o);
                        int i = itbl->layout.payload.ptrs;
                        ASSERT(  itbl->type == CONSTR
                              || itbl->type == CONSTR_STATIC
                              || itbl->type == CONSTR_NOCAF_STATIC
                              );
                        while (--i>=0) {
                            PushCPtr(payloadCPtr(o,i));
                        }
                        break;
                    }
                case i_VAR:
                    {
                        PushPtr(stackPtr(bcoInstr(bco,pc++)));
                        break;
                    }
                case i_CONST:
                    {
                        PushPtr(stgCast(StgPtr,bcoConstPtr(bco,bcoInstr(bco,pc++))));
                        break;
                    }
                case i_CONST2:
                    {
                        StgWord o1 = bcoInstr(bco,pc++);
                        StgWord o2 = bcoInstr(bco,pc++);
                        StgWord o  = o1*256 + o2;
                        PushPtr(stgCast(StgPtr,bcoConstPtr(bco,o)));
                        break;
                    }
                case i_VOID:
                    {
                        PushTaggedRealWorld();
                        break;
                    }
                case i_VAR_INT:
                    {
                        PushTaggedInt(taggedStackInt(bcoInstr(bco,pc++)));
                        break;
                    }
                case i_CONST_INT:
                    {
                        PushTaggedInt(bcoConstInt(bco,bcoInstr(bco,pc++)));
                        break;
                    }
                case i_RETURN_INT:
                    {
                        ASSERT(0);
                        break;
                    }
                case i_PACK_INT:
                    {
                        StgClosure* o = stgCast(StgClosure*,grabHpNonUpd(IZh_sizeW));
                        SET_HDR(o,&IZh_con_info,??);
                        payloadWord(o,0) = PopTaggedInt();
                        IF_DEBUG(evaluator,
                                 fprintf(stderr,"\tBuilt "); 
                                 printObj(stgCast(StgClosure*,o));
                                 );
                        PushPtr(stgCast(StgPtr,o));
                        break;
                    }
                case i_UNPACK_INT:
                    {
                        StgClosure* con = stgCast(StgClosure*,stackPtr(0));
                        /* ASSERT(isIntLike(con)); */
                        PushTaggedInt(payloadWord(con,0));
                        break;
                    }
                case i_TEST_INT:
                    {
                        StgWord offset = bcoInstr(bco,pc++);
                        StgInt  x      = PopTaggedInt();
                        StgInt  y      = PopTaggedInt();
                        if (x != y) {
                            pc += offset;
                        }
                        break;
                    }
#ifdef PROVIDE_INT64
                case i_VAR_INT64:
                    {
                        PushTaggedInt64(taggedStackInt64(bcoInstr(bco,pc++)));
                        break;
                    }
                case i_CONST_INT64:
                    {
                        PushTaggedInt64(bcoConstInt64(bco,bcoInstr(bco,pc++)));
                        break;
                    }
                case i_RETURN_INT64:
                    {
                        ASSERT(0); /* ToDo(); */
                        break;
                    }
                case i_PACK_INT64:
                    {
                        StgClosure* o = stgCast(StgClosure*,grabHpNonUpd(I64Zh_sizeW));
                        SET_HDR(o,&I64Zh_con_info,??);
                        ASSIGN_Int64(&payloadWord(o,0),PopTaggedInt64());
                        IF_DEBUG(evaluator,
                                 fprintf(stderr,"\tBuilt "); 
                                 printObj(stgCast(StgClosure*,o));
                                 );
                        PushPtr(stgCast(StgPtr,o));
                        break;
                    }
                case i_UNPACK_INT64:
                    {
                        StgClosure* con = stgCast(StgClosure*,stackPtr(0));
                        /*ASSERT(isInt64Like(con)); */
                        PushTaggedInt64(PK_Int64(&payloadWord(con,0)));
                        break;
                    }
#endif
#ifdef PROVIDE_INTEGER
                case i_CONST_INTEGER:
                    {
                        char* s = bcoConstAddr(bco,bcoInstr(bco,pc++));
                        mpz_ptr r = mpz_alloc();
                        if (s[0] == '0' && s[1] == 'x') {
                            mpz_set_str(r,s+2,16);
                        } else {
                            mpz_set_str(r,s,10);
                        }
                        PushTaggedInteger(r);
                        break;
                    }
#endif

#ifdef PROVIDE_WORD
                case i_VAR_WORD:
                    {
                        PushTaggedWord(taggedStackWord(bcoInstr(bco,pc++)));
                        break;
                    }
                case i_CONST_WORD:
                    {
                        PushTaggedWord(bcoConstWord(bco,bcoInstr(bco,pc++)));
                        break;
                    }
                case i_RETURN_WORD:
                    {
                        ASSERT(0);
                        break;
                    }
                case i_PACK_WORD:
                    {
                        StgClosure* o = stgCast(StgClosure*,grabHpNonUpd(WZh_sizeW));

                        SET_HDR(o,&WZh_con_info,??);
                        payloadWord(o,0) = PopTaggedWord();
                        IF_DEBUG(evaluator,
                                 fprintf(stderr,"\tBuilt "); 
                                 printObj(stgCast(StgClosure*,o));
                                 );
                        PushPtr(stgCast(StgPtr,o));
                        break;
                    }
                case i_UNPACK_WORD:
                    {
                        StgClosure* con = stgCast(StgClosure*,stackPtr(0));
                        /* ASSERT(isWordLike(con)); */
                        PushTaggedWord(payloadWord(con,0));
                        break;
                    }
#endif
#ifdef PROVIDE_ADDR
                case i_VAR_ADDR:
                    {
                        PushTaggedAddr(taggedStackAddr(bcoInstr(bco,pc++)));
                        break;
                    }
                case i_CONST_ADDR:
                    {
                        PushTaggedAddr(bcoConstAddr(bco,bcoInstr(bco,pc++)));
                        break;
                    }
                case i_RETURN_ADDR:
                    {
                        ASSERT(0);
                        break;
                    }
                case i_PACK_ADDR:
                    {
                        StgClosure* o = stgCast(StgClosure*,grabHpNonUpd(AZh_sizeW));
                        SET_HDR(o,&AZh_con_info,??);
                        payloadPtr(o,0) = PopTaggedAddr();
                        IF_DEBUG(evaluator,
                                 fprintf(stderr,"\tBuilt "); 
                                 printObj(stgCast(StgClosure*,o));
                                 );
                        PushPtr(stgCast(StgPtr,o));
                        break;
                    }
                case i_UNPACK_ADDR:
                    {
                        StgClosure* con = stgCast(StgClosure*,stackPtr(0));
                        /* ASSERT(isAddrLike(con)); */
                        PushTaggedAddr(payloadPtr(con,0));
                        break;
                    }
#endif
                case i_VAR_CHAR:
                    {
                        PushTaggedChar(taggedStackChar(bcoInstr(bco,pc++)));
                        break;
                    }
                case i_CONST_CHAR:
                    {
                        PushTaggedChar(bcoConstChar(bco,bcoInstr(bco,pc++)));
                        break;
                    }
                case i_RETURN_CHAR:
                    {
                        ASSERT(0);
                        break;
                    }
                case i_PACK_CHAR:
                    {
                        StgClosure* o = stgCast(StgClosure*,grabHpNonUpd(CZh_sizeW));
                        SET_HDR(o,&CZh_con_info,??);
                        payloadWord(o,0) = PopTaggedChar();
                        PushPtr(stgCast(StgPtr,o));
                        IF_DEBUG(evaluator,
                                 fprintf(stderr,"\tBuilt "); 
                                 printObj(stgCast(StgClosure*,o));
                                 );
                        break;
                    }
                case i_UNPACK_CHAR:
                    {
                        StgClosure* con = stgCast(StgClosure*,stackPtr(0));
                        /* ASSERT(isCharLike(con)); */
                        PushTaggedChar(payloadWord(con,0));
                        break;
                    }
                case i_VAR_FLOAT:
                    {
                        PushTaggedFloat(taggedStackFloat(bcoInstr(bco,pc++)));
                        break;
                    }
                case i_CONST_FLOAT:
                    {
                        PushTaggedFloat(bcoConstFloat(bco,bcoInstr(bco,pc++)));
                        break;
                    }
                case i_RETURN_FLOAT:
                    {
                        ASSERT(0);
                        break;
                    }
                case i_PACK_FLOAT:
                    {
                        StgClosure* o = stgCast(StgClosure*,grabHpNonUpd(FZh_sizeW));
                        SET_HDR(o,&FZh_con_info,??);
                        ASSIGN_FLT(&payloadWord(o,0),PopTaggedFloat());
                        IF_DEBUG(evaluator,
                                 fprintf(stderr,"\tBuilt "); 
                                 printObj(stgCast(StgClosure*,o));
                                 );
                        PushPtr(stgCast(StgPtr,o));
                        break;
                    }
                case i_UNPACK_FLOAT:
                    {
                        StgClosure* con = stgCast(StgClosure*,stackPtr(0));
                        /* ASSERT(isFloatLike(con)); */
                        PushTaggedFloat(PK_FLT(&payloadWord(con,0)));
                        break;
                    }
                case i_VAR_DOUBLE:
                    {
                        PushTaggedDouble(taggedStackDouble(bcoInstr(bco,pc++)));
                        break;
                    }
                case i_CONST_DOUBLE:
                    {
                        PushTaggedDouble(bcoConstDouble(bco,bcoInstr(bco,pc++)));
                        break;
                    }
                case i_RETURN_DOUBLE:
                    {
                        ASSERT(0);
                        break;
                    }
                case i_PACK_DOUBLE:
                    {
                        StgClosure* o = stgCast(StgClosure*,grabHpNonUpd(DZh_sizeW));
                        SET_HDR(o,&DZh_con_info,??);
                        ASSIGN_DBL(&payloadWord(o,0),PopTaggedDouble());
                        IF_DEBUG(evaluator,
                                 fprintf(stderr,"\tBuilt "); 
                                 printObj(stgCast(StgClosure*,o));
                                 );
                        PushPtr(stgCast(StgPtr,o));
                        break;
                    }
                case i_UNPACK_DOUBLE:
                    {
                        StgClosure* con = stgCast(StgClosure*,stackPtr(0));
                        /* ASSERT(isDoubleLike(con)); */
                        PushTaggedDouble(PK_DBL(&payloadWord(con,0)));
                        break;
                    }
#ifdef PROVIDE_STABLE
                case i_VAR_STABLE:
                    {
                        PushTaggedStablePtr(taggedStackStable(bcoInstr(bco,pc++)));
                        break;
                    }
                case i_RETURN_STABLE:
                    {
                        ASSERT(0);
                        break;
                    }
                case i_PACK_STABLE:
                    {
                        StgClosure* o = stgCast(StgClosure*,grabHpNonUpd(StableZh_sizeW));
                        SET_HDR(o,&StablePtr_con_info,??);
                        payloadWord(o,0) = PopTaggedStablePtr();
                        IF_DEBUG(evaluator,
                                 fprintf(stderr,"\tBuilt "); 
                                 printObj(stgCast(StgClosure*,o));
                                 );
                        PushPtr(stgCast(StgPtr,o));
                        break;
                    }
                case i_UNPACK_STABLE:
                    {
                        StgClosure* con = stgCast(StgClosure*,stackPtr(0));
                        /* ASSERT(isStableLike(con)); */
                        PushTaggedStablePtr(payloadWord(con,0));
                        break;
                    }
#endif
                case i_PRIMOP1:
                    {
                        switch (bcoInstr(bco,pc++)) {
                        case i_INTERNAL_ERROR1:
                                barf("INTERNAL_ERROR1 at %p:%d",bco,pc-1);

                        case i_gtChar:          OP_CC_B(x>y);        break;
                        case i_geChar:          OP_CC_B(x>=y);       break;
                        case i_eqChar:          OP_CC_B(x==y);       break;
                        case i_neChar:          OP_CC_B(x!=y);       break;
                        case i_ltChar:          OP_CC_B(x<y);        break;
                        case i_leChar:          OP_CC_B(x<=y);       break;
                        case i_charToInt:       OP_C_I(x);           break;
                        case i_intToChar:       OP_I_C(x);           break;

                        case i_gtInt:           OP_II_B(x>y);        break;
                        case i_geInt:           OP_II_B(x>=y);       break;
                        case i_eqInt:           OP_II_B(x==y);       break;
                        case i_neInt:           OP_II_B(x!=y);       break;
                        case i_ltInt:           OP_II_B(x<y);        break;
                        case i_leInt:           OP_II_B(x<=y);       break;
                        case i_minInt:          OP__I(INT_MIN);      break;
                        case i_maxInt:          OP__I(INT_MAX);      break;
                        case i_plusInt:         OP_II_I(x+y);        break;
                        case i_minusInt:        OP_II_I(x-y);        break;
                        case i_timesInt:        OP_II_I(x*y);        break;
                        case i_quotInt:
                            {
                                int x = PopTaggedInt();
                                int y = PopTaggedInt();
                                if (y == 0) {
                                    obj = raiseDiv0("quotInt");
                                    goto enterLoop;
                                }
                                /* ToDo: protect against minInt / -1 errors
                                 * (repeat for all other division primops)
				 */
                                PushTaggedInt(x/y);
                            }
                            break;
                        case i_remInt:
                            {
                                int x = PopTaggedInt();
                                int y = PopTaggedInt();
                                if (y == 0) {
                                    obj = raiseDiv0("remInt");
                                    goto enterLoop;
                                }
                                PushTaggedInt(x%y);
                            }
                            break;
                        case i_quotRemInt:
                            {
                                StgInt x = PopTaggedInt();
                                StgInt y = PopTaggedInt();
                                if (y == 0) {
                                    obj = raiseDiv0("quotRemInt");
                                    goto enterLoop;
                                }
                                PushTaggedInt(x%y); /* last result  */
                                PushTaggedInt(x/y); /* first result */
                            }
                            break;
                        case i_negateInt:       OP_I_I(-x);          break;

                        case i_andInt:          OP_II_I(x&y);        break;
                        case i_orInt:           OP_II_I(x|y);        break;
                        case i_xorInt:          OP_II_I(x^y);        break;
                        case i_notInt:          OP_I_I(~x);          break;
                        case i_shiftLInt:       OP_IW_I(x<<y);       break;
                        case i_shiftRAInt:      OP_IW_I(x>>y);       break; /* ToDo */
                        case i_shiftRLInt:      OP_IW_I(x>>y);       break; /* ToDo */

#ifdef PROVIDE_INT64
                        case i_gtInt64:         OP_zz_B(x>y);        break;
                        case i_geInt64:         OP_zz_B(x>=y);       break;
                        case i_eqInt64:         OP_zz_B(x==y);       break;
                        case i_neInt64:         OP_zz_B(x!=y);       break;
                        case i_ltInt64:         OP_zz_B(x<y);        break;
                        case i_leInt64:         OP_zz_B(x<=y);       break;
                        case i_minInt64:        OP__z(0x800000000000LL); break;
                        case i_maxInt64:        OP__z(0x7fffffffffffLL); break;
                        case i_plusInt64:       OP_zz_z(x+y);        break;
                        case i_minusInt64:      OP_zz_z(x-y);        break;
                        case i_timesInt64:      OP_zz_z(x*y);        break;
                        case i_quotInt64:
                            {
                                StgInt64 x = PopTaggedInt64();
                                StgInt64 y = PopTaggedInt64();
                                if (y == 0) {
                                    obj = raiseDiv0("quotInt64");
                                    goto enterLoop;
                                }
                                /* ToDo: protect against minInt64 / -1 errors
                                 * (repeat for all other division primops)
				 */
                                PushTaggedInt64(x/y);
                            }
                            break;
                        case i_remInt64:
                            {
                                StgInt64 x = PopTaggedInt64();
                                StgInt64 y = PopTaggedInt64();
                                if (y == 0) {
                                    obj = raiseDiv0("remInt64");
                                    goto enterLoop;
                                }
                                PushTaggedInt64(x%y);
                            }
                            break;
                        case i_quotRemInt64:
                            {
                                StgInt64 x = PopTaggedInt64();
                                StgInt64 y = PopTaggedInt64();
                                if (y == 0) {
                                    obj = raiseDiv0("quotRemInt64");
                                    goto enterLoop;
                                }
                                PushTaggedInt64(x%y); /* last result  */
                                PushTaggedInt64(x/y); /* first result */
                            }
                            break;
                        case i_negateInt64:     OP_z_z(-x);          break;

                        case i_andInt64:        OP_zz_z(x&y);        break;
                        case i_orInt64:         OP_zz_z(x|y);        break;
                        case i_xorInt64:        OP_zz_z(x^y);        break;
                        case i_notInt64:        OP_z_z(~x);          break;
                        case i_shiftLInt64:     OP_zW_z(x<<y);       break;
                        case i_shiftRAInt64:    OP_zW_z(x>>y);       break; /* ToDo */
                        case i_shiftRLInt64:    OP_zW_z(x>>y);       break; /* ToDo */

                        case i_int64ToInt:      OP_z_I(x);           break;
                        case i_intToInt64:      OP_I_z(x);           break;
#ifdef PROVIDE_WORD
                        case i_int64ToWord:     OP_z_W(x);           break;
                        case i_wordToInt64:     OP_W_z(x);           break;
#endif
                        case i_int64ToFloat:    OP_z_F(x);           break;
                        case i_floatToInt64:    OP_F_z(x);           break;
                        case i_int64ToDouble:   OP_z_D(x);           break;
                        case i_doubleToInt64:   OP_D_z(x);           break;
#endif
#ifdef PROVIDE_WORD
                        case i_gtWord:          OP_WW_B(x>y);        break;
                        case i_geWord:          OP_WW_B(x>=y);       break;
                        case i_eqWord:          OP_WW_B(x==y);       break;
                        case i_neWord:          OP_WW_B(x!=y);       break;
                        case i_ltWord:          OP_WW_B(x<y);        break;
                        case i_leWord:          OP_WW_B(x<=y);       break;
                        case i_minWord:         OP__W(0);            break;
                        case i_maxWord:         OP__W(UINT_MAX);     break;
                        case i_plusWord:        OP_WW_W(x+y);        break;
                        case i_minusWord:       OP_WW_W(x-y);        break;
                        case i_timesWord:       OP_WW_W(x*y);        break;
                        case i_quotWord:
                            {
                                StgWord x = PopTaggedWord();
                                StgWord y = PopTaggedWord();
                                if (y == 0) {
                                    obj = raiseDiv0("quotWord");
                                    goto enterLoop;
                                }
                                PushTaggedWord(x/y);
                            }
                            break;
                        case i_remWord:
                            {
                                StgWord x = PopTaggedWord();
                                StgWord y = PopTaggedWord();
                                if (y == 0) {
                                    obj = raiseDiv0("remWord");
                                    goto enterLoop;
                                }
                                PushTaggedWord(x%y);
                            }
                            break;
                        case i_quotRemWord:
                            {
                                StgWord x = PopTaggedWord();
                                StgWord y = PopTaggedWord();
                                if (y == 0) {
                                    obj = raiseDiv0("quotRemWord");
                                    goto enterLoop;
                                }
                                PushTaggedWord(x%y); /* last result  */
                                PushTaggedWord(x/y); /* first result */
                            }
                            break;
                        case i_negateWord:      OP_W_W(-x);         break;
                        case i_andWord:         OP_WW_W(x&y);        break;
                        case i_orWord:          OP_WW_W(x|y);        break;
                        case i_xorWord:         OP_WW_W(x^y);        break;
                        case i_notWord:         OP_W_W(~x);          break;
                        case i_shiftLWord:      OP_WW_W(x<<y);       break;
                        case i_shiftRAWord:     OP_WW_W(x>>y);       break; /* ToDo */
                        case i_shiftRLWord:     OP_WW_W(x>>y);       break; /* ToDo */
                        case i_intToWord:       OP_I_W(x);           break;
                        case i_wordToInt:       OP_W_I(x);           break;
#endif
#ifdef PROVIDE_ADDR
                        case i_gtAddr:          OP_AA_B(x>y);        break;
                        case i_geAddr:          OP_AA_B(x>=y);       break;
                        case i_eqAddr:          OP_AA_B(x==y);       break;
                        case i_neAddr:          OP_AA_B(x!=y);       break;
                        case i_ltAddr:          OP_AA_B(x<y);        break;
                        case i_leAddr:          OP_AA_B(x<=y);       break;
                        case i_intToAddr:       OP_I_A((StgAddr)x);  break;  /*  ToDo */
                        case i_addrToInt:       OP_A_I((StgInt)x);   break;  /* ToDo */

                        case i_indexCharOffAddr:   OP_AI_C(indexCharOffAddrZh(r,x,y));      break;
                        case i_readCharOffAddr:    OP_AI_C(indexCharOffAddrZh(r,x,y));      break;
                        case i_writeCharOffAddr:   OP_AIC_(writeCharOffAddrZh(x,y,z));      break;
										            
                        case i_indexIntOffAddr:    OP_AI_I(indexIntOffAddrZh(r,x,y));       break;
                        case i_readIntOffAddr:     OP_AI_I(indexIntOffAddrZh(r,x,y));       break;
                        case i_writeIntOffAddr:    OP_AII_(writeIntOffAddrZh(x,y,z));       break;
#ifdef PROVIDE_INT64									    
                        case i_indexInt64OffAddr:  OP_AI_z(indexInt64OffAddrZh(r,x,y));     break;
                        case i_readInt64OffAddr:   OP_AI_z(indexInt64OffAddrZh(r,x,y));     break;
                        case i_writeInt64OffAddr:  OP_AIz_(writeInt64OffAddrZh(x,y,z));     break;
#endif											    
											    
                        case i_indexAddrOffAddr:   OP_AI_A(indexAddrOffAddrZh(r,x,y));      break;
                        case i_readAddrOffAddr:    OP_AI_A(indexAddrOffAddrZh(r,x,y));      break;
                        case i_writeAddrOffAddr:   OP_AIA_(writeAddrOffAddrZh(x,y,z));      break;
											    
                        case i_indexFloatOffAddr:  OP_AI_F(indexFloatOffAddrZh(r,x,y));     break;
                        case i_readFloatOffAddr:   OP_AI_F(indexFloatOffAddrZh(r,x,y));     break;
                        case i_writeFloatOffAddr:  OP_AIF_(writeFloatOffAddrZh(x,y,z));     break;
											   
                        case i_indexDoubleOffAddr: OP_AI_D(indexDoubleOffAddrZh(r,x,y));    break;
                        case i_readDoubleOffAddr:  OP_AI_D(indexDoubleOffAddrZh(r,x,y));    break;
                        case i_writeDoubleOffAddr: OP_AID_(writeDoubleOffAddrZh(x,y,z));    break;

#ifdef PROVIDE_STABLE
                        case i_indexStableOffAddr: OP_AI_s(indexStablePtrOffAddrZh(r,x,y)); break;
                        case i_readStableOffAddr:  OP_AI_s(indexStablePtrOffAddrZh(r,x,y)); break;
                        case i_writeStableOffAddr: OP_AIs_(writeStablePtrOffAddrZh(x,y,z)); break;
#endif

#endif /* PROVIDE_ADDR */

#ifdef PROVIDE_INTEGER
                        case i_compareInteger:     
                            {
                                mpz_ptr x = PopTaggedInteger();
                                mpz_ptr y = PopTaggedInteger();
                                StgInt r = mpz_cmp(x,y);
                                PushTaggedInt(r<0 ? -1 : (r>0 ? 1 : 0));
                            }
                            break;
                        case i_negateInteger:      OP_Z_Z(mpz_neg(r,x));       break;
                        case i_plusInteger:        OP_ZZ_Z(mpz_add(r,x,y));    break;
                        case i_minusInteger:       OP_ZZ_Z(mpz_sub(r,x,y));    break;
                        case i_timesInteger:       OP_ZZ_Z(mpz_mul(r,x,y));    break;
                        case i_quotRemInteger:
                            {
                                mpz_ptr x = PopTaggedInteger();
                                mpz_ptr y = PopTaggedInteger();
                                mpz_ptr q = mpz_alloc();
                                mpz_ptr r = mpz_alloc();
                                if (mpz_sgn(y) == 0) {
                                    obj = raiseDiv0("quotRemInteger");
                                    goto enterLoop;
                                }
                                mpz_tdiv_qr(q,r,x,y);
                                PushTaggedInteger(r); /* last result  */
                                PushTaggedInteger(q); /* first result */
                            }
                            break;
                        case i_divModInteger:
                            {
                                mpz_ptr x = PopTaggedInteger();
                                mpz_ptr y = PopTaggedInteger();
                                mpz_ptr q = mpz_alloc();
                                mpz_ptr r = mpz_alloc();
                                if (mpz_sgn(y) == 0) {
                                    obj = raiseDiv0("divModInteger");
                                    goto enterLoop;
                                }
                                mpz_fdiv_qr(q,r,x,y);
                                PushTaggedInteger(r); /* last result  */
                                PushTaggedInteger(q); /* first result */
                            }
                            break;
                        case i_integerToInt:       OP_Z_I(mpz_get_si(x));   break;
                        case i_intToInteger:       OP_I_Z(mpz_set_si(r,x)); break;
#ifdef PROVIDE_INT64
                        case i_integerToInt64:     OP_Z_z(mpz_get_si(x));   break;
                        case i_int64ToInteger:     OP_z_Z(mpz_set_si(r,x)); break;
#endif
#ifdef PROVIDE_WORD
                        /* NB Use of mpz_get_si is quite deliberate since otherwise
                         * -255 is converted to 255.
	                 */
                        case i_integerToWord:      OP_Z_W(mpz_get_si(x));   break;
                        case i_wordToInteger:      OP_W_Z(mpz_set_ui(r,x)); break;
#endif
                        case i_integerToFloat:     OP_Z_F(mpz_get_d(x));    break;
                        case i_floatToInteger:     OP_F_Z(mpz_set_d(r,x));  break;
                        case i_integerToDouble:    OP_Z_D(mpz_get_d(x));    break;
                        case i_doubleToInteger:    OP_D_Z(mpz_set_d(r,x));  break;
#endif /* PROVIDE_INTEGER */

                        case i_gtFloat:         OP_FF_B(x>y);        break;
                        case i_geFloat:         OP_FF_B(x>=y);       break;
                        case i_eqFloat:         OP_FF_B(x==y);       break;
                        case i_neFloat:         OP_FF_B(x!=y);       break;
                        case i_ltFloat:         OP_FF_B(x<y);        break;
                        case i_leFloat:         OP_FF_B(x<=y);       break;
                        case i_minFloat:        OP__F(FLT_MIN);      break;
                        case i_maxFloat:        OP__F(FLT_MAX);      break;
                        case i_radixFloat:      OP__I(FLT_RADIX);    break;
                        case i_digitsFloat:     OP__I(FLT_MANT_DIG); break;
                        case i_minExpFloat:     OP__I(FLT_MIN_EXP);  break;
                        case i_maxExpFloat:     OP__I(FLT_MAX_EXP);  break;
                        case i_plusFloat:       OP_FF_F(x+y);        break;
                        case i_minusFloat:      OP_FF_F(x-y);        break;
                        case i_timesFloat:      OP_FF_F(x*y);        break;
                        case i_divideFloat:
                            {
                                StgFloat x = PopTaggedFloat();
                                StgFloat y = PopTaggedFloat();
#if 0
                                if (y == 0) {
                                    obj = raiseDiv0("divideFloat");
                                    goto enterLoop;
                                }
#endif
                                PushTaggedFloat(x/y);
                            }
                            break;
                        case i_negateFloat:     OP_F_F(-x);          break;
                        case i_floatToInt:      OP_F_I(x);           break;
                        case i_intToFloat:      OP_I_F(x);           break;
                        case i_expFloat:        OP_F_F(exp(x));      break;
                        case i_logFloat:        OP_F_F(log(x));      break;
                        case i_sqrtFloat:       OP_F_F(sqrt(x));     break;
                        case i_sinFloat:        OP_F_F(sin(x));      break;
                        case i_cosFloat:        OP_F_F(cos(x));      break;
                        case i_tanFloat:        OP_F_F(tan(x));      break;
                        case i_asinFloat:       OP_F_F(asin(x));     break;
                        case i_acosFloat:       OP_F_F(acos(x));     break;
                        case i_atanFloat:       OP_F_F(atan(x));     break;
                        case i_sinhFloat:       OP_F_F(sinh(x));     break;
                        case i_coshFloat:       OP_F_F(cosh(x));     break;
                        case i_tanhFloat:       OP_F_F(tanh(x));     break;
                        case i_powerFloat:      OP_FF_F(pow(x,y));   break;

#ifdef PROVIDE_INT64
                                /* Based on old Hugs code */
                                /* ToDo: use ~/fptools/ghc/runtime/prims/PrimArith.lc */
                        case i_encodeFloatz:     OP_zI_F(ldexp(x,y)); break;
                        case i_decodeFloatz:
                            {
                                /* ToDo: this code is known to give very approximate results
                                 * (even when StgInt64 overflow doesn't occur)
	                         */
                                double f0 = PopTaggedFloat();
                                int    n;
                                double f1 = frexp((double)(f0),&n); /* 0.5   <= f1 < 1                   */
                                double f2 = ldexp(f1,FLT_MANT_DIG); /* 2^m-1 <= f2 < 2^m, m=FLT_MANT_DIG */
                                PushTaggedInt(n-FLT_MANT_DIG);
                                PushTaggedInt64((StgInt64)f2);
#if 1 /* paranoia */
                                if (ldexp((StgInt64)f2,n-FLT_MANT_DIG) != f0) {
                                    fprintf(stderr,"*** primDecodeFloat mismatch: %.10f != %.10f\n",
                                            ldexp((StgInt64)f2,n-FLT_MANT_DIG),f0);
                                }
#endif
                            }
                            break;
#endif /* PROVIDE_INT64 */
#ifdef PROVIDE_INTEGER
                        case i_encodeFloatZ: OP_ZI_F(__encodeFloat(x,y)); break; 
                        case i_decodeFloatZ: OP_F_ZI(__decodeFloat(r1,&r2,x)); break;
#endif
                        case i_isNaNFloat:      OP_F_B(isFloatNaN(x));      break;
                        case i_isInfiniteFloat: OP_F_B(isFloatInfinite(x)); break;
                        case i_isNegativeZeroFloat: OP_F_B(isFloatNegativeZero(x)); break;
                        case i_isDenormalizedFloat: OP_F_B(isFloatDenormalized(x)); break;
                        case i_gtDouble:        OP_DD_B(x>y);        break;
                        case i_geDouble:        OP_DD_B(x>=y);       break;
                        case i_eqDouble:        OP_DD_B(x==y);       break;
                        case i_neDouble:        OP_DD_B(x!=y);       break;
                        case i_ltDouble:        OP_DD_B(x<y);        break;
                        case i_leDouble:        OP_DD_B(x<=y)        break;
                        case i_minDouble:       OP__D(DBL_MIN);      break;
                        case i_maxDouble:       OP__D(DBL_MAX);      break;
                        case i_radixDouble:     OP__I(FLT_RADIX);    break;
                        case i_digitsDouble:    OP__I(DBL_MANT_DIG); break;
                        case i_minExpDouble:    OP__I(DBL_MIN_EXP);  break;
                        case i_maxExpDouble:    OP__I(DBL_MAX_EXP);  break;
                        case i_plusDouble:      OP_DD_D(x+y);        break;
                        case i_minusDouble:     OP_DD_D(x-y);        break;
                        case i_timesDouble:     OP_DD_D(x*y);        break;
                        case i_divideDouble:
                            {
                                StgDouble x = PopTaggedDouble();
                                StgDouble y = PopTaggedDouble();
#if 0
                                if (y == 0) {
                                    obj = raiseDiv0("divideDouble");
                                    goto enterLoop;
                                }
#endif
                                PushTaggedDouble(x/y);
                            }
                            break;
                        case i_negateDouble:    OP_D_D(-x);          break;
                        case i_doubleToInt:     OP_D_I(x);           break;
                        case i_intToDouble:     OP_I_D(x);           break;
                        case i_doubleToFloat:   OP_D_F(x);           break;
                        case i_floatToDouble:   OP_F_F(x);           break;
                        case i_expDouble:       OP_D_D(exp(x));      break;
                        case i_logDouble:       OP_D_D(log(x));      break;
                        case i_sqrtDouble:      OP_D_D(sqrt(x));     break;
                        case i_sinDouble:       OP_D_D(sin(x));      break;
                        case i_cosDouble:       OP_D_D(cos(x));      break;
                        case i_tanDouble:       OP_D_D(tan(x));      break;
                        case i_asinDouble:      OP_D_D(asin(x));     break;
                        case i_acosDouble:      OP_D_D(acos(x));     break;
                        case i_atanDouble:      OP_D_D(atan(x));     break;
                        case i_sinhDouble:      OP_D_D(sinh(x));     break;
                        case i_coshDouble:      OP_D_D(cosh(x));     break;
                        case i_tanhDouble:      OP_D_D(tanh(x));     break;
                        case i_powerDouble:     OP_DD_D(pow(x,y));   break;
#ifdef PROVIDE_INT64
                        case i_encodeDoublez:    OP_zI_D(ldexp(x,y)); break;
                        case i_decodeDoublez:
                            {
                                /* ToDo: this code is known to give very approximate results 
                                 * (even when StgInt64 overflow doesn't occur)
				 */
                                double f0 = PopTaggedDouble();
                                int    n;
                                double f1 = frexp((double)(f0),&n); /* 0.5   <= f1 < 1                   */
                                double f2 = ldexp(f1,FLT_MANT_DIG); /* 2^m-1 <= f2 < 2^m, m=FLT_MANT_DIG */
                                PushTaggedInt(n-FLT_MANT_DIG);
                                PushTaggedInt64((StgInt64)f2);
#if 1 /* paranoia */
                                if (ldexp((StgInt64)f2,n-FLT_MANT_DIG) != f0) {
                                    fprintf(stderr,"*** primDecodeDouble mismatch: %.10f != %.10f\n",
                                            ldexp((StgInt64)f2,n-FLT_MANT_DIG),f0);
                                }
#endif
                            }
                            break;
#endif /* PROVIDE_INT64 */
#ifdef PROVIDE_INTEGER
                        case i_encodeDoubleZ: OP_ZI_D(__encodeDouble(x,y)); break; 
                        case i_decodeDoubleZ: OP_D_ZI(__decodeDouble(r1,&r2,x)); break;
#endif /* PROVIDE_INTEGER */
                        case i_isNaNDouble:      OP_D_B(isDoubleNaN(x));      break;
                        case i_isInfiniteDouble: OP_D_B(isDoubleInfinite(x)); break;
                        case i_isNegativeZeroDouble: OP_D_B(isDoubleNegativeZero(x)); break;
                        case i_isDenormalizedDouble: OP_D_B(isDoubleDenormalized(x)); break;
                        case i_isIEEEDouble:
                            {
                                PushTaggedBool(rtsTrue);
                            }
                            break;
                        default:
                                barf("Unrecognised primop1");
                        }
                        break;            
                    }
                case i_PRIMOP2:
                    {
                        switch (bcoInstr(bco,pc++)) {
                        case i_INTERNAL_ERROR2:
                                barf("INTERNAL_ERROR2 at %p:%d",bco,pc-1);
                        case i_catch:  /* catch#{e,h} */
                            {
                                StgClosure* h;
                                obj = PopCPtr();
                                h   = PopCPtr();

                                /* catch suffers the same problem as takeMVar:
                                 * it tries to do control flow even if it isn't
                                 * the last instruction in the BCO.
                                 * This can leave a mess on the stack if the 
                                 * last instructions are anything important
                                 * like SLIDE.  Our vile hack depends on the
                                 * fact that with the current code generator,
                                 * we know exactly that i_catch is followed
                                 * by code that drops 2 variables off the
				 * stack.
                                 * What a vile hack!
				 */
                                Sp += 2; 

                                PushCatchFrame(h);
                                goto enterLoop;
                            }
                        case i_raise:  /* raise#{err} */
                            {
                                StgClosure* err = PopCPtr();
                                obj = raiseAnError(err);
                                goto enterLoop;
                            }
                        case i_force:    /* force#{x} (evaluate x, primreturn nothing) */
                            {
                                StgClosure* x;
                                obj = PopCPtr();

                                /* force suffers the same problem as takeMVar:
                                 * it tries to do control flow even if it isn't
                                 * the last instruction in the BCO.
                                 * This can leave a mess on the stack if the 
                                 * last instructions are anything important
                                 * like SLIDE.  Our vile hack depends on the
                                 * fact that with the current code generator,
                                 * we know exactly that i_force is followed
                                 * by code that drops 1 variable off the stack.
                                 * What a vile hack!
                                 */
                                Sp += 1;

                                PushSeqFrame();
                                goto enterLoop;
                            }
#ifdef PROVIDE_ARRAY
                        case i_newRef:
                            {
                                StgClosure* init = PopCPtr();
                                StgMutVar* mv
                                    = stgCast(StgMutVar*,allocate(sizeofW(StgMutVar)));
                                SET_HDR(mv,&MUT_VAR_info,CCCS);
                                mv->var = init;
                                PushPtr(stgCast(StgPtr,mv));
                                break;
                            }
                        case i_readRef:
                            { 
                                StgMutVar* mv = stgCast(StgMutVar*,PopPtr());
                                PushCPtr(mv->var);
                                break;
                            }
                        case i_writeRef:
                            { 
                                StgMutVar*  mv    = stgCast(StgMutVar*,PopPtr());
                                StgClosure* value = PopCPtr();
                                mv->var = value;
                                break;
                            }
                        case i_newArray:
                            {
                                nat         n    = PopTaggedInt(); /* or Word?? */
                                StgClosure* init = PopCPtr();
                                StgWord     size = sizeofW(StgArrPtrs) + n;
                                nat i;
                                StgArrPtrs* arr 
                                    = stgCast(StgArrPtrs*,allocate(size));
                                SET_HDR(arr,&MUT_ARR_PTRS_info,CCCS);
                                arr->ptrs = n;
                                for (i = 0; i < n; ++i) {
                                    arr->payload[i] = init;
                                }
                                PushPtr(stgCast(StgPtr,arr));
                                break; 
                            }
                        case i_readArray:
                        case i_indexArray:
                            {
                                StgArrPtrs* arr = stgCast(StgArrPtrs*,PopPtr());
                                nat         i   = PopTaggedInt(); /* or Word?? */
                                StgWord     n   = arr->ptrs;
                                if (i >= n) {
                                    obj = raiseIndex("{index,read}Array");
                                    goto enterLoop;
                                }
                                PushCPtr(arr->payload[i]);
                                break;
                            }
                        case i_writeArray:
                            {
                                StgArrPtrs* arr = stgCast(StgArrPtrs*,PopPtr());
                                nat         i   = PopTaggedInt(); /* or Word? */
                                StgClosure* v   = PopCPtr();
                                StgWord     n   = arr->ptrs;
                                if (i >= n) {
                                    obj = raiseIndex("{index,read}Array");
                                    goto enterLoop;
                                }
                                arr->payload[i] = v;
                                break;
                            }
                        case i_sizeArray:
                        case i_sizeMutableArray:
                            {
                                StgArrPtrs* arr = stgCast(StgArrPtrs*,PopPtr());
                                PushTaggedInt(arr->ptrs);
                                break;
                            }
                        case i_unsafeFreezeArray:
                            {
                                StgArrPtrs* arr = stgCast(StgArrPtrs*,PopPtr());
                                SET_INFO(arr,&MUT_ARR_PTRS_FROZEN_info);
                                PushPtr(stgCast(StgPtr,arr));
                                break;
                            }
                        case i_unsafeFreezeByteArray:
                            {
                                /* Delightfully simple :-) */
                                break;
                            }
                        case i_sameRef:
                        case i_sameMutableArray:
                        case i_sameMutableByteArray:
                            {
                                StgPtr x = PopPtr();
                                StgPtr y = PopPtr();
                                PushTaggedBool(x==y);
                                break;
                            }

                        case i_newByteArray:
                            {
                                nat     n     = PopTaggedInt(); /* or Word?? */
                                StgInt  words = (n+sizeof(W_)-1)/sizeof(W_);
                                StgWord size  = sizeofW(StgArrWords) + words;
                                nat i;
                                StgArrWords* arr  = stgCast(StgArrWords*,allocate(size));
                                SET_HDR(arr,&MUT_ARR_WORDS_info,CCCS);
                                arr->words = words;
#ifdef DEBUG
                                for (i = 0; i < n; ++i) {
                                    arr->payload[i] = 0xdeadbeef;
                                }
#endif
                                PushPtr(stgCast(StgPtr,arr));
                                break; 
                            }

                        /* Most of these generate alignment warnings on Sparcs and similar architectures.
	                 * These are harmless and are caused by the cast to C* in BYTE_ARR_CTS.
	                 */
                        case i_indexCharArray:   OP_mI_ty(Char,"indexCharArray",    indexCharArrayZh(r,x,i)); break;
                        case i_readCharArray:    OP_mI_ty(Char,"readCharArray",     readCharArrayZh(r,x,i));  break;
                        case i_writeCharArray:   OP_mIty_(Char,"writeCharArray",    writeCharArrayZh(x,i,z)); break;

                        case i_indexIntArray:    OP_mI_ty(Int,"indexIntArray",      indexIntArrayZh(r,x,i)); break;
                        case i_readIntArray:     OP_mI_ty(Int,"readIntArray",       readIntArrayZh(r,x,i));  break;
                        case i_writeIntArray:    OP_mIty_(Int,"writeIntArray",      writeIntArrayZh(x,i,z)); break;
#ifdef PROVIDE_INT64
                        case i_indexInt64Array:  OP_mI_ty(Int64,"indexInt64Array",  indexInt64ArrayZh(r,x,i)); break;
                        case i_readInt64Array:   OP_mI_ty(Int64,"readInt64Array",   readInt64ArrayZh(r,x,i));  break;
                        case i_writeInt64Array:  OP_mIty_(Int64,"writeInt64Array",  writeInt64ArrayZh(x,i,z)); break;
#endif
#ifdef PROVIDE_ADDR
                        case i_indexAddrArray:   OP_mI_ty(Addr,"indexAddrArray",   indexAddrArrayZh(r,x,i)); break;
                        case i_readAddrArray:    OP_mI_ty(Addr,"readAddrArray",    readAddrArrayZh(r,x,i));  break;
                        case i_writeAddrArray:   OP_mIty_(Addr,"writeAddrArray",   writeAddrArrayZh(x,i,z)); break;
#endif
                        case i_indexFloatArray:  OP_mI_ty(Float,"indexFloatArray",  indexFloatArrayZh(r,x,i)); break;
                        case i_readFloatArray:   OP_mI_ty(Float,"readFloatArray",   readFloatArrayZh(r,x,i));  break;
                        case i_writeFloatArray:  OP_mIty_(Float,"writeFloatArray",  writeFloatArrayZh(x,i,z)); break;

                        case i_indexDoubleArray: OP_mI_ty(Double,"indexDoubleArray", indexDoubleArrayZh(r,x,i)); break;
                        case i_readDoubleArray:  OP_mI_ty(Double,"readDoubleArray",  readDoubleArrayZh(r,x,i));  break;
                        case i_writeDoubleArray: OP_mIty_(Double,"writeDoubleArray", writeDoubleArrayZh(x,i,z)); break;

#ifdef PROVIDE_STABLE
                        case i_indexStableArray: OP_mI_ty(StablePtr,"indexStableArray", indexStablePtrArrayZh(r,x,i)); break;
                        case i_readStableArray:  OP_mI_ty(StablePtr,"readStableArray",  readStablePtrArrayZh(r,x,i));  break;
                        case i_writeStableArray: OP_mIty_(StablePtr,"writeStableArray", writeStablePtrArrayZh(x,i,z)); break;
#endif

#endif /* PROVIDE_ARRAY */
#ifdef PROVIDE_COERCE
                        case i_unsafeCoerce:
                            {
                                /* Another nullop */
                                break;
                            }
#endif
#ifdef PROVIDE_PTREQUALITY
                        case i_reallyUnsafePtrEquality:
                            { /* identical to i_sameRef */
                                StgPtr x = PopPtr();
                                StgPtr y = PopPtr();
                                PushTaggedBool(x==y);
                                break;
                            }
#endif
#ifdef PROVIDE_FOREIGN
                                /* ForeignObj# operations */
                        case i_makeForeignObj:
                            {
                                StgForeignObj *result 
                                    = stgCast(StgForeignObj*,allocate(sizeofW(StgForeignObj)));
                                SET_HDR(result,&FOREIGN_info,CCCS);
                                result -> data      = PopTaggedAddr();
                                PushPtr(stgCast(StgPtr,result));
                                break;
                            }
#endif /* PROVIDE_FOREIGN */
#ifdef PROVIDE_WEAK
                        case i_makeWeak:
                            {
                                StgWeak *w
                                    = stgCast(StgWeak*,allocate(sizeofW(StgWeak)));
                                SET_HDR(w, &WEAK_info, CCCS);
                                w->key        = PopCPtr();
                                w->value      = PopCPtr();
                                w->finaliser  = PopCPtr();
                                w->link       = weak_ptr_list;
                                weak_ptr_list = w;
                                IF_DEBUG(weak, fprintf(stderr,"New weak pointer at %p\n",w));
                                PushPtr(stgCast(StgPtr,w));
                                break;
                            }
                        case i_deRefWeak:
                            {
                                StgWeak *w = stgCast(StgWeak*,PopPtr());
                                if (w->header.info == &WEAK_info) {
                                    PushCPtr(w->value); /* last result  */
                                    PushTaggedInt(1);   /* first result */
                                } else {
                                    PushPtr(stgCast(StgPtr,w)); /* ToDo: error thunk would be better */
                                    PushTaggedInt(0);
                                }
                                break;
                            }
#endif /* PROVIDE_WEAK */
#ifdef PROVIDE_STABLE
                                /* StablePtr# operations */
                        case i_makeStablePtr:
                            {
                                StgStablePtr stable_ptr;
                                if (stable_ptr_free == NULL) {
                                    enlargeStablePtrTable();
                                }
                        
                                stable_ptr = stable_ptr_free - stable_ptr_table;
                                stable_ptr_free  = (P_*)*stable_ptr_free;
                                stable_ptr_table[stable_ptr] = PopPtr();

                                PushTaggedStablePtr(stable_ptr);
                                break;
                            }
                        case i_deRefStablePtr:
                            {
                                StgStablePtr stable_ptr = PopTaggedStablePtr();
                                PushPtr(stable_ptr_table[stable_ptr]);
                                break;
                            }     

                        case i_freeStablePtr:
                            {
                                StgStablePtr stable_ptr = PopTaggedStablePtr();
                                stable_ptr_table[stable_ptr] = (P_)stable_ptr_free;
                                stable_ptr_free = stable_ptr_table + stable_ptr;
                                break;
                            }     
#endif /* PROVIDE_STABLE */
#ifdef PROVIDE_CONCURRENT
                        case i_fork:
                            {
                                StgClosure* c = PopCPtr();
                                StgTSO* t = createGenThread(RtsFlags.GcFlags.initialStkSize,c);
                                PushPtr(stgCast(StgPtr,t));

                                /* switch at the earliest opportunity */ 
                                context_switch = 1;
                                /* but don't automatically switch to GHC - or you'll waste your
                                 * time slice switching back.
                                 * 
                                 * Actually, there's more to it than that: the default
                                 * (ThreadEnterGHC) causes the thread to crash - don't 
                                 * understand why. - ADR
                                 */
                                t->whatNext = ThreadEnterHugs;
                                break;
                            }
                        case i_killThread:
                            {
                                StgTSO* tso = stgCast(StgTSO*,PopPtr());
                                deleteThread(tso);
                                if (tso == CurrentTSO) { /* suicide */
                                    return ThreadFinished;
                                }
                                break;
                            }
                        case i_sameMVar:
                            { /* identical to i_sameRef */
                                StgPtr x = PopPtr();
                                StgPtr y = PopPtr();
                                PushTaggedBool(x==y);
                                break;
                            }
                        case i_newMVar:
                            {
                                StgMVar *mvar = stgCast(StgMVar*,allocate(sizeofW(StgMVar)));
                                SET_INFO(mvar,&EMPTY_MVAR_info);
                                mvar->head = mvar->tail = EndTSOQueue;
                                /* ToDo: this is a little strange */
                                mvar->value = stgCast(StgClosure*,&END_TSO_QUEUE_closure);
                                PushPtr(stgCast(StgPtr,mvar));
                                break;
                            }
#if 1
#if 0
ToDo: another way out of the problem might be to add an explicit
continuation to primTakeMVar: takeMVar v = primTakeMVar v takeMVar.
The problem with this plan is that now I dont know how much to chop
off the stack.
#endif
                        case i_takeMVar:
                            {
                                StgMVar *mvar = stgCast(StgMVar*,PopPtr());
                                /* If the MVar is empty, put ourselves
                                 * on its blocking queue, and wait
                                 * until we're woken up.  
                                 */
                                if (GET_INFO(mvar) != &FULL_MVAR_info) {
                                    if (mvar->head == EndTSOQueue) {
                                        mvar->head = CurrentTSO;
                                    } else {
                                        mvar->tail->link = CurrentTSO;
                                    }
                                    CurrentTSO->link = EndTSOQueue;
                                    mvar->tail = CurrentTSO;

                                    /* Hack, hack, hack.
                                     * When we block, we push a restart closure
                                     * on the stack - but which closure?
                                     * We happen to know that the BCO we're
                                     * executing looks like this:
                                     *
                                     *	 0:      STK_CHECK 4
                                     *	 2:      HP_CHECK 3
                                     *	 4:      TEST 0 29
                                     *	 7:      UNPACK
                                     *	 8:      VAR 3
                                     *	 10:     VAR 1
                                     *	 12:     primTakeMVar
                                     *	 14:     ALLOC_CONSTR 0x8213a80
                                     *	 16:     VAR 2
                                     *	 18:     VAR 2
                                     *	 20:     PACK 2
                                     *	 22:     VAR 0
                                     *	 24:     SLIDE 1 7
                                     *	 27:     ENTER
                                     *	 28:     PANIC
                                     *	 29:     PANIC
                                     *
                                     * so we rearrange the stack to look the
                                     * way it did when we entered this BCO
				     * and push ths BCO.
                                     * What a disgusting hack!
                                     */

                                    PopPtr();
                                    PopPtr();
                                    PushCPtr(obj);
                                    return ThreadBlocked;

                                } else {
                                    PushCPtr(mvar->value);
                                    SET_INFO(mvar,&EMPTY_MVAR_info);
                                    /* ToDo: this is a little strange */
                                    mvar->value = (StgClosure*)&END_TSO_QUEUE_closure;
                                }
                                break;
                            }
#endif
                        case i_putMVar:
                            {
                                StgMVar*    mvar  = stgCast(StgMVar*,PopPtr());
                                StgClosure* value = PopCPtr();
                                if (GET_INFO(mvar) == &FULL_MVAR_info) {
                                    obj = raisePrim("putMVar {full MVar}");
                                    goto enterLoop;
                                } else {
                                    /* wake up the first thread on the
                                     * queue, it will continue with the
                                     * takeMVar operation and mark the
                                     * MVar empty again.  
                                     */
                                    StgTSO* tso = mvar->head;
                                    SET_INFO(mvar,&FULL_MVAR_info);
                                    mvar->value = value;
                                    if (tso != EndTSOQueue) {
                                        PUSH_ON_RUN_QUEUE(tso);
                                        mvar->head = tso->link;
                                        tso->link = EndTSOQueue;
                                        if (mvar->head == EndTSOQueue) {
                                            mvar->tail = EndTSOQueue;
                                        }
                                    }
                                }
                                /* yield for better communication performance */
                                context_switch = 1;
                                break;
                            }
                        case i_delay:
                        case i_waitRead:
                        case i_waitWrite:
                                /* As PrimOps.h says: Hmm, I'll think about these later. */
                                ASSERT(0);
                                break;
#endif /* PROVIDE_CONCURRENT */
                        case i_ccall_Id:
                        case i_ccall_IO:
                            {
                                CFunDescriptor* descriptor = PopTaggedAddr();
                                StgAddr funPtr = PopTaggedAddr();
                                ccall(descriptor,funPtr);
                                break;
                            }
                        default:
                                barf("Unrecognised primop2");
                        }
                        break;            
                    }
                default:
                        barf("Unrecognised instruction");
                }
            }
            barf("Ran off the end of bco - yoiks");
            break;
        }
    case CAF_UNENTERED:
        {
            StgCAF* caf = stgCast(StgCAF*,obj);
            if (Sp - sizeofW(StgUpdateFrame) < SpLim) {
                PushCPtr(obj); /* code to restart with */
                return StackOverflow;
            }
            /* ToDo: look for Sp==Su && stackInt(0) == UPD_FRAME and insert an indirection immediately */
            {
                StgBlackHole* bh = stgCast(StgBlackHole*,grabHpUpd(BLACKHOLE_sizeW()));
                SET_INFO(bh,&CAF_BLACKHOLE_info);
                bh->blocking_queue = EndTSOQueue;
                IF_DEBUG(gccafs,fprintf(stderr,"Created CAF_BLACKHOLE %p for CAF %p in evaluator\n",bh,caf));
                SET_INFO(caf,&CAF_ENTERED_info);
                caf->value = stgCast(StgClosure*,bh);
                PUSH_UPD_FRAME(bh,0);
                Sp -= sizeofW(StgUpdateFrame);
            }
            caf->link = enteredCAFs;
            enteredCAFs = caf;
            obj = caf->body;
            goto enterLoop;
        }
    case CAF_ENTERED:
        {
            StgCAF* caf = stgCast(StgCAF*,obj);
            obj = caf->value; /* it's just a fancy indirection */
            goto enterLoop;
        }
    case BLACKHOLE:
    case CAF_BLACKHOLE:
        {
            StgBlackHole* bh = stgCast(StgBlackHole*,obj);
            /* Put ourselves on the blocking queue for this black hole and block */
            CurrentTSO->link = bh->blocking_queue;
            bh->blocking_queue = CurrentTSO;
            PushCPtr(obj); /* code to restart with */
            return ThreadBlocked;
        }
    case AP_UPD:
        {
            StgAP_UPD* ap = stgCast(StgAP_UPD*,obj);
            int i = ap->n_args;
            if (Sp - (i + sizeofW(StgUpdateFrame)) < SpLim) {
                PushCPtr(obj); /* code to restart with */
                return StackOverflow;
            }
            /* ToDo: look for Sp==Su && stackInt(0) == UPD_FRAME and insert an indirection immediately  */
            PUSH_UPD_FRAME(ap,0);
            Sp -= sizeofW(StgUpdateFrame);
            while (--i >= 0) {
                PushWord(payloadWord(ap,i));
            }
            obj = ap->fun;
#ifndef LAZY_BLACKHOLING
            {
                /* superfluous - but makes debugging easier */
                StgBlackHole* bh = stgCast(StgBlackHole*,ap);
                SET_INFO(bh,&BLACKHOLE_info);
                bh->blocking_queue = EndTSOQueue;
                IF_DEBUG(gccafs,fprintf(stderr,"Eagerly blackholed AP_UPD %p in evaluator\n",bh));
                /*printObj(bh); */
            }
#endif /* LAZY_BLACKHOLING */
            goto enterLoop;
        }
    case PAP:
        {
            StgPAP* pap = stgCast(StgPAP*,obj);
            int i = pap->n_args;  /* ToDo: stack check */
            /* ToDo: if PAP is in whnf, we can update any update frames
             * on top of stack.
	     */
            while (--i >= 0) {
                PushWord(payloadWord(pap,i));
            }
            obj = pap->fun;
            goto enterLoop;
        }
    case IND:
        {
            obj = stgCast(StgInd*,obj)->indirectee;
            goto enterLoop;
        }
    case CONSTR:
    case CONSTR_INTLIKE:
    case CONSTR_CHARLIKE:
    case CONSTR_STATIC:
    case CONSTR_NOCAF_STATIC:
        {
            while (1) {
                switch (get_itbl(stgCast(StgClosure*,Sp))->type) {
                case CATCH_FRAME:
                        PopCatchFrame();
                        break;
                case UPDATE_FRAME:
                        PopUpdateFrame(obj);
                        break;
                case SEQ_FRAME:
                        PopSeqFrame();
                        break;
                case STOP_FRAME:
                    {
                        ASSERT(Sp==(P_)Su);
                        IF_DEBUG(evaluator,
                                 printObj(obj);
                                 /*fprintf(stderr,"Sp = %p\tSu = %p\n", Sp, Su);*/
                                 /*printStack(Sp,CurrentTSO->stack+CurrentTSO->stack_size,Su);*/
                                 );
                        PopStopFrame(obj);
                        return ThreadFinished;
                    }
                case RET_BCO:
                    {
                        StgClosure* ret;
                        PopPtr();
                        ret = PopCPtr();
                        PushPtr((P_)obj);
                        obj = ret;
                        goto enterLoop;
                    }
                case RET_SMALL:  /* return to GHC */
                case RET_VEC_SMALL:
                case RET_BIG:
                case RET_VEC_BIG:
                        barf("todo: RET_[VEC_]{BIG,SMALL}");
                default:
                        belch("entered CONSTR with invalid continuation on stack");
                        IF_DEBUG(evaluator,
                                 printObj(stgCast(StgClosure*,Sp))
                                 );
                        barf("bailing out");
                }
            }
        }
    default:
        {
            CurrentTSO->whatNext = ThreadEnterGHC;
            PushCPtr(obj); /* code to restart with */
            return ThreadYielding;
        }
    }
    barf("Ran off the end of enter - yoiks");
}

/* -----------------------------------------------------------------------------
 * ccall support code:
 *   marshall moves args from C stack to Haskell stack
 *   unmarshall moves args from Haskell stack to C stack
 *   argSize calculates how much space you need on the C stack
 * ---------------------------------------------------------------------------*/

/* Pop arguments off the C stack and Push them onto the Hugs stack.
 * Used when preparing for C calling Haskell or in response to
 *  Haskell calling C.
 */
nat marshall(char arg_ty, void* arg)
{
    switch (arg_ty) {
    case INT_REP:
            PushTaggedInt(*((int*)arg));
            return ARG_SIZE(INT_TAG);
#ifdef PROVIDE_INT64
    case INT64_REP:
            PushTaggedInt64(*((StgInt64*)arg));
            return ARG_SIZE(INT64_TAG);
#endif
#ifdef TODO_PROVIDE_INTEGER
    case INTEGER_REP:
            PushTaggedInteger(*((mpz_ptr*)arg));
            return ARG_SIZE(INTEGER_TAG);
#endif
#ifdef PROVIDE_WORD
    case WORD_REP:
            PushTaggedWord(*((unsigned int*)arg));
            return ARG_SIZE(WORD_TAG);
#endif
    case CHAR_REP:
            PushTaggedChar(*((char*)arg));
            return ARG_SIZE(CHAR_TAG);
    case FLOAT_REP:
            PushTaggedFloat(*((float*)arg));
            return ARG_SIZE(FLOAT_TAG);
    case DOUBLE_REP:
            PushTaggedDouble(*((double*)arg));
            return ARG_SIZE(DOUBLE_TAG);
#ifdef PROVIDE_ADDR
    case ADDR_REP:
            PushTaggedAddr(*((void**)arg));
            return ARG_SIZE(ADDR_TAG);
#endif
    case STABLE_REP:
            PushTaggedStablePtr(*((StgStablePtr*)arg));
            return ARG_SIZE(STABLE_TAG);
    case FOREIGN_REP:
            /* Not allowed in this direction - you have to
             * call makeForeignPtr explicitly
             */
            barf("marshall: ForeignPtr#\n");
            break;
#ifdef PROVIDE_ARRAY
    case BARR_REP:
    case MUTBARR_REP:
#endif
            /* Not allowed in this direction  */
            barf("marshall: [Mutable]ByteArray#\n");
            break;
    default:
            barf("marshall: unrecognised arg type %d\n",arg_ty);
            break;
    }
}

/* Pop arguments off the Hugs stack and Push them onto the C stack.
 * Used when preparing for Haskell calling C or in response to
 * C calling Haskell.
 */
nat unmarshall(char res_ty, void* res)
{
    switch (res_ty) {
    case INT_REP:
            *((int*)res) = PopTaggedInt();
            return ARG_SIZE(INT_TAG);
#ifdef PROVIDE_INT64
    case INT64_REP:
            *((StgInt64*)res) = PopTaggedInt64();
            return ARG_SIZE(INT64_TAG);
#endif
#ifdef TODO_PROVIDE_INTEGER
    case INTEGER_REP:
            *((mpz_ptr*)res) = PopTaggedInteger();
            return ARG_SIZE(INTEGER_TAG);
#endif
#ifdef PROVIDE_WORD
    case WORD_REP:
            *((unsigned int*)res) = PopTaggedWord();
            return ARG_SIZE(WORD_TAG);
#endif
    case CHAR_REP:
            *((int*)res) = PopTaggedChar();
            return ARG_SIZE(CHAR_TAG);
    case FLOAT_REP:
            *((float*)res) = PopTaggedFloat();
            return ARG_SIZE(FLOAT_TAG);
    case DOUBLE_REP:
            *((double*)res) = PopTaggedDouble();
            return ARG_SIZE(DOUBLE_TAG);
#ifdef PROVIDE_ADDR
    case ADDR_REP:
            *((void**)res) = PopTaggedAddr();
            return ARG_SIZE(ADDR_TAG);
#endif
    case STABLE_REP:
            *((StgStablePtr*)res) = PopTaggedStablePtr();
            return ARG_SIZE(STABLE_TAG);
    case FOREIGN_REP:
        {
            StgForeignObj *result = stgCast(StgForeignObj*,PopPtr());
            *((void**)res) = result->data;
            return sizeofW(StgPtr);
        }
#ifdef PROVIDE_ARRAY
    case BARR_REP:
    case MUTBARR_REP:
#endif
        {
            StgArrPtrs* arr = stgCast(StgArrPtrs*,PopPtr());
            *((void**)res) = stgCast(void*,&(arr->payload));
            return sizeofW(StgPtr);
        }
    default:
            barf("unmarshall: unrecognised result type %d\n",res_ty);
    }
}

nat argSize( const char* ks )
{
    nat sz = 0;
    for( ; *ks != '\0'; ++ks) {
        switch (*ks) {
        case INT_REP:
                sz += sizeof(StgWord) * ARG_SIZE(INT_TAG);
                break;
#ifdef PROVIDE_INT64
        case INT64_REP:
                sz += sizeof(StgWord) * ARG_SIZE(INT64_TAG);
                break;
#endif
#ifdef TODO_PROVIDE_INTEGER
        case INTEGER_REP:
                sz += sizeof(StgWord) * ARG_SIZE(INTEGER_TAG);
                break;
#endif
#ifdef PROVIDE_WORD
        case WORD_REP:
                sz += sizeof(StgWord) * ARG_SIZE(WORD_TAG);
                break;
#endif
        case CHAR_REP:
                sz += sizeof(StgWord) * ARG_SIZE(CHAR_TAG);
                break;
        case FLOAT_REP:
                sz += sizeof(StgWord) * ARG_SIZE(FLOAT_TAG);
                break;
        case DOUBLE_REP:
                sz += sizeof(StgWord) * ARG_SIZE(DOUBLE_TAG);
                break;
#ifdef PROVIDE_ADDR
        case ADDR_REP:
                sz += sizeof(StgWord) * ARG_SIZE(ADDR_TAG);
                break;
#endif
#ifdef PROVIDE_STABLE
        case STABLE_REP:
                sz += sizeof(StgWord) * ARG_SIZE(STABLE_TAG);
                break;
#endif
#ifdef PROVIDE_FOREIGN
        case FOREIGN_REP:
#endif
#ifdef PROVIDE_ARRAY
        case BARR_REP:
        case MUTBARR_REP:
#endif
                sz += sizeof(StgPtr);
                break;
        default:
                barf("argSize: unrecognised result type %d\n",*ks);
                break;
        }
    }
    return sz;
}

#endif /* INTERPRETER */
