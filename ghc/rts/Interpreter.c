#if 0
/* -----------------------------------------------------------------------------
 * Bytecode evaluator
 *
 * Copyright (c) 1994-2000.
 *
 * $RCSfile: Interpreter.c,v $
 * $Revision: 1.1 $
 * $Date: 2000/12/11 12:55:43 $
 * ---------------------------------------------------------------------------*/

#include "Rts.h"



#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Updates.h"
#include "Storage.h"
#include "SchedAPI.h" /* for createGenThread */
#include "Schedule.h" /* for context_switch  */
#include "Bytecodes.h"
#include "Assembler.h" /* for CFun stuff */
#include "ForeignCall.h"
#include "PrimOps.h"   /* for __{encode,decode}{Float,Double} */
#include "Prelude.h"
#include "Itimer.h"
#include "Evaluator.h"
#include "sainteger.h"

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


/* Allegedly useful macro, taken from ClosureMacros.h */
#define payloadWord( c, i )   (*stgCast(StgWord*,      ((c)->payload+(i))))
#define payloadPtr( c, i )    (*stgCast(StgPtr*,       ((c)->payload+(i))))

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

#if defined(__GNUC__) && !defined(DEBUG)
#define USE_GCC_LABELS 1
#else
#define USE_GCC_LABELS 0
#endif

/* Make it possible for the evaluator to get hold of bytecode
   for a given function by name.  Useful but a hack.  Sigh.
 */
extern void* /* StgClosure* */ getHugs_BCO_cptr_for ( char* s );
extern int   /* Bool */ combined;



/* --------------------------------------------------------------------------
 * Hugs Hooks - a bit of a hack
 * ------------------------------------------------------------------------*/

void setRtsFlags( int x );
void setRtsFlags( int x )
{
    unsigned int w    = 0x12345678;
    unsigned char* pw = (unsigned char *)&w;
    if (*pw == 0x78) {
       /* little endian */
       *(int*)(&(RtsFlags.DebugFlags)) = x;
    } else {
       /* big endian */
       unsigned int w1 = x;
       unsigned int w2 = 0;
       w2 |= (w1 && 0xFF); w2 <<= 8; w1 >>= 8;
       w2 |= (w1 && 0xFF); w2 <<= 8; w1 >>= 8;
       w2 |= (w1 && 0xFF); w2 <<= 8; w1 >>= 8;
       w2 |= (w1 && 0xFF); w2 <<= 8; w1 >>= 8;
       *(int*)(&(RtsFlags.DebugFlags)) = (int)w2;
    }
}


typedef struct { 
  StgTSOBlockReason reason;
  unsigned int delay;
} HugsBlock;


/* --------------------------------------------------------------------------
 * Entering-objects and bytecode interpreter part of evaluator
 * ------------------------------------------------------------------------*/

/* The primop (and all other) parts of this evaluator operate upon the 
   machine state which lives in MainRegTable.  enter is different: 
   to make its closure- and bytecode-interpreting loops go fast, some of that 
   state is pulled out into local vars (viz, registers, if we are lucky).  
   That means that we need to save(load) the local state at every exit(reentry) 
   into enter.  That is, around every procedure call it makes.  Blargh!
   If you modify this code, __be warned__ it will fail in mysterious ways if
   you fail to preserve this property.

   Currently the pulled-out state is Sp in xSp, Su in xSu and SpLim in xSpLim.
   The SSS macros saves the state back in MainRegTable, and LLL loads it from
   MainRegTable.  RETURN(v) does SSS and then returns v; all exits should
   be via RETURN and not plain return.

   Since xSp, xSu and xSpLim are local vars in enter, they are not visible
   in procedures called from enter.  To fix this, either (1) turn the 
   procedures into macros, so they get copied inline, or (2) bracket
   the procedure call with SSS and LLL so that the local and global
   machine states are synchronised for the duration of the call.
*/


/* Forward decls ... */
static        void* enterBCO_primop1 ( int );
static        void* enterBCO_primop2 ( int , int* /*StgThreadReturnCode* */, 
                                       StgBCO**, Capability*, HugsBlock * );
static inline void PopUpdateFrame ( StgClosure* obj );
static inline void PopCatchFrame  ( void );
static inline void PopSeqFrame    ( void );
static inline void PopStopFrame( StgClosure* obj );
static inline void PushTaggedRealWorld( void );
/* static inline void PushTaggedInteger  ( mpz_ptr ); */
static inline StgPtr grabHpUpd( nat size );
static inline StgPtr grabHpNonUpd( nat size );
static        StgClosure* raiseAnError   ( StgClosure* exception );

static int  enterCountI = 0;

StgDouble B__encodeDouble (B* s, I_ e);
void      B__decodeDouble (B* man, I_* exp, StgDouble dbl);
StgFloat  B__encodeFloat (B* s, I_ e);
void      B__decodeFloat (B* man, I_* exp, StgFloat flt);
StgPtr    CreateByteArrayToHoldInteger ( int );
B*        IntegerInsideByteArray ( StgPtr );
void      SloppifyIntegerEnd ( StgPtr );




#define gSp     MainRegTable.rSp
#define gSu     MainRegTable.rSu
#define gSpLim  MainRegTable.rSpLim


/* Macros to save/load local state. */
#ifdef DEBUG
#define SSS { tSp=gSp = xSp; tSu=gSu = xSu; tSpLim=gSpLim = xSpLim; }
#define LLL { tSp=xSp = gSp; tSu=xSu = gSu; tSpLim=xSpLim = gSpLim; }
#else
#define SSS { gSp = xSp; gSu = xSu; gSpLim = xSpLim; }
#define LLL { xSp = gSp; xSu = gSu; xSpLim = gSpLim; }
#endif

#define RETURN(vvv) {						\
           StgThreadReturnCode retVal=(vvv); 			\
	   SSS;							\
           cap->rCurrentTSO->sp    = gSp;			\
           cap->rCurrentTSO->su    = gSu;			\
           return retVal;					\
        }


/* Macros to operate directly on the pulled-out machine state.
   These mirror some of the small procedures used in the primop code
   below, except you have to be careful about side effects,
   ie xPushPtr(xStackPtr(n)) won't work!  It certainly isn't the
   same as PushPtr(StackPtr(n)).  Also note that (1) some of
   the macros, in particular xPopTagged*, do not make the tag
   sanity checks that their non-x cousins do, and (2) some of
   the macros depend critically on the semantics of C comma
   expressions to work properly.
*/
#define xPushPtr(ppp)           { xSp--; *xSp=(StgWord)(ppp); }
#define xPopPtr()               ((StgPtr)(*xSp++))

#define xPushCPtr(ppp)          { xSp--; *xSp=(StgWord)(ppp); }
#define xPopCPtr()              ((StgClosure*)(*xSp++))

#define xPushWord(ppp)          { xSp--; *xSp=(StgWord)(ppp); }
#define xPopWord()              ((StgWord)(*xSp++))

#define xStackPtr(nnn)          ((StgPtr)(*(xSp+(nnn))))
#define xStackWord(nnn)         ((StgWord)(*(xSp+(nnn))))
#define xSetStackWord(iii,www)  xSp[iii]=(StgWord)(www)

#define xPushTag(ttt)           { xSp--; *xSp=(StgWord)(ttt); }
#define xPopTag(ttt)            { StackTag t = (StackTag)(*xSp++); \
                                  ASSERT(t == ttt); }

#define xPushTaggedInt(xxx)     { xSp -= sizeofW(StgInt); \
                                  *xSp = (xxx); xPushTag(INT_TAG); }
#define xTaggedStackInt(iii)    ((StgInt)(*(xSp+1+(iii))))
#define xPopTaggedInt()         ((xSp++,xSp+=sizeofW(StgInt), \
                                 (StgInt)(*(xSp-sizeofW(StgInt)))))

#define xPushTaggedWord(xxx)    { xSp -= sizeofW(StgWord); \
                                  *xSp = (xxx); xPushTag(WORD_TAG); }
#define xTaggedStackWord(iii)   ((StgWord)(*(xSp+1+(iii))))
#define xPopTaggedWord()        ((xSp++,xSp+=sizeofW(StgWord), \
                                 (StgWord)(*(xSp-sizeofW(StgWord)))))

#define xPushTaggedAddr(xxx)    { xSp -= sizeofW(StgAddr); \
                                  *xSp = (StgWord)(xxx); xPushTag(ADDR_TAG); }
#define xTaggedStackAddr(iii)   ((StgAddr)(*(xSp+1+(iii))))
#define xPopTaggedAddr()        ((xSp++,xSp+=sizeofW(StgAddr), \
                                 (StgAddr)(*(xSp-sizeofW(StgAddr)))))

#define xPushTaggedStable(xxx)  { xSp -= sizeofW(StgStablePtr); \
                                  *xSp = (StgWord)(xxx); xPushTag(STABLE_TAG); }
#define xTaggedStackStable(iii) ((StgStablePtr)(*(xSp+1+(iii))))
#define xPopTaggedStable()      ((xSp++,xSp+=sizeofW(StgStablePtr), \
                                 (StgStablePtr)(*(xSp-sizeofW(StgStablePtr)))))

#define xPushTaggedChar(xxx)    { xSp -= sizeofW(StgChar); \
                                  *xSp = (StgWord)(xxx); xPushTag(CHAR_TAG); }
#define xTaggedStackChar(iii)   ((StgChar)(*(xSp+1+(iii))))
#define xPopTaggedChar()        ((xSp++,xSp+=sizeofW(StgChar), \
                                 (StgChar)(*(xSp-sizeofW(StgChar)))))

#define xPushTaggedFloat(xxx)   { xSp -= sizeofW(StgFloat); \
                                  ASSIGN_FLT(xSp,xxx); xPushTag(FLOAT_TAG); }
#define xTaggedStackFloat(iii)  PK_FLT(xSp+1+(iii))
#define xPopTaggedFloat()       ((xSp++,xSp+=sizeofW(StgFloat), \
                                 PK_FLT(xSp-sizeofW(StgFloat))))

#define xPushTaggedDouble(xxx)  { xSp -= sizeofW(StgDouble); \
                                  ASSIGN_DBL(xSp,xxx); xPushTag(DOUBLE_TAG); }
#define xTaggedStackDouble(iii) PK_DBL(xSp+1+(iii))
#define xPopTaggedDouble()      ((xSp++,xSp+=sizeofW(StgDouble), \
                                 PK_DBL(xSp-sizeofW(StgDouble))))


#define xPushUpdateFrame(target, xSp_offset)                      \
{                                                                 \
   StgUpdateFrame *__frame;                                       \
   __frame = (StgUpdateFrame *)(xSp + (xSp_offset)) - 1;          \
   SET_INFO(__frame, (StgInfoTable *)&upd_frame_info);            \
   __frame->link = xSu;                                           \
   __frame->updatee = (StgClosure *)(target);                     \
   xSu = __frame;                                                 \
}

#define xPopUpdateFrame(ooo)                                      \
{                                                                 \
    /* NB: doesn't assume that Sp == Su */                        \
    IF_DEBUG(evaluator,                                           \
             fprintf(stderr,  "Updating ");                       \
             printPtr(stgCast(StgPtr,xSu->updatee));              \
             fprintf(stderr,  " with ");                          \
             printObj(ooo);                                       \
             fprintf(stderr,"xSp = %p\txSu = %p\n\n", xSp, xSu);  \
             );                                                   \
    UPD_IND(xSu->updatee,ooo);                                    \
    xSp = stgCast(StgStackPtr,xSu) + sizeofW(StgUpdateFrame);     \
    xSu = xSu->link;                                              \
}



/* Instruction stream macros */
#define BCO_INSTR_8  *bciPtr++
#define BCO_INSTR_16 ((bciPtr += 2,  (*(bciPtr-2) << 8) + *(bciPtr-1)))
#define PC (bciPtr - &(bcoInstr(bco,0)))


/* State on entry to enter():
 *    - current thread  is in cap->rCurrentTSO;
 *    - allocation area is in cap->rCurrentNursery & cap->rNursery
 */

StgThreadReturnCode enter( Capability* cap, StgClosure* obj0 )
{
   /* use of register here is primarily to make it clear to compilers
      that these entities are non-aliasable.
   */
    register StgPtr           xSp;    /* local state -- stack pointer */
    register StgUpdateFrame*  xSu;    /* local state -- frame pointer */
    register StgPtr           xSpLim; /* local state -- stack lim pointer */
    register StgClosure*      obj;    /* object currently under evaluation */
             char             eCount; /* enter counter, for context switching */


   HugsBlock hugsBlock = { NotBlocked, 0 };


#ifdef DEBUG
    StgPtr tSp; StgUpdateFrame* tSu; StgPtr tSpLim;
#endif

    gSp    = cap->rCurrentTSO->sp;
    gSu    = cap->rCurrentTSO->su;
    gSpLim = cap->rCurrentTSO->stack + RESERVED_STACK_WORDS;

#ifdef DEBUG
    /* use the t values to check that Su/Sp/SpLim do not change unexpectedly */
    tSp = gSp; tSu = gSu; tSpLim = gSpLim;
#endif

    obj    = obj0;
    eCount = 0;

    /* Load the local state from global state, and Party On, Dudes! */
    /* From here onwards, we operate with the local state and 
       save/reload it as necessary.
    */
    LLL;

    enterLoop:

    numEnters++;

#ifdef DEBUG
    ASSERT(gSp == tSp);
    ASSERT(gSu == tSu);
    ASSERT(gSpLim == tSpLim);
    IF_DEBUG(evaluator,
             SSS;
             enterCountI++;
             ASSERT(xSpLim <= xSp && xSp <= stgCast(StgPtr,xSu));
             fprintf(stderr, 
             "\n---------------------------------------------------------------\n");
             fprintf(stderr,"(%d) Entering: ",enterCountI); printObj(obj);
             fprintf(stderr,"xSp = %p\txSu = %p\n", xSp, xSu);
             fprintf(stderr, "\n" );
             printStack(xSp,cap->rCurrentTSO->stack+cap->rCurrentTSO->stack_size,xSu);
             fprintf(stderr, "\n\n");
             LLL;
            );
#endif

    if (
#ifdef DEBUG
             ((++eCount) & 0x0F) == 0
#else
             ++eCount == 0
#endif
       ) {
       if (context_switch) {
	 switch(hugsBlock.reason) {
	 case NotBlocked: {
	   xPushCPtr(obj); /* code to restart with */
	   RETURN(ThreadYielding);
	 }
	 case BlockedOnDelay: /* fall through */
	 case BlockedOnRead:  /* fall through */
	 case BlockedOnWrite: {
	   ASSERT(cap->rCurrentTSO->why_blocked == NotBlocked);
	   cap->rCurrentTSO->why_blocked = BlockedOnDelay;
	   ACQUIRE_LOCK(&sched_mutex);
	   
#if defined(HAVE_SETITIMER) /* || defined(mingw32_TARGET_OS) */
	   cap->rCurrentTSO->block_info.delay
	     = hugsBlock.delay + ticks_since_select;
#else
	   cap->rCurrentTSO->block_info.target
	     = hugsBlock.delay + getourtimeofday();
#endif
	   APPEND_TO_BLOCKED_QUEUE(cap->rCurrentTSO);
	   
	   RELEASE_LOCK(&sched_mutex);
	   
	   xPushCPtr(obj); /* code to restart with */
	   RETURN(ThreadBlocked);
	 }
	 default:
	   barf("Unknown context switch reasoning");
	 }
       }
    }

    switch ( get_itbl(obj)->type ) {
    case INVALID_OBJECT:
            barf("Invalid object %p",obj);

    case BCO: bco_entry:

            /* ---------------------------------------------------- */
            /* Start of the bytecode evaluator                      */
            /* ---------------------------------------------------- */
        {
#           if USE_GCC_LABELS
#           define Ins(x)          &&l##x
            static void *labs[] = { INSTRLIST };
#           undef Ins
#           define LoopTopLabel
#           define Case(x)         l##x
#           define Continue        goto *labs[BCO_INSTR_8]
#           define Dispatch        Continue;
#           define EndDispatch
#           else
#           define LoopTopLabel    insnloop:
#           define Case(x)         case x
#           define Continue        goto insnloop
#           define Dispatch        switch (BCO_INSTR_8) {
#           define EndDispatch     }
#           endif

            register StgWord8* bciPtr; /* instruction pointer */
            register StgBCO*   bco = (StgBCO*)obj;
            StgWord wantToGC;

            /* Don't need to SSS ... LLL around doYouWantToGC */
            wantToGC = doYouWantToGC();
            if (wantToGC) {
                xPushCPtr((StgClosure*)bco); /* code to restart with */
                RETURN(HeapOverflow);
            }

            bciPtr = &(bcoInstr(bco,0));

            LoopTopLabel

            ASSERT((StgWord)(PC) < bco->n_instrs);
            IF_DEBUG(evaluator,
            fprintf(stderr,"Sp = %p\tSu = %p\tpc = %d\t", xSp, xSu, PC);
                    SSS;
                    disInstr(bco,PC);
                    if (0) { int i;
                    fprintf(stderr,"\n");
                      for (i = 8; i >= 0; i--) 
                         fprintf(stderr, "%d  %p\n", i, (StgPtr)(*(gSp+i)));
                      }
                    fprintf(stderr,"\n");
                    LLL;
                   );

            Dispatch

            Case(i_INTERNAL_ERROR):
                    barf("INTERNAL_ERROR at %p:%d",bco,PC-1);
            Case(i_PANIC):
                    barf("PANIC at %p:%d",bco,PC-1);
            Case(i_STK_CHECK):
                {
                    int n = BCO_INSTR_8;
                    if (xSp - n < xSpLim) {
                        xPushCPtr((StgClosure*)bco); /* code to restart with */
                        RETURN(StackOverflow);
                    }
                    Continue;
                }
            Case(i_STK_CHECK_big):
                {
                    int n = BCO_INSTR_16;
                    if (xSp - n < xSpLim) {
                        xPushCPtr((StgClosure*)bco); /* code to restart with */
                        RETURN(StackOverflow);
                    }
                    Continue;
                }
            Case(i_ARG_CHECK):
                {
                    nat n = BCO_INSTR_8;
                    if ((StgPtr*)(xSp + n) > (StgPtr*)xSu) {
                        StgWord words = (P_)xSu - xSp;
                         
                        /* first build a PAP */
                        ASSERT((P_)xSu >= xSp);  /* was (words >= 0) but that's always true */
                        if (words == 0) { /* optimisation */
                            /* Skip building the PAP and update with an indirection. */
                        } else { 
                            /* Build the PAP. */
                            /* In the evaluator, we avoid the need to do 
                             * a heap check here by including the size of
                             * the PAP in the heap check we performed
                             * when we entered the BCO.
        	             */
                             StgInt  i;
                             StgPAP* pap;
                             SSS; pap = (StgPAP*)grabHpNonUpd(PAP_sizeW(words)); LLL;
                             SET_HDR(pap,&PAP_info,CC_pap);
                             pap->n_args = words;
                             pap->fun = obj;
                             for (i = 0; i < (I_)words; ++i) {
                                 payloadWord(pap,i) = xSp[i];
                             }
                             xSp += words;
                             obj = stgCast(StgClosure*,pap);
                        }
        
                        /* now deal with "update frame" */
                        /* as an optimisation, we process all on top of stack */
                        /* instead of just the top one */
                        ASSERT(xSp==(P_)xSu);
                        do {
                            switch (get_itbl(xSu)->type) {
                                case CATCH_FRAME:
                                    /* Hit a catch frame during an arg satisfaction check,
                                     * so the thing returning (1) has not thrown an
                                     * exception, and (2) is of functional type.  Just
                                     * zap the catch frame and carry on down the stack
                                     * (looking for more arguments, basically).
                                     */
                                     SSS; PopCatchFrame(); LLL;
                                     break;
                                case UPDATE_FRAME:
                                     xPopUpdateFrame(obj);
                                     break;
                                case STOP_FRAME:
                                     barf("STOP frame during pap update");
#if 0
				     cap->rCurrentTSO->what_next = ThreadComplete;
                                     SSS; PopStopFrame(obj); LLL;
                                     RETURN(ThreadFinished);
#endif
                                case SEQ_FRAME:
                                     SSS; PopSeqFrame(); LLL;
                                     ASSERT(xSp != (P_)xSu);
                                     /* Hit a SEQ frame during an arg satisfaction check.
                                      * So now return to bco_info which is under the 
                                      * SEQ frame.  The following code is copied from a 
                                      * case RET_BCO further down.  (The reason why we're
                                      * here is that something of functional type has 
                                      * been seq-d on, and we're now returning to the
                                      * algebraic-case-continuation which forced the
                                      * evaluation in the first place.)
                                      */
                                      {
                                          StgClosure* ret;
                                          (void)xPopPtr();
                                          ret = xPopCPtr();
                                          xPushPtr((P_)obj);
                                          obj = ret;
                                          goto enterLoop;
                                      }
                                      break;
                                default:        
                                      barf("Invalid update frame during argcheck");
                            }
                        } while (xSp==(P_)xSu);
                        goto enterLoop;
                    }
                    Continue;
                }
            Case(i_ALLOC_AP):
                {
                    StgPtr p;
                    int words = BCO_INSTR_8;
                    SSS; p = grabHpUpd(AP_sizeW(words)); LLL;
                    xPushPtr(p);
                    Continue;
                }
            Case(i_ALLOC_CONSTR):
                {
                    StgPtr p;
                    StgInfoTable* info = bcoConstAddr(bco,BCO_INSTR_8);
                    SSS; p = grabHpNonUpd(sizeW_fromITBL(info)); LLL;
                    SET_HDR((StgClosure*)p,info,??);
                    xPushPtr(p);
                    Continue;
                }
            Case(i_ALLOC_CONSTR_big):
                {
                    StgPtr p;
                    int x = BCO_INSTR_16;
                    StgInfoTable* info = bcoConstAddr(bco,x);
                    SSS; p = grabHpNonUpd(sizeW_fromITBL(info)); LLL;
                    SET_HDR((StgClosure*)p,info,??);
                    xPushPtr(p);
                    Continue;
                }
            Case(i_MKAP):
                {
                    int x = BCO_INSTR_8;  /* ToDo: Word not Int! */
                    int y = BCO_INSTR_8;
                    StgAP_UPD* o = stgCast(StgAP_UPD*,xStackPtr(x));
                    SET_HDR(o,&AP_UPD_info,??);
                    o->n_args = y;
                    o->fun    = stgCast(StgClosure*,xPopPtr());
                    for(x=0; x < y; ++x) {
                        payloadWord(o,x) = xPopWord();
                    }
                    IF_DEBUG(evaluator,
                             fprintf(stderr,"\tBuilt "); 
                             SSS; 
                             printObj(stgCast(StgClosure*,o)); 
                             LLL;
                    );
                    Continue;
                }
            Case(i_MKAP_big):
                {
                    int x, y;
                    StgAP_UPD* o;
                    x = BCO_INSTR_16;
                    y = BCO_INSTR_16;
                    o = stgCast(StgAP_UPD*,xStackPtr(x));
                    SET_HDR(o,&AP_UPD_info,??);
                    o->n_args = y;
                    o->fun    = stgCast(StgClosure*,xPopPtr());
                    for(x=0; x < y; ++x) {
                        payloadWord(o,x) = xPopWord();
                    }
                    IF_DEBUG(evaluator,
                             fprintf(stderr,"\tBuilt "); 
                             SSS;
                             printObj(stgCast(StgClosure*,o));
                             LLL;
                    );
                    Continue;
                }
            Case(i_MKPAP):
                {
                    int x = BCO_INSTR_8;
                    int y = BCO_INSTR_8;
                    StgPAP* o = stgCast(StgPAP*,xStackPtr(x));
                    SET_HDR(o,&PAP_info,??);
                    o->n_args = y;
                    o->fun    = stgCast(StgClosure*,xPopPtr());
                    for(x=0; x < y; ++x) {
                        payloadWord(o,x) = xPopWord();
                    }
                    IF_DEBUG(evaluator,
                             fprintf(stderr,"\tBuilt "); 
                             SSS;
                             printObj(stgCast(StgClosure*,o));
                             LLL;
                            );
                    Continue;
                }
            Case(i_PACK):
                {
                    int offset = BCO_INSTR_8;
                    StgClosure* o = stgCast(StgClosure*,xStackPtr(offset));
                    const StgInfoTable* info = get_itbl(o);
                    nat p  = info->layout.payload.ptrs; 
                    nat np = info->layout.payload.nptrs; 
                    nat i;
                    for(i=0; i < p; ++i) {
                        o->payload[i] = xPopCPtr();
                    }
                    for(i=0; i < np; ++i) {
                        payloadWord(o,p+i) = 0xdeadbeef;
                    }
                    IF_DEBUG(evaluator,
                             fprintf(stderr,"\tBuilt "); 
                             SSS;
                             printObj(stgCast(StgClosure*,o));
                             LLL;
                             );
                    Continue;
                }
            Case(i_PACK_big):
                {
                    int offset = BCO_INSTR_16;
                    StgClosure* o = stgCast(StgClosure*,xStackPtr(offset));
                    const StgInfoTable* info = get_itbl(o);
                    nat p  = info->layout.payload.ptrs; 
                    nat np = info->layout.payload.nptrs; 
                    nat i;
                    for(i=0; i < p; ++i) {
                        o->payload[i] = xPopCPtr();
                    }
                    for(i=0; i < np; ++i) {
                        payloadWord(o,p+i) = 0xdeadbeef;
                    }
                    IF_DEBUG(evaluator,
                             fprintf(stderr,"\tBuilt "); 
                             SSS;
                             printObj(stgCast(StgClosure*,o));
                             LLL;
                             );
                    Continue;
                }
            Case(i_SLIDE):
                {
                    int x = BCO_INSTR_8;
                    int y = BCO_INSTR_8;
                    ASSERT(xSp+x+y <= stgCast(StgPtr,xSu));
                    /* a_1, .. a_x, b_1, .. b_y, s => a_1, .. a_x, s */
                    while(--x >= 0) {
                        xSetStackWord(x+y,xStackWord(x));
                    }
                    xSp += y;
                    Continue;
                }
            Case(i_SLIDE_big):
                {
                    int x, y;
                    x = BCO_INSTR_16;
                    y = BCO_INSTR_16;
                    ASSERT(xSp+x+y <= stgCast(StgPtr,xSu));
                    /* a_1, .. a_x, b_1, .. b_y, s => a_1, .. a_x, s */
                    while(--x >= 0) {
                        xSetStackWord(x+y,xStackWord(x));
                    }
                    xSp += y;
                    Continue;
                }
            Case(i_ENTER):
                {
                    obj = xPopCPtr();
                    goto enterLoop;
                }
            Case(i_RETADDR):
                {
                    xPushPtr(bcoConstPtr(bco,BCO_INSTR_8));
                    xPushPtr(stgCast(StgPtr,&ret_bco_info));
                    Continue;
                }
            Case(i_TEST):
                {
                    int  tag       = BCO_INSTR_8;
                    StgWord offset = BCO_INSTR_16;
                    if (constrTag( (StgClosure*)xStackPtr(0) ) != tag) {
                        bciPtr += offset;
                    }
                    Continue;
                }
            Case(i_UNPACK):
                {
                    StgClosure* o = stgCast(StgClosure*,xStackPtr(0));
                    const StgInfoTable* itbl = get_itbl(o);
                    int i = itbl->layout.payload.ptrs;
                    ASSERT(  itbl->type == CONSTR
                          || itbl->type == CONSTR_STATIC
                          || itbl->type == CONSTR_NOCAF_STATIC
                          || itbl->type == CONSTR_1_0
                          || itbl->type == CONSTR_0_1
                          || itbl->type == CONSTR_2_0
                          || itbl->type == CONSTR_1_1
                          || itbl->type == CONSTR_0_2
                          );
                    while (--i>=0) {
                        xPushCPtr(o->payload[i]);
                    }
                    Continue;
                }
            Case(i_VAR_big):
                {
                    int n = BCO_INSTR_16;
                    StgPtr p = xStackPtr(n);
                    xPushPtr(p);
                    Continue;
                }
            Case(i_VAR):
                {
                    StgPtr p = xStackPtr(BCO_INSTR_8);
                    xPushPtr(p);
                    Continue;
                }
            Case(i_CONST):
                {
                    xPushPtr(stgCast(StgPtr,bcoConstPtr(bco,BCO_INSTR_8)));
                    Continue;
                }
            Case(i_CONST_big):
                {
                    int n = BCO_INSTR_16;
                    xPushPtr(stgCast(StgPtr,bcoConstPtr(bco,n)));
                    Continue;
                }
#ifdef XMLAMBDA
            /* allocate rows, implemented on top of (frozen) Arrays */
            Case(i_ALLOC_ROW):
                {
                    StgMutArrPtrs* p;
                    StgWord n = BCO_INSTR_8;
                    SSS; p = stgCast(StgMutArrPtrs*,grabHpNonUpd(sizeofW(StgMutArrPtrs) + n)); LLL;
                    SET_HDR(p,&MUT_ARR_PTRS_FROZEN_info,CCCS);
                    p->ptrs = n;
                    xPushPtr(p);
                    Continue;
                }
            Case(i_ALLOC_ROW_big):
                {
                    StgMutArrPtrs* p;
                    StgWord n = BCO_INSTR_16;
                    SSS; p = stgCast(StgMutArrPtrs*,grabHpNonUpd(sizeofW(StgMutArrPtrs) + n)); LLL;
                    SET_HDR(p,&MUT_ARR_PTRS_FROZEN_info,CCCS);
                    p->ptrs = n;
                    xPushPtr(p);
                    Continue;
                }

            /* pack values into a row. */
            Case(i_PACK_ROW):
                {
                    StgWord offset   = BCO_INSTR_8;
                    StgMutArrPtrs* p = stgCast(StgMutArrPtrs*,xStackPtr(offset));
                    StgWord        n = p->ptrs;
                    StgWord i;

                    for (i=0; i<n; ++i)
                    {
                      p->payload[i] = xPopCPtr();
                    }
                    IF_DEBUG(evaluator,
                             fprintf(stderr,"\tBuilt "); 
                             SSS;
                             printObj(stgCast(StgClosure*,p));
                             LLL;
                            );
                    Continue;
                }
            Case(i_PACK_ROW_big):
                {
                    StgWord offset   = BCO_INSTR_16;
                    StgMutArrPtrs* p = stgCast(StgMutArrPtrs*,xStackPtr(offset));
                    StgWord        n = p->ptrs;
                    StgWord i;

                    for (i=0; i<n; ++i)
                    {
                      p->payload[i] = xPopCPtr();
                    }
                    IF_DEBUG(evaluator,
                             fprintf(stderr,"\tBuilt "); 
                             SSS;
                             printObj(stgCast(StgClosure*,p));
                             LLL;
                            );
                    Continue;
                }
                
            /* extract all fields of a row */
            Case(i_UNPACK_ROW):
                {
                    StgMutArrPtrs* p = stgCast(StgMutArrPtrs*,xStackPtr(0));
                    nat i = p->ptrs;
                    while (i > 0)
                    {
                      i--;
                      xPushCPtr(p->payload[i]);
                    }
                    Continue;
                }
      
            /* Trivial row (unit) */
            Case(i_CONST_ROW_TRIV):
                {
                    StgMutArrPtrs* p;
                    SSS; p = stgCast(StgMutArrPtrs*,grabHpNonUpd(sizeofW(StgMutArrPtrs) + 0)); LLL;
                    SET_HDR(p,&MUT_ARR_PTRS_FROZEN_info,CCCS);
                    p->ptrs = 0;
                    xPushPtr(p);
                    Continue;
                }
            
            /* pack values into an Inj */
            Case(i_PACK_INJ_VAR):
                {
                    const int size  = CONSTR_sizeW(sizeofW(StgPtr),sizeofW(StgWord));
                    StgWord offset  = BCO_INSTR_8;
                    
                    StgClosure* o;                    
                    SSS; o = (StgClosure*)grabHpNonUpd(size); LLL;
                    SET_HDR(o,Inj_con_info,??);
                    
                    payloadWord(o,sizeofW(StgPtr)) = xTaggedStackWord(offset);
                    payloadPtr(o,0)                = xPopPtr();                                        
                    
                    IF_DEBUG(evaluator,
                             fprintf(stderr,"\tBuilt "); 
                             SSS;
                             printObj(stgCast(StgClosure*,o));
                             LLL;
                             );
                    xPushPtr(stgCast(StgPtr,o));
                    Continue;
                }
            Case(i_PACK_INJ_VAR_big):
                {
                    const int size  = CONSTR_sizeW(sizeofW(StgPtr),sizeofW(StgWord));
                    StgWord offset  = BCO_INSTR_16;
                    
                    StgClosure* o;                    
                    SSS; o = (StgClosure*)grabHpNonUpd(size); LLL;
                    SET_HDR(o,Inj_con_info,??);

                    payloadWord(o,sizeofW(StgPtr)) = xTaggedStackWord(offset);
                    payloadPtr(o,0)                = xPopPtr();                    

                    IF_DEBUG(evaluator,
                             fprintf(stderr,"\tBuilt "); 
                             SSS;
                             printObj(stgCast(StgClosure*,o));
                             LLL;
                             );
                    xPushPtr(stgCast(StgPtr,o));
                    Continue;
                }
            Case(i_PACK_INJ_CONST_8):
                {
                    const int size  = CONSTR_sizeW(sizeofW(StgPtr),sizeofW(StgWord));
                    StgWord witness = BCO_INSTR_8;
                    
                    StgClosure* o;                    
                    SSS; o = (StgClosure*)grabHpNonUpd(size); LLL;
                    SET_HDR(o,Inj_con_info,??);

                    payloadWord(o,sizeofW(StgPtr)) = witness;
                    payloadPtr(o,0)                = xPopPtr();                    

                    IF_DEBUG(evaluator,
                             fprintf(stderr,"\tBuilt "); 
                             SSS;
                             printObj(stgCast(StgClosure*,o));
                             LLL;
                             );
                    xPushPtr(stgCast(StgPtr,o));
                    Continue;
                }
            Case(i_PACK_INJ_REL_8):
                {
                    const int size   = CONSTR_sizeW(sizeofW(StgPtr),sizeofW(StgWord));
                    StgWord offset   = BCO_INSTR_8;
                    StgWord cwitness = BCO_INSTR_8;

                    StgClosure* o;                    
                    SSS; o = (StgClosure*)grabHpNonUpd(size); LLL;
                    SET_HDR(o,Inj_con_info,??);
                    
                    payloadWord(o,sizeofW(StgPtr)) = xTaggedStackWord(offset) + cwitness;
                    payloadPtr(o,0)                = xPopPtr();                                        
                    
                    IF_DEBUG(evaluator,
                             fprintf(stderr,"\tBuilt "); 
                             SSS;
                             printObj(stgCast(StgClosure*,o));
                             LLL;
                             );
                    xPushPtr(stgCast(StgPtr,o));
                    Continue;
                }
            Case(i_PACK_INJ):
                {
                    const int size  = CONSTR_sizeW(sizeofW(StgPtr),sizeofW(StgWord));
                    
                    StgClosure* o;                    
                    SSS; o = (StgClosure*)grabHpNonUpd(size); LLL;
                    SET_HDR(o,Inj_con_info,??);

                    payloadWord(o,sizeofW(StgPtr)) = xPopTaggedWord();
                    payloadPtr(o,0)                = xPopPtr();                    

                    IF_DEBUG(evaluator,
                             fprintf(stderr,"\tBuilt "); 
                             SSS;
                             printObj(stgCast(StgClosure*,o));
                             LLL;
                             );
                    xPushPtr(stgCast(StgPtr,o));
                    Continue;
                }

            /* Test Inj witnesses. */
            Case(i_TEST_INJ_VAR):
                {
                    StgWord offset = BCO_INSTR_8;
                    StgWord jump   = BCO_INSTR_16;
                    
                    StgWord index  = payloadWord( (StgClosure*)xStackPtr(0), sizeofW(StgPtr) );
                    if (index != xTaggedStackWord(offset) )
                    {
                      bciPtr += jump;
                    }
                    Continue;
                }
            Case(i_TEST_INJ_VAR_big):
                {
                    StgWord offset = BCO_INSTR_16;
                    StgWord jump   = BCO_INSTR_16;
                    
                    StgWord index  = payloadWord( (StgClosure*)xStackPtr(0), sizeofW(StgPtr) );
                    if (index != xTaggedStackWord(offset) )
                    {
                      bciPtr += jump;
                    }
                    Continue;
                }
            Case(i_TEST_INJ_CONST_8):
                {
                    StgWord cwitness = BCO_INSTR_8;
                    StgWord jump     = BCO_INSTR_16;
                    
                    StgWord witness  = payloadWord( (StgClosure*)xStackPtr(0), sizeofW(StgPtr) );
                    if (witness != cwitness )
                    {
                      bciPtr += jump;
                    }
                    Continue;
                }  
            Case(i_TEST_INJ_REL_8):
                {
                    StgWord offset    = BCO_INSTR_8;
                    StgWord cwitness  = BCO_INSTR_8;
                    StgWord jump      = BCO_INSTR_16;
                    
                    StgWord witness  = payloadWord( (StgClosure*)xStackPtr(0), sizeofW(StgPtr) );
                    if (witness != xTaggedStackWord(offset) + cwitness )
                    {
                      bciPtr += jump;
                    }
                    Continue;   
                }
            Case(i_TEST_INJ):
                {
                    StgWord jump     = BCO_INSTR_16;
                    StgWord cwitness = xPopTaggedWord();
                    
                    StgWord witness  = payloadWord( (StgClosure*)xStackPtr(0), sizeofW(StgPtr) );
                    if (witness != cwitness )
                    {
                      bciPtr += jump;
                    }
                    Continue;
                }  

            /* extract the value of an INJ */
            Case(i_UNPACK_INJ):
                {
                    StgClosure* con = stgCast(StgClosure*,xStackPtr(0));
                    
                    ASSERT(get_itbl(con) == Inj_con_info);
                    
                    xPushPtr(payloadPtr(con,0));                    
                    Continue;
                }

            /* optimized witness (word) operations */
            Case(i_CONST_WORD_8):
                {
                    xPushTaggedWord(BCO_INSTR_8);
                    Continue;
                }
            Case(i_ADD_WORD_VAR):
                {
                    StgWord offset  = BCO_INSTR_8;
                    StgWord witness = xTaggedStackWord(offset);
                    witness += xPopTaggedWord();
                    xPushTaggedWord(witness);
                    Continue;
                }
            Case(i_ADD_WORD_VAR_big):
                {
                    StgWord offset  = BCO_INSTR_16;
                    StgWord witness = xTaggedStackWord(offset);
                    witness += xPopTaggedWord();
                    xPushTaggedWord(witness);
                    Continue;
                }           
            Case(i_ADD_WORD_VAR_8):
                { 
                    StgWord offset  = BCO_INSTR_8;
                    StgWord inc     = BCO_INSTR_8;
                    StgWord witness = xTaggedStackWord(offset);
                    xPushTaggedWord(witness + inc);
                    Continue;
                }
#endif /* XMLAMBA */

            Case(i_VOID):
                {
                    SSS; PushTaggedRealWorld(); LLL;
                    Continue;
                }
            Case(i_VAR_INT):
                {
                    StgInt i = xTaggedStackInt(BCO_INSTR_8);
                    xPushTaggedInt(i);
                    Continue;
                }
            Case(i_CONST_INT):
                {
                    xPushTaggedInt(bcoConstInt(bco,BCO_INSTR_8));
                    Continue;
                }
            Case(i_CONST_INT_big):
                {
                    int n = BCO_INSTR_16;
                    xPushTaggedInt(bcoConstInt(bco,n));
                    Continue;
                }
            Case(i_PACK_INT):
                {
                    StgClosure* o;
                    SSS; o = (StgClosure*)grabHpNonUpd(Izh_sizeW); LLL;
                    SET_HDR(o,Izh_con_info,??);
                    payloadWord(o,0) = xPopTaggedInt();
                    IF_DEBUG(evaluator,
                             fprintf(stderr,"\tBuilt "); 
                             SSS;
                             printObj(stgCast(StgClosure*,o));
                             LLL;
                             );
                    xPushPtr(stgCast(StgPtr,o));
                    Continue;
                }
            Case(i_UNPACK_INT):
                {
                    StgClosure* con = stgCast(StgClosure*,xStackPtr(0));
                    /* ASSERT(isIntLike(con)); */
                    xPushTaggedInt(payloadWord(con,0));
                    Continue;
                }
            Case(i_TEST_INT):
                {
                    StgWord offset = BCO_INSTR_16;
                    StgInt  x      = xPopTaggedInt();
                    StgInt  y      = xPopTaggedInt();
                    if (x != y) {
                        bciPtr += offset;
                    }
                    Continue;
                }
            Case(i_CONST_INTEGER):
                {
                    StgPtr p;
                    int n;
                    char* s = bcoConstAddr(bco,BCO_INSTR_8);
                    SSS;
                    n = size_fromStr(s);
                    p = CreateByteArrayToHoldInteger(n);
                    do_fromStr ( s, n, IntegerInsideByteArray(p));
                    SloppifyIntegerEnd(p);
		    LLL;
                    xPushPtr(p);
                    Continue;
                }
            Case(i_VAR_WORD):
                {
        	    StgWord w = xTaggedStackWord(BCO_INSTR_8);
                    xPushTaggedWord(w);
                    Continue;
                }
            Case(i_CONST_WORD):
                {
                    xPushTaggedWord(bcoConstWord(bco,BCO_INSTR_8));
                    Continue;
                }
            Case(i_CONST_WORD_big):
                {
                    StgWord n = BCO_INSTR_16;
                    xPushTaggedWord(bcoConstWord(bco,n));
                    Continue;
                }    
            Case(i_PACK_WORD):
                {
                    StgClosure* o;
                    SSS; o = (StgClosure*)grabHpNonUpd(Wzh_sizeW); LLL;
                    SET_HDR(o,Wzh_con_info,??);
                    payloadWord(o,0) = xPopTaggedWord();
                    IF_DEBUG(evaluator,
                             fprintf(stderr,"\tBuilt "); 
                             SSS;
                             printObj(stgCast(StgClosure*,o)); 
                             LLL;
                            );
                    xPushPtr(stgCast(StgPtr,o));
                    Continue;
                }
            Case(i_UNPACK_WORD):
                {
                    StgClosure* con = stgCast(StgClosure*,xStackPtr(0));
                    /* ASSERT(isWordLike(con)); */
                    xPushTaggedWord(payloadWord(con,0));
                    Continue;
                }
            Case(i_VAR_ADDR):
                {
                    StgAddr a = xTaggedStackAddr(BCO_INSTR_8);
                    xPushTaggedAddr(a);
                    Continue;
                }
            Case(i_CONST_ADDR):
                {
                    xPushTaggedAddr(bcoConstAddr(bco,BCO_INSTR_8));
                    Continue;
                }
            Case(i_CONST_ADDR_big):
                {
                    int n = BCO_INSTR_16;
                    xPushTaggedAddr(bcoConstAddr(bco,n));
                    Continue;
                }
            Case(i_PACK_ADDR):
                {
                    StgClosure* o;
                    SSS; o = (StgClosure*)grabHpNonUpd(Azh_sizeW); LLL;
                    SET_HDR(o,Azh_con_info,??);
                    payloadPtr(o,0) = xPopTaggedAddr();
                    IF_DEBUG(evaluator,
                             fprintf(stderr,"\tBuilt "); 
                             SSS;
                             printObj(stgCast(StgClosure*,o));
                             LLL;
                             );
                    xPushPtr(stgCast(StgPtr,o));
                    Continue;
                }
            Case(i_UNPACK_ADDR):
                {
                    StgClosure* con = (StgClosure*)xStackPtr(0);
                    /* ASSERT(isAddrLike(con)); */
                    xPushTaggedAddr(payloadPtr(con,0));
                    Continue;
                }
            Case(i_VAR_CHAR):
                {
                    StgChar c = xTaggedStackChar(BCO_INSTR_8);
                    xPushTaggedChar(c);
                    Continue;
                }
            Case(i_CONST_CHAR):
                {
                    xPushTaggedChar(bcoConstChar(bco,BCO_INSTR_8));
                    Continue;
                }
            Case(i_PACK_CHAR):
                {
                    StgClosure* o;
                    SSS; o = (StgClosure*)grabHpNonUpd(Czh_sizeW); LLL;
                    SET_HDR(o,Czh_con_info,??);
                    payloadWord(o,0) = xPopTaggedChar();
                    xPushPtr(stgCast(StgPtr,o));
                    IF_DEBUG(evaluator,
                             fprintf(stderr,"\tBuilt "); 
                             SSS;
                             printObj(stgCast(StgClosure*,o));
                             LLL;
                             );
                    Continue;
                }
            Case(i_UNPACK_CHAR):
                {
                    StgClosure* con = stgCast(StgClosure*,xStackPtr(0));
                    /* ASSERT(isCharLike(con)); */
                    xPushTaggedChar(payloadWord(con,0));
                    Continue;
                }
            Case(i_VAR_FLOAT):
                {
                    StgFloat f = xTaggedStackFloat(BCO_INSTR_8);
                    xPushTaggedFloat(f);
                    Continue;
                }
            Case(i_CONST_FLOAT):
                {
                    xPushTaggedFloat(bcoConstFloat(bco,BCO_INSTR_8));
                    Continue;
                }
            Case(i_PACK_FLOAT):
                {
                    StgClosure* o;
                    SSS; o = (StgClosure*)grabHpNonUpd(Fzh_sizeW); LLL;
                    SET_HDR(o,Fzh_con_info,??);
                    ASSIGN_FLT(&payloadWord(o,0),xPopTaggedFloat());
                    IF_DEBUG(evaluator,
                             fprintf(stderr,"\tBuilt "); 
                             SSS;
                             printObj(stgCast(StgClosure*,o));
                             LLL;
                             );
                    xPushPtr(stgCast(StgPtr,o));
                    Continue;
                }
            Case(i_UNPACK_FLOAT):
                {
                    StgClosure* con = stgCast(StgClosure*,xStackPtr(0));
                    /* ASSERT(isFloatLike(con)); */
                    xPushTaggedFloat(PK_FLT(&payloadWord(con,0)));
                    Continue;
                }
            Case(i_VAR_DOUBLE):
                {
                    StgDouble d = xTaggedStackDouble(BCO_INSTR_8);
                    xPushTaggedDouble(d);
                    Continue;
                }
            Case(i_CONST_DOUBLE):
                {
                    xPushTaggedDouble(bcoConstDouble(bco,BCO_INSTR_8));
                    Continue;
                }
            Case(i_CONST_DOUBLE_big):
                {
                    int n = BCO_INSTR_16;
                    xPushTaggedDouble(bcoConstDouble(bco,n));
                    Continue;
                }
            Case(i_PACK_DOUBLE):
                {
                    StgClosure* o;
                    SSS; o = (StgClosure*)grabHpNonUpd(Dzh_sizeW); LLL;
                    SET_HDR(o,Dzh_con_info,??);
                    ASSIGN_DBL(&payloadWord(o,0),xPopTaggedDouble());
                    IF_DEBUG(evaluator,
                             fprintf(stderr,"\tBuilt "); 
                             printObj(stgCast(StgClosure*,o));
                             );
                    xPushPtr(stgCast(StgPtr,o));
                    Continue;
                }
            Case(i_UNPACK_DOUBLE):
                {
                    StgClosure* con = stgCast(StgClosure*,xStackPtr(0));
                    /* ASSERT(isDoubleLike(con)); */
                    xPushTaggedDouble(PK_DBL(&payloadWord(con,0)));
                    Continue;
                }
            Case(i_VAR_STABLE):
                {   
                    StgStablePtr s = xTaggedStackStable(BCO_INSTR_8);
                    xPushTaggedStable(s);
                    Continue;
                }
            Case(i_PACK_STABLE):
                {
                    StgClosure* o;
                    SSS; o = (StgClosure*)grabHpNonUpd(Stablezh_sizeW); LLL;
                    SET_HDR(o,StablePtr_con_info,??);
                    payloadWord(o,0) = (W_)xPopTaggedStable();
                    IF_DEBUG(evaluator,
                             fprintf(stderr,"\tBuilt "); 
                             SSS;
                             printObj(stgCast(StgClosure*,o));
                             LLL;
                             );
                    xPushPtr(stgCast(StgPtr,o));
                    Continue;
                }
            Case(i_UNPACK_STABLE):
                {
                    StgClosure* con = (StgClosure*)xStackPtr(0);
                    /* ASSERT(isStableLike(con)); */
                    xPushTaggedStable(payloadWord(con,0));
                    Continue;
                }
            Case(i_PRIMOP1):
                {
                    int   i;
                    void* p;
                    i = BCO_INSTR_8;
                    SSS; p = enterBCO_primop1 ( i ); LLL;
                    if (p) { obj = p; goto enterLoop; };
                    Continue;
                }
            Case(i_PRIMOP2):
                {
                    int      i, trc, pc_saved;
                    void*    p;
                    StgBCO*  bco_tmp;
                    trc      = 12345678; /* Assume != any StgThreadReturnCode */
                    i        = BCO_INSTR_8;
                    pc_saved = PC; 
                    bco_tmp  = bco;
                    SSS;
                    p        = enterBCO_primop2 ( i, &trc, &bco_tmp, cap, 
						  &hugsBlock ); 
                    LLL;
                    bco      = bco_tmp;
                    bciPtr   = &(bcoInstr(bco,pc_saved));
                    if (p) {
                       if (trc == 12345678) {
                          /* we want to enter p */
                          obj = p; goto enterLoop;
                       } else {
                          /* trc is the the StgThreadReturnCode for 
			   * this thread */
			 RETURN((StgThreadReturnCode)trc);
                       };
                    }
                    Continue;
                }
        
            /* combined insns, created by peephole opt */
            Case(i_SE):
                {
                    int x = BCO_INSTR_8;
                    int y = BCO_INSTR_8;
                    ASSERT(xSp+x+y <= stgCast(StgPtr,xSu));
                    /* a_1, .. a_x, b_1, .. b_y, s => a_1, .. a_x, s */
                    if (x == 1) {
                       obj = xPopCPtr();
                       xSp += y;
                       goto enterLoop;
                    } else {
                       while(--x >= 0) {
                           xSetStackWord(x+y,xStackWord(x));
                       }
                       xSp += y;
                       obj = xPopCPtr();
                    }
                    goto enterLoop;
                }
            Case(i_VV):
                {
                    StgPtr p;
                    p = xStackPtr(BCO_INSTR_8);
                    xPushPtr(p);
                    p = xStackPtr(BCO_INSTR_8);
                    xPushPtr(p);
                    Continue;
                }
            Case(i_RV):
                {
                    StgPtr p;
                    xPushPtr(bcoConstPtr(bco,BCO_INSTR_8));
                    xPushPtr(stgCast(StgPtr,&ret_bco_info));
                    p = xStackPtr(BCO_INSTR_8);
                    xPushPtr(p);
                    Continue;
                }
            Case(i_RVE):
                {
                    StgPtr retaddr = bcoConstPtr(bco,BCO_INSTR_8);
                    StgPtr ptr = xStackPtr(BCO_INSTR_8);

                    /* A shortcut.  We're going to push the address of a
                       return continuation, and then enter a variable, so
                       that when the var is evaluated, we return to the
                       continuation.  The shortcut is: if the var is a 
                       constructor, don't bother to enter it.  Instead,
                       push the variable on the stack (since this is what
                       the continuation expects) and jump directly to the
                       continuation.
                     */
                    if (get_itbl((StgClosure*)ptr)->type == CONSTR) {
                       xPushPtr(ptr);
                       obj = (StgClosure*)retaddr;
                       IF_DEBUG(evaluator,
                                fprintf(stderr, "object to enter is a constructor -- "
                                        "jumping directly to return continuation\n" );
                               );
                       goto bco_entry;
                    }

                    /* This is the normal, non-short-cut route */
                    xPushPtr(retaddr);
                    xPushPtr(stgCast(StgPtr,&ret_bco_info));
                    obj = (StgClosure*)ptr;
                    goto enterLoop;
                }


            Case(i_VAR_DOUBLE_big):
            Case(i_CONST_FLOAT_big):
            Case(i_VAR_FLOAT_big):
            Case(i_CONST_CHAR_big):
            Case(i_VAR_CHAR_big):
            Case(i_VAR_ADDR_big):
            Case(i_VAR_STABLE_big):
            Case(i_CONST_INTEGER_big):
            Case(i_VAR_INT_big):
            Case(i_VAR_WORD_big):
            Case(i_RETADDR_big):
            Case(i_ALLOC_PAP):
#ifndef XMLAMBDA
            Case(i_UNPACK_INJ):
            Case(i_UNPACK_ROW):
            Case(i_TEST_INJ_CONST):
            Case(i_TEST_INJ_big):
            Case(i_TEST_INJ):
            Case(i_PACK_INJ_CONST):
            Case(i_PACK_INJ_big):
            Case(i_PACK_INJ):
            Case(i_PACK_ROW_big):
            Case(i_PACK_ROW):
            Case(i_ALLOC_ROW_big):
            Case(i_ALLOC_ROW):
#endif
                    bciPtr--;
                    printf ( "\n\n" );
                    disInstr ( bco, PC );
                    barf("\nUnrecognised instruction");
        
            EndDispatch
        
            barf("enterBCO: ran off end of loop");
            break;
        }

#           undef LoopTopLabel
#           undef Case
#           undef Continue
#           undef Dispatch
#           undef EndDispatch

            /* ---------------------------------------------------- */
            /* End of the bytecode evaluator                        */
            /* ---------------------------------------------------- */

    case CAF_UNENTERED:
        {
            StgBlockingQueue* bh;
            StgCAF* caf = (StgCAF*)obj;
            if (xSp - sizeofW(StgUpdateFrame) < xSpLim) {
                xPushCPtr(obj); /* code to restart with */
                RETURN(StackOverflow);
            }
            SSS; bh = (StgBlockingQueue*)grabHpUpd(BLACKHOLE_sizeW()); LLL;
            SET_INFO(bh,&CAF_BLACKHOLE_info);
            bh->blocking_queue = EndTSOQueue;
            IF_DEBUG(gccafs,
                     fprintf(stderr,"Created CAF_BLACKHOLE %p for CAF %p"
                                    " in evaluator\n",bh,caf));
            SET_INFO(caf,&CAF_ENTERED_info);
            caf->value = (StgClosure*)bh;

            SSS; newCAF_made_by_Hugs(caf); LLL;

            xPushUpdateFrame(bh,0);
            xSp -= sizeofW(StgUpdateFrame);
            obj = caf->body;
            goto enterLoop;
        }
    case CAF_ENTERED:
        {
            StgCAF* caf = (StgCAF*)obj;
            obj = caf->value; /* it's just a fancy indirection */
            goto enterLoop;
        }
    case BLACKHOLE:
    case SE_BLACKHOLE:
    case CAF_BLACKHOLE:
    case SE_CAF_BLACKHOLE:
        {
            /* Let the scheduler figure out what to do :-) */
            cap->rCurrentTSO->what_next = ThreadEnterGHC;
            xPushCPtr(obj);
            RETURN(ThreadYielding);
        }
    case AP_UPD:
        {
            StgAP_UPD* ap = stgCast(StgAP_UPD*,obj);
            int i = ap->n_args;
            if (xSp - (i + sizeofW(StgUpdateFrame)) < xSpLim) {
                xPushCPtr(obj); /* code to restart with */
                RETURN(StackOverflow);
            }
            /* ToDo: look for xSp==xSu && stackInt(0) == UPD_FRAME 
               and insert an indirection immediately  */
            xPushUpdateFrame(ap,0);
            xSp -= sizeofW(StgUpdateFrame);
            while (--i >= 0) {
                xPushWord(payloadWord(ap,i));
            }
            obj = ap->fun;
#ifdef EAGER_BLACKHOLING
#warn  LAZY_BLACKHOLING is default for StgHugs
#error Dont know if EAGER_BLACKHOLING works in StgHugs
            {
            /* superfluous - but makes debugging easier */
            StgBlackHole* bh = stgCast(StgBlackHole*,ap);
            SET_INFO(bh,&BLACKHOLE_info);
            bh->blocking_queue = EndTSOQueue;
            IF_DEBUG(gccafs,
                     fprintf(stderr,"Eagerly blackholed AP_UPD %p in evaluator\n",bh));
            /* printObj(bh); */
            }
#endif /* EAGER_BLACKHOLING */
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
                xPushWord(payloadWord(pap,i));
            }
            obj = pap->fun;
            goto enterLoop;
        }
    case IND:
        {
            obj = stgCast(StgInd*,obj)->indirectee;
            goto enterLoop;
        }
    case IND_OLDGEN:
        {
            obj = stgCast(StgIndOldGen*,obj)->indirectee;
            goto enterLoop;
        }
    case CONSTR:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_2_0:
    case CONSTR_1_1:
    case CONSTR_0_2:
    case CONSTR_INTLIKE:
    case CONSTR_CHARLIKE:
    case CONSTR_STATIC:
    case CONSTR_NOCAF_STATIC:
#ifdef XMLAMBDA
/* rows are mutarrays and should be treated as constructors. */
    case MUT_ARR_PTRS_FROZEN:
#endif
        {
            while (1) {
                switch (get_itbl(stgCast(StgClosure*,xSp))->type) {
                case CATCH_FRAME:
                        SSS; PopCatchFrame(); LLL;
                        break;
                case UPDATE_FRAME:
                        xPopUpdateFrame(obj);
                        break;
                case SEQ_FRAME:
                        SSS; PopSeqFrame(); LLL;
                        break;
                case STOP_FRAME:
                    {
                        ASSERT(xSp==(P_)xSu);
                        IF_DEBUG(evaluator,
                                 SSS;
                                 fprintf(stderr, "hit a STOP_FRAME\n");
                                 printObj(obj);
                                 fprintf(stderr,"xSp = %p\txSu = %p\n", xSp, xSu);
                                 printStack(xSp,cap->rCurrentTSO->stack
                                                + cap->rCurrentTSO->stack_size,xSu);
                                 LLL;
                                 );
                        cap->rCurrentTSO->what_next = ThreadComplete;
                        SSS; PopStopFrame(obj); LLL;
                        xPushPtr((P_)obj);
                        RETURN(ThreadFinished);
                    }
                case RET_BCO:
                    {
                        StgClosure* ret;
                        (void)xPopPtr();
                        ret = xPopCPtr();
                        xPushPtr((P_)obj);
                        obj = ret;
                        goto bco_entry;
                        /* was: goto enterLoop;
                           But we know that obj must be a bco now, so jump directly.
                        */
                    }
                case RET_SMALL:  /* return to GHC */
                case RET_VEC_SMALL:
                case RET_BIG:
                case RET_VEC_BIG:
                        cap->rCurrentTSO->what_next = ThreadEnterGHC;
                        xPushCPtr(obj);
                        RETURN(ThreadYielding);
                default:
                        belch("entered CONSTR with invalid continuation on stack");
                        IF_DEBUG(evaluator,
                                 SSS;
                                 printObj(stgCast(StgClosure*,xSp));
                                 LLL;
                                 );
                        barf("bailing out");
                }
            }
        }
    default:
        {
            //SSS;
            //fprintf(stderr, "enterCountI = %d\n", enterCountI);
            //fprintf(stderr, "entering unknown closure -- yielding to sched\n"); 
            //printObj(obj);
            //LLL;
            cap->rCurrentTSO->what_next = ThreadEnterGHC;
            xPushCPtr(obj); /* code to restart with */
            RETURN(ThreadYielding);
        }
    }
    barf("Ran off the end of enter - yoiks");
    ASSERT(0);
}

#undef RETURN
#undef BCO_INSTR_8
#undef BCO_INSTR_16
#undef SSS
#undef LLL
#undef PC
#undef xPushPtr
#undef xPopPtr
#undef xPushCPtr
#undef xPopCPtr
#undef xPopWord
#undef xStackPtr
#undef xStackWord
#undef xSetStackWord
#undef xPushTag
#undef xPopTag
#undef xPushTaggedInt
#undef xPopTaggedInt
#undef xTaggedStackInt
#undef xPushTaggedWord
#undef xPopTaggedWord
#undef xTaggedStackWord
#undef xPushTaggedAddr
#undef xTaggedStackAddr
#undef xPopTaggedAddr
#undef xPushTaggedStable
#undef xTaggedStackStable
#undef xPopTaggedStable
#undef xPushTaggedChar
#undef xTaggedStackChar
#undef xPopTaggedChar
#undef xPushTaggedFloat
#undef xTaggedStackFloat
#undef xPopTaggedFloat
#undef xPushTaggedDouble
#undef xTaggedStackDouble
#undef xPopTaggedDouble
#undef xPopUpdateFrame
#undef xPushUpdateFrame


/* --------------------------------------------------------------------------
 * Supporting routines for primops
 * ------------------------------------------------------------------------*/

static inline void            PushTag            ( StackTag    t ) 
   { *(--gSp) = t; }
       inline void            PushPtr            ( StgPtr      x ) 
   { *(--stgCast(StgPtr*,gSp))  = x; }
static inline void            PushCPtr           ( StgClosure* x ) 
   { *(--stgCast(StgClosure**,gSp)) = x; }
static inline void            PushInt            ( StgInt      x ) 
   { *(--stgCast(StgInt*,gSp))  = x; }
static inline void            PushWord           ( StgWord     x ) 
   { *(--stgCast(StgWord*,gSp)) = x; }
                                                     
                                                 
static inline void            checkTag           ( StackTag t1, StackTag t2 ) 
   { ASSERT(t1 == t2);}
static inline void            PopTag             ( StackTag t ) 
   { checkTag(t,*(gSp++));    }
       inline StgPtr          PopPtr             ( void )       
   { return *stgCast(StgPtr*,gSp)++; }
static inline StgClosure*     PopCPtr            ( void )       
   { return *stgCast(StgClosure**,gSp)++; }
static inline StgInt          PopInt             ( void )       
   { return *stgCast(StgInt*,gSp)++;  }
static inline StgWord         PopWord            ( void )       
   { return *stgCast(StgWord*,gSp)++; }

static inline StgPtr          stackPtr           ( StgStackOffset i ) 
   { return *stgCast(StgPtr*, gSp+i); }
static inline StgInt          stackInt           ( StgStackOffset i ) 
   { return *stgCast(StgInt*, gSp+i); }
static inline StgWord         stackWord          ( StgStackOffset i ) 
   { return *stgCast(StgWord*,gSp+i); }
                              
static inline void            setStackWord       ( StgStackOffset i, StgWord w ) 
   { gSp[i] = w; }

#ifdef XMLAMBDA
static inline void            setStackPtr        ( StgStackOffset i, StgPtr p )
   { *(stgCast(StgPtr*, gSp+i)) = p; }
#endif

static inline void            PushTaggedRealWorld( void            ) 
   { PushTag(REALWORLD_TAG);  }
       inline void            PushTaggedInt      ( StgInt        x ) 
   { gSp -= sizeofW(StgInt);        *gSp = x;          PushTag(INT_TAG);    }
       inline void            PushTaggedWord     ( StgWord       x ) 
   { gSp -= sizeofW(StgWord);       *gSp = x;          PushTag(WORD_TAG);   }
       inline void            PushTaggedAddr     ( StgAddr       x ) 
   { gSp -= sizeofW(StgAddr);       *gSp = (W_)x;      PushTag(ADDR_TAG);   }
       inline void            PushTaggedChar     ( StgChar       x ) 
   { gSp -= sizeofW(StgChar);         *gSp = stgCast(StgWord,x); PushTag(CHAR_TAG); }
       inline void            PushTaggedFloat    ( StgFloat      x ) 
   { gSp -= sizeofW(StgFloat);      ASSIGN_FLT(gSp,x); PushTag(FLOAT_TAG);  }
       inline void            PushTaggedDouble   ( StgDouble     x ) 
   { gSp -= sizeofW(StgDouble);     ASSIGN_DBL(gSp,x); PushTag(DOUBLE_TAG); }
       inline void            PushTaggedStablePtr   ( StgStablePtr  x ) 
   { gSp -= sizeofW(StgStablePtr);  *gSp = (W_)x;      PushTag(STABLE_TAG); }
static inline void            PushTaggedBool     ( int           x ) 
   { PushTaggedInt(x); }



static inline void            PopTaggedRealWorld ( void ) 
   { PopTag(REALWORLD_TAG); }
       inline StgInt          PopTaggedInt       ( void ) 
   { StgInt    r; PopTag(INT_TAG);     r = *stgCast(StgInt*,  gSp);      
     gSp += sizeofW(StgInt);        return r;}
       inline StgWord         PopTaggedWord      ( void ) 
   { StgWord   r; PopTag(WORD_TAG);    r = *stgCast(StgWord*, gSp);      
     gSp += sizeofW(StgWord);       return r;}
       inline StgAddr         PopTaggedAddr      ( void ) 
   { StgAddr   r; PopTag(ADDR_TAG);    r = *stgCast(StgAddr*, gSp);      
     gSp += sizeofW(StgAddr);       return r;}
       inline StgChar         PopTaggedChar      ( void ) 
   { StgChar   r; PopTag(CHAR_TAG);    r = stgCast(StgChar, *gSp);       
     gSp += sizeofW(StgChar);       return r;}
       inline StgFloat        PopTaggedFloat     ( void ) 
   { StgFloat  r; PopTag(FLOAT_TAG);   r = PK_FLT(gSp);                  
     gSp += sizeofW(StgFloat);      return r;}
       inline StgDouble       PopTaggedDouble    ( void ) 
   { StgDouble r; PopTag(DOUBLE_TAG);  r = PK_DBL(gSp);                  
     gSp += sizeofW(StgDouble);     return r;}
       inline StgStablePtr    PopTaggedStablePtr    ( void ) 
   { StgStablePtr r; PopTag(STABLE_TAG);  r = *stgCast(StgStablePtr*, gSp); 
     gSp += sizeofW(StgStablePtr);  return r;}



static inline StgInt          taggedStackInt     ( StgStackOffset i ) 
   { checkTag(INT_TAG,gSp[i]);     return *stgCast(StgInt*,         gSp+1+i); }
static inline StgWord         taggedStackWord    ( StgStackOffset i ) 
   { checkTag(WORD_TAG,gSp[i]);    return *stgCast(StgWord*,        gSp+1+i); }
static inline StgAddr         taggedStackAddr    ( StgStackOffset i ) 
   { checkTag(ADDR_TAG,gSp[i]);    return *stgCast(StgAddr*,        gSp+1+i); }
static inline StgChar         taggedStackChar    ( StgStackOffset i ) 
   { checkTag(CHAR_TAG,gSp[i]);    return stgCast(StgChar, *(gSp+1+i))   ; }
static inline StgFloat        taggedStackFloat   ( StgStackOffset i ) 
   { checkTag(FLOAT_TAG,gSp[i]);   return PK_FLT(gSp+1+i); }
static inline StgDouble       taggedStackDouble  ( StgStackOffset i ) 
   { checkTag(DOUBLE_TAG,gSp[i]);  return PK_DBL(gSp+1+i); }
static inline StgStablePtr    taggedStackStable  ( StgStackOffset i ) 
   { checkTag(STABLE_TAG,gSp[i]);  return *stgCast(StgStablePtr*,   gSp+1+i); }


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
 * o Error handling/catching (based on catchzh_fast and friends in Prims.hc)
 * o Seq frames              (based on seq_frame_entry in Prims.hc)
 * o Stop frames
 * ------------------------------------------------------------------------*/

static inline void PopUpdateFrame ( StgClosure* obj )
{
    /* NB: doesn't assume that gSp == gSu */
    IF_DEBUG(evaluator,
             fprintf(stderr,  "Updating ");
             printPtr(stgCast(StgPtr,gSu->updatee)); 
             fprintf(stderr,  " with ");
             printObj(obj);
             fprintf(stderr,"gSp = %p\tgSu = %p\n\n", gSp, gSu);
             );
#ifdef EAGER_BLACKHOLING
#warn  LAZY_BLACKHOLING is default for StgHugs
#error Dont know if EAGER_BLACKHOLING works in StgHugs
    ASSERT(get_itbl(gSu->updatee)->type == BLACKHOLE
           || get_itbl(gSu->updatee)->type == SE_BLACKHOLE
           || get_itbl(gSu->updatee)->type == CAF_BLACKHOLE
           || get_itbl(gSu->updatee)->type == SE_CAF_BLACKHOLE
           );
#endif /* EAGER_BLACKHOLING */
    UPD_IND(gSu->updatee,obj);
    gSp = stgCast(StgStackPtr,gSu) + sizeofW(StgUpdateFrame);
    gSu = gSu->link;
}

static inline void PopStopFrame ( StgClosure* obj )
{
    /* Move gSu just off the end of the stack, we're about to gSpam the
     * STOP_FRAME with the return value.
     */
    gSu = stgCast(StgUpdateFrame*,gSp+1);  
    *stgCast(StgClosure**,gSp) = obj;
}

static inline void PushCatchFrame ( StgClosure* handler )
{
    StgCatchFrame* fp;
    /* ToDo: stack check! */
    gSp -= sizeofW(StgCatchFrame);
    fp = stgCast(StgCatchFrame*,gSp);
    SET_HDR(fp,(StgInfoTable*)&catch_frame_info,CCCS);
    fp->handler         = handler;
    fp->link            = gSu;
    gSu = stgCast(StgUpdateFrame*,fp);
}

static inline void PopCatchFrame ( void )
{
    /* NB: doesn't assume that gSp == gSu */
    /* fprintf(stderr,"Popping catch frame\n"); */
    gSp = stgCast(StgStackPtr,gSu) + sizeofW(StgCatchFrame);
    gSu = stgCast(StgCatchFrame*,gSu)->link;		
}

static inline void PushSeqFrame ( void )
{
    StgSeqFrame* fp;
    /* ToDo: stack check! */
    gSp -= sizeofW(StgSeqFrame);
    fp = stgCast(StgSeqFrame*,gSp);
    SET_HDR(fp,(StgInfoTable*)&seq_frame_info,CCCS);
    fp->link = gSu;
    gSu = stgCast(StgUpdateFrame*,fp);
}

static inline void PopSeqFrame ( void )
{
    /* NB: doesn't assume that gSp == gSu */
    gSp = stgCast(StgStackPtr,gSu) + sizeofW(StgSeqFrame);
    gSu = stgCast(StgSeqFrame*,gSu)->link;		
}

static inline StgClosure* raiseAnError ( StgClosure* exception )
{
    /* This closure represents the expression 'primRaise E' where E
     * is the exception raised (:: Exception).  
     * It is used to overwrite all the
     * thunks which are currently under evaluation.
     */
    HaskellObj primRaiseClosure
       = getHugs_BCO_cptr_for("primRaise");
    HaskellObj reraiseClosure
       = rts_apply ( primRaiseClosure, exception );
   
    while (1) {
        switch (get_itbl(gSu)->type) {
        case UPDATE_FRAME:
                UPD_IND(gSu->updatee,reraiseClosure);
                gSp = stgCast(StgStackPtr,gSu) + sizeofW(StgUpdateFrame);
                gSu = gSu->link;
                break;
        case SEQ_FRAME:
                PopSeqFrame();
                break;
        case CATCH_FRAME:  /* found it! */
            {
                StgCatchFrame* fp = stgCast(StgCatchFrame*,gSu);
                StgClosure *handler = fp->handler;
                gSu = fp->link; 
                gSp += sizeofW(StgCatchFrame); /* Pop */
                PushCPtr(exception);
                return handler;
	    }
        case STOP_FRAME:
                barf("raiseError: uncaught exception: STOP_FRAME");
        default:
                barf("raiseError: weird activation record");
        }
    }
}


static StgClosure* makeErrorCall ( const char* msg )
{
   /* Note!  the msg string should be allocated in a 
      place which will not get freed -- preferably 
      read-only data of the program.  That's because
      the thunk we build here may linger indefinitely.
      (thinks: probably not so, but anyway ...)
   */
   HaskellObj error 
      = getHugs_BCO_cptr_for("error");
   HaskellObj unpack
      = getHugs_BCO_cptr_for("hugsprimUnpackString");
   HaskellObj thunk
      = rts_apply ( unpack, rts_mkAddr ( (void*)msg ) );
   thunk
      = rts_apply ( error, thunk );
   return 
      (StgClosure*) thunk;
}

#define raiseIndex(where) makeErrorCall("Array index out of range in " where)
#define raiseDiv0(where)  makeErrorCall("Division by zero in " where)

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

#define OP_I_s(e)            \
{                            \
    StgInt x = PopTaggedInt();  \
    PushTaggedStablePtr(e);  \
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

#define OP_s_I(e)             \
{                             \
    StgStablePtr x = PopTaggedStablePtr(); \
    PushTaggedInt(e);         \
}

#define OP_W_W(e)             \
{                             \
    StgWord x = PopTaggedWord(); \
    PushTaggedWord(e);        \
}

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
    PushTaggedStablePtr(r);   \
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


StgPtr CreateByteArrayToHoldInteger ( int nbytes )
{
   StgWord words     = (nbytes+sizeof(W_)-1)/sizeof(W_);
   StgWord size      = sizeofW(StgArrWords) + words;
   StgArrWords* arr  = (StgArrWords*)allocate(size);
   SET_HDR(arr,&ARR_WORDS_info,CCCS);
   arr->words = words;
   ASSERT((W_)nbytes <= arr->words * sizeof(W_));
#ifdef DEBUG
   {StgWord i;
    for (i = 0; i < words; ++i) {
    arr->payload[i] = 0xdeadbeef;
   }}
   { B* b = (B*) &(arr->payload[0]);
     b->used = b->sign = 0;
   }
#endif
   return (StgPtr)arr;
}

B* IntegerInsideByteArray ( StgPtr arr0 )
{
   B* b;
   StgArrWords* arr = (StgArrWords*)arr0;
   ASSERT(GET_INFO(arr) == &ARR_WORDS_info);
   b = (B*) &(arr->payload[0]);
   return b;
}

void SloppifyIntegerEnd ( StgPtr arr0 )
{
   StgArrWords* arr = (StgArrWords*)arr0;
   B* b = (B*) & (arr->payload[0]);
   I_ nwunused = arr->words - sizeofW(B) - (b->used+sizeof(W_)-1)/sizeof(W_);
   if (nwunused >= ((I_)sizeofW(StgArrWords))) {
      StgArrWords* slop;
      b->size -= nwunused * sizeof(W_);
      if (b->size < b->used) b->size = b->used;
      do_renormalise(b);
      ASSERT(is_sane(b));
      arr->words -= nwunused;
      slop = (StgArrWords*)&(arr->payload[arr->words]);
      SET_HDR(slop,&ARR_WORDS_info,CCCS);
      slop->words = nwunused - sizeofW(StgArrWords);
      ASSERT( &(slop->payload[slop->words]) == 
              &(arr->payload[arr->words + nwunused]) );
   }
}

#define OP_Z_Z(op)                                   \
{                                                    \
   B* x     = IntegerInsideByteArray(PopPtr());      \
   int n    = mycat2(size_,op)(x);                   \
   StgPtr p = CreateByteArrayToHoldInteger(n);       \
   mycat2(do_,op)(x,n,IntegerInsideByteArray(p));    \
   SloppifyIntegerEnd(p);                            \
   PushPtr(p);                                       \
}
#define OP_ZZ_Z(op)                                  \
{                                                    \
   B* x     = IntegerInsideByteArray(PopPtr());      \
   B* y     = IntegerInsideByteArray(PopPtr());      \
   int n    = mycat2(size_,op)(x,y);                 \
   StgPtr p = CreateByteArrayToHoldInteger(n);       \
   mycat2(do_,op)(x,y,n,IntegerInsideByteArray(p));  \
   SloppifyIntegerEnd(p);                            \
   PushPtr(p);                                       \
}




#define HEADER_mI(ty,where)          \
    StgArrWords* x = stgCast(StgArrWords*,PopPtr()); \
    nat i = PopTaggedInt();   \
    if (i * sizeof(ty) + (sizeof(ty)) > sizeof(StgWord) * x->words) {        \
        return (raiseIndex(where));  \
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


__attribute__ ((unused))
static void myStackCheck ( Capability* cap )
{
   /* fprintf(stderr, "myStackCheck\n"); */
   if (!(gSpLim <= gSp && gSp <= stgCast(StgPtr,gSu))) {
      fprintf(stderr, "myStackCheck: invalid initial gSp/gSu \n" );
      barf("aborting");
      ASSERT(0);
   }
   while (1) {
      if (!( (P_)gSu >= (P_)cap->rCurrentTSO->stack 
              && 
              (P_)gSu <= (P_)(cap->rCurrentTSO->stack 
                              + cap->rCurrentTSO->stack_size))) {
         fprintf ( stderr, "myStackCheck: gSu out of stack\n" );
         barf("aborting");
         ASSERT(0);
      }
      switch (get_itbl(stgCast(StgClosure*,gSu))->type) {
      case CATCH_FRAME:
         gSu = (StgUpdateFrame*) ((StgCatchFrame*)(gSu))->link;
         break;
      case UPDATE_FRAME:
         gSu = (StgUpdateFrame*) ((StgUpdateFrame*)(gSu))->link;
         break;
      case SEQ_FRAME:
         gSu = (StgUpdateFrame*) ((StgSeqFrame*)(gSu))->link;
         break;
      case STOP_FRAME:
         goto postloop;
      default:
         fprintf(stderr, "myStackCheck: invalid activation record\n"); 
         barf("aborting");
         ASSERT(0);
      }
   }
   postloop:
}


/* --------------------------------------------------------------------------
 * The new bytecode interpreter
 * ------------------------------------------------------------------------*/

/* Sp points to the lowest live word on the stack. */

#define StackWord(n)  ((W_*)Sp)[n]
#define BCO_NEXT      bco_instrs[bciPtr++]
#define BCO_PTR(n)    bco_ptrs[n]


{
      case bci_PUSH_L: {
         int o1 = BCO_NEXT;
         StackWord(-1) = StackWord(o1);
         Sp--;
         break;
      }
      case bci_PUSH_LL: {
         int o1 = BCO_NEXT;
         int o2 = BCO_NEXT;
         StackWord(-1) = StackWord(o1);
         StackWord(-2) = StackWord(o2);
         Sp -= 2;
         break;
      }
      case bci_PUSH_LLL: {
         int o1 = BCO_NEXT;
         int o2 = BCO_NEXT;
         int o3 = BCO_NEXT;
         StackWord(-1) = StackWord(o1);
         StackWord(-2) = StackWord(o2);
         StackWord(-3) = StackWord(o3);
         Sp -= 3;
         break;
      }
      case bci_PUSH_G: {
         int o1 = BCO_NEXT;
         StackWord(-1) = BCO_PTR(o1);
         Sp -= 3;
         break;
      }
      case bci_PUSH_AS: {
         int o_bco  = BCO_NEXT;
         int o_itbl = BCO_NEXT;
         StackWord(-1) = BCO_LITW(o_itbl);
         StackWord(-2) = BCO_PTR(o_bco);
         Sp -= 2;
         break;
      }
      case bci_PUSH_LIT:{
         int o = BCO_NEXT;
         StackWord(-1) = BCO_LIT(o);
         Sp --;
         break;
      }
      case bci_PUSH_TAG: {
         W_ tag = (W_)(BCO_NEXT);
         StackWord(-1) = tag;
         Sp --;
         break;
      }
      case bci_SLIDE: {
         int n  = BCO_NEXT;
         int by = BCO_NEXT;
         ASSERT(Sp+n+by <= (StgPtr)xSu);
         /* a_1, .. a_n, b_1, .. b_by, s => a_1, .. a_n, s */
         while(--n >= 0) {
            StackWord(n+by) = StackWord(n);
         }
         Sp += by;
         break;
      }
 case bci_ALLOC: {
   int n_payload = BCO_NEXT;
   P_ p = allocate(AP_sizeW(n_payload));
   StackWord(-1) = p;
   Sp --;
   break;
 }
      case bci_MKAP:	{
	int off = BCO_NEXT;
	int n_payload = BCO_NEXT - 1;
        StgAP_UPD* ap = StackWord(off);
        ap->n_args = n_payload;
        ap->fun = (StgClosure*)StackWord(0);
	for (i = 0; i < n_payload; i++)
	  ap->payload[i] = StackWord(i+1);
	}
      Sp += n_payload+1;
}
case bci_UNPACK:{
  /* Unpack N ptr words from t.o.s constructor */
  int n_words = BCO_NEXT;
  StgClosure* con = StackWord(0);
  Sp -= n_words;
  for (i = 0; i < n_words; i++)
    StackWord(i) = con->payload[i];
}
      case bci_PACK:
      case bci_TESTLT_I:
      case bci_TESTEQ_I:
      case bci_TESTLT_F:
      case bci_TESTEQ_F:
      case bci_TESTLT_D:
      case bci_TESTEQ_D:
      case bci_TESTLT_P:
      case bci_TESTEQ_P:
      case bci_CASEFAIL:
   
      /* Control-flow ish things */
      case bci_ARGCHECK:
      case bci_ENTER:
      case bci_RETURN:

      /* Errors */
      case bci_LABEL:
      default: barf
}

#endif /* 0 */
