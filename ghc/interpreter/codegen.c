
/* --------------------------------------------------------------------------
 * Code generator
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: codegen.c,v $
 * $Revision: 1.21 $
 * $Date: 2000/04/05 10:25:08 $
 * ------------------------------------------------------------------------*/

#include "hugsbasictypes.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"

#include "Assembler.h"
#include "Rts.h"    /* IF_DEBUG */
#include "RtsFlags.h"

/*#define DEBUG_CODEGEN*/

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

#define getPos(v)     intOf(stgVarInfo(v))
#define setPos(v,sp)  stgVarInfo(v) = mkInt(sp)
#define getObj(v)     ptrOf(stgVarInfo(v))
#define setObj(v,obj) stgVarInfo(v) = mkPtr(obj)

#define repOf(x)      charOf(stgVarRep(x))

static void  cgBind        ( AsmBCO bco, StgVar v );
static Void  pushVar       ( AsmBCO bco, StgVar v );
static Void  pushAtom      ( AsmBCO bco, StgAtom atom );
static Void  alloc         ( AsmBCO bco, StgRhs rhs );
static Void  build         ( AsmBCO bco, StgRhs rhs );
static Void  cgExpr        ( AsmBCO bco, AsmSp root, StgExpr e );
             
static AsmBCO cgAlts       ( AsmSp root, AsmSp sp, List alts );
static void   testPrimPats ( AsmBCO bco, AsmSp root, List pats, StgExpr e );
static AsmBCO cgLambda     ( StgExpr e );
static AsmBCO cgRhs        ( StgRhs rhs );
static void   beginTop     ( StgVar v );
static void   endTop       ( StgVar v );

static StgVar currentTop;

/* --------------------------------------------------------------------------
 * 
 * ------------------------------------------------------------------------*/

static Cell cptrFromName ( Name n )
{
   char  buf[1000];
   void* p;
   Module m = name(n).mod;
   Text  mt = module(m).text;
   sprintf(buf, MAYBE_LEADING_UNDERSCORE_STR("%s_%s_closure"), 
                textToStr(mt), 
                textToStr( enZcodeThenFindText ( 
                   textToStr (name(n).text) ) ) );
   p = lookupOTabName ( m, buf );
   if (!p) {
      ERRMSG(0) "Can't find object symbol %s", buf
      EEND;
   }
   return mkCPtr(p);
}

static Bool varHasClosure( StgVar v )
{
    return asmObjectHasClosure((AsmClosure*)ptrOf(stgVarInfo(v)));
}

/* should be AsmClosure* */
void* closureOfVar( StgVar v )
{
    return asmClosureOfObject((AsmClosure*)ptrOf(stgVarInfo(v)));
}

char* lookupHugsName( void* closure )
{
    extern Name nameHw;
    Name nm;
    for( nm = NAME_BASE_ADDR; 
         nm < NAME_BASE_ADDR+tabNameSz; ++nm ) 
       if (tabName[nm-NAME_BASE_ADDR].inUse) {
           StgVar v  = name(nm).stgVar;
           if (isStgVar(v) 
               && isPtr(stgVarInfo(v)) 
               && varHasClosure(v)
               && closureOfVar(v) == closure) {
               return textToStr(name(nm).text);
           }
    }
    return 0;
}

static void cgBindRep( AsmBCO bco, StgVar v, AsmRep rep )
{
    setPos(v,asmBind(bco,rep));
}

static void cgBind( AsmBCO bco, StgVar v )
{
    cgBindRep(bco,v,repOf(v));
}

static Void pushVar( AsmBCO bco, StgVar v )
{
    Cell info;
#if 0
printf ( "pushVar:  %d  ", v ); fflush(stdout);
print(v,10);printf("\n");
#endif
    assert(isStgVar(v) || isCPtr(v));

    if (isCPtr(v)) {
       asmGHCClosure(bco, cptrOf(v));
    } else {
       info = stgVarInfo(v);
       if (isPtr(info)) {
           asmClosure(bco,ptrOf(info));
       } else if (isInt(info)) {
           asmVar(bco,intOf(info),repOf(v));
       } else {
           internal("pushVar");
       }        
    }
}

static Void pushAtom( AsmBCO bco, StgAtom e )
{
#if 0
printf ( "pushAtom: %d  ", e ); fflush(stdout);
print(e,10);printf("\n");
#endif
    switch (whatIs(e)) {
    case STGVAR: 
            pushVar(bco,e);
            break;
    case NAME: 
            if (nonNull(name(e).stgVar)) {
	       pushVar(bco,name(e).stgVar);
            } else {
               Cell /*CPtr*/ addr = cptrFromName(e);
#              if DEBUG_CODEGEN
               fprintf ( stderr, "nativeAtom: name %s\n", 
                                 nameFromOPtr(cptrOf(addr)) );
#              endif
	       pushVar(bco,addr);
            }
            break;
    case CHARCELL: 
            asmConstChar(bco,charOf(e));
            break;
    case INTCELL: 
            asmConstInt(bco,intOf(e));
            break;
    case BIGCELL:
            asmConstInteger(bco,bignumToString(e)); 
            break;
    case FLOATCELL: 
            asmConstDouble(bco,floatOf(e));
            break;
    case STRCELL: 
#if USE_ADDR_FOR_STRINGS
            asmConstAddr(bco,textToStr(textOf(e)));
#else
            asmClosure(bco,asmStringObj(textToStr(textOf(e))));
#endif
            break;
    case CPTRCELL:
            asmGHCClosure(bco,cptrOf(e));
            break;
    case PTRCELL: 
            asmConstAddr(bco,ptrOf(e));
            break;
    default: 
            fprintf(stderr,"\nYoiks1: "); printExp(stderr,e);
            internal("pushAtom");
    }
}

static AsmBCO cgAlts( AsmSp root, AsmSp sp, List alts )
{
#ifdef CRUDE_PROFILING
    AsmBCO bco = asmBeginContinuation(sp, currentTop + 1000000000);
#else
    AsmBCO bco = asmBeginContinuation(sp, alts);
#endif
    Bool omit_test
       = length(alts) == 2 &&
         isDefaultAlt(hd(tl(alts))) &&
         !isDefaultAlt(hd(alts));
    if (omit_test) {
       /* refine the condition */              
       Name con;
       Tycon t;
       omit_test = FALSE;
       con = stgCaseAltCon(hd(alts));

       /* special case: dictionary constructors */
       if (isName(con) && strncmp(":D",textToStr(name(con).text),2)==0) {
          omit_test = TRUE;
          goto xyzzy;
       }
       /* special case: Tuples */
       if (isTuple(con) || (isName(con) && con==nameUnit)) {
          omit_test = TRUE;
          goto xyzzy;
       }          

       t = name(con).parent;
       if (tycon(t).what == DATATYPE) {
          if (length(tycon(t).defn) == 1) omit_test = TRUE;
       }
    }

    xyzzy:

    for(; nonNull(alts); alts=tl(alts)) {
        StgCaseAlt alt  = hd(alts);
        if (isDefaultAlt(alt)) {
            cgBind(bco,stgDefaultVar(alt));
            cgExpr(bco,root,stgDefaultBody(alt));
            asmEndContinuation(bco);
            return bco; /* ignore any further alternatives */
        } else {
            StgDiscr con   = stgCaseAltCon(alt);
            List     vs    = stgCaseAltVars(alt);
            AsmSp    begin = asmBeginAlt(bco);
            AsmPc    fix;
            if (omit_test) fix=-1; else fix = asmTest(bco,stgDiscrTag(con)); 

	    asmBind(bco,PTR_REP); /* Adjust simulated sp to acct for return value */
            if (isBoxingCon(con)) {
                setPos(hd(vs),asmUnbox(bco,boxingConRep(con)));
            } else {
                asmBeginUnpack(bco);
                map1Proc(cgBind,bco,reverse(vs));
                asmEndUnpack(bco);
            }
            cgExpr(bco,root,stgCaseAltBody(alt));
            asmEndAlt(bco,begin);
            if (fix != -1) asmFixBranch(bco,fix);
        }
    }
    /* if we got this far and didn't match, panic! */
    asmPanic(bco);
    asmEndContinuation(bco);
    return bco;
}

static void testPrimPats( AsmBCO bco, AsmSp root, List pats, StgExpr e )
{
    if (isNull(pats)) {
        cgExpr(bco,root,e);
    } else {
        StgVar pat = hd(pats);
        if (isInt(stgVarBody(pat))) {
            /* asmTestInt leaves stack unchanged - so no need to adjust it */
            AsmPc tst = asmTestInt(bco,getPos(pat),intOf(stgVarBody(pat)));
            assert(repOf(pat) == INT_REP);
            testPrimPats(bco,root,tl(pats),e);
            asmFixBranch(bco,tst);
        } else {
            testPrimPats(bco,root,tl(pats),e);
        }
    }
}


static AsmBCO cgLambda( StgExpr e )
{
    AsmBCO bco = asmBeginBCO(e);

    AsmSp root = asmBeginArgCheck(bco);
    map1Proc(cgBind,bco,reverse(stgLambdaArgs(e)));
    asmEndArgCheck(bco,root);

    /* ppStgExpr(e); */
    cgExpr(bco,root,stgLambdaBody(e));

    asmEndBCO(bco);
    return bco;
}

static AsmBCO cgRhs( StgRhs rhs )
{
    AsmBCO bco = asmBeginBCO(rhs );

    AsmSp root = asmBeginArgCheck(bco);
    asmEndArgCheck(bco,root);

    /* ppStgExpr(rhs); */
    cgExpr(bco,root,rhs);

    asmEndBCO(bco);
    return bco;
}


static Void cgExpr( AsmBCO bco, AsmSp root, StgExpr e )
{
#if 0
    printf("cgExpr:");ppStgExpr(e);printf("\n");
#endif
    switch (whatIs(e)) {
    case LETREC:
        {
            List binds = stgLetBinds(e);
            map1Proc(alloc,bco,binds);
            map1Proc(build,bco,binds);
            cgExpr(bco,root,stgLetBody(e));
            break;
        }
    case LAMBDA:
        {
            AsmSp begin = asmBeginEnter(bco);
            asmClosure(bco,cgLambda(e));
            asmEndEnter(bco,begin,root);
            break;
        }
    case CASE:
        {
            List  alts     = stgCaseAlts(e);
            AsmSp sp       = asmBeginCase(bco);
            AsmSp caseroot = asmContinuation(bco,cgAlts(root,sp,alts));
            cgExpr(bco,caseroot,stgCaseScrut(e));
            asmEndCase(bco);
            break;
        }
    case PRIMCASE:
        {
            StgExpr scrut = stgPrimCaseScrut(e);
            List alts = stgPrimCaseAlts(e);
            if (whatIs(scrut) == STGPRIM) {  /* this is an optimisation */

                /* No need to use return address or to Slide */
                AsmSp beginPrim = asmBeginPrim(bco);
                map1Proc(pushAtom,bco,reverse(stgPrimArgs(scrut)));
                asmEndPrim(bco,(AsmPrim*)name(stgPrimOp(scrut)).primop,beginPrim);

                for(; nonNull(alts); alts=tl(alts)) {
                    StgPrimAlt alt = hd(alts);
                    List    pats = stgPrimAltVars(alt);
                    StgExpr body = stgPrimAltBody(alt);
                    AsmSp altBegin = asmBeginAlt(bco);
                    map1Proc(cgBind,bco,reverse(pats));
                    testPrimPats(bco,root,pats,body);
                    asmEndAlt(bco,altBegin);
                }
                /* if we got this far and didn't match, panic! */
                asmPanic(bco);
                
            } else if (whatIs(scrut) == STGVAR) { /* another optimisation */

                /* No need to use return address or to Slide */

                /* only part different from primop code... todo */
                AsmSp beginCase = asmBeginCase(bco);
                pushVar(bco,scrut);
                asmEndAlt(bco,beginCase); /* hack, hack -  */

                for(; nonNull(alts); alts=tl(alts)) {
                    StgPrimAlt alt = hd(alts);
                    List    pats = stgPrimAltVars(alt);
                    StgExpr body = stgPrimAltBody(alt);
                    AsmSp altBegin = asmBeginAlt(bco);
                    map1Proc(cgBind,bco,pats);
                    testPrimPats(bco,root,pats,body);
                    asmEndAlt(bco,altBegin);
                }
                /* if we got this far and didn't match, panic! */
                asmPanic(bco);
                                
            } else {
                /* ToDo: implement this code...  */
                assert(0);
                /* asmPushRet(bco,delayPrimAlt( stgPrimCaseVars(e), 
                                                stgPrimCaseBody(e))); */
                /* cgExpr( bco,root,scrut ); */
            }
            break;
        }
    case STGAPP: /* Tail call */
        {
            AsmSp env = asmBeginEnter(bco);
            map1Proc(pushAtom,bco,reverse(stgAppArgs(e)));
            pushAtom(bco,stgAppFun(e));
            asmEndEnter(bco,env,root);
            break;
        }
    case NAME: /* Tail call (with no args) */
        {
            AsmSp env = asmBeginEnter(bco);
            /* JRS 000112: next line used to be: pushVar(bco,name(e).stgVar); */
            pushAtom(bco,e);
            asmEndEnter(bco,env,root);
            break;
        }
    case STGVAR: /* Tail call (with no args), plus unboxed return */
            switch (repOf(e)) {
            case PTR_REP:
            case ALPHA_REP:
            case BETA_REP:
                {
                    AsmSp env = asmBeginEnter(bco);
                    pushVar(bco,e);
                    asmEndEnter(bco,env,root);
                    break;
                }
            case INT_REP:
                    assert(0);
                    /* cgTailCall(bco,singleton(e)); */
                    /* asmReturnInt(bco); */
                    break;
            default:
                    internal("cgExpr StgVar");
            }
            break;
    case STGPRIM: /* Tail call again */
        {
            AsmSp beginPrim = asmBeginPrim(bco);
            map1Proc(pushAtom,bco,reverse(stgPrimArgs(e)));
            asmEndPrim(bco,(AsmPrim*)name(e).primop,beginPrim);
            /* map1Proc(cgBind,bco,rs_vars); */
            assert(0); /* asmReturn_retty(); */
            break;
        }
    default:
            fprintf(stderr,"\nYoiks2: "); printExp(stderr,e);
            internal("cgExpr");
    }
}

#define M_ITBLNAMES 35000

void* itblNames[M_ITBLNAMES];
int   nItblNames = 0;

/* allocate space for top level variable
 * any change requires a corresponding change in 'build'.
 */
static Void alloc( AsmBCO bco, StgVar v )
{
    StgRhs rhs = stgVarBody(v);
    assert(isStgVar(v));
#if 0
    printf("alloc: ");ppStgExpr(v);
#endif
    switch (whatIs(rhs)) {
    case STGCON:
        {
            StgDiscr con  = stgConCon(rhs);
            List     args = stgConArgs(rhs);
            if (isBoxingCon(con)) {
                pushAtom(bco,hd(args));
                setPos(v,asmBox(bco,boxingConRep(con)));
            } else {

                void* vv = stgConInfo(con);
                if (!(nItblNames < (M_ITBLNAMES-2))) 
                   internal("alloc -- M_ITBLNAMES too small");
                if (isName(con)) {
                   itblNames[nItblNames++] = vv;
                   itblNames[nItblNames++] = textToStr(name(con).text);
                } else
                if (isTuple(con)) {
                   itblNames[nItblNames++] = vv;
                   itblNames[nItblNames++] = textToStr(ghcTupleText(con));
                } else
                assert ( /* cant identify constructor name */ 0 );
                setPos(v,asmAllocCONSTR(bco, vv));
            }
            break;
        }
    case STGAPP: {
            Int  totSizeW = 0;
            List bs       = stgAppArgs(rhs);
            for (; nonNull(bs); bs=tl(bs)) {
               if (isName(hd(bs))) {
                  totSizeW += 1;
               } else {
                  ASSERT(whatIs(hd(bs))==STGVAR);
                  totSizeW += asmRepSizeW( charOf(stgVarRep(hd(bs))) );
               }
            }
            setPos(v,asmAllocAP(bco,totSizeW));
            //ORIGINALLY:setPos(v,asmAllocAP(bco,length(stgAppArgs(rhs))));
            break;
         }
    case LAMBDA: /* optimisation */
            setObj(v,cgLambda(rhs));
            break;
    default: 
            setPos(v,asmAllocAP(bco,0));
            break;
    }
}

static Void build( AsmBCO bco, StgVar v )
{
    StgRhs rhs = stgVarBody(v);
    assert(isStgVar(v));
    //ppStg(v);
    switch (whatIs(rhs)) {
    case STGCON:
        {
            StgDiscr con  = stgConCon(rhs);
            List     args = stgConArgs(rhs);
            if (isBoxingCon(con)) {
                doNothing();  /* already done in alloc */
            } else {
                AsmSp start = asmBeginPack(bco);
                map1Proc(pushAtom,bco,reverse(args));
                asmEndPack(bco,getPos(v),start,stgConInfo(con));
            }
            return;
        }
    case STGAPP: 
        {
            Bool   itsaPAP;
            StgVar fun  = stgAppFun(rhs);
            StgVar fun0 = fun;
            List   args = stgAppArgs(rhs);
            if (isName(fun)) {
                if (nonNull(name(fun).stgVar))
                   fun = name(fun).stgVar; else
                   fun = cptrFromName(fun);
            }

            if (isCPtr(fun)) {
               assert(isName(fun0));
               itsaPAP = name(fun0).arity > length(args);
#              if DEBUG_CODEGEN
               fprintf ( stderr, "nativeCall: name %s, arity %d, args %d\n",
                         nameFromOPtr(cptrOf(fun)), name(fun0).arity,
                         length(args) );
#              endif
            } else {
               itsaPAP = FALSE;
               if (nonNull(stgVarBody(fun))
                   && whatIs(stgVarBody(fun)) == LAMBDA 
                   && length(stgLambdaArgs(stgVarBody(fun))) > length(args)
                  )
                  itsaPAP = TRUE;
            }

            if (itsaPAP) {
                AsmSp  start = asmBeginMkPAP(bco);
                map1Proc(pushAtom,bco,reverse(args));
                pushAtom(bco,fun);
                asmEndMkPAP(bco,getPos(v),start); /* optimisation */
            } else {
                AsmSp  start = asmBeginMkAP(bco);
                map1Proc(pushAtom,bco,reverse(args));
                pushAtom(bco,fun);
                asmEndMkAP(bco,getPos(v),start);
            }
            return;
        }
    case LAMBDA: /* optimisation */
            doNothing(); /* already pushed in alloc */
            break;

    /* These two cases look almost identical to the default but they're really
     * special cases of STGAPP.  The essential thing here is that we can't call
     * cgRhs(rhs) because that expects the rhs to have no free variables when, 
     * in fact, the rhs is _always_ a free variable.
     *
     * ToDo: a simple optimiser would eliminate all examples
     * of this except "let x = x in ..."
     */
    case NAME:
        if (nonNull(name(rhs).stgVar))
           rhs = name(rhs).stgVar; else
           rhs = cptrFromName(rhs);
        /* fall thru */
    case STGVAR:
        {
            AsmSp  start = asmBeginMkAP(bco);
            pushAtom(bco,rhs);
            asmEndMkAP(bco,getPos(v),start);
        }
        return;
    default:
        {
            AsmSp start = asmBeginMkAP(bco);   /* make it updateable! */
            asmClosure(bco,cgRhs(rhs));
            asmEndMkAP(bco,getPos(v),start);
            return;
        }
    }
}

/* --------------------------------------------------------------------------
 * Top level variables
 *
 * ToDo: these should be handled by allocating a dynamic unentered CAF
 * for each top level variable - this should be simpler!
 * ------------------------------------------------------------------------*/

#if 0   /* appears to be unused */
static void cgAddVar( AsmObject obj, StgAtom v )
{
    if (isName(v)) {
        v = name(v).stgVar;
    }
    assert(isStgVar(v));
    asmAddPtr(obj,getObj(v));
}
#endif


/* allocate AsmObject for top level variables
 * any change requires a corresponding change in endTop
 */
static void beginTop( StgVar v )
{
    StgRhs rhs;
    assert(isStgVar(v));
    currentTop = v;
    rhs = stgVarBody(v);
    switch (whatIs(rhs)) {
    case STGCON:
        {
	    //List as = stgConArgs(rhs);
            setObj(v,asmBeginCon(stgConInfo(stgConCon(rhs))));
            break;
        }
    case LAMBDA:
#ifdef CRUDE_PROFILING
            setObj(v,asmBeginBCO(currentTop));
#else
            setObj(v,asmBeginBCO(rhs));
#endif
            break;
    default:
            setObj(v,asmBeginCAF());
            break;
    }
}

static void endTop( StgVar v )
{
    StgRhs rhs = stgVarBody(v);
    currentTop = v;
    switch (whatIs(rhs)) {
    case STGCON:
        {
            List as = stgConArgs(rhs);
            AsmCon con = (AsmCon)getObj(v);
            for( ; nonNull(as); as=tl(as)) {
                StgAtom a = hd(as);
                switch (whatIs(a)) {
                case STGVAR: 
                        /* should be a delayed combinator! */
                        asmAddPtr(con,(AsmObject)getObj(a));
                        break;
                case NAME: 
                    {
                        StgVar var = name(a).stgVar;
                        assert(var);
                        asmAddPtr(con,(AsmObject)getObj(a));
                        break;
                    }
#if !USE_ADDR_FOR_STRINGS
                case STRCELL:
                        asmAddPtr(con,asmStringObj(textToStr(textOf(a))));
                        break;
#endif
                default: 
                        /* asmAddPtr(con,??); */
                        assert(0);
                        break;
                }
            }
            asmEndCon(con);
            break;
        }
    case LAMBDA: /* optimisation */
        {
            /* ToDo: merge this code with cgLambda */
            AsmBCO bco = (AsmBCO)getObj(v);
            AsmSp root = asmBeginArgCheck(bco);
            map1Proc(cgBind,bco,reverse(stgLambdaArgs(rhs)));
            asmEndArgCheck(bco,root);
            
            cgExpr(bco,root,stgLambdaBody(rhs));
            
            asmEndBCO(bco);
            break;
        }
    default:   /* updateable caf */
        {
            AsmCAF caf = (AsmCAF)getObj(v);
            asmEndCAF(caf,cgRhs(rhs));
            break;
        }
    }
}

static void zap( StgVar v )
{
  // ToDo: reinstate
  //    stgVarBody(v) = NIL;
}

/* external entry point */
Void cgBinds( List binds )
{
    List b;
    int i;

#if 0
    if (lastModule() != modulePrelude) {
        printf("\n\ncgBinds: before ll\n\n" );
        for (b=binds; nonNull(b); b=tl(b)) {
           printStg ( stdout, hd(b) ); printf("\n\n");
        }
    }
#endif

    binds = liftBinds(binds);

#if 0
    if (lastModule() != modulePrelude) {
        printf("\n\ncgBinds: after ll\n\n" );
        for (b=binds; nonNull(b); b=tl(b)) {
           printStg ( stdout, hd(b) ); printf("\n\n");
        }
    }
#endif

    for (b=binds,i=0; nonNull(b); b=tl(b),i++) {
       /* printStg( stdout, hd(b) ); printf( "\n\n"); */
       beginTop(hd(b));
    }

    for (b=binds,i=0; nonNull(b); b=tl(b),i++) {
       /* printStg( stdout, hd(b) ); printf( "\n\n"); */
       endTop(hd(b));
    }

    /* mapProc(zap,binds); */
}

/* Called by the evaluator's GC to tell Hugs to mark stuff in the
   run-time heap.
*/
void markHugsObjects( void )
{
    extern Name nameHw;
    Name nm;
    for ( nm = NAME_BASE_ADDR; 
          nm < NAME_BASE_ADDR+tabNameSz; ++nm )
       if (tabName[nm-NAME_BASE_ADDR].inUse) {
           StgVar v  = name(nm).stgVar;
           if (isStgVar(v) && isPtr(stgVarInfo(v))) {
               asmMarkObject(ptrOf(stgVarInfo(v)));
           }
       }
}

/* --------------------------------------------------------------------------
 * Code Generator control:
 * ------------------------------------------------------------------------*/

Void codegen(what)
Int what; {
    switch (what) {
       case PREPREL:
       case RESET: 
       case MARK: 
       case POSTPREL:
          break;
    }
    liftControl(what);
}

/*-------------------------------------------------------------------------*/
