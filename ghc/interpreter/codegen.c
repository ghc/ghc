
/* --------------------------------------------------------------------------
 * Code generator
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: codegen.c,v $
 * $Revision: 1.4 $
 * $Date: 1999/03/01 14:46:42 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "backend.h"
#include "connect.h"
#include "errors.h"
#include "Assembler.h"
#include "link.h"

#include "Rts.h"    /* IF_DEBUG */
#include "RtsFlags.h"

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
//static void   cgPrimAlt    ( AsmBCO bco, AsmSp root, List vs, StgExpr e );
static AsmBCO cgLambda     ( StgExpr e );
static AsmBCO cgRhs        ( StgRhs rhs );
static void   beginTop     ( StgVar v );
static void   endTop       ( StgVar v );

/* --------------------------------------------------------------------------
 * 
 * ------------------------------------------------------------------------*/

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
    for( nm=NAMEMIN; nm<nameHw; ++nm ) {
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

/* called at the start of GC */
void markHugsObjects( void )
{
    extern Name nameHw;
    Name nm;
    for( nm=NAMEMIN; nm<nameHw; ++nm ) {
        StgVar v  = name(nm).stgVar;
        if (isStgVar(v) && isPtr(stgVarInfo(v))) {
            asmMarkObject((AsmClosure*)ptrOf(stgVarInfo(v)));
        }
    }
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
    Cell info = stgVarInfo(v);
    //    if (!isStgVar(v)) {
    //printf("\n\nprefail\n");
    //print(v,1000);
       assert(isStgVar(v));
    //}
    if (isPtr(info)) {
        asmClosure(bco,ptrOf(info));
    } else if (isInt(info)) {
        asmVar(bco,intOf(info),repOf(v));
    } else {
        internal("pushVar");
    }        
}

static Void pushAtom( AsmBCO bco, StgAtom e )
{
    switch (whatIs(e)) {
    case STGVAR: 
            pushVar(bco,e);
            break;
    case NAME: 
            pushVar(bco,name(e).stgVar);
            break;
    case CHARCELL: 
            asmConstChar(bco,charOf(e));
            break;
    case INTCELL: 
            asmConstInt(bco,intOf(e));
            break;
#if BIGNUM_IS_INTEGER
    case BIGCELL:
            asmConstInteger(bco,bignumToString(e)); 
            break;
#elif BIGNUM_IS_INT64
    case BIGCELL:
            asmConstInt64(bco,bignumOf(e)); 
            break;
#else
#warning What is BIGNUM?
#endif
    case FLOATCELL: 
#if 0
            asmConstFloat(bco,e); /* ToDo: support both float and double! */
#else
            asmConstDouble(bco,floatOf(e));
#endif
            break;
#if DOUBLES
    case DOUBLECELL: 
            asmConstDouble(bco,doubleOf(e));
            break;
#endif
    case STRCELL: 
#if USE_ADDR_FOR_STRINGS
            asmConstAddr(bco,textToStr(textOf(e)));
#else
            asmClosure(bco,asmStringObj(textToStr(textOf(e))));
#endif
            break;
    case PTRCELL: 
            asmConstAddr(bco,ptrOf(e));
            break;
    default: 
            fprintf(stderr,"\nYoiks: "); printExp(stderr,e);
            internal("pushAtom");
    }
}

static AsmBCO cgAlts( AsmSp root, AsmSp sp, List alts )
{
    AsmBCO bco = asmBeginContinuation(sp,alts);
    /* ppStgAlts(alts); */
    for(; nonNull(alts); alts=tl(alts)) {
        StgCaseAlt alt  = hd(alts);
        StgPat     pat  = stgCaseAltPat(alt);
        StgExpr    body = stgCaseAltBody(alt);
        if (isDefaultPat(pat)) {
	    //AsmSp      begin = asmBeginAlt(bco);
            cgBind(bco,pat);
            cgExpr(bco,root,body);
            asmEndContinuation(bco);
            return bco; /* ignore any further alternatives */
        } else {
            StgDiscr con = stgPatDiscr(pat);
            List     vs  = stgPatVars(pat);
            AsmSp    begin = asmBeginAlt(bco);
            AsmPc    fix = asmTest(bco,stgDiscrTag(con)); /* ToDo: omit in single constructor types! */
            cgBind(bco,pat);
            if (isBoxingCon(con)) {
                setPos(hd(vs),asmUnbox(bco,boxingConRep(con)));
            } else {
                asmBeginUnpack(bco);
                map1Proc(cgBind,bco,reverse(vs));
                asmEndUnpack(bco);
            }
            cgExpr(bco,root,body);
            asmEndAlt(bco,begin);
            asmFixBranch(bco,fix);
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
        StgPrimPat pat = hd(pats);
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

#if 0  /* appears to be unused */
static void cgPrimAlt( AsmBCO bco, AsmSp root, List vs, StgExpr e )
{
    assert(0); /* ToDo: test for patterns */
    map1Proc(cgBind,bco,vs); /* ToDo: are these in right order? */
    cgExpr(bco,root,e);
}
#endif


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
  //printf("cgExpr:");ppStgExpr(e);printf("\n");
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
                    List    pats = stgPrimAltPats(alt);
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
                    List    pats = stgPrimAltPats(alt);
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
                /* asmPushRet(bco,delayPrimAlt( stgPrimCaseVars(e), stgPrimCaseBody(e))); */
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
            pushVar(bco,name(e).stgVar);
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
            fprintf(stderr,"\nYoiks: "); printExp(stderr,e);
            internal("cgExpr");
    }
}

void* itblNames[1000];
int   nItblNames = 0;

/* allocate space for top level variable
 * any change requires a corresponding change in 'build'.
 */
static Void alloc( AsmBCO bco, StgVar v )
{
    StgRhs rhs = stgVarBody(v);
    assert(isStgVar(v));
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
                assert (nItblNames < (1000-2));
                if (isName(con)) {
                   itblNames[nItblNames++] = vv;
                   itblNames[nItblNames++] = textToStr(name(con).text);
                } else
                if (isTuple(con)) {
                   char* cc = malloc(10);
                   assert(cc);
                   sprintf(cc, "Tuple%d", tupleOf(con) );
                   itblNames[nItblNames++] = vv;
                   itblNames[nItblNames++] = cc;
                } else
                assert ( /* cant identify constructor name */ 0 );

                setPos(v,asmAllocCONSTR(bco, vv));
            }
            break;
        }
    case STGAPP: 
            setPos(v,asmAllocAP(bco,length(stgAppArgs(rhs))));
            break;
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
            StgVar fun  = stgAppFun(rhs);
            List   args = stgAppArgs(rhs);
            if (isName(fun)) {
                fun = name(fun).stgVar;
            }
            if (nonNull(stgVarBody(fun))
                && whatIs(stgVarBody(fun)) == LAMBDA 
                && length(stgLambdaArgs(stgVarBody(fun))) > length(args)) {
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
            rhs = name(rhs).stgVar;
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
    rhs = stgVarBody(v);
    switch (whatIs(rhs)) {
    case STGCON:
        {
	    //List as = stgConArgs(rhs);
            setObj(v,asmBeginCon(stgConInfo(stgConCon(rhs))));
            break;
        }
    case LAMBDA:
            setObj(v,asmBeginBCO(rhs));
            break;
    default:
            setObj(v,asmBeginCAF());
            break;
    }
}

static void endTop( StgVar v )
{
    StgRhs rhs = stgVarBody(v);
    //ppStgRhs(rhs);
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

    //if (lastModule() != modulePrelude) {
    //    printf("\n\ncgBinds: before ll\n\n" );
    //    for (b=binds; nonNull(b); b=tl(b)) {
    //       printStg ( stdout, hd(b) ); printf("\n\n");
    //    }
    //}

    binds = liftBinds(binds);

    //if (lastModule() != modulePrelude) {
    //    printf("\n\ncgBinds: after ll\n\n" );
    //    for (b=binds; nonNull(b); b=tl(b)) {
    //       printStg ( stdout, hd(b) ); printf("\n\n");
    //    }
    //}


    //mapProc(beginTop,binds);
    for (b=binds,i=0; nonNull(b); b=tl(b),i++) {
      //printf("beginTop %d\n", i);
       beginTop(hd(b));
    }

    //mapProc(endTop,binds);
    for (b=binds,i=0; nonNull(b); b=tl(b),i++) {
       endTop(hd(b));
       //if (lastModule() != modulePrelude) {
       //    printStg ( stdout, hd(b) ); printf("\n\n");
       //}
    }

    //mapProc(zap,binds);
}

/* --------------------------------------------------------------------------
 * Code Generator control:
 * ------------------------------------------------------------------------*/

Void codegen(what)
Int what; {
    switch (what) {
    case INSTALL:
            /* deliberate fall though */
    case RESET: 
            break;
    case MARK: 
            break;
    }
    liftControl(what);
}

/*-------------------------------------------------------------------------*/
