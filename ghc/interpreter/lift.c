
/* --------------------------------------------------------------------------
 * Lambda Lifter
 *
 * This is a very simple lambda lifter - it doesn't try to do Johnsson-style
 * lambda lifting (yet).
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: lift.c,v $
 * $Revision: 1.14 $
 * $Date: 2000/04/27 16:35:29 $
 * ------------------------------------------------------------------------*/

#include "hugsbasictypes.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"


/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static List liftedBinds    = NIL;

static StgExpr abstractExpr   ( List vars, StgExpr e );
static Bool    isTopLevel     ( StgVar v );
static List    filterFreeVars ( List vs );
static List    liftLetBinds   ( List binds, Bool topLevel );
static void    liftAlt        ( StgCaseAlt alt );
static void    liftPrimAlt    ( StgPrimAlt alt );
static void    liftExpr       ( StgExpr e );

/* --------------------------------------------------------------------------
 * Lambda lifter
 * ------------------------------------------------------------------------*/

/* abstract variables out of an expression */
static StgExpr abstractExpr( List vars, StgExpr e )
{
    List args = NIL;
    List sub  = NIL; /* association list */
    for(; nonNull(vars); vars=tl(vars)) {
        StgVar var = hd(vars);
        StgVar arg = mkStgVar(NIL,NIL);
        stgVarRep(arg) = stgVarRep(var);
        args = cons(arg,args);
        sub  = cons(pair(var,arg),sub);
    }
    return makeStgLambda(rev(args),substExpr(sub,e));
}

/* ToDo: should be conservative estimate but isn't */
/* Will a variable be floated out to top level - conservative estimate? */
static Bool isTopLevel( StgVar v )
{
    if (isNull(stgVarBody(v))) {
        return FALSE; /* only let bound vars can be floated */
    } else if (stgVarInfo(v) == NONE) {
        return TRUE;  /* those at top level are already there */
    } else {
        return FALSE;
    }
}

static List filterFreeVars( List vs )
{
    List fvs = NIL;
    if (vs == NONE) {
        return NIL;
    } else {
        for(; nonNull(vs); vs=tl(vs)) {
            StgVar v = hd(vs);
            if (!isTopLevel(v)) {
                fvs = cons(v,fvs);
            }
        }
        return fvs;
    }
}

static Int nameCounter;

static List liftLetBinds( List binds, Bool topLevel )
{
    List bs          = NIL;
    for(; nonNull(binds); binds=tl(binds)) {
        StgVar bind = hd(binds);
        StgRhs rhs  = stgVarBody(bind);
        List   fvs  = filterFreeVars(stgVarInfo(bind));

        switch (whatIs(rhs)) {
        case STGCON:
        case STGAPP:
        case STGVAR:
        case NAME:
                bs = cons(bind,bs);
                break;
        default:
                liftExpr(rhs);
                if (nonNull(fvs)) {
                    StgVar v = mkStgVar(abstractExpr(fvs,rhs),NONE);
                    {
                       Name n;
                       char s[16];
                       sprintf(s,"(lift%d)",nameCounter++);
                       n = newName(findText(s),NIL);
                       name(n).closure = v;
                       stgVarBody(bind) = makeStgApp(n, fvs);
                       liftedBinds = cons(n,liftedBinds);
                    }
                }
                bs = cons(bind,bs);
                break;
        }
    }
    return bs;
}

static void liftAlt( StgCaseAlt alt )
{
    if (isDefaultAlt(alt))
       liftExpr(stgDefaultBody(alt)); else
       liftExpr(stgCaseAltBody(alt));
}

static void liftPrimAlt( StgPrimAlt alt )
{
    liftExpr(stgPrimAltBody(alt));
}

static void liftExpr( StgExpr e )
{
    switch (whatIs(e)) {
    case LETREC:
            stgLetBinds(e) = liftLetBinds(stgLetBinds(e),FALSE);
            liftExpr(stgLetBody(e));
            break;
    case LAMBDA:
            liftExpr(stgLambdaBody(e));
            break;
    case CASE:
            liftExpr(stgCaseScrut(e));
            mapProc(liftAlt,stgCaseAlts(e));
            break;
    case PRIMCASE:
            liftExpr(stgPrimCaseScrut(e));
            mapProc(liftPrimAlt,stgPrimCaseAlts(e));
            break;
    case STGPRIM:
            break;
    case STGAPP:
            break;
    case STGVAR:
    case NAME:
    case TUPLE:
            break;
    default:
            internal("liftExpr");
    }
}

/* Lift the list of top-level binds for a module. */
void liftModule ( Module mod )
{
    List binds = NIL;
    List cl;

    nameCounter = 0;
    for (cl = module(mod).codeList; nonNull(cl); cl = tl(cl)) {
        StgVar bind = getNameOrTupleClosure(hd(cl));
        if (isCPtr(bind)) continue;
        assert(nonNull(bind));
        if (debugSC) {
           if (currentModule != modulePrelude) {
              fprintf(stderr, "\n");
              ppStg(bind);
              fprintf(stderr, "\n");
           }
        }
        freeVarsBind(NIL,bind);
        stgVarInfo(bind) = NONE; /* mark as top level */
        binds = cons(bind,binds);
    }

    liftedBinds = NIL;
    binds       = liftLetBinds(binds,TRUE);
    module(mod).codeList = revOnto(liftedBinds, module(mod).codeList);
    liftedBinds = NIL;
}

/* --------------------------------------------------------------------------
 * Compiler control:
 * ------------------------------------------------------------------------*/

Void liftControl(what)
Int what; {
    switch (what) {
       case POSTPREL: break;

       case PREPREL:
       case RESET: 
          liftedBinds = NIL;
          break;
       case MARK: 
          mark(liftedBinds);
          break;
    }
}

/*-------------------------------------------------------------------------*/
