
/* --------------------------------------------------------------------------
 * Lambda Lifter
 *
 * This is a very simple lambda lifter - it doesn't try to do Johnsson-style
 * lambda lifting (yet).
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: lift.c,v $
 * $Revision: 1.3 $
 * $Date: 1999/02/03 17:08:31 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "backend.h"
#include "connect.h"
#include "errors.h"


/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static List liftedBinds = NIL;

static StgExpr abstractExpr ( List vars, StgExpr e );
static inline Bool isTopLevel( StgVar v );
static List    filterFreeVars( List vs );
static List    liftLetBinds ( List binds );
static void    liftAlt      ( StgCaseAlt alt );
static void    liftPrimAlt  ( StgPrimAlt alt );
static void    liftExpr     ( StgExpr e );

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
        args = cons(arg,args);
        sub  = cons(pair(var,arg),sub);
    }
    return makeStgLambda(rev(args),substExpr(sub,e));
}

/* ToDo: should be conservative estimate but isn't */
/* Will a variable be floated out to top level - conservative estimate? */
static inline Bool isTopLevel( StgVar v )
{
    if (isNull(stgVarBody(v))) {
        return FALSE; /* only let bound vars can be floated */
    } else if (stgVarInfo(v) == NONE) {
        return TRUE;  /* those at top level are already there */
    } else {
#if LIFT_CONSTANTS
        StgRhs rhs  = stgVarBody(v);
        switch (whatIs(rhs)) {
        case STGCON:
        case STGAPP:
                return isNull(stgVarInfo(v));
        default:
                return FALSE;
        }
#else
        return FALSE;
#endif
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

static List liftLetBinds( List binds )
{
    List bs = NIL;
    for(; nonNull(binds); binds=tl(binds)) {
        StgVar bind = hd(binds);
        StgRhs rhs  = stgVarBody(bind);
        List   fvs  = filterFreeVars(stgVarInfo(bind));
        /* stgVarInfo(bind) = NIL; */ /* ToDo: discard fv list */

        switch (whatIs(rhs)) {
        case STGCON:
        case STGAPP:
#if LIFT_CONSTANTS
                if (isNull(fvs)) {
                    StgVar v = mkStgVar(rhs,NONE);
                    stgVarBody(bind) = mkStgLet(singleton(v),v);
                    /* ppStg(v); */
                    liftedBinds = cons(bind,liftedBinds);
                    break;
                }
                /* deliberate fall through */
#endif
        case STGVAR:
        case NAME:
                bs = cons(bind,bs);
                break;
        default:
                liftExpr(rhs);
                if (nonNull(fvs)) {
                    StgVar v = mkStgVar(abstractExpr(fvs,rhs),NONE);
                    /* ppStg(v); */
                    liftedBinds = cons(v,liftedBinds);
                    stgVarBody(bind) = makeStgApp(v, fvs);
                }
#if LIFT_CONSTANTS
                else {
                    StgVar r = mkStgVar(rhs,NIL); /* copy the var */
                    StgVar v = mkStgVar(mkStgLet(singleton(r),r),NONE);
                    stgVarBody(bind) = v; /* indirection to r */
                    /* ppStg(v); */
                    liftedBinds = cons(v,liftedBinds);
                    bs = cons(bind,bs); /* keep the old binding */
                    break;
                }
                /* deliberate fall through */
#endif
                bs = cons(bind,bs);
                break;
        }
    }
    return bs;
}

static void liftAlt( StgCaseAlt alt )
{
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
            stgLetBinds(e) = liftLetBinds(stgLetBinds(e));
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
            break;
    default:
            internal("liftExpr");
    }
}

List liftBinds( List binds )
{
    List bs;
    for(bs=binds; nonNull(bs); bs=tl(bs)) {
        StgVar bind = hd(bs);
        freeVarsBind(NIL,bind);
        stgVarInfo(bind) = NONE; /* mark as top level */
    }
    liftedBinds = NIL;
    binds = liftLetBinds(binds);
    binds = revOnto(liftedBinds,binds);
    liftedBinds = NIL;
    return binds;
}

/* --------------------------------------------------------------------------
 * Compiler control:
 * ------------------------------------------------------------------------*/

Void liftControl(what)
Int what; {
    switch (what) {
    case INSTALL:
            /* deliberate fall though */
    case RESET: 
            liftedBinds = NIL;
            break;
    case MARK: 
            mark(liftedBinds);
            break;
    }
}

/*-------------------------------------------------------------------------*/
