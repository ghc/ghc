
/* --------------------------------------------------------------------------
 * Free variable analysis
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: free.c,v $
 * $Revision: 1.11 $
 * $Date: 2000/03/23 14:54:21 $
 * ------------------------------------------------------------------------*/

#include "hugsbasictypes.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"


/* --------------------------------------------------------------------------
 * Local functions
 * ------------------------------------------------------------------------*/

static List freeVarsAlt     ( List, StgCaseAlt );
static List freeVarsPrimAlt ( List, StgPrimAlt );
static List freeVarsExpr    ( List, StgExpr );
static List freeVarsAtom    ( List, StgAtom );
static List freeVarsVar     ( List, StgVar );

/* --------------------------------------------------------------------------
 * Free variable analysis
 * ------------------------------------------------------------------------*/

static List freeVarsAtom( List acc, StgAtom a)
{
    switch (whatIs(a)) {
    case STGVAR:
            return freeVarsVar(acc,a);
    /* Note that NAMEs have no free vars. */
    default:
            return acc;
    }
}

static List freeVarsVar( List acc, StgVar v)
{
    if (cellIsMember(v,acc)) {
        return acc;
    } else {
        return cons(v,acc);
    }
}

List freeVarsBind( List acc, StgVar v )
{
    StgRhs rhs = stgVarBody(v);
    List fvs = NIL;
    switch (whatIs(rhs)) {
    case STGCON:
            mapAccum(freeVarsAtom,fvs,stgConArgs(rhs));
            break;
    default:
            fvs = freeVarsExpr(fvs,rhs);
            break;
    }
    /* fvs = rev(fvs); */  /* todo might cause less stack rearrangement? */
    stgVarInfo(v) = fvs;
    mapAccum(freeVarsVar,acc,fvs); /* copy onto acc */
    return acc;
}

static List freeVarsAlt( List acc, StgCaseAlt alt )
{
    if (isDefaultAlt(alt)) {
        acc = freeVarsExpr(acc,stgDefaultBody(alt));
        return deleteCell(acc,stgDefaultVar(alt)); 
    } else {
        acc = freeVarsExpr(acc,stgCaseAltBody(alt));
        return diffList(acc,stgCaseAltVars(alt));
    }
}

static List freeVarsPrimAlt( List acc, StgPrimAlt alt )
{
    List vs = stgPrimAltVars(alt);
    acc = freeVarsExpr(acc,stgPrimAltBody(alt));
    return diffList(acc,vs);
}

static List freeVarsExpr( List acc, StgExpr e )
{
#if 0
    printf( "freeVarsExpr: " );ppStgExpr(e);printf("\n");
#endif
    switch (whatIs(e)) {
    case LETREC:
            mapAccum(freeVarsBind,acc,stgLetBinds(e));
            return diffList(freeVarsExpr(acc,stgLetBody(e)),stgLetBinds(e));
    case LAMBDA:
            return diffList(freeVarsExpr(acc,stgLambdaBody(e)),stgLambdaArgs(e));
    case CASE:
            mapAccum(freeVarsAlt,acc,stgCaseAlts(e));
            return freeVarsExpr(acc,stgCaseScrut(e));
    case PRIMCASE:
            mapAccum(freeVarsPrimAlt,acc,stgPrimCaseAlts(e));
            return freeVarsExpr(acc,stgPrimCaseScrut(e));
    case STGPRIM:
            mapAccum(freeVarsAtom,acc,stgPrimArgs(e));
            /* primop is not a var */
            return acc;
    case STGAPP:
            /* Doing fun first causes slightly less stack rearrangement. */
            acc = freeVarsExpr(acc,stgAppFun(e));
            mapAccum(freeVarsAtom,acc,stgAppArgs(e));
            return acc;
    case STGVAR:
            return freeVarsVar(acc, e);
    case NAME:
            return acc;  /* Names are never free vars */
    default:
            printf("\n");
            ppStgExpr(e);
            printf("\n");
            internal("freeVarsExpr");
    }
}

/*-------------------------------------------------------------------------*/
