
/* --------------------------------------------------------------------------
 * Substitute variables in an expression
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: stgSubst.c,v $
 * $Revision: 1.8 $
 * $Date: 2000/03/23 14:54:21 $
 * ------------------------------------------------------------------------*/

#include "hugsbasictypes.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static StgVar     substVar     ( List sub, StgVar v );
static StgAtom    substAtom    ( List sub, StgAtom a );
static void       substBind    ( List sub, StgVar bind );
static void       substAlt     ( List sub, StgCaseAlt alt );
static void       substPrimAlt ( List sub, StgPrimAlt alt );

/* --------------------------------------------------------------------------
 * Substitute variables throughout an expression - updating in place.
 * ------------------------------------------------------------------------*/

static StgVar substVar( List sub, StgVar v )
{
    Pair p = cellAssoc(v,sub);
    if (nonNull(p)) {
        return snd(p);
    } else {
        return v;
    }
}

static StgAtom substAtom ( List sub, StgAtom a )
{
    switch (whatIs(a)) {
    case STGVAR: 
            return substVar(sub,a);
    default:
            return a;
    }
}

static Void substBind( List sub, StgVar bind )
{
    StgRhs rhs = stgVarBody(bind);
    switch (whatIs(rhs)) {
    case STGCON:
            map1Over(substAtom,sub,stgConArgs(rhs));
            return;
    default:
            stgVarBody(bind) = substExpr(sub,rhs);
            return;
    }
}

static Void substAlt( List sub, StgCaseAlt alt )
{
    if (isDefaultAlt(alt))
       stgDefaultBody(alt) = substExpr(sub,stgDefaultBody(alt)); else
       stgCaseAltBody(alt) = substExpr(sub,stgCaseAltBody(alt));
}

static Void substPrimAlt( List sub, StgPrimAlt alt )
{
    stgPrimAltBody(alt) = substExpr(sub,stgPrimAltBody(alt));
}

StgExpr substExpr( List sub, StgExpr e )
{
    switch (whatIs(e)) {
    case LETREC:
            map1Proc(substBind,sub,stgLetBinds(e));
            stgLetBody(e) = substExpr(sub,stgLetBody(e));
            break;
    case LAMBDA:
            stgLambdaBody(e) = substExpr(sub,stgLambdaBody(e));
            break;
    case CASE:
            stgCaseScrut(e) = substExpr(sub,stgCaseScrut(e));
            map1Proc(substAlt,sub,stgCaseAlts(e));
            break;
    case PRIMCASE:
            stgPrimCaseScrut(e) = substExpr(sub,stgPrimCaseScrut(e));
            map1Proc(substPrimAlt,sub,stgPrimCaseAlts(e));
            break;
    case STGPRIM:
            map1Over(substAtom,sub,stgPrimArgs(e));
            break;
    case STGAPP:
            stgAppFun(e) = substVar(sub,stgAppFun(e));
            map1Over(substAtom,sub,stgAppArgs(e));
            break;
    case STGCON:
            map1Over(substAtom,sub,stgConArgs(e));
            break;
    case STGVAR:
    case NAME:
            return substVar(sub,e);
    default:
            internal("substExpr");
    }
    return e;
}


/*-------------------------------------------------------------------------*/
