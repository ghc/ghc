
/* --------------------------------------------------------------------------
 * Substitute variables in an expression
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: stgSubst.c,v $
 * $Revision: 1.4 $
 * $Date: 1999/04/27 10:07:04 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "backend.h"
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


/* A substitution engine more suitable for the optimiser.
   Doesn't make so many assumptions about what is an atom.
*/
StgExpr zubstExpr( List sub, StgExpr e )
{
    List bs;
    switch (whatIs(e)) {
    case LETREC:
            for (bs=stgLetBinds(e); nonNull(bs); bs=tl(bs))
               stgVarBody(hd(bs)) = zubstExpr(sub,stgVarBody(hd(bs)));
            stgLetBody(e) = zubstExpr(sub,stgLetBody(e));
            break;
    case LAMBDA:
            stgLambdaBody(e) = zubstExpr(sub,stgLambdaBody(e));
            break;
    case CASE:
            stgCaseScrut(e) = zubstExpr(sub,stgCaseScrut(e));
            map1Proc(zubstExpr,sub,stgCaseAlts(e));
            break;
    case PRIMCASE:
            stgPrimCaseScrut(e) = zubstExpr(sub,stgPrimCaseScrut(e));
            map1Proc(zubstExpr,sub,stgPrimCaseAlts(e));
            break;
    case CASEALT:
            stgCaseAltBody(e) = zubstExpr(sub,stgCaseAltBody(e));
            break;
    case DEEFALT:
            stgDefaultBody(e) = zubstExpr(sub,stgDefaultBody(e));
            break;
    case PRIMALT:
            stgPrimAltBody(e) = zubstExpr(sub,stgPrimAltBody(e));
            break;
    case STGPRIM:
            map1Over(zubstExpr,sub,stgPrimArgs(e));
            break;
    case STGAPP:
            stgAppFun(e) = zubstExpr(sub,stgAppFun(e));
            map1Over(zubstExpr,sub,stgAppArgs(e));
            break;
    case STGCON:
            map1Over(zubstExpr,sub,stgConArgs(e));
            break;
    case STGVAR:
            return substVar(sub,e);
    case NAME:
    case INTCELL:
    case STRCELL:
    case PTRCELL:
    case CHARCELL:
    case FLOATCELL:
            break;
    default:
            internal("zubstExpr");
    }
    return e;
}



/*-------------------------------------------------------------------------*/
