/* -*- mode: hugs-c; -*- */
/* --------------------------------------------------------------------------
 * Substitute variables in an expression
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: stgSubst.c,v $
 * $Revision: 1.2 $
 * $Date: 1998/12/02 13:22:40 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "stg.h"

#include "stgSubst.h"

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
    case STGVAR:
    case NAME:
            return substVar(sub,e);
    default:
            internal("substExpr");
    }
    return e;
}

/*-------------------------------------------------------------------------*/
