/* -*- mode: hugs-c; -*- */
/* --------------------------------------------------------------------------
 * Optimiser
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: optimise.c,v $
 * $Revision: 1.2 $
 * $Date: 1998/12/02 13:22:23 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "stg.h"
#include "optimise.h"

/* --------------------------------------------------------------------------
 * Local functions
 * ------------------------------------------------------------------------*/

static StgAtom    optimiseAtom    Args((StgAtom));
static StgVar     optimiseVar     Args((StgVar));
static StgCaseAlt optimiseAlt     Args((StgCaseAlt));
static StgPrimAlt optimisePrimAlt Args((StgPrimAlt));
static StgExpr    optimiseExpr    Args((StgExpr));

/* --------------------------------------------------------------------------
 * A simple optimiser
 * ------------------------------------------------------------------------*/

static StgAtom optimiseAtom(StgAtom a)
{
    switch (whatIs(a)) {
    case STGVAR:
            return optimiseVar(a);
    /* Note that NAMEs have no free vars. */
    default:
            return a;
    }
}

static StgVar optimiseVar(StgVar v)
{
    StgRhs rhs = stgVarBody(v);
    /* short circuit: let x = y in ...x... --> let x = y ...y... */
    if (whatIs(rhs) == STGVAR && rhs != v) {
	StgVar v1 = rhs;

	/* find last variable in chain */
	rhs = stgVarBody(v1);
	while (whatIs(rhs) == STGVAR
	       && rhs != v  /* infinite loop */
	       ) {
	    v1 = rhs;
	    rhs = stgVarBody(rhs);
	}

	/* Make all variables in chain point to v1
	 * This makes sure we always resolve cycles the same way
	 * as well as making things faster if we call optimiseVar again
	 */
	while (v != v1) {
	    StgRhs r = stgVarBody(v);
	    assert(whatIs(r) == STGVAR);
	    stgVarBody(v) = v1;
	    v = r;
	}
	return v1;
    }
    return v;
}

void optimiseBind( StgVar v )
{
    StgRhs rhs = stgVarBody(v);
    switch (whatIs(rhs)) {
    case STGCON:
            mapOver(optimiseAtom,stgConArgs(rhs));
	    break;
    default:
            stgVarBody(v) = optimiseExpr(rhs);
	    break;
    }
}

static StgCaseAlt optimiseAlt( StgCaseAlt alt )
{
    /* StgPat pat = stgCaseAltPat(alt); */
    stgCaseAltBody(alt) = optimiseExpr(stgCaseAltBody(alt));
    return alt;
}

static StgPrimAlt optimisePrimAlt( StgPrimAlt alt )
{
    /* List vs = stgPrimAltPats(alt); */
    stgPrimAltBody(alt) = optimiseExpr(stgPrimAltBody(alt));
    return alt;
}

static StgExpr optimiseExpr( StgExpr e )
{
    switch (whatIs(e)) {
    case LETREC:
	{
	    List binds = stgLetBinds(e);
	    {
		/* First we filter out trivial bindings.
		 * this has to be done before optimising the individual
		 * bindings so that we don't get confused by the results
		 * of other optimisations.
		 */
		List bs = binds;
		binds = NIL;
		for(; nonNull(bs); bs=tl(bs)) {
		    StgVar b = optimiseVar(hd(bs));
		    StgRhs rhs = stgVarBody(b);
		    if (whatIs(rhs) == STGVAR && b != rhs) {
			/* This variable will be short-circuited
			 * by optimiseVar so we can drop the binding
			 * right now.
			 */
		    } else {
			binds = cons(hd(bs),binds);
		    }
		}
		binds = rev(binds); /* preserve original order */
	    }
            stgLetBody(e) = optimiseExpr(stgLetBody(e));
	    if (isNull(binds)) {
		return stgLetBody(e);
	    } else {
		mapProc(optimiseBind,binds);
		stgLetBinds(e) = binds;
	    }
	    break;
	}
    case LAMBDA:
            stgLambdaBody(e) = optimiseExpr(stgLambdaBody(e));
	    break;
    case CASE:
	{ 
	    StgExpr scrut = optimiseExpr(stgCaseScrut(e));
	    StgExpr alts  = stgCaseAlts(e);
	    if (whatIs(scrut) == STGVAR
		&& whatIs(stgVarBody(scrut)) == STGCON
		) {
		StgRhs rhs = stgVarBody(scrut);
		StgDiscr d = stgConCon(rhs);
		List  args = stgConArgs(rhs);
		for(; nonNull(alts); alts=tl(alts)) {
		    StgCaseAlt alt = hd(alts);
		    StgPat     pat = stgCaseAltPat(alt);
		    if (isDefaultPat(pat)) {  /* the easy case */
			StgExpr body = stgCaseAltBody(alt);
			stgVarBody(pat) = rhs;
			return optimiseExpr(body);
		    } else if (stgPatDiscr(pat) == d) {
			/* The tricky case:
			 * rebind all the pattern args to the con args
			 * and rebind the pattern var to con
			 * and run optimiser (to eliminate the binding)
			 */
			StgExpr body  = stgCaseAltBody(alt);
			List    binds = stgPatVars(pat);
			{
			    List vs = binds;
			    for(; 
				nonNull(vs) && nonNull(args);
				vs = tl(vs), args=tl(args)
				) {
				stgVarBody(hd(vs)) = hd(args);
			    }
			}   
			binds = cons(pat,binds);  /* turn patvar into a var! */
			stgVarBody(pat) = rhs;

			/* This letrec will always be optimised away */
			body = makeStgLet(binds,body);
			return optimiseExpr(body);
		    }
		}
		internal("optimiseExpr: no patterns matched");
	    }
            stgCaseScrut(e) = scrut;
            mapOver(optimiseAlt,alts);
	    break;
	}
    case PRIMCASE:
            mapOver(optimisePrimAlt,stgPrimCaseAlts(e));
            stgPrimCaseScrut(e) = optimiseExpr(stgPrimCaseScrut(e));
	    break;
    case STGPRIM:
            mapOver(optimiseAtom,stgPrimArgs(e));
            /* primop is not a var */
	    break;
    case STGAPP:
            stgAppFun(e) = optimiseExpr(stgAppFun(e));
            mapOver(optimiseAtom,stgAppArgs(e));
            break;
    case STGVAR:
            return optimiseVar(e);
    case NAME:
            break;  /* Names are never free vars */
    default:
            internal("optimiseExpr");
    }
    return e;
}

/*-------------------------------------------------------------------------*/
