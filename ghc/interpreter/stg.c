/* -*- mode: hugs-c; -*- */
/* --------------------------------------------------------------------------
 * STG syntax
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: stg.c,v $
 * $Revision: 1.2 $
 * $Date: 1998/12/02 13:22:38 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "stg.h"
#include "link.h"      /* for nameTrue/False     */
#include "Assembler.h" /* for AsmRep and primops */

/* --------------------------------------------------------------------------
 * Utility functions
 * ------------------------------------------------------------------------*/

int stgConTag( StgDiscr d )
{
    switch (whatIs(d)) {
    case NAME:
            return cfunOf(d);
    case TUPLE: 
            return 0;
    default: 
            internal("stgConTag");
    }
}

void* stgConInfo( StgDiscr d )
{
    switch (whatIs(d)) {
    case NAME:
            return asmMkInfo(cfunOf(d),name(d).arity);
    case TUPLE: 
            return asmMkInfo(0,tupleOf(d));
    default: 
            internal("stgConInfo");
    }
}

/* ToDo: identical to stgConTag */
int stgDiscrTag( StgDiscr d )
{
    switch (whatIs(d)) {
    case NAME:
            return cfunOf(d);
    case TUPLE: 
            return 0;
    default: 
            internal("stgDiscrTag");
    }
}

/* --------------------------------------------------------------------------
 * Utility functions for manipulating STG syntax trees.
 * ------------------------------------------------------------------------*/

List makeArgs( Int n )
{
    List args = NIL;
    for(; n>0; --n) {
        args = cons(mkStgVar(NIL,NIL),args);
    }
    return args;
}

StgExpr makeStgLambda( List args, StgExpr body )
{
    if (isNull(args)) {
        return body;
    } else {
        if (whatIs(body) == LAMBDA) {
            return mkStgLambda(dupListOnto(args,stgLambdaArgs(body)),
                               stgLambdaBody(body));
        } else {
            return mkStgLambda(args,body);
        }
    }
}

StgExpr makeStgApp( StgVar fun, List args )
{
    if (isNull(args)) {
        return fun;
    } else {
        return mkStgApp(fun,args);
    }
}

StgExpr makeStgLet( List binds, StgExpr body )
{
    if (isNull(binds)) {
        return body;
    } else {
        return mkStgLet(binds,body);
    }
}

StgExpr makeStgIf( StgExpr cond, StgExpr e1, StgExpr e2 )
{
    if (cond == nameTrue) {
        return e1;
    } else if (cond == nameFalse) {
        return e2;
    } else {
        return mkStgCase(cond,doubleton(mkStgCaseAlt(nameTrue,NIL,e1),
                                        mkStgCaseAlt(nameFalse,NIL,e2))); 
    }
}

Bool isStgVar(e)
StgRhs e; {
    switch (whatIs(e)) {
    case STGVAR:
            return TRUE;
    default:
            return FALSE;
    }
}

Bool isAtomic(e) 
StgRhs e; {
    switch (whatIs(e)) {
    case STGVAR:
    case NAME:
    case CHARCELL:
    case INTCELL:
    case BIGCELL:
    case FLOATCELL:
    case STRCELL:
    case PTRCELL:
            return TRUE;
    default:
            return FALSE;
    }
}

StgVar mkStgVar( StgRhs rhs, Cell info )
{
    return ap(STGVAR,triple(rhs,mkStgRep(PTR_REP),info));
}

/*-------------------------------------------------------------------------*/
