
/* --------------------------------------------------------------------------
 * STG syntax
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: stg.c,v $
 * $Revision: 1.4 $
 * $Date: 1999/03/01 14:46:53 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "backend.h"
#include "connect.h"
#include "errors.h"
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
  //printf("{%d %d %d} ", namePMFail, e, whatIs(e) );
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

/* --------------------------------------------------------------------------
 * STG pretty printer
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: stg.c,v $
 * $Revision: 1.4 $
 * $Date: 1999/03/01 14:46:53 $
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Local functions
 * ------------------------------------------------------------------------*/

static Void local pIndent        Args((Int));

static Void local putStgVar       Args((StgVar));
static Void local putStgVars      Args((List));
static Void local putStgAtom      Args((StgAtom a));
static Void local putStgAtoms     Args((List as));
static Void local putStgBinds     Args((List));
static Void local putStgExpr      Args((StgExpr));
static Void local putStgRhs       Args((StgRhs));
static Void local putStgPat       Args((StgPat));
static Void local putStgPrimPat   Args((StgPrimPat));


/* --------------------------------------------------------------------------
 * Indentation and showing names/constants
 * ------------------------------------------------------------------------*/

static Void local pIndent(n)           /* indent to particular position    */
Int n; {
    outColumn = n;
    while (0<n--) {
        Putc(' ',outputStream);
    }
}


/* --------------------------------------------------------------------------
 * Pretty printer for stg code:
 * ------------------------------------------------------------------------*/

static Void putStgAlts    ( Int left, List alts );
//static Void putStgPrimAlt ( Int left, List vs, StgExpr body );

static Void local putStgVar(StgVar v) 
{
    if (isName(v)) {
        unlexVar(name(v).text);
    } else {
        putStr("id");
        putInt(-v);
    }
}

static Void local putStgVars( List vs )
{
    for(; nonNull(vs); vs=tl(vs)) {
        putStgVar(hd(vs));
        putChr(' ');
    }
}

static Void local putStgAtom( StgAtom a )
{
    switch (whatIs(a)) {
    case STGVAR: 
    case NAME: 
            putStgVar(a);
            break;
    case CHARCELL: 
            unlexCharConst(charOf(a));
            putChr('#');
            break;
    case INTCELL: 
            putInt(intOf(a));
            putChr('#');
            break;
    case BIGCELL: 
            putStr(bignumToString(a));
            putChr('#');
            break;
    case FLOATCELL: 
            putStr(floatToString(a));
            putChr('#');
            break;
    case STRCELL: 
            unlexStrConst(textOf(a));
            break;
    case PTRCELL: 
            putPtr(ptrOf(a));
            putChr('#');
            break;
    default: 
            fprintf(stderr,"\nYoiks: "); printExp(stderr,a);
            internal("putStgAtom");
    }
}

Void putStgAtoms( List as )
{
    putChr('{');
    while (nonNull(as)) {
        putStgAtom(hd(as));
        as=tl(as);
        if (nonNull(as)) {
            putChr(',');
        }
    }
    putChr('}');
}

Void putStgPat( StgPat pat )
{
    putStgVar(pat);
    if (nonNull(stgVarBody(pat))) {
        StgDiscr d  = stgConCon(stgVarBody(pat));
        List     vs = stgConArgs(stgVarBody(pat));
        putChr('@');
        switch (whatIs(d)) {
        case NAME:
            { 
                unlexVar(name(d).text);
                for (; nonNull(vs); vs=tl(vs)) {
                    putChr(' ');
                    putStgVar(hd(vs));
                }
                break;
            }
        case TUPLE: 
            { 
                putChr('(');
                putStgVar(hd(vs));
                vs=tl(vs);
                while (nonNull(vs)) {
                    putChr(',');
                    putStgVar(hd(vs));
                    vs=tl(vs);
                }
                putChr(')');
                break;
            }
        default: 
                fprintf(stderr,"\nYoiks: "); printExp(stderr,d);
                internal("putStgPat");
        }
    }
}

Void putStgPrimPat( StgPrimPat pat )  
{
    putStgVar(pat);
    if (nonNull(stgVarBody(pat))) {
        StgExpr d  = stgVarBody(pat);
        putChr('@');
        switch (whatIs(d)) {
        case INTCELL:
            {
                putInt(intOf(d));
                putChr('#');
                break;
            }
        default: 
                fprintf(stderr,"\nYoiks: "); printExp(stderr,d);
                internal("putStgPrimPat");
        }
    }
    putChr(' ');
}

Void putStgBinds(binds)        /* pretty print locals           */
List binds; {
    Int left = outColumn;

    putStr("let { ");
    while (nonNull(binds)) {
        Cell bind = hd(binds);
        putStgVar(bind);
        putStr(" = ");
        putStgRhs(stgVarBody(bind));
        putStr("\n");
        binds = tl(binds);
        if (nonNull(binds))
            pIndent(left+6);
    }
    pIndent(left);
    putStr("} in  ");
}

static Void putStgAlts( Int left, List alts )
{
  if (length(alts) == 1) {
        StgCaseAlt alt = hd(alts);
        putStr("{ ");
        putStgPat(stgCaseAltPat(alt));
        putStr(" ->\n");
        pIndent(left);
        putStgExpr(stgCaseAltBody(alt));
        putStr("}");
    } else {
        putStr("{\n");
        for (; nonNull(alts); alts=tl(alts)) {
            StgCaseAlt alt = hd(alts);
            pIndent(left+2);
            putStgPat(stgCaseAltPat(alt));

            //putStr(" -> ");
            putStr(" ->\n");
            pIndent(left+4);

            putStgExpr(stgCaseAltBody(alt));
            putStr("\n");
        }
        pIndent(left);
        putStr("}\n");
    }
}

static Void putStgPrimAlts( Int left, List alts )
{
    if (length(alts) == 1) {
        StgPrimAlt alt = hd(alts);
        putStr("{ ");
        mapProc(putStgPrimPat,stgPrimAltPats(alt));
        putStr(" ->\n");
        pIndent(left);
        putStgExpr(stgPrimAltBody(alt));
        putStr("}");
    } else {
        putStr("{\n");
        for (; nonNull(alts); alts=tl(alts)) {
            StgPrimAlt alt = hd(alts);
            pIndent(left+2);
            mapProc(putStgPrimPat,stgPrimAltPats(alt));
            putStr(" -> ");
            putStgExpr(stgPrimAltBody(alt));
            putStr("\n");
        }
        pIndent(left);
        putStr("}\n");
    }
}

Void putStgExpr( StgExpr e )                        /* pretty print expr */
{
    switch (whatIs(e)) {
    case LETREC: 
            putStgBinds(stgLetBinds(e));
            putStgExpr(stgLetBody(e));
            break;
    case LAMBDA:
        {   
            Int left = outColumn;
            putStr("\\ ");
            putStgVars(stgLambdaArgs(e));
            putStr("->\n");
            pIndent(left+2);
            putStgExpr(stgLambdaBody(e));
            break;
        }
    case CASE: 
        {
            Int left = outColumn;
            putStr("case ");
            putStgExpr(stgCaseScrut(e));
            putStr(" of ");
            putStgAlts(left,stgCaseAlts(e));
            break;
        }
    case PRIMCASE:
        { 
            Int  left = outColumn;
            putStr("case# ");
            putStgExpr(stgPrimCaseScrut(e));
            putStr(" of ");
            putStgPrimAlts(left,stgPrimCaseAlts(e));
            break;
        }
    case STGPRIM: 
        {
            Cell op = stgPrimOp(e);
            unlexVar(name(op).text);
            putStgAtoms(stgPrimArgs(e));
            break;
        }
    case STGAPP: 
            putStgVar(stgAppFun(e));
            putStgAtoms(stgAppArgs(e));
            break;
    case STGVAR: 
    case NAME: 
            putStgVar(e);
            break;
    default: 
      //fprintf(stderr,"\nYoiks: "); printExp(stderr,e);
      //internal("putStgExpr");
      //ToDo: rm this appalling hack
      fprintf(stderr, "   "); putStgAlts(3,e);
    }
}

Void putStgRhs( StgRhs e )            /* print lifted definition         */
{
    switch (whatIs(e)) {
    case STGCON:
        {
            Name   con  = stgConCon(e);
            if (isTuple(con)) {
                putStr("Tuple");
                putInt(tupleOf(con));
            } else {
                unlexVar(name(con).text);
            }
            putStgAtoms(stgConArgs(e));
            break;
        }
    default: 
            putStgExpr(e);
            break;
    }
}

static void beginStgPP( FILE* fp );
static void endStgPP( FILE* fp );

static void beginStgPP( FILE* fp )
{
    outputStream = fp;
    //putChr('\n');
    outColumn = 0;
}

static void endStgPP( FILE* fp )
{
    fflush(fp);
}

Void printStg(fp,b)              /* Pretty print sc defn on fp      */
FILE  *fp;
StgVar b; 
{
    beginStgPP(fp);
    putStgVar(b);
    putStr(" = ");
    putStgRhs(stgVarBody(b));
    putStr("\n");
    endStgPP(fp);
}

#if 1 /*DEBUG_PRINTER*/
Void ppStg( StgVar v )
{
  if ( 1 /*debugCode*/ ) {
        printStg(stdout,v);
    }
}

Void ppStgExpr( StgExpr e )
{
    if ( 1 /*debugCode*/ ) {
        beginStgPP(stderr);
        putStgExpr(e);
        endStgPP(stdout);
    }
}

Void ppStgRhs( StgRhs rhs )
{
  if (1 /*debugCode*/ ) {
        beginStgPP(stdout);
        putStgRhs(rhs);
        endStgPP(stdout);
    }
}

Void ppStgAlts( List alts )
{
    if (debugCode) {
        beginStgPP(stdout);
        putStgAlts(0,alts);
        endStgPP(stdout);
    }
}

extern Void ppStgPrimAlts( List alts )
{
    if (debugCode) {
        beginStgPP(stdout);
        putStgPrimAlts(0,alts);
        endStgPP(stdout);
    }
}

extern Void ppStgVars( List vs )
{
    if (debugCode) {
        beginStgPP(stdout);
        printf("Vars: ");
        putStgVars(vs);
        printf("\n");
        endStgPP(stdout);
    }
}
#endif

/*-------------------------------------------------------------------------*/
