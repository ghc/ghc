/* -*- mode: hugs-c; -*- */
/* --------------------------------------------------------------------------
 * This is the Hugs compiler, handling translation of typechecked code to
 * `kernel' language, elimination of pattern matching and translation to
 * super combinators (lambda lifting).
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: compiler.c,v $
 * $Revision: 1.2 $
 * $Date: 1998/12/02 13:22:01 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "input.h"
#include "compiler.h"
#include "hugs.h"  /* for target */
#include "errors.h"

#include "desugar.h"
#include "pmc.h"

#include "optimise.h"

#include "Rts.h"    /* for rts_eval and related stuff */
#include "RtsAPI.h" /* for rts_eval and related stuff */

Name currentName;                      /* Top level name being processed   */
#if DEBUG_CODE
Bool   debugCode     = FALSE;           /* TRUE => print G-code to screen  */
#endif

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static List local addGlobals( List binds );
static Void local compileGlobalFunction Args((Pair));
static Void local compileGenFunction    Args((Name));
static Name local compileSelFunction    Args((Pair));

/* --------------------------------------------------------------------------
 * STG stuff
 * ------------------------------------------------------------------------*/

#include "stg.h"
#include "translate.h"
#include "codegen.h"

static Void local stgCGBinds( List );

static Void local stgCGBinds(binds)
List binds; {
    cgBinds(binds);
}

/* --------------------------------------------------------------------------
 * Main entry points to compiler:
 * ------------------------------------------------------------------------*/

static List addGlobals( List binds )
{
    /* stgGlobals = pieces of code generated for selectors, tuples, etc */
    for(;nonNull(stgGlobals);stgGlobals=tl(stgGlobals)) {
        StgVar bind = snd(hd(stgGlobals));
        if (nonNull(stgVarBody(bind))) {
            binds = cons(bind,binds);
        }
    }
    return binds;
}

#if 0
/* This is a hack to see if "show [1..1000]" will go any faster if I
 * code primShowInt in C
 */
char* prim_showInt(int x)
{
    char buffer[50];
    sprintf(buffer,"%d",x);
    return buffer;
}

void prim_flush_stdout(void)
{
    fflush(stdout);
}
#endif

Void evalExp() {                    /* compile and run input expression    */
    /* ToDo: this name (and other names generated during pattern match?)
     * get inserted in the symbol table but never get removed.
     */
    Name n = newName(inventText());
    StgVar v = mkStgVar(NIL,NIL);
    name(n).stgVar = v;
    compiler(RESET);
    stgDefn(n,0,pmcTerm(0,NIL,translate(inputExpr)));
    inputExpr = NIL;
    stgCGBinds(addGlobals(singleton(v)));
    

    /* Run thread (and any other runnable threads) */

    /* Re-initialise the scheduler - ToDo: do I need this? */
    initScheduler();
    {
        HaskellObj result; /* ignored */
        SchedulerStatus status = rts_eval_(closureOfVar(v),10000,&result);
        switch (status) {
        case Deadlock:
        case AllBlocked: /* I don't understand the distinction - ADR */
                printf("{Deadlock}");
                RevertCAFs();
                break;
        case Interrupted:
                printf("{Interrupted}");
                RevertCAFs();
                break;
        case Killed:
                printf("{Killed}");
                RevertCAFs();
                break;
        case Success:
                /* Nothing to do */
                break;
        default:
                internal("evalExp: Unrecognised SchedulerStatus");
        }
        fflush(stdout);
        fflush(stderr);
    }
}

static List local addStgVar( List binds, Pair bind ); /* todo */

static List local addStgVar( List binds, Pair bind )
{
    StgVar nv = mkStgVar(NIL,NIL);
    Text   t  = textOf(fst(bind));
    Name   n  = findName(t);

    if (isNull(n)) {                   /* Lookup global name - the only way*/
        n = newName(t);                /* this (should be able to happen)  */
    }                                  /* is with new global var introduced*/
                                       /* after type check; e.g. remPat1   */
    name(n).stgVar = nv;
    return cons(nv,binds);
}


Void compileDefns() {                  /* compile script definitions       */
    Target t = length(valDefns) + length(genDefns) + length(selDefns);
    Target i = 0;

    List binds = NIL;
    {
        List vss;
        List vs;
        for(vs=genDefns; nonNull(vs); vs=tl(vs)) {
            Name   n  = hd(vs);
            StgVar nv = mkStgVar(NIL,NIL);
            assert(isName(n));
            name(n).stgVar = nv;
            binds = cons(nv,binds);
        }
        for(vss=selDefns; nonNull(vss); vss=tl(vss)) {
            for(vs=hd(vss); nonNull(vs); vs=tl(vs)) {
                Pair p = hd(vs);
                Name n = fst(p);
                StgVar nv = mkStgVar(NIL,NIL);
                assert(isName(n));
                name(n).stgVar = nv;
                binds = cons(nv,binds);
            }
        }
    }

    setGoal("Compiling",t);
    /* do valDefns before everything else so that all stgVar's get added. */
    for (; nonNull(valDefns); valDefns=tl(valDefns)) {
        hd(valDefns) = transBinds(hd(valDefns));
        mapAccum(addStgVar,binds,hd(valDefns));
        mapProc(compileGlobalFunction,hd(valDefns));
        soFar(i++);
    }
    for (; nonNull(genDefns); genDefns=tl(genDefns)) {
        compileGenFunction(hd(genDefns));
        soFar(i++);
    }
    for (; nonNull(selDefns); selDefns=tl(selDefns)) {
        mapOver(compileSelFunction,hd(selDefns));
        soFar(i++);
    }

    /* binds=revOnto(binds,NIL); *//* ToDo: maintain compilation order?? */
    binds = addGlobals(binds);
#if USE_HUGS_OPTIMIZER
    mapProc(optimiseBind,binds);
#endif
    stgCGBinds(binds);

    done();
}

static Void local compileGlobalFunction(bind)
Pair bind; {
    Name n     = findName(textOf(fst(bind)));
    List defs  = snd(bind);
    Int  arity = length(fst(hd(defs)));
    assert(isName(n));
    compiler(RESET);
    stgDefn(n,arity,match(arity,altsMatch(1,arity,NIL,defs)));
}

static Void local compileGenFunction(n) /* Produce code for internally     */
Name n; {                               /* generated function              */
    List defs  = name(n).defn;
    Int  arity = length(fst(hd(defs)));

    compiler(RESET);
    mapProc(transAlt,defs);
    stgDefn(n,arity,match(arity,altsMatch(1,arity,NIL,defs)));
    name(n).defn = NIL;
}

static Name local compileSelFunction(p) /* Produce code for selector func  */
Pair p; {                               /* Should be merged with genDefns, */
    Name s     = fst(p);                /* but the name(_).defn field is   */
    List defs  = snd(p);                /* already used for other purposes */
    Int  arity = length(fst(hd(defs))); /* in selector functions.          */

    compiler(RESET);
    mapProc(transAlt,defs);
    stgDefn(s,arity,match(arity,altsMatch(1,arity,NIL,defs)));
    return s;
}

/* --------------------------------------------------------------------------
 * Compiler control:
 * ------------------------------------------------------------------------*/

Void compiler(what)
Int what; {
    switch (what) {
        case INSTALL :
        case RESET   : break;
        case MARK    : break;
    }
}

/*-------------------------------------------------------------------------*/
