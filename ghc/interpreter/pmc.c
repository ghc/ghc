/* -*- mode: hugs-c; -*- */
/* --------------------------------------------------------------------------
 * Pattern matching Compiler
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: pmc.c,v $
 * $Revision: 1.2 $
 * $Date: 1998/12/02 13:22:29 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "link.h"

#include "desugar.h"
#include "pat.h"
#include "pmc.h"

/* --------------------------------------------------------------------------
 * Eliminate pattern matching in function definitions -- pattern matching
 * compiler:
 *
 * The original Gofer/Hugs pattern matching compiler was based on Wadler's
 * algorithms described in `Implementation of functional programming
 * languages'.  That should still provide a good starting point for anyone
 * wanting to understand this part of the system.  However, the original
 * algorithm has been generalized and restructured in order to implement
 * new features added in Haskell 1.3.
 *
 * During the translation, in preparation for later stages of compilation,
 * all local and bound variables are replaced by suitable offsets, and
 * locally defined function symbols are given new names (which will
 * eventually be their names when lifted to make top level definitions).
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Cell local pmcPair               Args((Int,List,Pair));
static Cell local pmcTriple             Args((Int,List,Triple));
static Cell local pmcVar                Args((List,Text));
static Void local pmcLetrec             Args((Int,List,Pair));
static Cell local pmcVarDef             Args((Int,List,List));
static Void local pmcFunDef             Args((Int,List,Triple));
static Cell local joinMas               Args((Int,List));
static Bool local canFail               Args((Cell));
static List local addConTable           Args((Cell,Cell,List));
static Void local advance               Args((Int,Int,Cell));
static Bool local emptyMatch            Args((Cell));
static Cell local maDiscr               Args((Cell));
static Bool local isNumDiscr            Args((Cell));
static Bool local eqNumDiscr            Args((Cell,Cell));
#if TREX
static Bool local isExtDiscr            Args((Cell));
static Bool local eqExtDiscr            Args((Cell,Cell));
#endif

/* --------------------------------------------------------------------------
 * 
 * ------------------------------------------------------------------------*/

Cell pmcTerm(co,sc,e)                  /* apply pattern matching compiler  */
Int  co;                               /* co = current offset              */
List sc;                               /* sc = scope                       */
Cell e;  {                             /* e  = expr to transform           */
    switch (whatIs(e)) {
        case GUARDED  : map2Over(pmcPair,co,sc,snd(e));
                        break;

        case LETREC   : pmcLetrec(co,sc,snd(e));
                        break;

        case VARIDCELL:
        case VAROPCELL:
        case DICTVAR  : return pmcVar(sc,textOf(e));

        case COND     : return ap(COND,pmcTriple(co,sc,snd(e)));

        case AP       : return pmcPair(co,sc,e);

#if NPLUSK
        case ADDPAT   :
#endif
#if TREX
        case EXT      :
#endif
        case TUPLE    :
        case NAME     :
        case CHARCELL :
        case INTCELL  :
        case BIGCELL  :
        case FLOATCELL:
        case STRCELL  : break;

        default       : internal("pmcTerm");
                        break;
    }
    return e;
}

static Cell local pmcPair(co,sc,pr)    /* apply pattern matching compiler  */
Int  co;                               /* to a pair of exprs               */
List sc;
Pair pr; {
    return pair(pmcTerm(co,sc,fst(pr)),
                pmcTerm(co,sc,snd(pr)));
}

static Cell local pmcTriple(co,sc,tr)  /* apply pattern matching compiler  */
Int    co;                             /* to a triple of exprs             */
List   sc;
Triple tr; {
    return triple(pmcTerm(co,sc,fst3(tr)),
                  pmcTerm(co,sc,snd3(tr)),
                  pmcTerm(co,sc,thd3(tr)));
}

static Cell local pmcVar(sc,t)         /* find translation of variable     */
List sc;                               /* in current scope                 */
Text t; {
    List xs;
    Name n;

    for (xs=sc; nonNull(xs); xs=tl(xs)) {
        Cell x = hd(xs);
        if (t==textOf(fst(x)))
            if (isOffset(snd(x))) {                  /* local variable ... */
                return snd(x);
            }
            else {                                   /* local function ... */
                return fst3(snd(x));
            }
    }

    n = findName(t);
    assert(nonNull(n));
    return n;
}

static Void local pmcLetrec(co,sc,e)   /* apply pattern matching compiler  */
Int  co;                               /* to LETREC, splitting decls into  */
List sc;                               /* two sections                     */
Pair e; {
    List fs = NIL;                     /* local function definitions       */
    List vs = NIL;                     /* local variable definitions       */
    List ds;

    for (ds=fst(e); nonNull(ds); ds=tl(ds)) {      /* Split decls into two */
        Cell v     = fst(hd(ds));
        Int  arity = length(fst(hd(snd(hd(ds)))));

        if (arity==0) {                            /* Variable declaration */
            vs = cons(snd(hd(ds)),vs);
            sc = cons(pair(v,mkOffset(++co)),sc);
        }
        else {                                     /* Function declaration */
            fs = cons(triple(inventVar(),mkInt(arity),snd(hd(ds))),fs);
            sc = cons(pair(v,hd(fs)),sc);
        }
    }
    vs       = rev(vs);                /* Put declaration lists back in    */
    fs       = rev(fs);                /* original order                   */
    fst(e)   = pair(vs,fs);            /* Store declaration lists          */
    map2Over(pmcVarDef,co,sc,vs);      /* Translate variable definitions   */
    map2Proc(pmcFunDef,co,sc,fs);      /* Translate function definitions   */
    snd(e)   = pmcTerm(co,sc,snd(e));  /* Translate LETREC body            */
}

static Cell local pmcVarDef(co,sc,vd)  /* apply pattern matching compiler  */
Int  co;                               /* to variable definition           */
List sc;
List vd; {                             /* vd :: [ ([], rhs) ]              */
    Cell d = snd(hd(vd));
    if (nonNull(tl(vd)) && canFail(d))
        return ap(FATBAR,pair(pmcTerm(co,sc,d),
                              pmcVarDef(co,sc,tl(vd))));
    return pmcTerm(co,sc,d);
}

static Void local pmcFunDef(co,sc,fd)  /* apply pattern matching compiler  */
Int    co;                             /* to function definition           */
List   sc;
Triple fd; {                           /* fd :: (Var, Arity, [Alt])        */
    Int    arity         = intOf(snd3(fd));
    Cell   temp          = altsMatch(co+1,arity,sc,thd3(fd));
    Cell   xs;

    temp      = match(co+arity,temp);
    thd3(fd)  = triple(NIL,NIL,temp);  /* used to be freevar info */

}

/* ---------------------------------------------------------------------------
 * Main part of pattern matching compiler: convert [Alt] to case constructs
 *
 * This section of Hugs has been almost completely rewritten to be more
 * general, in particular, to allow pattern matching in orders other than the
 * strictly left-to-right approach of the previous version.  This is needed
 * for the implementation of the so-called Haskell 1.3 `record' syntax.
 *
 * At each stage, the different branches for the cases to be considered
 * are represented by a list of values of type:
 *   Match ::= { maPats :: [Pat],       patterns to match
 *               maOffs :: [Offs],      offsets of corresponding values
 *               maSc   :: Scope,       mapping from vars to offsets
 *               maRhs  :: Rhs }        right hand side
 * [Implementation uses nested pairs, ((pats,offs),(sc,rhs)).]
 *
 * The Scope component has type:
 *   Scope  ::= [(Var,Expr)]
 * and provides a mapping from variable names to offsets used in the matching
 * process.
 *
 * Matches can be normalized by reducing them to a form in which the list
 * of patterns is empty (in which case the match itself is described as an
 * empty match), or in which the list is non-empty and the first pattern is
 * one that requires either a CASE or NUMCASE (or EXTCASE) to decompose.  
 * ------------------------------------------------------------------------*/

#define mkMatch(ps,os,sc,r)     pair(pair(ps,os),pair(sc,r))
#define maPats(ma)              fst(fst(ma))
#define maOffs(ma)              snd(fst(ma))
#define maSc(ma)                fst(snd(ma))
#define maRhs(ma)               snd(snd(ma))
#define extSc(v,o,ma)           maSc(ma) = cons(pair(v,o),maSc(ma))

List altsMatch(co,n,sc,as)              /* Make a list of matches from list*/
Int  co;                                /* of Alts, with initial offsets   */
Int  n;                                 /* reverse (take n [co..])         */
List sc;
List as; {
    List mas = NIL;
    List us  = NIL;
    for (; n>0; n--)
        us = cons(mkOffset(co++),us);
    for (; nonNull(as); as=tl(as))      /* Each Alt is ([Pat], Rhs)        */
        mas = cons(mkMatch(fst(hd(as)),us,sc,snd(hd(as))),mas);
    return rev(mas);
}

Cell match(co,mas)              /* Generate case statement for Matches mas */
Int  co;                        /* at current offset co                    */
List mas; {                     /* N.B. Assumes nonNull(mas).              */
    Cell srhs = NIL;            /* Rhs for selected matches                */
    List smas = mas;            /* List of selected matches                */
    mas       = tl(mas);
    tl(smas)  = NIL;

    if (emptyMatch(hd(smas))) {         /* The case for empty matches:     */
        while (nonNull(mas) && emptyMatch(hd(mas))) {
            List temp = tl(mas);
            tl(mas)   = smas;
            smas      = mas;
            mas       = temp;
        }
        srhs = joinMas(co,rev(smas));
    }
    else {                              /* Non-empty match                 */
        Int  o = offsetOf(hd(maOffs(hd(smas))));
        Cell d = maDiscr(hd(smas));
        if (isNumDiscr(d)) {            /* Numeric match                   */
            Int  da = discrArity(d);
            Cell d1 = pmcTerm(co,maSc(hd(smas)),d);
            while (nonNull(mas) && !emptyMatch(hd(mas))
                                && o==offsetOf(hd(maOffs(hd(mas))))
                                && isNumDiscr(d=maDiscr(hd(mas)))
                                && eqNumDiscr(d,d1)) {
                List temp = tl(mas);
                tl(mas)   = smas;
                smas      = mas;
                mas       = temp;
            }
            smas = rev(smas);
            map2Proc(advance,co,da,smas);
            srhs = ap(NUMCASE,triple(mkOffset(o),d1,match(co+da,smas)));
        }
#if TREX
        else if (isExtDiscr(d)) {       /* Record match                    */
            Int  da = discrArity(d);
            Cell d1 = pmcTerm(co,maSc(hd(smas)),d);
            while (nonNull(mas) && !emptyMatch(hd(mas))
                                && o==offsetOf(hd(maOffs(hd(mas))))
                                && isExtDiscr(d=maDiscr(hd(mas)))
                                && eqExtDiscr(d,d1)) {
                List temp = tl(mas);
                tl(mas)   = smas;
                smas      = mas;
                mas       = temp;
            }
            smas = rev(smas);
            map2Proc(advance,co,da,smas);
            srhs = ap(EXTCASE,triple(mkOffset(o),d1,match(co+da,smas)));
        }
#endif
        else {                          /* Constructor match               */
            List tab = addConTable(d,hd(smas),NIL);
            Int  da;
            while (nonNull(mas) && !emptyMatch(hd(mas))
                                && o==offsetOf(hd(maOffs(hd(mas))))
                                && !isNumDiscr(d=maDiscr(hd(mas)))) {
                tab = addConTable(d,hd(mas),tab);
                mas = tl(mas);
            }
            for (tab=rev(tab); nonNull(tab); tab=tl(tab)) {
                d    = fst(hd(tab));
                smas = snd(hd(tab));
                da   = discrArity(d);
                map2Proc(advance,co,da,smas);
                srhs = cons(pair(d,match(co+da,smas)),srhs);
            }
            srhs = ap(CASE,pair(mkOffset(o),srhs));
        }
    }
    return nonNull(mas) ? ap(FATBAR,pair(srhs,match(co,mas))) : srhs;
}

static Cell local joinMas(co,mas)       /* Combine list of matches into rhs*/
Int  co;                                /* using FATBARs as necessary      */
List mas; {                             /* Non-empty list of empty matches */
    Cell ma  = hd(mas);
    Cell rhs = pmcTerm(co,maSc(ma),maRhs(ma));
    if (nonNull(tl(mas)) && canFail(rhs))
        return ap(FATBAR,pair(rhs,joinMas(co,tl(mas))));
    else
        return rhs;
}

static Bool local canFail(rhs)         /* Determine if expression (as rhs) */
Cell rhs; {                            /* might ever be able to fail       */
    switch (whatIs(rhs)) {
        case LETREC  : return canFail(snd(snd(rhs)));
        case GUARDED : return TRUE;    /* could get more sophisticated ..? */
        default      : return FALSE;
    }
}

/* type Table a b = [(a, [b])]
 *
 * addTable                 :: a -> b -> Table a b -> Table a b
 * addTable x y []           = [(x,[y])]
 * addTable x y (z@(n,sws):zs)
 *              | n == x     = (n,sws++[y]):zs
 *              | otherwise  = (n,sws):addTable x y zs
 */

static List local addConTable(x,y,tab) /* add element (x,y) to table       */
Cell x, y;
List tab; {
    if (isNull(tab))
        return singleton(pair(x,singleton(y)));
    else if (fst(hd(tab))==x)
        snd(hd(tab)) = appendOnto(snd(hd(tab)),singleton(y));
    else
        tl(tab) = addConTable(x,y,tl(tab));

    return tab;
}

static Void local advance(co,a,ma)      /* Advance non-empty match by      */
Int  co;                                /* processing head pattern         */
Int  a;                                 /* discriminator arity             */
Cell ma; {
    Cell p  = hd(maPats(ma));
    List ps = tl(maPats(ma));
    List us = tl(maOffs(ma));
    if (whatIs(p)==CONFLDS) {           /* Special case for record syntax  */
        Name c  = fst(snd(p));
        List fs = snd(snd(p));
        List qs = NIL;
        List vs = NIL;
        for (; nonNull(fs); fs=tl(fs)) {
            vs = cons(mkOffset(co+a+1-sfunPos(fst(hd(fs)),c)),vs);
            qs = cons(snd(hd(fs)),qs);
        }
        ps = revOnto(qs,ps);
        us = revOnto(vs,us);
    }
    else                                /* Normally just spool off patterns*/
        for (; a>0; --a) {              /* and corresponding offsets ...   */
            us = cons(mkOffset(++co),us);
            ps = cons(arg(p),ps);
            p  = fun(p);
        }

    maPats(ma) = ps;
    maOffs(ma) = us;
}

/* --------------------------------------------------------------------------
 * Normalize and test for empty match:
 * ------------------------------------------------------------------------*/

static Bool local emptyMatch(ma)/* Normalize and test to see if a given    */
Cell ma; {                      /* match, ma, is empty.                    */

    while (nonNull(maPats(ma))) {
        Cell p;
tidyHd: switch (whatIs(p=hd(maPats(ma)))) {
            case LAZYPAT   : {   Cell nv   = inventVar();
                                 maRhs(ma) = ap(LETREC,
                                                pair(remPat(snd(p),nv,NIL),
                                                     maRhs(ma)));
                                 p         = nv;
                             }
                             /* intentional fall-thru */
            case VARIDCELL :
            case VAROPCELL :
            case DICTVAR   : extSc(p,hd(maOffs(ma)),ma);
            case WILDCARD  : maPats(ma) = tl(maPats(ma));
                             maOffs(ma) = tl(maOffs(ma));
                             continue;

            /* So-called "as-patterns"are really just pattern intersections:
             *    (p1@p2:ps, o:os, sc, e) ==> (p1:p2:ps, o:o:os, sc, e)
             * (But the input grammar probably doesn't let us take
             * advantage of this, so we stick with the special case
             * when p1 is a variable.)
             */
            case ASPAT     : extSc(fst(snd(p)),hd(maOffs(ma)),ma);
                             hd(maPats(ma)) = snd(snd(p));
                             goto tidyHd;

            case FINLIST   : hd(maPats(ma)) = mkConsList(snd(p));
                             return FALSE;

            case STRCELL   : {   String s = textToStr(textOf(p));
                                 for (p=NIL; *s!='\0'; ++s) {
                                     if (*s!='\\' || *++s=='\\') {
                                         p = ap2(nameCons,mkChar(*s),p);
                                     } else {
                                         p = ap2(nameCons,mkChar('\0'),p);
                                     }
                                 }
                                 hd(maPats(ma)) = revOnto(p,nameNil);
                             }
                             return FALSE;

            case AP        : if (isName(fun(p)) && isCfun(fun(p))
                                 && cfunOf(fun(p))==0
                                 && name(fun(p)).defn==nameId) {
                                  hd(maPats(ma)) = arg(p);
                                  goto tidyHd;
                             }
                             /* intentional fall-thru */
            case CHARCELL  :
#if !OVERLOADED_CONSTANTS
            case INTCELL   :
            case BIGCELL   :
            case FLOATCELL :
#endif
            case NAME      :
            case CONFLDS   :
                             return FALSE;

            default        : internal("emptyMatch");
        }
    }
    return TRUE;
}

/* --------------------------------------------------------------------------
 * Discriminators:
 * ------------------------------------------------------------------------*/

static Cell local maDiscr(ma)   /* Get the discriminator for a non-empty   */
Cell ma; {                      /* match, ma.                              */
    Cell p = hd(maPats(ma));
    Cell h = getHead(p);
    switch (whatIs(h)) {
        case CONFLDS : return fst(snd(p));
#if NPLUSK
        case ADDPAT  : arg(fun(p)) = translate(arg(fun(p)));
                       return fun(p);
#endif
#if TREX
        case EXT     : h      = fun(fun(p));
                       arg(h) = translate(arg(h));
                       return h;
#endif
#if OVERLOADED_CONSTANTS
        case NAME    : if (h==nameFromInt || h==nameFromInteger
                                          || h==nameFromDouble) {
                           if (argCount==2)
                               arg(fun(p)) = translate(arg(fun(p)));
                           return p;
                        }
#endif
    }
    return h;
}

static Bool local isNumDiscr(d) /* TRUE => numeric discriminator           */
Cell d; {
    switch (whatIs(d)) {
        case NAME      :
        case TUPLE     :
        case CHARCELL  : return FALSE;
#if OVERLOADED_CONSTANTS
#if TREX
        case AP        : return !isExt(fun(d));
#else
        case AP        : return TRUE;   /* must be a literal or (n+k)      */
#endif
#else
        case INTCELL  :
        case BIGCELL  :
        case FLOATCELL:
                        return TRUE;
#endif
    }
    internal("isNumDiscr");
    return 0;/*NOTREACHED*/
}

Int discrArity(d)                      /* Find arity of discriminator      */
Cell d; {
    switch (whatIs(d)) {
        case NAME      : return name(d).arity;
        case TUPLE     : return tupleOf(d);
        case CHARCELL  : return 0;
#if !OVERLOADED_CONSTANTS
        case INTCELL   :
        case BIGCELL   :
        case FLOATCELL : return 0;
#endif /* !OVERLOADED_CONSTANTS */

#if TREX
        case AP        : switch (whatIs(fun(d))) {
#if NPLUSK
                             case ADDPAT : return 1;
#endif
                             case EXT    : return 2;
                             default     : return 0;
                         }
#else
#if NPLUSK
        case AP        : return (whatIs(fun(d))==ADDPAT) ? 1 : 0;
#else
        case AP        : return 0;      /* must be an Int or Float lit     */
#endif
#endif
    }
    internal("discrArity");
    return 0;/*NOTREACHED*/
}

static Bool local eqNumDiscr(d1,d2)     /* Determine whether two numeric   */
Cell d1, d2; {                          /* descriptors have same value     */
#if NPLUSK
    if (whatIs(fun(d1))==ADDPAT)
        return whatIs(fun(d2))==ADDPAT && bignumEq(snd(fun(d1)),snd(fun(d2)));
#endif
#if OVERLOADED_CONSTANTS
    d1 = arg(d1);
    d2 = arg(d2);
#endif
    if (isInt(d1))
        return isInt(d2) && intEq(d1,d2);
    if (isFloat(d1))
        return isFloat(d2) && floatEq(d1,d2);
    if (isBignum(d1))
        return isBignum(d2) && bignumEq(d1,d2);
    internal("eqNumDiscr");
    return FALSE;/*NOTREACHED*/
}

#if TREX
static Bool local isExtDiscr(d)         /* Test of extension discriminator */
Cell d; {
    return isAp(d) && isExt(fun(d));
}

static Bool local eqExtDiscr(d1,d2)     /* Determine whether two extension */
Cell d1, d2; {                          /* discriminators have same label  */
    return fun(d1)==fun(d2);
}
#endif

/*-------------------------------------------------------------------------*/
