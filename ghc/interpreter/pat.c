/* -*- mode: hugs-c; -*- */
/* --------------------------------------------------------------------------
 * Desugarer
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: pat.c,v $
 * $Revision: 1.2 $
 * $Date: 1998/12/02 13:22:28 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "link.h"

#include "pat.h"
#include "desugar.h"

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Cell local refutePat             Args((Cell));
static Cell local refutePatAp           Args((Cell));
static Cell local matchPat              Args((Cell));
static List local remPat1               Args((Cell,Cell,List));

/* --------------------------------------------------------------------------
 * Elimination of pattern bindings:
 *
 * The following code adopts the definition of failure free patterns as given
 * in the Haskell 1.3 report; the term "irrefutable" is also used there for
 * a subset of the failure free patterns described here, but has no useful
 * role in this implementation.  Basically speaking, the failure free patterns
 * are:         variable, wildcard, ~apat
 *              var@apat,               if apat is failure free
 *              C apat1 ... apatn       if C is a product constructor
 *                                      (i.e. an only constructor) and
 *                                      apat1,...,apatn are failure free
 * Note that the last case automatically covers the case where C comes from
 * a newtype construction.
 * ------------------------------------------------------------------------*/

Bool failFree(pat)                /* is pattern failure free? (do we need  */
Cell pat; {                       /* a conformality check?)                */
    Cell c = getHead(pat);

    switch (whatIs(c)) {
        case ASPAT     : return failFree(snd(snd(pat)));

        case NAME      : if (!isCfun(c) || cfunOf(c)!=0)
                             return FALSE;
                         /*intentional fall-thru*/
        case TUPLE     : for (; isAp(pat); pat=fun(pat))
                             if (!failFree(arg(pat)))
                                return FALSE;
                         /*intentional fall-thru*/
        case LAZYPAT   :
        case VAROPCELL :
        case VARIDCELL :
        case DICTVAR   :
        case WILDCARD  : return TRUE;

#if TREX
        case EXT       : return failFree(extField(pat)) &&
                                failFree(extRow(pat));
#endif

        case CONFLDS   : if (cfunOf(fst(snd(c)))==0) {
                             List fs = snd(snd(c));
                             for (; nonNull(fs); fs=tl(fs))
                                 if (!failFree(snd(hd(fs))))
                                     return FALSE;
                             return TRUE;
                         }
                         /*intentional fall-thru*/
        default        : return FALSE;
    }
}

static Cell local refutePat(pat)  /* find pattern to refute in conformality*/
Cell pat; {                       /* test with pat.                        */
                                  /* e.g. refPat  (x:y) == (_:_)           */
                                  /*      refPat ~(x:y) == _      etc..    */

    switch (whatIs(pat)) {
        case ASPAT     : return refutePat(snd(snd(pat)));

        case FINLIST   : {   Cell ys = snd(pat);
                             Cell xs = NIL;
                             for (; nonNull(ys); ys=tl(ys)) {
                                 xs = ap2(nameCons,refutePat(hd(ys)),xs);
                             }
                             return revOnto(xs,nameNil);
                         }

        case CONFLDS   : {   Cell ps = NIL;
                             Cell fs = snd(snd(pat));
                             for (; nonNull(fs); fs=tl(fs)) {
                                 Cell p = refutePat(snd(hd(fs)));
                                 ps     = cons(pair(fst(hd(fs)),p),ps);
                             }
                             return pair(CONFLDS,pair(fst(snd(pat)),rev(ps)));
                         }

        case VAROPCELL :
        case VARIDCELL :
        case DICTVAR   :
        case WILDCARD  :
        case LAZYPAT   : return WILDCARD;

        case STRCELL   :
        case CHARCELL  :
#if NPLUSK
        case ADDPAT    :
#endif
        case TUPLE     :
        case NAME      : return pat;

        case AP        : return refutePatAp(pat);

        default        : internal("refutePat");
                         return NIL; /*NOTREACHED*/
    }
}

static Cell local refutePatAp(p)  /* find pattern to refute in conformality*/
Cell p; {
    Cell h = getHead(p);
    if (h==nameFromInt || h==nameFromInteger || h==nameFromDouble)
        return p;
#if NPLUSK
    else if (whatIs(h)==ADDPAT)
        return ap(fun(p),refutePat(arg(p)));
#endif
#if TREX
    else if (isExt(h)) {
        Cell pf = refutePat(extField(p));
        Cell pr = refutePat(extRow(p));
        return ap2(fun(fun(p)),pf,pr);
    }
#endif
    else {
        List as = getArgs(p);
        mapOver(refutePat,as);
        return applyToArgs(h,as);
    }
}

static Cell local matchPat(pat) /* find pattern to match against           */
Cell pat; {                     /* replaces parts of pattern that do not   */
                                /* include variables with wildcards        */
    switch (whatIs(pat)) {
        case ASPAT     : {   Cell p = matchPat(snd(snd(pat)));
                             return (p==WILDCARD) ? fst(snd(pat))
                                                  : ap(ASPAT,
                                                       pair(fst(snd(pat)),p));
                         }

        case FINLIST   : {   Cell ys = snd(pat);
                             Cell xs = NIL;
                             for (; nonNull(ys); ys=tl(ys))
                                 xs = cons(matchPat(hd(ys)),xs);
                             while (nonNull(xs) && hd(xs)==WILDCARD)
                                 xs = tl(xs);
                             for (ys=nameNil; nonNull(xs); xs=tl(xs))
                                 ys = ap2(nameCons,hd(xs),ys);
                             return ys;
                         }

        case CONFLDS   : {   Cell ps   = NIL;
                             Name c    = fst(snd(pat));
                             Cell fs   = snd(snd(pat));
                             Bool avar = FALSE;
                             for (; nonNull(fs); fs=tl(fs)) {
                                 Cell p = matchPat(snd(hd(fs)));
                                 ps     = cons(pair(fst(hd(fs)),p),ps);
                                 if (p!=WILDCARD)
                                     avar = TRUE;
                             }
                             return avar ? pair(CONFLDS,pair(c,rev(ps)))
                                         : WILDCARD;
                         }

        case VAROPCELL :
        case VARIDCELL :
        case DICTVAR   : return pat;

        case LAZYPAT   : {   Cell p = matchPat(snd(pat));
                             return (p==WILDCARD) ? WILDCARD : ap(LAZYPAT,p);
                         }

        case WILDCARD  :
        case STRCELL   :
        case CHARCELL  : return WILDCARD;

        case TUPLE     :
        case NAME      :
        case AP        : {   Cell h = getHead(pat);
                             if (h==nameFromInt     ||
                                 h==nameFromInteger || h==nameFromDouble)
                                 return WILDCARD;
#if NPLUSK
                             else if (whatIs(h)==ADDPAT)
                                 return pat;
#endif
#if TREX
                             else if (isExt(h)) {
                                 Cell pf = matchPat(extField(pat));
                                 Cell pr = matchPat(extRow(pat));
                                 return (pf==WILDCARD && pr==WILDCARD)
                                          ? WILDCARD
                                          : ap2(fun(fun(pat)),pf,pr);
                             }
#endif
                             else {
                                 List args = NIL;
                                 Bool avar = FALSE;
                                 for (; isAp(pat); pat=fun(pat)) {
                                     Cell p = matchPat(arg(pat));
                                     if (p!=WILDCARD)
                                         avar = TRUE;
                                     args = cons(p,args);
                                 }
                                 return avar ? applyToArgs(pat,args)
                                             : WILDCARD;
                             }
                         }

        default        : internal("matchPat");
                         return NIL; /*NOTREACHED*/
    }
}

#define addEqn(v,val,lds)  cons(pair(v,singleton(pair(NIL,val))),lds)

List remPat(pat,expr,lds)
Cell pat;                         /* Produce list of definitions for eqn   */
Cell expr;                        /* pat = expr, including a conformality  */
List lds; {                       /* check if required.                    */

    /* Conformality test (if required):
     *   pat = expr  ==>    nv = LETREC confCheck nv@pat = nv
     *                           IN confCheck expr
     *                      remPat1(pat,nv,.....);
     */

    if (!failFree(pat)) {
        Cell confVar = inventVar();
        Cell nv      = inventVar();
        Cell locfun  = pair(confVar,         /* confVar [([nv@refPat],nv)] */
                            singleton(pair(singleton(ap(ASPAT,
                                                        pair(nv,
                                                             refutePat(pat)))),
                                           nv)));

        if (whatIs(expr)==GUARDED) {         /* A spanner ... special case */
            lds  = addEqn(nv,expr,lds);      /* for guarded pattern binding*/
            expr = nv;
            nv   = inventVar();
        }

        if (whatIs(pat)==ASPAT) {            /* avoid using new variable if*/
            nv   = fst(snd(pat));            /* a variable is already given*/
            pat  = snd(snd(pat));            /* by an as-pattern           */
        }

        lds = addEqn(nv,                                /* nv =            */
                     ap(LETREC,pair(singleton(locfun),  /* LETREC [locfun] */
                                    ap(confVar,expr))), /* IN confVar expr */
                     lds);

        return remPat1(matchPat(pat),nv,lds);
    }

    return remPat1(matchPat(pat),expr,lds);
}

static List local remPat1(pat,expr,lds)
Cell pat;                         /* Add definitions for: pat = expr to    */
Cell expr;                        /* list of local definitions in lds.     */
List lds; {
    Cell c;

    switch (whatIs(c=getHead(pat))) {
        case WILDCARD  :
        case STRCELL   :
        case CHARCELL  : break;

        case ASPAT     : return remPat1(snd(snd(pat)),     /* v@pat = expr */
                                        fst(snd(pat)),
                                        addEqn(fst(snd(pat)),expr,lds));

        case LAZYPAT   : {   Cell nv;

                             if (isVar(expr) || isName(expr))
                                 nv  = expr;
                             else {
                                 nv  = inventVar();
                                 lds = addEqn(nv,expr,lds);
                             }

                             return remPat(snd(pat),nv,lds);
                         }

#if NPLUSK
        case ADDPAT    : return remPat1(arg(pat),       /* n + k = expr */
                                        ap3(namePmSub, arg(fun(pat)), snd(c),
                                            expr),
                                        lds);
#endif

        case FINLIST   : return remPat1(mkConsList(snd(pat)),expr,lds);

        case CONFLDS   : {   Name h  = fst(snd(pat));
                             Int  m  = name(h).arity;
                             Cell p  = h;
                             List fs = snd(snd(pat));
                             Int  i  = m;
                             while (0<i--)
                                 p = ap(p,WILDCARD);
                             for (; nonNull(fs); fs=tl(fs)) {
                                 Cell r = p;
                                 for (i=m-sfunPos(fst(hd(fs)),h); i>0; i--)
                                     r = fun(r);
                                 arg(r) = snd(hd(fs));
                             }
                             return remPat1(p,expr,lds);
                         }

        case DICTVAR   : /* shouldn't really occur */
                         assert(0); /* so let's test for it then! ADR */
        case VARIDCELL :
        case VAROPCELL : return addEqn(pat,expr,lds);

        case NAME      : if (c==nameFromInt || c==nameFromInteger
                                            || c==nameFromDouble) {
                             if (argCount==2)
                                 arg(fun(pat)) = translate(arg(fun(pat)));
                             break;
                         }

                         if (argCount==1 && isCfun(c)       /* for newtype */
                             && cfunOf(c)==0 && name(c).defn==nameId)
                             return remPat1(arg(pat),expr,lds);

                         /* intentional fall-thru */
        case TUPLE     : {   List ps = getArgs(pat);

                             if (nonNull(ps)) {
                                 Cell nv, sel;
                                 Int  i;

                                 if (isVar(expr) || isName(expr))
                                     nv  = expr;
                                 else {
                                     nv  = inventVar();
                                     lds = addEqn(nv,expr,lds);
                                 }

                                 sel = ap2(nameSel,c,nv);
                                 for (i=1; nonNull(ps); ++i, ps=tl(ps))
                                      lds = remPat1(hd(ps),
                                                    ap(sel,mkInt(i)),
                                                    lds);
                             }
                         }
                         break;

#if TREX
        case EXT       : {   Cell nv = inventVar();
                             arg(fun(fun(pat)))
                                 = translate(arg(fun(fun(pat))));
                             lds = addEqn(nv,
                                          ap2(nameRecBrk,
                                              arg(fun(fun(pat))),
                                              expr),
                                          lds);
                             lds = remPat1(extField(pat),ap(nameFst,nv),lds);
                             lds = remPat1(extRow(pat),ap(nameSnd,nv),lds);
                         }
                         break;
#endif

        default        : internal("remPat1");
                         break;
    }
    return lds;
}

/* --------------------------------------------------------------------------
 * Pattern control:
 * ------------------------------------------------------------------------*/

Void patControl( Int what )
{
    switch (what) {
        case INSTALL :
                /* Fall through */
        case RESET   : break;
        case MARK    : break;
    }
}

/*-------------------------------------------------------------------------*/
