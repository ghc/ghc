/* -*- mode: hugs-c; -*- */
/* --------------------------------------------------------------------------
 * Desugarer
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: desugar.c,v $
 * $Revision: 1.2 $
 * $Date: 1998/12/02 13:22:05 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "link.h"

#include "desugar.h"
#include "pat.h"

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void local transPair             Args((Pair));
static Void local transTriple           Args((Triple));
static Void local transCase             Args((Cell));
static Cell local transRhs              Args((Cell));
static Cell local expandLetrec          Args((Cell));
static Cell local transComp             Args((Cell,List,Cell));
static Cell local transDo               Args((Cell,Cell,Cell,List));
static Cell local transConFlds          Args((Cell,List));
static Cell local transUpdFlds          Args((Cell,List,List));

/* --------------------------------------------------------------------------
 * Translation:    Convert input expressions into a less complex language
 *                 of terms using only LETREC, AP, constants and vars.
 *                 Also remove pattern definitions on lhs of eqns.
 * ------------------------------------------------------------------------*/

Cell translate(e)                       /* Translate expression:            */
Cell e; {
    switch (whatIs(e)) {
        case LETREC     : snd(snd(e)) = translate(snd(snd(e)));
                          return expandLetrec(e);

        case COND       : transTriple(snd(e));
                          return e;

        case AP         : fst(e) = translate(fst(e));

                          if (fst(e)==nameId || fst(e)==nameInd)
                              return translate(snd(e));
#if USE_NEWTYPE_FOR_DICTS
                          if (isName(fst(e)) &&
                              isMfun(fst(e)) &&
                              mfunOf(fst(e))==0)
                              return translate(snd(e));
#endif
                          snd(e) = translate(snd(e));
                          return e;

        case NAME       : if (e==nameOtherwise)
                              return nameTrue;
                          if (isCfun(e)) {
                              if (isName(name(e).defn))
                                  return name(e).defn;
                              if (isPair(name(e).defn))
                                  return snd(name(e).defn);
                          }
                          return e;

#if TREX
        case RECSEL     : return nameRecSel;

        case EXT        :
#endif
        case TUPLE      :
        case VAROPCELL  :
        case VARIDCELL  :
        case DICTVAR    :
        case INTCELL    :
        case BIGCELL    :
        case FLOATCELL  :
        case STRCELL    :
        case CHARCELL   : return e;

        case FINLIST    : mapOver(translate,snd(e));
                          return mkConsList(snd(e));

        case DOCOMP     : {   Cell m  = translate(fst(fst(snd(e))));
                              Cell m0 = snd(fst(snd(e)));
                              Cell r  = translate(fst(snd(snd(e))));
                              if (nonNull(m0))
                                  m0 = translate(m0);
                              return transDo(m,m0,r,snd(snd(snd(e))));
                          }

        case COMP       : return transComp(translate(fst(snd(e))),
                                           snd(snd(e)),
                                           nameNil);

        case CONFLDS    : return transConFlds(fst(snd(e)),snd(snd(e)));

        case UPDFLDS    : return transUpdFlds(fst3(snd(e)),
                                              snd3(snd(e)),
                                              thd3(snd(e)));

        case CASE       : {   Cell nv = inventVar();
                              mapProc(transCase,snd(snd(e)));
                              return ap(LETREC,
                                        pair(singleton(pair(nv,snd(snd(e)))),
                                             ap(nv,translate(fst(snd(e))))));
                          }

        case LAMBDA     : {   Cell nv = inventVar();
                              transAlt(snd(e));
                              return ap(LETREC,
                                        pair(singleton(pair(
                                                        nv,
                                                        singleton(snd(e)))),
                                             nv));
                          }

        default         : internal("translate");
    }
    return e;
}

static Void local transPair(pr)        /* Translate each component in a    */
Pair pr; {                             /* pair of expressions.             */
    fst(pr) = translate(fst(pr));
    snd(pr) = translate(snd(pr));
}

static Void local transTriple(tr)      /* Translate each component in a    */
Triple tr; {                           /* triple of expressions.           */
    fst3(tr) = translate(fst3(tr));
    snd3(tr) = translate(snd3(tr));
    thd3(tr) = translate(thd3(tr));
}

Void transAlt(e)                       /* Translate alt:                   */
Cell e; {                              /* ([Pat], Rhs) ==> ([Pat], Rhs')   */
    snd(e) = transRhs(snd(e));
}

static Void local transCase(c)         /* Translate case:                  */
Cell c; {                              /* (Pat, Rhs) ==> ([Pat], Rhs')     */
    fst(c) = singleton(fst(c));
    snd(c) = transRhs(snd(c));
}

List transBinds(bs)                    /* Translate list of bindings:      */
List bs; {                             /* eliminating pattern matching on  */
    List newBinds=NIL;                 /* lhs of bindings.                 */
    for (; nonNull(bs); bs=tl(bs)) {
        if (isVar(fst(hd(bs)))) {
            mapProc(transAlt,snd(hd(bs)));
            newBinds = cons(hd(bs),newBinds);
        }
        else
            newBinds = remPat(fst(snd(hd(bs))),
                              snd(snd(hd(bs)))=transRhs(snd(snd(hd(bs)))),
                              newBinds);
    }
    return newBinds;
}

static Cell local transRhs(rhs)        /* Translate rhs: removing line nos */
Cell rhs; {
    switch (whatIs(rhs)) {
        case LETREC  : snd(snd(rhs)) = transRhs(snd(snd(rhs)));
                       return expandLetrec(rhs);

        case GUARDED : mapOver(snd,snd(rhs));       /* discard line number */
                       mapProc(transPair,snd(rhs));
                       return rhs;

        default      : return translate(snd(rhs));  /* discard line number */
    }
}

Cell mkConsList(es)                    /* Construct expression for list es */
List es; {                             /* using nameNil and nameCons       */
    if (isNull(es))
        return nameNil;
    else
        return ap2(nameCons,hd(es),mkConsList(tl(es)));
}

static Cell local expandLetrec(root)   /* translate LETREC with list of    */
Cell root; {                           /* groups of bindings (from depend. */
    Cell e   = snd(snd(root));         /* analysis) to use nested LETRECs  */
    List bss = fst(snd(root));
    Cell temp;

    if (isNull(bss))                   /* should never happen, but just in */
        return e;                      /* case:  LETREC [] IN e  ==>  e    */

    mapOver(transBinds,bss);           /* translate each group of bindings */

    for (temp=root; nonNull(tl(bss)); bss=tl(bss)) {
        fst(snd(temp)) = hd(bss);
        snd(snd(temp)) = ap(LETREC,pair(NIL,e));
        temp           = snd(snd(temp));
    }
    fst(snd(temp)) = hd(bss);

    return root;
}

/* --------------------------------------------------------------------------
 * Translation of list comprehensions is based on the description in
 * `The Implementation of Functional Programming Languages':
 *
 * [ e | qs ] ++ l            => transComp e qs l
 * transComp e []           l => e : l
 * transComp e ((p<-xs):qs) l => LETREC _h []      = l
 *                                      _h (p:_xs) = transComp e qs (_h _xs)
 *                                      _h (_:_xs) = _h _xs --if p !failFree
 *                               IN _h xs
 * transComp e (b:qs)       l => if b then transComp e qs l else l
 * transComp e (decls:qs)   l => LETREC decls IN transComp e qs l
 * ------------------------------------------------------------------------*/

static Cell local transComp(e,qs,l)    /* Translate [e | qs] ++ l          */
Cell e;
List qs;
Cell l; {
    if (nonNull(qs)) {
        Cell q   = hd(qs);
        Cell qs1 = tl(qs);

        switch (fst(q)) {
            case FROMQUAL : {   Cell ld    = NIL;
                                Cell hVar  = inventVar();
                                Cell xsVar = inventVar();

                                if (!failFree(fst(snd(q))))
                                    ld = cons(pair(singleton(
                                                    ap2(nameCons,
                                                        WILDCARD,
                                                        xsVar)),
                                                   ap(hVar,xsVar)),
                                              ld);

                                ld = cons(pair(singleton(
                                                ap2(nameCons,
                                                    fst(snd(q)),
                                                    xsVar)),
                                               transComp(e,
                                                         qs1,
                                                         ap(hVar,xsVar))),
                                          ld);
                                ld = cons(pair(singleton(nameNil),
                                               l),
                                          ld);

                                return ap(LETREC,
                                          pair(singleton(pair(hVar,
                                                              ld)),
                                               ap(hVar,
                                                  translate(snd(snd(q))))));
                            }

            case QWHERE   : return
                                expandLetrec(ap(LETREC,
                                                pair(snd(q),
                                                     transComp(e,qs1,l))));

            case BOOLQUAL : return ap(COND,
                                      triple(translate(snd(q)),
                                             transComp(e,qs1,l),
                                             l));
        }
    }

    return ap2(nameCons,e,l);
}

/* --------------------------------------------------------------------------
 * Translation of monad comprehensions written using do-notation:
 *
 * do { e }               =>  e
 * do { p <- exp; qs }    =>  LETREC _h p = do { qs }
 *                                   _h _ = zero{m0}   -- if monad with 0
 *                            IN exp >>={m} _h
 * do { LET decls; qs }   =>  LETREC decls IN do { qs }
 * do { IF guard; qs }    =>  if guard then do { qs } else zero{m0}
 * do { e; qs }           =>  LETREC _h _ = [ e | qs ] in bind m exp _h
 *
 * where  m :: Monad f,  m0 :: Monad0 f
 * ------------------------------------------------------------------------*/

static Cell local transDo(m,m0,e,qs)    /* Translate do { qs ; e }         */
Cell m;
Cell m0;
Cell e;
List qs; {
    if (nonNull(qs)) {
        Cell q   = hd(qs);
        Cell qs1 = tl(qs);

        switch (fst(q)) {
            case FROMQUAL : {   Cell ld   = NIL;
                                Cell hVar = inventVar();

                                if (!failFree(fst(snd(q))) && nonNull(m0))
                                    ld = cons(pair(singleton(WILDCARD),
                                                   ap(nameZero,m0)),ld);

                                ld = cons(pair(singleton(fst(snd(q))),
                                               transDo(m,m0,e,qs1)),
                                          ld);

                                return ap(LETREC,
                                          pair(singleton(pair(hVar,ld)),
                                               ap3(nameBind,
                                                   m,
                                                   translate(snd(snd(q))),
                                                   hVar)));
                            }

            case DOQUAL :   {   Cell hVar = inventVar();
                                Cell ld   = cons(pair(singleton(WILDCARD),
                                                      transDo(m,m0,e,qs1)),
                                                 NIL);
                                return ap(LETREC,
                                          pair(singleton(pair(hVar,ld)),
                                               ap3(nameBind,
                                                   m,
                                                   translate(snd(q)),
                                                   hVar)));
                            }

            case QWHERE   : return
                                expandLetrec(ap(LETREC,
                                                pair(snd(q),
                                                     transDo(m,m0,e,qs1))));

            case BOOLQUAL : return ap(COND,
                                      triple(translate(snd(q)),
                                             transDo(m,m0,e,qs1),
                                             ap(nameZero,m0)));
        }
    }
    return e;
}

/* --------------------------------------------------------------------------
 * Translation of named field construction and update:
 *
 * Construction is implemented using the following transformation:
 *
 *   C{x1=e1, ..., xn=en} =  C v1 ... vm
 * where:
 *   vi = e1,        if the ith component of C is labelled with x1
 *       ...
 *      = en,        if the ith component of C is labelled with xn
 *      = undefined, otherwise
 *
 * Update is implemented using the following transformation:
 *
 *   e{x1=e1, ..., xn=en}
 *      =  let nv (C a1 ... am) v1 ... vn = C a1' .. am'
 *             nv (D b1 ... bk) v1 ... vn = D b1' .. bk
 *             ...
 *             nv _             v1 ... vn = error "failed update"
 *         in nv e e1 ... en
 * where:
 *   nv, v1, ..., vn, a1, ..., am, b1, ..., bk, ... are new variables,
 *   C,D,... = { K | K is a constr fun s.t. {x1,...,xn} subset of sels(K)}
 * and:
 *   ai' = v1,   if the ith component of C is labelled with x1
 *       ...
 *       = vn,   if the ith component of C is labelled with xn
 *       = ai,   otherwise
 *  etc...
 *
 * The error case may be omitted if C,D,... is an enumeration of all of the
 * constructors for the datatype concerned.  Strictly speaking, error case
 * isn't needed at all -- the only benefit of including it is that the user
 * will get a "failed update" message rather than a cryptic {v354 ...}.
 * So, for now, we'll go with the second option!
 *
 * For the time being, code for each update operation is generated
 * independently of any other updates.  However, if updates are used
 * frequently, then we might want to consider changing the implementation
 * at a later stage to cache definitions of functions like nv above.  This
 * would create a shared library of update functions, indexed by a set of
 * constructors {C,D,...}.
 * ------------------------------------------------------------------------*/

static Cell local transConFlds(c,flds)  /* Translate C{flds}               */
Name c;
List flds; {
    Cell e = c;
    Int  m = name(c).arity;
    Int  i;
    for (i=m; i>0; i--)
        e = ap(e,nameUndefined);
    for (; nonNull(flds); flds=tl(flds)) {
        Cell a = e;
        for (i=m-sfunPos(fst(hd(flds)),c); i>0; i--)
            a = fun(a);
        arg(a) = translate(snd(hd(flds)));
    }
    return e;
}

static Cell local transUpdFlds(e,cs,flds)/* Translate e{flds}              */
Cell e;                                 /* (cs is corresp list of constrs) */
List cs;
List flds; {
    Cell nv   = inventVar();
    Cell body = ap(nv,translate(e));
    List fs   = flds;
    List args = NIL;
    List alts = NIL;

    for (; nonNull(fs); fs=tl(fs)) {    /* body = nv e1 ... en             */
        Cell b = hd(fs);                /* args = [v1, ..., vn]            */
        body   = ap(body,translate(snd(b)));
        args   = cons(inventVar(),args);
    }

    for (; nonNull(cs); cs=tl(cs)) {    /* Loop through constructors to    */
        Cell c   = hd(cs);              /* build up list of alts.          */
        Cell pat = c;
        Cell rhs = c;
        List as  = args;
        Int  m   = name(c).arity;
        Int  i;

        for (i=m; i>0; i--) {           /* pat  = C a1 ... am              */
            Cell a = inventVar();       /* rhs  = C a1 ... am              */
            pat    = ap(pat,a);
            rhs    = ap(rhs,a);
        }

        for (fs=flds; nonNull(fs); fs=tl(fs), as=tl(as)) {
            Name s = fst(hd(fs));       /* Replace approp ai in rhs with   */
            Cell r = rhs;               /* vars from [v1,...,vn]           */
            for (i=m-sfunPos(s,c); i>0; i--)
                r = fun(r);
            arg(r) = hd(as);
        }

        alts     = cons(pair(cons(pat,args),rhs),alts);
    }
    return ap(LETREC,pair(singleton(pair(nv,alts)),body));
}

/* --------------------------------------------------------------------------
 * Desugar control:
 * ------------------------------------------------------------------------*/

Void desugarControl(what)
Int what; {
    patControl(what);
    switch (what) {
        case INSTALL :
                /* Fall through */
        case RESET   : break;
        case MARK    : break;
    }
}

/*-------------------------------------------------------------------------*/
