/* -*- mode: hugs-c; -*- */
/* --------------------------------------------------------------------------
 * Translator: generates stg code from output of pattern matching
 * compiler.
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: translate.c,v $
 * $Revision: 1.2 $
 * $Date: 1998/12/02 13:22:47 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "stg.h"
#include "compiler.h"
#include "pmc.h"  /* for discrArity                 */
#include "hugs.h" /* for debugCode                  */
#include "type.h" /* for conToTagType, tagToConType */
#include "link.h"
#include "pp.h"
#include "dynamic.h"
#include "Assembler.h"
#include "translate.h"

/* ---------------------------------------------------------------- */

static StgVar  local stgOffset       Args((Offset,List));
static StgVar  local stgText         Args((Text,List));
static StgRhs  local stgRhs          Args((Cell,Int,List));
static StgCaseAlt local stgCaseAlt   Args((Cell,Int,List,StgExpr));
static StgExpr local stgExpr         Args((Cell,Int,List,StgExpr));

/* ---------------------------------------------------------------- */

/* Association list storing globals assigned to dictionaries, tuples, etc */
List stgGlobals = NIL;

static StgVar local getSTGTupleVar  Args((Cell));

static StgVar local getSTGTupleVar( Cell d )
{
    Pair p = cellAssoc(d,stgGlobals);
    /* Yoiks - only the Prelude sees Tuple decls! */
    if (isNull(p)) {
        implementTuple(tupleOf(d));
        p = cellAssoc(d,stgGlobals);
    }
    assert(nonNull(p));
    return snd(p);
}

/* ---------------------------------------------------------------- */

static Cell local stgOffset(Offset o, List sc)
{
    Cell r = cellAssoc(o,sc);
    assert(nonNull(r));
    return snd(r);
}

static Cell local stgText(Text t,List sc)
{
    List xs = sc;
    for (; nonNull(xs); xs=tl(xs)) {
        Cell x = hd(xs);
        Cell v = fst(x);
        if (!isOffset(v) && t == textOf(v)) {
            return snd(x);
        }
    }
    internal("stgText");
}

/* ---------------------------------------------------------------- */

static StgRhs local stgRhs(e,co,sc)
Cell e; 
Int  co; 
List sc; {
    switch (whatIs(e)) {

    /* Identifiers */
    case OFFSET:
            return stgOffset(e,sc);
    case VARIDCELL:
    case VAROPCELL:
            return stgText(textOf(e),sc);
    case TUPLE: 
            return getSTGTupleVar(e);
    case NAME:
            return e;
    /* Literals */
    case CHARCELL:
            return mkStgCon(nameMkC,singleton(e));
    case INTCELL:
            return mkStgCon(nameMkI,singleton(e));
    case BIGCELL:
            return mkStgCon(nameMkBignum,singleton(e));
    case FLOATCELL:
            return mkStgCon(nameMkD,singleton(e));
    case STRCELL:
#if USE_ADDR_FOR_STRINGS
        {
            StgVar v = mkStgVar(mkStgCon(nameMkA,singleton(e)),NIL);
            return mkStgLet(singleton(v),
                            makeStgApp(nameUnpackString,singleton(v)));
        }                            
#else
            return mkStgApp(nameUnpackString,singleton(e));
#endif
    case AP:
            return stgExpr(e,co,sc,namePMFail);
    case NIL:
            internal("stgRhs2");
    default:
            return stgExpr(e,co,sc,namePMFail);
    }
}

static StgCaseAlt local stgCaseAlt(alt,co,sc,failExpr)
Cell alt;
Int co;
List sc;
StgExpr failExpr;
{
    StgDiscr d     = fst(alt);
    Int      da    = discrArity(d);
    Cell     vs    = NIL;
    Int  i;
    for(i=1; i<=da; ++i) {
        StgVar nv = mkStgVar(NIL,NIL);
        vs    = cons(nv,vs);
        sc    = cons(pair(mkOffset(co+i),nv),sc);
    }
    return mkStgCaseAlt(d,vs,stgExpr(snd(alt),co+da,sc,failExpr));
}

static StgExpr local stgExpr(e,co,sc,failExpr)
Cell e; 
Int  co; 
List sc; 
StgExpr failExpr; 
{
    switch (whatIs(e)) {
    case COND:
        {
            return makeStgIf(stgExpr(fst3(snd(e)),co,sc,namePMFail),
                             stgExpr(snd3(snd(e)),co,sc,failExpr),
                             stgExpr(thd3(snd(e)),co,sc,failExpr));
        }
    case GUARDED:
        {   
            List guards = reverse(snd(e));
            e = failExpr;
            for(; nonNull(guards); guards=tl(guards)) {
                Cell g   = hd(guards);
                Cell c   = stgExpr(fst(g),co,sc,namePMFail);
                Cell rhs = stgExpr(snd(g),co,sc,failExpr);
                e = makeStgIf(c,rhs,e);
            }
            return e;
        }
    case FATBAR:
        {
            StgExpr e2 = stgExpr(snd(snd(e)),co,sc,failExpr);
            StgVar alt = mkStgVar(e2,NIL);
            return mkStgLet(singleton(alt),stgExpr(fst(snd(e)),co,sc,alt));
        }
    case CASE:
        {   
            List alts  = snd(snd(e));
            Cell scrut = stgExpr(fst(snd(e)),co,sc,namePMFail);
            if (isNull(alts)) {
                return failExpr;
            } else if (isChar(fst(hd(alts)))) {
                Cell     alt  = hd(alts);
                StgDiscr d    = fst(alt);
                StgVar   c    = mkStgVar(mkStgCon(nameMkC,singleton(d)),NIL);
                StgExpr  test = nameEqChar;
                /* duplicates scrut but it should be atomic */
                return makeStgIf(makeStgLet(singleton(c),makeStgApp(test,doubleton(scrut,c))),
                                 stgExpr(snd(alt),co,sc,failExpr),
                                 stgExpr(ap(CASE,pair(fst(snd(e)),tl(alts))),co,sc,failExpr));
            } else {
                List as    = NIL;
                for(; nonNull(alts); alts=tl(alts)) {
                    as = cons(stgCaseAlt(hd(alts),co,sc,failExpr),as);
                }
                return mkStgCase(scrut, revOnto(as, singleton(mkStgDefault(mkStgVar(NIL,NIL),failExpr))));
            }
        }
    case NUMCASE:
#if OVERLOADED_CONSTANTS                
        {
            Triple nc    = snd(e);
            Offset o     = fst3(nc);
            Cell   discr = snd3(nc);
            Cell   r     = thd3(nc);
            Cell   scrut = stgOffset(o,sc);
            Cell   h     = getHead(discr);
            Int    da    = discrArity(discr);

#if NPLUSK
            if (whatIs(h) == ADDPAT && argCount == 1) {
                /*   ADDPAT num dictIntegral
                 * ==>
                 *   let n = fromInteger num in 
                 *   if pmLe dictIntegral n scrut
                 *   then let v = pmSubtract dictIntegral scrut v
                 *   else fail
                 */
                Cell   n            = snd(h);
                Cell   dictIntegral = arg(discr);  /* Integral dictionary */
                StgVar v            = NIL;
                List   binds        = NIL;
                StgVar dIntegral    = NIL;

                /* bind dictionary */
                dIntegral = stgRhs(dictIntegral,co,sc);
                if (!isAtomic(dIntegral)) { /* wasn't atomic */
                    dIntegral = mkStgVar(dIntegral,NIL);
                    binds = cons(dIntegral,binds);
                }
                /* box number */
                n = mkStgVar(mkStgCon(nameMkBignum,singleton(n)),NIL);
                binds = cons(n,binds);

                /* coerce number to right type (using Integral dict) */
                n = mkStgVar(mkStgApp(namePmFromInteger,doubleton(dIntegral,n)),NIL);
                binds = cons(n,binds);

                ++co;
                v = mkStgVar(mkStgApp(namePmSubtract,tripleton(dIntegral,scrut,n)),NIL);
                return mkStgLet(binds,
                                makeStgIf(mkStgApp(namePmLe,tripleton(dIntegral,n,scrut)),
                                          mkStgLet(singleton(v),
                                                   stgExpr(r,
                                                           co,
                                                           cons(pair(mkOffset(co),v),sc),
                                                           failExpr)),
                                          failExpr));
            }
#endif /* NPLUSK */

            assert(isName(h) && argCount == 2);
            {
                /* This code is rather ugly.
                 * We ought to desugar it using one of the following:
                 *   if (==) dEq (fromInt     dNum        pat) scrut
                 *   if (==) dEq (fromInteger dNum        pat) scrut
                 *   if (==) dEq (fromFloat   dFractional pat) scrut
                 * But it would be very hard to obtain the Eq dictionary
                 * from the Num or Fractional dictionary we have.
                 * Instead, we rely on the Prelude to supply 3 helper
                 * functions which do the test for us.
                 *   primPmInt     :: Num a => Int -> a -> Bool
                 *   primPmInteger :: Num a => Integer -> a -> Bool
                 *   primPmDouble  :: Fractional a => Double -> a -> Bool
                 */
                Cell   n      = arg(discr);
                Cell   dict   = arg(fun(discr));
                StgExpr d     = NIL;
                List    binds = NIL;
                StgExpr m     = NIL;
                Name   box
                    = h == nameFromInt     ? nameMkI
                    : h == nameFromInteger ? nameMkBignum
                    :                        nameMkD;
                Name   testFun
                    = h == nameFromInt     ? namePmInt
                    : h == nameFromInteger ? namePmInteger 
                    :                        namePmDouble;
                Cell   altsc  = sc;
                Cell   vs     = NIL;
                Int    i;

                for(i=1; i<=da; ++i) {
                    Cell nv = mkStgVar(NIL,NIL);
                    vs    = cons(nv,vs);
                    altsc = cons(pair(mkOffset(co+i),nv),altsc);
                }
                /* bind dictionary */
                d = stgRhs(dict,co,sc);
                if (!isAtomic(d)) { /* wasn't atomic */
                    d = mkStgVar(d,NIL);
                    binds = cons(d,binds);
                }
                /* bind number */
                n = mkStgVar(mkStgCon(box,singleton(n)),NIL);
                binds = cons(n,binds);

                return makeStgIf(mkStgLet(binds,
                                          mkStgApp(testFun,tripleton(d,n,scrut))),
                                 stgExpr(r,co+da,altsc,failExpr),
                                 failExpr);
            }
        }
#else /* ! OVERLOADED_CONSTANTS */
        {
            Triple nc    = snd(e);
            Offset o     = fst3(nc);
            Cell   discr = snd3(nc);
            Cell   r     = thd3(nc);
            Cell   scrut = stgOffset(o,sc);
            Cell   h     = getHead(discr);
            Int    da    = discrArity(discr);
            Cell   n     = discr;
            List   binds = NIL;
            Name   eq
                = isInt(discr)    ? nameEqInt
                : isBignum(discr) ? nameEqInteger
                :                   nameEqDouble;
            Name   box
                = isInt(discr)    ? nameMkI
                : isBignum(discr) ? nameMkBignum
                :                   nameMkD;
            StgExpr test = NIL;
            Cell   altsc = sc;
            Cell   vs    = NIL;
            Int    i;

            for(i=1; i<=da; ++i) {
                Cell nv = mkStgVar(NIL,NIL);
                vs    = cons(nv,vs);
                altsc = cons(pair(mkOffset(co+i),nv),altsc);
            }

            /* bind number */
            n = mkStgVar(mkStgCon(box,singleton(n)),NIL);
            binds = cons(n,binds);
            
            test = mkStgLet(binds, mkStgApp(eq, doubleton(n,scrut)));
            return makeStgIf(test,
                             stgExpr(r,co+da,altsc,failExpr),
                             failExpr);
        }
#endif /* ! OVERLOADED_CONSTANTS */
    case LETREC:
        {
            List binds = NIL;
            List vs = NIL;
            List bs;
            /* allocate variables, extend scope */
            for(bs = snd(fst(snd(e))); nonNull(bs); bs=tl(bs)) {
                Cell nv  = mkStgVar(NIL,NIL);
                sc = cons(pair(fst3(hd(bs)),nv),sc);
                binds = cons(nv,binds);
                vs = cons(nv,vs);
            }
            for(bs = fst(fst(snd(e))); nonNull(bs); bs=tl(bs)) {
                Cell nv  = mkStgVar(NIL,NIL);
                sc = cons(pair(mkOffset(++co),nv),sc);
                binds = cons(nv,binds);
                vs = cons(nv,vs);
            }
            vs = rev(vs);
            /* transform functions */
            for(bs = snd(fst(snd(e))); nonNull(bs); bs=tl(bs), vs=tl(vs)) {
                Cell fun = hd(bs);
                Cell nv  = hd(vs);
                List as = NIL;
                List funsc = sc;
                Int  arity = intOf(snd3(fun));
                Int  i;
                for(i=1; i<=arity; ++i) {
                    Cell v = mkStgVar(NIL,NIL);
                    as = cons(v,as);
                    funsc = cons(pair(mkOffset(co+i),v),funsc);
                }
                stgVarBody(nv) = mkStgLambda(as,stgExpr(thd3(thd3(fun)),co+arity,funsc,namePMFail));
            }
            /* transform expressions */
            for(bs = fst(fst(snd(e))); nonNull(bs); bs=tl(bs), vs=tl(vs)) {
                Cell rhs = hd(bs);
                Cell nv  = hd(vs);
                stgVarBody(nv) = stgRhs(rhs,co,sc);
            }
            return mkStgLet(binds,stgRhs(snd(snd(e)),co,sc));
        }
    default: /* convert to an StgApp or StgVar plus some bindings */
        {   
            List args  = NIL;
            List binds = NIL;
            List as    = NIL;

            /* Unwind args */
            while (isAp(e)) {
                Cell arg = arg(e);
                e        = fun(e);
                args = cons(arg,args);
            }

            /* Special cases */
            if (e == nameSel && length(args) == 3) {
                Cell   con   = hd(args);
#if 0
                StgVar v     = stgOffset(hd(tl(args)),sc);
#else
                StgExpr v    = stgExpr(hd(tl(args)),co,sc,namePMFail);
#endif
                Int    ix    = intOf(hd(tl(tl(args))));
                Int    da    = discrArity(con);
                List   vs    = NIL;
                Int    i;
                for(i=1; i<=da; ++i) {
                    Cell nv = mkStgVar(NIL,NIL);
                    vs=cons(nv,vs);
                }
                return mkStgCase(v,
                                 doubleton(mkStgCaseAlt(con,vs,nth(ix-1,vs)),
                                 mkStgDefault(mkStgVar(NIL,NIL),namePMFail)));
            }
            
            /* Arguments must be StgAtoms */
            for(as=args; nonNull(as); as=tl(as)) {
                StgRhs a = stgRhs(hd(as),co,sc);
#if 1 /* optional flattening of let bindings */
                if (whatIs(a) == LETREC) {
                    binds = appendOnto(stgLetBinds(a),binds);
                    a = stgLetBody(a);
                }
#endif
                    
                if (!isAtomic(a)) {
                    a     = mkStgVar(a,NIL);
                    binds = cons(a,binds);
                }
                hd(as) = a;
            }

            /* Function must be StgVar or Name */
            e = stgRhs(e,co,sc);
            if (!isStgVar(e) && !isName(e)) {
                e = mkStgVar(e,NIL);
                binds = cons(e,binds);
            }

            return makeStgLet(binds,makeStgApp(e,args));
        }
    }
}

static Void ppExp( Name n, Int arity, Cell e );
static Void ppExp( Name n, Int arity, Cell e )
{
#if DEBUG_CODE
    if (debugCode) {
        Int i;
        printf("BEFORE: %s", textToStr(name(n).text));
        for (i = arity; i > 0; i--) {
            printf(" o%d", i);
        }
        printf(" = ");
        printExp(stdout,e); 
        printf("\n");
    }
#endif
}

Void stgDefn( Name n, Int arity, Cell e )
{
    List vs = NIL;
    List sc = NIL;
    Int i;
    ppExp(n,arity,e);
    for (i = 1; i <= arity; ++i) {
        Cell nv = mkStgVar(NIL,NIL);
        vs = cons(nv,vs);
        sc = cons(pair(mkOffset(i),nv),sc);
    }
    stgVarBody(name(n).stgVar) = makeStgLambda(vs,stgExpr(e,arity,sc,namePMFail));
    ppStg(name(n).stgVar);
}

static StgExpr forceArgs( List is, List args, StgExpr e );

/* force the args numbered in is */
static StgExpr forceArgs( List is, List args, StgExpr e )
{
    for(; nonNull(is); is=tl(is)) {
        e = mkSeq(nth(intOf(hd(is))-1,args),e);
    }
    return e;
}

/* \ v -> case v of { ...; Ci _ _ -> i; ... } */
Void implementConToTag(t)
Tycon t; {                    
    if (isNull(tycon(t).conToTag)) {
        List   cs  = tycon(t).defn;
        Name   nm  = newName(inventText());
        StgVar v   = mkStgVar(NIL,NIL);
        List alts  = NIL; /* can't fail */

        assert(isTycon(t) && (tycon(t).what==DATATYPE || tycon(t).what==NEWTYPE));
        for (; hasCfun(cs); cs=tl(cs)) {
            Name    c   = hd(cs);
            Int     num = cfunOf(c) == 0 ? 0 : cfunOf(c)-1;
            StgVar  r   = mkStgVar(mkStgCon(nameMkI,singleton(mkInt(num))),NIL);
            StgExpr tag = mkStgLet(singleton(r),r);
            List    vs  = NIL;
            Int i;
            for(i=0; i < name(c).arity; ++i) {
                vs = cons(mkStgVar(NIL,NIL),vs);
            }
            alts = cons(mkStgCaseAlt(c,vs,tag),alts);
        }

        name(nm).line   = tycon(t).line;
        name(nm).type   = conToTagType(t);
        name(nm).arity  = 1;
        name(nm).stgVar = mkStgVar(mkStgLambda(singleton(v),mkStgCase(v,alts)),NIL);
        tycon(t).conToTag = nm;
        /* hack to make it print out */
        stgGlobals = cons(pair(nm,name(nm).stgVar),stgGlobals); 
    }
}

/* \ v -> case v of { ...; i -> Ci; ... } */
Void implementTagToCon(t)
Tycon t; {                    
    if (isNull(tycon(t).tagToCon)) {
        List   cs  = tycon(t).defn;
        Name   nm  = newName(inventText());
        StgVar v1  = mkStgVar(NIL,NIL);
        StgVar v2  = mkStgPrimVar(NIL,mkStgRep(INT_REP),NIL);
        List alts  = singleton(mkStgPrimAlt(singleton(mkStgPrimVar(NIL,mkStgRep(INT_REP),NIL)),namePMFail));

        assert(namePMFail);
        assert(isTycon(t) && (tycon(t).what==DATATYPE || tycon(t).what==NEWTYPE));
        for (; hasCfun(cs); cs=tl(cs)) {
            Name   c   = hd(cs);
            Int    num = cfunOf(c) == 0 ? 0 : cfunOf(c)-1;
            StgVar pat = mkStgPrimVar(mkInt(num),mkStgRep(INT_REP),NIL);
            assert(name(c).arity==0);
            alts = cons(mkStgPrimAlt(singleton(pat),c),alts);
        }

        name(nm).line   = tycon(t).line;
        name(nm).type   = tagToConType(t);
        name(nm).arity  = 1;
        name(nm).stgVar = mkStgVar(mkStgLambda(singleton(v1),
                                               mkStgCase(v1,singleton(mkStgCaseAlt(nameMkI,singleton(v2),
                                                                                   mkStgPrimCase(v2,alts))))),NIL);
        tycon(t).tagToCon = nm;
        /* hack to make it print out */
        stgGlobals = cons(pair(nm,name(nm).stgVar),stgGlobals); 
    }
}

Void implementCfun(c,scs)               /* Build implementation for constr */
Name c;                                 /* fun c.  scs lists integers (1..)*/
List scs; {                             /* in incr order of strict comps.  */
    Int a = name(c).arity;
    if (name(c).arity > 0) {
        List    args = makeArgs(a);
        StgVar  tv   = mkStgVar(mkStgCon(c,args),NIL);
        StgExpr e1   = mkStgLet(singleton(tv),tv);
        StgExpr e2   = forceArgs(scs,args,e1);
        StgVar  v    = mkStgVar(mkStgLambda(args,e2),NIL);
        name(c).stgVar = v;
    } else {
        StgVar v = mkStgVar(mkStgCon(c,NIL),NIL);
        name(c).stgVar = v;
    }
    /* hack to make it print out */
    stgGlobals = cons(pair(c,name(c).stgVar),stgGlobals); 
}

/* --------------------------------------------------------------------------
 * Foreign function calls and primops
 * ------------------------------------------------------------------------*/

static String  charListToString( List cs );
static Cell    foreignResultTy( Type t );
static Cell    foreignArgTy( Type t );
static Name    repToBox        Args(( char c ));
static StgRhs  makeStgPrim     Args(( Name,Bool,List,String,String ));

static String charListToString( List cs )
{
    static char s[100];

    Int i = 0;
    assert( length(cs) < 100 );
    for(; nonNull(cs); ++i, cs=tl(cs)) {
        s[i] = charOf(hd(cs));
    }
    s[i] = '\0';
    return textToStr(findText(s));
}

static Cell foreignResultTy( Type t )
{
    if      (t == typeChar)   return mkChar(CHAR_REP);
    else if (t == typeInt)    return mkChar(INT_REP);
#ifdef PROVIDE_INT64
    else if (t == typeInt64)  return mkChar(INT64_REP);
#endif
#ifdef PROVIDE_INTEGER
    else if (t == typeInteger)return mkChar(INTEGER_REP);
#endif
#ifdef PROVIDE_WORD
    else if (t == typeWord)   return mkChar(WORD_REP);
#endif
#ifdef PROVIDE_ADDR
    else if (t == typeAddr)   return mkChar(ADDR_REP);
#endif
    else if (t == typeFloat)  return mkChar(FLOAT_REP);
    else if (t == typeDouble) return mkChar(DOUBLE_REP);
#ifdef PROVIDE_FOREIGN
    else if (t == typeForeign)return mkChar(FOREIGN_REP); /* ToDo: argty only! */
#endif
#ifdef PROVIDE_ARRAY
    else if (t == typePrimByteArray) return mkChar(BARR_REP); /* ToDo: argty only! */
    else if (whatIs(t) == AP) {
        Type h = getHead(t);
        if (h == typePrimMutableByteArray) return mkChar(MUTBARR_REP); /* ToDo: argty only! */
    }
#endif
   /* ToDo: decent line numbers! */
   ERRMSG(0) "Illegal foreign type" ETHEN
   ERRTEXT " \"" ETHEN ERRTYPE(t);
   ERRTEXT "\""
   EEND;
}

static Cell foreignArgTy( Type t )
{
    return foreignResultTy( t );
}

static Name repToBox( char c )
{
    switch (c) {
    case CHAR_REP:    return nameMkC;
    case INT_REP:     return nameMkI;
#ifdef PROVIDE_INT64
    case INT64_REP:   return nameMkInt64;
#endif
#ifdef PROVIDE_INTEGER
    case INTEGER_REP: return nameMkInteger;
#endif
#ifdef PROVIDE_WORD
    case WORD_REP:    return nameMkW;
#endif
#ifdef PROVIDE_ADDR
    case ADDR_REP:    return nameMkA;
#endif
    case FLOAT_REP:   return nameMkF;
    case DOUBLE_REP:  return nameMkD;
#ifdef PROVIDE_ARRAY
    case ARR_REP:     return nameMkPrimArray;            
    case BARR_REP:    return nameMkPrimByteArray;
    case REF_REP:     return nameMkRef;                  
    case MUTARR_REP:  return nameMkPrimMutableArray;     
    case MUTBARR_REP: return nameMkPrimMutableByteArray; 
#endif
#ifdef PROVIDE_STABLE
    case STABLE_REP:  return nameMkStable;
#endif
#ifdef PROVIDE_WEAK
    case WEAK_REP:  return nameMkWeak;
#endif
#ifdef PROVIDE_FOREIGN
    case FOREIGN_REP: return nameMkForeign;
#endif
#ifdef PROVIDE_CONCURRENT
    case THREADID_REP: return nameMkThreadId;
    case MVAR_REP:     return nameMkMVar;
#endif
    default: return NIL;
    }
}

static StgPrimAlt boxResults( String reps, StgVar state )
{
    List rs = NIL;     /* possibly unboxed results     */
    List bs = NIL;     /* boxed results of wrapper     */
    List rbinds = NIL; /* bindings used to box results */
    StgExpr e   = NIL;
    Int i;
    for(i=0; reps[i] != '\0'; ++i) {
        StgRep k = mkStgRep(reps[i]);
        Cell v   = mkStgPrimVar(NIL,k,NIL);
        Name box = repToBox(reps[i]);
        if (isNull(box)) {
            bs = cons(v,bs);
        } else {
            StgRhs rhs = mkStgCon(box,singleton(v));
            StgVar bv = mkStgVar(rhs,NIL); /* boxed */
            bs     = cons(bv,bs);
            rbinds = cons(bv,rbinds);
        }
        rs = cons(v,rs);
    }
    /* Construct tuple of results */
    if (i == 1) {
        e = hd(bs);
    } else { /* includes i==0 case */
        StgVar r = mkStgVar(mkStgCon(mkTuple(i),rev(bs)),NIL);
        rbinds = cons(r,rbinds);
        e = r;
    }
    /* construct result pair if needed */
    if (nonNull(state)) {
        /* Note that this builds a tuple directly - we know it's
         * saturated.
         */
        StgVar r = mkStgVar(mkStgCon(mkTuple(2),doubleton(e,state)),NIL);
        rbinds   = cons(r,rbinds);
        rs       = cons(state,rs);      /* last result is a state */
        e = r;
    }
    return mkStgPrimAlt(rev(rs),makeStgLet(rbinds,e));
}

static List mkUnboxedVars( String reps )
{
    List as = NIL;
    Int i;
    for(i=0; reps[i] != '\0'; ++i) {
        Cell v = mkStgPrimVar(NIL,mkStgRep(reps[i]),NIL);
        as = cons(v,as);
    }
    return rev(as);
}

static List mkBoxedVars( String reps )
{
    List as = NIL;
    Int i;
    for(i=0; reps[i] != '\0'; ++i) {
        as = cons(mkStgVar(NIL,NIL),as);
    }
    return rev(as);
}

static StgRhs unboxVars( String reps, List b_args, List u_args, StgExpr e )
{
    if (nonNull(b_args)) {
        StgVar b_arg = hd(b_args); /* boxed arg   */
        StgVar u_arg = hd(u_args); /* unboxed arg */
        StgRep k     = mkStgRep(*reps);
        Name   box   = repToBox(*reps);
        e = unboxVars(reps+1,tl(b_args),tl(u_args),e);
        if (isNull(box)) {
            /* Use a trivial let-binding */
            stgVarBody(u_arg) = b_arg;
            return mkStgLet(singleton(u_arg),e);
        } else {
            StgCaseAlt alt = mkStgCaseAlt(box,singleton(u_arg),e);
            return mkStgCase(b_arg,singleton(alt));
        }
    } else {
        return e;
    }
}

/* Generate wrapper for primop based on list of arg types and result types:
 *
 * makeStgPrim op# False "II" "II" =
 *   \ x y -> "case x of { I# x# -> 
 *             case y of { I# y# -> 
 *             case op#{x#,y#} of { r1# r2# ->
 *             let r1 = I# r1#; r2 = I# r2# in
 *             (r1, r2)
 *             }}}"
 */
static StgRhs local makeStgPrim(op,addState,extra_args,a_reps,r_reps)
Name   op;
Bool   addState;
List   extra_args;
String a_reps;
String r_reps; {
    List b_args = NIL; /* boxed args to primop            */
    List u_args = NIL; /* possibly unboxed args to primop */
    List alts   = NIL; 
    StgVar s0 = addState ? mkStgVar(NIL,NIL) : NIL;
    StgVar s1 = addState ? mkStgVar(NIL,NIL) : NIL;

    /* box results */
    if (strcmp(r_reps,"B") == 0) {
        StgPrimAlt altF = mkStgPrimAlt(singleton(mkStgPrimVar(mkInt(0),mkStgRep(INT_REP),NIL)),
                                       nameFalse);
        StgPrimAlt altT = mkStgPrimAlt(singleton(mkStgPrimVar(NIL,mkStgRep(INT_REP),NIL)),
                                       nameTrue);
        alts = doubleton(altF,altT); 
        assert(nonNull(nameTrue));
        assert(!addState);
    } else {
        alts = singleton(boxResults(r_reps,s1));
    }
    b_args = mkBoxedVars(a_reps);
    u_args = mkUnboxedVars(a_reps);
    if (addState) {
        List actual_args = appendOnto(extra_args,dupListOnto(u_args,singleton(s0)));
        StgRhs rhs = makeStgLambda(singleton(s0),
                                   unboxVars(a_reps,b_args,u_args,
                                             mkStgPrimCase(mkStgPrim(op,actual_args),
                                                           alts)));
        StgVar m = mkStgVar(rhs,NIL);
        return makeStgLambda(b_args,
                             mkStgLet(singleton(m),
                                      mkStgApp(nameMkIO,singleton(m))));
    } else {
        List actual_args = appendOnto(extra_args,u_args);
        return makeStgLambda(b_args,
                             unboxVars(a_reps,b_args,u_args,mkStgPrimCase(mkStgPrim(op,actual_args),alts)));
    }
}    

Void implementPrim( n )
Name n; {
    const AsmPrim* p = name(n).primop;
    StgRhs   rhs = makeStgPrim(n,p->monad!=MONAD_Id,NIL,p->args,p->results);
    StgVar   v   = mkStgVar(rhs,NIL);
    name(n).stgVar = v;
    stgGlobals=cons(pair(n,v),stgGlobals);  /* so it will get codegened */
}

/* Generate wrapper code from (in,out) type lists.
 *
 * For example:
 * 
 *     inTypes  = [Int,Float]
 *     outTypes = [Char,Addr]
 * ==>
 *     \ fun a1 a2 -> 
 *	 let m = (\ s0 ->
 *	     case a1 of { I# a1# ->
 *	     case s2 of { F# a2# ->
 *	     case ccall# "IF" "CA" fun a1# a2# s0 of { r1# r2# s1 ->
 *	     let r1 = C# r1# in
 *	     let r2 = A# r2# in
 *	     let r  = (r1,r2) in
 *	     (r,s1)
 *	     }}})
 *	 in primMkIO m
 *	 ::
 *	 Addr -> (Int -> Float -> IO (Char,Addr)
 */
Void implementForeignImport( Name n )
{
    Type t       = name(n).type;
    List argTys    = NIL;
    List resultTys = NIL;
    CFunDescriptor* descriptor = 0;
    Bool addState = TRUE;
    while (getHead(t)==typeArrow && argCount==2) {
        Type ta = fullExpand(arg(fun(t)));
        Type tr = arg(t);
        argTys = cons(ta,argTys);
        t = tr;
    }
    argTys = rev(argTys);
    if (getHead(t) == typeIO) {
        resultTys = getArgs(t);
        assert(length(resultTys) == 1);
        resultTys = hd(resultTys);
        addState = TRUE;
    } else {
        resultTys = t;
        addState = FALSE;
    }
    resultTys = fullExpand(resultTys);
    if (isTuple(getHead(resultTys))) {
        resultTys = getArgs(resultTys);
    } else if (getHead(resultTys) == typeUnit) {
        resultTys = NIL;
    } else {
        resultTys = singleton(resultTys);
    }
    mapOver(foreignArgTy,argTys);      /* allows foreignObj, byteArrays, etc */
    mapOver(foreignResultTy,resultTys);/* doesn't */
    descriptor = mkDescriptor(charListToString(argTys),
                              charListToString(resultTys));
    name(n).primop = addState ? &ccall_IO : &ccall_Id;
    {
        Pair    extName = name(n).defn;
        void*   funPtr  = getDLLSymbol(textToStr(textOf(fst(extName))),
                                       textToStr(textOf(snd(extName))));
        List extra_args = doubleton(mkPtr(descriptor),mkPtr(funPtr));
        StgRhs rhs = makeStgPrim(n,addState,extra_args,descriptor->arg_tys,descriptor->result_tys);
        StgVar v   = mkStgVar(rhs,NIL);
        if (funPtr == 0) {
            ERRMSG(0) "Could not find foreign function \"%s\" in \"%s\"", 
                textToStr(textOf(snd(extName))),
                textToStr(textOf(fst(extName)))
            EEND;
        }
        ppStg(v);
        name(n).defn = NIL;
        name(n).stgVar = v; 
        stgGlobals=cons(pair(n,v),stgGlobals);  /* so it will get codegened */
    }
}

Void implementForeignExport( Name n )
{
    internal("implementForeignExport: not implemented");
}

Void implementTuple(size)
Int size; {
    if (size > 0) {
        Cell    t    = mkTuple(size);
        List    args = makeArgs(size);
        StgVar  tv   = mkStgVar(mkStgCon(t,args),NIL);
        StgExpr e    = mkStgLet(singleton(tv),tv);
        StgVar  v    = mkStgVar(mkStgLambda(args,e),NIL);
        stgGlobals   = cons(pair(t,v),stgGlobals);   /* so we can see it */
    } else {
        StgVar  tv   = mkStgVar(mkStgCon(nameUnit,NIL),NIL);
        stgGlobals   = cons(pair(nameUnit,tv),stgGlobals);   /* so we can see it */
    }        
}

/* --------------------------------------------------------------------------
 * Compiler control:
 * ------------------------------------------------------------------------*/

Void translateControl(what)
Int what; {
    switch (what) {
    case INSTALL:
        {
            /* deliberate fall through */
        }
    case RESET: 
            stgGlobals=NIL;
            break;
    case MARK: 
            mark(stgGlobals);
            break;
    }
}

/*-------------------------------------------------------------------------*/
