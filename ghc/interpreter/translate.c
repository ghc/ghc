
/* --------------------------------------------------------------------------
 * Translator: generates stg code from output of pattern matching
 * compiler.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: translate.c,v $
 * $Revision: 1.27 $
 * $Date: 2000/03/10 20:03:37 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "link.h"

#include "Assembler.h"


/* ---------------------------------------------------------------- */

static StgVar  local stgOffset       Args((Offset,List));
static StgVar  local stgText         Args((Text,List));
static StgRhs  local stgRhs          Args((Cell,Int,List,StgExpr));
static StgCaseAlt local stgCaseAlt   Args((Cell,Int,List,StgExpr));
static StgExpr local stgExpr         Args((Cell,Int,List,StgExpr));

/* ---------------------------------------------------------------- */

/* Association list storing globals assigned to                     */
/* dictionaries, tuples, etc                                        */
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

static StgRhs local stgRhs(e,co,sc,failExpr)
Cell e; 
Int  co; 
List sc;
StgExpr failExpr; {
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
            return mkStgCon(nameMkInteger,singleton(e));
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
            return stgExpr(e,co,sc,failExpr/*namePMFail*/);
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
                StgVar   c    = mkStgVar(
                                   mkStgCon(nameMkC,singleton(d)),NIL);
                StgExpr  test = nameEqChar;
                /* duplicates scrut but it should be atomic */
                return makeStgIf(
                          makeStgLet(singleton(c),
                             makeStgApp(test,doubleton(scrut,c))),
                          stgExpr(snd(alt),co,sc,failExpr),
                          stgExpr(ap(CASE,pair(fst(snd(e)),
                             tl(alts))),co,sc,failExpr));
            } else {
                List as    = NIL;
                for(; nonNull(alts); alts=tl(alts)) {
                    as = cons(stgCaseAlt(hd(alts),co,sc,failExpr),as);
                }
                return mkStgCase(
                          scrut,
                          revOnto(
                             as, 
                             singleton(mkStgDefault(mkStgVar(NIL,NIL),
                                       failExpr))));
            }
        }
    case NUMCASE:
        {
            Triple nc    = snd(e);
            Offset o     = fst3(nc);
            Cell   discr = snd3(nc);
            Cell   r     = thd3(nc);
            Cell   scrut = stgOffset(o,sc);
            Cell   h     = getHead(discr);
            Int    da    = discrArity(discr);
            char   str[30];

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
                dIntegral = stgRhs(dictIntegral,co,sc,namePMFail);
                if (!isAtomic(dIntegral)) { /* wasn't atomic */
                    dIntegral = mkStgVar(dIntegral,NIL);
                    binds = cons(dIntegral,binds);
                }

                /* box number */
                sprintf(str, "%d", n);
                n = mkStgVar(mkStgCon(nameMkInteger,singleton(stringToBignum(str))),NIL);
                binds = cons(n,binds);

                /* coerce number to right type (using Integral dict) */
                n = mkStgVar(mkStgApp(
                       namePmFromInteger,doubleton(dIntegral,n)),NIL);
                binds = cons(n,binds);

                ++co;
                v = mkStgVar(mkStgApp(
                       namePmSubtract,tripleton(dIntegral,scrut,n)),NIL);
                return 
                   mkStgLet(
                      binds,
                      makeStgIf(
                         mkStgApp(namePmLe,tripleton(dIntegral,n,scrut)),
                         mkStgLet(singleton(v),
                                  stgExpr(r,
                                          co,
                                          cons(pair(mkOffset(co),v),sc),
                                          failExpr)),
                         failExpr));
            }

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
                //StgExpr m     = NIL;
                Name   box
                    = h == nameFromInt     ? nameMkI
                    : h == nameFromInteger ? nameMkInteger
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
                d = stgRhs(dict,co,sc,namePMFail);
                if (!isAtomic(d)) { /* wasn't atomic */
                    d = mkStgVar(d,NIL);
                    binds = cons(d,binds);
                }
                /* bind number */
                n = mkStgVar(mkStgCon(box,singleton(n)),NIL);
                binds = cons(n,binds);

                return 
                   makeStgIf(
                      mkStgLet(binds,
                               mkStgApp(testFun,tripleton(d,n,scrut))),
                      stgExpr(r,co+da,altsc,failExpr),
                      failExpr
                   );
            }
        }

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
                stgVarBody(nv) 
                   = mkStgLambda(
                        as,
                        stgExpr(thd3(thd3(fun)),co+arity,funsc,namePMFail));
            }
            /* transform expressions */
            for(bs = fst(fst(snd(e))); nonNull(bs); bs=tl(bs), vs=tl(vs)) {
                Cell rhs = hd(bs);
                Cell nv  = hd(vs);
                stgVarBody(nv) = stgRhs(rhs,co,sc,namePMFail);
            }
            return mkStgLet(binds,stgRhs(snd(snd(e)),co,sc,failExpr/*namePMFail*/));
        }

    default: /* convert to an StgApp or StgVar plus some bindings */
        {   
            List args  = NIL;
            List binds = NIL;
            List as    = NIL;
            Int  length_args;

            /* Unwind args */
            while (isAp(e)) {
                Cell arg = arg(e);
                e        = fun(e);
                args = cons(arg,args);
            }

            /* Special cases */
            if (e == nameSel && length(args) == 3) {
                Cell   con   = hd(args);
                StgExpr v    = stgExpr(hd(tl(args)),co,sc,namePMFail);
                Int    ix    = intOf(hd(tl(tl(args))));
                Int    da    = discrArity(con);
                List   vs    = NIL;
                Int    i;
                for(i=1; i<=da; ++i) {
                    Cell nv = mkStgVar(NIL,NIL);
                    vs=cons(nv,vs);
                }
                return 
                   mkStgCase(v,
                             doubleton(mkStgCaseAlt(con,vs,nth(ix-1,vs)),
                             mkStgDefault(mkStgVar(NIL,NIL),namePMFail)));
            }
            
            /* Arguments must be StgAtoms */
            for(as=args; nonNull(as); as=tl(as)) {
                StgRhs a = stgRhs(hd(as),co,sc,namePMFail);
                if (whatIs(a) == LETREC) {
                    binds = appendOnto(stgLetBinds(a),binds);
                    a = stgLetBody(a);
                }
                if (!isAtomic(a)) {
                    a     = mkStgVar(a,NIL);
                    binds = cons(a,binds);
                }
                hd(as) = a;
            }

            /* Special case: saturated constructor application */
            length_args = length(args);
            if ( (isName(e) && isCfun(e)
                  && name(e).arity > 0 
                  && name(e).arity == length_args)
                 ||
                 (isTuple(e) && tycon(e).tuple == length_args)
               ) {
               StgVar v; 
               /* fprintf ( stderr, "saturated application of %s\n",
                 	    textToStr(isTuple(e) ? tycon(e).text : name(e).text)); */
               v = mkStgVar(mkStgCon(e,args),NIL);
               binds = cons(v,binds);
               return mkStgLet(binds,v);

               
            }

            /* Function must be StgVar or Name */
            e = stgRhs(e,co,sc,namePMFail);
            if (!isStgVar(e) && !isName(e)) {
                e = mkStgVar(e,NIL);
                binds = cons(e,binds);
            }

            return makeStgLet(binds,makeStgApp(e,args));
        }
    }
}


Void stgDefn( Name n, Int arity, Cell e )
{
    List vs = NIL;
    List sc = NIL;
    Int i, s;
    for (i = 1; i <= arity; ++i) {
        Cell nv = mkStgVar(NIL,NIL);
        vs = cons(nv,vs);
        sc = cons(pair(mkOffset(i),nv),sc);
    }
    stgVarBody(name(n).stgVar) 
       = makeStgLambda(vs,stgExpr(e,arity,sc,namePMFail));
}

Void implementCfun(c,scs)               /* Build implementation for constr */
Name c;                                 /* fun c.  scs lists integers (1..)*/
List scs; {                             /* in incr order of strict fields. */
    Int a = name(c).arity;

    if (a > 0) {
        StgVar  vcurr, e1, v, vsi;
        List    args  = makeArgs(a);
        StgVar  v0    = mkStgVar(mkStgCon(c,args),NIL);
        List    binds = singleton(v0);

        vcurr = v0;
        for (; nonNull(scs); scs=tl(scs)) {
           vsi   = nth(intOf(hd(scs))-1,args);
           vcurr = mkStgVar(mkStgApp(namePrimSeq,doubleton(vsi,vcurr)), NIL);
           binds = cons(vcurr,binds);
        }
        binds = rev(binds);
        e1    = mkStgLet(binds,vcurr);
        v     = mkStgVar(mkStgLambda(args,e1),NIL);
        name(c).stgVar = v;
    } else {
        StgVar v = mkStgVar(mkStgCon(c,NIL),NIL);
        name(c).stgVar = v;
    }
    stgGlobals = cons(pair(c,name(c).stgVar),stgGlobals); 
    /* printStg(stderr, name(c).stgVar); fprintf(stderr,"\n\n"); */
}

/* --------------------------------------------------------------------------
 * Foreign function calls and primops
 * ------------------------------------------------------------------------*/

/* Outbound denotes data moving from Haskell world to elsewhere.
   Inbound denotes data moving from elsewhere to Haskell world.
*/
static String  charListToString   ( List cs );
static Cell    foreignTy          ( Bool outBound, Type t );
static Cell    foreignOutboundTy  ( Type t );
static Cell    foreignInboundTy   ( Type t );
static Name    repToBox           ( char c );
static StgRhs  makeStgPrim        ( Name,Bool,List,String,String );

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

static Cell foreignTy ( Bool outBound, Type t )
{
    if      (t == typeChar)   return mkChar(CHAR_REP);
    else if (t == typeInt)    return mkChar(INT_REP);
#if 0
    else if (t == typeInteger)return mkChar(INTEGER_REP);
#endif
    else if (t == typeWord)   return mkChar(WORD_REP);
    else if (t == typeAddr)   return mkChar(ADDR_REP);
    else if (t == typeFloat)  return mkChar(FLOAT_REP);
    else if (t == typeDouble) return mkChar(DOUBLE_REP);
    else if (t == typeStable) return mkChar(STABLE_REP);
#ifdef PROVIDE_FOREIGN
    else if (t == typeForeign)return mkChar(FOREIGN_REP); 
         /* ToDo: argty only! */
#endif
#if 0
    else if (t == typePrimByteArray) return mkChar(BARR_REP); 
         /* ToDo: argty only! */
    else if (whatIs(t) == AP) {
        Type h = getHead(t);
        if (h == typePrimMutableByteArray) return mkChar(MUTBARR_REP); 
         /* ToDo: argty only! */
    }
#endif
   /* ToDo: decent line numbers! */
   if (outBound) {
      ERRMSG(0) "Illegal outbound (away from Haskell) type" ETHEN
      ERRTEXT " \"" ETHEN ERRTYPE(t);
      ERRTEXT "\""
      EEND;
   } else {
      ERRMSG(0) "Illegal inbound (towards Haskell) type" ETHEN
      ERRTEXT " \"" ETHEN ERRTYPE(t);
      ERRTEXT "\""
      EEND;
   }
}

static Cell foreignOutboundTy ( Type t )
{
    return foreignTy ( TRUE, t );
}

static Cell foreignInboundTy ( Type t )
{
    return foreignTy ( FALSE, t );
}

static Name repToBox( char c )
{
    switch (c) {
    case CHAR_REP:     return nameMkC;
    case INT_REP:      return nameMkI;
    case INTEGER_REP:  return nameMkInteger;
    case WORD_REP:     return nameMkW;
    case ADDR_REP:     return nameMkA;
    case FLOAT_REP:    return nameMkF;
    case DOUBLE_REP:   return nameMkD;
    case ARR_REP:      return nameMkPrimArray;            
    case BARR_REP:     return nameMkPrimByteArray;
    case REF_REP:      return nameMkRef;                  
    case MUTARR_REP:   return nameMkPrimMutableArray;     
    case MUTBARR_REP:  return nameMkPrimMutableByteArray; 
    case STABLE_REP:   return nameMkStable;
    case THREADID_REP: return nameMkThreadId;
    case MVAR_REP:     return nameMkPrimMVar;
#ifdef PROVIDE_WEAK
    case WEAK_REP:  return nameMkWeak;
#endif
#ifdef PROVIDE_FOREIGN
    case FOREIGN_REP: return nameMkForeign;
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
    if (i == 0) {
        e = nameUnit;
    } else
    if (i == 1) {
        e = hd(bs);
    } else {
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
        StgPrimAlt altF 
           = mkStgPrimAlt(singleton(
                            mkStgPrimVar(mkInt(0),
                                         mkStgRep(INT_REP),NIL)
                          ),
                          nameFalse);
        StgPrimAlt altT 
           = mkStgPrimAlt(
                singleton(mkStgPrimVar(NIL,mkStgRep(INT_REP),NIL)),
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
        List actual_args 
           = appendOnto(extra_args,dupListOnto(u_args,singleton(s0)));
        StgRhs rhs 
           = makeStgLambda(singleton(s0),
                           unboxVars(a_reps,b_args,u_args,
                                     mkStgPrimCase(mkStgPrim(op,actual_args),
                                                   alts)));
        StgVar m = mkStgVar(rhs,NIL);
        return makeStgLambda(b_args,
                             mkStgLet(singleton(m),
                                      mkStgApp(nameMkIO,singleton(m))));
    } else {
        List actual_args = appendOnto(extra_args,u_args);
        return makeStgLambda(
                  b_args,
                  unboxVars(a_reps,b_args,u_args,
                            mkStgPrimCase(mkStgPrim(op,actual_args),alts))
               );
    }
}    

Void implementPrim ( n )
Name n; {
    const AsmPrim* p = name(n).primop;
    StgRhs   rhs = makeStgPrim(n,p->monad!=MONAD_Id,NIL,p->args,p->results);
    StgVar   v   = mkStgVar(rhs,NIL);
    name(n).stgVar   = v;
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
 *	 Addr -> (Int -> Float -> IO (Char,Addr))
 */
Void implementForeignImport ( Name n )
{
    Type t         = name(n).type;
    List argTys    = NIL;
    List resultTys = NIL;
    CFunDescriptor* descriptor = 0;
    Bool addState  = TRUE;
    Bool dynamic   = isNull(name(n).defn);
    while (getHead(t)==typeArrow && argCount==2) {
        Type ta = fullExpand(arg(fun(t)));
        Type tr = arg(t);
        argTys = cons(ta,argTys);
        t = tr;
    }
    argTys = rev(argTys);

    /* argTys now holds the argument tys.  If this is a dynamic call,
       the first one had better be an Addr.
    */
    if (dynamic) {
       if (isNull(argTys) || hd(argTys) != typeAddr) {
          ERRMSG(name(n).line) "First argument in f-i-dynamic must be an Addr"
          EEND;
       }
    }

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
    mapOver(foreignOutboundTy,argTys);  /* allows foreignObj, byteArrays, etc */
    mapOver(foreignInboundTy,resultTys); /* doesn't */
    descriptor 
       = mkDescriptor(charListToString(argTys),
                      charListToString(resultTys));
    if (!descriptor) {
       ERRMSG(name(n).line) "Can't allocate memory for call descriptor"
       EEND;
    }

    /* ccall is the default convention, if it wasn't specified */
    if (isNull(name(n).callconv)
        || name(n).callconv == textCcall) {
       name(n).primop = addState ? &ccall_ccall_IO : &ccall_ccall_Id;
    } 
    else if (name(n).callconv == textStdcall) {
       if (!stdcallAllowed()) {
          ERRMSG(name(n).line) "stdcall is not supported on this platform"
          EEND;
       }
       name(n).primop = addState ? &ccall_stdcall_IO : &ccall_stdcall_Id;
    }
    else
       internal ( "implementForeignImport: unknown calling convention");

    {
        Pair   extName;
        void*  funPtr;
        List   extra_args;
        StgRhs rhs;
        StgVar v;

        if (dynamic) {
           funPtr     = NULL;
           extra_args = singleton(mkPtr(descriptor));
           /* and we know that the first arg will be the function pointer */
        } else {
           extName = name(n).defn;
           funPtr  = getDLLSymbol(name(n).line,
                                  textToStr(textOf(fst(extName))),
                                  textToStr(textOf(snd(extName))));
           if (funPtr == 0) {
               ERRMSG(name(n).line) 
                   "Could not find foreign function \"%s\" in \"%s\"", 
                   textToStr(textOf(snd(extName))),
                   textToStr(textOf(fst(extName)))
               EEND;
           }
           extra_args = doubleton(mkPtr(descriptor),mkPtr(funPtr));
        }

        rhs              = makeStgPrim(n,addState,extra_args,
                                       descriptor->arg_tys,
                                       descriptor->result_tys);
        v                = mkStgVar(rhs,NIL);
        name(n).defn     = NIL;
        name(n).stgVar   = v;
        stgGlobals       = cons(pair(n,v),stgGlobals);
    }

    /* At this point the descriptor contains a tags for all args,
       because that makes makeStgPrim generate the correct unwrap
       code.  From now on, the descriptor is only used at the time
       the actual ccall is made.  So we need to zap the leading
       addr arg IF this is a f-i-dynamic call.
    */
    if (dynamic) {
       descriptor->arg_tys++;
       descriptor->num_args--;
    }
}


/* Generate code:
 *
 * \ fun ->
     let e1 = A# "...."
         e3 = C# 'c' -- (ccall), or 's' (stdcall)
     in  primMkAdjThunk fun e1 e3

   we require, and check that,
     fun :: prim_arg* -> IO prim_result
 */
Void implementForeignExport ( Name n )
{
    Type t         = name(n).type;
    List argTys    = NIL;
    List resultTys = NIL;
    Char cc_char;

    if (getHead(t)==typeArrow && argCount==2) {
       t = arg(fun(t));
    } else {
        ERRMSG(name(n).line) "foreign export has illegal type" ETHEN
        ERRTEXT " \"" ETHEN ERRTYPE(t);
        ERRTEXT "\""
        EEND;        
    }

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
    } else {
        ERRMSG(name(n).line) "function to be exported doesn't return an IO type: " ETHEN
        ERRTEXT " \"" ETHEN ERRTYPE(t);
        ERRTEXT "\""
        EEND;        
    }
    resultTys = fullExpand(resultTys);

    mapOver(foreignInboundTy,argTys);

    /* ccall is the default convention, if it wasn't specified */
    if (isNull(name(n).callconv)
        || name(n).callconv == textCcall) {
        cc_char = 'c';
    } 
    else if (name(n).callconv == textStdcall) {
       if (!stdcallAllowed()) {
          ERRMSG(name(n).line) "stdcall is not supported on this platform"
          EEND;
       }
       cc_char = 's';
    }
    else
       internal ( "implementForeignExport: unknown calling convention");

    {
    List     tdList;
    Text     tdText;
    List     args;
    StgVar   e1, e2, e3, v;
    StgExpr  fun;

    tdList = cons(mkChar(':'),argTys);
    if (resultTys != typeUnit)
       tdList = cons(foreignOutboundTy(resultTys),tdList);

    tdText = findText(charListToString ( tdList ));
    args   = makeArgs(1);
    e1     = mkStgVar(
                mkStgCon(nameMkA,singleton(ap(STRCELL,tdText))),
                NIL
             );
    e2     = mkStgVar(
                mkStgApp(nameUnpackString,singleton(e1)),
                NIL
             );
    e3     = mkStgVar(
                mkStgCon(nameMkC,singleton(mkChar(cc_char))),
                NIL
             );
    fun    = mkStgLambda(
                args,
                mkStgLet(
                   tripleton(e1,e2,e3),
                   mkStgApp(
                      nameCreateAdjThunk,
                      cons(hd(args),cons(e2,cons(e3,NIL)))
                   )
                )
             );

    v = mkStgVar(fun,NIL);

    name(n).defn     = NIL;    
    name(n).stgVar   = v;
    stgGlobals       = cons(pair(n,v),stgGlobals);
    }
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
        stgGlobals   = cons(pair(nameUnit,tv),stgGlobals);      /* ditto */
    }        
}

/* --------------------------------------------------------------------------
 * Compiler control:
 * ------------------------------------------------------------------------*/

Void translateControl(what)
Int what; {
    switch (what) {
       case POSTPREL: break;
       case PREPREL:
       case RESET: 
          stgGlobals=NIL;
          break;
       case MARK: 
          mark(stgGlobals);
          break;
    }
}

/*-------------------------------------------------------------------------*/
