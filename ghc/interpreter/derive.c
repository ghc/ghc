
/* --------------------------------------------------------------------------
 * Deriving
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: derive.c,v $
 * $Revision: 1.3 $
 * $Date: 1999/02/03 17:08:27 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "backend.h"
#include "connect.h"
#include "errors.h"

static Cell varTrue;
static Cell varFalse;
#if DERIVE_ORD
static Cell varCompAux;                /* auxiliary function for compares */
static Cell varCompare;
static Cell varEQ;
#endif
#if DERIVE_IX
static Cell varRangeSize;              /* calculate size of index range   */
static Cell varInRange;
static Cell varRange;
static Cell varIndex;
static Cell varMult; 
static Cell varPlus;
static Cell varMap;
static Cell varMinus;
static Cell varError;
#endif
#if DERIVE_ENUM
static Cell varToEnum;
static Cell varFromEnum; 
static Cell varEnumFromTo;    
static Cell varEnumFromThenTo;  
#endif
#if DERIVE_BOUNDED
static Cell varMinBound;
static Cell varMaxBound;
#endif
#if DERIVE_SHOW
       Cell conCons;
static Cell varShowField;              /* display single field            */
static Cell varShowParen;              /* wrap with parens                */
static Cell varCompose;                /* function composition            */
static Cell varShowsPrec;
static Cell varLe;
#endif                                 
#if DERIVE_READ                        
static Cell varReadField;              /* read single field               */
static Cell varReadParen;              /* unwrap from parens              */
static Cell varLex;                    /* lexer                           */
static Cell varReadsPrec;
static Cell varGt;
#endif                                 
#if DERIVE_SHOW || DERIVE_READ         
static Cell varAppend;                 /* list append                     */
List cfunSfuns;                        /* List of (Cfun,[SelectorVar])    */
#endif                                 
#if DERIVE_EQ || DERIVE_IX             
static Cell varAnd;                    /* built-in logical connectives    */
#endif
#if DERIVE_EQ || DERIVE_ORD            
static Cell varEq;
#endif


/* --------------------------------------------------------------------------
 * local function prototypes:
 * ------------------------------------------------------------------------*/

static List  local getDiVars            Args((Int));
static Cell  local mkBind               Args((String,List));
static Cell  local mkVarAlts            Args((Int,Cell));

#if DERIVE_EQ || DERIVE_ORD
static List  local makeDPats2           Args((Cell,Int));
#endif
#if DERIVE_ORD || DERIVE_ENUM || DERIVE_IX || DERIVE_BOUNDED
static Bool  local isEnumType           Args((Tycon));
#endif

static Pair   local mkAltEq             Args((Int,List));
static Pair   local mkAltOrd            Args((Int,List));
static Cell   local prodRange           Args((Int,List,Cell,Cell,Cell));
static Cell   local prodIndex           Args((Int,List,Cell,Cell,Cell));
static Cell   local prodInRange         Args((Int,List,Cell,Cell,Cell));
static List   local mkIxBinds           Args((Int,Cell,Int));
static Cell   local mkAltShow           Args((Int,Cell,Int));
static Cell   local showsPrecRhs        Args((Cell,Cell,Int));
static Cell   local mkReadCon           Args((Name,Cell,Cell));
static Cell   local mkReadPrefix        Args((Cell));
static Cell   local mkReadInfix         Args((Cell));
static Cell   local mkReadTuple         Args((Cell));
static Cell   local mkReadRecord        Args((Cell,List));
static List   local mkBndBinds          Args((Int,Cell,Int));



/* --------------------------------------------------------------------------
 * Deriving Utilities
 * ------------------------------------------------------------------------*/

List diVars = NIL;                      /* Acts as a cache of invented vars*/
Int  diNum  = 0;

static List local getDiVars(n)          /* get list of at least n vars for */
Int n; {                                /* derived instance generation     */
    for (; diNum<n; diNum++) {
        diVars = cons(inventVar(),diVars);
    }
    return diVars;
}

static Cell local mkBind(s,alts)        /* make a binding for a variable   */
String s;
List   alts; {
    return pair(mkVar(findText(s)),pair(NIL,alts));
}

static Cell local mkVarAlts(line,r)     /* make alts for binding a var to  */
Int  line;                              /* a simple expression             */
Cell r; {
    return singleton(pair(NIL,pair(mkInt(line),r)));
}

/* --------------------------------------------------------------------------
 * Given a datatype:   data T a b = A a b | B Int | C  deriving (Eq, Ord)
 * The derived definitions of equality and ordering are given by:
 *
 *   A a b == A x y  =  a==x && b==y
 *   B a   == B x    =  a==x
 *   C     == C      =  True
 *   _     == _      =  False
 *
 *   compare (A a b) (A x y) =  primCompAux a x (compare b y)
 *   compare (B a)   (B x)   =  compare a x
 *   compare C       C       =  EQ
 *   compare a       x       =  cmpConstr a x
 *
 * In each case, the last line is only needed if there are multiple
 * constructors in the datatype definition.
 * ------------------------------------------------------------------------*/

#define ap2(f,x,y) ap(ap(f,x),y)

List local deriveEq(t)                  /* generate binding for derived == */
Type t; {                               /* for some TUPLE or DATATYPE t    */
    List alts = NIL;
    if (isTycon(t)) {                   /* deal with type constrs          */
        List cs = tycon(t).defn;
        for (; hasCfun(cs); cs=tl(cs)) {
            alts = cons(mkAltEq(tycon(t).line,
                                makeDPats2(hd(cs),userArity(hd(cs)))),
                        alts);
        }
        if (cfunOf(hd(tycon(t).defn))!=0) {
            alts = cons(pair(cons(WILDCARD,cons(WILDCARD,NIL)),
                             pair(mkInt(tycon(t).line),nameFalse)),alts);
        }
        alts = rev(alts);
    }
    else {                              /* special case for tuples         */
        alts = singleton(mkAltEq(0,makeDPats2(t,tupleOf(t))));
    }
    return singleton(mkBind("==",alts));
}

static Pair local mkAltEq(line,pats)    /* make alt for an equation for == */
Int  line;                              /* using patterns in pats for lhs  */
List pats; {                            /* arguments                       */
    Cell p = hd(pats);
    Cell q = hd(tl(pats));
    Cell e = nameTrue;

    if (isAp(p)) {
        e = ap2(nameEq,arg(p),arg(q));
        for (p=fun(p), q=fun(q); isAp(p); p=fun(p), q=fun(q)) {
            e = ap2(nameAnd,ap2(nameEq,arg(p),arg(q)),e);
        }
    }
    return pair(pats,pair(mkInt(line),e));
}

List deriveOrd(t)                       /* make binding for derived compare*/
Type t; {                               /* for some TUPLE or DATATYPE t    */
    List alts = NIL;
    if (isEnumType(t)) {                /* special case for enumerations   */
        alts = mkVarAlts(tycon(t).line,nameConCmp);
    } else if (isTycon(t)) {            /* deal with type constrs          */
        List cs = tycon(t).defn;
        for (; hasCfun(cs); cs=tl(cs)) {
            alts = cons(mkAltOrd(tycon(t).line,
                                 makeDPats2(hd(cs),userArity(hd(cs)))),
                        alts);
        }
        if (cfunOf(hd(tycon(t).defn))!=0) {
            Cell u = inventVar();
            Cell w = inventVar();
            alts   = cons(pair(cons(u,singleton(w)),
                               pair(mkInt(tycon(t).line),
                                    ap2(nameConCmp,u,w))),alts);
        }
        alts = rev(alts);
    } else {                            /* special case for tuples         */
        alts = singleton(mkAltOrd(0,makeDPats2(t,tupleOf(t))));
    }
    return singleton(mkBind("compare",alts));
}

static Pair local mkAltOrd(line,pats)   /* make alt for eqn for compare    */
Int  line;                              /* using patterns in pats for lhs  */
List pats; {                            /* arguments                       */
    Cell p = hd(pats);
    Cell q = hd(tl(pats));
    Cell e = nameEQ;

    if (isAp(p)) {
        e = ap2(nameCompare,arg(p),arg(q));
        for (p=fun(p), q=fun(q); isAp(p); p=fun(p), q=fun(q)) {
            e = ap(ap2(nameCompAux,arg(p),arg(q)),e);
        }
    }

    return pair(pats,pair(mkInt(line),e));
}

static List local makeDPats2(h,n)       /* generate pattern list           */
Cell h;                                 /* by putting two new patterns with*/
Int  n; {                               /* head h and new var components   */
    List us = getDiVars(2*n);
    List vs = NIL;
    Cell p;
    Int  i;

    for (i=0, p=h; i<n; ++i) {          /* make first version of pattern   */
        p  = ap(p,hd(us));
        us = tl(us);
    }
    vs = cons(p,vs);

    for (i=0, p=h; i<n; ++i) {          /* make second version of pattern  */
        p  = ap(p,hd(us));
        us = tl(us);
    }
    return cons(p,vs);
}

/* --------------------------------------------------------------------------
 * Deriving Ix and Enum:
 * ------------------------------------------------------------------------*/

List deriveEnum(t)              /* Construct definition of enumeration     */
Tycon t; {
    Int l = tycon(t).line;

    if (!isEnumType(t)) {
        ERRMSG(l) "Can only derive instances of Enum for enumeration types"
        EEND;
    }

    return cons(mkBind("toEnum",mkVarAlts(l,ap(nameEnToEn,hd(tycon(t).defn)))),
            cons(mkBind("fromEnum",mkVarAlts(l,nameEnFrEn)),
             cons(mkBind("enumFrom",mkVarAlts(l,nameEnFrom)),
              cons(mkBind("enumFromTo",mkVarAlts(l,nameEnFrTo)),
               cons(mkBind("enumFromThen",mkVarAlts(l,nameEnFrTh)),NIL)))));
}

List deriveIx(t)                /* Construct definition of indexing        */
Tycon t; {
    if (isEnumType(t)) {        /* Definitions for enumerations            */
        return cons(mkBind("range",mkVarAlts(tycon(t).line,nameEnRange)),
                cons(mkBind("index",mkVarAlts(tycon(t).line,nameEnIndex)),
                 cons(mkBind("inRange",mkVarAlts(tycon(t).line,nameEnInRng)),
                  NIL)));
    } else if (isTuple(t)) {    /* Definitions for product types           */
        return mkIxBinds(0,t,tupleOf(t));
    } else if (isTycon(t) && cfunOf(hd(tycon(t).defn))==0) {
        return mkIxBinds(tycon(t).line,
                         hd(tycon(t).defn),
                         userArity(hd(tycon(t).defn)));
    }
    ERRMSG(tycon(t).line)
        "Can only derive instances of Ix for enumeration or product types"
    EEND;
    return NIL;/* NOTREACHED*/
}

static Bool local isEnumType(t) /* Determine whether t is an enumeration   */
Tycon t; {                      /* type (i.e. all constructors arity == 0) */
    if (isTycon(t) && (tycon(t).what==DATATYPE || tycon(t).what==NEWTYPE)) {
        List cs = tycon(t).defn;
        for (; hasCfun(cs); cs=tl(cs)) {
            if (name(hd(cs)).arity!=0) {
                return FALSE;
            }
        }
        /* ToDo: correct?  addCfunTable(t); */
        return TRUE;
    }
    return FALSE;
}

static List local mkIxBinds(line,h,n)   /* build bindings for derived Ix on*/
Int  line;                              /* a product type                  */
Cell h;
Int  n; {
    List vs   = getDiVars(3*n);
    Cell ls   = h;
    Cell us   = h;
    Cell is   = h;
    Cell pr   = NIL;
    Cell pats = NIL;
    Int  i;

    for (i=0; i<n; ++i, vs=tl(vs)) {    /* build three patterns for values */
        ls = ap(ls,hd(vs));             /* of the datatype concerned       */
        us = ap(us,hd(vs=tl(vs)));
        is = ap(is,hd(vs=tl(vs)));
    }
    pr   = ap2(mkTuple(2),ls,us);       /* Build (ls,us)                   */
    pats = cons(pr,cons(is,NIL));       /* Build [(ls,us),is]              */

    return cons(prodRange(line,singleton(pr),ls,us,is),
                cons(prodIndex(line,pats,ls,us,is),
                     cons(prodInRange(line,pats,ls,us,is),NIL)));
}

static Cell local prodRange(line,pats,ls,us,is)
Int  line;                              /* Make definition of range for a  */
List pats;                              /* product type                    */
Cell ls, us, is; {
    /* range :: (a,a) -> [a]
     * range (X a b c, X p q r)
     *   = [ X x y z | x <- range (a,p), y <- range (b,q), z <- range (c,r) ]
     */
    Cell is1 = is;
    List e   = NIL;
    for (; isAp(ls); ls=fun(ls), us=fun(us), is=fun(is)) {
        e = cons(ap(FROMQUAL,pair(arg(is),
                                  ap(nameRange,ap2(mkTuple(2),
                                                   arg(ls),
                                                   arg(us))))),e);
    }
    e = ap(COMP,pair(is1,e));
    e = singleton(pair(pats,pair(mkInt(line),e)));
    return mkBind("range",e);
}

static Cell local prodIndex(line,pats,ls,us,is)
Int  line;                              /* Make definition of index for a  */
List pats;                              /* product type                    */
Cell ls, us, is; {
    /* index :: (a,a) -> a -> Bool
     * index (X a b c, X p q r) (X x y z)
     *  = index (c,r) z + rangeSize (c,r) * (
     *     index (b,q) y + rangeSize (b,q) * (
     *      index (a,x) x))
     */
    List xs = NIL;
    Cell e  = NIL;
    for (; isAp(ls); ls=fun(ls), us=fun(us), is=fun(is)) {
        xs = cons(ap2(nameIndex,ap2(mkTuple(2),arg(ls),arg(us)),arg(is)),xs);
    }
    for (e=hd(xs); nonNull(xs=tl(xs));) {
        Cell x = hd(xs);
        e = ap2(namePlus,x,ap2(nameMult,ap(nameRangeSize,arg(fun(x))),e));
    }
    e = singleton(pair(pats,pair(mkInt(line),e)));
    return mkBind("index",e);
}

static Cell local prodInRange(line,pats,ls,us,is)
Int  line;                              /* Make definition of inRange for a*/
List pats;                              /* product type                    */
Cell ls, us, is; {
    /* inRange :: (a,a) -> a -> Bool
     * inRange (X a b c, X p q r) (X x y z)
     *          = inRange (a,p) x && inRange (b,q) y && inRange (c,r) z
     */
    Cell e = ap2(nameInRange,ap2(mkTuple(2),arg(ls),arg(us)),arg(is));
    while (ls=fun(ls), us=fun(us), is=fun(is), isAp(ls)) {
        e = ap2(nameAnd,
                ap2(nameInRange,ap2(mkTuple(2),arg(ls),arg(us)),arg(is)),
                e);
    }
    e = singleton(pair(pats,pair(mkInt(line),e)));
    return mkBind("inRange",e);
}

/* --------------------------------------------------------------------------
 * Deriving Show:
 * ------------------------------------------------------------------------*/

List deriveShow(t)              /* Construct definition of text conversion */
Tycon t; {
    List alts = NIL;
    if (isTycon(t)) {                   /* deal with type constrs          */
        List cs = tycon(t).defn;
        for (; hasCfun(cs); cs=tl(cs)) {
            alts = cons(mkAltShow(tycon(t).line,hd(cs),userArity(hd(cs))),
                        alts);
        }
        alts = rev(alts);
    } else {                            /* special case for tuples         */
        alts = singleton(mkAltShow(0,t,tupleOf(t)));
    }
    return singleton(mkBind("showsPrec",alts));
}

static Cell local mkAltShow(line,h,a)   /* make alt for showsPrec eqn      */
Int  line;
Cell h;
Int  a; {
    List vs   = getDiVars(a+1);
    Cell d    = hd(vs);
    Cell pat  = h;
    List pats = NIL;
    Int  i    = 0;
    for (vs=tl(vs); i<a; i++) {
        pat = ap(pat,hd(vs));
        vs  = tl(vs);
    }
    pats = cons(d,cons(pat,NIL));
    return pair(pats,pair(mkInt(line),showsPrecRhs(d,pat,a)));
}

#define shows0   ap(nameShowsPrec,mkInt(0))
#define shows10  ap(nameShowsPrec,mkInt(10))
#define showsOP  ap(nameComp,consChar('('))
#define showsOB  ap(nameComp,consChar('{'))
#define showsCM  ap(nameComp,consChar(','))
#define showsSP  ap(nameComp,consChar(' '))
#define showsBQ  ap(nameComp,consChar('`'))
#define showsCP  consChar(')')
#define showsCB  consChar('}')

static Cell local showsPrecRhs(d,pat,a) /* build a rhs for showsPrec for a */
Cell d, pat;                            /* given pattern, pat              */
Int  a; {
    Cell h   = getHead(pat);
    List cfs = cfunSfuns;

    if (isTuple(h)) {
        /* To display a tuple:
         *    showsPrec d (a,b,c,d) = showChar '(' . showsPrec 0 a .
         *                            showChar ',' . showsPrec 0 b .
         *                            showChar ',' . showsPrec 0 c .
         *                            showChar ',' . showsPrec 0 d .
         *                            showChar ')'
         */
        Int  i   = tupleOf(h);
        Cell rhs = showsCP;
        for (; i>1; --i) {
            rhs = ap(showsCM,ap2(nameComp,ap(shows0,arg(pat)),rhs));
            pat = fun(pat);
        }
        return ap(showsOP,ap2(nameComp,ap(shows0,arg(pat)),rhs));
    }

    for (; nonNull(cfs) && h!=fst(hd(cfs)); cfs=tl(cfs)) {
    }
    if (nonNull(cfs)) {
        /* To display a value using record syntax:
         *    showsPrec d C{x=e, y=f, z=g} = showString "C"  . showChar '{' .
         *                                   showField "x" e . showChar ',' .
         *                                   showField "y" f . showChar ',' .
         *                                   showField "z" g . showChar '}'
         *    showField lab val
         *      = showString lab . showChar '=' . shows val
         */
        Cell rhs     = showsCB;
        List vs      = dupOnto(snd(hd(cfs)),NIL);
        if (isAp(pat)) {
            for (;;) {
                rhs = ap2(nameComp,
                          ap2(nameShowField,
                              mkStr(textOf(hd(vs))),
                              arg(pat)),
                          rhs);
                pat = fun(pat);
                vs  = tl(vs);
                if (isAp(pat)) {
                    rhs = ap(showsCM,rhs);
                } else {
                    break;
                }
            }
        }
        rhs = ap2(nameComp,ap(nameApp,mkStr(name(h).text)),ap(showsOB,rhs));
        return rhs;
    }
    else if (a==0) {
        /* To display a nullary constructor:
         *    showsPrec d Foo = showString "Foo"
         */
        return ap(nameApp,mkStr(name(h).text));
    } else {
        Syntax s = syntaxOf(h);
        if (a==2 && assocOf(s)!=APPLIC) {
            /* For a binary constructor with prec p:
             * showsPrec d (a :* b) = showParen (d > p)
             *                          (showsPrec lp a . showChar ' ' .
             *                           showsString s  . showChar ' ' .
             *                           showsPrec rp b)
             */
            Int  p   = precOf(s);
            Int  lp  = (assocOf(s)==LEFT_ASS)  ? p : (p+1);
            Int  rp  = (assocOf(s)==RIGHT_ASS) ? p : (p+1);
            Cell rhs = ap(showsSP,ap2(nameShowsPrec,mkInt(rp),arg(pat)));
            if (defaultSyntax(name(h).text)==APPLIC) {
                rhs = ap(showsBQ,
                         ap2(nameComp,
                             ap(nameApp,mkStr(name(h).text)),
                             ap(showsBQ,rhs)));
            } else {
                rhs = ap2(nameComp,ap(nameApp,mkStr(name(h).text)),rhs);
            }

            rhs = ap2(nameComp,
                      ap2(nameShowsPrec,mkInt(lp),arg(fun(pat))),
                      ap(showsSP,rhs));
            rhs = ap2(nameShowParen,ap2(nameLe,mkInt(p+1),d),rhs);
            return rhs;
        }
        else {
            /* To display a non-nullary constructor with applicative syntax:
             *    showsPrec d (Foo x y) = showParen (d>=10)
             *                             (showString "Foo" .
             *                              showChar ' ' . showsPrec 10 x .
             *                              showChar ' ' . showsPrec 10 y)
             */
            Cell rhs = ap(showsSP,ap(shows10,arg(pat)));
            for (pat=fun(pat); isAp(pat); pat=fun(pat)) {
                rhs = ap(showsSP,ap2(nameComp,ap(shows10,arg(pat)),rhs));
            }
            rhs = ap2(nameComp,ap(nameApp,mkStr(name(h).text)),rhs);
            rhs = ap2(nameShowParen,ap2(nameLe,mkInt(10),d),rhs);
            return rhs;
        }
    }
}
#undef  shows10
#undef  shows0
#undef  showsOP
#undef  showsOB
#undef  showsCM
#undef  showsSP
#undef  showsBQ
#undef  showsCP
#undef  showsCB

/* --------------------------------------------------------------------------
 * Deriving Read:
 * ------------------------------------------------------------------------*/

#define Tuple2(f,s)      ap2(mkTuple(2),f,s)
#define Lex(r)           ap(nameLex,r)  
#define ZFexp(h,q)       ap(FROMQUAL, pair(h,q))
#define ReadsPrec(n,e)   ap2(nameReadsPrec,n,e)
#define Lambda(v,e)      ap(LAMBDA,pair(v, pair(mkInt(0),e)))
#define ReadParen(a,b,c) ap(ap2(nameReadParen,a,b),c)
#define ReadField(f,s)   ap2(nameReadField,f,s)
#define GT(l,r)          ap2(nameGt,l,r)
#define Append(a,b)      ap2(nameApp,a,b)      

/*  Construct the readsPrec function of the form:
 *
 *    readsPrec d r = (readParen (d>p1) (\r -> [ (C1 ...,s) | ... ]) r ++
 *                    (readParen (d>p2) (\r -> [ (C2 ...,s) | ... ]) r ++
 *                    ...
 *                    (readParen (d>pn) (\r -> [ (Cn ...,s) | ... ]) r) ... ))
 */
List deriveRead(t)              /* construct definition of text reader     */
Cell t; {
    Cell alt  = NIL;
    Cell exp  = NIL;
    Cell d    = inventVar();
    Cell r    = inventVar();
    List pat  = cons(d,cons(r,NIL));
    Int  line = 0;

    if (isTycon(t)) {
        List cs = tycon(t).defn;
        List exps = NIL;
        for (; hasCfun(cs); cs=tl(cs)) {
            exps = cons(mkReadCon(hd(cs),d,r),exps);
        }
        /* reverse concatenate list of subexpressions */
        exp = hd(exps);
        for (exps=tl(exps); nonNull(exps); exps=tl(exps)) {
            exp = ap2(nameApp,hd(exps),exp);
        }
        line = tycon(t).line;
    }
    else { /* Tuples */
        exp = ap(mkReadTuple(t),r);
    }
    /* printExp(stdout,exp); putc('\n',stdout); */
    alt  = pair(pat,pair(mkInt(line),exp)); 
    return singleton(mkBind("readsPrec",singleton(alt)));
}

/* Generate an expression of the form:
 *
 *   readParen (d > p) <derived expression> r
 *
 * for a (non-tuple) constructor "con" of precedence "p".
 */

static Cell local mkReadCon(con, d, r) /* generate reader for a constructor */
Name con;
Cell d;
Cell r; {
    Cell exp = NIL;
    Int  p   = 0;
    Syntax s = syntaxOf(con);
    List cfs = cfunSfuns;
    for (; nonNull(cfs) && con!=fst(hd(cfs)); cfs=tl(cfs)) {
    }
    if (nonNull(cfs)) {
        exp = mkReadRecord(con,snd(hd(cfs)));
        return ReadParen(nameFalse, exp, r);
    }

    if (userArity(con)==2 && assocOf(s)!=APPLIC) {
        exp = mkReadInfix(con);
        p   = precOf(s);
    } else {
        exp = mkReadPrefix(con);
        p   = 9;
    }
    return ReadParen(userArity(con)==0 ? nameFalse : GT(d,mkInt(p)), exp, r);
}

/* Given an n-ary prefix constructor, generate a single lambda
 * expression, such that
 *
 *   data T ... = Constr a1 a2 .. an | ....
 *
 * derives 
 *
 *   \ r -> [ (Constr t1 t2 ... tn, sn) | ("Constr",s0) <- lex r,
 *                                        (t1,s1) <- readsPrec 10 s0,
 *                                        (t2,s2) <- readsPrec 10 s1,
 *                                        ...,
 *                                        (tn,sn) <- readsPrec 10 sn-1 ]
 *
 */
static Cell local mkReadPrefix(con)    /* readsPrec for prefix constructor */
Cell con; {
    Int  arity  = userArity(con);
    Cell cn     = mkStr(name(con).text);
    Cell r      = inventVar();
    Cell prev_s = inventVar();
    Cell exp    = con;
    List quals  = NIL;
    Int  i;

    /* build (reversed) list of qualifiers and constructor */
    quals = cons(ZFexp(Tuple2(cn,prev_s),Lex(r)),quals);
    for(i=0; i<arity; i++) { 
        Cell t = inventVar();
        Cell s = inventVar();
        quals  = cons(ZFexp(Tuple2(t,s),ReadsPrec(mkInt(10),prev_s)), quals);
        exp    = ap(exp,t);
        prev_s = s;
    }

    /* \r -> [ (exp, prev_s) | quals ] */
    return Lambda(singleton(r),ap(COMP,pair(Tuple2(exp, prev_s), rev(quals))));
}

/* Given a binary infix constructor of precedence p
 *
 *   ... | T1 `con` T2 | ...
 * 
 * generate the lambda expression
 *
 *   \ r -> [ (u `con` v, s2) | (u,s0)     <- readsPrec lp r,
 *                              ("con",s1) <- lex s0,
 *                              (v,s2)     <- readsPrec rp s1 ]
 *
 * where lp and rp are either p or p+1 depending on associativity
 */
static Cell local mkReadInfix( con )
Cell con;
{
    Syntax s  = syntaxOf(con);
    Int    p  = precOf(s); 
    Int    lp = assocOf(s)==LEFT_ASS  ? p : (p+1);
    Int    rp = assocOf(s)==RIGHT_ASS ? p : (p+1);
    Cell   cn = mkStr(name(con).text);  
    Cell   r  = inventVar();
    Cell   s0 = inventVar();
    Cell   s1 = inventVar();
    Cell   s2 = inventVar();
    Cell   u  = inventVar();
    Cell   v  = inventVar();
    List quals = NIL;

    quals = cons(ZFexp(Tuple2(u, s0), ReadsPrec(mkInt(lp),r)),  quals);
    quals = cons(ZFexp(Tuple2(cn,s1), Lex(s0)),                 quals);
    quals = cons(ZFexp(Tuple2(v, s2), ReadsPrec(mkInt(rp),s1)), quals);

    return Lambda(singleton(r), 
                  ap(COMP,pair(Tuple2(ap2(con,u,v),s2),rev(quals))));
}

/* Given the n-ary tuple constructor return a lambda expression:
 *
 *   \ r -> [ ((t1,t2,...tn),s(2n+1)) | ("(",s0)      <- lex r,
 *                                      (t1, s1)      <- readsPrec 0 s0,
 *                                      ...
 *                                      (",",s(2n-1)) <- lex s(2n-2),
 *                                      (tn, s(2n))   <- readsPrec 0 s(2n-1),
 *                                      (")",s(2n+1)) <- lex s(2n) ]
 */
static Cell local mkReadTuple( tup ) /* readsPrec for n-tuple */
Cell tup; {
    Int  arity  = tupleOf(tup);
    Cell lp     = mkStr(findText("("));
    Cell rp     = mkStr(findText(")"));
    Cell co     = mkStr(findText(","));
    Cell sep    = lp;
    Cell r      = inventVar();
    Cell prev_s = r;
    Cell s      = inventVar();
    Cell exp    = tup;
    List quals  = NIL;
    Int  i;

    /* build (reversed) list of qualifiers and constructor */
    for(i=0; i<arity; i++) { 
        Cell t  = inventVar();
        Cell si = inventVar();
        Cell sj = inventVar();
        quals  = cons(ZFexp(Tuple2(sep,si),Lex(prev_s)),quals); 
        quals  = cons(ZFexp(Tuple2(t,sj),ReadsPrec(mkInt(0),si)), quals);
        exp    = ap(exp,t);
        prev_s = sj;
        sep    = co;
    }
    quals = cons(ZFexp(Tuple2(rp,s),Lex(prev_s)),quals);

    /* \ r -> [ (exp,s) | quals ] */
    return Lambda(singleton(r),ap(COMP,pair(Tuple2(exp,s),rev(quals))));
}

/* Given a record constructor 
 *
 *   ... | C { f1 :: T1, ... fn :: Tn } | ...
 *
 * generate the expression:
 *
 *   \ r -> [(C t1 t2 ... tn,s(2n+1)) | ("C", s0)    <- lex r,
 *                                      ("{", s1)    <- lex s0,
 *                                      (t1,  s2)    <- readField "f1" s1,
 *                                      ...
 *                                      (",", s(2n-1)) <- lex s(2n),
 *                                      (tn,  s(2n)) <- readField "fn" s(2n+1),
 *                                      ("}", s(2n+1)) <- lex s(2n+2) ]
 *
 * where
 *
 *   readField    :: Read a => String -> ReadS a
 *   readField m s0 = [ r | (t,  s1) <- lex s0, t == m,
 *                          ("=",s2) <- lex s1,
 *                          r        <- readsPrec 10 s2 ]
 */
static Cell local mkReadRecord(con, fs) /* readsPrec for record constructor */
Cell con; 
List fs; {
    Cell cn     = mkStr(name(con).text);  
    Cell lb     = mkStr(findText("{"));
    Cell rb     = mkStr(findText("}"));
    Cell co     = mkStr(findText(","));
    Cell sep    = lb;
    Cell r      = inventVar();
    Cell s0     = inventVar();
    Cell prev_s = s0;
    Cell s      = inventVar();
    Cell exp    = con;
    List quals  = NIL;

    /* build (reversed) list of qualifiers and constructor */
    quals  = cons(ZFexp(Tuple2(cn,s0),Lex(r)), quals); 
    for(; nonNull(fs); fs=tl(fs)) { 
        Cell f  = mkStr(textOf(hd(fs))); 
        Cell t  = inventVar();
        Cell si = inventVar();
        Cell sj = inventVar();
        quals  = cons(ZFexp(Tuple2(sep,si),Lex(prev_s)),     quals); 
        quals  = cons(ZFexp(Tuple2(t,  sj),ReadField(f,si)), quals);
        exp    = ap(exp,t);
        prev_s = sj;
        sep    = co;
    }
    quals = cons(ZFexp(Tuple2(rb,s),Lex(prev_s)),quals);

    /* \ r -> [ (exp,s) | quals ] */
    return Lambda(singleton(r),ap(COMP,pair(Tuple2(exp,s),rev(quals))));
}

#undef Tuple2
#undef Lex
#undef ZFexp
#undef ReadsPrec
#undef Lambda
#undef ReadParen
#undef ReadField
#undef GT
#undef Append

/* --------------------------------------------------------------------------
 * Deriving Bounded:
 * ------------------------------------------------------------------------*/

#if DERIVE_BOUNDED

List deriveBounded(t)             /* construct definition of bounds        */
Tycon t; {
    if (isEnumType(t)) {
        Cell last  = tycon(t).defn;
        Cell first = hd(last);
        while (hasCfun(tl(last))) {
            last = tl(last);
        }
        return cons(mkBind("minBound",mkVarAlts(tycon(t).line,first)),
                cons(mkBind("maxBound",mkVarAlts(tycon(t).line,hd(last))),
                 NIL));
    } else if (isTuple(t)) {    /* Definitions for product types           */
        return mkBndBinds(0,t,tupleOf(t));
    } else if (isTycon(t) && cfunOf(hd(tycon(t).defn))==0) {
        return mkBndBinds(tycon(t).line,
                          hd(tycon(t).defn),
                          userArity(hd(tycon(t).defn)));
    }
    ERRMSG(tycon(t).line)
     "Can only derive instances of Bounded for enumeration and product types"
    EEND;
    return NIL;
}

static List local mkBndBinds(line,h,n)  /* build bindings for derived      */
Int  line;                              /* Bounded on a product type       */
Cell h;
Int  n; {
    Cell minB = h;
    Cell maxB = h;
    while (n-- > 0) {
        minB = ap(minB,nameMinBnd);
        maxB = ap(maxB,nameMaxBnd);
    }
    return cons(mkBind("minBound",mkVarAlts(line,minB)),
            cons(mkBind("maxBound",mkVarAlts(line,maxB)),
             NIL));
}
#endif /* DERIVE_BOUNDED */


/* --------------------------------------------------------------------------
 * Derivation control:
 * ------------------------------------------------------------------------*/

Void deriveControl(what)
Int what; {
    Text textPrelude = findText("PreludeBuiltin");
    switch (what) {
        case INSTALL :
                varTrue           = mkQVar(textPrelude,findText("True"));
                varFalse          = mkQVar(textPrelude,findText("False"));
#if DERIVE_ORD
                varCompAux        = mkQVar(textPrelude,findText("primCompAux"));
                varCompare        = mkQVar(textPrelude,findText("compare"));
                varEQ             = mkQVar(textPrelude,findText("EQ"));
#endif
#if DERIVE_IX   
                varRangeSize      = mkQVar(textPrelude,findText("rangeSize"));
                varInRange        = mkQVar(textPrelude,findText("inRange"));
                varRange          = mkQVar(textPrelude,findText("range"));
                varIndex          = mkQVar(textPrelude,findText("index"));
                varMult           = mkQVar(textPrelude,findText("*"));
                varPlus           = mkQVar(textPrelude,findText("+"));
                varMap            = mkQVar(textPrelude,findText("map"));
                varMinus          = mkQVar(textPrelude,findText("-"));
                varError          = mkQVar(textPrelude,findText("error"));
#endif
#if DERIVE_ENUM
                varToEnum         = mkQVar(textPrelude,findText("toEnum"));
                varFromEnum       = mkQVar(textPrelude,findText("fromEnum"));  
                varEnumFromTo     = mkQVar(textPrelude,findText("enumFromTo"));      
                varEnumFromThenTo = mkQVar(textPrelude,findText("enumFromThenTo"));  
#endif
#if DERIVE_BOUNDED
                varMinBound       = mkQVar(textPrelude,findText("minBound"));
                varMaxBound       = mkQVar(textPrelude,findText("maxBound"));
#endif
#if DERIVE_SHOW 
                conCons           = mkQCon(textPrelude,findText(":"));
                varShowField      = mkQVar(textPrelude,findText("primShowField"));
                varShowParen      = mkQVar(textPrelude,findText("showParen"));
                varCompose        = mkQVar(textPrelude,findText("."));
                varShowsPrec      = mkQVar(textPrelude,findText("showsPrec"));
                varLe             = mkQVar(textPrelude,findText("<="));
#endif          
#if DERIVE_READ
                varReadField      = mkQVar(textPrelude,findText("primReadField"));
                varReadParen      = mkQVar(textPrelude,findText("readParen"));
                varLex            = mkQVar(textPrelude,findText("lex"));
                varReadsPrec      = mkQVar(textPrelude,findText("readsPrec"));
                varGt             = mkQVar(textPrelude,findText(">"));
#endif
#if DERIVE_SHOW || DERIVE_READ         
                varAppend         = mkQVar(textPrelude,findText("++"));
#endif                                 
#if DERIVE_EQ || DERIVE_IX             
                varAnd            = mkQVar(textPrelude,findText("&&"));
#endif
#if DERIVE_EQ || DERIVE_ORD            
                varEq             = mkQVar(textPrelude,findText("=="));
#endif
                /* deliberate fall through */
        case RESET   : 
                diVars      = NIL;
                diNum       = 0;
#if DERIVE_SHOW | DERIVE_READ
                cfunSfuns   = NIL;
#endif
                break;

        case MARK    : 
                mark(diVars);
#if DERIVE_SHOW | DERIVE_READ
                mark(cfunSfuns);
#endif
                mark(varTrue);        
                mark(varFalse);        
#if DERIVE_ORD
                mark(varCompAux);        
                mark(varCompare);        
                mark(varEQ);        
#endif                            
#if DERIVE_IX                     
                mark(varRangeSize);      
                mark(varInRange);        
                mark(varRange);          
                mark(varIndex);          
                mark(varMult);           
                mark(varPlus);           
                mark(varMap);           
                mark(varMinus);           
                mark(varError);           
#endif                            
#if DERIVE_ENUM                   
                mark(varToEnum); 
                mark(varFromEnum);   
                mark(varEnumFromTo);     
                mark(varEnumFromThenTo);   
#endif                            
#if DERIVE_BOUNDED                
                mark(varMinBound);       
                mark(varMaxBound);       
#endif                            
#if DERIVE_SHOW                   
                mark(conCons);
                mark(varShowField);      
                mark(varShowParen);      
                mark(varCompose);        
                mark(varShowsPrec);      
                mark(varLe);             
#endif                            
#if DERIVE_READ                   
                mark(varReadField);      
                mark(varReadParen);      
                mark(varLex);            
                mark(varReadsPrec);      
                mark(varGt);             
#endif                            
#if DERIVE_SHOW || DERIVE_READ    
                mark(varAppend);         
#endif                            
#if DERIVE_EQ || DERIVE_IX        
                mark(varAnd);            
#endif                            
#if DERIVE_EQ || DERIVE_ORD       
                mark(varEq);             
#endif
                break;
    }
}

/*-------------------------------------------------------------------------*/
