/* -*- mode: hugs-c; -*- */
/* --------------------------------------------------------------------------
 * Static Analysis for Hugs
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: static.c,v $
 * $Revision: 1.2 $
 * $Date: 1998/12/02 13:22:35 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "input.h"
#include "type.h"
#include "static.h"
#include "translate.h"
#include "hugs.h"  /* for target */
#include "errors.h"
#include "subst.h"
#include "link.h"
#include "modules.h"
#include "derive.h"

/* --------------------------------------------------------------------------
 * local function prototypes:
 * ------------------------------------------------------------------------*/

static Module thisModule = 0;           /* module currently being processed*/

static Void  local kindError         Args((Int,Constr,Constr,String,Kind,Int));

static Void  local checkTyconDefn    Args((Tycon));
static Void  local depConstrs        Args((Tycon,List,Cell));
static List  local addSels           Args((Int,Name,List,List));
static List  local selectCtxt        Args((List,List));
static Void  local checkSynonyms     Args((List));
static List  local visitSyn          Args((List,Tycon,List));
#if EVAL_INSTANCES
static Void  local deriveEval        Args((List));
static List  local calcEvalContexts  Args((Tycon,List,List));
#endif
static Void  local checkBanged       Args((Name,Kinds,List,Type));
static Type  local instantiateSyn    Args((Type,Type));

static Void  local checkClassDefn    Args((Class));
static Void  local depPredExp        Args((Int,List,Cell));
static Void  local checkMems         Args((Class,List,Cell));
static Void  local addMembers        Args((Class));
static Name  local newMember         Args((Int,Int,Cell,Type));
static Name  local newDSel           Args((Class,Int));
static Name  local newDBuild         Args((Class));
static Text  local generateText      Args((String, Class));
static Int   local visitClass        Args((Class));

static List  local classBindings     Args((String,Class,List));
static Name  local memberName        Args((Class,Text));
static List  local numInsert         Args((Int,Cell,List));

static List  local typeVarsIn        Args((Cell,List,List));
static List  local maybeAppendVar    Args((Cell,List));

static Type  local checkSigType      Args((Int,String,Cell,Type));
static Type  local depTopType        Args((Int,List,Type));
static Type  local depCompType       Args((Int,List,Type));
static Type  local depTypeExp        Args((Int,List,Type));
static Type  local depTypeVar        Args((Int,List,Text));
static Void  local kindConstr        Args((Int,Int,Int,Constr));
static Kind  local kindAtom          Args((Int,Constr));
static Void  local kindPred          Args((Int,Int,Int,Cell));
static Void  local kindType          Args((Int,String,Type));
static Void  local fixKinds          Args((Void));

static Void  local kindTCGroup       Args((List));
static Void  local initTCKind        Args((Cell));
static Void  local kindTC            Args((Cell));
static Void  local genTC             Args((Cell));

static Void  local checkInstDefn     Args((Inst));
static Void  local insertInst        Args((Inst));
static Bool  local instCompare       Args((Inst,Inst));
static Name  local newInstImp        Args((Inst));
static Void  local kindInst          Args((Inst,Int));
static Void  local checkDerive       Args((Tycon,List,List,Cell));
static Void  local addDerInst        Args((Int,Class,List,List,Type,Int));

static Void  local deriveContexts    Args((List));
static Void  local initDerInst       Args((Inst));
static Void  local calcInstPreds     Args((Inst));
static Void  local maybeAddPred      Args((Cell,Int,Int,List));
static Cell  local copyAdj           Args((Cell,Int,Int));
static Void  local tidyDerInst       Args((Inst));

static Void  local addDerivImp       Args((Inst));

static Void  local checkDefaultDefns Args((Void));

static Void  local checkForeignImport Args((Name));
static Void  local checkForeignExport Args((Name));

static Cell  local checkPat          Args((Int,Cell));
static Cell  local checkMaybeCnkPat  Args((Int,Cell));
static Cell  local checkApPat        Args((Int,Int,Cell));
static Void  local addPatVar         Args((Int,Cell));
static Name  local conDefined        Args((Int,Cell));
static Void  local checkIsCfun       Args((Int,Name));
static Void  local checkCfunArgs     Args((Int,Cell,Int));
static Cell  local applyBtyvs        Args((Cell));
static Cell  local bindPat           Args((Int,Cell));
static Void  local bindPats          Args((Int,List));

static List  local extractSigdecls   Args((List));
static List  local extractBindings   Args((List));
static List  local eqnsToBindings    Args((List));
static Void  local notDefined        Args((Int,List,Cell));
static Cell  local findBinding       Args((Text,List));
static Void  local addSigDecl        Args((List,Cell));
static Void  local setType           Args((Int,Cell,Cell,List));

static List  local dependencyAnal    Args((List));
static List  local topDependAnal     Args((List));
static Void  local addDepField       Args((Cell));
static Void  local remDepField       Args((List));
static Void  local remDepField1      Args((Cell));
static Void  local clearScope        Args((Void));
static Void  local withinScope       Args((List));
static Void  local leaveScope        Args((Void));

static Void  local depBinding        Args((Cell));
static Void  local depDefaults       Args((Class));
static Void  local depInsts          Args((Inst));
static Void  local depClassBindings  Args((List));
static Void  local depAlt            Args((Cell));
static Void  local depRhs            Args((Cell));
static Void  local depGuard          Args((Cell));
static Cell  local depExpr           Args((Int,Cell));
static Void  local depPair           Args((Int,Cell));
static Void  local depTriple         Args((Int,Cell));
static Void  local depComp           Args((Int,Cell,List));
static Void  local depCaseAlt        Args((Int,Cell));
static Cell  local depVar            Args((Int,Cell));
static Cell  local depQVar           Args((Int,Cell));
static Void  local depConFlds        Args((Int,Cell,Bool));
static Void  local depUpdFlds        Args((Int,Cell));
static List  local depFields         Args((Int,Cell,List,Bool));
#if TREX
static Cell  local depRecord         Args((Int,Cell));
#endif

static List  local tcscc             Args((List,List));
static List  local bscc              Args((List));

static Void  local addRSsigdecls     Args((Pair));
static Void  local opDefined         Args((List,Cell));
static Void  local allNoPrevDef      Args((Cell));
static Void  local noPrevDef         Args((Int,Cell));
static Void  local duplicateError       Args((Int,Module,Text,String));
static Void  local checkTypeIn       Args((Pair));

/* --------------------------------------------------------------------------
 * The code in this file is arranged in roughly the following order:
 *  - Kind inference preliminaries
 *  - Type declarations (data, type, newtype, type in)
 *  - Class declarations
 *  - Type signatures
 *  - Instance declarations
 *  - Default declarations
 *  - Patterns
 *  - Value definitions
 *  - Top-level static analysis and control
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Kind checking preliminaries:
 * ------------------------------------------------------------------------*/

Bool kindExpert = FALSE;                /* TRUE => display kind errors in  */
                                        /*         full detail             */

static Void local kindError(l,c,in,wh,k,o)
Int    l;                               /* line number near constuctor exp */
Constr c;                               /* constructor                     */
Constr in;                              /* context (if any)                */
String wh;                              /* place in which error occurs     */
Kind   k;                               /* expected kind (k,o)             */
Int    o; {                             /* inferred kind (typeIs,typeOff)  */
    clearMarks();

    if (!kindExpert) {                  /* for those with a fear of kinds  */
        ERRMSG(l) "Illegal type" ETHEN
        if (nonNull(in)) {
            ERRTEXT " \"" ETHEN ERRTYPE(in);
            ERRTEXT "\""  ETHEN
        }
        ERRTEXT " in %s\n", wh
        EEND;
    }

    ERRMSG(l) "Kind error in %s", wh ETHEN
    if (nonNull(in)) {
        ERRTEXT "\n*** expression     : " ETHEN ERRTYPE(in);
    }
    ERRTEXT "\n*** constructor    : " ETHEN ERRTYPE(c);
    ERRTEXT "\n*** kind           : " ETHEN ERRKIND(copyType(typeIs,typeOff));
    ERRTEXT "\n*** does not match : " ETHEN ERRKIND(copyType(k,o));
    if (unifyFails) {
        ERRTEXT "\n*** because        : %s", unifyFails ETHEN
    }
    ERRTEXT "\n"
    EEND;
}

#define shouldKind(l,c,in,wh,k,o)       if (!kunify(typeIs,typeOff,k,o)) \
                                            kindError(l,c,in,wh,k,o)
#define checkKind(l,a,m,c,in,wh,k,o)    kindConstr(l,a,m,c); \
                                        shouldKind(l,c,in,wh,k,o)
#define inferKind(k,o)                  typeIs=k; typeOff=o

static List unkindTypes;                /* types in need of kind annotation*/
#if TREX
Kind   extKind;                         /* Kind of extension, *->row->row  */
#endif

/* --------------------------------------------------------------------------
 * Static analysis of type declarations:
 *
 * Type declarations come in two forms:
 * - data declarations - define new constructed data types
 * - type declarations - define new type synonyms
 *
 * A certain amount of work is carried out as the declarations are
 * read during parsing.  In particular, for each type constructor
 * definition encountered:
 * - check that there is no previous definition of constructor
 * - ensure type constructor not previously used as a class name
 * - make a new entry in the type constructor table
 * - record line number of declaration
 * - Build separate lists of newly defined constructors for later use.
 * ------------------------------------------------------------------------*/

Void tyconDefn(line,lhs,rhs,what)       /* process new type definition     */
Int  line;                              /* definition line number          */
Cell lhs;                               /* left hand side of definition    */
Cell rhs;                               /* right hand side of definition   */
Cell what; {                            /* SYNONYM/DATATYPE/etc...         */
    Text t = textOf(getHead(lhs));

    if (nonNull(findTycon(t))) {
        ERRMSG(line) "Repeated definition of type constructor \"%s\"",
                     textToStr(t)
        EEND;
    }
    else if (nonNull(findClass(t))) {
        ERRMSG(line) "\"%s\" used as both class and type constructor",
                     textToStr(t)
        EEND;
    }
    else {
        Tycon nw        = newTycon(t);
        tyconDefns      = cons(nw,tyconDefns);
        tycon(nw).line  = line;
        tycon(nw).arity = argCount;
        tycon(nw).what  = what;
        if (what==RESTRICTSYN) {
            typeInDefns = cons(pair(nw,snd(rhs)),typeInDefns);
            rhs         = fst(rhs);
        }
        tycon(nw).defn  = pair(lhs,rhs);
    }
}

Void setTypeIns(bs)                     /* set local synonyms for given    */
List bs; {                              /* binding group                   */
    List cvs = typeInDefns;
    for (; nonNull(cvs); cvs=tl(cvs)) {
        Tycon c  = fst(hd(cvs));
        List  vs = snd(hd(cvs));
        for (tycon(c).what = RESTRICTSYN; nonNull(vs); vs=tl(vs)) {
            if (nonNull(findBinding(textOf(hd(vs)),bs))) {
                tycon(c).what = SYNONYM;
                break;
            }
        }
    }
}

Void clearTypeIns() {                   /* clear list of local synonyms    */
    for (; nonNull(typeInDefns); typeInDefns=tl(typeInDefns))
        tycon(fst(hd(typeInDefns))).what = RESTRICTSYN;
}

/* --------------------------------------------------------------------------
 * Further analysis of Type declarations:
 *
 * In order to allow the definition of mutually recursive families of
 * data types, the static analysis of the right hand sides of type
 * declarations cannot be performed until all of the type declarations
 * have been read.
 *
 * Once parsing is complete, we carry out the following:
 *
 * - check format of lhs, extracting list of bound vars and ensuring that
 *   there are no repeated variables and no Skolem variables.
 * - run dependency analysis on rhs to check that only bound type vars
 *   appear in type and that all constructors are defined.
 *   Replace type variables by offsets, constructors by Tycons.
 * - use list of dependents to sort into strongly connected components.
 * - ensure that there is not more than one synonym in each group.
 * - kind-check each group of type definitions.
 *
 * - check that there are no previous definitions for constructor
 *   functions in data type definitions.
 * - install synonym expansions and constructor definitions.
 * ------------------------------------------------------------------------*/

static List tcDeps = NIL;               /* list of dependent tycons/classes*/

static Void local checkTyconDefn(d)     /* validate type constructor defn  */
Tycon d; {
    Cell lhs    = fst(tycon(d).defn);
    Cell rhs    = snd(tycon(d).defn);
    Int  line   = tycon(d).line;
    List tyvars = getArgs(lhs);
    List temp;
                                        /* check for repeated tyvars on lhs*/
    for (temp=tyvars; nonNull(temp); temp=tl(temp))
        if (nonNull(varIsMember(textOf(hd(temp)),tl(temp)))) {
            ERRMSG(line) "Repeated type variable \"%s\" on left hand side",
                         textToStr(textOf(hd(temp)))
            EEND;
        }

    tcDeps = NIL;                       /* find dependents                 */
    switch (whatIs(tycon(d).what)) {
        case RESTRICTSYN :
        case SYNONYM     : rhs = depTypeExp(line,tyvars,rhs);
                           if (cellIsMember(d,tcDeps)) {
                               ERRMSG(line) "Recursive type synonym \"%s\"",
                                            textToStr(tycon(d).text)
                               EEND;
                           }
                           break;

        case DATATYPE    :
        case NEWTYPE     : depConstrs(d,tyvars,rhs);
                           rhs = fst(rhs);
                           break;

        default          : internal("checkTyconDefn");
                           break;
    }

    tycon(d).defn = rhs;
    tycon(d).kind = tcDeps;
    tcDeps        = NIL;
}

static Void local depConstrs(t,tyvars,cd)
Tycon t;                                /* Define constructor functions and*/
List  tyvars;                           /* do dependency analysis for data */
Cell  cd; {                             /* definitions (w or w/o deriving) */
    Int  line      = tycon(t).line;
    List ctxt      = NIL;
    Int  conNo     = 1;
    Type lhs       = t;
    List cs        = fst(cd);
    List derivs    = snd(cd);
    List compTypes = NIL;
    List sels      = NIL;
    Int  ntvs      = length(tyvars);
    Int  i;

    for (i=0; i<tycon(t).arity; ++i)    /* build representation for tycon  */
        lhs = ap(lhs,mkOffset(i));      /* applied to full comp. of args   */

    if (whatIs(cs)==QUAL) {             /* allow for possible context      */
        ctxt = fst(snd(cs));
        cs   = snd(snd(cs));
        map2Proc(depPredExp,line,tyvars,ctxt);
    }

    if (nonNull(cs) && isNull(tl(cs)))  /* Single constructor datatype?    */
        conNo = 0;

    for (; nonNull(cs); cs=tl(cs)) {    /* For each constructor function:  */
        Cell con   = hd(cs);
        List sig   = typeVarsIn(con,NIL,dupList(tyvars));
        Int  etvs  = length(sig);
        List ctxt1 = ctxt;              /* constructor function context    */
        List scs   = NIL;               /* strict components               */
        List fs    = NONE;              /* selector names                  */
        Type type  = lhs;               /* constructor function type       */
        Int  arity = 0;                 /* arity of constructor function   */
        Int  nr2   = 0;                 /* Number of rank 2 args           */
        Name n;                         /* name for constructor function   */

        if (whatIs(con)==LABC) {        /* Skeletize constr components     */
            Cell fls = snd(snd(con));   /* get field specifications        */
            con      = fst(snd(con));
            fs       = NIL;
            for (; nonNull(fls); fls=tl(fls)) { /* for each field spec:    */
                List vs     = fst(hd(fls));
                Type t      = snd(hd(fls));     /* - scrutinize type       */
                Bool banged = whatIs(t)==BANG;
                t           = depCompType(line,sig,(banged ? arg(t) : t));
                while (nonNull(vs)) {           /* - add named components  */
                    Cell us = tl(vs);
                    tl(vs)  = fs;
                    fs      = vs;
                    vs      = us;
                    con     = ap(con,t);
                    arity++;
                    if (banged)
                        scs = cons(mkInt(arity),scs);
                }
            }
            fs  = rev(fs);
            scs = rev(scs);             /* put strict comps in ascend ord  */
        }
        else {                          /* Non-labelled constructor        */
            Cell c = con;
            Int  compNo;
            for (; isAp(c); c=fun(c))
                arity++;
            for (compNo=arity, c=con; isAp(c); c=fun(c)) {
                Type t = arg(c);
                if (whatIs(t)==BANG) {
                    scs = cons(mkInt(compNo),scs);
                    t   = arg(t);
                }
                compNo--;
                arg(c) = depCompType(line,sig,t);
            }
        }

        if (nonNull(ctxt1))             /* Extract relevant part of context*/
            ctxt1 = selectCtxt(ctxt1,offsetTyvarsIn(con,NIL));

        for (i=arity; isAp(con); i--) { /* Calculate type of constructor   */
            Type t   = fun(con);
            Type cmp = arg(con);
            fun(con) = typeArrow;
            if (isPolyType(cmp)) {
                if (nonNull(derivs)) {
                    ERRMSG(line) "Cannot derive instances for types" ETHEN
                    ERRTEXT      " with polymorphic components"
                    EEND;
                }
                if (nr2==0)
                    nr2 = i;
            }
            if (nonNull(derivs))        /* and build list of components    */
                compTypes = cons(cmp,compTypes);
            type     = ap(con,type);
            con      = t;
        }

        if (nr2>0)                      /* Add rank 2 annotation           */
            type = ap(RANK2,pair(mkInt(nr2),type));

        if (etvs>ntvs) {                /* Add existential annotation      */
            if (nonNull(derivs)) {
                ERRMSG(line) "Cannot derive instances for types" ETHEN
                ERRTEXT      " with existentially typed components"
                EEND;
            }
            if (fs!=NONE) {
                ERRMSG(line)
                   "Cannot use selectors with existentially typed components"
                EEND;
            }
            type = ap(EXIST,pair(mkInt(etvs-ntvs),type));
        }
        if (nonNull(ctxt1)) {           /* Add context part to type        */
            type = ap(QUAL,pair(ctxt1,type));
        }
        if (nonNull(sig)) {             /* Add quantifiers to type         */
            List ts1 = sig;
            for (; nonNull(ts1); ts1=tl(ts1)) {
                hd(ts1) = NIL;
            }
            type = mkPolyType(sig,type);
        }

        n = findName(textOf(con));      /* Allocate constructor fun name   */
        if (isNull(n)) {
            n = newName(textOf(con));
        } else if (name(n).defn!=PREDEFINED) {
            duplicateError(line,name(n).mod,name(n).text,
                           "constructor function");
        }
        name(n).arity  = arity;         /* Save constructor fun details    */
        name(n).line   = line;
        name(n).number = cfunNo(conNo++);
        name(n).type   = type;
        if (tycon(t).what==NEWTYPE) {
            name(n).defn = nameId;
        } else {
            implementCfun(n,scs);
        }
        hd(cs) = n;
        if (fs!=NONE) {
            sels = addSels(line,n,fs,sels);
        }
    }

    if (nonNull(sels)) {
        sels     = rev(sels);
        fst(cd)  = appendOnto(fst(cd),sels);
        selDefns = cons(sels,selDefns);
    }

    if (nonNull(derivs)) {              /* Generate derived instances      */
        map3Proc(checkDerive,t,ctxt,compTypes,derivs);
    }
}

static List local addSels(line,c,fs,ss) /* Add fields to selector list     */
Int  line;                              /* line number of constructor      */
Name c;                                 /* corresponding constr function   */
List fs;                                /* list of fields (varids)         */
List ss; {                              /* list of existing selectors      */
    Int sn    = 1;
#if DERIVE_SHOW | DERIVE_READ
    cfunSfuns = cons(pair(c,fs),cfunSfuns);
#endif
    for (; nonNull(fs); fs=tl(fs), ++sn) {
        List ns = ss;
        Text t  = textOf(hd(fs));

        if (nonNull(varIsMember(t,tl(fs)))) {
            ERRMSG(line) "Repeated field name \"%s\" for constructor \"%s\"",
                         textToStr(t), textToStr(name(c).text)
            EEND;
        }

        while (nonNull(ns) && t!=name(hd(ns)).text) {
            ns = tl(ns);
        }
        if (nonNull(ns)) {
            name(hd(ns)).defn = cons(pair(c,mkInt(sn)),name(hd(ns)).defn);
        } else {
            Name n = findName(t);
            if (nonNull(n)) {
                ERRMSG(line) "Repeated definition for selector \"%s\"",
                             textToStr(t)
                EEND;
            }
            n              = newName(t);
            name(n).line   = line;
            name(n).number = SELNAME;
            name(n).defn   = singleton(pair(c,mkInt(sn)));
            ss             = cons(n,ss);
        }
    }
    return ss;
}

static List local selectCtxt(ctxt,vs)   /* calculate subset of context     */
List ctxt;
List vs; {
    if (isNull(vs)) {
        return NIL;
    } else {
        List ps = NIL;
        for (; nonNull(ctxt); ctxt=tl(ctxt)) {
            List us = offsetTyvarsIn(hd(ctxt),NIL);
            for (; nonNull(us) && cellIsMember(hd(us),vs); us=tl(us)) {
            }
            if (isNull(us)) {
                ps = cons(hd(ctxt),ps);
            }
        }
        return rev(ps);
    }
}

static Void local checkSynonyms(ts)     /* Check for mutually recursive    */
List ts; {                              /* synonyms                        */
    List syns = NIL;
    for (; nonNull(ts); ts=tl(ts)) {    /* build list of all synonyms      */
        Tycon t = hd(ts);
        switch (whatIs(tycon(t).what)) {
            case SYNONYM     :
            case RESTRICTSYN : syns = cons(t,syns);
                               break;
        }
    }
    while (nonNull(syns)) {             /* then visit each synonym         */
        syns = visitSyn(NIL,hd(syns),syns);
    }
}

static List local visitSyn(path,t,syns) /* visit synonym definition to look*/
List  path;                             /* for cycles                      */
Tycon t;
List  syns; {
    if (cellIsMember(t,path)) {         /* every elt in path depends on t  */
        ERRMSG(tycon(t).line)
            "Type synonyms \"%s\" and \"%s\" are mutually recursive",
            textToStr(tycon(t).text), textToStr(tycon(hd(path)).text)
        EEND;
    } else {
        List ds    = tycon(t).kind;
        List path1 = NIL;
        for (; nonNull(ds); ds=tl(ds)) {
            if (cellIsMember(hd(ds),syns)) {
                if (isNull(path1))
                    path1 = cons(t,path);
                syns = visitSyn(path1,hd(ds),syns);
            }
        }
    }
    tycon(t).defn = fullExpand(tycon(t).defn);
    return removeCell(t,syns);
}

/* --------------------------------------------------------------------------
 * The following code is used in calculating contexts for the automatically
 * derived Eval instances for newtype and restricted type synonyms.  This is
 * ugly code, resulting from an ugly feature in the language, and I hope that
 * the feature, and hence the code, will be removed in the not too distant
 * future.
 * ------------------------------------------------------------------------*/

#if EVAL_INSTANCES
static Void local deriveEval(tcs)       /* Derive instances of Eval        */
List tcs; {
    List ts1 = tcs;
    List ts  = NIL;
    for (; nonNull(ts1); ts1=tl(ts1)) { /* Build list of rsyns and newtypes*/
        Tycon t = hd(ts1);              /* and derive instances for data   */
        switch (whatIs(tycon(t).what)) {
            case DATATYPE    : addEvalInst(tycon(t).line,t,tycon(t).arity,NIL);
                               break;
            case NEWTYPE     :
            case RESTRICTSYN : ts = cons(t,ts);
                               break;
        }
    }
    emptySubstitution();                /* then derive other instances     */
    while (nonNull(ts)) {
        ts = calcEvalContexts(hd(ts),tl(ts),NIL);
    }
    emptySubstitution();

    for (; nonNull(tcs); tcs=tl(tcs)) { /* Check any banged components     */
        Tycon t = hd(tcs);
        if (whatIs(tycon(t).what)==DATATYPE) {
            List cs = tycon(t).defn;
            for (; hasCfun(cs); cs=tl(cs)) {
                Name c = hd(cs);
                if (isPair(name(c).defn)) {
                    Type  t    = name(c).type;
                    List  scs  = fst(name(c).defn);
                    Kinds ks   = NIL;
                    List  ctxt = NIL;
                    Int   n    = 1;
                    if (isPolyType(t)) {
                        ks = polySigOf(t);
                        t  = monotypeOf(t);
                    }
                    if (whatIs(t)==QUAL) {
                        ctxt = fst(snd(t));
                        t    = snd(snd(t));
                    }
                    for (; nonNull(scs); scs=tl(scs)) {
                        Int i = intOf(hd(scs));
                        for (; n<i; n++) {
                            t = arg(t);
                        }
                        checkBanged(c,ks,ctxt,arg(fun(t)));
                    }
                }
            }
        }
    }
}

static List local calcEvalContexts(tc,ts,ps)
Tycon tc;                               /* Worker code for deriveEval      */
List  ts;                               /* ts = not visited, ps = visiting */
List  ps; {
    Cell ctxt = NIL;
    Int  o    = newKindedVars(tycon(tc).kind);
    Type t    = tycon(tc).defn;
    Int  i;

    if (whatIs(tycon(tc).what)==NEWTYPE) {
        t = name(hd(t)).type;
        if (isPolyType(t)) {
            t = monotypeOf(t);
        }
        if (whatIs(t)==QUAL) {
            t = snd(snd(t));
        }
        if (whatIs(t)==EXIST) {         /* No instance if existentials used*/
            return ts;
        }
        if (whatIs(t)==RANK2) {         /* No instance if arg is poly/qual */
            return ts;
        }
        t = arg(fun(t));
    }

    clearMarks();                       /* Make sure generics are marked   */
    for (i=0; i<tycon(tc).arity; i++) { /* in the correct order.           */
        copyTyvar(o+i);
    }

    for (;;) {
        Type h = getDerefHead(t,o);
        if (isSynonym(h) && argCount>=tycon(h).arity) {
            expandSyn(h,argCount,&t,&o);
        } else if (isOffset(h)) {               /* Stop if var at head     */
            ctxt = singleton(ap(classEval,copyType(t,o)));
            break;
        } else if (isTuple(h)                   /* Check for tuples ...    */
                || h==tc                        /* ... direct recursion    */
                || cellIsMember(h,ps)           /* ... mutual recursion    */
                || tycon(h).what==DATATYPE) {   /* ... or datatype.        */
            break;                              /* => empty context        */
        } else {
            Cell pi = ap(classEval,t);
            Inst in;

            if (cellIsMember(h,ts)) {           /* Not yet visited?        */
                ts = calcEvalContexts(h,removeCell(h,ts),cons(h,ts));
            }
            if (nonNull(in=findInstFor(pi,o))) {/* Look for Eval instance  */
                List qs = inst(in).specifics;
                Int  o1 = typeOff;
                if (isNull(qs)) {               /* No context there        */
                    break;                      /* => empty context here   */
                }
                if (isNull(tl(qs)) && classEval==fun(hd(qs))) {
                    t = arg(hd(qs));
                    o = o1;
                    continue;
                }
            }
            return ts;                          /* No instance, so give up */
        }
    }
    addEvalInst(tycon(tc).line,tc,tycon(tc).arity,ctxt);
    return ts;
}

static Void local checkBanged(c,ks,ps,ty)
Name  c;                                /* Check that banged component of c*/
Kinds ks;                               /* with type ty is an instance of  */
List  ps;                               /* Eval under the predicates in ps.*/
Type  ty; {                             /* (All types using ks)            */
    Cell pi = ap(classEval,ty);
    if (isNull(provePred(ks,ps,pi))) {
        ERRMSG(name(c).line) "Illegal datatype strictness annotation:" ETHEN
        ERRTEXT "\n*** Constructor : "  ETHEN ERREXPR(c);
        ERRTEXT "\n*** Context     : "  ETHEN ERRCONTEXT(ps);
        ERRTEXT "\n*** Required    : "  ETHEN ERRPRED(pi);
        ERRTEXT "\n"
        EEND;
    }
}
#endif

/* --------------------------------------------------------------------------
 * Expanding out all type synonyms in a type expression:
 * ------------------------------------------------------------------------*/

Type fullExpand(t)                      /* find full expansion of type exp */
Type t; {                               /* assuming that all relevant      */
    Cell h = t;                         /* synonym defns of lower rank have*/
    Int  n = 0;                         /* already been fully expanded     */
    List args;
    for (args=NIL; isAp(h); h=fun(h), n++) {
        args = cons(fullExpand(arg(h)),args);
    }
    t = applyToArgs(h,args);
    if (isSynonym(h) && n>=tycon(h).arity) {
        if (n==tycon(h).arity) {
            t = instantiateSyn(tycon(h).defn,t);
        } else {
            Type p = t;
            while (--n > tycon(h).arity) {
                p = fun(p);
            }
            fun(p) = instantiateSyn(tycon(h).defn,fun(p));
        }
    }
    return t;
}

static Type local instantiateSyn(t,env) /* instantiate type according using*/
Type t;                                 /* env to determine appropriate    */
Type env; {                             /* values for OFFSET type vars     */
    switch (whatIs(t)) {
        case AP      : return ap(instantiateSyn(fun(t),env),
                                 instantiateSyn(arg(t),env));

        case OFFSET  : return nthArg(offsetOf(t),env);

        default      : return t;
    }
}

/* --------------------------------------------------------------------------
 * Static analysis of class declarations:
 *
 * Performed in a similar manner to that used for type declarations.
 *
 * The first part of the static analysis is performed as the declarations
 * are read during parsing.  The parser ensures that:
 * - the class header and all superclass predicates are of the form
 *   ``Class var''
 *
 * The classDefn() function:
 * - ensures that there is no previous definition for class
 * - checks that class name has not previously been used as a type constr.
 * - make new entry in class table
 * - record line number of declaration
 * - build list of classes defined in current script for use in later
 *   stages of static analysis.
 * ------------------------------------------------------------------------*/

Void classDefn(line,head,ms)            /* process new class definition    */
Int  line;                              /* definition line number          */
Cell head;                              /* class header :: ([Supers],Class)*/
List ms; {                              /* class definition body           */
    Text ct   = textOf(getHead(snd(head)));
    Int arity = argCount;

    if (nonNull(findClass(ct))) {
        ERRMSG(line) "Repeated definition of class \"%s\"",
                     textToStr(ct)
        EEND;
    } else if (nonNull(findTycon(ct))) {
        ERRMSG(line) "\"%s\" used as both class and type constructor",
                     textToStr(ct)
        EEND;
    } else {
        Class nw           = newClass(ct);
        cclass(nw).line    = line;
        cclass(nw).arity   = arity;
        cclass(nw).head    = snd(head);
        cclass(nw).supers  = fst(head);
        cclass(nw).members = ms;
        cclass(nw).level   = 0;
        classDefns         = cons(nw,classDefns);
    }
}

/* --------------------------------------------------------------------------
 * Further analysis of class declarations:
 *
 * Full static analysis of class definitions must be postponed until the
 * complete script has been read and all static analysis on type definitions
 * has been completed.
 *
 * Once this has been achieved, we carry out the following checks on each
 * class definition:
 * - check that variables in header are distinct
 * - replace head by skeleton
 * - check superclass declarations, replace by skeltons
 * - split body of class into members and declarations
 * - make new name entry for each member function
 * - record member function number (eventually an offset into dictionary!)
 * - no member function has a previous definition ...
 * - no member function is mentioned more than once in the list of members
 * - each member function type is valid, replace vars by offsets
 * - qualify each member function type by class header
 * - only bindings for members appear in defaults
 * - only function bindings appear in defaults
 * - check that extended class hierarchy does not contain any cycles
 * ------------------------------------------------------------------------*/

static Void local checkClassDefn(c)     /* validate class definition       */
Class c; {
    List tyvars = NIL;
    Int  args   = cclass(c).arity - 1;
    Cell temp   = cclass(c).head;

    for (; isAp(temp); temp=fun(temp)) {
        if (!isVar(arg(temp))) {
            ERRMSG(cclass(c).line) "Type variable required in class head"
            EEND;
        }
        if (nonNull(varIsMember(textOf(arg(temp)),tyvars))) {
            ERRMSG(cclass(c).line)
                "Repeated type variable \"%s\" in class head",
                textToStr(textOf(arg(temp)))
            EEND;
        }
        tyvars = cons(arg(temp),tyvars);
    }

    for (temp=cclass(c).head; args>0; temp=fun(temp), args--) {
        arg(temp) = mkOffset(args);
    }
    arg(temp) = mkOffset(0);
    fun(temp) = c;

    tcDeps              = NIL;          /* find dependents                 */
    map2Proc(depPredExp,cclass(c).line,tyvars,cclass(c).supers);
    cclass(c).numSupers = length(cclass(c).supers);
    cclass(c).defaults  = extractBindings(cclass(c).members);   /* defaults*/
    cclass(c).members   = extractSigdecls(cclass(c).members);
    map2Proc(checkMems,c,tyvars,cclass(c).members);
    cclass(c).kinds     = tcDeps;
    tcDeps              = NIL;
}

static Void local depPredExp(line,tyvars,pred)
Int  line;
List tyvars;
Cell pred; {
    Int  args = 1;                      /* parser guarantees >=1 args      */
    Cell h    = fun(pred);
    for (; isAp(h); args++) {
        arg(pred) = depTypeExp(line,tyvars,arg(pred));
        pred      = h;
        h         = fun(pred);
    }
    arg(pred) = depTypeExp(line,tyvars,arg(pred));

    if (isQCon(h)) {                    /* standard class constraint       */
        Class c = findQualClass(h);
        if (isNull(c)) {
            ERRMSG(line) "Undefined class \"%s\"", identToStr(h)
            EEND;
        }
        fun(pred) = c;
        if (args!=cclass(c).arity) {
            ERRMSG(line) "Wrong number of arguments for class \"%s\"",
                        textToStr(cclass(c).text)
            EEND;
        }
        if (cellIsMember(c,classDefns) && !cellIsMember(c,tcDeps))
            tcDeps = cons(c,tcDeps);
    }
#if TREX
    else if (isExt(h)) {                /* Lacks predicate                 */
        if (args!=1) {                  /* parser shouldn't let this happen*/
            ERRMSG(line) "Wrong number of arguments for lacks predicate"
            EEND;
        }
    }
#endif
    else {                              /* check for other kinds of pred   */
        internal("depPredExp");         /* ... but there aren't any!       */
    }
}

static Void local checkMems(c,tyvars,m) /* check member function details   */
Class c;
List  tyvars;
Cell  m; {
    Int  line = intOf(fst3(m));
    List vs   = snd3(m);
    Type t    = thd3(m);
    List sig  = NIL;
    List tvs  = NIL;

    tyvars    = typeVarsIn(t,NIL,tyvars);/* Look for extra type vars.      */

    if (whatIs(t)==QUAL) {              /* Overloaded member signatures?   */
        map2Proc(depPredExp,line,tyvars,fst(snd(t)));
    } else {
        t = ap(QUAL,pair(NIL,t));
    }

    fst(snd(t)) = cons(cclass(c).head,fst(snd(t)));/* Add main predicate   */
    snd(snd(t)) = depTopType(line,tyvars,snd(snd(t)));

    for (tvs=tyvars; nonNull(tvs); tvs=tl(tvs)) { /* Quantify              */
        sig = ap(NIL,sig);
    }
    t       = mkPolyType(sig,t);
    thd3(m) = t;                                /* Save type               */
    take(cclass(c).arity,tyvars);               /* Delete extra type vars  */

    if (isAmbiguous(t)) {
        ambigError(line,"class declaration",hd(vs),t);
    }
}

static Void local addMembers(c)         /* Add definitions of member funs  */
Class c; {                              /* and other parts of class struct.*/
    List ms  = cclass(c).members;
    List ns  = NIL;                     /* List of names                   */
    Int  mno;                           /* Member function number          */

    for (mno=0; mno<cclass(c).numSupers; mno++) {
        ns = cons(newDSel(c,mno),ns);
    }
    cclass(c).dsels = rev(ns);          /* Save dictionary selectors       */

    for (mno=1, ns=NIL; nonNull(ms); ms=tl(ms)) {
        Int  line = intOf(fst3(hd(ms)));
        List vs   = rev(snd3(hd(ms)));
        Type t    = thd3(hd(ms));
        for (; nonNull(vs); vs=tl(vs)) {
            ns = cons(newMember(line,mno++,hd(vs),t),ns);
        }
    }
    cclass(c).members    = rev(ns);     /* Save list of members            */
    cclass(c).numMembers = length(cclass(c).members);

/*  Not actually needed just yet; for the time being, dictionary code will
    not be passed through the type checker.

    cclass(c).dtycon    = addPrimTycon(generateText("Dict.%s",c),
                                       NIL,
                                       cclass(c).arity,
                                       DATATYPE,
                                       NIL);
*/

    mno                  = cclass(c).numSupers + cclass(c).numMembers;
    cclass(c).dcon       = addPrimCfun(generateText("Make.%s",c),mno,0,0);
    implementCfun(cclass(c).dcon,NIL); /* ADR addition */
#if USE_NEWTYPE_FOR_DICTS
    if (mno==1) {                       /* Single entry dicts use newtype  */
        name(cclass(c).dcon).defn = nameId;
        name(hd(cclass(c).members)).number = mfunNo(0);
    }
#endif
    cclass(c).dbuild     = newDBuild(c);
    cclass(c).defaults   = classBindings("class",c,cclass(c).defaults);
}

static Name local newMember(l,no,v,t)   /* Make definition for member fn   */
Int  l;
Int  no;
Cell v;
Type t; {
    Name m = findName(textOf(v));

    if (isNull(m)) {
        m = newName(textOf(v));
    } else if (name(m).defn!=PREDEFINED) {
        ERRMSG(l) "Repeated definition for member function \"%s\"",
                  textToStr(name(m).text)
        EEND;
    }

    name(m).line   = l;
    name(m).arity  = 1;
    name(m).number = mfunNo(no);
    name(m).type   = t;
    return m;
}

static Name local newDSel(c,no)         /* Make definition for dict selectr*/
Class c;
Int   no; {
    Name s;
    char buf[16];

    sprintf(buf,"sc%d.%s",no,"%s");
    s              = newName(generateText(buf,c));
    name(s).line   = cclass(c).line;
    name(s).arity  = 1;
    name(s).number = DFUNNAME;
    return s;
}

static Name local newDBuild(c)          /* Make definition for builder     */
Class c; {
    Name b         = newName(generateText("class.%s",c));
    name(b).line   = cclass(c).line;
    name(b).arity  = cclass(c).numSupers+1;
    return b;
}

#define MAX_GEN  128

static Text local generateText(sk,c)    /* We need to generate names for   */
String sk;                              /* certain objects corresponding   */
Class  c; {                             /* to each class.                  */
    String cname = textToStr(cclass(c).text);
    char buffer[MAX_GEN+1];

    if ((strlen(sk)+strlen(cname))>=MAX_GEN) {
        ERRMSG(0) "Please use a shorter name for class \"%s\"", cname
        EEND;
    }
    sprintf(buffer,sk,cname);
    return findText(buffer);
}

static Int local visitClass(c)          /* visit class defn to check that  */
Class c; {                              /* class hierarchy is acyclic      */
#if TREX
    if (isExt(c)) {                     /* special case for lacks preds    */
        return 0;
    }
#endif
    if (cclass(c).level < 0) {          /* already visiting this class?    */
        ERRMSG(cclass(c).line) "Class hierarchy for \"%s\" is not acyclic",
                               textToStr(cclass(c).text)
        EEND;
    } else if (cclass(c).level == 0) {   /* visiting class for first time   */
        List scs = cclass(c).supers;
        Int  lev = 0;
        cclass(c).level = (-1);
        for (; nonNull(scs); scs=tl(scs)) {
            Int l = visitClass(getHead(hd(scs)));
            if (l>lev) lev=l;
        }
        cclass(c).level = 1+lev;        /* level = 1 + max level of supers */
    }
    return cclass(c).level;
}

/* --------------------------------------------------------------------------
 * Process class and instance declaration binding groups:
 * ------------------------------------------------------------------------*/

static List local classBindings(where,c,bs)
String where;                           /*check validity of bindings bs for*/
Class  c;                               /* class c (or an instance of c)   */
List   bs; {                            /* sort into approp. member order  */
    List nbs = NIL;

    for (; nonNull(bs); bs=tl(bs)) {
        Cell b = hd(bs);
        Name mnm;

        if (!isVar(fst(b))) {           /* only allows function bindings   */
            ERRMSG(rhsLine(snd(snd(snd(b)))))
               "Pattern binding illegal in %s declaration", where
            EEND;
        }

        if (isNull(mnm=memberName(c,textOf(fst(b))))) {
            ERRMSG(rhsLine(snd(hd(snd(snd(b))))))
                "No member \"%s\" in class \"%s\"",
                textToStr(textOf(fst(b))), textToStr(cclass(c).text)
            EEND;
        }

        snd(b) = snd(snd(b));
        nbs = numInsert(mfunOf(mnm)-1,b,nbs);
    }
    return nbs;
}

static Name local memberName(c,t)       /* return name of member function  */
Class c;                                /* with name t in class c          */
Text  t; {                              /* return NIL if not a member      */
    List ms = cclass(c).members;
    for (; nonNull(ms); ms=tl(ms)) {
        if (t==name(hd(ms)).text) {
            return hd(ms);
        }
    }
    return NIL;
}

static List local numInsert(n,x,xs)     /* insert x at nth position in xs, */
Int  n;                                 /* filling gaps with NIL           */
Cell x;
List xs; {
    List start = isNull(xs) ? cons(NIL,NIL) : xs;

    for (xs=start; 0<n--; xs=tl(xs)) {
        if (isNull(tl(xs))) {
            tl(xs) = cons(NIL,NIL);
        }
    }
    hd(xs) = x;
    return start;
}

/* --------------------------------------------------------------------------
 * Calculate set of variables appearing in a given type expression (possibly
 * qualified) as a list of distinct values.  The order in which variables
 * appear in the list is the same as the order in which those variables
 * occur in the type expression when read from left to right.
 * ------------------------------------------------------------------------*/

static List local typeVarsIn(ty,us,vs)  /* Calculate list of type variables*/
Cell ty;                                /* used in type expression, reading*/
List us;                                /* from left to right ignoring any */
List vs; {                              /* listed in us.                   */
    switch (whatIs(ty)) {
        case AP        : return typeVarsIn(snd(ty),us,
                                           typeVarsIn(fst(ty),us,vs));

        case VARIDCELL :
        case VAROPCELL : if (nonNull(findBtyvs(textOf(ty)))
                             || varIsMember(textOf(ty),us)) {
                             return vs;
                          } else {
                             return maybeAppendVar(ty,vs);
                          }
        case POLYTYPE  : return typeVarsIn(monotypeOf(ty),polySigOf(ty),vs);

        case QUAL      : {   List qs = fst(snd(ty));
                             for (; nonNull(qs); qs=tl(qs)) {
                                 vs = typeVarsIn(hd(qs),us,vs);
                             }
                             return typeVarsIn(snd(snd(ty)),us,vs);
                         }

        case BANG      : return typeVarsIn(snd(ty),us,vs);

        case LABC      : {   List fs = snd(snd(ty));
                             for (; nonNull(fs); fs=tl(fs)) {
                                vs = typeVarsIn(snd(hd(fs)),us,vs);
                             }
                             return vs;
                         }
    }
    return vs;
}

static List local maybeAppendVar(v,vs)  /* append variable to list if not  */
Cell v;                                 /* already included                */
List vs; {
    Text t = textOf(v);
    List p = NIL;
    List c = vs;

    while (nonNull(c)) {
        if (textOf(hd(c))==t) {
            return vs;
        }
        p = c;
        c = tl(c);
    }

    if (nonNull(p)) {
        tl(p) = cons(v,NIL);
    } else {
        vs    = cons(v,NIL);
    }
    return vs;
}

/* --------------------------------------------------------------------------
 * Static analysis for type expressions is required to:
 *   - ensure that each type constructor or class used has been defined.
 *   - replace type variables by offsets, constructor names by Tycons.
 *   - ensure that the type is well-kinded.
 * ------------------------------------------------------------------------*/

static Type local checkSigType(line,where,e,type)
Int    line;                            /* Check validity of type expr in  */
String where;                           /* explicit type signature         */
Cell   e;
Type   type; {
    List tvs  = typeVarsIn(type,NIL,NIL);
    Int  n    = length(tvs);
    List sunk = unkindTypes;

    if (whatIs(type)==QUAL) {
        map2Proc(depPredExp,line,tvs,fst(snd(type)));
        snd(snd(type)) = depTopType(line,tvs,snd(snd(type)));

        if (isAmbiguous(type)) {
            ambigError(line,where,e,type);
        }
    } else {
        type = depTopType(line,tvs,type);
    }
    if (n>0) {
        if (n>=NUM_OFFSETS) {
            ERRMSG(line) "Too many type variables in %s\n", where
            EEND;
        } else {
            List ts = tvs;
            for (; nonNull(ts); ts=tl(ts)) {
                hd(ts) = NIL;
            }
            type    = mkPolyType(tvs,type);
        }
    }

    unkindTypes = NIL;
    kindType(line,"type expression",type);
    fixKinds();
    unkindTypes = sunk;
    return type;
}

static Type local depTopType(l,tvs,t)   /* Check top-level of type sig     */
Int  l;
List tvs;
Type t; {
    Type prev = NIL;
    Type t1   = t;
    Int  nr2  = 0;
    Int  i    = 1;
    for (; getHead(t1)==typeArrow; ++i) {
        arg(fun(t1)) = depCompType(l,tvs,arg(fun(t1)));
        if (isPolyType(arg(fun(t1)))) {
            nr2 = i;
        }
        prev = t1;
        t1   = arg(t1);
    }
    if (nonNull(prev)) {
        arg(prev) = depTypeExp(l,tvs,t1);
    } else {
        t = depTypeExp(l,tvs,t1);
    }
    if (nr2>0) {
        t = ap(RANK2,pair(mkInt(nr2),t));
    }
    return t;
}

static Type local depCompType(l,tvs,t)  /* Check component type for constr */
Int  l;
List tvs;
Type t; {
    if (isPolyType(t)) {
        Int  ntvs = length(tvs);
        List nfr  = NIL;
        if (isPolyType(t)) {
            List vs  = fst(snd(t));
            List bvs = typeVarsIn(monotypeOf(t),NIL,NIL);
            List us  = vs;
            for (; nonNull(us); us=tl(us)) {
                Text u = textOf(hd(us));
                if (varIsMember(u,tl(us))) {
                    ERRMSG(l) "Duplicated quantified variable %s",
                              textToStr(u)
                    EEND;
                }
                if (varIsMember(u,tvs)) {
                    ERRMSG(l) "Local quantifier for %s hides an outer use",
                              textToStr(u)
                    EEND;
                }
                if (!varIsMember(u,bvs)) {
                    ERRMSG(l) "Locally quantified variable %s is not used",
                              textToStr(u)
                    EEND;
                }
            }
            nfr = replicate(length(vs),NIL);
            tvs = appendOnto(tvs,vs);
            t   = monotypeOf(t);
        }
        if (whatIs(t)==QUAL) {
            map2Proc(depPredExp,l,tvs,fst(snd(t)));
            snd(snd(t)) = depTypeExp(l,tvs,snd(snd(t)));
            if (isAmbiguous(t))
                ambigError(l,"type component",NIL,t);
        } else {
            t = depTypeExp(l,tvs,t);
        }
        if (isNull(nfr)) {
            return t;
        }
        take(ntvs,tvs);
        return mkPolyType(nfr,t);
    } else {
        return depTypeExp(l,tvs,t);
    }
}

static Type local depTypeExp(line,tyvars,type)
Int  line;
List tyvars;
Type type; {
    switch (whatIs(type)) {
        case AP         : fst(type) = depTypeExp(line,tyvars,fst(type));
                          snd(type) = depTypeExp(line,tyvars,snd(type));
                          break;

        case VARIDCELL  : return depTypeVar(line,tyvars,textOf(type));

        case QUALIDENT  : if (isQVar(type)) {
                              ERRMSG(line) "Qualified type variables not allowed"
                              EEND;
                          }
                          /* deliberate fall through */
        case CONIDCELL  : {   Tycon tc = findQualTycon(type);
                              if (isNull(tc)) {
                                  ERRMSG(line)
                                      "Undefined type constructor \"%s\"",
                                      identToStr(type)
                                  EEND;
                              }
                              if (cellIsMember(tc,tyconDefns) &&
                                  !cellIsMember(tc,tcDeps)) {
                                  tcDeps = cons(tc,tcDeps);
                              }
                              return tc;
                          }

#if TREX
        case EXT        :
#endif
        case TYCON      :
        case TUPLE      : break;

        default         : internal("depTypeExp");
    }
    return type;
}

static Type local depTypeVar(line,tyvars,tv)
Int  line;
List tyvars;
Text tv; {
    Int  offset = 0;
    Cell vt     = findBtyvs(tv);

    if (nonNull(vt)) {
        return fst(vt);
    }
    for (; nonNull(tyvars) && tv!=textOf(hd(tyvars)); offset++) {
        tyvars = tl(tyvars);
    }
    if (isNull(tyvars)) {
        ERRMSG(line) "Undefined type variable \"%s\"", textToStr(tv)
        EEND;
    }
    return mkOffset(offset);
}

/* --------------------------------------------------------------------------
 * Check for ambiguous types:
 * A type  Preds => type  is ambiguous if not (TV(P) `subset` TV(type))
 * ------------------------------------------------------------------------*/

Bool isAmbiguous(type)                  /* Determine whether type is       */
Type type; {                            /* ambiguous                       */
    if (isPolyType(type)) {
        type = monotypeOf(type);
    }
    if (whatIs(type)==QUAL) {           /* only qualified types can be     */
        List tvps = offsetTyvarsIn(fst(snd(type)),NIL); /* ambiguous       */
        List tvts = offsetTyvarsIn(snd(snd(type)),NIL);
        while (nonNull(tvps) && cellIsMember(hd(tvps),tvts)) {
            tvps = tl(tvps);
        }
        return nonNull(tvps);
    }
    return FALSE;
}

Void ambigError(line,where,e,type)      /* produce error message for       */
Int    line;                            /* ambiguity                       */
String where;
Cell   e;
Type   type; {
    ERRMSG(line) "Ambiguous type signature in %s", where ETHEN
    ERRTEXT      "\n*** ambiguous type : " ETHEN ERRTYPE(type);
    if (nonNull(e)) {
        ERRTEXT  "\n*** assigned to    : " ETHEN ERREXPR(e);
    }
    ERRTEXT      "\n"
    EEND;
}

/* --------------------------------------------------------------------------
 * Kind inference for simple types:
 * ------------------------------------------------------------------------*/

static Void local kindConstr(line,alpha,m,c)
Int  line;                              /* Determine kind of constructor   */
Int  alpha;
Int  m;
Cell c; {
    Cell h = getHead(c);
    Int  n = argCount;

#ifdef DEBUG_KINDS
    printf("kindConstr: alpha=%d, m=%d, c=",alpha,m);
    printType(stdout,c);
    printf("\n");
#endif

    switch (whatIs(h)) {
        case POLYTYPE : if (n!=0) {
                            internal("kindConstr1");
                        } else {
                            static String pt = "polymorphic type";
                            Type  t  = dropRank1(c,alpha,m);
                            Kinds ks = polySigOf(t);
                            Int   m1 = 0;
                            Int   beta;
                            for (; isAp(ks); ks=tl(ks))
                                m1++;
                            beta        = newKindvars(m1);
                            unkindTypes = cons(pair(mkInt(beta),t),unkindTypes);
                            checkKind(line,beta,m1,monotypeOf(t),NIL,pt,STAR,0);
                        }
                        return;

        case QUAL     : if (n!=0) {
                            internal("kindConstr2");
                        }
                        map3Proc(kindPred,line,alpha,m,fst(snd(c)));
                        kindConstr(line,alpha,m,snd(snd(c)));
                        return;

        case EXIST    :
        case RANK2    : kindConstr(line,alpha,m,snd(snd(c)));
                        return;

#if TREX
        case EXT      : if (n!=2) {
                            ERRMSG(line)
                                "Illegal use of row in " ETHEN ERRTYPE(c);
                            ERRTEXT "\n"
                            EEND;
                        }
                        break;
#endif

        case TYCON    : if (isSynonym(h) && n<tycon(h).arity) {
                            ERRMSG(line)
                              "Not enough arguments for type synonym \"%s\"",
                              textToStr(tycon(h).text)
                            EEND;
                        }
                        break;
    }

    if (n==0) {                         /* trivial case, no arguments      */
        typeIs = kindAtom(alpha,c);
    } else {                              /* non-trivial application         */
        static String app = "constructor application";
        Cell   a = c;
        Int    i;
        Kind   k;
        Int    beta;

        varKind(n);
        beta   = typeOff;
        k      = typeIs;

        typeIs = kindAtom(alpha,h);     /* h  :: v1 -> ... -> vn -> w      */
        shouldKind(line,h,c,app,k,beta);

        for (i=n; i>0; --i) {           /* ci :: vi for each 1 <- 1..n     */
            checkKind(line,alpha,m,arg(a),c,app,aVar,beta+i-1);
            a = fun(a);
        }
        tyvarType(beta+n);              /* inferred kind is w              */
    }
}

static Kind local kindAtom(alpha,c)     /* Find kind of atomic constructor */
Int  alpha;
Cell c; {
    switch (whatIs(c)) {
        case TUPLE     : return simpleKind(tupleOf(c)); /*(,)::* -> * -> * */
        case OFFSET    : return mkInt(alpha+offsetOf(c));
        case TYCON     : return tycon(c).kind;
        case INTCELL   : return c;
        case VARIDCELL :
        case VAROPCELL : {   Cell vt = findBtyvs(textOf(c));
                             if (nonNull(vt)) {
                                 return snd(vt);
                             }
                         }
#if TREX
        case EXT       : return extKind;
#endif
    }
#if DEBUG_KINDS
    printf("kindAtom(%d,whatIs(%d)) on ",alpha,whatIs(c));
    printType(stdout,c);
    printf("\n");
#endif
    internal("kindAtom");
    return STAR;/* not reached */
}

static Void local kindPred(l,alpha,m,pi)/* Check kinds of arguments in pred*/
Int  l;
Int  alpha;
Int  m;
Cell pi; {
#if TREX
    if (isExt(fun(pi))) {
        static String lackspred = "lacks predicate";
        checkKind(l,alpha,m,arg(pi),NIL,lackspred,ROW,0);
        return;
    }
#endif
    {   static String predicate = "class constraint";
        Class c  = getHead(pi);
        List  as = getArgs(pi);
        Kinds ks = cclass(c).kinds;

        while (nonNull(ks)) {
            checkKind(l,alpha,m,hd(as),NIL,predicate,hd(ks),0);
            ks = tl(ks);
            as = tl(as);
        }
    }
}

static Void local kindType(line,wh,type)/* check that (poss qualified) type*/
Int    line;                            /* is well-kinded                  */
String wh;
Type   type; {
    checkKind(line,0,0,type,NIL,wh,STAR,0);
}

static Void local fixKinds() {          /* add kind annotations to types   */
    for (; nonNull(unkindTypes); unkindTypes=tl(unkindTypes)) {
        Pair pr   = hd(unkindTypes);
        Int  beta = intOf(fst(pr));
        Cell qts  = polySigOf(snd(pr));
        for (;;) {
            if (isNull(hd(qts))) {
                hd(qts) = copyKindvar(beta++);
            } else {
                internal("fixKinds");
            }
            if (nonNull(tl(qts))) {
                qts = tl(qts);
            } else {
                tl(qts) = STAR;
                break;
            }
        }
#ifdef DEBUG_KINDS
        printf("Type expression: ");
        printType(stdout,snd(pr));
        printf(" :: ");
        printKind(stdout,polySigOf(snd(pr)));
        printf("\n");
#endif
    }
}

/* --------------------------------------------------------------------------
 * Kind checking of groups of type constructors and classes:
 * ------------------------------------------------------------------------*/

static Void local kindTCGroup(tcs)      /* find kinds for mutually rec. gp */
List tcs; {                             /* of tycons and classes           */
    emptySubstitution();
    unkindTypes = NIL;
    mapProc(initTCKind,tcs);
    mapProc(kindTC,tcs);
    mapProc(genTC,tcs);
    fixKinds();
    emptySubstitution();
}
    
static Void local initTCKind(c)         /* build initial kind/arity for c  */
Cell c; {
    if (isTycon(c)) {                   /* Initial kind of tycon is:       */
        Int beta = newKindvars(1);      /*    v1 -> ... -> vn -> vn+1      */
        varKind(tycon(c).arity);        /* where n is the arity of c.      */
        bindTv(beta,typeIs,typeOff);    /* For data definitions, vn+1 == * */
        switch (whatIs(tycon(c).what)) {
            case NEWTYPE  :
            case DATATYPE : bindTv(typeOff+tycon(c).arity,STAR,0);
        }
        tycon(c).kind = mkInt(beta);
    } else {
        Int n    = cclass(c).arity;
        Int beta = newKindvars(n);
        cclass(c).kinds = NIL;
        do {
            n--;
            cclass(c).kinds = pair(mkInt(beta+n),cclass(c).kinds);
        } while (n>0);
    }
}

static Void local kindTC(c)             /* check each part of a tycon/class*/
Cell c; {                               /* is well-kinded                  */
    if (isTycon(c)) {
        static String cfun = "constructor function";
        static String tsyn = "synonym definition";
        Int line = tycon(c).line;
        Int beta = tyvar(intOf(tycon(c).kind))->offs;
        Int m    = tycon(c).arity;
        switch (whatIs(tycon(c).what)) {
            case NEWTYPE     :
            case DATATYPE    : {   List cs = tycon(c).defn;
                                   if (whatIs(cs)==QUAL) {
                                       map3Proc(kindPred,line,beta,m,
                                                                fst(snd(cs)));
                                       tycon(c).defn = cs = snd(snd(cs));
                                   }
                                   for (; hasCfun(cs); cs=tl(cs)) {
                                       kindType(line,cfun,name(hd(cs)).type);
                                   }
                                   break;
                               }

            default          : checkKind(line,beta,m,tycon(c).defn,NIL,
                                                        tsyn,aVar,beta+m);
        }
    }
    else {                              /* scan type exprs in class defn to*/
        List ms   = cclass(c).members;  /* determine the class signature   */
        Int  m    = cclass(c).arity;
        Int  beta = newKindvars(m);
        kindPred(cclass(c).line,beta,m,cclass(c).head);
        map3Proc(kindPred,cclass(c).line,beta,m,cclass(c).supers);
        for (; nonNull(ms); ms=tl(ms)) {
            Int  line = intOf(fst3(hd(ms)));
            Type type = thd3(hd(ms));
            kindType(line,"member function type signature",type);
        }
    }
}

static Void local genTC(c)              /* generalise kind inferred for    */
Cell c; {                               /* given tycon/class               */
    if (isTycon(c)) {
        tycon(c).kind = copyKindvar(intOf(tycon(c).kind));
#ifdef DEBUG_KINDS
        printf("%s :: ",textToStr(tycon(c).text));
        printKind(stdout,tycon(c).kind);
        putchar('\n');
#endif
    } else {
        Kinds ks = cclass(c).kinds;
        for (; nonNull(ks); ks=tl(ks)) {
            hd(ks) = copyKindvar(intOf(hd(ks)));
        }
#ifdef DEBUG_KINDS
        printf("%s :: ",textToStr(cclass(c).text));
        printKinds(stdout,cclass(c).kinds);
        putchar('\n');
#endif
    }
}

/* --------------------------------------------------------------------------
 * Static analysis of instance declarations:
 *
 * The first part of the static analysis is performed as the declarations
 * are read during parsing:
 * - make new entry in instance table
 * - record line number of declaration
 * - build list of instances defined in current script for use in later
 *   stages of static analysis.
 * ------------------------------------------------------------------------*/

Void instDefn(line,head,ms)             /* process new instance definition */
Int  line;                              /* definition line number          */
Cell head;                              /* inst header :: (context,Class)  */
List ms; {                              /* instance members                */
    Inst nw             = newInst();
    inst(nw).line       = line;
    inst(nw).specifics  = fst(head);
    inst(nw).head       = snd(head);
    inst(nw).implements = ms;
    instDefns           = cons(nw,instDefns);
}

/* --------------------------------------------------------------------------
 * Further static analysis of instance declarations:
 *
 * Makes the following checks:
 * - Class part of header has form C (T a1 ... an) where C is a known
 *   class, and T is a known datatype constructor (or restricted synonym),
 *   and there is no previous C-T instance, and (T a1 ... an) has a kind
 *   appropriate for the class C.
 * - Each element of context is a valid class expression, with type vars
 *   drawn from a1, ..., an.
 * - All bindings are function bindings
 * - All bindings define member functions for class C
 * - Arrange bindings into appropriate order for member list
 * - No top level type signature declarations
 * ------------------------------------------------------------------------*/

Bool allowOverlap = FALSE;              /* TRUE => allow overlapping insts */

static Void local checkInstDefn(in)     /* Validate instance declaration   */
Inst in; {
    Int  line   = inst(in).line;
    List tyvars = typeVarsIn(inst(in).head,NIL,NIL);

    depPredExp(line,tyvars,inst(in).head);
    map2Proc(depPredExp,line,tyvars,inst(in).specifics);
    inst(in).numSpecifics = length(inst(in).specifics);
    inst(in).c            = getHead(inst(in).head);
    if (!isClass(inst(in).c)) {
        ERRMSG(line) "Illegal predicate in instance declaration"
        EEND;
    }
#if EVAL_INSTANCES
    if (inst(in).c==classEval) {
        ERRMSG(line) "Instances of class \"%s\" are generated automatically",
                     textToStr(cclass(inst(in).c).text)
        EEND;
    }
#endif
    kindInst(in,length(tyvars));
    insertInst(in);

    if (nonNull(extractSigdecls(inst(in).implements))) {
        ERRMSG(line) "Type signature decls not permitted in instance decl"
        EEND;
    }
    inst(in).implements = classBindings("instance",
                                        inst(in).c,
                                        extractBindings(inst(in).implements));
    inst(in).builder    = newInstImp(in);
}

static Void local insertInst(in)        /* Insert instance into class      */
Inst in; {
    Class c    = inst(in).c;
    List  ins  = cclass(c).instances;
    List  prev = NIL;

    substitution(RESET);
    while (nonNull(ins)) {              /* Look for overlap w/ other insts */
        Int alpha = newKindedVars(inst(in).kinds);
        Int beta  = newKindedVars(inst(hd(ins)).kinds);
        if (unifyPred(inst(in).head,alpha,inst(hd(ins)).head,beta)) {
            Cell pi  = copyPred(inst(in).head,alpha);
            if (allowOverlap) {         /* So long as one is more specific */
                Bool bef = instCompare(in,hd(ins));
                Bool aft = instCompare(hd(ins),in);
                if (bef && !aft) {      /* in comes strictly before hd(ins)*/
                    break;
                }
                if (aft && !bef) {      /* in comes strictly after hd(ins) */
                    prev = ins;
                    ins  = tl(ins);
                    continue;
                }
            }
            ERRMSG(inst(in).line) "Overlapping instances for class \"%s\"",
                                  textToStr(cclass(c).text)
            ETHEN
            ERRTEXT "\n*** This instance   : " ETHEN ERRPRED(inst(in).head);
            ERRTEXT "\n*** Overlaps with   : " ETHEN
                                               ERRPRED(inst(hd(ins)).head);
            ERRTEXT "\n*** Common instance : " ETHEN
                                               ERRPRED(pi);
            ERRTEXT "\n"
            EEND;
        }
        prev = ins;                     /* No overlap detected, so move on */
        ins  = tl(ins);                 /* to next instance                */
    }
    substitution(RESET);

    if (nonNull(prev)) {                /* Insert instance at this point   */
        tl(prev) = cons(in,ins);
    } else {
        cclass(c).instances = cons(in,ins);
    }
}

static Bool local instCompare(ia,ib)    /* See if ia is an instance of ib  */
Inst ia, ib;{
    Int alpha = newKindedVars(inst(ia).kinds);
    Int beta  = newKindedVars(inst(ib).kinds);
    return matchPred(inst(ia).head,alpha,inst(ib).head,beta);
}

static Name local newInstImp(in)        /* Make definition for inst builder*/
Inst in; {
    Name b         = newName(inventText());
    name(b).line   = inst(in).line;
    name(b).arity  = inst(in).numSpecifics;
    name(b).number = DFUNNAME;
    return b;
}

/* --------------------------------------------------------------------------
 * Kind checking of instance declaration headers:
 * ------------------------------------------------------------------------*/

static Void local kindInst(in,freedom)  /* check predicates in instance    */
Inst in;
Int  freedom; {
    Int beta;

    emptySubstitution();
    beta = newKindvars(freedom);
    kindPred(inst(in).line,beta,freedom,inst(in).head);
    if (whatIs(inst(in).specifics)!=DERIVE) {
        map3Proc(kindPred,inst(in).line,beta,freedom,inst(in).specifics);
    }
    for (inst(in).kinds = NIL; 0<freedom--; ) {
        inst(in).kinds = cons(copyKindvar(beta+freedom),inst(in).kinds);
    }
#ifdef DEBUG_KINDS
    printf("instance ");
    printPred(stdout,inst(in).head);
    printf(" :: ");
    printKinds(stdout,inst(in).kinds);
    putchar('\n');
#endif
    emptySubstitution();
}

/* --------------------------------------------------------------------------
 * Process derived instance requests:
 * ------------------------------------------------------------------------*/

static List derivedInsts;               /* list of derived instances       */

static Void local checkDerive(t,p,ts,ct)/* verify derived instance request */
Tycon t;                                /* for tycon t, with explicit      */
List  p;                                /* context p, component types ts   */
List  ts;                               /* and named class ct              */
Cell  ct; {
    Int   line = tycon(t).line;
    Class c    = findClass(textOf(ct));
    if (isNull(c)) {
        ERRMSG(line) "Unknown class \"%s\" in derived instance",
                     textToStr(textOf(ct))
        EEND;
    }
    addDerInst(line,c,p,dupList(ts),t,tycon(t).arity);
}

static Void local addDerInst(line,c,p,cts,t,a)  /* Add a derived instance  */
Int   line;
Class c;
List  p, cts;
Type  t;
Int   a; {
    Inst in;
    Cell head = t;                              /* Build instance head     */
    Int  i    = 0;

    for (; i<a; i++) {
        head = ap(head,mkOffset(i));
    }
    head = ap(c,head);

    in                  = newInst();
    inst(in).c          = c;
    inst(in).line       = line;
    inst(in).head       = head;
    inst(in).specifics  = ap(DERIVE,pair(dupList(p),cts));
    inst(in).implements = NIL;
    inst(in).kinds      = mkInt(a);
    derivedInsts        = cons(in,derivedInsts);
}

Void addTupInst(c,n)                    /* Request derived instance of c   */
Class c;                                /* for mkTuple(n) constructor      */
Int   n; {
    Int  m   = n;
    List cts = NIL;
    while (0<m--) {
        cts = cons(mkOffset(m),cts);
    }
    cts = rev(cts);
    addDerInst(0,c,NIL,cts,mkTuple(n),n);
}

#if EVAL_INSTANCES
/* ADR addition */
static List evalInsts = NIL;

Void addEvalInst(line,t,arity,ctxt)     /* Add dummy instance for Eval     */
Int  line;
Cell t;
Int  arity;
List ctxt; {
    Inst in   = newInst();
    Cell head = t;
    Int  i;
    for (i=0; i<arity; i++) {
        head = ap(head,mkOffset(i));
    }
    inst(in).line         = line;
    inst(in).c            = classEval;
    inst(in).head         = ap(classEval,head);
    inst(in).specifics    = ctxt;
    inst(in).builder      = newInstImp(in);
    inst(in).numSpecifics = length(ctxt);
    kindInst(in,arity);
    cclass(classEval).instances
             = appendOnto(cclass(classEval).instances,singleton(in));
    /* ADR addition */
    evalInsts             = cons(in,evalInsts);
}
#endif

#if TREX
Inst addRecShowInst(c,e)                /* Generate instance for ShowRecRow*/
Class c;                                /* c *must* be ShowRecRow          */
Ext   e; {
    Inst in               = newInst();
    inst(in).c            = c;
    inst(in).head         = ap(c,ap2(e,mkOffset(0),mkOffset(1)));
    inst(in).kinds        = extKind;
    inst(in).specifics    = cons(ap(classShow,mkOffset(0)),
                                 cons(ap(e,mkOffset(1)),
                                      cons(ap(c,mkOffset(1)),NIL)));
    inst(in).numSpecifics = 3;
    inst(in).builder      = implementRecShw(extText(e));
    cclass(c).instances   = appendOnto(cclass(c).instances,singleton(in));
    return in;
}

Inst addRecEqInst(c,e)                  /* Generate instance for EqRecRow  */
Class c;                                /* c *must* be EqRecRow            */
Ext   e; {
    Inst in               = newInst();
    inst(in).c            = c;
    inst(in).head         = ap(c,ap2(e,mkOffset(0),mkOffset(1)));
    inst(in).kinds        = extKind;
    inst(in).specifics    = cons(ap(classEq,mkOffset(0)),
                                 cons(ap(e,mkOffset(1)),
                                      cons(ap(c,mkOffset(1)),NIL)));
    inst(in).numSpecifics = 3;
    inst(in).builder      = implementRecEq(extText(e));
    cclass(c).instances   = appendOnto(cclass(c).instances,singleton(in));
    return in;
}
#endif

/* --------------------------------------------------------------------------
 * Calculation of contexts for derived instances:
 *
 * Allowing arbitrary types to appear in contexts makes it rather harder
 * to decide what the context for a derived instance should be.  For
 * example, given:
 *
 *    data T a = MkT [a] deriving Show,
 *
 * we could have either of the following:
 *
 *    instance (Show [a]) => Show (T a) where ...
 *    instance (Show a) => Show (T a) where ...
 *
 * (assuming, of course, that instance (Show a) => Show [a]).  For now, we
 * choose to reduce contexts in the hope of detecting errors at an earlier
 * stage---in contrast with value definitions, there is no way for a user
 * to provide something analogous to a `type signature' by which they might
 * be able to control this behaviour themselves.  We eliminate tautological
 * predicates, but only allow predicates to appear in the final result if
 * they have at least one argument with a variable at its head.
 *
 * In general, we have to deal with mutually recursive instance declarations.
 * We find a solution in the obvious way by iterating to find a fixed point.
 * Of course, without restrictions on the form of instance declarations, we
 * cannot be sure that this will always terminate!
 *
 * For each instance we maintain a pair of the form DERIVE (ctxt,ps).
 * Ctxt is a list giving the parts of the context that have been produced
 * so far in the form of predicate skeletons.  During the calculation of
 * derived instances, we attach a dummy NIL value to the end of the list
 * which acts as a kind of `variable': other parts of the system maintain
 * pointers to this variable, and use it to detect when the context has
 * been extended with new elements.  Meanwhile, ps is a list containing
 * predicates (pi,o) together with (delayed) substitutions of the form
 * (o,xs) where o is an offset and xs is one of the context variables
 * described above, which may have been partially instantiated.
 * ------------------------------------------------------------------------*/

static Bool instsChanged;

static Void local deriveContexts(is)    /* Calc contexts for derived insts */
List is; {
    emptySubstitution();
    mapProc(initDerInst,is);            /* Prepare derived instances       */

    do {                                /* Main calculation of contexts    */
        instsChanged = FALSE;
        mapProc(calcInstPreds,is);
    } while (instsChanged);

    mapProc(tidyDerInst,is);            /* Tidy up results                 */
#if DERIVE_SHOW | DERIVE_READ
    cfunSfuns = NIL;                    /* Only needed to derive Read/Show */
#endif
}

static Void local initDerInst(in)       /* Prepare instance for calculation*/
Inst in; {                              /* of derived instance context     */
    Cell spcs = inst(in).specifics;
    Int  beta = newKindedVars(inst(in).kinds);
    if (whatIs(spcs)!=DERIVE) {
        internal("initDerInst");
    }
    fst(snd(spcs)) = appendOnto(fst(snd(spcs)),singleton(NIL));
    for (spcs=snd(snd(spcs)); nonNull(spcs); spcs=tl(spcs)) {
        hd(spcs) = ap2(inst(in).c,hd(spcs),mkInt(beta));
    }
    inst(in).numSpecifics = beta;

#ifdef DEBUG_DERIVING
    printf("initDerInst: ");
    printPred(stdout,inst(in).head);
    printf("\n");
    printContext(stdout,snd(snd(inst(in).specifics)));
    printf("\n");
#endif
}

static Void local calcInstPreds(in)     /* Calculate next approximation    */
Inst in; {                              /* of the context for a derived    */
    List retain = NIL;                  /* instance                        */
    List ps     = snd(snd(inst(in).specifics));
    List spcs   = fst(snd(inst(in).specifics));
    Int  beta   = inst(in).numSpecifics;

#ifdef DEBUG_DERIVING
    printf("calcInstPreds: ");
    printPred(stdout,inst(in).head);
    printf("\n");
#endif

    while (nonNull(ps)) {
        Cell p = hd(ps);
        ps     = tl(ps);
        if (isInt(fst(p))) {                    /* Delayed substitution?   */
            List qs = snd(p);
            for (; nonNull(hd(qs)); qs=tl(qs)) {
                ps = cons(pair(hd(qs),fst(p)),ps);
            }
            retain = cons(pair(fst(p),qs),retain);
        }
#if TREX
        else if (isExt(fun(fst(p)))) {          /* Lacks predicate         */
            Text   l = extText(fun(fst(p)));
            Type   t = arg(fst(p));
            Int    o = intOf(snd(p));
            Type   h;
            Tyvar *tyv;

            deRef(tyv,t,o);
            h = getDerefHead(t,o);
            while (isExt(h) && argCount==2 && l!=extText(h)) {
                t = arg(t);
                deRef(tyv,t,o);
                h = getDerefHead(t,o);
            }
            if (argCount==0 && isOffset(h)) {
                maybeAddPred(ap(fun(fun(p)),h),o,beta,spcs);
            } else if (argCount!=0 || h!=typeNoRow) {
                Cell bpi = inst(in).head;
                Cell pi  = copyPred(fun(p),intOf(snd(p)));
                ERRMSG(inst(in).line) "Cannot derive " ETHEN ERRPRED(bpi);
                ERRTEXT " because predicate " ETHEN ERRPRED(pi);
                ERRTEXT " does not hold\n"
                EEND;
            }
        }
#endif
        else {                                  /* Class predicate         */
            Cell pi  = fst(p);
            Int  o   = intOf(snd(p));
            Inst in1 = findInstFor(pi,o);
            if (nonNull(in1)) {
                List qs  = inst(in1).specifics;
                Int  off = mkInt(typeOff);
                if (whatIs(qs)==DERIVE) {       /* Still being derived     */
                    for (qs=fst(snd(qs)); nonNull(hd(qs)); qs=tl(qs))
                        ps = cons(pair(hd(qs),off),ps);
                    retain = cons(pair(off,qs),retain);
                } else {                        /* Previously def'd inst   */
                    for (; nonNull(qs); qs=tl(qs)) {
                        ps = cons(pair(hd(qs),off),ps);
                    }
                }
            } else {                            /* No matching instance    */
                Cell qi = pi;
                while (isAp(qi) && isOffset(getDerefHead(arg(qi),o))) {
                    qi = fun(qi);
                }
                if (isAp(qi)) {
                    Cell bpi = inst(in).head;
                    pi       = copyPred(pi,o);
                    ERRMSG(inst(in).line) "An instance of " ETHEN ERRPRED(pi);
                    ERRTEXT " is required to derive "       ETHEN ERRPRED(bpi);
                    ERRTEXT "\n"
                    EEND;
                } else {
                    maybeAddPred(pi,o,beta,spcs);
                }
            }
        }
    }
    snd(snd(inst(in).specifics)) = retain;
}

static Void local maybeAddPred(pi,o,beta,ps)
Cell pi;                                /* Add predicate pi to the list ps,*/
Int  o;                                 /* setting the instsChanged flag if*/
Int  beta;                              /* pi is not already a member and  */
List ps; {                              /* using beta to adjust vars       */
    Cell c = getHead(pi);
    for (; nonNull(ps); ps=tl(ps)) {
        if (isNull(hd(ps))) {           /* reached the `dummy' end of list?*/
            hd(ps)       = copyAdj(pi,o,beta);
            tl(ps)       = pair(NIL,NIL);
            instsChanged = TRUE;
            return;
        } else if (c==getHead(hd(ps)) && samePred(pi,o,hd(ps),beta)) {
            return;
        }
    }
}

static Cell local copyAdj(c,o,beta)     /* Copy (c,o), replacing vars with */
Cell c;                                 /* offsets relative to beta.       */
Int  o;
Int  beta; {
    switch (whatIs(c)) {
        case AP     : {   Cell l = copyAdj(fst(c),o,beta);
                          Cell r = copyAdj(snd(c),o,beta);
                          return ap(l,r);
                      }

        case OFFSET : {   Int   vn   = o+offsetOf(c);
                          Tyvar *tyv = tyvar(vn);
                          if (isBound(tyv)) {
                              return copyAdj(tyv->bound,tyv->offs,beta);
                          }
                          vn -= beta;
                          if (vn<0 || vn>=NUM_OFFSETS) {
                              internal("copyAdj");
                          }
                          return mkOffset(vn);
                      }
    }
    return c;
}

static Void local tidyDerInst(in)       /* Tidy up results of derived inst */
Inst in; {                              /* calculations                    */
    Int  o  = inst(in).numSpecifics;
    List ps = tl(rev(fst(snd(inst(in).specifics))));
    clearMarks();
    copyPred(inst(in).head,o);
    inst(in).specifics    = simpleContext(ps,o);
    inst(in).numSpecifics = length(inst(in).specifics);

#ifdef DEBUG_DERIVING
    printf("Derived instance: ");
    printContext(stdout,inst(in).specifics);
    printf(" ||- ");
    printPred(stdout,inst(in).head);
    printf("\n");
#endif
}

/* --------------------------------------------------------------------------
 * Generate code for derived instances:
 * ------------------------------------------------------------------------*/

static Void local addDerivImp(in)
Inst in; {
    List  imp = NIL;
    Type  t   = getHead(arg(inst(in).head));
    Class c   = inst(in).c;
#if DERIVE_EQ
    if (c==classEq)
        imp = deriveEq(t);
    else
#endif
#if DERIVE_ORD
    if (c==classOrd)
        imp = deriveOrd(t);
    else 
#endif
#if DERIVE_ENUM
    if (c==classEnum)
        imp = deriveEnum(t);
    else 
#endif
#if DERIVE_IX
    if (c==classIx)
        imp = deriveIx(t);
    else 
#endif
#if DERIVE_SHOW
    if (c==classShow)
        imp = deriveShow(t);
    else 
#endif
#if DERIVE_READ
    if (c==classRead)
        imp = deriveRead(t);
    else 
#endif
#if DERIVE_BOUNDED
    if (c==classBounded)
        imp = deriveBounded(t);
    else 
#endif
    {
        ERRMSG(inst(in).line) "Cannot derive instances of class \"%s\"",
                              textToStr(cclass(inst(in).c).text)
        EEND;
    }

    kindInst(in,intOf(inst(in).kinds));
    insertInst(in);
    inst(in).builder    = newInstImp(in);
    inst(in).implements = classBindings("derived instance",
                                        inst(in).c,
                                        imp);
}

/* --------------------------------------------------------------------------
 * Default definitions; only one default definition is permitted in a
 * given script file.  If no default is supplied, then a standard system
 * default will be used where necessary.
 * ------------------------------------------------------------------------*/

Void defaultDefn(line,defs)             /* Handle default types definition */
Int  line;
List defs; {
    if (defaultLine!=0) {
        ERRMSG(line) "Multiple default declarations are not permitted in" ETHEN
        ERRTEXT     "a single script file.\n"
        EEND;
    }
    defaultDefns = defs;
    defaultLine  = line;
}

static Void local checkDefaultDefns() { /* check that default types are    */
    List ds = NIL;                      /* well-kinded instances of Num    */

    if (defaultLine!=0) {
        map2Over(depTypeExp,defaultLine,NIL,defaultDefns);
        emptySubstitution();
        unkindTypes = NIL;
        map2Proc(kindType,defaultLine,"default type",defaultDefns);
        fixKinds();
        emptySubstitution();
        mapOver(fullExpand,defaultDefns);
    } else {
        defaultDefns = stdDefaults;
    }
    for (ds=defaultDefns; nonNull(ds); ds=tl(ds)) {
        if (isNull(provePred(NIL,NIL,ap(classNum,hd(ds))))) {
            ERRMSG(defaultLine)
                "Default types must be instances of the Num class"
            EEND;
        }
    }
}

/* --------------------------------------------------------------------------
 * Foreign import declarations are Hugs' equivalent of GHC's ccall mechanism.
 * They are used to "import" C functions into a module.
 * They are usually not written by hand but, rather, generated automatically
 * by GreenCard, IDL compilers or whatever.
 *
 * Foreign export declarations generate C wrappers for Hugs functions.
 * Hugs only provides "foreign export dynamic" because it's not obvious
 * what "foreign export static" would mean in an interactive setting.
 * ------------------------------------------------------------------------*/

Void foreignImport(line,extName,intName,type) /* Handle foreign imports    */
Cell line;
Pair extName;
Cell intName;
Cell type; {
    Text t = textOf(intName);
    Name n = findName(t);
    Int  l = intOf(line);

    if (isNull(n)) {
        n = newName(t);
    } else if (name(n).defn!=PREDEFINED) {
        ERRMSG(l) "Redeclaration of foreign \"%s\"", textToStr(t)
        EEND;
    }
    name(n).line = l;
    name(n).defn = extName;
    name(n).type = type;
    foreignImports = cons(n,foreignImports);
}

static Void local checkForeignImport(p)   /* Check foreign import          */
Name p; {
    emptySubstitution();
    name(p).type = checkSigType(name(p).line,
                                "foreign import declaration",
                                p,
                                name(p).type);
    /* We don't expand synonyms here because we don't want the IO
     * part to be expanded.
     * name(p).type = fullExpand(name(p).type);
     */
    implementForeignImport(p);
}

Void foreignExport(line,extName,intName,type)/* Handle foreign exports    */
Cell line;
Cell extName;
Cell intName;
Cell type; {
    Text t = textOf(intName);
    Name n = findName(t);
    Int  l = intOf(line);

    if (isNull(n)) {
        n = newName(t);
    } else if (name(n).defn!=PREDEFINED) {
        ERRMSG(l) "Redeclaration of foreign \"%s\"", textToStr(t)
        EEND;
    }
    name(n).line = l;
    name(n).defn = NIL;  /* nothing to say */
    name(n).type = type;
    foreignExports = cons(n,foreignExports);
}

static Void local checkForeignExport(p)       /* Check foreign export      */
Name p; {
    emptySubstitution();
    name(p).type = checkSigType(name(p).line,
                                "foreign export declaration",
                                p,
                                name(p).type);
    implementForeignExport(p);
}

/* --------------------------------------------------------------------------
 * Static analysis of patterns:
 *
 * Patterns are parsed as ordinary (atomic) expressions.  Static analysis
 * makes the following checks:
 *  - Patterns are well formed (according to pattern syntax), including the
 *    special case of (n+k) patterns.
 *  - All constructor functions have been defined and are used with the
 *    correct number of arguments.
 *  - No variable name is used more than once in a pattern.
 *
 * The list of pattern variables occuring in each pattern is accumulated in
 * a global list `patVars', which must be initialised to NIL at appropriate
 * points before using these routines to check for valid patterns.  This
 * mechanism enables the pattern checking routine to be mapped over a list
 * of patterns, ensuring that no variable occurs more than once in the
 * complete pattern list (as is required on the lhs of a function defn).
 * ------------------------------------------------------------------------*/

static List patVars;                    /* List of vars bound in pattern   */

static Cell local checkPat(line,p)      /* Check valid pattern syntax      */
Int  line;
Cell p; {
    switch (whatIs(p)) {
        case VARIDCELL :
        case VAROPCELL : addPatVar(line,p);
                         break;

        case AP        : return checkMaybeCnkPat(line,p);

        case NAME      :
        case QUALIDENT : 
        case CONIDCELL :
        case CONOPCELL : return checkApPat(line,0,p);

        case WILDCARD  :
        case STRCELL   :
        case CHARCELL  :
        case INTCELL   : 
        case BIGCELL   : 
        case FLOATCELL : break;

        case ASPAT     : addPatVar(line,fst(snd(p)));
                         snd(snd(p)) = checkPat(line,snd(snd(p)));
                         break;

        case LAZYPAT   : snd(p) = checkPat(line,snd(p));
                         break;

        case FINLIST   : map1Over(checkPat,line,snd(p));
                         break;

        case CONFLDS   : depConFlds(line,p,TRUE);
                         break;

        case ESIGN     : {   Type t   = snd(snd(p));
                             List tvs = typeVarsIn(t,NIL,NIL);
                             for (; nonNull(tvs); tvs=tl(tvs)) {
                                 Int beta    = newKindvars(1);
                                 hd(btyvars) = cons(pair(hd(tvs),mkInt(beta)),
                                                    hd(btyvars));
                             }
                             t = checkSigType(line,
                                              "pattern type",
                                              fst(snd(p)),
                                              t);
                             if (isPolyType(t) 
                                 || whatIs(t)==QUAL
                                 || whatIs(t)==RANK2) {
                                 ERRMSG(line)
                                  "Illegal type in pattern annotation"
                                 EEND;
                             }
                             snd(snd(p)) = t;
                             fst(snd(p)) = checkPat(line,fst(snd(p)));
                         }
                         break;

        default        : ERRMSG(line) "Illegal pattern syntax"
                         EEND;
    }
    return p;
}

static Cell local checkMaybeCnkPat(l,p) /* Check applicative pattern with  */
Int  l;                                 /* the possibility of n+k pattern  */
Cell p; {
#if NPLUSK
    Cell h = getHead(p);

    if (argCount==2 && isVar(h) && textOf(h)==textPlus) {       /* n+k     */
        Cell v = arg(fun(p));
        if (!isInt(arg(p)) && !isBignum(arg(p))) {
                ERRMSG(l) "Second argument in (n+k) pattern must be an integer"
                EEND;
        }
#if 0 /* can't call intOf - it might be a bignum */
        if (intOf(arg(p))<=0) {
                ERRMSG(l) "Integer k in (n+k) pattern must be > 0"
                EEND;
        }
#endif
        overwrite2(fun(p),ADDPAT,arg(p));
        arg(p)           = checkPat(l,v);
        return p;
    }
#endif
    return checkApPat(l,0,p);
}

static Cell local checkApPat(line,args,p)
Int  line;                              /* check validity of application   */
Int  args;                              /* of constructor to arguments     */
Cell p; {
    switch (whatIs(p)) {
        case AP        : fun(p) = checkApPat(line,args+1,fun(p));
                         arg(p) = checkPat(line,arg(p));
                         break;

        case TUPLE     : if (tupleOf(p)!=args) {
                             ERRMSG(line) "Illegal tuple pattern"
                             EEND;
                         }
                         break;

#if TREX
        case EXT       : if (args!=2) {
                             ERRMSG(line) "Illegal record pattern"
                             EEND;
                         }
                         break;
#endif

        case QUALIDENT : 
                if (!isQCon(p)) {
                    ERRMSG(line) "Illegal use of qualified variable in pattern"
                    EEND;
                }
                /* deliberate fall through */
        case CONIDCELL :
        case CONOPCELL : p = conDefined(line,p);
                         checkCfunArgs(line,p,args);
                         break;

        case NAME      : checkIsCfun(line,p);
                         checkCfunArgs(line,p,args);
                         break;

        default        : ERRMSG(line) "Illegal pattern syntax"
                         EEND;
    }
    return p;
}

static Void local addPatVar(line,v)     /* add variable v to list of vars  */
Int  line;                              /* in current pattern, checking for*/
Cell v; {                               /* repeated variables.             */
     Text t = textOf(v);
     List p = NIL;
     List n = patVars;

     for (; nonNull(n); p=n, n=tl(n)) {
         if (textOf(hd(n))==t) {
             ERRMSG(line) "Repeated variable \"%s\" in pattern",
                          textToStr(t)
             EEND;
         }
     }
     if (isNull(p)) {
         patVars = cons(v,NIL);
     } else {
         tl(p)   = cons(v,NIL);
     }
}

static Name local conDefined(line,nm)   /* check that nm is the name of a  */
Int  line;                              /* previously defined constructor  */
Cell nm; {                              /* function.                       */
    Cell c=findQualName(line,nm);
    if (isNull(c)) {
        ERRMSG(line) "Undefined constructor function \"%s\"", identToStr(nm)
        EEND;
    }
    checkIsCfun(line,c);
    return c;
}

static Void local checkIsCfun(line,c)   /* Check that c is a constructor fn*/
Int  line;
Name c; {
    if (!isCfun(c)) {
        ERRMSG(line) "\"%s\" is not a constructor function",
                     textToStr(name(c).text)
        EEND;
    }
}

static Void local checkCfunArgs(line,c,args)
Int  line;                              /* Check constructor applied with  */
Cell c;                                 /* correct number of arguments     */
Int  args; {
    if (name(c).arity!=args) {
        ERRMSG(line) "Constructor function \"%s\" needs %d args in pattern",
                     textToStr(name(c).text), name(c).arity
        EEND;
    }
}

static Cell local applyBtyvs(pat)       /* Record bound type vars in pat   */
Cell pat; {
    List bts = hd(btyvars);
    btyvars  = tl(btyvars);
    if (nonNull(bts)) {
        pat = ap(BIGLAM,pair(bts,pat));
        for (; nonNull(bts); bts=tl(bts)) {
            snd(hd(bts)) = copyKindvar(intOf(snd(hd(bts))));
        }
    }
    return pat;
}

/* --------------------------------------------------------------------------
 * Maintaining lists of bound variables and local definitions, for
 * dependency and scope analysis.
 * ------------------------------------------------------------------------*/

static List bounds;                     /* list of lists of bound vars     */
static List bindings;                   /* list of lists of binds in scope */
static List depends;                    /* list of lists of dependents     */

#define saveBvars()      hd(bounds)     /* list of bvars in current scope  */
#define restoreBvars(bs) hd(bounds)=bs  /* restore list of bound variables */

static Cell local bindPat(line,p)       /* add new bound vars for pattern  */
Int  line;
Cell p; {
    patVars    = NIL;
    p          = checkPat(line,p);
    hd(bounds) = revOnto(patVars,hd(bounds));
    return p;
}

static Void local bindPats(line,ps)     /* add new bound vars for patterns */
Int  line;
List ps; {
    patVars    = NIL;
    map1Over(checkPat,line,ps);
    hd(bounds) = revOnto(patVars,hd(bounds));
}

/* --------------------------------------------------------------------------
 * Before processing value and type signature declarations, all data and
 * type definitions have been processed so that:
 * - all valid type constructors (with their arities) are known.
 * - all valid constructor functions (with their arities and types) are
 *   known.
 *
 * The result of parsing a list of value declarations is a list of Eqns:
 *       Eqn ::= (SIGDECL,(Line,[Var],type))  |  (Expr,Rhs)
 * The ordering of the equations in this list is the reverse of the original
 * ordering in the script parsed.  This is a consequence of the structure of
 * the parser ... but also turns out to be most convenient for the static
 * analysis.
 *
 * As the first stage of the static analysis of value declarations, each
 * list of Eqns is converted to a list of Bindings.  As part of this
 * process:
 * - The ordering of the list of Bindings produced is the same as in the
 *   original script.
 * - When a variable (function) is defined over a number of lines, all
 *   of the definitions should appear together and each should give the
 *   same arity to the variable being defined.
 * - No variable can have more than one definition.
 * - For pattern bindings:
 *   - Each lhs is a valid pattern/function lhs, all constructor functions
 *     have been defined and are used with the correct number of arguments.
 *   - Each lhs contains no repeated pattern variables.
 *   - Each equation defines at least one variable (e.g. True = False is
 *     not allowed).
 * - Types appearing in type signatures are well formed:
 *    - Type constructors used are defined and used with correct number
 *      of arguments.
 *    - type variables are replaced by offsets, type constructor names
 *      by Tycons.
 * - Every variable named in a type signature declaration is defined by
 *   one or more equations elsewhere in the script.
 * - No variable has more than one type declaration.
 *
 * ------------------------------------------------------------------------*/

#define bindingType(b) fst(snd(b))      /* type (or types) for binding     */
#define fbindAlts(b)   snd(snd(b))      /*alternatives for function binding*/

static List local extractSigdecls(es)   /* extract the SIGDECLS from list  */
List es; {                              /* of equations                    */
    List sigDecls  = NIL;               /* :: [(Line,[Var],Type)]          */

    for(; nonNull(es); es=tl(es)) {
        if (fst(hd(es))==SIGDECL) {                  /* type-declaration?  */
            Pair sig  = snd(hd(es));
            Int  line = intOf(fst3(sig));
            List vs   = snd3(sig);
            for(; nonNull(vs); vs=tl(vs)) {
                if (isQualIdent(hd(vs))) {
                    ERRMSG(line) "Type signature for qualified variable \"%s\" is not allowed",
                                 identToStr(hd(vs))
                    EEND;
                }
            }
            sigDecls = cons(sig,sigDecls);          /* discard SIGDECL tag */
        }
    }
    return sigDecls;
}

static List local extractBindings(es)   /* extract untyped bindings from   */
List es; {                              /* given list of equations         */
    Cell lastVar   = NIL;               /* = var def'd in last eqn (if any)*/
    Int  lastArity = 0;                 /* = number of args in last defn   */
    List bs        = NIL;               /* :: [Binding]                    */

    for(; nonNull(es); es=tl(es)) {
        Cell e = hd(es);

        if (fst(e)!=SIGDECL) {
            Int  line    = rhsLine(snd(e));
            Cell lhsHead = getHead(fst(e));

            switch (whatIs(lhsHead)) {
                case VARIDCELL :
                case VAROPCELL : {                    /* function-binding? */
                    Cell newAlt = pair(getArgs(fst(e)), snd(e));
                    if (nonNull(lastVar) && textOf(lhsHead)==textOf(lastVar)) {
                        if (argCount!=lastArity) {
                            ERRMSG(line)
                                "Equations give different arities for \"%s\"",
                                textToStr(textOf(lhsHead))
                            EEND;
                        }
                        fbindAlts(hd(bs)) = cons(newAlt,fbindAlts(hd(bs)));
                    }
                    else {
                        lastVar   = lhsHead;
                        lastArity = argCount;
                        notDefined(line,bs,lhsHead);
                        bs        = cons(pair(lhsHead,
                                              pair(NIL,
                                                   singleton(newAlt))),
                                         bs);
                    }
                }
                break;

            case QUALIDENT: if (isQVar(lhsHead)) {
            ERRMSG(line) "Binding for qualified variable \"%s\" not allowed",
                         identToStr(lhsHead)
            EEND;
        }
        break;
        /* deliberate fall through */
#if TREX
                case EXT       :
#endif
                case CONFLDS   :
                case CONOPCELL :
                case CONIDCELL :
                case FINLIST   :
                case TUPLE     :
                case NAME      :
                case LAZYPAT   : 
                case ASPAT     : lastVar = NIL;       /* pattern-binding?  */
                                 patVars = NIL;
                                 enterBtyvs();
                                 fst(e)  = checkPat(line,fst(e));
                                 if (isNull(patVars)) {
                                     ERRMSG(line)
                                       "No variables defined in lhs pattern"
                                     EEND;
                                 }
                                 map2Proc(notDefined,line,bs,patVars);
                                 bs = cons(pair(patVars,pair(NIL,e)),bs);
                                 if (nonNull(hd(btyvars))) {
                                     ERRMSG(line)
                                      "Sorry, no type variables are allowed in pattern binding type annotations"
                                     EEND;
                                 }
                                 leaveBtyvs();
                                 break;

                default        : ERRMSG(line) "Improper left hand side"
                                 EEND;
            }
        }
    }
    return bs;
}

static List local eqnsToBindings(es)    /*Convert list of equations to list*/
List es; {                              /*of typed bindings                */
    List bs = extractBindings(es);
    map1Proc(addSigDecl,bs,extractSigdecls(es));
    return bs;
}

static Void local notDefined(line,bs,v) /* check if name already defined in*/
Int  line;                              /* list of bindings                */
List bs;
Cell v; {
    if (nonNull(findBinding(textOf(v),bs))) {
        ERRMSG(line) "\"%s\" multiply defined", textToStr(textOf(v))
        EEND;
    }
}

static Cell local findBinding(t,bs)     /* look for binding for variable t */
Text t;                                 /* in list of bindings bs          */
List bs; {
    for (; nonNull(bs); bs=tl(bs)) {
        if (isVar(fst(hd(bs)))) {                     /* function-binding? */
            if (textOf(fst(hd(bs)))==t) {
                return hd(bs);
            }
        } else if (nonNull(varIsMember(t,fst(hd(bs))))) { /* pattern-binding?  */
            return hd(bs);
        }
    }
    return NIL;
}

static Void local addSigDecl(bs,sigDecl)/* add type information to bindings*/
List bs;                                /* :: [Binding]                    */
Cell sigDecl; {                         /* :: (Line,[Var],Type)            */
    Int  line = intOf(fst3(sigDecl));
    Cell vs   = snd3(sigDecl);
    Cell type = checkSigType(line,"type declaration",hd(vs),thd3(sigDecl));

    map3Proc(setType,line,type,bs,vs);
}

static Void local setType(line,type,bs,v)
Int  line;                              /* Set type of variable            */
Cell type;
Cell v;
List bs; {
    Text t = textOf(v);
    Cell b = findBinding(t,bs);

    if (isNull(b)) {
        ERRMSG(line) "Type declaration for variable \"%s\" with no body",
                     textToStr(t)
        EEND;
    }

    if (isVar(fst(b))) {                              /* function-binding? */
        if (isNull(bindingType(b))) {
            bindingType(b) = type;
            return;
        }
    } else {                                          /* pattern-binding?  */
        List vs = fst(b);
        List ts = bindingType(b);

        if (isNull(ts)) {
            bindingType(b) = ts = replicate(length(vs),NIL);
        }
        while (nonNull(vs) && t!=textOf(hd(vs))) {
            vs = tl(vs);
            ts = tl(ts);
        }

        if (nonNull(vs) && isNull(hd(ts))) {
            hd(ts) = type;
            return;
        }
    }

    ERRMSG(line) "Repeated type declaration for \"%s\"", textToStr(t)
    EEND;
}

/* --------------------------------------------------------------------------
 * To facilitate dependency analysis, lists of bindings are temporarily
 * augmented with an additional field, which is used in two ways:
 * - to build the `adjacency lists' for the dependency graph. Represented by
 *   a list of pointers to other bindings in the same list of bindings.
 * - to hold strictly positive integer values (depth first search numbers) of
 *   elements `on the stack' during the strongly connected components search
 *   algorithm, or a special value mkInt(0), once the binding has been added
 *   to a particular strongly connected component.
 *
 * Using this extra field, the type of each list of declarations during
 * dependency analysis is [Binding'] where:
 *
 *    Binding' ::= (Var, (Dep, (Type, [Alt])))         -- function binding
 *              |  ([Var], (Dep, ([Type], (Pat,Rhs)))) -- pattern binding
 *
 * ------------------------------------------------------------------------*/

#define depVal(d) (fst(snd(d)))         /* Access to dependency information*/
                                                                           
static List local dependencyAnal(bs)    /* Separate lists of bindings into */
List bs; {                              /* mutually recursive groups in    */
                                        /* order of dependency             */
                                                                           
    mapProc(addDepField,bs);            /* add extra field for dependents  */
    mapProc(depBinding,bs);             /* find dependents of each binding */
    bs = bscc(bs);                      /* sort to strongly connected comps*/
    mapProc(remDepField,bs);            /* remove dependency info field    */
    return bs;                                                             
}                                                                          
                                                                           
static List local topDependAnal(bs)     /* Like dependencyAnal(), but at   */
List bs; {                              /* top level, reporting on progress*/
    List xs;                                                               
    Int  i = 0;                                                            
                                                                           
    setGoal("Dependency analysis",(Target)(length(bs)));                   
    mapProc(addDepField,bs);            /* add extra field for dependents  */
    for (xs=bs; nonNull(xs); xs=tl(xs)) {                                  
        emptySubstitution();                                               
        depBinding(hd(xs));                                                
        soFar((Target)(i++));                                              
    }                                                                      
    bs = bscc(bs);                      /* sort to strongly connected comps*/
    mapProc(remDepField,bs);            /* remove dependency info field    */
    done();                                                                
    return bs;                                                             
}                                                                          
                                                                           
static Void local addDepField(b)        /* add extra field to binding to   */
Cell b; {                               /* hold list of dependents         */
    snd(b) = pair(NIL,snd(b));
}

static Void local remDepField(bs)       /* remove dependency field from    */
List bs; {                              /* list of bindings                */
    mapProc(remDepField1,bs);                                              
}                                                                          
                                                                           
static Void local remDepField1(b)       /* remove dependency field from    */
Cell b; {                               /* single binding                  */
    snd(b) = snd(snd(b));                                                  
}                                                                          
                                                                           
static Void local clearScope() {        /* initialise dependency scoping   */
    bounds   = NIL;                                                        
    bindings = NIL;                                                        
    depends  = NIL;                                                        
}                                                                          
                                                                           
static Void local withinScope(bs)       /* enter scope of bindings bs      */
List bs; {                                                                 
    bounds   = cons(NIL,bounds);                                           
    bindings = cons(bs,bindings);                                          
    depends  = cons(NIL,depends);                                          
}                                                                          
                                                                           
static Void local leaveScope() {        /* leave scope of last withinScope */
    bounds   = tl(bounds);
    bindings = tl(bindings);
    depends  = tl(depends);
}

/* --------------------------------------------------------------------------
 * As a side effect of the dependency analysis we also make the following
 * checks:
 * - Each lhs is a valid pattern/function lhs, all constructor functions
 *   have been defined and are used with the correct number of arguments.
 * - No lhs contains repeated pattern variables.
 * - Expressions used on the rhs of an eqn should be well formed.  This
 *   includes:
 *   - Checking for valid patterns (including repeated vars) in lambda,
 *     case, and list comprehension expressions.
 *   - Recursively checking local lists of equations.
 * - No free (i.e. unbound) variables are used in the declaration list.
 * ------------------------------------------------------------------------*/

static Void local depBinding(b)         /* find dependents of binding      */
Cell b; {
    Cell defpart = snd(snd(snd(b)));    /* definition part of binding      */

    hd(depends) = NIL;

    if (isVar(fst(b))) {                /* function-binding?               */
        mapProc(depAlt,defpart);
        if (isNull(fst(snd(snd(b))))) { /* Save dep info for implicitly    */
            fst(snd(snd(b))) = ap(IMPDEPS,hd(depends)); /* typed var binds */
        }
    } else {                            /* pattern-binding?                */
        depRhs(snd(defpart));
    }
    depVal(b) = hd(depends);
}

static Void local depDefaults(c)        /* dependency analysis on defaults */
Class c; {                              /* from class definition           */
    depClassBindings(cclass(c).defaults);
}

static Void local depInsts(in)          /* dependency analysis on instance */
Inst in; {                              /* bindings                        */
    depClassBindings(inst(in).implements);
}

static Void local depClassBindings(bs)  /* dependency analysis on list of  */
List bs; {                              /* bindings, possibly containing   */
    for (; nonNull(bs); bs=tl(bs)) {    /* NIL bindings ...                */
        if (nonNull(hd(bs))) {          /* No need to add extra field for  */
           mapProc(depAlt,snd(hd(bs))); /* dependency information ...      */
        }
    }
}

static Void local depAlt(a)             /* Find dependents of alternative  */
Cell a; {
    List obvs = saveBvars();            /* Save list of bound variables    */
    enterBtyvs();
    bindPats(rhsLine(snd(a)),fst(a));   /* add new bound vars for patterns */
    depRhs(snd(a));                     /* find dependents of rhs          */
    fst(a)    = applyBtyvs(fst(a));
    restoreBvars(obvs);                 /* restore original list of bvars  */
}

static Void local depRhs(r)             /* Find dependents of rhs          */
Cell r; {
    switch (whatIs(r)) {
        case GUARDED : mapProc(depGuard,snd(r));
                       break;

        case LETREC  : fst(snd(r)) = eqnsToBindings(fst(snd(r)));
                       withinScope(fst(snd(r)));
                       fst(snd(r)) = dependencyAnal(fst(snd(r)));
                       hd(depends) = fst(snd(r));
                       depRhs(snd(snd(r)));
                       leaveScope();
                       break;

        default      : snd(r) = depExpr(intOf(fst(r)),snd(r));
                       break;
    }
}

static Void local depGuard(g)           /*find dependents of single guarded*/
Cell g; {                               /* expression                      */
    depPair(intOf(fst(g)),snd(g));
}

static Cell local depExpr(line,e)       /* find dependents of expression   */
Int  line;
Cell e; {
    switch (whatIs(e)) {

        case VARIDCELL  :
        case VAROPCELL  : return depVar(line,e);

        case CONIDCELL  :
        case CONOPCELL  : return conDefined(line,e);

        case QUALIDENT  : if (isQVar(e)) {
                              return depQVar(line,e);
                          } else { /* QConOrConOp */
                              return conDefined(line,e);
                          }

#if TREX
        case RECSEL     : break;

        case AP         : if (isAp(e) && isAp(fun(e)) && isExt(fun(fun(e)))) {
                              return depRecord(line,e);
                          } else {
                              Cell nx = e;
                              Cell a;
                              do {
                                  a      = nx;
                                  arg(a) = depExpr(line,arg(a));
                                  nx     = fun(a);
                              } while (isAp(nx));
                              fun(a) = depExpr(line,fun(a));
                          }
                          break;
#else
        case AP         : depPair(line,e);
                          break;
#endif

        case NAME       :
        case TUPLE      :
        case STRCELL    :
        case CHARCELL   :
        case INTCELL    : 
        case BIGCELL    : 
        case FLOATCELL  : break;

        case COND       : depTriple(line,snd(e));
                          break;

        case FINLIST    : map1Over(depExpr,line,snd(e));
                          break;

        case LETREC     : fst(snd(e)) = eqnsToBindings(fst(snd(e)));
                          withinScope(fst(snd(e)));
                          fst(snd(e)) = dependencyAnal(fst(snd(e)));
                          hd(depends) = fst(snd(e));
                          snd(snd(e)) = depExpr(line,snd(snd(e)));
                          leaveScope();
                          break;

        case LAMBDA     : depAlt(snd(e));
                          break;

        case DOCOMP     : /* fall-thru */
        case COMP       : depComp(line,snd(e),snd(snd(e)));
                          break;

        case ESIGN      : fst(snd(e)) = depExpr(line,fst(snd(e)));
                          snd(snd(e)) = checkSigType(line,
                                                     "expression",
                                                     fst(snd(e)),
                                                     snd(snd(e)));
                          break;

        case CASE       : fst(snd(e)) = depExpr(line,fst(snd(e)));
                          map1Proc(depCaseAlt,line,snd(snd(e)));
                          break;

        case CONFLDS    : depConFlds(line,e,FALSE);
                          break;

        case UPDFLDS    : depUpdFlds(line,e);
                          break;

        case ASPAT      : ERRMSG(line) "Illegal `@' in expression"
                          EEND;

        case LAZYPAT    : ERRMSG(line) "Illegal `~' in expression"
                          EEND;

        case WILDCARD   : ERRMSG(line) "Illegal `_' in expression"
                          EEND;

#if TREX
        case EXT        : ERRMSG(line) "Illegal application of record"
                          EEND;
#endif

        default         : internal("in depExpr");
   }
   return e;
}

static Void local depPair(line,e)       /* find dependents of pair of exprs*/
Int  line;
Cell e; {
    fst(e) = depExpr(line,fst(e));
    snd(e) = depExpr(line,snd(e));
}

static Void local depTriple(line,e)     /* find dependents of triple exprs */
Int  line;
Cell e; {
    fst3(e) = depExpr(line,fst3(e));
    snd3(e) = depExpr(line,snd3(e));
    thd3(e) = depExpr(line,thd3(e));
}

static Void local depComp(l,e,qs)       /* find dependents of comprehension*/
Int  l;
Cell e;
List qs; {
    if (isNull(qs))
        fst(e) = depExpr(l,fst(e));
    else {
        Cell q   = hd(qs);
        List qs1 = tl(qs);
        switch (whatIs(q)) {
            case FROMQUAL : {   List obvs   = saveBvars();
                                snd(snd(q)) = depExpr(l,snd(snd(q)));
                                enterBtyvs();
                                fst(snd(q)) = bindPat(l,fst(snd(q)));
                                depComp(l,e,qs1);
                                fst(snd(q)) = applyBtyvs(fst(snd(q)));
                                restoreBvars(obvs);
                            }
                            break;

            case QWHERE   : snd(q)      = eqnsToBindings(snd(q));
                            withinScope(snd(q));
                            snd(q)      = dependencyAnal(snd(q));
                            hd(depends) = snd(q);
                            depComp(l,e,qs1);
                            leaveScope();
                            break;

            case DOQUAL   : /* fall-thru */
            case BOOLQUAL : snd(q) = depExpr(l,snd(q));
                            depComp(l,e,qs1);
                            break;
        }
    }
}

static Void local depCaseAlt(line,a)    /* Find dependents of case altern. */
Int  line;
Cell a; {
    List obvs = saveBvars();            /* Save list of bound variables    */
    enterBtyvs();
    fst(a)    = bindPat(line,fst(a));   /* Add new bound vars for pats     */
    depRhs(snd(a));                     /* Find dependents of rhs          */
    fst(a)    = applyBtyvs(fst(a));
    restoreBvars(obvs);                 /* Restore original list of bvars  */
}

static Cell local depVar(line,e)        /* Register occurrence of variable */
Int line;
Cell e; {
    List bounds1   = bounds;
    List bindings1 = bindings;
    List depends1  = depends;
    Text t         = textOf(e);
    Cell n;

    while (nonNull(bindings1)) {
        n = varIsMember(t,hd(bounds1));   /* look for t in bound variables */
        if (nonNull(n)) {
            return n;
        }
        n = findBinding(t,hd(bindings1)); /* look for t in var bindings    */
        if (nonNull(n)) {
           if (!cellIsMember(n,hd(depends1)))
               hd(depends1) = cons(n,hd(depends1));
           return (isVar(fst(n)) ? fst(n) : e);
        }

        bounds1   = tl(bounds1);
        bindings1 = tl(bindings1);
        depends1  = tl(depends1);
    }

    if (isNull(n=findName(t))) {               /* check global definitions */
        ERRMSG(line) "Undefined variable \"%s\"", textToStr(t)
        EEND;
    }

    if (name(n).mod != thisModule) {
        return n;
    }
    /* Later phases of the system cannot cope if we resolve references
     * to unprocessed objects too early.  This is the main reason that
     * we cannot cope with recursive modules at the moment.
     */
    return n;
}

static Cell local depQVar(line,e)/* register occurrence of qualified variable */
Int line;
Cell e; {
    Cell n = findQualName(line,e);
    if (isNull(n)) {                            /* check global definitions */
        ERRMSG(line) "Undefined qualified variable \"%s\"", identToStr(e)
        EEND;
    }
    if (name(n).mod != currentModule) {
        return n;
    }
    if (fst(e) == VARIDCELL) {
        e = mkVar(qtextOf(e));
    } else {
        e = mkVarop(qtextOf(e));
    }
    return depVar(line,e);
}

static Void local depConFlds(line,e,isP)/* check construction using fields */
Int  line;
Cell e;
Bool isP; {
    Name c = conDefined(line,fst(snd(e)));
    if (isNull(snd(snd(e))) ||
        nonNull(cellIsMember(c,depFields(line,e,snd(snd(e)),isP)))) {
        fst(snd(e)) = c;
    } else {
        ERRMSG(line) "Constructor \"%s\" does not have selected fields in ",
                     textToStr(name(c).text)
        ETHEN ERREXPR(e);
        ERRTEXT "\n"
        EEND;
    }
    if (!isP && isPair(name(c).defn)) { /* Check that banged fields defined*/
        List scs = fst(name(c).defn);   /* List of strict components       */
        Type t   = name(c).type;
        Int  a   = name(c).arity;
        List fs  = snd(snd(e));
        List ss;
        if (isPolyType(t)) {            /* Find tycon that c belongs to    */
            t = monotypeOf(t);
        }
        if (whatIs(t)==QUAL) {
            t = snd(snd(t));
        }
        while (0<a--) {
            t = arg(t);
        }
        while (isAp(t)) {
            t = fun(t);
        }
        for (ss=tycon(t).defn; hasCfun(ss); ss=tl(ss)) {
        }
        /* Now we know the tycon t that c belongs to, and the corresponding
         * list of selectors for that type, ss.  Now we have to check that
         * each of the fields identified by scs appears in fs, using ss to
         * cross reference, and convert integers to selector names.
         */
        for (; nonNull(scs); scs=tl(scs)) {
            Int  i   = intOf(hd(scs));
            List ss1 = ss;
            for (; nonNull(ss1); ss1=tl(ss1)) {
                List cns = name(hd(ss1)).defn;
                for (; nonNull(cns); cns=tl(cns)) {
                    if (fst(hd(cns))==c) {
                        break;
                    }
                }
                if (nonNull(cns) && intOf(snd(hd(cns)))==i) {
                    break;
                }
            }
            if (isNull(ss1)) {
                internal("depConFlds");
            } else {
                Name s   = hd(ss1);
                List fs1 = fs;
                for (; nonNull(fs1) && s!=fst(hd(fs1)); fs1=tl(fs1)) {
                }
                if (isNull(fs1)) {
                    ERRMSG(line) "Construction does not define strict field"
                    ETHEN
                    ERRTEXT      "\nExpression : " ETHEN ERREXPR(e);
                    ERRTEXT      "\nField      : " ETHEN ERREXPR(s);
                    ERRTEXT      "\n"
                    EEND;
                }
            }
        }
    }
}

static Void local depUpdFlds(line,e)    /* check update using fields       */
Int  line;
Cell e; {
    if (isNull(thd3(snd(e)))) {
        ERRMSG(line) "Empty field list in update"
        EEND;
    }
    fst3(snd(e)) = depExpr(line,fst3(snd(e)));
    snd3(snd(e)) = depFields(line,e,thd3(snd(e)),FALSE);
}

static List local depFields(l,e,fs,isP) /* check field binding list        */
Int  l;
Cell e;
List fs;
Bool isP; {
    List cs = NIL;
    List ss = NIL;

    for (; nonNull(fs); fs=tl(fs)) {    /* for each field binding          */
        Cell fb = hd(fs);
        Name s;

        if (isVar(fb)) {                /* expand  var  to  var = var      */
            fb = hd(fs) = pair(fb,fb);
        }
        s = findQualName(l,fst(fb));    /* check for selector              */
        if (nonNull(s) && isSfun(s)) {
            fst(fb) = s;
        } else {
            ERRMSG(l) "\"%s\" is not a selector function/field name",
                      textToStr(textOf(fst(fb)))
            EEND;
        }

        if (isNull(ss)) {               /* for first named selector        */
            List scs = name(s).defn;    /* calculate list of constructors  */
            for (; nonNull(scs); scs=tl(scs))
                cs = cons(fst(hd(scs)),cs);
            ss = singleton(s);          /* initialize selector list        */
        } else {                        /* for subsequent selectors        */
            List ds = cs;               /* intersect constructor lists     */
            for (cs=NIL; nonNull(ds); ) {
                List scs = name(s).defn;
                while (nonNull(scs) && fst(hd(scs))!=hd(ds)) {
                    scs = tl(scs);
                }
                if (isNull(scs)) {
                    ds = tl(ds);
                } else {
                    List next = tl(ds);
                    tl(ds)    = cs;
                    cs        = ds;
                    ds        = next;
                }
            }

            if (cellIsMember(s,ss)) {   /* check for repeated uses         */
                ERRMSG(l) "Repeated field name \"%s\" in field list",
                          textToStr(name(s).text)
                EEND;
            }
            ss = cons(s,ss);
        }

        if (isNull(cs)) {               /* Are there any matching constrs? */
            ERRMSG(l) "No constructor has all of the fields specified in "
            ETHEN ERREXPR(e);
            ERRTEXT "\n"
            EEND;
        }

        snd(fb) = (isP ? checkPat(l,snd(fb)) : depExpr(l,snd(fb)));
    }
    return cs;
}

#if TREX
static Cell local depRecord(line,e)     /* find dependents of record and   */
Int  line;                              /* sort fields into approp. order  */
Cell e; {                               /* to make construction and update */
    List exts = NIL;                    /* more efficient.                 */
    Cell r    = e;

    do {                                /* build up list of extensions     */
        Text   t    = extText(fun(fun(r)));
        String s    = textToStr(t);
        List   prev = NIL;
        List   nx   = exts;
        while (nonNull(nx) && strcmp(textToStr(extText(fun(fun(nx)))),s)>0) {
            prev = nx;
            nx   = extRow(nx);
        }
        if (nonNull(nx) && t==extText(fun(fun(nx)))) {
            ERRMSG(line) "Repeated label \"%s\" in record ", s
            ETHEN ERREXPR(e);
            ERRTEXT "\n"
            EEND;
        }
        if (isNull(prev)) {
            exts = cons(fun(r),exts);
        } else {
            tl(prev) = cons(fun(r),nx);
        }
        extField(r) = depExpr(line,extField(r));
        r           = extRow(r);
    } while (isAp(r) && isAp(fun(r)) && isExt(fun(fun(r))));
    r = depExpr(line,r);
    return revOnto(exts,r);
}
#endif

/* --------------------------------------------------------------------------
 * Several parts of this program require an algorithm for sorting a list
 * of values (with some added dependency information) into a list of strongly
 * connected components in which each value appears before its dependents.
 *
 * Each of these algorithms is obtained by parameterising a standard
 * algorithm in "scc.c" as shown below.
 * ------------------------------------------------------------------------*/

#define  SCC2            tcscc          /* make scc algorithm for Tycons   */
#define  LOWLINK         tclowlink
#define  DEPENDS(c)      (isTycon(c) ? tycon(c).kind : cclass(c).kinds)
#define  SETDEPENDS(c,v) if(isTycon(c))tycon(c).kind=v;else cclass(c).kinds=v
#include "scc.c"
#undef   SETDEPENDS
#undef   DEPENDS
#undef   LOWLINK
#undef   SCC2

#define  SCC             bscc           /* make scc algorithm for Bindings */
#define  LOWLINK         blowlink
#define  DEPENDS(t)      depVal(t)
#define  SETDEPENDS(c,v) depVal(c)=v
#include "scc.c"
#undef   SETDEPENDS
#undef   DEPENDS
#undef   LOWLINK
#undef   SCC

/* --------------------------------------------------------------------------
 * Main static analysis:
 * ------------------------------------------------------------------------*/

Void checkExp() {                       /* Top level static check on Expr  */
    staticAnalysis(RESET);
    clearScope();                       /* Analyse expression in the scope */
    withinScope(NIL);                   /* of no local bindings            */
    inputExpr = depExpr(0,inputExpr);
    leaveScope();
    staticAnalysis(RESET);
}

Void checkDefns() {                     /* Top level static analysis       */
    staticAnalysis(RESET);
    thisModule = lastModule();
    setCurrModule(thisModule);

    /* Resolve module references */
    mapProc(checkQualImport,  module(thisModule).qualImports);
    mapProc(checkUnqualImport,unqualImports);

    /* Add implicit import declarations - if Prelude has been loaded */
    {
        Module modulePrelude = findModule(findText("Prelude"));
        if (nonNull(modulePrelude)) {
            /* Add "import Prelude" if there`s no explicit import */
            if (thisModule != modulePrelude
                && isNull(cellAssoc(modulePrelude,unqualImports))
                && isNull(cellRevAssoc(modulePrelude,module(thisModule).qualImports))) {
                unqualImports = cons(pair(modulePrelude,DOTDOT),unqualImports);
            }
            /* Add "import qualified Prelude" */
            module(thisModule).qualImports=cons(pair(conPrelude,modulePrelude),
                                                module(thisModule).qualImports);
        }
    }
    map1Proc(checkImportList, thisModule, unqualImports);

    linkPreludeTC();                    /* Get prelude tycons and classes  */
    setCurrModule(thisModule);

    mapProc(checkTyconDefn,tyconDefns); /* validate tycon definitions      */
    checkSynonyms(tyconDefns);          /* check synonym definitions       */
    mapProc(checkClassDefn,classDefns); /* process class definitions       */
    mapProc(kindTCGroup,tcscc(tyconDefns,classDefns)); /* attach kinds     */
    mapProc(addMembers,classDefns);     /* add definitions for member funs */
    mapProc(visitClass,classDefns);     /* check class hierarchy           */

    instDefns = rev(instDefns);         /* process instance definitions    */
    mapProc(checkInstDefn,instDefns);

    linkPreludeCM();                    /* Get prelude cfuns and mfuns     */
    setCurrModule(thisModule);

    mapProc(addDerivImp,derivedInsts);  /* Add impls for derived instances */
    deriveContexts(derivedInsts);       /* Calculate derived inst contexts */
#if EVAL_INSTANCES
    deriveEval(tyconDefns);             /* Derive instances of Eval        */
#endif
    tyconDefns = NIL;
    instDefns  = appendOnto(instDefns,derivedInsts);
#if EVAL_INSTANCES
    instDefns  = appendOnto(evalInsts,instDefns); /* ADR addition */
#endif
    checkDefaultDefns();                /* validate default definitions    */

    mapProc(addRSsigdecls,typeInDefns); /* add sigdecls for RESTRICTSYN    */
    valDefns = eqnsToBindings(valDefns);/* translate value equations       */
    map1Proc(opDefined,valDefns,opDefns);/*check all declared ops bound    */
    mapProc(allNoPrevDef,valDefns);     /* check against previous defns    */

    linkPreludeNames();         /* Get prelude names           */
    setCurrModule(thisModule);

    mapProc(checkForeignImport,foreignImports);	/* check foreign imports   */
    mapProc(checkForeignExport,foreignExports);	/* check foreign exports   */
    foreignImports = NIL;
    foreignExports = NIL;

    /* Every top-level name has now been created - so we can build the     */
    /* export list.  Note that this has to happen before dependency        */
    /* analysis so that references to Prelude.foo will be resolved         */
    /* when compiling the prelude.                                         */
    /* Note too that this is just a little too late to catch the use of    */
    /* qualified tycons (for the current module) in data declarations      */
    module(thisModule).exports = checkExports(thisModule,module(thisModule).exports);

    mapProc(checkTypeIn,typeInDefns);   /* check restricted synonym defns  */

    clearScope();
    withinScope(valDefns);
    valDefns = topDependAnal(valDefns); /* top level dependency ordering   */
    mapProc(depDefaults,classDefns);    /* dep. analysis on class defaults */
    mapProc(depInsts,instDefns);        /* dep. analysis on inst defns     */
    leaveScope();

    /* ToDo: evalDefaults should match current evaluation module */
    evalDefaults = defaultDefns;        /* Set defaults for evaluator      */

    staticAnalysis(RESET);
}

static Void local addRSsigdecls(pr)     /* add sigdecls from TYPE ... IN ..*/
Pair pr; {
    List vs = snd(pr);                  /* get list of variables           */
    for (; nonNull(vs); vs=tl(vs)) {
        if (fst(hd(vs))==SIGDECL) {     /* find a sigdecl                  */
            valDefns = cons(hd(vs),valDefns);   /* add to valDefns         */
            hd(vs)   = hd(snd3(snd(hd(vs))));   /* and replace with var    */
        }
    }
}

static Void local opDefined(bs,op)      /* check that op bound in bs       */
List bs;                                /* (or in current module for       */
Cell op; {                              /* constructor functions etc...)   */
    Name n;

    if (isNull(findBinding(textOf(op),bs))
           && (isNull(n=findName(textOf(op))) || name(n).mod != thisModule)) {
        ERRMSG(0) "No top level definition for operator symbol \"%s\"",
                  textToStr(textOf(op))
        EEND;
    }
}

static Void local allNoPrevDef(b)       /* ensure no previous bindings for */
Cell b; {                               /* variables in new binding        */
    if (isVar(fst(b))) {
        noPrevDef(rhsLine(snd(hd(snd(snd(b))))),fst(b));
    } else {
        Int line = rhsLine(snd(snd(snd(b))));
        map1Proc(noPrevDef,line,fst(b));
    }
}

static Void local noPrevDef(line,v)     /* ensure no previous binding for  */
Int  line;                              /* new variable                    */
Cell v; {
    Name n = findName(textOf(v));

    if (isNull(n)) {
        n            = newName(textOf(v));
        name(n).defn = PREDEFINED;
    } else if (name(n).defn!=PREDEFINED) {
        ERRMSG(line) "Attempt to redefine variable \"%s\"",
                     textToStr(name(n).text)
        EEND;
    }
    name(n).line = line;
}

static Void local duplicateError(line,mod,t,kind)/* report duplicate defn */
Int    line;
Module mod;
Text   t;
String kind; {
    if (mod == currentModule) {
        ERRMSG(line) "Repeated definition for %s \"%s\"", kind, 
            textToStr(t)
        EEND;
    } else {
        ERRMSG(line) "Definition of %s \"%s\" clashes with import", kind,
            textToStr(t)
        EEND;
    }
}

static Void local checkTypeIn(cvs)      /* Check that vars in restricted   */
Pair cvs; {                             /* synonym are defined             */
    Tycon c  = fst(cvs);
    List  vs = snd(cvs);

    for (; nonNull(vs); vs=tl(vs)) {
        if (isNull(findName(textOf(hd(vs))))) {
            ERRMSG(tycon(c).line)
                "No top level binding of \"%s\" for restricted synonym \"%s\"",
                textToStr(textOf(hd(vs))), textToStr(tycon(c).text)
            EEND;
        }
    }
}

/* --------------------------------------------------------------------------
 * Static Analysis control:
 * ------------------------------------------------------------------------*/

Void staticAnalysis(what)
Int what; {
    switch (what) {
        case RESET   : daSccs       = NIL;
                       patVars      = NIL;
                       bounds       = NIL;
                       bindings     = NIL;
                       depends      = NIL;
                       tcDeps       = NIL;
                       derivedInsts = NIL;
#if EVAL_INSTANCES
                       evalInsts    = NIL;
#endif
                       unkindTypes  = NIL;
                       thisModule   = 0;
                       break;

        case MARK    : mark(daSccs);
                       mark(patVars);
                       mark(bounds);
                       mark(bindings);
                       mark(depends);
                       mark(tcDeps);
                       mark(derivedInsts);
#if EVAL_INSTANCES
                       mark(evalInsts);
#endif
                       mark(unkindTypes);
#if TREX
                       mark(extKind);
#endif
                       break;

        case INSTALL : staticAnalysis(RESET);
#if TREX
                       extKind = pair(STAR,pair(ROW,ROW));
#endif
                       break;
    }
}

/*-------------------------------------------------------------------------*/
