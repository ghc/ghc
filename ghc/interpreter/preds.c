
/* --------------------------------------------------------------------------
 * Part of the type checker dealing with predicates and entailment
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: preds.c,v $
 * $Revision: 1.6 $
 * $Date: 1999/10/15 21:40:54 $
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Cell   local assumeEvid        Args((Cell,Int));
static List   local makePredAss       Args((List,Int));
static List   local copyPreds         Args((List));
static Void   local qualify           Args((List,Cell));
static Void   local qualifyBinding    Args((List,Cell));
static Cell   local qualifyExpr       Args((Int,List,Cell));
static Void   local overEvid          Args((Cell,Cell));

static Void   local cutoffExceeded    Args((Cell,Int,Cell,Int,List));
static Cell   local scFind            Args((Cell,Cell,Int,Cell,Int,Int));
static Cell   local scEntail          Args((List,Cell,Int,Int));
static Cell   local entail            Args((List,Cell,Int,Int));
static Cell   local inEntail          Args((List,Cell,Int,Int));
#if TREX
static Cell   local lacksNorm         Args((Type,Int,Cell));
#endif

static List   local scSimplify        Args((List));
static Void   local elimTauts         Args((Void));
static Bool   local anyGenerics       Args((Type,Int));
static List   local elimOuterPreds    Args((List));
static List   local elimPredsUsing    Args((List,List));
static Void   local reducePreds       Args((Void));
static Void   local normPreds         Args((Int));

static Bool   local resolveDefs       Args((List));
static Bool   local resolveVar        Args((Int));
static Class  local classConstraining Args((Int,Cell,Int));

/* --------------------------------------------------------------------------
 * Predicate assignments:
 *
 * A predicate assignment is represented by a list of triples (pi,o,ev)
 * where o is the offset for types in pi, with evidence required at the
 * node pointed to by ev (which is taken as a dictionary parameter if
 * no other evidence is available).  Note that the ev node will be
 * overwritten at a later stage if evidence for that predicate is found
 * subsequently.
 * ------------------------------------------------------------------------*/

static List preds;                      /* Current predicate assignment    */

static Cell local assumeEvid(pi,o)      /* Add predicate pi (offset o) to  */
Cell pi;                                /* preds with new dict var nd      */
Int  o; {
    Cell nd = inventDictVar();
    preds   = cons(triple(pi,mkInt(o),nd),preds);
    return nd;
}

static List local makePredAss(qs,o)     /* Make list of predicate assumps. */
List qs;                                /* from qs (offset o), w/ new dict */
Int  o; {                               /* vars for each predicate         */
    List result = NIL;
    for (; nonNull(qs); qs=tl(qs))
        result = cons(triple(hd(qs),mkInt(o),inventDictVar()),result);
    return rev(result);
}

static List local copyPreds(qs)         /* Copy list of predicates         */
List qs; {
    List result = NIL;
    for (; nonNull(qs); qs=tl(qs)) {
        Cell pi = hd(qs);
        result  = cons(copyPred(fst3(pi),intOf(snd3(pi))),result);
    }
    return rev(result);
}

static Void local qualify(qs,alt)       /* Add extra dictionary args to    */
List qs;                                /* qualify alt by predicates in qs */
Cell alt; {                             /* :: ([Pat],Rhs)                  */
    List ds;
    for (ds=NIL; nonNull(qs); qs=tl(qs))
        ds = cons(thd3(hd(qs)),ds);
    fst(alt) = revOnto(ds,fst(alt));
}

static Void local qualifyBinding(qs,b)  /* Add extra dict args to each     */
List qs;                                /* alternative in function binding */
Cell b ; {
    if (!isVar(fst(b)))                 /* check for function binding      */
        internal("qualifyBinding");
    map1Proc(qualify,qs,snd(snd(b)));
}

static Cell local qualifyExpr(l,ps,e)   /* Add dictionary params to expr   */
Int  l;
List ps;
Cell e; {
    if (nonNull(ps)) {                  /* Qualify input expression with   */
        if (whatIs(e)!=LAMBDA)          /* additional dictionary params    */
            e = ap(LAMBDA,pair(NIL,pair(mkInt(l),e)));
        qualify(ps,snd(e));
    }
    return e;
}

static Void local overEvid(dv,ev)       /* Overwrite dict var dv with      */
Cell dv;                                /* evidence ev                     */
Cell ev; {
    fst(dv) = nameInd;
    snd(dv) = ev;
}

/* --------------------------------------------------------------------------
 * Predicate entailment:
 *
 * Entailment plays a prominent role in the theory of qualified types, and
 * so, unsurprisingly, in the implementation too.  For practical reasons,
 * we break down entailment into two pieces.  The first, scEntail, uses
 * only the information provided by class declarations, while the second,
 * entail, also uses the information in instance declarations.
 *
 * scEntail uses the following auxiliary function to do its work:
 *
 *   scFind (e : pi') pi : Find evidence for predicate pi using only
 *                           equality of predicates, superclass entailment,
 *                           and the evidence e for pi'.
 *
 *   scFind (e : pi') pi =
 *
 *      if pi = pi' then
 *          return e
 *
 *      if (pi.class.level < pi'.class.level)
 *          get superclass entailment pi' ||- P
 *          for each (sc, pi0) in P
 *              if (ev := scFind (sc e : pi0) pi) /= NIL
 *                  return ev
 *
 *      return NIL
 *
 * This code assumes that the class hierarchy is acyclic, and that
 * each class has been assigned a `level', which is its height in
 * the hierachy.  The first of the assumptions guarantees that the
 * algorithm will terminate.  The comparison of levels is an
 * optimization that cuts down the search space: given that superclass
 * entailments can only be used to descend the hierarchy, there is no
 * way we can reach a higher level than the one that we start with,
 * and hence there is no point in looking if we reach such a position.
 *
 * scEntail extends scFind to work on whole predicate assignments:
 *
 *   scEntail P pi : Find evidence for predicate pi using the evidence
 *                   provided by the predicate assignment P, and using
 *                   only superclass entailments.
 *
 *   scEntail P pi =
 *
 *       for each (v:pi') in P
 *           if (ev := scFind (v:pi') pi) /= NIL
 *               return ev;
 *       return NIL
 *
 * ------------------------------------------------------------------------*/

Int cutoff = 16;                        /* Used to limit depth of recursion*/

static Void local cutoffExceeded(pi,o,pi1,o1,ps)
Cell pi, pi1;                           /* Display error msg when cutoff   */
Int  o,  o1;
List ps; {
    clearMarks();
    ERRMSG(0)
        "\n*** The type checker has reached the cutoff limit while trying to\n"
    ETHEN ERRTEXT
        "*** determine whether:\n***     "     ETHEN ERRPRED(copyPred(pi,o));
    ps = (isNull(pi1)) ? copyPreds(ps) : singleton(copyPred(pi1,o1));
    ERRTEXT
        "\n*** can be deduced from:\n***     " ETHEN ERRCONTEXT(ps);
    ERRTEXT
        "\n*** This may indicate that the problem is undecidable.  However,\n"
    ETHEN ERRTEXT
        "*** you may still try to increase the cutoff limit using the -c\n"
    ETHEN ERRTEXT
        "*** option and then try again.  (The current setting is -c%d)\n",
        cutoff
    EEND;
}

static Cell local scFind(e,pi1,o1,pi,o,d)/* Use superclass entailment to   */
Cell e;                                 /* find evidence for (pi,o) using  */
Cell pi1;                               /* the evidence e for (pi1,o1).    */
Int  o1;
Cell pi;
Int  o;
Int  d; {
    Class h1 = getHead(pi1);
    Class h  = getHead(pi);

    if (h==h1 && samePred(pi1,o1,pi,o))
        return e;

    if (isClass(h1) && (!isClass(h) || cclass(h).level<cclass(h1).level)) {
        Int  beta  = newKindedVars(cclass(h1).kinds);
        List scs   = cclass(h1).supers;
        List dsels = cclass(h1).dsels;
        if (!matchPred(pi1,o1,cclass(h1).head,beta))
            internal("scFind");

        if (d++ >= cutoff)
            cutoffExceeded(pi,o,pi1,o1,NIL);

        for (; nonNull(scs); scs=tl(scs), dsels=tl(dsels)) {
            Cell ev = scFind(ap(hd(dsels),e),hd(scs),beta,pi,o,d);
            if (nonNull(ev))
                return ev;
        }
    }

    return NIL;
}

static Cell local scEntail(ps,pi,o,d)   /* Calc evidence for (pi,o) from ps*/
List ps;                                /* Using superclasses and equality.*/
Cell pi;
Int  o;
Int  d; {
    for (; nonNull(ps); ps=tl(ps)) {
        Cell pi1 = hd(ps);
        Cell ev  = scFind(thd3(pi1),fst3(pi1),intOf(snd3(pi1)),pi,o,d);
        if (nonNull(ev))
            return ev;
    }
    return NIL;
}

/* --------------------------------------------------------------------------
 * Now we reach the main entailment routine:
 *
 *   entail P pi : Find evidence for predicate pi using the evidence
 *                 provided by the predicate assignment P.
 *
 *   entail P pi =
 *
 *       if (ev := scEntail P pi) /= NIL
 *           return ev;
 *
 *       if there is an instance entailment i : Q ||- pi
 *           for each pi' in Q
 *               if (ev := entail P pi') /= NIL
 *                   i := ap(i,ev)
 *               else
 *                   return NIL
 *           return i
 *
 *       return NIL;
 *
 * The form of evidence expressions produced by scEntail can be described
 * by the grammar:
 *
 *    e  =  v  |  sc e            (v = evidence var, sc = superclass sel)
 *
 * while entail extends this to include dictionary expressions given by:
 *
 *    d  =  e  |  mki d1 ... dn   (mki = dictionary constructor)
 *
 * A full grammar for evidence expressions is:
 *
 *    d   =   v   |   sc d   |   mki d1 ... dn
 *
 * and this includes evidence expressions of the form  sc (mki d1 ... dn)
 * that can never be produced by either of the entail functions described
 * above.  This is good, from a practical perspective, because t means
 * that we won't waste effort building a dictionary (mki d1 ... dn) only
 * to extract just one superclass component and throw the rest away.
 * Moreover, conditions on instance decls already guarantee that any
 * expression of this form can be rewritten in the form  mki' d1' ... dn'.
 * (Minor point: they don't guarantee that such rewritings will lead to
 * smaller terms, and hence to termination.  However, we have already
 * accepted the benefits of an undecidable entailment relation over
 * guarantees of termination, and this additional quirk is unlikely
 * to cause any further concern, except in pathological cases.)
 * ------------------------------------------------------------------------*/

static Cell local entail(ps,pi,o,d)     /* Calc evidence for (pi,o) from ps*/
List ps;                                /* Uses superclasses, equality,    */
Cell pi;                                /* tautology, and construction     */
Int  o;
Int  d; {
    Cell ev = scEntail(ps,pi,o,d);
    return nonNull(ev) ? ev : inEntail(ps,pi,o,d);
}

static Cell local inEntail(ps,pi,o,d)   /* Calc evidence for (pi,o) from ps*/
List ps;                                /* using a top-level instance      */
Cell pi;                                /* entailment                      */
Int  o;
Int  d; {
#if TREX
    if (isAp(pi) && isExt(fun(pi))) {   /* Lacks predicates                */
        Cell e  = fun(pi);
        Cell l;
        l  = lacksNorm(arg(pi),o,e);
        if (isNull(l) || isInt(l))
            return l;
        else {
            List qs = ps;
            for (; nonNull(qs); qs=tl(qs)) {
                Cell qi = fst3(hd(qs));
                if (isAp(qi) && fun(qi)==e) {
                    Cell lq = lacksNorm(arg(qi),intOf(snd3(hd(qs))),e);
                    if (isAp(lq) && intOf(fst(l))==intOf(fst(lq))) {
                        Int f = intOf(snd(l)) - intOf(snd(lq));
                        return (f==0) ? thd3(hd(qs)) : ap2(nameAddEv,
                                                           mkInt(f),
                                                           thd3(hd(qs)));
                    }
                }
            }
            return NIL;
        }
    }
    else {
#endif
    Inst in = findInstFor(pi,o);        /* Class predicates                */

    if (nonNull(in)) {
        Int  beta = typeOff;
        Cell e    = inst(in).builder;
        Cell es   = inst(in).specifics;
        if (d++ >= cutoff)
            cutoffExceeded(pi,o,NIL,0,ps);
        for (; nonNull(es); es=tl(es)) {
            Cell ev = entail(ps,hd(es),beta,d);
            if (nonNull(ev))
                e = ap(e,ev);
            else
                return NIL;
        }
        return e;
    }
    return NIL;
#if TREX
    }
#endif
}

Cell provePred(ks,ps,pi)                /* Find evidence for predicate pi  */
Kinds ks;                               /* assuming ps.  If ps is null,    */
List  ps;                               /* then we get to decide whether   */
Cell  pi; {                             /* is tautological, and we can use */
    Int  beta;                          /* the evidence as a dictionary.   */
    Cell ev;
    emptySubstitution();
    beta = newKindedVars(ks);           /* (ks provides kinds for any      */
    ps   = makePredAss(ps,beta);        /*  vars that appear in pi.)       */
    ev   = entail(ps,pi,beta,0);
    emptySubstitution();
    return ev;
}

#if TREX
static Cell local lacksNorm(t,o,e)      /* Normalize lacks pred (t,o)\l    */
Type t;                                 /* returning NIL (unsatisfiable),  */
Int  o;                                 /* Int (tautological) or pair (v,a)*/
Cell e; {                               /* such that, if e is evid for v\l,*/
    Text l = extText(e);                /* then (e+a) is evid for (t,o)\l. */
    Int  a = 0;
    for (;;) {
        Tyvar *tyv;
        deRef(tyv,t,o);
        if (tyv)
            return pair(mkInt(tyvNum(tyv)),mkInt(a));
        else {
            Cell h = getDerefHead(t,o);
            if (h==typeNoRow && argCount==0)
                return mkInt(a);
            else if (isExt(h) && argCount==2) {
                Text l1 = extText(h);
                if (l1==l)
                    return NIL;
                else if (strcmp(textToStr(l1),textToStr(l))<0)
                    a++;
                t = arg(t);
            }
            else
                return NIL;
        }
    }
}
#endif

/* --------------------------------------------------------------------------
 * Predicate set Simplification:
 *
 * Calculate a minimal equivalent subset of a given set of predicates.
 * ------------------------------------------------------------------------*/

static List local scSimplify(qs)        /* Simplify predicates in qs,      */
List qs; {                              /* returning equiv minimal subset  */
    Int n = length(qs);

    while (0<n--) {
        Cell pi = hd(qs);
        Cell ev = scEntail(tl(qs),fst3(pi),intOf(snd3(pi)),0);
        if (nonNull(ev)) {
            overEvid(thd3(pi),ev);      /* Overwrite dict var with evidence*/
            qs      = tl(qs);           /* ... and discard predicate       */
        }
        else {                          /* Otherwise, retain predicate     */
            Cell tmp = tl(qs);
            tl(qs)   = NIL;
            qs       = appendOnto(tmp,qs);
        }
    }
    return qs;
}

List simpleContext(ps,o)                /* Simplify context of skeletons   */
List ps;                                /* skeletons, offset o, using      */
Int  o; {                               /* superclass hierarchy            */
    return copyPreds(scSimplify(makePredAss(ps,o)));
}

/* --------------------------------------------------------------------------
 * Context splitting --- tautological and locally tautological predicates:
 * ------------------------------------------------------------------------*/

static Void local elimTauts() {         /* Remove tautological constraints */
    if (haskell98) {                    /* from preds                      */
        reducePreds();                  /* (or context reduce for Hask98)  */
    } else {
        List ps = preds;
        preds   = NIL;
        while (nonNull(ps)) {
            Cell pi = hd(ps);
            Cell ev = entail(NIL,fst3(pi),intOf(snd3(pi)),0);
            if (nonNull(ev)) {
                overEvid(thd3(pi),ev);
                ps = tl(ps);
            }
            else {
                List tmp = tl(ps);
                tl(ps)   = preds;
                preds    = ps;
                ps           = tmp;
            }
        }
    }
}

static Int numFixedVars = 0;            /* Number of fixed vars found      */

static Bool local anyGenerics(t,o)      /* Test for generic vars, and count*/
Type t;                                 /* fixed variables                 */
Int  o; {
    Type h = getDerefHead(t,o);         /* This code is careful to expand  */
    Int  a = argCount;                  /* synonyms; mark* & copy* do not. */
    if (isSynonym(h) && a>=tycon(h).arity) {
        expandSyn(h,a,&t,&o);
        return anyGenerics(t,o);
    }
    else {
        Tyvar* tyv;
        for (; 0<a--; t=fun(t)) {       /* cycle through any arguments     */
            deRef(tyv,t,o);
            if (anyGenerics(arg(t),o))
                return TRUE;
        }
        deRef(tyv,t,o);
        if (tyv) {
            if (tyv->offs == FIXED_TYVAR) {
                numFixedVars++;
                return FALSE;
            }
            else
                return TRUE;
        }
        else
            return FALSE;
    }
}

static List local elimOuterPreds(sps)   /* Simplify and defer any remaining*/
List sps; {                             /* preds that contain no generics. */
    List qs = NIL;
    elimTauts();
    for (preds=scSimplify(preds); nonNull(preds); ) {
        Cell pi = hd(preds);
        Cell nx = tl(preds);
        if (anyGenerics(fst3(pi),intOf(snd3(pi)))) {    /* Retain predicate*/
            tl(preds) = qs;
            qs        = preds;
        }
        else {                                          /* Defer predicate */
            tl(preds) = sps;
            sps       = preds;
        }
        preds = nx;
    }
    preds = qs;
    return sps;
}

static List local elimPredsUsing(ps,sps)/* Try to discharge or defer preds,*/
List ps;                                /* splitting if necessary to match */
List sps; {                             /* context ps.  sps = savePreds.   */
    List rems = NIL;
    while (nonNull(preds)) {            /* Pick a predicate from preds     */
        Cell p  = preds;
        Cell pi = fst3(hd(p));
        Int  o  = intOf(snd3(hd(p)));
        Cell ev = entail(ps,pi,o,0);
        preds   = tl(preds);

        if (nonNull(ev))                /* Discharge if ps ||- (pi,o)      */
            overEvid(thd3(hd(p)),ev);
        else if (!anyGenerics(pi,o)) {  /* Defer if no generics            */
            tl(p) = sps;
            sps   = p;
        }
        else {                          /* Try to split generics and fixed */
            Inst in;
            if (numFixedVars>0 && nonNull(in=findInstFor(pi,o))) {
                List qs = inst(in).specifics;
                for (ev=inst(in).builder; nonNull(qs); qs=tl(qs))
                    ev = ap(ev,assumeEvid(hd(qs),typeOff));
                overEvid(thd3(hd(p)),ev);
            }
            else {                      /* No worthwhile progress possible */
                tl(p) = rems;
                rems  = p;
            }
        }
    }
    preds = rems;                       /* Return any remaining predicates */
    return sps;
}

static Void local reducePreds() {       /* Context reduce predicates: uggh!*/
    List rems = NIL;                    /* (A last resort for defaulting)  */
    while (nonNull(preds)) {            /* Pick a predicate from preds     */
        Cell p  = preds;
        Cell pi = fst3(hd(p));
        Int  o  = intOf(snd3(hd(p)));
        Inst in = findInstFor(pi,o);
        preds   = tl(preds);
        if (nonNull(in)) {
            List qs = inst(in).specifics;
            Cell ev = inst(in).builder;
            for (; nonNull(qs); qs=tl(qs))
                ev = ap(ev,assumeEvid(hd(qs),typeOff));
            overEvid(thd3(hd(p)),ev);
        }
        else {                          /* No worthwhile progress possible */
            tl(p) = rems;
            rems  = p;
        }
    }
    preds = scSimplify(rems);           /* Return any remaining predicates */
}

static Void local normPreds(line)       /* Normalize each element of preds */
Int line; {                             /* in some appropriate manner      */
#if TREX
    List ps = preds;
    List pr = NIL;
    while (nonNull(ps)) {
        Cell pi = fst3(hd(ps));
        Cell ev = thd3(hd(ps));
        if (isAp(pi) && isExt(fun(pi))) {
            Cell r = lacksNorm(arg(pi),intOf(snd3(hd(ps))),fun(pi));
            if (isNull(r)) {
                ERRMSG(line) "Cannot satisfy constraint " ETHEN
                ERRPRED(copyPred(pi,intOf(snd3(hd(ps)))));
                ERRTEXT      "\n"
                EEND;
            }
            else if (isInt(r)) {
                overEvid(ev,r);
                ps = tl(ps);
                if (isNull(pr))
                    preds  = ps;
                else
                    tl(pr) = ps;
            }
            else if (intOf(snd(r))!=0) {
                Cell nd  = inventDictVar();
                Cell ev1 = ap2(nameAddEv,snd(r),nd);
                pi       = ap(fun(pi),aVar);
                hd(ps)   = triple(pi,fst(r),nd);
                overEvid(ev,ev1);
                pr       = ps;
                ps       = tl(ps);
            }
            else {
                fst3(hd(ps)) = ap(fun(pi),fst(r));
                pr = ps;
                ps = tl(ps);
            }
        }
        else {
            pr = ps;
            ps = tl(ps);
        }
    }
#endif
}

/* --------------------------------------------------------------------------
 * Mechanisms for dealing with defaults:
 * ------------------------------------------------------------------------*/

static Bool local resolveDefs(vs)       /* Attempt to resolve defaults  */
List vs; {                              /* for variables vs subject to  */
    List pvs       = NIL;               /* constraints in preds         */
    List qs        = preds;
    Bool defaulted = FALSE;

#ifdef DEBUG_DEFAULTS
    Printf("Attempt to resolve variables ");
    printExp(stdout,vs);
    Printf(" with context ");
    printContext(stdout,copyPreds(preds));
    Printf("\n");
#endif

    resetGenerics();                    /* find type variables in ps    */
    for (; nonNull(qs); qs=tl(qs)) {
        Cell pi = fst3(hd(qs));
        Int  o  = intOf(snd3(hd(qs)));
        for (; isAp(pi); pi=fun(pi))
            pvs = genvarType(arg(pi),o,pvs);
    }

    for (; nonNull(pvs); pvs=tl(pvs)) { /* now try defaults             */
        Int vn = intOf(hd(pvs));

#ifdef DEBUG_DEFAULTS
        Printf("is var %d included in ",vn);
        printExp(stdout,vs);
        Printf("?\n");
#endif

        if (!intIsMember(vn,vs))
            defaulted |= resolveVar(vn);
#ifdef DEBUG_DEFAULTS
        else
            Printf("Yes, so no ambiguity!\n");
#endif
    }

    return defaulted;
}

static Bool local resolveVar(vn)        /* Determine whether an ambig.  */
Int  vn; {                              /* variable vn can be resolved  */
    List ps        = preds;             /* by default in the context of */
    List cs        = NIL;               /* the predicates in ps         */
    Bool aNumClass = FALSE;

    if (tyvar(vn)->bound == SKOLEM)
        return FALSE;

    /* According to the Haskell definition, we can only default an ambiguous
     * variable if the set of classes that constrain it:
     *   (a) includes at least one numeric class.
     *   (b) includes only numeric or standard classes.
     * In addition, we will not allow a variable to be defaulted unless it
     * appears only in predicates of the form (Class var).
     */

#ifdef DEBUG_DEFAULTS
    Printf("Trying to default variable %d\n",vn);
#endif

    for (; nonNull(ps); ps=tl(ps)) {
        Cell  pi = hd(ps);
        Class c  = classConstraining(vn,fst3(pi),intOf(snd3(pi)));
        if (nonNull(c)) {
            if (c==classRealFrac   || c==classRealFloat ||
                c==classFractional || c==classFloating  ||
                c==classReal       || c==classIntegral  || c==classNum)
                aNumClass = TRUE;
            else if (c!=classEq    && c!=classOrd  && c!=classShow &&
                     c!=classRead  && c!=classIx   && c!=classEnum &&
                     c!=classBounded)
                return FALSE;

            {   Type  t = arg(fst3(pi));/* Check for single var as arg     */
                Int   o = intOf(snd3(pi));
                Tyvar *tyv;
                deRef(tyv,t,o);
                if (!tyv || tyvNum(tyv)!=vn)
                    return FALSE;
            }
            if (!cellIsMember(c,cs))
                cs = cons(c,cs);
        }
    }

    /* Now find the first class (if any) in the list of defaults that
     * is an instance of all of the required classes.
     *
     * If we get this far, then cs only mentions classes from the list
     * above, all of which have only a single parameter of kind *.
     */

    if (aNumClass) {
        List ds = defaultDefns;         /* N.B. guaranteed to be monotypes */
#ifdef DEBUG_DEFAULTS
        Printf("Default conditions met, looking for type\n");
#endif
        for (; nonNull(ds); ds=tl(ds)) {
            List cs1 = cs;
            while (nonNull(cs1) && nonNull(entail(NIL,ap(hd(cs1),hd(ds)),0,0)))
                cs1 = tl(cs1);
            if (isNull(cs1)) {
                bindTv(vn,hd(ds),0);
#ifdef DEBUG_DEFAULTS
                Printf("Default type for variable %d is ",vn);
                printType(stdout,hd(ds));
                Printf("\n");
#endif
                return TRUE;
            }
        }
    }

#ifdef DEBUG_DEFAULTS
    Printf("No default permitted/found\n");
#endif
    return FALSE;
}

static Class local classConstraining(vn,pi,o)
Int  vn;                                /* Return class constraining var*/
Cell pi;                                /* vn in predicate pi, or NIL if*/
Int  o; {                               /* vn is not involved           */
    for (; isAp(pi); pi=fun(pi))
        if (!doesntOccurIn(tyvar(vn),arg(pi),o))
            return getHead(pi);
    return NIL;
}

/*-------------------------------------------------------------------------*/
