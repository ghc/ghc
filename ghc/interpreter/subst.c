
/* --------------------------------------------------------------------------
 * Provides an implementation for the `current substitution' used during
 * type and kind inference in both static analysis and type checking.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: subst.c,v $
 * $Revision: 1.9 $
 * $Date: 1999/11/23 15:12:07 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "link.h"
#include "subst.h"

/*#define DEBUG_TYPES*/

static Int numTyvars;                   /* no. type vars currently in use  */
static Int maxTyvars = 0;
static Int nextGeneric;                 /* number of generics found so far */

#if    FIXED_SUBST
Tyvar  tyvars[NUM_TYVARS];              /* storage for type variables      */
#else
Tyvar  *tyvars = 0;                     /* storage for type variables      */
#endif
Int    typeOff;                         /* offset of result type           */
Type   typeIs;                          /* skeleton of result type         */
Int    typeFree;                        /* freedom in instantiated type    */
List   predsAre;                        /* list of predicates in type      */
List   genericVars;                     /* list of generic vars            */
List   btyvars = NIL;                   /* explicitly scoped type vars     */

/* --------------------------------------------------------------------------
 * local function prototypes:
 * ------------------------------------------------------------------------*/

static Void local expandSubst           Args((Int));
static Int  local findBtyvsInt          Args((Text));
static Type local makeTupleType         Args((Int));
static Kind local makeSimpleKind        Args((Int));
static Kind local makeVarKind           Args((Int));
static Void local expandSyn1            Args((Tycon, Type *, Int *));
static List local listTyvar		Args((Int,List));
static List local listTyvars		Args((Type,Int,List));
static Cell local dupTyvar		Args((Int,List));
static Cell local dupTyvars		Args((Cell,Int,List));
static Pair local copyNoMark		Args((Cell,Int));
static Type local dropRank1Body         Args((Type,Int,Int));
static Type local liftRank1Body         Args((Type,Int));
static Bool local matchTypeAbove	Args((Type,Int,Type,Int,Int));

static Bool local varToVarBind          Args((Tyvar *,Tyvar *));
static Bool local varToTypeBind         Args((Tyvar *,Type,Int));
#if TREX
static Bool local inserter              Args((Type,Int,Type,Int));
static Int  local remover               Args((Text,Type,Int));
static Int  local tailVar               Args((Type,Int));
#endif

static Bool local improveAgainst	Args((Int,List,Cell,Int));
static Bool local instImprove		Args((Int,Class,Cell,Int));
static Bool local pairImprove		Args((Int,Class,Cell,Int,Cell,Int,Int));
#if IPARAM
static Bool local ipImprove		Args((Int,Cell,Int,Cell,Int));
#endif

static Bool local kvarToVarBind         Args((Tyvar *,Tyvar *));
static Bool local kvarToTypeBind        Args((Tyvar *,Type,Int));

/* --------------------------------------------------------------------------
 * The substitution, types, and kinds:
 *
 * In early versions of Gofer, the `substitution' data structure was only
 * used by the type checker, so it made sense to include support for it in
 * type.c.  This changed when kinds and kind inference where introduced,
 * which required access to the substitution during static analysis.  The
 * links between type.c and static.c that were intially used to accomplish
 * this have now been avoided by making the substitution visible as an
 * independent data structure in storage.c.
 *
 * In the same way that values have types, type constructors (and more
 * generally, expressions built from such constructors) have kinds.
 * The syntax of kinds in the current implementation is very simple:
 *
 *        kind ::= STAR         -- the kind of types
 *              |  kind => kind -- constructors
 *              |  variables    -- either INTCELL or OFFSET
 *
 * For various reasons, this implementation uses structure sharing, instead
 * of a copying approach.  In principal, this is fast and avoids the need to
 * build new type expressions.  Unfortunately, this implementation will not
 * be able to handle *very* large expressions.
 *
 * The substitution is represented by an array of type variables each of
 * which is a triple:
 *      bound   a (skeletal) type expression, or NIL if the variable
 *              is not bound, or SKOLEM for a Skolem constant (i.e., an
 *              uninstantiable variable).
 *      offs    offset of skeleton in bound.  If isNull(bound), then offs is
 *              used to indicate whether that variable is generic (i.e. free
 *              in the current assumption set) or fixed (i.e. bound in the
 *              current assumption set).  Generic variables are assigned
 *              offset numbers whilst copying type expressions (t,o) to
 *              obtain their most general form.
 *      kind    kind of value bound to type variable (`type variable' is
 *              rather inaccurate -- `constructor variable' would be better).
 * ------------------------------------------------------------------------*/

Void emptySubstitution() {              /* clear current substitution      */
    numTyvars   = 0;
#if !FIXED_SUBST
    if (maxTyvars!=NUM_TYVARS) {
        maxTyvars = 0;
        if (tyvars) {
            free(tyvars);
            tyvars = 0;
        }
    }
#endif
    nextGeneric = 0;
    genericVars = NIL;
    typeIs      = NIL;
    predsAre    = NIL;
    btyvars     = NIL;
}

static Void local expandSubst(n)        /* add further n type variables to */
Int n; {                                /* current substituion             */
#if FIXED_SUBST
    if (numTyvars+n>NUM_TYVARS) {
        ERRMSG(0) "Too many type variables in type checker"
        EEND;
    }
#else
    if (numTyvars+n>maxTyvars) {        /* need to expand substitution     */
        Int   newMax = maxTyvars+NUM_TYVARS;
        Tyvar *newTvs;
        Int   i;

        if (numTyvars+n>newMax) {       /* safety precaution               */
            ERRMSG(0) "Substitution expanding too quickly"
            EEND;
        }

        /* It would be better to realloc() here, but that isn't portable
         * enough for calloc()ed arrays.  The following code could cause
         * a space leak if an interrupt occurs while we're copying the
         * array ... we won't worry about this for the time being because
         * we don't expect to have to go through this process much (if at
         * all) in normal use of the type checker.
         */

        newTvs = (Tyvar *)calloc(newMax,sizeof(Tyvar));
        if (!newTvs) {
            ERRMSG(0) "Too many variables (%d) in type checker", newMax
            EEND;
        }
        for (i=0; i<numTyvars;++i) {            /* copy substitution       */
            newTvs[i].offs  = tyvars[i].offs;
            newTvs[i].bound = tyvars[i].bound;
            newTvs[i].kind  = tyvars[i].kind;
        }
        maxTyvars = 0;                          /* protection from SIGINT? */
        if (tyvars) free(tyvars);
        tyvars    = newTvs;
        maxTyvars = newMax;
    }
#endif
}

Int newTyvars(n)                        /* allocate new type variables     */
Int n; {                                /* all of kind STAR                */
    Int beta = numTyvars;

    expandSubst(n);
    for (numTyvars+=n; n>0; n--) {
        tyvars[numTyvars-n].offs  = UNUSED_GENERIC;
        tyvars[numTyvars-n].bound = NIL;
        tyvars[numTyvars-n].kind  = STAR;
#ifdef DEBUG_TYPES
        Printf("new type variable: _%d ::: ",numTyvars-n);
        printKind(stdout,tyvars[numTyvars-n].kind);
        Putchar('\n');
#endif
    }
    return beta;
}

Int newKindedVars(k)                    /* allocate new variables with     */
Kind k; {                               /* specified kinds                 */
    Int beta = numTyvars;               /* if k = k0 -> k1 -> ... -> kn    */
    for (; isPair(k); k=snd(k)) {       /* then allocate n vars with kinds */
        expandSubst(1);                 /* k0, k1, ..., k(n-1)             */
        tyvars[numTyvars].offs  = UNUSED_GENERIC;
        tyvars[numTyvars].bound = NIL;
        tyvars[numTyvars].kind  = fst(k);
#ifdef DEBUG_TYPES
        Printf("new type variable: _%d ::: ",numTyvars);
        printKind(stdout,tyvars[numTyvars].kind);
        Putchar('\n');
#endif
        numTyvars++;
    }
    return beta;
}

Void instantiate(type)                  /* instantiate type, if nonNull    */
Type type; {
    predsAre = NIL;
    typeIs   = type;
    typeFree = 0;

    if (nonNull(typeIs)) {             /* instantiate type expression ?    */

        if (isPolyType(typeIs)) {      /* Polymorphic type scheme ?        */
            Kinds ks = polySigOf(typeIs);
            typeOff  = newKindedVars(ks);
            typeIs   = monotypeOf(typeIs);
            for (; isAp(ks); ks=arg(ks))
                typeFree++;
        }

	if (isQualType(typeIs)) {    /* Qualified type?			   */
            predsAre = fst(snd(typeIs));
            typeIs   = snd(snd(typeIs));
        }
    }
}

/* --------------------------------------------------------------------------
 * Bound type variables:
 * ------------------------------------------------------------------------*/

Pair findBtyvs(t)                       /* Look for bound tyvar            */
Text t; {
    List bts = btyvars;
    for (; nonNull(bts); bts=tl(bts)) {
        List bts1 = hd(bts);
        for (; nonNull(bts1); bts1=tl(bts1))
            if (t==textOf(fst(hd(bts1))))
                return hd(bts1);
    }
    return NIL;
}

static Int local findBtyvsInt(t)        /* Look for bound type variable    */
Text t; {                               /* expecting to find an integer    */
    Pair p = findBtyvs(t);
    if (isNull(p))
        internal("findBtyvsInt");
    return intOf(snd(p));
}

Void markBtyvs() {                      /* Mark explicitly scoped vars     */
    List bts = btyvars;
    for (; nonNull(bts); bts=tl(bts)) {
        List bts1 = hd(bts);
        for (; nonNull(bts1); bts1=tl(bts1))
            markTyvar(intOf(snd(hd(bts1))));
    }
}

Type localizeBtyvs(t)                   /* Localize type to eliminate refs */
Type t; {                               /* to explicitly scoped vars       */
    switch (whatIs(t)) {
        case RANK2    :
        case POLYTYPE : snd(snd(t)) = localizeBtyvs(snd(snd(t)));
                        break;

        case QUAL     : fst(snd(t)) = localizeBtyvs(fst(snd(t)));
                        snd(snd(t)) = localizeBtyvs(snd(snd(t)));
                        break;

        case AP       : fst(t) = localizeBtyvs(fst(t));
                        snd(t) = localizeBtyvs(snd(t));
                        break;

        case VARIDCELL:
        case VAROPCELL: return mkInt(findBtyvsInt(textOf(t)));
    }
    return t;
}

/* --------------------------------------------------------------------------
 * Dereference or bind types in subsitution:
 * ------------------------------------------------------------------------*/

Tyvar *getTypeVar(t,o)                  /* get number of type variable     */
Type t;                                 /* represented by (t,o) [if any].  */
Int  o; {
    switch (whatIs(t)) {
        case INTCELL   : return tyvar(intOf(t));
        case OFFSET    : return tyvar(o+offsetOf(t));
        case VARIDCELL :
        case VAROPCELL : return tyvar(findBtyvsInt(textOf(t)));
    }
    return ((Tyvar *)0);
}

Void tyvarType(vn)                      /* load type held in type variable */
Int vn; {                               /* vn into (typeIs,typeOff)        */
    Tyvar *tyv;

    while ((tyv=tyvar(vn)), isBound(tyv))
        switch(whatIs(tyv->bound)) {
            case INTCELL   : vn = intOf(tyv->bound);
                             break;

            case OFFSET    : vn = offsetOf(tyv->bound)+(tyv->offs);
                             break;

            case VARIDCELL :
            case VAROPCELL : vn = findBtyvsInt(textOf(tyv->bound));
                             break;

            default        : typeIs  = tyv->bound;
                             typeOff = tyv->offs;
                             return;
        }
    typeIs  = aVar;
    typeOff = vn;
}

Void bindTv(vn,t,o)                     /* set type variable vn to (t,o)   */
Int  vn;
Type t;
Int  o; {
    Tyvar *tyv = tyvar(vn);
    tyv->bound = t;
    tyv->offs  = o;
#ifdef DEBUG_TYPES
    Printf("binding type variable: _%d to ",vn);
    printType(stdout,debugType(t,o));
    Putchar('\n');
#endif
}

Cell getDerefHead(t,o)                  /* get value at head of type exp.  */
Type t;
Int  o; {
    Tyvar *tyv;
    argCount = 0;
    for (;;) {
        while (isAp(t)) {
            argCount++;
            t = fun(t);
        }
        if ((tyv=getTypeVar(t,o)) && isBound(tyv)) {
            t = tyv->bound;
            o = tyv->offs;
        }
        else
            break;
    }
    return t;
}

/* --------------------------------------------------------------------------
 * Expand type synonyms:
 * ------------------------------------------------------------------------*/

Void expandSyn(h,ar,at,ao)              /* Expand type synonym with:       */
Tycon h;                                /* head h                          */
Int   ar;                               /* ar args (NB. ar>=tycon(h).arity)*/
Type  *at;                              /* original expression (*at,*ao)   */
Int   *ao; {                            /* expansion returned in (*at,*ao) */
    ar -= tycon(h).arity;               /* calculate surplus arguments     */
    if (ar==0)
        expandSyn1(h,at,ao);
    else {                              /* if there are more args than the */
        Type t    = *at;                /* arity, we have to do a little   */
        Int  o    = *ao;                /* bit of work to isolate args that*/
        Type args = NIL;                /* will not be changed by expansion*/
        Int  i;
        while (ar-- > 0) {              /* find part to expand, and the    */
            Tyvar *tyv;                 /* unused arguments                */
            args = cons(arg(t),args);
            t    = fun(t);
            deRef(tyv,t,o);
        }
        expandSyn1(h,&t,&o);            /* do the expansion                */
        bindTv((i=newTyvars(1)),t,o);   /* and embed the results back in   */
        tyvar(i)->kind = getKind(t,o);  /* (*at, *ao) as required          */
        *at = applyToArgs(mkInt(i),args);
    }
}

static Void local expandSyn1(h,at,ao)   /* Expand type synonym with:       */
Tycon h;                                /* head h, tycon(h).arity args,    */
Type  *at;                              /* original expression (*at,*ao)   */
Int   *ao; {                            /* expansion returned in (*at,*ao) */
    Int   n = tycon(h).arity;
    Type  t = *at;
    Int   o = *ao;
    Tyvar *tyv;

    *at = tycon(h).defn;
    *ao = newKindedVars(tycon(h).kind);
    for (; 0<n--; t=fun(t)) {
        deRef(tyv,t,o);
        if (tyv || !isAp(t))
            internal("expandSyn1");
        bindTv(*ao+n,arg(t),o);
    }
}

/* --------------------------------------------------------------------------
 * Marking fixed variables in type expressions:
 * ------------------------------------------------------------------------*/

Void clearMarks() {                     /* Set all unbound type vars to    */
    Int i;                              /* unused generic variables        */
    for (i=0; i<numTyvars; ++i)
        if (!isBound(tyvar(i)))
            tyvar(i)->offs = UNUSED_GENERIC;
    genericVars = NIL;
    nextGeneric = 0;
}

Void markAllVars() {                    /* Set all unbound type vars to    */
    Int i;                              /* be fixed vars                   */
    for (i=0; i<numTyvars; ++i)
        if (!isBound(tyvar(i)))
            tyvar(i)->offs = FIXED_TYVAR;
    genericVars = NIL;
    nextGeneric = 0;
}

Void resetGenerics() {                  /* Reset all generic vars to unused*/
    Int i;
    for (i=0; i<numTyvars; ++i)
        if (!isBound(tyvar(i)) && tyvar(i)->offs>=GENERIC)
            tyvar(i)->offs = UNUSED_GENERIC;
    genericVars = NIL;
    nextGeneric = 0;
}

Void markTyvar(vn)                      /* mark fixed vars in type bound to*/
Int vn; {                               /* given type variable             */
    Tyvar *tyv = tyvar(vn);

    if (isBound(tyv))
        markType(tyv->bound, tyv->offs);
    else
        (tyv->offs) = FIXED_TYVAR;
}

Void markType(t,o)                      /* mark fixed vars in type (t,o)   */
Type t;
Int  o; {
    STACK_CHECK
    switch (whatIs(t)) {
        case POLYTYPE  :
        case QUAL      :
#if TREX
        case EXT       :
#endif
        case TYCON     :
        case TUPLE     : return;

        case AP        : markType(fst(t),o);
                         markType(snd(t),o);
                         return;

        case OFFSET    : markTyvar(o+offsetOf(t));
                         return;

        case INTCELL   : markTyvar(intOf(t));
                         return;

        case VARIDCELL :
        case VAROPCELL : markTyvar(findBtyvsInt(textOf(t)));
                         return;

        case RANK2     : markType(snd(snd(t)),o);
                         return;

        default        : internal("markType");
    }
}

Void markPred(pi)                       /* Marked fixed type vars in pi    */
Cell pi; {
    Cell cl = fst3(pi);
    Int  o  = intOf(snd3(pi));

    for (; isAp(cl); cl=fun(cl))
        markType(arg(cl),o);
}

/* --------------------------------------------------------------------------
 * Copy type expression from substitution to make a single type expression:
 * ------------------------------------------------------------------------*/

Type copyTyvar(vn)                      /* calculate most general form of  */
Int vn; {                               /* type bound to given type var    */
    Tyvar *tyv = tyvar(vn);

    if ((tyv->bound)==SKOLEM) {
        return mkInt(vn);
    } else if (tyv->bound) {
        return copyType(tyv->bound,tyv->offs);
    }

    switch (tyv->offs) {
        case FIXED_TYVAR    : return mkInt(vn);

        case UNUSED_GENERIC : (tyv->offs) = GENERIC + nextGeneric++;
                              if (nextGeneric>=NUM_OFFSETS) {
                                  ERRMSG(0)
                                      "Too many quantified type variables"
                                  EEND;
                              }
                              genericVars = cons(mkInt(vn),genericVars);

        default             : return mkOffset(tyv->offs - GENERIC);
    }
}

Type copyType(t,o)                      /* calculate most general form of  */
Type t;                                 /* type expression (t,o)           */
Int  o; {
    STACK_CHECK
    switch (whatIs(t)) {
        case AP        : {   Type l = copyType(fst(t),o);/* ensure correct */
                             Type r = copyType(snd(t),o);/* eval. order    */
                             return ap(l,r);
                         }
        case OFFSET    : return copyTyvar(o+offsetOf(t));
        case INTCELL   : return copyTyvar(intOf(t));
        case VARIDCELL :
        case VAROPCELL : return copyTyvar(findBtyvsInt(textOf(t)));
    }

    return t;
}

Cell copyPred(pi,o)                     /* Copy single predicate (or part  */
Cell pi;                                /* thereof) ...                    */
Int  o; {
    if (isAp(pi)) {
        Cell temp = copyPred(fun(pi),o);/* to ensure correct order of eval.*/
        return ap(temp,copyType(arg(pi),o));
    }
    else
        return pi;
}

Type zonkTyvar(vn)	/* flatten type by chasing all references	   */
Int vn; {		/* and collapsing OFFSETS to absolute indexes	   */
    Tyvar *tyv = tyvar(vn);

    if (tyv->bound)
	return zonkType(tyv->bound,tyv->offs);
    else
	return mkInt(vn);
}

Type zonkType(t,o)	/* flatten type by chasing all references	   */
Type t;			/* and collapsing OFFSETS to absolute indexes	   */
Int  o; {
    STACK_CHECK
    switch (whatIs(t)) {
	case AP        : {   Type l = zonkType(fst(t),o);/* ensure correct */
			     Type r = zonkType(snd(t),o);/* eval. order    */
			     return ap(l,r);
			 }
	case OFFSET    : return zonkTyvar(o+offsetOf(t));
	case INTCELL   : return zonkTyvar(intOf(t));
    }

    return t;
}

#ifdef DEBUG_TYPES
Type debugTyvar(vn)                     /* expand type structure in full   */
Int vn; {                               /* detail                          */
    Tyvar *tyv = tyvar(vn);

    if (isBound(tyv))
        return debugType(tyv->bound,tyv->offs);
    return mkInt(vn);
}

Type debugType(t,o)
Type t;
Int  o; {
    STACK_CHECK
    switch (whatIs(t)) {
        case AP        : {   Type l = debugType(fst(t),o);
                             Type r = debugType(snd(t),o);
                             return ap(l,r);
                         }
        case OFFSET    : return debugTyvar(o+offsetOf(t));
        case INTCELL   : return debugTyvar(intOf(t));
        case VARIDCELL :
        case VAROPCELL : return debugTyvar(findBtyvsInt(textOf(t)));
    }

    return t;
}
List debugContext(ps)
List ps; {
    Cell p;
    List qs = NIL;
    for (; nonNull(ps); ps=tl(ps)) {
        p = debugPred(fst3(hd(ps)),intOf(snd3(hd(ps))));
	qs = cons(p,qs);
    }
    return rev(qs);
}

Cell debugPred(pi,o)
Cell pi;
Int  o; {
    if (isAp(pi)) {
	return pair(debugPred(fun(pi),o),debugType(arg(pi),o));
    }
    return pi;
}
#endif /*DEBUG_TYPES*/

Kind copyKindvar(vn)                    /* build kind attatched to variable*/
Int vn; {
    Tyvar *tyv = tyvar(vn);
    if (tyv->bound)
        return copyKind(tyv->bound,tyv->offs);
    return STAR;                        /* any unbound variable defaults to*/
}                                       /* the kind of all types           */

Kind copyKind(k,o)                      /* build kind expression from      */
Kind k;                                 /* given skeleton                  */
Int  o; {
    switch (whatIs(k)) {
        case AP      : {   Kind l = copyKind(fst(k),o);  /* ensure correct */
                           Kind r = copyKind(snd(k),o);  /* eval. order    */
                           return ap(l,r);
                       }
        case OFFSET  : return copyKindvar(o+offsetOf(k));
        case INTCELL : return copyKindvar(intOf(k));
    }
    return k;
}

/* --------------------------------------------------------------------------
 * Copy type expression from substitution without marking:
 * ------------------------------------------------------------------------*/

static List local listTyvar(vn,ns)
Int  vn;
List ns; {
    Tyvar *tyv = tyvar(vn);

    if (isBound(tyv)) {
	return listTyvars(tyv->bound,tyv->offs,ns);
    } else if (!intIsMember(vn,ns)) {
	ns = cons(mkInt(vn),ns);
    }
    return ns;
}

static List local listTyvars(t,o,ns)
Cell t;
Int  o;
List ns; {
    switch (whatIs(t)) {
	case AP        : return listTyvars(fst(t),o,
				 listTyvars(snd(t),o,
				  ns));
	case OFFSET    : return listTyvar(o+offsetOf(t),ns);
	case INTCELL   : return listTyvar(intOf(t),ns);
	default        : break;
    }
    return ns;
}

static Cell local dupTyvar(vn,ns)
Int  vn;
List ns; {
    Tyvar *tyv = tyvar(vn);

    if (isBound(tyv)) {
	return dupTyvars(tyv->bound,tyv->offs,ns);
    } else {
	Int i = 0;
	for (; nonNull(ns) && vn!=intOf(hd(ns)); ns=tl(ns)) {
	    i++;
	}
	return mkOffset(i);
    }
}

static Cell local dupTyvars(t,o,ns)
Cell t;
Int  o;
List ns; {
    switch (whatIs(t)) {
	case AP        : {   Type l = dupTyvars(fst(t),o,ns);
			     Type r = dupTyvars(snd(t),o,ns);
			     return ap(l,r);
			 }
	case OFFSET    : return dupTyvar(o+offsetOf(t),ns);
	case INTCELL   : return dupTyvar(intOf(t),ns);
    }
    return t;
}

static Cell local copyNoMark(t,o)	/* Copy a type or predicate without*/
Cell t;					/* changing marks		   */
Int  o; {
    List ns     = listTyvars(t,o,NIL);
    Cell result = pair(ns,dupTyvars(t,o,ns));
    for (; nonNull(ns); ns=tl(ns)) {
	hd(ns) = tyvar(intOf(hd(ns)))->kind;
    }
    return result;
}

/* --------------------------------------------------------------------------
 * Droping and lifting of type schemes that appear in rank 2 position:
 * ------------------------------------------------------------------------*/

Type dropRank2(t,alpha,n)               /* Drop a (potentially) rank2 type */
Type t;
Int  alpha;
Int  n; {
    if (whatIs(t)==RANK2) {
        Cell r  = fst(snd(t));
        Int  i  = intOf(r);
        Type as = NIL;
        for (t=snd(snd(t)); i>0; i--) {
            Type a = arg(fun(t));
            if (isPolyType(a))
                a = dropRank1(a,alpha,n);
            as = fn(a,as);
            t  = arg(t);
        }
        t = ap(RANK2,pair(r,revOnto(as,t)));
    }
    return t;
}

Type dropRank1(t,alpha,n)               /* Copy rank1 argument type t to   */
Type t;                                 /* make a rank1 type scheme        */
Int  alpha;
Int  n; {
    if (n>0 && isPolyType(t))
        t = mkPolyType(polySigOf(t),dropRank1Body(monotypeOf(t),alpha,n));
    return t;
}

static Type local dropRank1Body(t,alpha,n)
Type t;
Int  alpha;
Int  n; {
    switch (whatIs(t)) {
        case OFFSET   : {   Int m = offsetOf(t);
                            return (m>=n) ? mkOffset(m-n) : mkInt(alpha+m);
                        }

        case POLYTYPE : return mkPolyType(polySigOf(t),
                                          dropRank1Body(monotypeOf(t),alpha,n));

        case QUAL     : return ap(QUAL,dropRank1Body(snd(t),alpha,n));

        case RANK2    : return ap(RANK2,pair(fst(snd(t)),
                                             dropRank1Body(snd(snd(t)),
                                                           alpha,
                                                           n)));

        case AP       : return ap(dropRank1Body(fun(t),alpha,n),
                                  dropRank1Body(arg(t),alpha,n));

        default       : return t;
    }
}

Void liftRank2Args(as,alpha,m)
List as;
Int  alpha;
Int  m; {
    Int i = 0;
    for (; i<m; i++)
        copyTyvar(alpha+i);
    for (m=nextGeneric; nonNull(as); as=tl(as)) {
        Type ta = arg(fun(as));
        ta      = isPolyType(ta) ? liftRank1Body(ta,m) : copyType(ta,alpha);
        arg(fun(as))
                = ta;
    }
}

Type liftRank2(t,alpha,m)
Type t;
Int  alpha;
Int  m; {
    if (whatIs(t)==RANK2) {
        Cell r  = fst(snd(t));
        Int  i  = 0;
        Type as = NIL;
        for (; i<m; i++)
            copyTyvar(alpha+i);
        m = nextGeneric;
        t = snd(snd(t));
        for (i=intOf(r); i>0; i--) {
            Type a = arg(fun(t));
            a      = isPolyType(a) ? liftRank1Body(a,m) : copyType(a,alpha);
            as     = fn(a,as);
            t      = arg(t);
        }
        t = ap(RANK2,pair(r,revOnto(as,copyType(t,alpha))));
    }
    else
        t = copyType(t,alpha);
    return t;
}

Type liftRank1(t,alpha,m)
Type t;
Int  alpha;
Int  m; {
    if (m>0 && isPolyType(t)) {
        Int i = 0;
        resetGenerics();
        for (; i<m; i++)
            copyTyvar(alpha+i);
        t = liftRank1Body(t,nextGeneric);
    }
    return t;
}

static Type local liftRank1Body(t,n)
Type t;
Int  n; {
    switch (whatIs(t)) {
        case OFFSET    : return mkOffset(n+offsetOf(t));

        case INTCELL   : return copyTyvar(intOf(t));

        case VARIDCELL :
        case VAROPCELL : return copyTyvar(findBtyvsInt(textOf(t)));

        case POLYTYPE  : return mkPolyType(polySigOf(t),
                                           liftRank1Body(monotypeOf(t),n));

        case QUAL      : return ap(QUAL,liftRank1Body(snd(t),n));

        case RANK2     : return ap(RANK2,pair(fst(snd(t)),
                                              liftRank1Body(snd(snd(t)),n)));

        case AP        : return ap(liftRank1Body(fun(t),n),
                                   liftRank1Body(arg(t),n));

        default        : return t;
    }
}

/* --------------------------------------------------------------------------
 * Support for `kind preserving substitutions' from unification:
 * ------------------------------------------------------------------------*/

Bool eqKind(k1,k2)                      /* check that two (mono)kinds are  */
Kind k1, k2; {                          /* equal                           */
    return k1==k2
           || (isPair(k1) && isPair(k2)
              && eqKind(fst(k1),fst(k2))
              && eqKind(snd(k1),snd(k2)));
}

Kind getKind(c,o)                       /* Find kind of constr during type */
Cell c;                                 /* checking process                */
Int  o; {
    if (isAp(c))                                        /* application     */
        return snd(getKind(fst(c),o));
    switch (whatIs(c)) {
        case TUPLE     : return simpleKind(tupleOf(c)); /*(,)::* -> * -> * */
        case OFFSET    : return tyvar(o+offsetOf(c))->kind;
        case INTCELL   : return tyvar(intOf(c))->kind;
        case VARIDCELL :
        case VAROPCELL : return tyvar(findBtyvsInt(textOf(c)))->kind;
        case TYCON     : return tycon(c).kind;
#if TREX
        case EXT    : return extKind;
#endif
    }
#ifdef DEBUG_KINDS
    Printf("getKind c = %d, whatIs=%d\n",c,whatIs(c));
#endif
    internal("getKind");
    return STAR;/* not reached */
}

/* --------------------------------------------------------------------------
 * Find generic variables in a type:
 * ------------------------------------------------------------------------*/

Type genvarTyvar(vn,vs)                 /* calculate list of generic vars  */
Int  vn;                                /* thru variable vn, prepended to  */
List vs; {                              /* list vs                         */
    Tyvar *tyv = tyvar(vn);

    if (isBound(tyv))
        return genvarType(tyv->bound,tyv->offs,vs);
    else if (tyv->offs == UNUSED_GENERIC) {
        tyv->offs += GENERIC + nextGeneric++;
        return cons(mkInt(vn),vs);
    }
    else if (tyv->offs>=GENERIC && !intIsMember(vn,vs))
        return cons(mkInt(vn),vs);
    else
        return vs;
}

List genvarType(t,o,vs)                 /* calculate list of generic vars  */
Type t;                                 /* in type expression (t,o)        */
Int  o;                                 /* results are prepended to vs     */
List vs; {
    switch (whatIs(t)) {
        case AP        : return genvarType(snd(t),o,genvarType(fst(t),o,vs));
        case OFFSET    : return genvarTyvar(o+offsetOf(t),vs);
        case INTCELL   : return genvarTyvar(intOf(t),vs);
        case VARIDCELL :
        case VAROPCELL : return genvarTyvar(findBtyvsInt(textOf(t)),vs);
    }
    return vs;
}

/* --------------------------------------------------------------------------
 * Occurs check:
 * ------------------------------------------------------------------------*/

Bool doesntOccurIn(lookFor,t,o)         /* Return TRUE if var lookFor      */
Tyvar *lookFor;                         /* isn't referenced in (t,o)       */
Type  t;
Int   o; {
    Tyvar *tyv;

    STACK_CHECK
    for (;;) {
        deRef(tyv,t,o);
        if (tyv)                        /* type variable                   */
            return tyv!=lookFor;
        else if (isAp(t)) {             /* application                     */
            if (doesntOccurIn(lookFor,snd(t),o))
                t = fst(t);
            else
                return FALSE;
        }
        else                            /* no variable found               */
            break;
    }
    return TRUE;
}

/* --------------------------------------------------------------------------
 * Unification algorithm:
 * ------------------------------------------------------------------------*/

char   *unifyFails   = 0;               /* Unification error message       */
static Int bindAbove = 0;               /* Used to restrict var binding    */

#define bindOnlyAbove(beta)     bindAbove=beta
#define noBind()                bindAbove=MAXPOSINT
#define unrestrictBind()        bindAbove=0

static Bool local varToVarBind(tyv1,tyv2)/* Make binding tyv1 := tyv2      */
Tyvar *tyv1, *tyv2; {
    if (tyv1!=tyv2) {                   /* If vars are same, nothing to do!*/

        /* Check that either tyv1 or tyv2 is in allowed range for binding  */
        /* and is not a Skolem constant, and swap vars if nec. so we can   */
        /* bind to tyv1.                                                   */

        if (tyvNum(tyv1)<bindAbove || tyv1->bound==SKOLEM) {
            if (tyvNum(tyv2)<bindAbove || tyv2->bound==SKOLEM) {
                unifyFails = "types do not match";
                return FALSE;
            }
            else {
                Tyvar *tyv = tyv1;
                tyv1       = tyv2;
                tyv2       = tyv;
            }
        }
        if (!eqKind(tyv1->kind,tyv2->kind)) {
            unifyFails = "constructor variable kinds do not match";
            return FALSE;
        }
        tyv1->bound = aVar;
        tyv1->offs  = tyvNum(tyv2);
#ifdef DEBUG_TYPES
        Printf("vv binding tyvar: _%d to _%d\n",tyvNum(tyv1),tyvNum(tyv2));
#endif
    }
    return TRUE;
}

static Bool local varToTypeBind(tyv,t,o)/* Make binding tyv := (t,o)       */
Tyvar *tyv;
Type  t;                                /* guaranteed not to be a v'ble or */
Int   o; {                              /* have synonym as outermost constr*/
    if (tyvNum(tyv)<bindAbove) {        /* Check that tyv is in range      */
        unifyFails = "types do not match";
        return FALSE;
    }
    else if (tyv->bound == SKOLEM) {    /* Check that it is not Skolemized */
        unifyFails = "cannot instantiate Skolem constant";
        return FALSE;
    }
    else if (!doesntOccurIn(tyv,t,o))   /* Carry out occurs check          */
        unifyFails = "unification would give infinite type";
    else if (!eqKind(tyv->kind,getKind(t,o)))
        unifyFails = "kinds do not match";
    else {
        tyv->bound = t;
        tyv->offs  = o;
#ifdef DEBUG_TYPES
        Printf("vt binding type variable: _%d to ",tyvNum(tyv));
        printType(stdout,debugType(t,o));
        Putchar('\n');
#endif
        return TRUE;
    }
    return FALSE;
}

Bool unify(t1,o1,t2,o2)                 /* Main unification routine        */
Type t1,t2;                             /* unify (t1,o1) with (t2,o2)      */
Int  o1,o2; {
    Tyvar *tyv1, *tyv2;

    STACK_CHECK
    deRef(tyv1,t1,o1);
    deRef(tyv2,t2,o2);

un: if (tyv1) {
        if (tyv2)
            return varToVarBind(tyv1,tyv2);         /* t1, t2 variables    */
        else {
            Cell h2 = getDerefHead(t2,o2);          /* t1 variable, t2 not */
            if (isSynonym(h2) && argCount>=tycon(h2).arity) {
                expandSyn(h2,argCount,&t2,&o2);
                deRef(tyv2,t2,o2);
                goto un;
            }
            return varToTypeBind(tyv1,t2,o2);
        }
    }
    else
        if (tyv2) {
            Cell h1 = getDerefHead(t1,o1);          /* t2 variable, t1 not */
            if (isSynonym(h1) && argCount>=tycon(h1).arity) {
                expandSyn(h1,argCount,&t1,&o1);
                deRef(tyv1,t1,o1);
                goto un;
            }
            return varToTypeBind(tyv2,t1,o1);
        }
        else {                                      /* t1, t2 not vars     */
            Type h1 = getDerefHead(t1,o1);
            Int  a1 = argCount;
            Type h2 = getDerefHead(t2,o2);
            Int  a2 = argCount;

#ifdef DEBUG_TYPES
            Printf("tt unifying types: ");
            printType(stdout,debugType(t1,o1));
            Printf(" with ");
            printType(stdout,debugType(t2,o2));
            Putchar('\n');
#endif
            if (isOffset(h1) || isInt(h1)) h1=NIL;  /* represent var by NIL*/
            if (isOffset(h2) || isInt(h2)) h2=NIL;

#if TREX
            if (isExt(h1) || isExt(h2)) {
                if (a1==2 && isExt(h1) && a2==2 && isExt(h2)) {
                    if (extText(h1)==extText(h2)) {
                        return unify(arg(fun(t1)),o1,arg(fun(t2)),o2) &&
                                unify(arg(t1),o1,arg(t2),o2);
                    } else {
                        return inserter(t1,o1,t2,o2) &&
                                  unify(arg(t1),o1,aVar,
                                     remover(extText(h1),t2,o2));
                    }
                } else {
                    unifyFails = "rows are not compatible";
                    return FALSE;
                }
            }
#endif
            if (nonNull(h1) && h1==h2) {/* Assuming well-formed types, both*/
                if (a1!=a2) {           /* t1, t2 must have same no of args*/
                    unifyFails = "incompatible constructors";
                    return FALSE;
                }
                while (isAp(t1)) {
                    if (!unify(arg(t1),o1,arg(t2),o2))
                        return FALSE;
                    t1 = fun(t1);
                    deRef(tyv1,t1,o1);
                    t2 = fun(t2);
                    deRef(tyv2,t2,o2);
                }
                unifyFails = 0;
                return TRUE;
            }

            /* Types do not match -- look for type synonyms to expand */

            if (isSynonym(h1) && a1>=tycon(h1).arity) {
                expandSyn(h1,a1,&t1,&o1);
                deRef(tyv1,t1,o1);
                goto un;
            }
            if (isSynonym(h2) && a2>=tycon(h2).arity) {
                expandSyn(h2,a2,&t2,&o2);
                deRef(tyv2,t2,o2);
                goto un;
            }

            if ((isNull(h1) && a1<=a2) ||       /* last attempt -- maybe   */
                (isNull(h2) && a2<=a1)) {       /* one head is a variable? */
                for (;;) {
                    deRef(tyv1,t1,o1);
                    deRef(tyv2,t2,o2);

                    if (tyv1) {                         /* unify heads!    */
                        if (tyv2)
                            return varToVarBind(tyv1,tyv2);
                        else
                            return varToTypeBind(tyv1,t2,o2);
                    }
                    else if (tyv2)
                        return varToTypeBind(tyv2,t1,o1);

                    /* at this point, neither t1 nor t2 is a variable. In  */
                    /* addition, they must both be APs unless one of the   */
                    /* head variables has been bound during unification of */
                    /* the arguments.                                      */

                    if (!isAp(t1) || !isAp(t2)) {       /* might not be APs*/
                        unifyFails = 0;
                        return t1==t2;
                    }
                    if (!unify(arg(t1),o1,arg(t2),o2))  /* o/w must be APs */
                        return FALSE;
                    t1 = fun(t1);
                    t2 = fun(t2);
                }
            }
        }
    unifyFails = 0;
    return FALSE;
}

#if TREX
static Bool local inserter(r1,o1,r,o)   /* Insert first field in (r1,o1)   */
Type r1;                                /* into row (r,o), both of which   */
Int  o1;                                /* are known to begin with an EXT  */
Type r;
Int  o; {
    Text labt = extText(fun(fun(r1)));  /* Find the text of the label      */
#ifdef DEBUG_TYPES
    Printf("inserting ");
    printType(stdout,debugType(r1,o1));
    Printf(" into ");
    printType(stdout,debugType(r,o));
    Putchar('\n');
#endif
    for (;;) {
        Tyvar *tyv;
        deRef(tyv,r,o);
        if (tyv) {
            Int beta;                   /* Test for common tail            */
            if (tailVar(arg(r1),o1)==tyvNum(tyv)) {
                unifyFails = "distinct rows have common tail";
                return FALSE;
            }
            beta = newTyvars(1);        /* Extend row with new field       */
            tyvar(beta)->kind = ROW;
            return varToTypeBind(tyv,ap(fun(r1),mkInt(beta)),o1);
        }
        else if (isAp(r) && isAp(fun(r)) && isExt(fun(fun(r)))) {
            if (labt==extText(fun(fun(r))))/* Compare existing fields      */
                return unify(arg(fun(r1)),o1,extField(r),o);
            r = extRow(r);              /* Or skip to next field           */
        }
        else {                          /* Nothing else will match         */
            unifyFails = "field mismatch";
            return FALSE;
        }
    }
}

static Int local remover(l,r,o)         /* Make a new row by copying (r,o) */
Text l;                                 /* but removing the l field (which */
Type r;                                 /* MUST exist)                     */
Int  o; {
    Tyvar *tyv;
    Int    beta       = newTyvars(1);
    tyvar(beta)->kind = ROW;
#ifdef DEBUG_TYPES
    Printf("removing %s from",textToStr(l));
    printType(stdout,debugType(r,o));
    Putchar('\n');
#endif
    deRef(tyv,r,o);
    if (tyv || !isAp(r) || !isAp(fun(r)) || !isExt(fun(fun(r))))
        internal("remover");
    if (l==extText(fun(fun(r))))
        r = extRow(r);
    else
        r = ap(fun(r),mkInt(remover(l,extRow(r),o)));
    bindTv(beta,r,o);
    return beta;
}


static Int local tailVar(r,o)           /* Find var at tail end of a row   */
Type r;
Int  o; {
    for (;;) {
        Tyvar *tyv;
        deRef(tyv,r,o);
        if (tyv) {
            return tyvNum(tyv);
        }
        else if (isAp(r) && isAp(fun(r)) && isExt(fun(fun(r)))) {
            r = extRow(r);
        }
        else {
            return (-1);
        }
    }
}
#endif


Bool typeMatches(type,mt)               /* test if type matches monotype mt*/
    Type type, mt; {                    /* imported from STG Hugs          */
    Bool result;
     if (isPolyOrQualType(type))
        return FALSE;
    emptySubstitution();
    noBind();
    result = unify(mt,0,type,0);
    unrestrictBind();
    emptySubstitution();
    return result;
}

Bool isProgType(ks,type)		/* Test if type is of the form	   */
List ks;				/* IO t for some t.		   */
Type type; {
    Bool result;
    Int  alpha;
    Int  beta;
    emptySubstitution();
    alpha  = newKindedVars(ks);
    beta   = newTyvars(1);
    bindOnlyAbove(beta);
    result = unify(type,alpha,typeProgIO,beta);
    unrestrictBind();
    emptySubstitution();
    return result;
}

/* --------------------------------------------------------------------------
 * Matching predicates:
 *
 * There are (at least) four situations where we need to match up pairs
 * of predicates:
 *
 *   1) Testing to see if two predicates are the same (ignoring differences
 *      caused by the use of type synonyms, for example).
 *
 *   2) Matching a predicate with the head of its class so that we can
 *      find the corresponding superclass predicates.  If the predicates
 *      have already been kind-checked, and the classes are known to be
 *      the same, then this should never fail.
 *
 *   3) Matching a predicate against the head of an instance to see if
 *      that instance is applicable.
 *
 *   4) Matching two instance heads to see if there is an overlap.
 *
 * For (1), we need a matching process that does not bind any variables.
 * For (2) and (3), we need to use one-way matching, only allowing
 * variables in the class or instance head to be instantiated.  For
 * (4), we need two-way unification.
 *
 * Another situation in which both one-way and two-way unification might
 * be used is in an implementation of improvement.  Here, a one-way match
 * would be used to determine applicability of a rule for improvement
 * that would then be followed by unification with another predicate.
 * One possible syntax for this might be:
 *
 *     instance P => pi [improves pi'] where ...
 *
 * The intention here is that any predicate matching pi' can be unified
 * with pi to get more accurate types.  A simple example of this is:
 *
 *   instance Collection [a] a improves Collection [a] b where ...
 *
 * As soon as we know what the collection type is (in this case, a list),
 * we will also know what the element type is.  To ensure that the rule
 * for improvement is valid, the compilation system will also need to use
 * a one-way matching process to ensure that pi is a (substitution) instance
 * of pi'.  Another extension would be to allow more than one predicate pi'
 * in an improving rule.  Read the paper on simplification and improvement
 * for technical background.  Watch this space for implementation news!
 * ------------------------------------------------------------------------*/

Bool samePred(pi1,o1,pi,o)              /* Test to see if predicates are   */
Cell pi1;                               /* the same, with no binding of    */
Int  o1;                                /* the variables in either one.    */
Cell pi;                                /* Assumes preds are kind correct  */
Int  o; {                               /* with the same class.            */
    Bool result;
    noBind();
    result = unifyPred(pi1,o1,pi,o);
    unrestrictBind();
    return result;
}

Bool matchPred(pi1,o1,pi,o)             /* One way match predicate (pi1,o1)*/
Cell pi1;                               /* against (pi,o), allowing only   */
Int  o1;                                /* vars in 2nd pred to be bound.   */
Cell pi;                                /* Assumes preds are kind correct  */
Int  o; {                               /* with the same class and that no */
    Bool result;                        /* vars have been alloc'd since o. */
    bindOnlyAbove(o);
    result = unifyPred(pi1,o1,pi,o);
    unrestrictBind();
    return result;
}

Bool unifyPred(pi1,o1,pi,o)             /* Unify two predicates            */
Cell pi1;                               /* Assumes preds are kind correct  */
Int  o1;                                /* with the same class.            */
Cell pi;
Int  o; {
    for (; isAp(pi1); pi1=fun(pi1), pi=fun(pi))
        if (!unify(arg(pi1),o1,arg(pi),o))
            return FALSE;
#if IPARAM
    if (isIP(pi1) && isIP(pi))
	return textOf(pi1)==textOf(pi);
    else
#endif
    return pi1==pi;
}

#if TREX
static Cell trexShow = NIL;             /* Used to test for show on records*/
static Cell trexEq   = NIL;             /* Used to test for eq on records  */
#endif

Inst findInstFor(pi,o)                  /* Find matching instance for pred */
Cell  pi;                               /* (pi,o), or otherwise NIL.  If a */
Int   o; {                              /* match is found, then tyvars from*/
    Class c = getHead(pi);              /* typeOff have been initialized to*/
    List  ins;                          /* allow direct use of specifics.  */
    Cell  kspi = NIL;

    if (!isClass(c))
        return NIL;

    for (ins=cclass(c).instances; nonNull(ins); ins=tl(ins)) {
        Inst in   = hd(ins);
        Int  beta = newKindedVars(inst(in).kinds);
        if (matchPred(pi,o,inst(in).head,beta)) {
            typeOff = beta;
            return in;
        }
	else {
	    numTyvars = beta;
	    if (allowOverlap) {
		Int alpha = newKindedVars(inst(in).kinds);
		if (isNull(kspi)) {
		    kspi = copyNoMark(pi,o);
		}
		beta = newKindedVars(fst(kspi));
		if (matchPred(inst(in).head,alpha,snd(kspi),beta)) {
		    numTyvars = alpha;
		    return NIL;
		}
		numTyvars = alpha;
	    }
	}
    }
    unrestrictBind();

#if TREX
    {   Bool wantShow   = (c==findQualClass(trexShow));
        Bool wantEither = wantShow || (c==findQualClass(trexEq));

        if (wantEither) {                       /* Generate instances of   */
            Type  t = arg(pi);                  /* ShowRecRow and EqRecRow */
            Tyvar *tyv;                         /* on the fly              */
            Cell  e;
            deRef(tyv,t,o);
            e = getHead(t);
            if (isExt(e)) {
                Inst in = NIL;
                for (ins=cclass(c).instances; nonNull(ins); ins=tl(ins))
                    if (getHead(arg(inst(hd(ins)).head))==e) {
                        in = hd(ins);
                        break;
                    }
                if (isNull(in))
                    in = (wantShow ? addRecShowInst(c,e) : addRecEqInst(c,e));
                typeOff = newKindedVars(extKind);
                bindTv(typeOff,arg(fun(t)),o);
                bindTv(typeOff+1,arg(t),o);
                return in;
            }
        }
    }
#endif

    return NIL;
}

#if MULTI_INST
List findInstsFor(pi,o)			/* Find matching instance for pred */
Cell  pi;				/* (pi,o), or otherwise NIL.  If a */
Int   o; {				/* match is found, then tyvars from*/
    Class c = getHead(pi);		/* typeOff have been initialized to*/
    List  ins;				/* allow direct use of specifics.  */
    List  res = NIL;

    if (!isClass(c))
	return NIL;

    for (ins=cclass(c).instances; nonNull(ins); ins=tl(ins)) {
	Inst in   = hd(ins);
	Int  beta = newKindedVars(inst(in).kinds);
	if (matchPred(pi,o,inst(in).head,beta)) {
	    res = cons (pair (beta, in), res);
	    continue;
	}
	else
	    numTyvars = beta;
    }
    if (res == NIL) {
	unrestrictBind();
    }

    return rev(res);
}
#endif

/* --------------------------------------------------------------------------
 * Improvement:
 * ------------------------------------------------------------------------*/

Void improve(line,sps,ps)		/* Improve a list of predicates    */
Int  line;
List sps;
List ps; {
    Bool improved;
    List ps1;
    do {
	improved = FALSE;
	for (ps1=ps; nonNull(ps1); ps1=tl(ps1)) {
	    Cell pi = fst3(hd(ps1));
	    Int  o  = intOf(snd3(hd(ps1)));
	    Cell c  = getHead(pi);
	    if ((isClass(c) && nonNull(cclass(c).xfds)) || isIP(c)) {
		improved |= improveAgainst(line,sps,pi,o);
		if (!isIP(c))
		    improved |= instImprove(line,c,pi,o);
		improved |= improveAgainst(line,tl(ps1),pi,o);
	    }
	}
    } while (improved);
}

Void improve1(line,sps,pi,o)		/* Improve a single predicate	   */
Int  line;
List sps;
Cell pi;
Int o; {
    Bool improved;
    Cell c  = getHead(pi);
    do {
	improved = FALSE;
	if ((isClass(c) && nonNull(cclass(c).xfds)) || isIP(c)) {
	    improved |= improveAgainst(line,sps,pi,o);
	    if (!isIP(c))
		improved |= instImprove(line,c,pi,o);
	}
    } while (improved);
}

Bool improveAgainst(line,ps,pi,o)
Int line;
List ps;
Cell pi;
Int o; {
    Bool improved = FALSE;
    Cell h = getHead(pi);
    for (; nonNull(ps); ps=tl(ps)) {
	Cell pr = hd(ps);
	Cell pi1 = fst3(pr);
	Int o1 = intOf(snd3(pr));
	Cell h1 = getHead(pi1);
	/* it would be nice to optimize for the common case
	   where h == h1 */
	if (isClass(h) && isClass(h1)) {
	    improved |= pairImprove(line,h,pi,o,pi1,o1,numTyvars);
	    if (h != h1)
		improved |= pairImprove(line,h1,pi1,o1,pi,o,numTyvars);
	}
#if IPARAM
	else if (isIP(h1) && textOf(h1) == textOf(h))
	    improved |= ipImprove(line,pi,o,pi1,o1);
#endif
    }
    return improved;
}

Bool instImprove(line,c,pi,o)
Int line;
Class c;
Cell pi;
Int o; {
    Bool improved = FALSE;
    List ins      = cclass(c).instances;
    for (; nonNull(ins); ins=tl(ins)) {
	Cell in   = hd(ins);
	Int alpha = newKindedVars(inst(in).kinds);
	improved |= pairImprove(line,c,pi,o,inst(in).head,alpha,alpha);
    }
    return improved;
}

#if IPARAM
Bool ipImprove(line,pi,o,pi1,o1)
Int line;
Cell pi;
Int o;
Cell pi1;
Int o1; {
    Type t  = arg(pi);
    Type t1 = arg(pi1);
    if (!sameType(t,o,t1,o1)) {
	if (!unify(t,o,t1,o1)) {
	    ERRMSG(line) "Mismatching uses of implicit parameter\n"
	    ETHEN
	    ERRTEXT "\n***  "
	    ETHEN ERRPRED(copyPred(pi1,o1));
	    ERRTEXT "\n***  "
	    ETHEN ERRPRED(copyPred(pi,o));
	    ERRTEXT "\n"
	    EEND;
	}
	return TRUE;
    }
    return FALSE;
}
#endif

Bool pairImprove(line,c,pi1,o1,pi2,o2,above)	/* Look for improvement of (pi1,o1)*/
Int   line;				/* against (pi2,o2)                */
Class c;
Cell  pi1;
Int   o1;
Cell  pi2;
Int   o2;
Int above; {
    Bool improved = FALSE;
    List xfds     = cclass(c).xfds;
    for (; nonNull(xfds); xfds=tl(xfds)) {
	Cell xfd = hd(xfds);
	Cell hs  = fst(xfd);
	Int alpha;
	for (; nonNull(hs); hs=tl(hs)) {
	    Cell h  = hd(hs);
	    Class d = getHead(h);
	    alpha = newKindedVars(cclass(d).kinds);
	    if (matchPred(pi2,o2,h,alpha))
		break;
	    numTyvars = alpha;
	}
	if (nonNull(hs)) {
	    List fds = snd(xfd);
	    for (; nonNull(fds); fds=tl(fds)) {
		List as   = fst(hd(fds));
		Bool same = TRUE;
		for (; same && nonNull(as); as=tl(as)) {
		    Int n = offsetOf(hd(as));
		    same &= matchTypeAbove(nthArg(n,pi1),o1,
					   mkOffset(n),alpha,above);
		}
		if (isNull(as) && same) {
		    for (as=snd(hd(fds)); same && nonNull(as); as=tl(as)) {
			Int  n    = offsetOf(hd(as));
			Type t1   = nthArg(n,pi1);
			Type t2   = mkOffset(n);
			if (!matchTypeAbove(t1,o1,t2,alpha,above)) {
			    same &= unify(t1,o1,t2,alpha);
			    improved = TRUE;
			}
		    }
		    if (!same) {
			ERRMSG(line)
			  "Constraints are not consistent with functional dependency"
			ETHEN
			ERRTEXT "\n*** Constraint       : "
			ETHEN ERRPRED(copyPred(pi1,o1));
			ERRTEXT "\n*** And constraint   : "
			ETHEN ERRPRED(copyPred(pi2,o2));
			ERRTEXT "\n*** For class        : "
			ETHEN ERRPRED(cclass(c).head);
			ERRTEXT "\n*** Break dependency : "
			ETHEN ERRFD(hd(fds));
			ERRTEXT "\n"
			EEND;
		    }
		}
	    }
	    numTyvars = alpha;
	}
    }
    return improved;
}

/* --------------------------------------------------------------------------
 * Compare type schemes:
 * ------------------------------------------------------------------------*/

Bool sameSchemes(s,s1)                  /* Test to see whether two type    */
Type s;                                 /* schemes are the same            */
Type s1; {
    Int  o   = 0;
    Int  m   = 0;
    Int  nr2 = 0;
    Bool b   = isPolyType(s);           /* Check quantifiers are the same  */
    Bool b1  = isPolyType(s1);
    if (b || b1) {
        if (b && b1 && eqKind(polySigOf(s),polySigOf(s1))) {
            Kind k = polySigOf(s);
            s      = monotypeOf(s);
            s1     = monotypeOf(s1);
            o      = newKindedVars(k);
            for (; isAp(k); k=arg(k))
                m++;
        }
        else
            return FALSE;
    }

    b  = (whatIs(s)==QUAL);             /* Check that contexts are the same*/
    b1 = (whatIs(s1)==QUAL);
    if (b || b1) {
        if (b && b1) {
            List ps  = fst(snd(s));
            List ps1 = fst(snd(s1));
            noBind();
            while (nonNull(ps) && nonNull(ps1)) {
                Cell pi  = hd(ps);
                Cell pi1 = hd(ps1);
                if (getHead(pi)!=getHead(pi1)
                        || !unifyPred(pi,o,pi1,o))
                    break;
                ps  = tl(ps);
                ps1 = tl(ps1);
            }
            unrestrictBind();
            if (nonNull(ps) || nonNull(ps1))
                return FALSE;
            s  = snd(snd(s));
            s1 = snd(snd(s1));
        }
        else
            return FALSE;
    }

    b  = (whatIs(s)==RANK2);            /* Check any rank 2 annotations    */
    b1 = (whatIs(s1)==RANK2);
    if (b || b1) {
        if (b && b1 && intOf(fst(snd(s)))==intOf(fst(snd(s1)))) {
            nr2 = intOf(fst(snd(s)));
            s   = snd(snd(s));
            s1  = snd(snd(s1));
        }
        else
            return FALSE;
    }

    for (; nr2>0; nr2--) {              /* Deal with rank 2 arguments      */
        Type t  = arg(fun(s));
        Type t1 = arg(fun(s1));
	b       = isPolyOrQualType(t);
	b1      = isPolyOrQualType(t1);
        if (b || b1) {
            if (b && b1) {
                t  = dropRank1(t,o,m);
                t1 = dropRank1(t1,o,m);
                if (!sameSchemes(t,t1))
                    return FALSE;
            }
            else
                return FALSE;
        }
        else {
 	    if (!sameType(t,o,t1,o)) {
                return FALSE;
	    }
        }

        s  = arg(s);
        s1 = arg(s1);
    }

    return sameType(s,o,s1,o);		/* Ensure body types are the same  */
}

Bool sameType(t1,o1,t,o)		/* Test to see if types are	   */
Type t1;				/* the same, with no binding of	   */
Int  o1;				/* the variables in either one.	   */
Cell t;					/* Assumes types are kind correct  */
Int  o; {				/* with the same kind.		   */
    Bool result;
    noBind();
    result = unify(t1,o1,t,o);
    unrestrictBind();
    return result;
}

Bool matchType(t1,o1,t,o)		/* One way match type (t1,o1)	   */
Type t1;				/* against (t,o), allowing only	   */
Int  o1;				/* vars in 2nd type to be bound.   */
Type t;					/* Assumes types are kind correct  */
Int  o; {				/* and that no vars have been	   */
    Bool result;			/* alloc'd since o.		   */
    bindOnlyAbove(o);
    result = unify(t1,o1,t,o);
    unrestrictBind();
    return result;
}

static Bool local matchTypeAbove(t1,o1,t,o,a)	/* match, allowing only vars */
Type t1;				/* allocated since `a' to be bound   */
Int  o1;				/* this is deeply hacky, since it    */
Type t;					/* relies on careful use of the	     */
Int  o;					/* substitution stack		     */
Int  a; {
    Bool result;
    bindOnlyAbove(a);
    result = unify(t1,o1,t,o);
    unrestrictBind();
    return result;
}

/* --------------------------------------------------------------------------
 * Unify kind expressions:
 * ------------------------------------------------------------------------*/

static Bool local kvarToVarBind(tyv1,tyv2)/* Make binding tyv1 := tyv2     */
Tyvar *tyv1, *tyv2; {                     /* for kind variable bindings    */
    if (tyv1!=tyv2) {
        tyv1->bound = aVar;
        tyv1->offs  = tyvNum(tyv2);
#ifdef DEBUG_KINDS
        Printf("vv binding kvar: _%d to _%d\n",tyvNum(tyv1),tyvNum(tyv2));
#endif
    }
    return TRUE;
}

static Bool local kvarToTypeBind(tyv,t,o)/* Make binding tyv := (t,o)      */
Tyvar *tyv;                             /* for kind variable bindings      */
Type  t;                                /* guaranteed not to be a v'ble or */
Int   o; {                              /* have synonym as outermost constr*/
    if (doesntOccurIn(tyv,t,o)) {
        tyv->bound = t;
        tyv->offs  = o;
#ifdef DEBUG_KINDS
        Printf("vt binding kind variable: _%d to ",tyvNum(tyv));
        printType(stdout,debugType(t,o));
        Putchar('\n');
#endif
        return TRUE;
    }
    unifyFails = "unification would give infinite kind";
    return FALSE;
}

Bool kunify(k1,o1,k2,o2)                /* Unify kind expr (k1,o1) with    */
Kind k1,k2;                             /* (k2,o2)                         */
Int  o1,o2; {
    Tyvar *kyv1, *kyv2;

    deRef(kyv1,k1,o1);
    deRef(kyv2,k2,o2);

    if (kyv1) {
        if (kyv2)
            return kvarToVarBind(kyv1,kyv2);        /* k1, k2 variables    */
        else
            return kvarToTypeBind(kyv1,k2,o2);      /* k1 variable, k2 not */
    }
    else
        if (kyv2)
            return kvarToTypeBind(kyv2,k1,o1);      /* k2 variable, k1 not */
        else {
#ifdef DEBUG_KINDS
            Printf("unifying kinds: ");
            printType(stdout,debugType(k1,o1));
            Printf(" with ");
            printType(stdout,debugType(k2,o2));
            Putchar('\n');
#endif
            if (k1==STAR && k2==STAR)               /* k1, k2 not vars     */
                return TRUE;
#if TREX
            else if (k1==ROW && k2==ROW)
                return TRUE;
#endif
            else if (isAp(k1) && isAp(k2))
                return kunify(fst(k1),o1,fst(k2),o2) &&
                       kunify(snd(k1),o1,snd(k2),o2);
        }
    unifyFails = 0;
    return FALSE;
}

/* --------------------------------------------------------------------------
 * Tuple type constructors: are generated as necessary.  The most common
 * n-tuple constructors (n<MAXTUPCON) are held in a cache to avoid
 * repeated generation of the constructor types.
 * ------------------------------------------------------------------------*/

#define MAXTUPCON 10
static Type tupleConTypes[MAXTUPCON];

Void typeTuple(e)                      /* find type for tuple constr, using*/
Cell e; {                              /* tupleConTypes to cache previously*/
    Int n   = tupleOf(e);              /* calculated tuple constr. types.  */
    typeOff = newTyvars(n);
    if (n>=MAXTUPCON)
         typeIs = makeTupleType(n);
    else if (tupleConTypes[n])
         typeIs = tupleConTypes[n];
    else
         typeIs = tupleConTypes[n] = makeTupleType(n);
}

static Type local makeTupleType(n)     /* construct type for tuple constr. */
Int n; {                               /* t1 -> ... -> tn -> (t1,...,tn)   */
    Type h = mkTuple(n);
    Int  i;

    for (i=0; i<n; ++i)
        h = ap(h,mkOffset(i));
    while (0<n--)
        h = fn(mkOffset(n),h);
    return h;
}

/* --------------------------------------------------------------------------
 * Two forms of kind expression are used quite frequently:
 *      *  -> *  -> ... -> *  -> *      for kinds of ->, [], ->, (,) etc...
 *      v1 -> v2 -> ... -> vn -> vn+1   skeletons for constructor kinds
 * Expressions of these forms are produced by the following functions which
 * use a cache to avoid repeated construction of commonly used values.
 * A similar approach is used to store the types of tuple constructors in the
 * main type checker.
 * ------------------------------------------------------------------------*/

#define MAXKINDFUN 10
static  Kind simpleKindCache[MAXKINDFUN];
static  Kind varKindCache[MAXKINDFUN];

static Kind local makeSimpleKind(n)     /* construct * -> ... -> * (n args)*/
Int n; {
    Kind k = STAR;
    while (n-- > 0)
        k = ap(STAR,k);
    return k;
}

Kind simpleKind(n)                      /* return (possibly cached) simple */
Int n; {                                /* function kind                   */
    if (n>=MAXKINDFUN)
        return makeSimpleKind(n);
    else if (nonNull(simpleKindCache[n]))
        return simpleKindCache[n];
    else if (n==0)
        return simpleKindCache[0] = STAR;
    else
        return simpleKindCache[n] = ap(STAR,simpleKind(n-1));
}

static Kind local makeVarKind(n)        /* construct v0 -> .. -> vn        */
Int n; {
    Kind k = mkOffset(n);
    while (n-- > 0)
        k = ap(mkOffset(n),k);
    return k;
}

Void varKind(n)                         /* return (possibly cached) var    */
Int n; {                                /* function kind                   */
    typeOff = newKindvars(n+1);
    if (n>=MAXKINDFUN)
        typeIs = makeVarKind(n);
    else if (nonNull(varKindCache[n]))
        typeIs = varKindCache[n];
    else
        typeIs = varKindCache[n] = makeVarKind(n);
}

/* --------------------------------------------------------------------------
 * Substitutution control:
 * ------------------------------------------------------------------------*/

Void substitution(what)
Int what; {
    Int  i;

    switch (what) {
        case RESET   : emptySubstitution();
                       unrestrictBind();
                       btyvars = NIL;
                       break;

        case MARK    : for (i=0; i<MAXTUPCON; ++i)
                           mark(tupleConTypes[i]);
                       for (i=0; i<MAXKINDFUN; ++i) {
                           mark(simpleKindCache[i]);
                           mark(varKindCache[i]);
                       }
                       for (i=0; i<numTyvars; ++i)
                           mark(tyvars[i].bound);
                       mark(btyvars);
                       mark(typeIs);
                       mark(predsAre);
                       mark(genericVars);
#if TREX
                       mark(trexShow);
                       mark(trexEq);
#endif
                       break;

        case INSTALL : substitution(RESET);
                       for (i=0; i<MAXTUPCON; ++i)
                           tupleConTypes[i] = NIL;
                       for (i=0; i<MAXKINDFUN; ++i) {
                           simpleKindCache[i] = NIL;
                           varKindCache[i]    = NIL;
                       }
#if TREX
                       trexShow = mkQCon(findText("Trex"),
                                         findText("ShowRecRow"));
                       trexEq   = mkQCon(findText("Trex"),
                                         findText("EqRecRow"));
#endif
                       break;
    }
}

/*-------------------------------------------------------------------------*/
