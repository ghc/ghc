#ifndef tree_defined
#define tree_defined

#include <stdio.h>

#ifndef PROTO
#ifdef __STDC__
#define PROTO(x) x
#else
#define PROTO(x) /**/
#endif
#endif

typedef enum {
	hmodule,
	ident,
	lit,
	tuple,
	ap,
	lambda,
	let,
	casee,
	ife,
	par,
	as,
	lazyp,
	plusp,
	wildp,
	restr,
	comprh,
	qual,
	guard,
	def,
	tinfixop,
	lsection,
	rsection,
	eenum,
	llist,
	ccall,
	scc,
	negate
} Ttree;

typedef struct { Ttree tag; } *tree;

#ifdef __GNUC__
Ttree ttree(tree t);
extern __inline__ Ttree ttree(tree t)
{
	return(t -> tag);
}
#else  /* ! __GNUC__ */
extern Ttree ttree PROTO((tree));
#endif /* ! __GNUC__ */

struct Shmodule {
	Ttree tag;
	stringId Xghname;
	list Xghimplist;
	list Xghexplist;
	binding Xghmodlist;
	long Xghmodline;
};

struct Sident {
	Ttree tag;
	unkId Xgident;
};

struct Slit {
	Ttree tag;
	literal Xglit;
};

struct Stuple {
	Ttree tag;
	list Xgtuplelist;
};

struct Sap {
	Ttree tag;
	tree Xgfun;
	tree Xgarg;
};

struct Slambda {
	Ttree tag;
	list Xglampats;
	tree Xglamexpr;
	long Xglamline;
};

struct Slet {
	Ttree tag;
	binding Xgletvdeflist;
	tree Xgletvexpr;
};

struct Scasee {
	Ttree tag;
	tree Xgcaseexpr;
	list Xgcasebody;
};

struct Sife {
	Ttree tag;
	tree Xgifpred;
	tree Xgifthen;
	tree Xgifelse;
};

struct Spar {
	Ttree tag;
	tree Xgpare;
};

struct Sas {
	Ttree tag;
	unkId Xgasid;
	tree Xgase;
};

struct Slazyp {
	Ttree tag;
	tree Xglazyp;
};

struct Splusp {
	Ttree tag;
	tree Xgplusp;
	literal Xgplusi;
};

struct Swildp {
	Ttree tag;
};

struct Srestr {
	Ttree tag;
	tree Xgrestre;
	ttype Xgrestrt;
};

struct Scomprh {
	Ttree tag;
	tree Xgcexp;
	list Xgcquals;
};

struct Squal {
	Ttree tag;
	tree Xgqpat;
	tree Xgqexp;
};

struct Sguard {
	Ttree tag;
	tree Xggexp;
};

struct Sdef {
	Ttree tag;
	tree Xggdef;
};

struct Stinfixop {
	Ttree tag;
	infixTree Xgdummy;
};

struct Slsection {
	Ttree tag;
	tree Xglsexp;
	unkId Xglsop;
};

struct Srsection {
	Ttree tag;
	unkId Xgrsop;
	tree Xgrsexp;
};

struct Seenum {
	Ttree tag;
	tree Xgefrom;
	list Xgestep;
	list Xgeto;
};

struct Sllist {
	Ttree tag;
	list Xgllist;
};

struct Sccall {
	Ttree tag;
	stringId Xgccid;
	stringId Xgccinfo;
	list Xgccargs;
};

struct Sscc {
	Ttree tag;
	hstring Xgsccid;
	tree Xgsccexp;
};

struct Snegate {
	Ttree tag;
	tree Xgnexp;
};

extern tree mkhmodule PROTO((stringId, list, list, binding, long));
#ifdef __GNUC__

stringId *Rghname PROTO((struct Shmodule *));

extern __inline__ stringId *Rghname(struct Shmodule *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghname: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghname);
}
#else  /* ! __GNUC__ */
extern stringId *Rghname PROTO((struct Shmodule *));
#endif /* ! __GNUC__ */

#define ghname(xyzxyz) (*Rghname((struct Shmodule *) (xyzxyz)))
#ifdef __GNUC__

list *Rghimplist PROTO((struct Shmodule *));

extern __inline__ list *Rghimplist(struct Shmodule *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghimplist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghimplist);
}
#else  /* ! __GNUC__ */
extern list *Rghimplist PROTO((struct Shmodule *));
#endif /* ! __GNUC__ */

#define ghimplist(xyzxyz) (*Rghimplist((struct Shmodule *) (xyzxyz)))
#ifdef __GNUC__

list *Rghexplist PROTO((struct Shmodule *));

extern __inline__ list *Rghexplist(struct Shmodule *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghexplist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghexplist);
}
#else  /* ! __GNUC__ */
extern list *Rghexplist PROTO((struct Shmodule *));
#endif /* ! __GNUC__ */

#define ghexplist(xyzxyz) (*Rghexplist((struct Shmodule *) (xyzxyz)))
#ifdef __GNUC__

binding *Rghmodlist PROTO((struct Shmodule *));

extern __inline__ binding *Rghmodlist(struct Shmodule *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghmodlist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghmodlist);
}
#else  /* ! __GNUC__ */
extern binding *Rghmodlist PROTO((struct Shmodule *));
#endif /* ! __GNUC__ */

#define ghmodlist(xyzxyz) (*Rghmodlist((struct Shmodule *) (xyzxyz)))
#ifdef __GNUC__

long *Rghmodline PROTO((struct Shmodule *));

extern __inline__ long *Rghmodline(struct Shmodule *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghmodline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghmodline);
}
#else  /* ! __GNUC__ */
extern long *Rghmodline PROTO((struct Shmodule *));
#endif /* ! __GNUC__ */

#define ghmodline(xyzxyz) (*Rghmodline((struct Shmodule *) (xyzxyz)))

extern tree mkident PROTO((unkId));
#ifdef __GNUC__

unkId *Rgident PROTO((struct Sident *));

extern __inline__ unkId *Rgident(struct Sident *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ident)
		fprintf(stderr,"gident: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgident);
}
#else  /* ! __GNUC__ */
extern unkId *Rgident PROTO((struct Sident *));
#endif /* ! __GNUC__ */

#define gident(xyzxyz) (*Rgident((struct Sident *) (xyzxyz)))

extern tree mklit PROTO((literal));
#ifdef __GNUC__

literal *Rglit PROTO((struct Slit *));

extern __inline__ literal *Rglit(struct Slit *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != lit)
		fprintf(stderr,"glit: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglit);
}
#else  /* ! __GNUC__ */
extern literal *Rglit PROTO((struct Slit *));
#endif /* ! __GNUC__ */

#define glit(xyzxyz) (*Rglit((struct Slit *) (xyzxyz)))

extern tree mktuple PROTO((list));
#ifdef __GNUC__

list *Rgtuplelist PROTO((struct Stuple *));

extern __inline__ list *Rgtuplelist(struct Stuple *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != tuple)
		fprintf(stderr,"gtuplelist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtuplelist);
}
#else  /* ! __GNUC__ */
extern list *Rgtuplelist PROTO((struct Stuple *));
#endif /* ! __GNUC__ */

#define gtuplelist(xyzxyz) (*Rgtuplelist((struct Stuple *) (xyzxyz)))

extern tree mkap PROTO((tree, tree));
#ifdef __GNUC__

tree *Rgfun PROTO((struct Sap *));

extern __inline__ tree *Rgfun(struct Sap *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ap)
		fprintf(stderr,"gfun: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgfun);
}
#else  /* ! __GNUC__ */
extern tree *Rgfun PROTO((struct Sap *));
#endif /* ! __GNUC__ */

#define gfun(xyzxyz) (*Rgfun((struct Sap *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgarg PROTO((struct Sap *));

extern __inline__ tree *Rgarg(struct Sap *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ap)
		fprintf(stderr,"garg: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgarg);
}
#else  /* ! __GNUC__ */
extern tree *Rgarg PROTO((struct Sap *));
#endif /* ! __GNUC__ */

#define garg(xyzxyz) (*Rgarg((struct Sap *) (xyzxyz)))

extern tree mklambda PROTO((list, tree, long));
#ifdef __GNUC__

list *Rglampats PROTO((struct Slambda *));

extern __inline__ list *Rglampats(struct Slambda *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != lambda)
		fprintf(stderr,"glampats: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglampats);
}
#else  /* ! __GNUC__ */
extern list *Rglampats PROTO((struct Slambda *));
#endif /* ! __GNUC__ */

#define glampats(xyzxyz) (*Rglampats((struct Slambda *) (xyzxyz)))
#ifdef __GNUC__

tree *Rglamexpr PROTO((struct Slambda *));

extern __inline__ tree *Rglamexpr(struct Slambda *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != lambda)
		fprintf(stderr,"glamexpr: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglamexpr);
}
#else  /* ! __GNUC__ */
extern tree *Rglamexpr PROTO((struct Slambda *));
#endif /* ! __GNUC__ */

#define glamexpr(xyzxyz) (*Rglamexpr((struct Slambda *) (xyzxyz)))
#ifdef __GNUC__

long *Rglamline PROTO((struct Slambda *));

extern __inline__ long *Rglamline(struct Slambda *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != lambda)
		fprintf(stderr,"glamline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglamline);
}
#else  /* ! __GNUC__ */
extern long *Rglamline PROTO((struct Slambda *));
#endif /* ! __GNUC__ */

#define glamline(xyzxyz) (*Rglamline((struct Slambda *) (xyzxyz)))

extern tree mklet PROTO((binding, tree));
#ifdef __GNUC__

binding *Rgletvdeflist PROTO((struct Slet *));

extern __inline__ binding *Rgletvdeflist(struct Slet *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != let)
		fprintf(stderr,"gletvdeflist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgletvdeflist);
}
#else  /* ! __GNUC__ */
extern binding *Rgletvdeflist PROTO((struct Slet *));
#endif /* ! __GNUC__ */

#define gletvdeflist(xyzxyz) (*Rgletvdeflist((struct Slet *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgletvexpr PROTO((struct Slet *));

extern __inline__ tree *Rgletvexpr(struct Slet *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != let)
		fprintf(stderr,"gletvexpr: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgletvexpr);
}
#else  /* ! __GNUC__ */
extern tree *Rgletvexpr PROTO((struct Slet *));
#endif /* ! __GNUC__ */

#define gletvexpr(xyzxyz) (*Rgletvexpr((struct Slet *) (xyzxyz)))

extern tree mkcasee PROTO((tree, list));
#ifdef __GNUC__

tree *Rgcaseexpr PROTO((struct Scasee *));

extern __inline__ tree *Rgcaseexpr(struct Scasee *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != casee)
		fprintf(stderr,"gcaseexpr: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcaseexpr);
}
#else  /* ! __GNUC__ */
extern tree *Rgcaseexpr PROTO((struct Scasee *));
#endif /* ! __GNUC__ */

#define gcaseexpr(xyzxyz) (*Rgcaseexpr((struct Scasee *) (xyzxyz)))
#ifdef __GNUC__

list *Rgcasebody PROTO((struct Scasee *));

extern __inline__ list *Rgcasebody(struct Scasee *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != casee)
		fprintf(stderr,"gcasebody: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcasebody);
}
#else  /* ! __GNUC__ */
extern list *Rgcasebody PROTO((struct Scasee *));
#endif /* ! __GNUC__ */

#define gcasebody(xyzxyz) (*Rgcasebody((struct Scasee *) (xyzxyz)))

extern tree mkife PROTO((tree, tree, tree));
#ifdef __GNUC__

tree *Rgifpred PROTO((struct Sife *));

extern __inline__ tree *Rgifpred(struct Sife *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ife)
		fprintf(stderr,"gifpred: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgifpred);
}
#else  /* ! __GNUC__ */
extern tree *Rgifpred PROTO((struct Sife *));
#endif /* ! __GNUC__ */

#define gifpred(xyzxyz) (*Rgifpred((struct Sife *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgifthen PROTO((struct Sife *));

extern __inline__ tree *Rgifthen(struct Sife *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ife)
		fprintf(stderr,"gifthen: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgifthen);
}
#else  /* ! __GNUC__ */
extern tree *Rgifthen PROTO((struct Sife *));
#endif /* ! __GNUC__ */

#define gifthen(xyzxyz) (*Rgifthen((struct Sife *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgifelse PROTO((struct Sife *));

extern __inline__ tree *Rgifelse(struct Sife *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ife)
		fprintf(stderr,"gifelse: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgifelse);
}
#else  /* ! __GNUC__ */
extern tree *Rgifelse PROTO((struct Sife *));
#endif /* ! __GNUC__ */

#define gifelse(xyzxyz) (*Rgifelse((struct Sife *) (xyzxyz)))

extern tree mkpar PROTO((tree));
#ifdef __GNUC__

tree *Rgpare PROTO((struct Spar *));

extern __inline__ tree *Rgpare(struct Spar *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != par)
		fprintf(stderr,"gpare: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgpare);
}
#else  /* ! __GNUC__ */
extern tree *Rgpare PROTO((struct Spar *));
#endif /* ! __GNUC__ */

#define gpare(xyzxyz) (*Rgpare((struct Spar *) (xyzxyz)))

extern tree mkas PROTO((unkId, tree));
#ifdef __GNUC__

unkId *Rgasid PROTO((struct Sas *));

extern __inline__ unkId *Rgasid(struct Sas *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != as)
		fprintf(stderr,"gasid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgasid);
}
#else  /* ! __GNUC__ */
extern unkId *Rgasid PROTO((struct Sas *));
#endif /* ! __GNUC__ */

#define gasid(xyzxyz) (*Rgasid((struct Sas *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgase PROTO((struct Sas *));

extern __inline__ tree *Rgase(struct Sas *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != as)
		fprintf(stderr,"gase: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgase);
}
#else  /* ! __GNUC__ */
extern tree *Rgase PROTO((struct Sas *));
#endif /* ! __GNUC__ */

#define gase(xyzxyz) (*Rgase((struct Sas *) (xyzxyz)))

extern tree mklazyp PROTO((tree));
#ifdef __GNUC__

tree *Rglazyp PROTO((struct Slazyp *));

extern __inline__ tree *Rglazyp(struct Slazyp *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != lazyp)
		fprintf(stderr,"glazyp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglazyp);
}
#else  /* ! __GNUC__ */
extern tree *Rglazyp PROTO((struct Slazyp *));
#endif /* ! __GNUC__ */

#define glazyp(xyzxyz) (*Rglazyp((struct Slazyp *) (xyzxyz)))

extern tree mkplusp PROTO((tree, literal));
#ifdef __GNUC__

tree *Rgplusp PROTO((struct Splusp *));

extern __inline__ tree *Rgplusp(struct Splusp *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != plusp)
		fprintf(stderr,"gplusp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgplusp);
}
#else  /* ! __GNUC__ */
extern tree *Rgplusp PROTO((struct Splusp *));
#endif /* ! __GNUC__ */

#define gplusp(xyzxyz) (*Rgplusp((struct Splusp *) (xyzxyz)))
#ifdef __GNUC__

literal *Rgplusi PROTO((struct Splusp *));

extern __inline__ literal *Rgplusi(struct Splusp *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != plusp)
		fprintf(stderr,"gplusi: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgplusi);
}
#else  /* ! __GNUC__ */
extern literal *Rgplusi PROTO((struct Splusp *));
#endif /* ! __GNUC__ */

#define gplusi(xyzxyz) (*Rgplusi((struct Splusp *) (xyzxyz)))

extern tree mkwildp PROTO((void));

extern tree mkrestr PROTO((tree, ttype));
#ifdef __GNUC__

tree *Rgrestre PROTO((struct Srestr *));

extern __inline__ tree *Rgrestre(struct Srestr *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != restr)
		fprintf(stderr,"grestre: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrestre);
}
#else  /* ! __GNUC__ */
extern tree *Rgrestre PROTO((struct Srestr *));
#endif /* ! __GNUC__ */

#define grestre(xyzxyz) (*Rgrestre((struct Srestr *) (xyzxyz)))
#ifdef __GNUC__

ttype *Rgrestrt PROTO((struct Srestr *));

extern __inline__ ttype *Rgrestrt(struct Srestr *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != restr)
		fprintf(stderr,"grestrt: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrestrt);
}
#else  /* ! __GNUC__ */
extern ttype *Rgrestrt PROTO((struct Srestr *));
#endif /* ! __GNUC__ */

#define grestrt(xyzxyz) (*Rgrestrt((struct Srestr *) (xyzxyz)))

extern tree mkcomprh PROTO((tree, list));
#ifdef __GNUC__

tree *Rgcexp PROTO((struct Scomprh *));

extern __inline__ tree *Rgcexp(struct Scomprh *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != comprh)
		fprintf(stderr,"gcexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcexp);
}
#else  /* ! __GNUC__ */
extern tree *Rgcexp PROTO((struct Scomprh *));
#endif /* ! __GNUC__ */

#define gcexp(xyzxyz) (*Rgcexp((struct Scomprh *) (xyzxyz)))
#ifdef __GNUC__

list *Rgcquals PROTO((struct Scomprh *));

extern __inline__ list *Rgcquals(struct Scomprh *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != comprh)
		fprintf(stderr,"gcquals: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcquals);
}
#else  /* ! __GNUC__ */
extern list *Rgcquals PROTO((struct Scomprh *));
#endif /* ! __GNUC__ */

#define gcquals(xyzxyz) (*Rgcquals((struct Scomprh *) (xyzxyz)))

extern tree mkqual PROTO((tree, tree));
#ifdef __GNUC__

tree *Rgqpat PROTO((struct Squal *));

extern __inline__ tree *Rgqpat(struct Squal *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != qual)
		fprintf(stderr,"gqpat: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgqpat);
}
#else  /* ! __GNUC__ */
extern tree *Rgqpat PROTO((struct Squal *));
#endif /* ! __GNUC__ */

#define gqpat(xyzxyz) (*Rgqpat((struct Squal *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgqexp PROTO((struct Squal *));

extern __inline__ tree *Rgqexp(struct Squal *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != qual)
		fprintf(stderr,"gqexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgqexp);
}
#else  /* ! __GNUC__ */
extern tree *Rgqexp PROTO((struct Squal *));
#endif /* ! __GNUC__ */

#define gqexp(xyzxyz) (*Rgqexp((struct Squal *) (xyzxyz)))

extern tree mkguard PROTO((tree));
#ifdef __GNUC__

tree *Rggexp PROTO((struct Sguard *));

extern __inline__ tree *Rggexp(struct Sguard *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != guard)
		fprintf(stderr,"ggexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xggexp);
}
#else  /* ! __GNUC__ */
extern tree *Rggexp PROTO((struct Sguard *));
#endif /* ! __GNUC__ */

#define ggexp(xyzxyz) (*Rggexp((struct Sguard *) (xyzxyz)))

extern tree mkdef PROTO((tree));
#ifdef __GNUC__

tree *Rggdef PROTO((struct Sdef *));

extern __inline__ tree *Rggdef(struct Sdef *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != def)
		fprintf(stderr,"ggdef: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xggdef);
}
#else  /* ! __GNUC__ */
extern tree *Rggdef PROTO((struct Sdef *));
#endif /* ! __GNUC__ */

#define ggdef(xyzxyz) (*Rggdef((struct Sdef *) (xyzxyz)))

extern tree mktinfixop PROTO((infixTree));
#ifdef __GNUC__

infixTree *Rgdummy PROTO((struct Stinfixop *));

extern __inline__ infixTree *Rgdummy(struct Stinfixop *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != tinfixop)
		fprintf(stderr,"gdummy: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdummy);
}
#else  /* ! __GNUC__ */
extern infixTree *Rgdummy PROTO((struct Stinfixop *));
#endif /* ! __GNUC__ */

#define gdummy(xyzxyz) (*Rgdummy((struct Stinfixop *) (xyzxyz)))

extern tree mklsection PROTO((tree, unkId));
#ifdef __GNUC__

tree *Rglsexp PROTO((struct Slsection *));

extern __inline__ tree *Rglsexp(struct Slsection *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != lsection)
		fprintf(stderr,"glsexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglsexp);
}
#else  /* ! __GNUC__ */
extern tree *Rglsexp PROTO((struct Slsection *));
#endif /* ! __GNUC__ */

#define glsexp(xyzxyz) (*Rglsexp((struct Slsection *) (xyzxyz)))
#ifdef __GNUC__

unkId *Rglsop PROTO((struct Slsection *));

extern __inline__ unkId *Rglsop(struct Slsection *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != lsection)
		fprintf(stderr,"glsop: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglsop);
}
#else  /* ! __GNUC__ */
extern unkId *Rglsop PROTO((struct Slsection *));
#endif /* ! __GNUC__ */

#define glsop(xyzxyz) (*Rglsop((struct Slsection *) (xyzxyz)))

extern tree mkrsection PROTO((unkId, tree));
#ifdef __GNUC__

unkId *Rgrsop PROTO((struct Srsection *));

extern __inline__ unkId *Rgrsop(struct Srsection *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != rsection)
		fprintf(stderr,"grsop: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrsop);
}
#else  /* ! __GNUC__ */
extern unkId *Rgrsop PROTO((struct Srsection *));
#endif /* ! __GNUC__ */

#define grsop(xyzxyz) (*Rgrsop((struct Srsection *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgrsexp PROTO((struct Srsection *));

extern __inline__ tree *Rgrsexp(struct Srsection *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != rsection)
		fprintf(stderr,"grsexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrsexp);
}
#else  /* ! __GNUC__ */
extern tree *Rgrsexp PROTO((struct Srsection *));
#endif /* ! __GNUC__ */

#define grsexp(xyzxyz) (*Rgrsexp((struct Srsection *) (xyzxyz)))

extern tree mkeenum PROTO((tree, list, list));
#ifdef __GNUC__

tree *Rgefrom PROTO((struct Seenum *));

extern __inline__ tree *Rgefrom(struct Seenum *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != eenum)
		fprintf(stderr,"gefrom: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgefrom);
}
#else  /* ! __GNUC__ */
extern tree *Rgefrom PROTO((struct Seenum *));
#endif /* ! __GNUC__ */

#define gefrom(xyzxyz) (*Rgefrom((struct Seenum *) (xyzxyz)))
#ifdef __GNUC__

list *Rgestep PROTO((struct Seenum *));

extern __inline__ list *Rgestep(struct Seenum *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != eenum)
		fprintf(stderr,"gestep: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgestep);
}
#else  /* ! __GNUC__ */
extern list *Rgestep PROTO((struct Seenum *));
#endif /* ! __GNUC__ */

#define gestep(xyzxyz) (*Rgestep((struct Seenum *) (xyzxyz)))
#ifdef __GNUC__

list *Rgeto PROTO((struct Seenum *));

extern __inline__ list *Rgeto(struct Seenum *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != eenum)
		fprintf(stderr,"geto: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgeto);
}
#else  /* ! __GNUC__ */
extern list *Rgeto PROTO((struct Seenum *));
#endif /* ! __GNUC__ */

#define geto(xyzxyz) (*Rgeto((struct Seenum *) (xyzxyz)))

extern tree mkllist PROTO((list));
#ifdef __GNUC__

list *Rgllist PROTO((struct Sllist *));

extern __inline__ list *Rgllist(struct Sllist *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != llist)
		fprintf(stderr,"gllist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgllist);
}
#else  /* ! __GNUC__ */
extern list *Rgllist PROTO((struct Sllist *));
#endif /* ! __GNUC__ */

#define gllist(xyzxyz) (*Rgllist((struct Sllist *) (xyzxyz)))

extern tree mkccall PROTO((stringId, stringId, list));
#ifdef __GNUC__

stringId *Rgccid PROTO((struct Sccall *));

extern __inline__ stringId *Rgccid(struct Sccall *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ccall)
		fprintf(stderr,"gccid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgccid);
}
#else  /* ! __GNUC__ */
extern stringId *Rgccid PROTO((struct Sccall *));
#endif /* ! __GNUC__ */

#define gccid(xyzxyz) (*Rgccid((struct Sccall *) (xyzxyz)))
#ifdef __GNUC__

stringId *Rgccinfo PROTO((struct Sccall *));

extern __inline__ stringId *Rgccinfo(struct Sccall *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ccall)
		fprintf(stderr,"gccinfo: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgccinfo);
}
#else  /* ! __GNUC__ */
extern stringId *Rgccinfo PROTO((struct Sccall *));
#endif /* ! __GNUC__ */

#define gccinfo(xyzxyz) (*Rgccinfo((struct Sccall *) (xyzxyz)))
#ifdef __GNUC__

list *Rgccargs PROTO((struct Sccall *));

extern __inline__ list *Rgccargs(struct Sccall *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ccall)
		fprintf(stderr,"gccargs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgccargs);
}
#else  /* ! __GNUC__ */
extern list *Rgccargs PROTO((struct Sccall *));
#endif /* ! __GNUC__ */

#define gccargs(xyzxyz) (*Rgccargs((struct Sccall *) (xyzxyz)))

extern tree mkscc PROTO((hstring, tree));
#ifdef __GNUC__

hstring *Rgsccid PROTO((struct Sscc *));

extern __inline__ hstring *Rgsccid(struct Sscc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != scc)
		fprintf(stderr,"gsccid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgsccid);
}
#else  /* ! __GNUC__ */
extern hstring *Rgsccid PROTO((struct Sscc *));
#endif /* ! __GNUC__ */

#define gsccid(xyzxyz) (*Rgsccid((struct Sscc *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgsccexp PROTO((struct Sscc *));

extern __inline__ tree *Rgsccexp(struct Sscc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != scc)
		fprintf(stderr,"gsccexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgsccexp);
}
#else  /* ! __GNUC__ */
extern tree *Rgsccexp PROTO((struct Sscc *));
#endif /* ! __GNUC__ */

#define gsccexp(xyzxyz) (*Rgsccexp((struct Sscc *) (xyzxyz)))

extern tree mknegate PROTO((tree));
#ifdef __GNUC__

tree *Rgnexp PROTO((struct Snegate *));

extern __inline__ tree *Rgnexp(struct Snegate *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != negate)
		fprintf(stderr,"gnexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnexp);
}
#else  /* ! __GNUC__ */
extern tree *Rgnexp PROTO((struct Snegate *));
#endif /* ! __GNUC__ */

#define gnexp(xyzxyz) (*Rgnexp((struct Snegate *) (xyzxyz)))

#endif
