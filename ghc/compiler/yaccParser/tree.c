

#include "hspincl.h"
#include "yaccParser/tree.h"

Ttree ttree(t)
 tree t;
{
	return(t -> tag);
}


/************** hmodule ******************/

tree mkhmodule(PPghname, PPghimplist, PPghexplist, PPghmodlist, PPghmodline)
 stringId PPghname;
 list PPghimplist;
 list PPghexplist;
 binding PPghmodlist;
 long PPghmodline;
{
	register struct Shmodule *pp =
		(struct Shmodule *) malloc(sizeof(struct Shmodule));
	pp -> tag = hmodule;
	pp -> Xghname = PPghname;
	pp -> Xghimplist = PPghimplist;
	pp -> Xghexplist = PPghexplist;
	pp -> Xghmodlist = PPghmodlist;
	pp -> Xghmodline = PPghmodline;
	return((tree)pp);
}

stringId *Rghname(t)
 struct Shmodule *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghname: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghname);
}

list *Rghimplist(t)
 struct Shmodule *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghimplist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghimplist);
}

list *Rghexplist(t)
 struct Shmodule *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghexplist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghexplist);
}

binding *Rghmodlist(t)
 struct Shmodule *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghmodlist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghmodlist);
}

long *Rghmodline(t)
 struct Shmodule *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != hmodule)
		fprintf(stderr,"ghmodline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xghmodline);
}

/************** ident ******************/

tree mkident(PPgident)
 unkId PPgident;
{
	register struct Sident *pp =
		(struct Sident *) malloc(sizeof(struct Sident));
	pp -> tag = ident;
	pp -> Xgident = PPgident;
	return((tree)pp);
}

unkId *Rgident(t)
 struct Sident *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ident)
		fprintf(stderr,"gident: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgident);
}

/************** lit ******************/

tree mklit(PPglit)
 literal PPglit;
{
	register struct Slit *pp =
		(struct Slit *) malloc(sizeof(struct Slit));
	pp -> tag = lit;
	pp -> Xglit = PPglit;
	return((tree)pp);
}

literal *Rglit(t)
 struct Slit *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != lit)
		fprintf(stderr,"glit: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglit);
}

/************** tuple ******************/

tree mktuple(PPgtuplelist)
 list PPgtuplelist;
{
	register struct Stuple *pp =
		(struct Stuple *) malloc(sizeof(struct Stuple));
	pp -> tag = tuple;
	pp -> Xgtuplelist = PPgtuplelist;
	return((tree)pp);
}

list *Rgtuplelist(t)
 struct Stuple *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != tuple)
		fprintf(stderr,"gtuplelist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtuplelist);
}

/************** ap ******************/

tree mkap(PPgfun, PPgarg)
 tree PPgfun;
 tree PPgarg;
{
	register struct Sap *pp =
		(struct Sap *) malloc(sizeof(struct Sap));
	pp -> tag = ap;
	pp -> Xgfun = PPgfun;
	pp -> Xgarg = PPgarg;
	return((tree)pp);
}

tree *Rgfun(t)
 struct Sap *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ap)
		fprintf(stderr,"gfun: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgfun);
}

tree *Rgarg(t)
 struct Sap *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ap)
		fprintf(stderr,"garg: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgarg);
}

/************** lambda ******************/

tree mklambda(PPglampats, PPglamexpr, PPglamline)
 list PPglampats;
 tree PPglamexpr;
 long PPglamline;
{
	register struct Slambda *pp =
		(struct Slambda *) malloc(sizeof(struct Slambda));
	pp -> tag = lambda;
	pp -> Xglampats = PPglampats;
	pp -> Xglamexpr = PPglamexpr;
	pp -> Xglamline = PPglamline;
	return((tree)pp);
}

list *Rglampats(t)
 struct Slambda *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != lambda)
		fprintf(stderr,"glampats: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglampats);
}

tree *Rglamexpr(t)
 struct Slambda *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != lambda)
		fprintf(stderr,"glamexpr: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglamexpr);
}

long *Rglamline(t)
 struct Slambda *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != lambda)
		fprintf(stderr,"glamline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglamline);
}

/************** let ******************/

tree mklet(PPgletvdeflist, PPgletvexpr)
 binding PPgletvdeflist;
 tree PPgletvexpr;
{
	register struct Slet *pp =
		(struct Slet *) malloc(sizeof(struct Slet));
	pp -> tag = let;
	pp -> Xgletvdeflist = PPgletvdeflist;
	pp -> Xgletvexpr = PPgletvexpr;
	return((tree)pp);
}

binding *Rgletvdeflist(t)
 struct Slet *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != let)
		fprintf(stderr,"gletvdeflist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgletvdeflist);
}

tree *Rgletvexpr(t)
 struct Slet *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != let)
		fprintf(stderr,"gletvexpr: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgletvexpr);
}

/************** casee ******************/

tree mkcasee(PPgcaseexpr, PPgcasebody)
 tree PPgcaseexpr;
 list PPgcasebody;
{
	register struct Scasee *pp =
		(struct Scasee *) malloc(sizeof(struct Scasee));
	pp -> tag = casee;
	pp -> Xgcaseexpr = PPgcaseexpr;
	pp -> Xgcasebody = PPgcasebody;
	return((tree)pp);
}

tree *Rgcaseexpr(t)
 struct Scasee *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != casee)
		fprintf(stderr,"gcaseexpr: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcaseexpr);
}

list *Rgcasebody(t)
 struct Scasee *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != casee)
		fprintf(stderr,"gcasebody: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcasebody);
}

/************** ife ******************/

tree mkife(PPgifpred, PPgifthen, PPgifelse)
 tree PPgifpred;
 tree PPgifthen;
 tree PPgifelse;
{
	register struct Sife *pp =
		(struct Sife *) malloc(sizeof(struct Sife));
	pp -> tag = ife;
	pp -> Xgifpred = PPgifpred;
	pp -> Xgifthen = PPgifthen;
	pp -> Xgifelse = PPgifelse;
	return((tree)pp);
}

tree *Rgifpred(t)
 struct Sife *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ife)
		fprintf(stderr,"gifpred: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgifpred);
}

tree *Rgifthen(t)
 struct Sife *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ife)
		fprintf(stderr,"gifthen: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgifthen);
}

tree *Rgifelse(t)
 struct Sife *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ife)
		fprintf(stderr,"gifelse: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgifelse);
}

/************** par ******************/

tree mkpar(PPgpare)
 tree PPgpare;
{
	register struct Spar *pp =
		(struct Spar *) malloc(sizeof(struct Spar));
	pp -> tag = par;
	pp -> Xgpare = PPgpare;
	return((tree)pp);
}

tree *Rgpare(t)
 struct Spar *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != par)
		fprintf(stderr,"gpare: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgpare);
}

/************** as ******************/

tree mkas(PPgasid, PPgase)
 unkId PPgasid;
 tree PPgase;
{
	register struct Sas *pp =
		(struct Sas *) malloc(sizeof(struct Sas));
	pp -> tag = as;
	pp -> Xgasid = PPgasid;
	pp -> Xgase = PPgase;
	return((tree)pp);
}

unkId *Rgasid(t)
 struct Sas *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != as)
		fprintf(stderr,"gasid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgasid);
}

tree *Rgase(t)
 struct Sas *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != as)
		fprintf(stderr,"gase: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgase);
}

/************** lazyp ******************/

tree mklazyp(PPglazyp)
 tree PPglazyp;
{
	register struct Slazyp *pp =
		(struct Slazyp *) malloc(sizeof(struct Slazyp));
	pp -> tag = lazyp;
	pp -> Xglazyp = PPglazyp;
	return((tree)pp);
}

tree *Rglazyp(t)
 struct Slazyp *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != lazyp)
		fprintf(stderr,"glazyp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglazyp);
}

/************** plusp ******************/

tree mkplusp(PPgplusp, PPgplusi)
 tree PPgplusp;
 literal PPgplusi;
{
	register struct Splusp *pp =
		(struct Splusp *) malloc(sizeof(struct Splusp));
	pp -> tag = plusp;
	pp -> Xgplusp = PPgplusp;
	pp -> Xgplusi = PPgplusi;
	return((tree)pp);
}

tree *Rgplusp(t)
 struct Splusp *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != plusp)
		fprintf(stderr,"gplusp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgplusp);
}

literal *Rgplusi(t)
 struct Splusp *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != plusp)
		fprintf(stderr,"gplusi: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgplusi);
}

/************** wildp ******************/

tree mkwildp(void)
{
	register struct Swildp *pp =
		(struct Swildp *) malloc(sizeof(struct Swildp));
	pp -> tag = wildp;
	return((tree)pp);
}

/************** restr ******************/

tree mkrestr(PPgrestre, PPgrestrt)
 tree PPgrestre;
 ttype PPgrestrt;
{
	register struct Srestr *pp =
		(struct Srestr *) malloc(sizeof(struct Srestr));
	pp -> tag = restr;
	pp -> Xgrestre = PPgrestre;
	pp -> Xgrestrt = PPgrestrt;
	return((tree)pp);
}

tree *Rgrestre(t)
 struct Srestr *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != restr)
		fprintf(stderr,"grestre: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrestre);
}

ttype *Rgrestrt(t)
 struct Srestr *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != restr)
		fprintf(stderr,"grestrt: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrestrt);
}

/************** comprh ******************/

tree mkcomprh(PPgcexp, PPgcquals)
 tree PPgcexp;
 list PPgcquals;
{
	register struct Scomprh *pp =
		(struct Scomprh *) malloc(sizeof(struct Scomprh));
	pp -> tag = comprh;
	pp -> Xgcexp = PPgcexp;
	pp -> Xgcquals = PPgcquals;
	return((tree)pp);
}

tree *Rgcexp(t)
 struct Scomprh *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != comprh)
		fprintf(stderr,"gcexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcexp);
}

list *Rgcquals(t)
 struct Scomprh *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != comprh)
		fprintf(stderr,"gcquals: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcquals);
}

/************** qual ******************/

tree mkqual(PPgqpat, PPgqexp)
 tree PPgqpat;
 tree PPgqexp;
{
	register struct Squal *pp =
		(struct Squal *) malloc(sizeof(struct Squal));
	pp -> tag = qual;
	pp -> Xgqpat = PPgqpat;
	pp -> Xgqexp = PPgqexp;
	return((tree)pp);
}

tree *Rgqpat(t)
 struct Squal *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != qual)
		fprintf(stderr,"gqpat: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgqpat);
}

tree *Rgqexp(t)
 struct Squal *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != qual)
		fprintf(stderr,"gqexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgqexp);
}

/************** guard ******************/

tree mkguard(PPggexp)
 tree PPggexp;
{
	register struct Sguard *pp =
		(struct Sguard *) malloc(sizeof(struct Sguard));
	pp -> tag = guard;
	pp -> Xggexp = PPggexp;
	return((tree)pp);
}

tree *Rggexp(t)
 struct Sguard *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != guard)
		fprintf(stderr,"ggexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xggexp);
}

/************** def ******************/

tree mkdef(PPggdef)
 tree PPggdef;
{
	register struct Sdef *pp =
		(struct Sdef *) malloc(sizeof(struct Sdef));
	pp -> tag = def;
	pp -> Xggdef = PPggdef;
	return((tree)pp);
}

tree *Rggdef(t)
 struct Sdef *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != def)
		fprintf(stderr,"ggdef: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xggdef);
}

/************** tinfixop ******************/

tree mktinfixop(PPgdummy)
 infixTree PPgdummy;
{
	register struct Stinfixop *pp =
		(struct Stinfixop *) malloc(sizeof(struct Stinfixop));
	pp -> tag = tinfixop;
	pp -> Xgdummy = PPgdummy;
	return((tree)pp);
}

infixTree *Rgdummy(t)
 struct Stinfixop *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != tinfixop)
		fprintf(stderr,"gdummy: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdummy);
}

/************** lsection ******************/

tree mklsection(PPglsexp, PPglsop)
 tree PPglsexp;
 unkId PPglsop;
{
	register struct Slsection *pp =
		(struct Slsection *) malloc(sizeof(struct Slsection));
	pp -> tag = lsection;
	pp -> Xglsexp = PPglsexp;
	pp -> Xglsop = PPglsop;
	return((tree)pp);
}

tree *Rglsexp(t)
 struct Slsection *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != lsection)
		fprintf(stderr,"glsexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglsexp);
}

unkId *Rglsop(t)
 struct Slsection *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != lsection)
		fprintf(stderr,"glsop: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xglsop);
}

/************** rsection ******************/

tree mkrsection(PPgrsop, PPgrsexp)
 unkId PPgrsop;
 tree PPgrsexp;
{
	register struct Srsection *pp =
		(struct Srsection *) malloc(sizeof(struct Srsection));
	pp -> tag = rsection;
	pp -> Xgrsop = PPgrsop;
	pp -> Xgrsexp = PPgrsexp;
	return((tree)pp);
}

unkId *Rgrsop(t)
 struct Srsection *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != rsection)
		fprintf(stderr,"grsop: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrsop);
}

tree *Rgrsexp(t)
 struct Srsection *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != rsection)
		fprintf(stderr,"grsexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgrsexp);
}

/************** eenum ******************/

tree mkeenum(PPgefrom, PPgestep, PPgeto)
 tree PPgefrom;
 list PPgestep;
 list PPgeto;
{
	register struct Seenum *pp =
		(struct Seenum *) malloc(sizeof(struct Seenum));
	pp -> tag = eenum;
	pp -> Xgefrom = PPgefrom;
	pp -> Xgestep = PPgestep;
	pp -> Xgeto = PPgeto;
	return((tree)pp);
}

tree *Rgefrom(t)
 struct Seenum *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != eenum)
		fprintf(stderr,"gefrom: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgefrom);
}

list *Rgestep(t)
 struct Seenum *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != eenum)
		fprintf(stderr,"gestep: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgestep);
}

list *Rgeto(t)
 struct Seenum *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != eenum)
		fprintf(stderr,"geto: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgeto);
}

/************** llist ******************/

tree mkllist(PPgllist)
 list PPgllist;
{
	register struct Sllist *pp =
		(struct Sllist *) malloc(sizeof(struct Sllist));
	pp -> tag = llist;
	pp -> Xgllist = PPgllist;
	return((tree)pp);
}

list *Rgllist(t)
 struct Sllist *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != llist)
		fprintf(stderr,"gllist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgllist);
}

/************** ccall ******************/

tree mkccall(PPgccid, PPgccinfo, PPgccargs)
 stringId PPgccid;
 stringId PPgccinfo;
 list PPgccargs;
{
	register struct Sccall *pp =
		(struct Sccall *) malloc(sizeof(struct Sccall));
	pp -> tag = ccall;
	pp -> Xgccid = PPgccid;
	pp -> Xgccinfo = PPgccinfo;
	pp -> Xgccargs = PPgccargs;
	return((tree)pp);
}

stringId *Rgccid(t)
 struct Sccall *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ccall)
		fprintf(stderr,"gccid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgccid);
}

stringId *Rgccinfo(t)
 struct Sccall *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ccall)
		fprintf(stderr,"gccinfo: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgccinfo);
}

list *Rgccargs(t)
 struct Sccall *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ccall)
		fprintf(stderr,"gccargs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgccargs);
}

/************** scc ******************/

tree mkscc(PPgsccid, PPgsccexp)
 hstring PPgsccid;
 tree PPgsccexp;
{
	register struct Sscc *pp =
		(struct Sscc *) malloc(sizeof(struct Sscc));
	pp -> tag = scc;
	pp -> Xgsccid = PPgsccid;
	pp -> Xgsccexp = PPgsccexp;
	return((tree)pp);
}

hstring *Rgsccid(t)
 struct Sscc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != scc)
		fprintf(stderr,"gsccid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgsccid);
}

tree *Rgsccexp(t)
 struct Sscc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != scc)
		fprintf(stderr,"gsccexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgsccexp);
}

/************** negate ******************/

tree mknegate(PPgnexp)
 tree PPgnexp;
{
	register struct Snegate *pp =
		(struct Snegate *) malloc(sizeof(struct Snegate));
	pp -> tag = negate;
	pp -> Xgnexp = PPgnexp;
	return((tree)pp);
}

tree *Rgnexp(t)
 struct Snegate *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != negate)
		fprintf(stderr,"gnexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnexp);
}
