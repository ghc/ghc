

#include "hspincl.h"
#include "yaccParser/ttype.h"

Tttype tttype(t)
 ttype t;
{
	return(t -> tag);
}


/************** tname ******************/

ttype mktname(PPgtypeid, PPgtypel)
 unkId PPgtypeid;
 list PPgtypel;
{
	register struct Stname *pp =
		(struct Stname *) malloc(sizeof(struct Stname));
	pp -> tag = tname;
	pp -> Xgtypeid = PPgtypeid;
	pp -> Xgtypel = PPgtypel;
	return((ttype)pp);
}

unkId *Rgtypeid(t)
 struct Stname *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != tname)
		fprintf(stderr,"gtypeid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtypeid);
}

list *Rgtypel(t)
 struct Stname *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != tname)
		fprintf(stderr,"gtypel: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtypel);
}

/************** namedtvar ******************/

ttype mknamedtvar(PPgnamedtvar)
 unkId PPgnamedtvar;
{
	register struct Snamedtvar *pp =
		(struct Snamedtvar *) malloc(sizeof(struct Snamedtvar));
	pp -> tag = namedtvar;
	pp -> Xgnamedtvar = PPgnamedtvar;
	return((ttype)pp);
}

unkId *Rgnamedtvar(t)
 struct Snamedtvar *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != namedtvar)
		fprintf(stderr,"gnamedtvar: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnamedtvar);
}

/************** tllist ******************/

ttype mktllist(PPgtlist)
 ttype PPgtlist;
{
	register struct Stllist *pp =
		(struct Stllist *) malloc(sizeof(struct Stllist));
	pp -> tag = tllist;
	pp -> Xgtlist = PPgtlist;
	return((ttype)pp);
}

ttype *Rgtlist(t)
 struct Stllist *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != tllist)
		fprintf(stderr,"gtlist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtlist);
}

/************** ttuple ******************/

ttype mkttuple(PPgttuple)
 list PPgttuple;
{
	register struct Sttuple *pp =
		(struct Sttuple *) malloc(sizeof(struct Sttuple));
	pp -> tag = ttuple;
	pp -> Xgttuple = PPgttuple;
	return((ttype)pp);
}

list *Rgttuple(t)
 struct Sttuple *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ttuple)
		fprintf(stderr,"gttuple: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgttuple);
}

/************** tfun ******************/

ttype mktfun(PPgtfun, PPgtarg)
 ttype PPgtfun;
 ttype PPgtarg;
{
	register struct Stfun *pp =
		(struct Stfun *) malloc(sizeof(struct Stfun));
	pp -> tag = tfun;
	pp -> Xgtfun = PPgtfun;
	pp -> Xgtarg = PPgtarg;
	return((ttype)pp);
}

ttype *Rgtfun(t)
 struct Stfun *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != tfun)
		fprintf(stderr,"gtfun: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtfun);
}

ttype *Rgtarg(t)
 struct Stfun *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != tfun)
		fprintf(stderr,"gtarg: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtarg);
}

/************** context ******************/

ttype mkcontext(PPgtcontextl, PPgtcontextt)
 list PPgtcontextl;
 ttype PPgtcontextt;
{
	register struct Scontext *pp =
		(struct Scontext *) malloc(sizeof(struct Scontext));
	pp -> tag = context;
	pp -> Xgtcontextl = PPgtcontextl;
	pp -> Xgtcontextt = PPgtcontextt;
	return((ttype)pp);
}

list *Rgtcontextl(t)
 struct Scontext *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != context)
		fprintf(stderr,"gtcontextl: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtcontextl);
}

ttype *Rgtcontextt(t)
 struct Scontext *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != context)
		fprintf(stderr,"gtcontextt: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtcontextt);
}

/************** unidict ******************/

ttype mkunidict(PPgunidict_clas, PPgunidict_ty)
 unkId PPgunidict_clas;
 ttype PPgunidict_ty;
{
	register struct Sunidict *pp =
		(struct Sunidict *) malloc(sizeof(struct Sunidict));
	pp -> tag = unidict;
	pp -> Xgunidict_clas = PPgunidict_clas;
	pp -> Xgunidict_ty = PPgunidict_ty;
	return((ttype)pp);
}

unkId *Rgunidict_clas(t)
 struct Sunidict *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != unidict)
		fprintf(stderr,"gunidict_clas: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgunidict_clas);
}

ttype *Rgunidict_ty(t)
 struct Sunidict *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != unidict)
		fprintf(stderr,"gunidict_ty: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgunidict_ty);
}

/************** unityvartemplate ******************/

ttype mkunityvartemplate(PPgunityvartemplate)
 unkId PPgunityvartemplate;
{
	register struct Sunityvartemplate *pp =
		(struct Sunityvartemplate *) malloc(sizeof(struct Sunityvartemplate));
	pp -> tag = unityvartemplate;
	pp -> Xgunityvartemplate = PPgunityvartemplate;
	return((ttype)pp);
}

unkId *Rgunityvartemplate(t)
 struct Sunityvartemplate *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != unityvartemplate)
		fprintf(stderr,"gunityvartemplate: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgunityvartemplate);
}

/************** uniforall ******************/

ttype mkuniforall(PPguniforall_tv, PPguniforall_ty)
 list PPguniforall_tv;
 ttype PPguniforall_ty;
{
	register struct Suniforall *pp =
		(struct Suniforall *) malloc(sizeof(struct Suniforall));
	pp -> tag = uniforall;
	pp -> Xguniforall_tv = PPguniforall_tv;
	pp -> Xguniforall_ty = PPguniforall_ty;
	return((ttype)pp);
}

list *Rguniforall_tv(t)
 struct Suniforall *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != uniforall)
		fprintf(stderr,"guniforall_tv: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xguniforall_tv);
}

ttype *Rguniforall_ty(t)
 struct Suniforall *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != uniforall)
		fprintf(stderr,"guniforall_ty: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xguniforall_ty);
}

/************** ty_maybe_nothing ******************/

ttype mkty_maybe_nothing(void)
{
	register struct Sty_maybe_nothing *pp =
		(struct Sty_maybe_nothing *) malloc(sizeof(struct Sty_maybe_nothing));
	pp -> tag = ty_maybe_nothing;
	return((ttype)pp);
}

/************** ty_maybe_just ******************/

ttype mkty_maybe_just(PPgty_maybe)
 ttype PPgty_maybe;
{
	register struct Sty_maybe_just *pp =
		(struct Sty_maybe_just *) malloc(sizeof(struct Sty_maybe_just));
	pp -> tag = ty_maybe_just;
	pp -> Xgty_maybe = PPgty_maybe;
	return((ttype)pp);
}

ttype *Rgty_maybe(t)
 struct Sty_maybe_just *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ty_maybe_just)
		fprintf(stderr,"gty_maybe: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgty_maybe);
}
