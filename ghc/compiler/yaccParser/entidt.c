

#include "hspincl.h"
#include "yaccParser/entidt.h"

Tentidt tentidt(t)
 entidt t;
{
	return(t -> tag);
}


/************** entid ******************/

entidt mkentid(PPgentid)
 stringId PPgentid;
{
	register struct Sentid *pp =
		(struct Sentid *) malloc(sizeof(struct Sentid));
	pp -> tag = entid;
	pp -> Xgentid = PPgentid;
	return((entidt)pp);
}

stringId *Rgentid(t)
 struct Sentid *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != entid)
		fprintf(stderr,"gentid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgentid);
}

/************** enttype ******************/

entidt mkenttype(PPgitentid)
 stringId PPgitentid;
{
	register struct Senttype *pp =
		(struct Senttype *) malloc(sizeof(struct Senttype));
	pp -> tag = enttype;
	pp -> Xgitentid = PPgitentid;
	return((entidt)pp);
}

stringId *Rgitentid(t)
 struct Senttype *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != enttype)
		fprintf(stderr,"gitentid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgitentid);
}

/************** enttypeall ******************/

entidt mkenttypeall(PPgatentid)
 stringId PPgatentid;
{
	register struct Senttypeall *pp =
		(struct Senttypeall *) malloc(sizeof(struct Senttypeall));
	pp -> tag = enttypeall;
	pp -> Xgatentid = PPgatentid;
	return((entidt)pp);
}

stringId *Rgatentid(t)
 struct Senttypeall *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != enttypeall)
		fprintf(stderr,"gatentid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgatentid);
}

/************** enttypecons ******************/

entidt mkenttypecons(PPgctentid, PPgctentcons)
 stringId PPgctentid;
 list PPgctentcons;
{
	register struct Senttypecons *pp =
		(struct Senttypecons *) malloc(sizeof(struct Senttypecons));
	pp -> tag = enttypecons;
	pp -> Xgctentid = PPgctentid;
	pp -> Xgctentcons = PPgctentcons;
	return((entidt)pp);
}

stringId *Rgctentid(t)
 struct Senttypecons *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != enttypecons)
		fprintf(stderr,"gctentid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgctentid);
}

list *Rgctentcons(t)
 struct Senttypecons *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != enttypecons)
		fprintf(stderr,"gctentcons: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgctentcons);
}

/************** entclass ******************/

entidt mkentclass(PPgcentid, PPgcentops)
 stringId PPgcentid;
 list PPgcentops;
{
	register struct Sentclass *pp =
		(struct Sentclass *) malloc(sizeof(struct Sentclass));
	pp -> tag = entclass;
	pp -> Xgcentid = PPgcentid;
	pp -> Xgcentops = PPgcentops;
	return((entidt)pp);
}

stringId *Rgcentid(t)
 struct Sentclass *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != entclass)
		fprintf(stderr,"gcentid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcentid);
}

list *Rgcentops(t)
 struct Sentclass *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != entclass)
		fprintf(stderr,"gcentops: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcentops);
}

/************** entmod ******************/

entidt mkentmod(PPgmentid)
 stringId PPgmentid;
{
	register struct Sentmod *pp =
		(struct Sentmod *) malloc(sizeof(struct Sentmod));
	pp -> tag = entmod;
	pp -> Xgmentid = PPgmentid;
	return((entidt)pp);
}

stringId *Rgmentid(t)
 struct Sentmod *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != entmod)
		fprintf(stderr,"gmentid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgmentid);
}
