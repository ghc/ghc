

#include "hspincl.h"
#include "yaccParser/atype.h"

Tatype tatype(t)
 atype t;
{
	return(t -> tag);
}


/************** atc ******************/

atype mkatc(PPgatcid, PPgatctypel, PPgatcline)
 unkId PPgatcid;
 list PPgatctypel;
 long PPgatcline;
{
	register struct Satc *pp =
		(struct Satc *) malloc(sizeof(struct Satc));
	pp -> tag = atc;
	pp -> Xgatcid = PPgatcid;
	pp -> Xgatctypel = PPgatctypel;
	pp -> Xgatcline = PPgatcline;
	return((atype)pp);
}

unkId *Rgatcid(t)
 struct Satc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != atc)
		fprintf(stderr,"gatcid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgatcid);
}

list *Rgatctypel(t)
 struct Satc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != atc)
		fprintf(stderr,"gatctypel: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgatctypel);
}

long *Rgatcline(t)
 struct Satc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != atc)
		fprintf(stderr,"gatcline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgatcline);
}
