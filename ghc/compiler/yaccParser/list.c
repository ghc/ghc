

#include "hspincl.h"
#include "yaccParser/list.h"

Tlist tlist(t)
 list t;
{
	return(t -> tag);
}


/************** lcons ******************/

list mklcons(PPlhd, PPltl)
 VOID_STAR PPlhd;
 list PPltl;
{
	register struct Slcons *pp =
		(struct Slcons *) malloc(sizeof(struct Slcons));
	pp -> tag = lcons;
	pp -> Xlhd = PPlhd;
	pp -> Xltl = PPltl;
	return((list)pp);
}

VOID_STAR *Rlhd(t)
 struct Slcons *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != lcons)
		fprintf(stderr,"lhd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xlhd);
}

list *Rltl(t)
 struct Slcons *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != lcons)
		fprintf(stderr,"ltl: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xltl);
}

/************** lnil ******************/

list mklnil(void)
{
	register struct Slnil *pp =
		(struct Slnil *) malloc(sizeof(struct Slnil));
	pp -> tag = lnil;
	return((list)pp);
}
