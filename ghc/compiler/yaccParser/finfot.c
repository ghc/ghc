

#include "hspincl.h"
#include "yaccParser/finfot.h"

Tfinfot tfinfot(t)
 finfot t;
{
	return(t -> tag);
}


/************** finfo ******************/

finfot mkfinfo(PPfi1, PPfi2)
 stringId PPfi1;
 stringId PPfi2;
{
	register struct Sfinfo *pp =
		(struct Sfinfo *) malloc(sizeof(struct Sfinfo));
	pp -> tag = finfo;
	pp -> Xfi1 = PPfi1;
	pp -> Xfi2 = PPfi2;
	return((finfot)pp);
}

stringId *Rfi1(t)
 struct Sfinfo *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != finfo)
		fprintf(stderr,"fi1: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xfi1);
}

stringId *Rfi2(t)
 struct Sfinfo *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != finfo)
		fprintf(stderr,"fi2: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xfi2);
}
