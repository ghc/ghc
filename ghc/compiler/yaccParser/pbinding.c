

#include "hspincl.h"
#include "yaccParser/pbinding.h"

Tpbinding tpbinding(t)
 pbinding t;
{
	return(t -> tag);
}


/************** pgrhs ******************/

pbinding mkpgrhs(PPggpat, PPggdexprs, PPggbind, PPggfuncname, PPggline)
 tree PPggpat;
 list PPggdexprs;
 binding PPggbind;
 stringId PPggfuncname;
 long PPggline;
{
	register struct Spgrhs *pp =
		(struct Spgrhs *) malloc(sizeof(struct Spgrhs));
	pp -> tag = pgrhs;
	pp -> Xggpat = PPggpat;
	pp -> Xggdexprs = PPggdexprs;
	pp -> Xggbind = PPggbind;
	pp -> Xggfuncname = PPggfuncname;
	pp -> Xggline = PPggline;
	return((pbinding)pp);
}

tree *Rggpat(t)
 struct Spgrhs *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != pgrhs)
		fprintf(stderr,"ggpat: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xggpat);
}

list *Rggdexprs(t)
 struct Spgrhs *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != pgrhs)
		fprintf(stderr,"ggdexprs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xggdexprs);
}

binding *Rggbind(t)
 struct Spgrhs *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != pgrhs)
		fprintf(stderr,"ggbind: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xggbind);
}

stringId *Rggfuncname(t)
 struct Spgrhs *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != pgrhs)
		fprintf(stderr,"ggfuncname: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xggfuncname);
}

long *Rggline(t)
 struct Spgrhs *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != pgrhs)
		fprintf(stderr,"ggline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xggline);
}
