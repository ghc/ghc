

#include "hspincl.h"
#include "yaccParser/literal.h"

Tliteral tliteral(t)
 literal t;
{
	return(t -> tag);
}


/************** integer ******************/

literal mkinteger(PPginteger)
 stringId PPginteger;
{
	register struct Sinteger *pp =
		(struct Sinteger *) malloc(sizeof(struct Sinteger));
	pp -> tag = integer;
	pp -> Xginteger = PPginteger;
	return((literal)pp);
}

stringId *Rginteger(t)
 struct Sinteger *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != integer)
		fprintf(stderr,"ginteger: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xginteger);
}

/************** intprim ******************/

literal mkintprim(PPgintprim)
 stringId PPgintprim;
{
	register struct Sintprim *pp =
		(struct Sintprim *) malloc(sizeof(struct Sintprim));
	pp -> tag = intprim;
	pp -> Xgintprim = PPgintprim;
	return((literal)pp);
}

stringId *Rgintprim(t)
 struct Sintprim *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != intprim)
		fprintf(stderr,"gintprim: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgintprim);
}

/************** floatr ******************/

literal mkfloatr(PPgfloatr)
 stringId PPgfloatr;
{
	register struct Sfloatr *pp =
		(struct Sfloatr *) malloc(sizeof(struct Sfloatr));
	pp -> tag = floatr;
	pp -> Xgfloatr = PPgfloatr;
	return((literal)pp);
}

stringId *Rgfloatr(t)
 struct Sfloatr *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != floatr)
		fprintf(stderr,"gfloatr: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgfloatr);
}

/************** doubleprim ******************/

literal mkdoubleprim(PPgdoubleprim)
 stringId PPgdoubleprim;
{
	register struct Sdoubleprim *pp =
		(struct Sdoubleprim *) malloc(sizeof(struct Sdoubleprim));
	pp -> tag = doubleprim;
	pp -> Xgdoubleprim = PPgdoubleprim;
	return((literal)pp);
}

stringId *Rgdoubleprim(t)
 struct Sdoubleprim *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != doubleprim)
		fprintf(stderr,"gdoubleprim: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdoubleprim);
}

/************** floatprim ******************/

literal mkfloatprim(PPgfloatprim)
 stringId PPgfloatprim;
{
	register struct Sfloatprim *pp =
		(struct Sfloatprim *) malloc(sizeof(struct Sfloatprim));
	pp -> tag = floatprim;
	pp -> Xgfloatprim = PPgfloatprim;
	return((literal)pp);
}

stringId *Rgfloatprim(t)
 struct Sfloatprim *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != floatprim)
		fprintf(stderr,"gfloatprim: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgfloatprim);
}

/************** charr ******************/

literal mkcharr(PPgchar)
 hstring PPgchar;
{
	register struct Scharr *pp =
		(struct Scharr *) malloc(sizeof(struct Scharr));
	pp -> tag = charr;
	pp -> Xgchar = PPgchar;
	return((literal)pp);
}

hstring *Rgchar(t)
 struct Scharr *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != charr)
		fprintf(stderr,"gchar: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgchar);
}

/************** charprim ******************/

literal mkcharprim(PPgcharprim)
 hstring PPgcharprim;
{
	register struct Scharprim *pp =
		(struct Scharprim *) malloc(sizeof(struct Scharprim));
	pp -> tag = charprim;
	pp -> Xgcharprim = PPgcharprim;
	return((literal)pp);
}

hstring *Rgcharprim(t)
 struct Scharprim *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != charprim)
		fprintf(stderr,"gcharprim: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcharprim);
}

/************** string ******************/

literal mkstring(PPgstring)
 hstring PPgstring;
{
	register struct Sstring *pp =
		(struct Sstring *) malloc(sizeof(struct Sstring));
	pp -> tag = string;
	pp -> Xgstring = PPgstring;
	return((literal)pp);
}

hstring *Rgstring(t)
 struct Sstring *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != string)
		fprintf(stderr,"gstring: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgstring);
}

/************** stringprim ******************/

literal mkstringprim(PPgstringprim)
 hstring PPgstringprim;
{
	register struct Sstringprim *pp =
		(struct Sstringprim *) malloc(sizeof(struct Sstringprim));
	pp -> tag = stringprim;
	pp -> Xgstringprim = PPgstringprim;
	return((literal)pp);
}

hstring *Rgstringprim(t)
 struct Sstringprim *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != stringprim)
		fprintf(stderr,"gstringprim: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgstringprim);
}

/************** clitlit ******************/

literal mkclitlit(PPgclitlit, PPgclitlit_kind)
 stringId PPgclitlit;
 stringId PPgclitlit_kind;
{
	register struct Sclitlit *pp =
		(struct Sclitlit *) malloc(sizeof(struct Sclitlit));
	pp -> tag = clitlit;
	pp -> Xgclitlit = PPgclitlit;
	pp -> Xgclitlit_kind = PPgclitlit_kind;
	return((literal)pp);
}

stringId *Rgclitlit(t)
 struct Sclitlit *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != clitlit)
		fprintf(stderr,"gclitlit: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgclitlit);
}

stringId *Rgclitlit_kind(t)
 struct Sclitlit *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != clitlit)
		fprintf(stderr,"gclitlit_kind: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgclitlit_kind);
}

/************** norepi ******************/

literal mknorepi(PPgnorepi)
 stringId PPgnorepi;
{
	register struct Snorepi *pp =
		(struct Snorepi *) malloc(sizeof(struct Snorepi));
	pp -> tag = norepi;
	pp -> Xgnorepi = PPgnorepi;
	return((literal)pp);
}

stringId *Rgnorepi(t)
 struct Snorepi *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != norepi)
		fprintf(stderr,"gnorepi: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnorepi);
}

/************** norepr ******************/

literal mknorepr(PPgnorepr_n, PPgnorepr_d)
 stringId PPgnorepr_n;
 stringId PPgnorepr_d;
{
	register struct Snorepr *pp =
		(struct Snorepr *) malloc(sizeof(struct Snorepr));
	pp -> tag = norepr;
	pp -> Xgnorepr_n = PPgnorepr_n;
	pp -> Xgnorepr_d = PPgnorepr_d;
	return((literal)pp);
}

stringId *Rgnorepr_n(t)
 struct Snorepr *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != norepr)
		fprintf(stderr,"gnorepr_n: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnorepr_n);
}

stringId *Rgnorepr_d(t)
 struct Snorepr *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != norepr)
		fprintf(stderr,"gnorepr_d: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnorepr_d);
}

/************** noreps ******************/

literal mknoreps(PPgnoreps)
 hstring PPgnoreps;
{
	register struct Snoreps *pp =
		(struct Snoreps *) malloc(sizeof(struct Snoreps));
	pp -> tag = noreps;
	pp -> Xgnoreps = PPgnoreps;
	return((literal)pp);
}

hstring *Rgnoreps(t)
 struct Snoreps *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != noreps)
		fprintf(stderr,"gnoreps: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnoreps);
}
