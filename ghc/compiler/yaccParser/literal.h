#ifndef literal_defined
#define literal_defined

#include <stdio.h>

#ifndef PROTO
#ifdef __STDC__
#define PROTO(x) x
#else
#define PROTO(x) /**/
#endif
#endif

typedef enum {
	integer,
	intprim,
	floatr,
	doubleprim,
	floatprim,
	charr,
	charprim,
	string,
	stringprim,
	clitlit,
	norepi,
	norepr,
	noreps
} Tliteral;

typedef struct { Tliteral tag; } *literal;

#ifdef __GNUC__
extern __inline__ Tliteral tliteral(literal t)
{
	return(t -> tag);
}
#else  /* ! __GNUC__ */
extern Tliteral tliteral PROTO((literal));
#endif /* ! __GNUC__ */

struct Sinteger {
	Tliteral tag;
	stringId Xginteger;
};

struct Sintprim {
	Tliteral tag;
	stringId Xgintprim;
};

struct Sfloatr {
	Tliteral tag;
	stringId Xgfloatr;
};

struct Sdoubleprim {
	Tliteral tag;
	stringId Xgdoubleprim;
};

struct Sfloatprim {
	Tliteral tag;
	stringId Xgfloatprim;
};

struct Scharr {
	Tliteral tag;
	hstring Xgchar;
};

struct Scharprim {
	Tliteral tag;
	hstring Xgcharprim;
};

struct Sstring {
	Tliteral tag;
	hstring Xgstring;
};

struct Sstringprim {
	Tliteral tag;
	hstring Xgstringprim;
};

struct Sclitlit {
	Tliteral tag;
	stringId Xgclitlit;
	stringId Xgclitlit_kind;
};

struct Snorepi {
	Tliteral tag;
	stringId Xgnorepi;
};

struct Snorepr {
	Tliteral tag;
	stringId Xgnorepr_n;
	stringId Xgnorepr_d;
};

struct Snoreps {
	Tliteral tag;
	hstring Xgnoreps;
};

extern literal mkinteger PROTO((stringId));
#ifdef __GNUC__

extern __inline__ stringId *Rginteger(struct Sinteger *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != integer)
		fprintf(stderr,"ginteger: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xginteger);
}
#else  /* ! __GNUC__ */
extern stringId *Rginteger PROTO((struct Sinteger *));
#endif /* ! __GNUC__ */

#define ginteger(xyzxyz) (*Rginteger((struct Sinteger *) (xyzxyz)))

extern literal mkintprim PROTO((stringId));
#ifdef __GNUC__

extern __inline__ stringId *Rgintprim(struct Sintprim *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != intprim)
		fprintf(stderr,"gintprim: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgintprim);
}
#else  /* ! __GNUC__ */
extern stringId *Rgintprim PROTO((struct Sintprim *));
#endif /* ! __GNUC__ */

#define gintprim(xyzxyz) (*Rgintprim((struct Sintprim *) (xyzxyz)))

extern literal mkfloatr PROTO((stringId));
#ifdef __GNUC__

extern __inline__ stringId *Rgfloatr(struct Sfloatr *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != floatr)
		fprintf(stderr,"gfloatr: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgfloatr);
}
#else  /* ! __GNUC__ */
extern stringId *Rgfloatr PROTO((struct Sfloatr *));
#endif /* ! __GNUC__ */

#define gfloatr(xyzxyz) (*Rgfloatr((struct Sfloatr *) (xyzxyz)))

extern literal mkdoubleprim PROTO((stringId));
#ifdef __GNUC__

extern __inline__ stringId *Rgdoubleprim(struct Sdoubleprim *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != doubleprim)
		fprintf(stderr,"gdoubleprim: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdoubleprim);
}
#else  /* ! __GNUC__ */
extern stringId *Rgdoubleprim PROTO((struct Sdoubleprim *));
#endif /* ! __GNUC__ */

#define gdoubleprim(xyzxyz) (*Rgdoubleprim((struct Sdoubleprim *) (xyzxyz)))

extern literal mkfloatprim PROTO((stringId));
#ifdef __GNUC__

extern __inline__ stringId *Rgfloatprim(struct Sfloatprim *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != floatprim)
		fprintf(stderr,"gfloatprim: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgfloatprim);
}
#else  /* ! __GNUC__ */
extern stringId *Rgfloatprim PROTO((struct Sfloatprim *));
#endif /* ! __GNUC__ */

#define gfloatprim(xyzxyz) (*Rgfloatprim((struct Sfloatprim *) (xyzxyz)))

extern literal mkcharr PROTO((hstring));
#ifdef __GNUC__

extern __inline__ hstring *Rgchar(struct Scharr *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != charr)
		fprintf(stderr,"gchar: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgchar);
}
#else  /* ! __GNUC__ */
extern hstring *Rgchar PROTO((struct Scharr *));
#endif /* ! __GNUC__ */

#define gchar(xyzxyz) (*Rgchar((struct Scharr *) (xyzxyz)))

extern literal mkcharprim PROTO((hstring));
#ifdef __GNUC__

extern __inline__ hstring *Rgcharprim(struct Scharprim *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != charprim)
		fprintf(stderr,"gcharprim: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcharprim);
}
#else  /* ! __GNUC__ */
extern hstring *Rgcharprim PROTO((struct Scharprim *));
#endif /* ! __GNUC__ */

#define gcharprim(xyzxyz) (*Rgcharprim((struct Scharprim *) (xyzxyz)))

extern literal mkstring PROTO((hstring));
#ifdef __GNUC__

extern __inline__ hstring *Rgstring(struct Sstring *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != string)
		fprintf(stderr,"gstring: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgstring);
}
#else  /* ! __GNUC__ */
extern hstring *Rgstring PROTO((struct Sstring *));
#endif /* ! __GNUC__ */

#define gstring(xyzxyz) (*Rgstring((struct Sstring *) (xyzxyz)))

extern literal mkstringprim PROTO((hstring));
#ifdef __GNUC__

extern __inline__ hstring *Rgstringprim(struct Sstringprim *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != stringprim)
		fprintf(stderr,"gstringprim: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgstringprim);
}
#else  /* ! __GNUC__ */
extern hstring *Rgstringprim PROTO((struct Sstringprim *));
#endif /* ! __GNUC__ */

#define gstringprim(xyzxyz) (*Rgstringprim((struct Sstringprim *) (xyzxyz)))

extern literal mkclitlit PROTO((stringId, stringId));
#ifdef __GNUC__

extern __inline__ stringId *Rgclitlit(struct Sclitlit *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != clitlit)
		fprintf(stderr,"gclitlit: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgclitlit);
}
#else  /* ! __GNUC__ */
extern stringId *Rgclitlit PROTO((struct Sclitlit *));
#endif /* ! __GNUC__ */

#define gclitlit(xyzxyz) (*Rgclitlit((struct Sclitlit *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ stringId *Rgclitlit_kind(struct Sclitlit *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != clitlit)
		fprintf(stderr,"gclitlit_kind: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgclitlit_kind);
}
#else  /* ! __GNUC__ */
extern stringId *Rgclitlit_kind PROTO((struct Sclitlit *));
#endif /* ! __GNUC__ */

#define gclitlit_kind(xyzxyz) (*Rgclitlit_kind((struct Sclitlit *) (xyzxyz)))

extern literal mknorepi PROTO((stringId));
#ifdef __GNUC__

extern __inline__ stringId *Rgnorepi(struct Snorepi *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != norepi)
		fprintf(stderr,"gnorepi: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnorepi);
}
#else  /* ! __GNUC__ */
extern stringId *Rgnorepi PROTO((struct Snorepi *));
#endif /* ! __GNUC__ */

#define gnorepi(xyzxyz) (*Rgnorepi((struct Snorepi *) (xyzxyz)))

extern literal mknorepr PROTO((stringId, stringId));
#ifdef __GNUC__

extern __inline__ stringId *Rgnorepr_n(struct Snorepr *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != norepr)
		fprintf(stderr,"gnorepr_n: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnorepr_n);
}
#else  /* ! __GNUC__ */
extern stringId *Rgnorepr_n PROTO((struct Snorepr *));
#endif /* ! __GNUC__ */

#define gnorepr_n(xyzxyz) (*Rgnorepr_n((struct Snorepr *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ stringId *Rgnorepr_d(struct Snorepr *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != norepr)
		fprintf(stderr,"gnorepr_d: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnorepr_d);
}
#else  /* ! __GNUC__ */
extern stringId *Rgnorepr_d PROTO((struct Snorepr *));
#endif /* ! __GNUC__ */

#define gnorepr_d(xyzxyz) (*Rgnorepr_d((struct Snorepr *) (xyzxyz)))

extern literal mknoreps PROTO((hstring));
#ifdef __GNUC__

extern __inline__ hstring *Rgnoreps(struct Snoreps *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != noreps)
		fprintf(stderr,"gnoreps: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnoreps);
}
#else  /* ! __GNUC__ */
extern hstring *Rgnoreps PROTO((struct Snoreps *));
#endif /* ! __GNUC__ */

#define gnoreps(xyzxyz) (*Rgnoreps((struct Snoreps *) (xyzxyz)))

#endif
