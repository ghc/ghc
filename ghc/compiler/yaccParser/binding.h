#ifndef binding_defined
#define binding_defined

#include <stdio.h>

#ifndef PROTO
#ifdef __STDC__
#define PROTO(x) x
#else
#define PROTO(x) /**/
#endif
#endif

typedef enum {
	tbind,
	nbind,
	pbind,
	fbind,
	abind,
	ibind,
	dbind,
	cbind,
	sbind,
	mbind,
	nullbind,
	import,
	hiding,
	vspec_uprag,
	vspec_ty_and_id,
	ispec_uprag,
	inline_uprag,
	deforest_uprag,
	magicuf_uprag,
	abstract_uprag,
	dspec_uprag
} Tbinding;

typedef struct { Tbinding tag; } *binding;

#ifdef __GNUC__
Tbinding tbinding(binding t);
extern __inline__ Tbinding tbinding(binding t)
{
	return(t -> tag);
}
#else  /* ! __GNUC__ */
extern Tbinding tbinding PROTO((binding));
#endif /* ! __GNUC__ */

struct Stbind {
	Tbinding tag;
	list Xgtbindc;
	ttype Xgtbindid;
	list Xgtbindl;
	list Xgtbindd;
	long Xgtline;
	hpragma Xgtpragma;
};

struct Snbind {
	Tbinding tag;
	ttype Xgnbindid;
	ttype Xgnbindas;
	long Xgnline;
	hpragma Xgnpragma;
};

struct Spbind {
	Tbinding tag;
	list Xgpbindl;
	long Xgpline;
};

struct Sfbind {
	Tbinding tag;
	list Xgfbindl;
	long Xgfline;
};

struct Sabind {
	Tbinding tag;
	binding Xgabindfst;
	binding Xgabindsnd;
};

struct Sibind {
	Tbinding tag;
	list Xgibindc;
	unkId Xgibindid;
	ttype Xgibindi;
	binding Xgibindw;
	long Xgiline;
	hpragma Xgipragma;
};

struct Sdbind {
	Tbinding tag;
	list Xgdbindts;
	long Xgdline;
};

struct Scbind {
	Tbinding tag;
	list Xgcbindc;
	ttype Xgcbindid;
	binding Xgcbindw;
	long Xgcline;
	hpragma Xgcpragma;
};

struct Ssbind {
	Tbinding tag;
	list Xgsbindids;
	ttype Xgsbindid;
	long Xgsline;
	hpragma Xgspragma;
};

struct Smbind {
	Tbinding tag;
	stringId Xgmbindmodn;
	list Xgmbindimp;
	list Xgmbindren;
	long Xgmline;
};

struct Snullbind {
	Tbinding tag;
};

struct Simport {
	Tbinding tag;
	stringId Xgiebindmod;
	list Xgiebindexp;
	list Xgiebindren;
	binding Xgiebinddef;
	stringId Xgiebindfile;
	long Xgiebindline;
};

struct Shiding {
	Tbinding tag;
	stringId Xgihbindmod;
	list Xgihbindexp;
	list Xgihbindren;
	binding Xgihbinddef;
	stringId Xgihbindfile;
	long Xgihbindline;
};

struct Svspec_uprag {
	Tbinding tag;
	unkId Xgvspec_id;
	list Xgvspec_tys;
	long Xgvspec_line;
};

struct Svspec_ty_and_id {
	Tbinding tag;
	ttype Xgvspec_ty;
	list Xgvspec_tyid;
};

struct Sispec_uprag {
	Tbinding tag;
	unkId Xgispec_clas;
	ttype Xgispec_ty;
	long Xgispec_line;
};

struct Sinline_uprag {
	Tbinding tag;
	unkId Xginline_id;
	list Xginline_howto;
	long Xginline_line;
};

struct Sdeforest_uprag {
	Tbinding tag;
	unkId Xgdeforest_id;
	long Xgdeforest_line;
};

struct Smagicuf_uprag {
	Tbinding tag;
	unkId Xgmagicuf_id;
	stringId Xgmagicuf_str;
	long Xgmagicuf_line;
};

struct Sabstract_uprag {
	Tbinding tag;
	unkId Xgabstract_id;
	long Xgabstract_line;
};

struct Sdspec_uprag {
	Tbinding tag;
	unkId Xgdspec_id;
	list Xgdspec_tys;
	long Xgdspec_line;
};

extern binding mktbind PROTO((list, ttype, list, list, long, hpragma));
#ifdef __GNUC__

list *Rgtbindc PROTO((struct Stbind *));

extern __inline__ list *Rgtbindc(struct Stbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != tbind)
		fprintf(stderr,"gtbindc: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtbindc);
}
#else  /* ! __GNUC__ */
extern list *Rgtbindc PROTO((struct Stbind *));
#endif /* ! __GNUC__ */

#define gtbindc(xyzxyz) (*Rgtbindc((struct Stbind *) (xyzxyz)))
#ifdef __GNUC__

ttype *Rgtbindid PROTO((struct Stbind *));

extern __inline__ ttype *Rgtbindid(struct Stbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != tbind)
		fprintf(stderr,"gtbindid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtbindid);
}
#else  /* ! __GNUC__ */
extern ttype *Rgtbindid PROTO((struct Stbind *));
#endif /* ! __GNUC__ */

#define gtbindid(xyzxyz) (*Rgtbindid((struct Stbind *) (xyzxyz)))
#ifdef __GNUC__

list *Rgtbindl PROTO((struct Stbind *));

extern __inline__ list *Rgtbindl(struct Stbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != tbind)
		fprintf(stderr,"gtbindl: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtbindl);
}
#else  /* ! __GNUC__ */
extern list *Rgtbindl PROTO((struct Stbind *));
#endif /* ! __GNUC__ */

#define gtbindl(xyzxyz) (*Rgtbindl((struct Stbind *) (xyzxyz)))
#ifdef __GNUC__

list *Rgtbindd PROTO((struct Stbind *));

extern __inline__ list *Rgtbindd(struct Stbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != tbind)
		fprintf(stderr,"gtbindd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtbindd);
}
#else  /* ! __GNUC__ */
extern list *Rgtbindd PROTO((struct Stbind *));
#endif /* ! __GNUC__ */

#define gtbindd(xyzxyz) (*Rgtbindd((struct Stbind *) (xyzxyz)))
#ifdef __GNUC__

long *Rgtline PROTO((struct Stbind *));

extern __inline__ long *Rgtline(struct Stbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != tbind)
		fprintf(stderr,"gtline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtline);
}
#else  /* ! __GNUC__ */
extern long *Rgtline PROTO((struct Stbind *));
#endif /* ! __GNUC__ */

#define gtline(xyzxyz) (*Rgtline((struct Stbind *) (xyzxyz)))
#ifdef __GNUC__

hpragma *Rgtpragma PROTO((struct Stbind *));

extern __inline__ hpragma *Rgtpragma(struct Stbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != tbind)
		fprintf(stderr,"gtpragma: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtpragma);
}
#else  /* ! __GNUC__ */
extern hpragma *Rgtpragma PROTO((struct Stbind *));
#endif /* ! __GNUC__ */

#define gtpragma(xyzxyz) (*Rgtpragma((struct Stbind *) (xyzxyz)))

extern binding mknbind PROTO((ttype, ttype, long, hpragma));
#ifdef __GNUC__

ttype *Rgnbindid PROTO((struct Snbind *));

extern __inline__ ttype *Rgnbindid(struct Snbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != nbind)
		fprintf(stderr,"gnbindid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnbindid);
}
#else  /* ! __GNUC__ */
extern ttype *Rgnbindid PROTO((struct Snbind *));
#endif /* ! __GNUC__ */

#define gnbindid(xyzxyz) (*Rgnbindid((struct Snbind *) (xyzxyz)))
#ifdef __GNUC__

ttype *Rgnbindas PROTO((struct Snbind *));

extern __inline__ ttype *Rgnbindas(struct Snbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != nbind)
		fprintf(stderr,"gnbindas: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnbindas);
}
#else  /* ! __GNUC__ */
extern ttype *Rgnbindas PROTO((struct Snbind *));
#endif /* ! __GNUC__ */

#define gnbindas(xyzxyz) (*Rgnbindas((struct Snbind *) (xyzxyz)))
#ifdef __GNUC__

long *Rgnline PROTO((struct Snbind *));

extern __inline__ long *Rgnline(struct Snbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != nbind)
		fprintf(stderr,"gnline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnline);
}
#else  /* ! __GNUC__ */
extern long *Rgnline PROTO((struct Snbind *));
#endif /* ! __GNUC__ */

#define gnline(xyzxyz) (*Rgnline((struct Snbind *) (xyzxyz)))
#ifdef __GNUC__

hpragma *Rgnpragma PROTO((struct Snbind *));

extern __inline__ hpragma *Rgnpragma(struct Snbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != nbind)
		fprintf(stderr,"gnpragma: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnpragma);
}
#else  /* ! __GNUC__ */
extern hpragma *Rgnpragma PROTO((struct Snbind *));
#endif /* ! __GNUC__ */

#define gnpragma(xyzxyz) (*Rgnpragma((struct Snbind *) (xyzxyz)))

extern binding mkpbind PROTO((list, long));
#ifdef __GNUC__

list *Rgpbindl PROTO((struct Spbind *));

extern __inline__ list *Rgpbindl(struct Spbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != pbind)
		fprintf(stderr,"gpbindl: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgpbindl);
}
#else  /* ! __GNUC__ */
extern list *Rgpbindl PROTO((struct Spbind *));
#endif /* ! __GNUC__ */

#define gpbindl(xyzxyz) (*Rgpbindl((struct Spbind *) (xyzxyz)))
#ifdef __GNUC__

long *Rgpline PROTO((struct Spbind *));

extern __inline__ long *Rgpline(struct Spbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != pbind)
		fprintf(stderr,"gpline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgpline);
}
#else  /* ! __GNUC__ */
extern long *Rgpline PROTO((struct Spbind *));
#endif /* ! __GNUC__ */

#define gpline(xyzxyz) (*Rgpline((struct Spbind *) (xyzxyz)))

extern binding mkfbind PROTO((list, long));
#ifdef __GNUC__

list *Rgfbindl PROTO((struct Sfbind *));

extern __inline__ list *Rgfbindl(struct Sfbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != fbind)
		fprintf(stderr,"gfbindl: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgfbindl);
}
#else  /* ! __GNUC__ */
extern list *Rgfbindl PROTO((struct Sfbind *));
#endif /* ! __GNUC__ */

#define gfbindl(xyzxyz) (*Rgfbindl((struct Sfbind *) (xyzxyz)))
#ifdef __GNUC__

long *Rgfline PROTO((struct Sfbind *));

extern __inline__ long *Rgfline(struct Sfbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != fbind)
		fprintf(stderr,"gfline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgfline);
}
#else  /* ! __GNUC__ */
extern long *Rgfline PROTO((struct Sfbind *));
#endif /* ! __GNUC__ */

#define gfline(xyzxyz) (*Rgfline((struct Sfbind *) (xyzxyz)))

extern binding mkabind PROTO((binding, binding));
#ifdef __GNUC__

binding *Rgabindfst PROTO((struct Sabind *));

extern __inline__ binding *Rgabindfst(struct Sabind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != abind)
		fprintf(stderr,"gabindfst: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgabindfst);
}
#else  /* ! __GNUC__ */
extern binding *Rgabindfst PROTO((struct Sabind *));
#endif /* ! __GNUC__ */

#define gabindfst(xyzxyz) (*Rgabindfst((struct Sabind *) (xyzxyz)))
#ifdef __GNUC__

binding *Rgabindsnd PROTO((struct Sabind *));

extern __inline__ binding *Rgabindsnd(struct Sabind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != abind)
		fprintf(stderr,"gabindsnd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgabindsnd);
}
#else  /* ! __GNUC__ */
extern binding *Rgabindsnd PROTO((struct Sabind *));
#endif /* ! __GNUC__ */

#define gabindsnd(xyzxyz) (*Rgabindsnd((struct Sabind *) (xyzxyz)))

extern binding mkibind PROTO((list, unkId, ttype, binding, long, hpragma));
#ifdef __GNUC__

list *Rgibindc PROTO((struct Sibind *));

extern __inline__ list *Rgibindc(struct Sibind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ibind)
		fprintf(stderr,"gibindc: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgibindc);
}
#else  /* ! __GNUC__ */
extern list *Rgibindc PROTO((struct Sibind *));
#endif /* ! __GNUC__ */

#define gibindc(xyzxyz) (*Rgibindc((struct Sibind *) (xyzxyz)))
#ifdef __GNUC__

unkId *Rgibindid PROTO((struct Sibind *));

extern __inline__ unkId *Rgibindid(struct Sibind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ibind)
		fprintf(stderr,"gibindid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgibindid);
}
#else  /* ! __GNUC__ */
extern unkId *Rgibindid PROTO((struct Sibind *));
#endif /* ! __GNUC__ */

#define gibindid(xyzxyz) (*Rgibindid((struct Sibind *) (xyzxyz)))
#ifdef __GNUC__

ttype *Rgibindi PROTO((struct Sibind *));

extern __inline__ ttype *Rgibindi(struct Sibind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ibind)
		fprintf(stderr,"gibindi: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgibindi);
}
#else  /* ! __GNUC__ */
extern ttype *Rgibindi PROTO((struct Sibind *));
#endif /* ! __GNUC__ */

#define gibindi(xyzxyz) (*Rgibindi((struct Sibind *) (xyzxyz)))
#ifdef __GNUC__

binding *Rgibindw PROTO((struct Sibind *));

extern __inline__ binding *Rgibindw(struct Sibind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ibind)
		fprintf(stderr,"gibindw: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgibindw);
}
#else  /* ! __GNUC__ */
extern binding *Rgibindw PROTO((struct Sibind *));
#endif /* ! __GNUC__ */

#define gibindw(xyzxyz) (*Rgibindw((struct Sibind *) (xyzxyz)))
#ifdef __GNUC__

long *Rgiline PROTO((struct Sibind *));

extern __inline__ long *Rgiline(struct Sibind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ibind)
		fprintf(stderr,"giline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgiline);
}
#else  /* ! __GNUC__ */
extern long *Rgiline PROTO((struct Sibind *));
#endif /* ! __GNUC__ */

#define giline(xyzxyz) (*Rgiline((struct Sibind *) (xyzxyz)))
#ifdef __GNUC__

hpragma *Rgipragma PROTO((struct Sibind *));

extern __inline__ hpragma *Rgipragma(struct Sibind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ibind)
		fprintf(stderr,"gipragma: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgipragma);
}
#else  /* ! __GNUC__ */
extern hpragma *Rgipragma PROTO((struct Sibind *));
#endif /* ! __GNUC__ */

#define gipragma(xyzxyz) (*Rgipragma((struct Sibind *) (xyzxyz)))

extern binding mkdbind PROTO((list, long));
#ifdef __GNUC__

list *Rgdbindts PROTO((struct Sdbind *));

extern __inline__ list *Rgdbindts(struct Sdbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != dbind)
		fprintf(stderr,"gdbindts: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdbindts);
}
#else  /* ! __GNUC__ */
extern list *Rgdbindts PROTO((struct Sdbind *));
#endif /* ! __GNUC__ */

#define gdbindts(xyzxyz) (*Rgdbindts((struct Sdbind *) (xyzxyz)))
#ifdef __GNUC__

long *Rgdline PROTO((struct Sdbind *));

extern __inline__ long *Rgdline(struct Sdbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != dbind)
		fprintf(stderr,"gdline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdline);
}
#else  /* ! __GNUC__ */
extern long *Rgdline PROTO((struct Sdbind *));
#endif /* ! __GNUC__ */

#define gdline(xyzxyz) (*Rgdline((struct Sdbind *) (xyzxyz)))

extern binding mkcbind PROTO((list, ttype, binding, long, hpragma));
#ifdef __GNUC__

list *Rgcbindc PROTO((struct Scbind *));

extern __inline__ list *Rgcbindc(struct Scbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != cbind)
		fprintf(stderr,"gcbindc: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcbindc);
}
#else  /* ! __GNUC__ */
extern list *Rgcbindc PROTO((struct Scbind *));
#endif /* ! __GNUC__ */

#define gcbindc(xyzxyz) (*Rgcbindc((struct Scbind *) (xyzxyz)))
#ifdef __GNUC__

ttype *Rgcbindid PROTO((struct Scbind *));

extern __inline__ ttype *Rgcbindid(struct Scbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != cbind)
		fprintf(stderr,"gcbindid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcbindid);
}
#else  /* ! __GNUC__ */
extern ttype *Rgcbindid PROTO((struct Scbind *));
#endif /* ! __GNUC__ */

#define gcbindid(xyzxyz) (*Rgcbindid((struct Scbind *) (xyzxyz)))
#ifdef __GNUC__

binding *Rgcbindw PROTO((struct Scbind *));

extern __inline__ binding *Rgcbindw(struct Scbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != cbind)
		fprintf(stderr,"gcbindw: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcbindw);
}
#else  /* ! __GNUC__ */
extern binding *Rgcbindw PROTO((struct Scbind *));
#endif /* ! __GNUC__ */

#define gcbindw(xyzxyz) (*Rgcbindw((struct Scbind *) (xyzxyz)))
#ifdef __GNUC__

long *Rgcline PROTO((struct Scbind *));

extern __inline__ long *Rgcline(struct Scbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != cbind)
		fprintf(stderr,"gcline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcline);
}
#else  /* ! __GNUC__ */
extern long *Rgcline PROTO((struct Scbind *));
#endif /* ! __GNUC__ */

#define gcline(xyzxyz) (*Rgcline((struct Scbind *) (xyzxyz)))
#ifdef __GNUC__

hpragma *Rgcpragma PROTO((struct Scbind *));

extern __inline__ hpragma *Rgcpragma(struct Scbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != cbind)
		fprintf(stderr,"gcpragma: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcpragma);
}
#else  /* ! __GNUC__ */
extern hpragma *Rgcpragma PROTO((struct Scbind *));
#endif /* ! __GNUC__ */

#define gcpragma(xyzxyz) (*Rgcpragma((struct Scbind *) (xyzxyz)))

extern binding mksbind PROTO((list, ttype, long, hpragma));
#ifdef __GNUC__

list *Rgsbindids PROTO((struct Ssbind *));

extern __inline__ list *Rgsbindids(struct Ssbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != sbind)
		fprintf(stderr,"gsbindids: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgsbindids);
}
#else  /* ! __GNUC__ */
extern list *Rgsbindids PROTO((struct Ssbind *));
#endif /* ! __GNUC__ */

#define gsbindids(xyzxyz) (*Rgsbindids((struct Ssbind *) (xyzxyz)))
#ifdef __GNUC__

ttype *Rgsbindid PROTO((struct Ssbind *));

extern __inline__ ttype *Rgsbindid(struct Ssbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != sbind)
		fprintf(stderr,"gsbindid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgsbindid);
}
#else  /* ! __GNUC__ */
extern ttype *Rgsbindid PROTO((struct Ssbind *));
#endif /* ! __GNUC__ */

#define gsbindid(xyzxyz) (*Rgsbindid((struct Ssbind *) (xyzxyz)))
#ifdef __GNUC__

long *Rgsline PROTO((struct Ssbind *));

extern __inline__ long *Rgsline(struct Ssbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != sbind)
		fprintf(stderr,"gsline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgsline);
}
#else  /* ! __GNUC__ */
extern long *Rgsline PROTO((struct Ssbind *));
#endif /* ! __GNUC__ */

#define gsline(xyzxyz) (*Rgsline((struct Ssbind *) (xyzxyz)))
#ifdef __GNUC__

hpragma *Rgspragma PROTO((struct Ssbind *));

extern __inline__ hpragma *Rgspragma(struct Ssbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != sbind)
		fprintf(stderr,"gspragma: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgspragma);
}
#else  /* ! __GNUC__ */
extern hpragma *Rgspragma PROTO((struct Ssbind *));
#endif /* ! __GNUC__ */

#define gspragma(xyzxyz) (*Rgspragma((struct Ssbind *) (xyzxyz)))

extern binding mkmbind PROTO((stringId, list, list, long));
#ifdef __GNUC__

stringId *Rgmbindmodn PROTO((struct Smbind *));

extern __inline__ stringId *Rgmbindmodn(struct Smbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != mbind)
		fprintf(stderr,"gmbindmodn: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgmbindmodn);
}
#else  /* ! __GNUC__ */
extern stringId *Rgmbindmodn PROTO((struct Smbind *));
#endif /* ! __GNUC__ */

#define gmbindmodn(xyzxyz) (*Rgmbindmodn((struct Smbind *) (xyzxyz)))
#ifdef __GNUC__

list *Rgmbindimp PROTO((struct Smbind *));

extern __inline__ list *Rgmbindimp(struct Smbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != mbind)
		fprintf(stderr,"gmbindimp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgmbindimp);
}
#else  /* ! __GNUC__ */
extern list *Rgmbindimp PROTO((struct Smbind *));
#endif /* ! __GNUC__ */

#define gmbindimp(xyzxyz) (*Rgmbindimp((struct Smbind *) (xyzxyz)))
#ifdef __GNUC__

list *Rgmbindren PROTO((struct Smbind *));

extern __inline__ list *Rgmbindren(struct Smbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != mbind)
		fprintf(stderr,"gmbindren: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgmbindren);
}
#else  /* ! __GNUC__ */
extern list *Rgmbindren PROTO((struct Smbind *));
#endif /* ! __GNUC__ */

#define gmbindren(xyzxyz) (*Rgmbindren((struct Smbind *) (xyzxyz)))
#ifdef __GNUC__

long *Rgmline PROTO((struct Smbind *));

extern __inline__ long *Rgmline(struct Smbind *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != mbind)
		fprintf(stderr,"gmline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgmline);
}
#else  /* ! __GNUC__ */
extern long *Rgmline PROTO((struct Smbind *));
#endif /* ! __GNUC__ */

#define gmline(xyzxyz) (*Rgmline((struct Smbind *) (xyzxyz)))

extern binding mknullbind PROTO((void));

extern binding mkimport PROTO((stringId, list, list, binding, stringId, long));
#ifdef __GNUC__

stringId *Rgiebindmod PROTO((struct Simport *));

extern __inline__ stringId *Rgiebindmod(struct Simport *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != import)
		fprintf(stderr,"giebindmod: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgiebindmod);
}
#else  /* ! __GNUC__ */
extern stringId *Rgiebindmod PROTO((struct Simport *));
#endif /* ! __GNUC__ */

#define giebindmod(xyzxyz) (*Rgiebindmod((struct Simport *) (xyzxyz)))
#ifdef __GNUC__

list *Rgiebindexp PROTO((struct Simport *));

extern __inline__ list *Rgiebindexp(struct Simport *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != import)
		fprintf(stderr,"giebindexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgiebindexp);
}
#else  /* ! __GNUC__ */
extern list *Rgiebindexp PROTO((struct Simport *));
#endif /* ! __GNUC__ */

#define giebindexp(xyzxyz) (*Rgiebindexp((struct Simport *) (xyzxyz)))
#ifdef __GNUC__

list *Rgiebindren PROTO((struct Simport *));

extern __inline__ list *Rgiebindren(struct Simport *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != import)
		fprintf(stderr,"giebindren: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgiebindren);
}
#else  /* ! __GNUC__ */
extern list *Rgiebindren PROTO((struct Simport *));
#endif /* ! __GNUC__ */

#define giebindren(xyzxyz) (*Rgiebindren((struct Simport *) (xyzxyz)))
#ifdef __GNUC__

binding *Rgiebinddef PROTO((struct Simport *));

extern __inline__ binding *Rgiebinddef(struct Simport *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != import)
		fprintf(stderr,"giebinddef: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgiebinddef);
}
#else  /* ! __GNUC__ */
extern binding *Rgiebinddef PROTO((struct Simport *));
#endif /* ! __GNUC__ */

#define giebinddef(xyzxyz) (*Rgiebinddef((struct Simport *) (xyzxyz)))
#ifdef __GNUC__

stringId *Rgiebindfile PROTO((struct Simport *));

extern __inline__ stringId *Rgiebindfile(struct Simport *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != import)
		fprintf(stderr,"giebindfile: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgiebindfile);
}
#else  /* ! __GNUC__ */
extern stringId *Rgiebindfile PROTO((struct Simport *));
#endif /* ! __GNUC__ */

#define giebindfile(xyzxyz) (*Rgiebindfile((struct Simport *) (xyzxyz)))
#ifdef __GNUC__

long *Rgiebindline PROTO((struct Simport *));

extern __inline__ long *Rgiebindline(struct Simport *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != import)
		fprintf(stderr,"giebindline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgiebindline);
}
#else  /* ! __GNUC__ */
extern long *Rgiebindline PROTO((struct Simport *));
#endif /* ! __GNUC__ */

#define giebindline(xyzxyz) (*Rgiebindline((struct Simport *) (xyzxyz)))

extern binding mkhiding PROTO((stringId, list, list, binding, stringId, long));
#ifdef __GNUC__

stringId *Rgihbindmod PROTO((struct Shiding *));

extern __inline__ stringId *Rgihbindmod(struct Shiding *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != hiding)
		fprintf(stderr,"gihbindmod: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgihbindmod);
}
#else  /* ! __GNUC__ */
extern stringId *Rgihbindmod PROTO((struct Shiding *));
#endif /* ! __GNUC__ */

#define gihbindmod(xyzxyz) (*Rgihbindmod((struct Shiding *) (xyzxyz)))
#ifdef __GNUC__

list *Rgihbindexp PROTO((struct Shiding *));

extern __inline__ list *Rgihbindexp(struct Shiding *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != hiding)
		fprintf(stderr,"gihbindexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgihbindexp);
}
#else  /* ! __GNUC__ */
extern list *Rgihbindexp PROTO((struct Shiding *));
#endif /* ! __GNUC__ */

#define gihbindexp(xyzxyz) (*Rgihbindexp((struct Shiding *) (xyzxyz)))
#ifdef __GNUC__

list *Rgihbindren PROTO((struct Shiding *));

extern __inline__ list *Rgihbindren(struct Shiding *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != hiding)
		fprintf(stderr,"gihbindren: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgihbindren);
}
#else  /* ! __GNUC__ */
extern list *Rgihbindren PROTO((struct Shiding *));
#endif /* ! __GNUC__ */

#define gihbindren(xyzxyz) (*Rgihbindren((struct Shiding *) (xyzxyz)))
#ifdef __GNUC__

binding *Rgihbinddef PROTO((struct Shiding *));

extern __inline__ binding *Rgihbinddef(struct Shiding *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != hiding)
		fprintf(stderr,"gihbinddef: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgihbinddef);
}
#else  /* ! __GNUC__ */
extern binding *Rgihbinddef PROTO((struct Shiding *));
#endif /* ! __GNUC__ */

#define gihbinddef(xyzxyz) (*Rgihbinddef((struct Shiding *) (xyzxyz)))
#ifdef __GNUC__

stringId *Rgihbindfile PROTO((struct Shiding *));

extern __inline__ stringId *Rgihbindfile(struct Shiding *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != hiding)
		fprintf(stderr,"gihbindfile: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgihbindfile);
}
#else  /* ! __GNUC__ */
extern stringId *Rgihbindfile PROTO((struct Shiding *));
#endif /* ! __GNUC__ */

#define gihbindfile(xyzxyz) (*Rgihbindfile((struct Shiding *) (xyzxyz)))
#ifdef __GNUC__

long *Rgihbindline PROTO((struct Shiding *));

extern __inline__ long *Rgihbindline(struct Shiding *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != hiding)
		fprintf(stderr,"gihbindline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgihbindline);
}
#else  /* ! __GNUC__ */
extern long *Rgihbindline PROTO((struct Shiding *));
#endif /* ! __GNUC__ */

#define gihbindline(xyzxyz) (*Rgihbindline((struct Shiding *) (xyzxyz)))

extern binding mkvspec_uprag PROTO((unkId, list, long));
#ifdef __GNUC__

unkId *Rgvspec_id PROTO((struct Svspec_uprag *));

extern __inline__ unkId *Rgvspec_id(struct Svspec_uprag *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != vspec_uprag)
		fprintf(stderr,"gvspec_id: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgvspec_id);
}
#else  /* ! __GNUC__ */
extern unkId *Rgvspec_id PROTO((struct Svspec_uprag *));
#endif /* ! __GNUC__ */

#define gvspec_id(xyzxyz) (*Rgvspec_id((struct Svspec_uprag *) (xyzxyz)))
#ifdef __GNUC__

list *Rgvspec_tys PROTO((struct Svspec_uprag *));

extern __inline__ list *Rgvspec_tys(struct Svspec_uprag *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != vspec_uprag)
		fprintf(stderr,"gvspec_tys: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgvspec_tys);
}
#else  /* ! __GNUC__ */
extern list *Rgvspec_tys PROTO((struct Svspec_uprag *));
#endif /* ! __GNUC__ */

#define gvspec_tys(xyzxyz) (*Rgvspec_tys((struct Svspec_uprag *) (xyzxyz)))
#ifdef __GNUC__

long *Rgvspec_line PROTO((struct Svspec_uprag *));

extern __inline__ long *Rgvspec_line(struct Svspec_uprag *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != vspec_uprag)
		fprintf(stderr,"gvspec_line: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgvspec_line);
}
#else  /* ! __GNUC__ */
extern long *Rgvspec_line PROTO((struct Svspec_uprag *));
#endif /* ! __GNUC__ */

#define gvspec_line(xyzxyz) (*Rgvspec_line((struct Svspec_uprag *) (xyzxyz)))

extern binding mkvspec_ty_and_id PROTO((ttype, list));
#ifdef __GNUC__

ttype *Rgvspec_ty PROTO((struct Svspec_ty_and_id *));

extern __inline__ ttype *Rgvspec_ty(struct Svspec_ty_and_id *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != vspec_ty_and_id)
		fprintf(stderr,"gvspec_ty: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgvspec_ty);
}
#else  /* ! __GNUC__ */
extern ttype *Rgvspec_ty PROTO((struct Svspec_ty_and_id *));
#endif /* ! __GNUC__ */

#define gvspec_ty(xyzxyz) (*Rgvspec_ty((struct Svspec_ty_and_id *) (xyzxyz)))
#ifdef __GNUC__

list *Rgvspec_tyid PROTO((struct Svspec_ty_and_id *));

extern __inline__ list *Rgvspec_tyid(struct Svspec_ty_and_id *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != vspec_ty_and_id)
		fprintf(stderr,"gvspec_tyid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgvspec_tyid);
}
#else  /* ! __GNUC__ */
extern list *Rgvspec_tyid PROTO((struct Svspec_ty_and_id *));
#endif /* ! __GNUC__ */

#define gvspec_tyid(xyzxyz) (*Rgvspec_tyid((struct Svspec_ty_and_id *) (xyzxyz)))

extern binding mkispec_uprag PROTO((unkId, ttype, long));
#ifdef __GNUC__

unkId *Rgispec_clas PROTO((struct Sispec_uprag *));

extern __inline__ unkId *Rgispec_clas(struct Sispec_uprag *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ispec_uprag)
		fprintf(stderr,"gispec_clas: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgispec_clas);
}
#else  /* ! __GNUC__ */
extern unkId *Rgispec_clas PROTO((struct Sispec_uprag *));
#endif /* ! __GNUC__ */

#define gispec_clas(xyzxyz) (*Rgispec_clas((struct Sispec_uprag *) (xyzxyz)))
#ifdef __GNUC__

ttype *Rgispec_ty PROTO((struct Sispec_uprag *));

extern __inline__ ttype *Rgispec_ty(struct Sispec_uprag *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ispec_uprag)
		fprintf(stderr,"gispec_ty: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgispec_ty);
}
#else  /* ! __GNUC__ */
extern ttype *Rgispec_ty PROTO((struct Sispec_uprag *));
#endif /* ! __GNUC__ */

#define gispec_ty(xyzxyz) (*Rgispec_ty((struct Sispec_uprag *) (xyzxyz)))
#ifdef __GNUC__

long *Rgispec_line PROTO((struct Sispec_uprag *));

extern __inline__ long *Rgispec_line(struct Sispec_uprag *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ispec_uprag)
		fprintf(stderr,"gispec_line: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgispec_line);
}
#else  /* ! __GNUC__ */
extern long *Rgispec_line PROTO((struct Sispec_uprag *));
#endif /* ! __GNUC__ */

#define gispec_line(xyzxyz) (*Rgispec_line((struct Sispec_uprag *) (xyzxyz)))

extern binding mkinline_uprag PROTO((unkId, list, long));
#ifdef __GNUC__

unkId *Rginline_id PROTO((struct Sinline_uprag *));

extern __inline__ unkId *Rginline_id(struct Sinline_uprag *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != inline_uprag)
		fprintf(stderr,"ginline_id: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xginline_id);
}
#else  /* ! __GNUC__ */
extern unkId *Rginline_id PROTO((struct Sinline_uprag *));
#endif /* ! __GNUC__ */

#define ginline_id(xyzxyz) (*Rginline_id((struct Sinline_uprag *) (xyzxyz)))
#ifdef __GNUC__

list *Rginline_howto PROTO((struct Sinline_uprag *));

extern __inline__ list *Rginline_howto(struct Sinline_uprag *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != inline_uprag)
		fprintf(stderr,"ginline_howto: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xginline_howto);
}
#else  /* ! __GNUC__ */
extern list *Rginline_howto PROTO((struct Sinline_uprag *));
#endif /* ! __GNUC__ */

#define ginline_howto(xyzxyz) (*Rginline_howto((struct Sinline_uprag *) (xyzxyz)))
#ifdef __GNUC__

long *Rginline_line PROTO((struct Sinline_uprag *));

extern __inline__ long *Rginline_line(struct Sinline_uprag *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != inline_uprag)
		fprintf(stderr,"ginline_line: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xginline_line);
}
#else  /* ! __GNUC__ */
extern long *Rginline_line PROTO((struct Sinline_uprag *));
#endif /* ! __GNUC__ */

#define ginline_line(xyzxyz) (*Rginline_line((struct Sinline_uprag *) (xyzxyz)))

extern binding mkdeforest_uprag PROTO((unkId, long));
#ifdef __GNUC__

unkId *Rgdeforest_id PROTO((struct Sdeforest_uprag *));

extern __inline__ unkId *Rgdeforest_id(struct Sdeforest_uprag *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != deforest_uprag)
		fprintf(stderr,"gdeforest_id: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdeforest_id);
}
#else  /* ! __GNUC__ */
extern unkId *Rgdeforest_id PROTO((struct Sdeforest_uprag *));
#endif /* ! __GNUC__ */

#define gdeforest_id(xyzxyz) (*Rgdeforest_id((struct Sdeforest_uprag *) (xyzxyz)))
#ifdef __GNUC__

long *Rgdeforest_line PROTO((struct Sdeforest_uprag *));

extern __inline__ long *Rgdeforest_line(struct Sdeforest_uprag *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != deforest_uprag)
		fprintf(stderr,"gdeforest_line: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdeforest_line);
}
#else  /* ! __GNUC__ */
extern long *Rgdeforest_line PROTO((struct Sdeforest_uprag *));
#endif /* ! __GNUC__ */

#define gdeforest_line(xyzxyz) (*Rgdeforest_line((struct Sdeforest_uprag *) (xyzxyz)))

extern binding mkmagicuf_uprag PROTO((unkId, stringId, long));
#ifdef __GNUC__

unkId *Rgmagicuf_id PROTO((struct Smagicuf_uprag *));

extern __inline__ unkId *Rgmagicuf_id(struct Smagicuf_uprag *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != magicuf_uprag)
		fprintf(stderr,"gmagicuf_id: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgmagicuf_id);
}
#else  /* ! __GNUC__ */
extern unkId *Rgmagicuf_id PROTO((struct Smagicuf_uprag *));
#endif /* ! __GNUC__ */

#define gmagicuf_id(xyzxyz) (*Rgmagicuf_id((struct Smagicuf_uprag *) (xyzxyz)))
#ifdef __GNUC__

stringId *Rgmagicuf_str PROTO((struct Smagicuf_uprag *));

extern __inline__ stringId *Rgmagicuf_str(struct Smagicuf_uprag *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != magicuf_uprag)
		fprintf(stderr,"gmagicuf_str: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgmagicuf_str);
}
#else  /* ! __GNUC__ */
extern stringId *Rgmagicuf_str PROTO((struct Smagicuf_uprag *));
#endif /* ! __GNUC__ */

#define gmagicuf_str(xyzxyz) (*Rgmagicuf_str((struct Smagicuf_uprag *) (xyzxyz)))
#ifdef __GNUC__

long *Rgmagicuf_line PROTO((struct Smagicuf_uprag *));

extern __inline__ long *Rgmagicuf_line(struct Smagicuf_uprag *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != magicuf_uprag)
		fprintf(stderr,"gmagicuf_line: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgmagicuf_line);
}
#else  /* ! __GNUC__ */
extern long *Rgmagicuf_line PROTO((struct Smagicuf_uprag *));
#endif /* ! __GNUC__ */

#define gmagicuf_line(xyzxyz) (*Rgmagicuf_line((struct Smagicuf_uprag *) (xyzxyz)))

extern binding mkabstract_uprag PROTO((unkId, long));
#ifdef __GNUC__

unkId *Rgabstract_id PROTO((struct Sabstract_uprag *));

extern __inline__ unkId *Rgabstract_id(struct Sabstract_uprag *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != abstract_uprag)
		fprintf(stderr,"gabstract_id: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgabstract_id);
}
#else  /* ! __GNUC__ */
extern unkId *Rgabstract_id PROTO((struct Sabstract_uprag *));
#endif /* ! __GNUC__ */

#define gabstract_id(xyzxyz) (*Rgabstract_id((struct Sabstract_uprag *) (xyzxyz)))
#ifdef __GNUC__

long *Rgabstract_line PROTO((struct Sabstract_uprag *));

extern __inline__ long *Rgabstract_line(struct Sabstract_uprag *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != abstract_uprag)
		fprintf(stderr,"gabstract_line: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgabstract_line);
}
#else  /* ! __GNUC__ */
extern long *Rgabstract_line PROTO((struct Sabstract_uprag *));
#endif /* ! __GNUC__ */

#define gabstract_line(xyzxyz) (*Rgabstract_line((struct Sabstract_uprag *) (xyzxyz)))

extern binding mkdspec_uprag PROTO((unkId, list, long));
#ifdef __GNUC__

unkId *Rgdspec_id PROTO((struct Sdspec_uprag *));

extern __inline__ unkId *Rgdspec_id(struct Sdspec_uprag *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != dspec_uprag)
		fprintf(stderr,"gdspec_id: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdspec_id);
}
#else  /* ! __GNUC__ */
extern unkId *Rgdspec_id PROTO((struct Sdspec_uprag *));
#endif /* ! __GNUC__ */

#define gdspec_id(xyzxyz) (*Rgdspec_id((struct Sdspec_uprag *) (xyzxyz)))
#ifdef __GNUC__

list *Rgdspec_tys PROTO((struct Sdspec_uprag *));

extern __inline__ list *Rgdspec_tys(struct Sdspec_uprag *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != dspec_uprag)
		fprintf(stderr,"gdspec_tys: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdspec_tys);
}
#else  /* ! __GNUC__ */
extern list *Rgdspec_tys PROTO((struct Sdspec_uprag *));
#endif /* ! __GNUC__ */

#define gdspec_tys(xyzxyz) (*Rgdspec_tys((struct Sdspec_uprag *) (xyzxyz)))
#ifdef __GNUC__

long *Rgdspec_line PROTO((struct Sdspec_uprag *));

extern __inline__ long *Rgdspec_line(struct Sdspec_uprag *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != dspec_uprag)
		fprintf(stderr,"gdspec_line: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdspec_line);
}
#else  /* ! __GNUC__ */
extern long *Rgdspec_line PROTO((struct Sdspec_uprag *));
#endif /* ! __GNUC__ */

#define gdspec_line(xyzxyz) (*Rgdspec_line((struct Sdspec_uprag *) (xyzxyz)))

#endif
