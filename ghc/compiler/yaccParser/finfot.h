#ifndef finfot_defined
#define finfot_defined

#include <stdio.h>

#ifndef PROTO
#ifdef __STDC__
#define PROTO(x) x
#else
#define PROTO(x) /**/
#endif
#endif

typedef enum {
	nofinfo,
	finfo
} Tfinfot;

typedef struct { Tfinfot tag; } *finfot;

#ifdef __GNUC__
extern __inline__ Tfinfot tfinfot(finfot t)
{
	return(t -> tag);
}
#else  /* ! __GNUC__ */
extern Tfinfot tfinfot PROTO((finfot));
#endif /* ! __GNUC__ */

struct Snofinfo {
	Tfinfot tag;
};

struct Sfinfo {
	Tfinfot tag;
	stringId Xfi1;
	stringId Xfi2;
};

extern finfot mknofinfo PROTO(());

extern finfot mkfinfo PROTO((stringId, stringId));
#ifdef __GNUC__

extern __inline__ stringId *Rfi1(struct Sfinfo *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != finfo)
		fprintf(stderr,"fi1: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xfi1);
}
#else  /* ! __GNUC__ */
extern stringId *Rfi1 PROTO((struct Sfinfo *));
#endif /* ! __GNUC__ */

#define fi1(xyzxyz) (*Rfi1((struct Sfinfo *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ stringId *Rfi2(struct Sfinfo *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != finfo)
		fprintf(stderr,"fi2: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xfi2);
}
#else  /* ! __GNUC__ */
extern stringId *Rfi2 PROTO((struct Sfinfo *));
#endif /* ! __GNUC__ */

#define fi2(xyzxyz) (*Rfi2((struct Sfinfo *) (xyzxyz)))

#endif
