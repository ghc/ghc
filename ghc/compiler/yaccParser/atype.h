#ifndef atype_defined
#define atype_defined

#include <stdio.h>

#ifndef PROTO
#ifdef __STDC__
#define PROTO(x) x
#else
#define PROTO(x) /**/
#endif
#endif

typedef enum {
	atc
} Tatype;

typedef struct { Tatype tag; } *atype;

#ifdef __GNUC__
Tatype tatype(atype t);
extern __inline__ Tatype tatype(atype t)
{
	return(t -> tag);
}
#else  /* ! __GNUC__ */
extern Tatype tatype PROTO((atype));
#endif /* ! __GNUC__ */

struct Satc {
	Tatype tag;
	unkId Xgatcid;
	list Xgatctypel;
	long Xgatcline;
};

extern atype mkatc PROTO((unkId, list, long));
#ifdef __GNUC__

unkId *Rgatcid PROTO((struct Satc *));

extern __inline__ unkId *Rgatcid(struct Satc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != atc)
		fprintf(stderr,"gatcid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgatcid);
}
#else  /* ! __GNUC__ */
extern unkId *Rgatcid PROTO((struct Satc *));
#endif /* ! __GNUC__ */

#define gatcid(xyzxyz) (*Rgatcid((struct Satc *) (xyzxyz)))
#ifdef __GNUC__

list *Rgatctypel PROTO((struct Satc *));

extern __inline__ list *Rgatctypel(struct Satc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != atc)
		fprintf(stderr,"gatctypel: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgatctypel);
}
#else  /* ! __GNUC__ */
extern list *Rgatctypel PROTO((struct Satc *));
#endif /* ! __GNUC__ */

#define gatctypel(xyzxyz) (*Rgatctypel((struct Satc *) (xyzxyz)))
#ifdef __GNUC__

long *Rgatcline PROTO((struct Satc *));

extern __inline__ long *Rgatcline(struct Satc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != atc)
		fprintf(stderr,"gatcline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgatcline);
}
#else  /* ! __GNUC__ */
extern long *Rgatcline PROTO((struct Satc *));
#endif /* ! __GNUC__ */

#define gatcline(xyzxyz) (*Rgatcline((struct Satc *) (xyzxyz)))

#endif
