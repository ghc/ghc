#ifndef list_defined
#define list_defined

#include <stdio.h>

#ifndef PROTO
#ifdef __STDC__
#define PROTO(x) x
#else
#define PROTO(x) /**/
#endif
#endif

typedef enum {
	lcons,
	lnil
} Tlist;

typedef struct { Tlist tag; } *list;

#ifdef __GNUC__
extern __inline__ Tlist tlist(list t)
{
	return(t -> tag);
}
#else  /* ! __GNUC__ */
extern Tlist tlist PROTO((list));
#endif /* ! __GNUC__ */

struct Slcons {
	Tlist tag;
	VOID_STAR Xlhd;
	list Xltl;
};

struct Slnil {
	Tlist tag;
};

extern list mklcons PROTO((VOID_STAR, list));
#ifdef __GNUC__

extern __inline__ VOID_STAR *Rlhd(struct Slcons *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != lcons)
		fprintf(stderr,"lhd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xlhd);
}
#else  /* ! __GNUC__ */
extern VOID_STAR *Rlhd PROTO((struct Slcons *));
#endif /* ! __GNUC__ */

#define lhd(xyzxyz) (*Rlhd((struct Slcons *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ list *Rltl(struct Slcons *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != lcons)
		fprintf(stderr,"ltl: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xltl);
}
#else  /* ! __GNUC__ */
extern list *Rltl PROTO((struct Slcons *));
#endif /* ! __GNUC__ */

#define ltl(xyzxyz) (*Rltl((struct Slcons *) (xyzxyz)))

extern list mklnil PROTO(());

#endif
