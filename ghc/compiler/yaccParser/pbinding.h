#ifndef pbinding_defined
#define pbinding_defined

#include <stdio.h>

#ifndef PROTO
#ifdef __STDC__
#define PROTO(x) x
#else
#define PROTO(x) /**/
#endif
#endif

typedef enum {
	pgrhs
} Tpbinding;

typedef struct { Tpbinding tag; } *pbinding;

#ifdef __GNUC__
extern __inline__ Tpbinding tpbinding(pbinding t)
{
	return(t -> tag);
}
#else  /* ! __GNUC__ */
extern Tpbinding tpbinding PROTO((pbinding));
#endif /* ! __GNUC__ */

struct Spgrhs {
	Tpbinding tag;
	tree Xggpat;
	list Xggdexprs;
	binding Xggbind;
	stringId Xggfuncname;
	long Xggline;
};

extern pbinding mkpgrhs PROTO((tree, list, binding, stringId, long));
#ifdef __GNUC__

extern __inline__ tree *Rggpat(struct Spgrhs *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != pgrhs)
		fprintf(stderr,"ggpat: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xggpat);
}
#else  /* ! __GNUC__ */
extern tree *Rggpat PROTO((struct Spgrhs *));
#endif /* ! __GNUC__ */

#define ggpat(xyzxyz) (*Rggpat((struct Spgrhs *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ list *Rggdexprs(struct Spgrhs *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != pgrhs)
		fprintf(stderr,"ggdexprs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xggdexprs);
}
#else  /* ! __GNUC__ */
extern list *Rggdexprs PROTO((struct Spgrhs *));
#endif /* ! __GNUC__ */

#define ggdexprs(xyzxyz) (*Rggdexprs((struct Spgrhs *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ binding *Rggbind(struct Spgrhs *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != pgrhs)
		fprintf(stderr,"ggbind: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xggbind);
}
#else  /* ! __GNUC__ */
extern binding *Rggbind PROTO((struct Spgrhs *));
#endif /* ! __GNUC__ */

#define ggbind(xyzxyz) (*Rggbind((struct Spgrhs *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ stringId *Rggfuncname(struct Spgrhs *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != pgrhs)
		fprintf(stderr,"ggfuncname: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xggfuncname);
}
#else  /* ! __GNUC__ */
extern stringId *Rggfuncname PROTO((struct Spgrhs *));
#endif /* ! __GNUC__ */

#define ggfuncname(xyzxyz) (*Rggfuncname((struct Spgrhs *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ long *Rggline(struct Spgrhs *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != pgrhs)
		fprintf(stderr,"ggline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xggline);
}
#else  /* ! __GNUC__ */
extern long *Rggline PROTO((struct Spgrhs *));
#endif /* ! __GNUC__ */

#define ggline(xyzxyz) (*Rggline((struct Spgrhs *) (xyzxyz)))

#endif
