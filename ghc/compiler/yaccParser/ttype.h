#ifndef ttype_defined
#define ttype_defined

#include <stdio.h>

#ifndef PROTO
#ifdef __STDC__
#define PROTO(x) x
#else
#define PROTO(x) /**/
#endif
#endif

typedef enum {
	tname,
	namedtvar,
	tllist,
	ttuple,
	tfun,
	context,
	unidict,
	unityvartemplate,
	uniforall,
	ty_maybe_nothing,
	ty_maybe_just
} Tttype;

typedef struct { Tttype tag; } *ttype;

#ifdef __GNUC__
extern __inline__ Tttype tttype(ttype t)
{
	return(t -> tag);
}
#else  /* ! __GNUC__ */
extern Tttype tttype PROTO((ttype));
#endif /* ! __GNUC__ */

struct Stname {
	Tttype tag;
	unkId Xgtypeid;
	list Xgtypel;
};

struct Snamedtvar {
	Tttype tag;
	unkId Xgnamedtvar;
};

struct Stllist {
	Tttype tag;
	ttype Xgtlist;
};

struct Sttuple {
	Tttype tag;
	list Xgttuple;
};

struct Stfun {
	Tttype tag;
	ttype Xgtfun;
	ttype Xgtarg;
};

struct Scontext {
	Tttype tag;
	list Xgtcontextl;
	ttype Xgtcontextt;
};

struct Sunidict {
	Tttype tag;
	unkId Xgunidict_clas;
	ttype Xgunidict_ty;
};

struct Sunityvartemplate {
	Tttype tag;
	unkId Xgunityvartemplate;
};

struct Suniforall {
	Tttype tag;
	list Xguniforall_tv;
	ttype Xguniforall_ty;
};

struct Sty_maybe_nothing {
	Tttype tag;
};

struct Sty_maybe_just {
	Tttype tag;
	ttype Xgty_maybe;
};

extern ttype mktname PROTO((unkId, list));
#ifdef __GNUC__

extern __inline__ unkId *Rgtypeid(struct Stname *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != tname)
		fprintf(stderr,"gtypeid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtypeid);
}
#else  /* ! __GNUC__ */
extern unkId *Rgtypeid PROTO((struct Stname *));
#endif /* ! __GNUC__ */

#define gtypeid(xyzxyz) (*Rgtypeid((struct Stname *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ list *Rgtypel(struct Stname *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != tname)
		fprintf(stderr,"gtypel: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtypel);
}
#else  /* ! __GNUC__ */
extern list *Rgtypel PROTO((struct Stname *));
#endif /* ! __GNUC__ */

#define gtypel(xyzxyz) (*Rgtypel((struct Stname *) (xyzxyz)))

extern ttype mknamedtvar PROTO((unkId));
#ifdef __GNUC__

extern __inline__ unkId *Rgnamedtvar(struct Snamedtvar *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != namedtvar)
		fprintf(stderr,"gnamedtvar: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnamedtvar);
}
#else  /* ! __GNUC__ */
extern unkId *Rgnamedtvar PROTO((struct Snamedtvar *));
#endif /* ! __GNUC__ */

#define gnamedtvar(xyzxyz) (*Rgnamedtvar((struct Snamedtvar *) (xyzxyz)))

extern ttype mktllist PROTO((ttype));
#ifdef __GNUC__

extern __inline__ ttype *Rgtlist(struct Stllist *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != tllist)
		fprintf(stderr,"gtlist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtlist);
}
#else  /* ! __GNUC__ */
extern ttype *Rgtlist PROTO((struct Stllist *));
#endif /* ! __GNUC__ */

#define gtlist(xyzxyz) (*Rgtlist((struct Stllist *) (xyzxyz)))

extern ttype mkttuple PROTO((list));
#ifdef __GNUC__

extern __inline__ list *Rgttuple(struct Sttuple *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ttuple)
		fprintf(stderr,"gttuple: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgttuple);
}
#else  /* ! __GNUC__ */
extern list *Rgttuple PROTO((struct Sttuple *));
#endif /* ! __GNUC__ */

#define gttuple(xyzxyz) (*Rgttuple((struct Sttuple *) (xyzxyz)))

extern ttype mktfun PROTO((ttype, ttype));
#ifdef __GNUC__

extern __inline__ ttype *Rgtfun(struct Stfun *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != tfun)
		fprintf(stderr,"gtfun: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtfun);
}
#else  /* ! __GNUC__ */
extern ttype *Rgtfun PROTO((struct Stfun *));
#endif /* ! __GNUC__ */

#define gtfun(xyzxyz) (*Rgtfun((struct Stfun *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ ttype *Rgtarg(struct Stfun *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != tfun)
		fprintf(stderr,"gtarg: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtarg);
}
#else  /* ! __GNUC__ */
extern ttype *Rgtarg PROTO((struct Stfun *));
#endif /* ! __GNUC__ */

#define gtarg(xyzxyz) (*Rgtarg((struct Stfun *) (xyzxyz)))

extern ttype mkcontext PROTO((list, ttype));
#ifdef __GNUC__

extern __inline__ list *Rgtcontextl(struct Scontext *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != context)
		fprintf(stderr,"gtcontextl: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtcontextl);
}
#else  /* ! __GNUC__ */
extern list *Rgtcontextl PROTO((struct Scontext *));
#endif /* ! __GNUC__ */

#define gtcontextl(xyzxyz) (*Rgtcontextl((struct Scontext *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ ttype *Rgtcontextt(struct Scontext *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != context)
		fprintf(stderr,"gtcontextt: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtcontextt);
}
#else  /* ! __GNUC__ */
extern ttype *Rgtcontextt PROTO((struct Scontext *));
#endif /* ! __GNUC__ */

#define gtcontextt(xyzxyz) (*Rgtcontextt((struct Scontext *) (xyzxyz)))

extern ttype mkunidict PROTO((unkId, ttype));
#ifdef __GNUC__

extern __inline__ unkId *Rgunidict_clas(struct Sunidict *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != unidict)
		fprintf(stderr,"gunidict_clas: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgunidict_clas);
}
#else  /* ! __GNUC__ */
extern unkId *Rgunidict_clas PROTO((struct Sunidict *));
#endif /* ! __GNUC__ */

#define gunidict_clas(xyzxyz) (*Rgunidict_clas((struct Sunidict *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ ttype *Rgunidict_ty(struct Sunidict *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != unidict)
		fprintf(stderr,"gunidict_ty: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgunidict_ty);
}
#else  /* ! __GNUC__ */
extern ttype *Rgunidict_ty PROTO((struct Sunidict *));
#endif /* ! __GNUC__ */

#define gunidict_ty(xyzxyz) (*Rgunidict_ty((struct Sunidict *) (xyzxyz)))

extern ttype mkunityvartemplate PROTO((unkId));
#ifdef __GNUC__

extern __inline__ unkId *Rgunityvartemplate(struct Sunityvartemplate *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != unityvartemplate)
		fprintf(stderr,"gunityvartemplate: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgunityvartemplate);
}
#else  /* ! __GNUC__ */
extern unkId *Rgunityvartemplate PROTO((struct Sunityvartemplate *));
#endif /* ! __GNUC__ */

#define gunityvartemplate(xyzxyz) (*Rgunityvartemplate((struct Sunityvartemplate *) (xyzxyz)))

extern ttype mkuniforall PROTO((list, ttype));
#ifdef __GNUC__

extern __inline__ list *Rguniforall_tv(struct Suniforall *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != uniforall)
		fprintf(stderr,"guniforall_tv: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xguniforall_tv);
}
#else  /* ! __GNUC__ */
extern list *Rguniforall_tv PROTO((struct Suniforall *));
#endif /* ! __GNUC__ */

#define guniforall_tv(xyzxyz) (*Rguniforall_tv((struct Suniforall *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ ttype *Rguniforall_ty(struct Suniforall *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != uniforall)
		fprintf(stderr,"guniforall_ty: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xguniforall_ty);
}
#else  /* ! __GNUC__ */
extern ttype *Rguniforall_ty PROTO((struct Suniforall *));
#endif /* ! __GNUC__ */

#define guniforall_ty(xyzxyz) (*Rguniforall_ty((struct Suniforall *) (xyzxyz)))

extern ttype mkty_maybe_nothing PROTO(());

extern ttype mkty_maybe_just PROTO((ttype));
#ifdef __GNUC__

extern __inline__ ttype *Rgty_maybe(struct Sty_maybe_just *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != ty_maybe_just)
		fprintf(stderr,"gty_maybe: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgty_maybe);
}
#else  /* ! __GNUC__ */
extern ttype *Rgty_maybe PROTO((struct Sty_maybe_just *));
#endif /* ! __GNUC__ */

#define gty_maybe(xyzxyz) (*Rgty_maybe((struct Sty_maybe_just *) (xyzxyz)))

#endif
