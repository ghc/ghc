#ifndef entidt_defined
#define entidt_defined

#include <stdio.h>

#ifndef PROTO
#ifdef __STDC__
#define PROTO(x) x
#else
#define PROTO(x) /**/
#endif
#endif

typedef enum {
	entid,
	enttype,
	enttypeall,
	enttypecons,
	entclass,
	entmod
} Tentidt;

typedef struct { Tentidt tag; } *entidt;

#ifdef __GNUC__
extern __inline__ Tentidt tentidt(entidt t)
{
	return(t -> tag);
}
#else  /* ! __GNUC__ */
extern Tentidt tentidt PROTO((entidt));
#endif /* ! __GNUC__ */

struct Sentid {
	Tentidt tag;
	stringId Xgentid;
};

struct Senttype {
	Tentidt tag;
	stringId Xgitentid;
};

struct Senttypeall {
	Tentidt tag;
	stringId Xgatentid;
};

struct Senttypecons {
	Tentidt tag;
	stringId Xgctentid;
	list Xgctentcons;
};

struct Sentclass {
	Tentidt tag;
	stringId Xgcentid;
	list Xgcentops;
};

struct Sentmod {
	Tentidt tag;
	stringId Xgmentid;
};

extern entidt mkentid PROTO((stringId));
#ifdef __GNUC__

extern __inline__ stringId *Rgentid(struct Sentid *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != entid)
		fprintf(stderr,"gentid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgentid);
}
#else  /* ! __GNUC__ */
extern stringId *Rgentid PROTO((struct Sentid *));
#endif /* ! __GNUC__ */

#define gentid(xyzxyz) (*Rgentid((struct Sentid *) (xyzxyz)))

extern entidt mkenttype PROTO((stringId));
#ifdef __GNUC__

extern __inline__ stringId *Rgitentid(struct Senttype *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != enttype)
		fprintf(stderr,"gitentid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgitentid);
}
#else  /* ! __GNUC__ */
extern stringId *Rgitentid PROTO((struct Senttype *));
#endif /* ! __GNUC__ */

#define gitentid(xyzxyz) (*Rgitentid((struct Senttype *) (xyzxyz)))

extern entidt mkenttypeall PROTO((stringId));
#ifdef __GNUC__

extern __inline__ stringId *Rgatentid(struct Senttypeall *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != enttypeall)
		fprintf(stderr,"gatentid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgatentid);
}
#else  /* ! __GNUC__ */
extern stringId *Rgatentid PROTO((struct Senttypeall *));
#endif /* ! __GNUC__ */

#define gatentid(xyzxyz) (*Rgatentid((struct Senttypeall *) (xyzxyz)))

extern entidt mkenttypecons PROTO((stringId, list));
#ifdef __GNUC__

extern __inline__ stringId *Rgctentid(struct Senttypecons *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != enttypecons)
		fprintf(stderr,"gctentid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgctentid);
}
#else  /* ! __GNUC__ */
extern stringId *Rgctentid PROTO((struct Senttypecons *));
#endif /* ! __GNUC__ */

#define gctentid(xyzxyz) (*Rgctentid((struct Senttypecons *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ list *Rgctentcons(struct Senttypecons *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != enttypecons)
		fprintf(stderr,"gctentcons: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgctentcons);
}
#else  /* ! __GNUC__ */
extern list *Rgctentcons PROTO((struct Senttypecons *));
#endif /* ! __GNUC__ */

#define gctentcons(xyzxyz) (*Rgctentcons((struct Senttypecons *) (xyzxyz)))

extern entidt mkentclass PROTO((stringId, list));
#ifdef __GNUC__

extern __inline__ stringId *Rgcentid(struct Sentclass *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != entclass)
		fprintf(stderr,"gcentid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcentid);
}
#else  /* ! __GNUC__ */
extern stringId *Rgcentid PROTO((struct Sentclass *));
#endif /* ! __GNUC__ */

#define gcentid(xyzxyz) (*Rgcentid((struct Sentclass *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ list *Rgcentops(struct Sentclass *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != entclass)
		fprintf(stderr,"gcentops: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcentops);
}
#else  /* ! __GNUC__ */
extern list *Rgcentops PROTO((struct Sentclass *));
#endif /* ! __GNUC__ */

#define gcentops(xyzxyz) (*Rgcentops((struct Sentclass *) (xyzxyz)))

extern entidt mkentmod PROTO((stringId));
#ifdef __GNUC__

extern __inline__ stringId *Rgmentid(struct Sentmod *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != entmod)
		fprintf(stderr,"gmentid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgmentid);
}
#else  /* ! __GNUC__ */
extern stringId *Rgmentid PROTO((struct Sentmod *));
#endif /* ! __GNUC__ */

#define gmentid(xyzxyz) (*Rgmentid((struct Sentmod *) (xyzxyz)))

#endif
