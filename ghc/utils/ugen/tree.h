#ifndef tree_defined
#define tree_defined

#include <stdio.h>

#ifndef PROTO
#ifdef __STDC__
#define PROTO(x) x
#else
#define PROTO(x) /**/
#endif
#endif

typedef enum {
	typdef,
	deflist,
	def,
	itemlist,
	emitemlist,
	item
} Ttree;

typedef struct { Ttree tag; } *tree;

#ifdef __GNUC__
Ttree ttree(tree t);
extern __inline__ Ttree ttree(tree t)
{
	return(t -> tag);
}
#else  /* ! __GNUC__ */
extern Ttree ttree PROTO((tree));
#endif /* ! __GNUC__ */

struct Stypdef {
	Ttree tag;
	id Xgtid;
	tree Xgtdeflist;
};

struct Sdeflist {
	Ttree tag;
	tree Xgdeflist;
	tree Xgdef;
};

struct Sdef {
	Ttree tag;
	id Xgdid;
	tree Xgditemlist;
};

struct Sitemlist {
	Ttree tag;
	tree Xgitemlist;
	tree Xgitem;
};

struct Semitemlist {
	Ttree tag;
};

struct Sitem {
	Ttree tag;
	id Xgitemfunid;
	id Xgitemtypid;
};

extern tree mktypdef PROTO((id, tree));
#ifdef __GNUC__

id *Rgtid PROTO((struct Stypdef *));

extern __inline__ id *Rgtid(struct Stypdef *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != typdef)
		fprintf(stderr,"gtid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtid);
}
#else  /* ! __GNUC__ */
extern id *Rgtid PROTO((struct Stypdef *));
#endif /* ! __GNUC__ */

#define gtid(xyzxyz) (*Rgtid((struct Stypdef *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgtdeflist PROTO((struct Stypdef *));

extern __inline__ tree *Rgtdeflist(struct Stypdef *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != typdef)
		fprintf(stderr,"gtdeflist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtdeflist);
}
#else  /* ! __GNUC__ */
extern tree *Rgtdeflist PROTO((struct Stypdef *));
#endif /* ! __GNUC__ */

#define gtdeflist(xyzxyz) (*Rgtdeflist((struct Stypdef *) (xyzxyz)))

extern tree mkdeflist PROTO((tree, tree));
#ifdef __GNUC__

tree *Rgdeflist PROTO((struct Sdeflist *));

extern __inline__ tree *Rgdeflist(struct Sdeflist *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != deflist)
		fprintf(stderr,"gdeflist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdeflist);
}
#else  /* ! __GNUC__ */
extern tree *Rgdeflist PROTO((struct Sdeflist *));
#endif /* ! __GNUC__ */

#define gdeflist(xyzxyz) (*Rgdeflist((struct Sdeflist *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgdef PROTO((struct Sdeflist *));

extern __inline__ tree *Rgdef(struct Sdeflist *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != deflist)
		fprintf(stderr,"gdef: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdef);
}
#else  /* ! __GNUC__ */
extern tree *Rgdef PROTO((struct Sdeflist *));
#endif /* ! __GNUC__ */

#define gdef(xyzxyz) (*Rgdef((struct Sdeflist *) (xyzxyz)))

extern tree mkdef PROTO((id, tree));
#ifdef __GNUC__

id *Rgdid PROTO((struct Sdef *));

extern __inline__ id *Rgdid(struct Sdef *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != def)
		fprintf(stderr,"gdid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdid);
}
#else  /* ! __GNUC__ */
extern id *Rgdid PROTO((struct Sdef *));
#endif /* ! __GNUC__ */

#define gdid(xyzxyz) (*Rgdid((struct Sdef *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgditemlist PROTO((struct Sdef *));

extern __inline__ tree *Rgditemlist(struct Sdef *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != def)
		fprintf(stderr,"gditemlist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgditemlist);
}
#else  /* ! __GNUC__ */
extern tree *Rgditemlist PROTO((struct Sdef *));
#endif /* ! __GNUC__ */

#define gditemlist(xyzxyz) (*Rgditemlist((struct Sdef *) (xyzxyz)))

extern tree mkitemlist PROTO((tree, tree));
#ifdef __GNUC__

tree *Rgitemlist PROTO((struct Sitemlist *));

extern __inline__ tree *Rgitemlist(struct Sitemlist *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != itemlist)
		fprintf(stderr,"gitemlist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgitemlist);
}
#else  /* ! __GNUC__ */
extern tree *Rgitemlist PROTO((struct Sitemlist *));
#endif /* ! __GNUC__ */

#define gitemlist(xyzxyz) (*Rgitemlist((struct Sitemlist *) (xyzxyz)))
#ifdef __GNUC__

tree *Rgitem PROTO((struct Sitemlist *));

extern __inline__ tree *Rgitem(struct Sitemlist *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != itemlist)
		fprintf(stderr,"gitem: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgitem);
}
#else  /* ! __GNUC__ */
extern tree *Rgitem PROTO((struct Sitemlist *));
#endif /* ! __GNUC__ */

#define gitem(xyzxyz) (*Rgitem((struct Sitemlist *) (xyzxyz)))

extern tree mkemitemlist PROTO((void));

extern tree mkitem PROTO((id, id));
#ifdef __GNUC__

id *Rgitemfunid PROTO((struct Sitem *));

extern __inline__ id *Rgitemfunid(struct Sitem *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != item)
		fprintf(stderr,"gitemfunid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgitemfunid);
}
#else  /* ! __GNUC__ */
extern id *Rgitemfunid PROTO((struct Sitem *));
#endif /* ! __GNUC__ */

#define gitemfunid(xyzxyz) (*Rgitemfunid((struct Sitem *) (xyzxyz)))
#ifdef __GNUC__

id *Rgitemtypid PROTO((struct Sitem *));

extern __inline__ id *Rgitemtypid(struct Sitem *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != item)
		fprintf(stderr,"gitemtypid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgitemtypid);
}
#else  /* ! __GNUC__ */
extern id *Rgitemtypid PROTO((struct Sitem *));
#endif /* ! __GNUC__ */

#define gitemtypid(xyzxyz) (*Rgitemtypid((struct Sitem *) (xyzxyz)))

#endif
