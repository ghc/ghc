

#include "id.h"
#include "tree.h"

Ttree ttree(t)
 tree t;
{
	return(t -> tag);
}


/************** typdef ******************/

tree mktypdef(PPgtid, PPgtdeflist)
 id PPgtid;
 tree PPgtdeflist;
{
	register struct Stypdef *pp =
		(struct Stypdef *) malloc(sizeof(struct Stypdef));
	pp -> tag = typdef;
	pp -> Xgtid = PPgtid;
	pp -> Xgtdeflist = PPgtdeflist;
	return((tree)pp);
}

id *Rgtid(t)
 struct Stypdef *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != typdef)
		fprintf(stderr,"gtid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtid);
}

tree *Rgtdeflist(t)
 struct Stypdef *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != typdef)
		fprintf(stderr,"gtdeflist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtdeflist);
}

/************** deflist ******************/

tree mkdeflist(PPgdeflist, PPgdef)
 tree PPgdeflist;
 tree PPgdef;
{
	register struct Sdeflist *pp =
		(struct Sdeflist *) malloc(sizeof(struct Sdeflist));
	pp -> tag = deflist;
	pp -> Xgdeflist = PPgdeflist;
	pp -> Xgdef = PPgdef;
	return((tree)pp);
}

tree *Rgdeflist(t)
 struct Sdeflist *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != deflist)
		fprintf(stderr,"gdeflist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdeflist);
}

tree *Rgdef(t)
 struct Sdeflist *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != deflist)
		fprintf(stderr,"gdef: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdef);
}

/************** def ******************/

tree mkdef(PPgdid, PPgditemlist)
 id PPgdid;
 tree PPgditemlist;
{
	register struct Sdef *pp =
		(struct Sdef *) malloc(sizeof(struct Sdef));
	pp -> tag = def;
	pp -> Xgdid = PPgdid;
	pp -> Xgditemlist = PPgditemlist;
	return((tree)pp);
}

id *Rgdid(t)
 struct Sdef *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != def)
		fprintf(stderr,"gdid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdid);
}

tree *Rgditemlist(t)
 struct Sdef *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != def)
		fprintf(stderr,"gditemlist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgditemlist);
}

/************** itemlist ******************/

tree mkitemlist(PPgitemlist, PPgitem)
 tree PPgitemlist;
 tree PPgitem;
{
	register struct Sitemlist *pp =
		(struct Sitemlist *) malloc(sizeof(struct Sitemlist));
	pp -> tag = itemlist;
	pp -> Xgitemlist = PPgitemlist;
	pp -> Xgitem = PPgitem;
	return((tree)pp);
}

tree *Rgitemlist(t)
 struct Sitemlist *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != itemlist)
		fprintf(stderr,"gitemlist: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgitemlist);
}

tree *Rgitem(t)
 struct Sitemlist *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != itemlist)
		fprintf(stderr,"gitem: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgitem);
}

/************** emitemlist ******************/

tree mkemitemlist(void)
{
	register struct Semitemlist *pp =
		(struct Semitemlist *) malloc(sizeof(struct Semitemlist));
	pp -> tag = emitemlist;
	return((tree)pp);
}

/************** item ******************/

tree mkitem(PPgitemfunid, PPgitemtypid)
 id PPgitemfunid;
 id PPgitemtypid;
{
	register struct Sitem *pp =
		(struct Sitem *) malloc(sizeof(struct Sitem));
	pp -> tag = item;
	pp -> Xgitemfunid = PPgitemfunid;
	pp -> Xgitemtypid = PPgitemtypid;
	return((tree)pp);
}

id *Rgitemfunid(t)
 struct Sitem *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != item)
		fprintf(stderr,"gitemfunid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgitemfunid);
}

id *Rgitemtypid(t)
 struct Sitem *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != item)
		fprintf(stderr,"gitemtypid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgitemtypid);
}
