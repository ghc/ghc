#include "id.h"
#include "tree.h"

extern char *malloc();

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
	if(t -> tag != typdef)
		printf("gtid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgtid);
}

tree *Rgtdeflist(t)
 struct Stypdef *t;
{
	if(t -> tag != typdef)
		printf("gtdeflist: illegal selection; was %d\n", t -> tag);
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
	if(t -> tag != deflist)
		printf("gdeflist: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgdeflist);
}

tree *Rgdef(t)
 struct Sdeflist *t;
{
	if(t -> tag != deflist)
		printf("gdef: illegal selection; was %d\n", t -> tag);
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
	if(t -> tag != def)
		printf("gdid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgdid);
}

tree *Rgditemlist(t)
 struct Sdef *t;
{
	if(t -> tag != def)
		printf("gditemlist: illegal selection; was %d\n", t -> tag);
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
	if(t -> tag != itemlist)
		printf("gitemlist: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgitemlist);
}

tree *Rgitem(t)
 struct Sitemlist *t;
{
	if(t -> tag != itemlist)
		printf("gitem: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgitem);
}

/************** emitemlist ******************/

tree mkemitemlist()
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
	if(t -> tag != item)
		printf("gitemfunid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgitemfunid);
}

id *Rgitemtypid(t)
 struct Sitem *t;
{
	if(t -> tag != item)
		printf("gitemtypid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgitemtypid);
}
