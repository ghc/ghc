

#include "hspincl.h"
#include "yaccParser/impidt.h"
Timpidt timpidt(t)
 impidt t;
{
	return(t -> tag);
}


/************** impid ******************/

impidt mkimpid(PPgimpid, PPgimptype, PPgimpfinfo, PPgivline)
 id PPgimpid;
 ttype PPgimptype;
 finfot PPgimpfinfo;
 long PPgivline;
{
	register struct Simpid *pp =
		(struct Simpid *) malloc(sizeof(struct Simpid));
	pp -> tag = impid;
	pp -> Xgimpid = PPgimpid;
	pp -> Xgimptype = PPgimptype;
	pp -> Xgimpfinfo = PPgimpfinfo;
	pp -> Xgivline = PPgivline;
	return((impidt)pp);
}

id *Rgimpid(t)
 struct Simpid *t;
{
	if(t -> tag != impid)
		fprintf(stderr,"gimpid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpid);
}

ttype *Rgimptype(t)
 struct Simpid *t;
{
	if(t -> tag != impid)
		fprintf(stderr,"gimptype: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimptype);
}

finfot *Rgimpfinfo(t)
 struct Simpid *t;
{
	if(t -> tag != impid)
		fprintf(stderr,"gimpfinfo: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpfinfo);
}

long *Rgivline(t)
 struct Simpid *t;
{
	if(t -> tag != impid)
		fprintf(stderr,"givline: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgivline);
}

/************** imptype ******************/

impidt mkimptype(PPgimptypec, PPgimptypet, PPgimptyped, PPgitline)
 list PPgimptypec;
 ttype PPgimptypet;
 list PPgimptyped;
 long PPgitline;
{
	register struct Simptype *pp =
		(struct Simptype *) malloc(sizeof(struct Simptype));
	pp -> tag = imptype;
	pp -> Xgimptypec = PPgimptypec;
	pp -> Xgimptypet = PPgimptypet;
	pp -> Xgimptyped = PPgimptyped;
	pp -> Xgitline = PPgitline;
	return((impidt)pp);
}

list *Rgimptypec(t)
 struct Simptype *t;
{
	if(t -> tag != imptype)
		fprintf(stderr,"gimptypec: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimptypec);
}

ttype *Rgimptypet(t)
 struct Simptype *t;
{
	if(t -> tag != imptype)
		fprintf(stderr,"gimptypet: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimptypet);
}

list *Rgimptyped(t)
 struct Simptype *t;
{
	if(t -> tag != imptype)
		fprintf(stderr,"gimptyped: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimptyped);
}

long *Rgitline(t)
 struct Simptype *t;
{
	if(t -> tag != imptype)
		fprintf(stderr,"gitline: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgitline);
}

/************** impsyn ******************/

impidt mkimpsyn(PPgimpsynti, PPgimpsynts, PPgisline)
 ttype PPgimpsynti;
 ttype PPgimpsynts;
 long PPgisline;
{
	register struct Simpsyn *pp =
		(struct Simpsyn *) malloc(sizeof(struct Simpsyn));
	pp -> tag = impsyn;
	pp -> Xgimpsynti = PPgimpsynti;
	pp -> Xgimpsynts = PPgimpsynts;
	pp -> Xgisline = PPgisline;
	return((impidt)pp);
}

ttype *Rgimpsynti(t)
 struct Simpsyn *t;
{
	if(t -> tag != impsyn)
		fprintf(stderr,"gimpsynti: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpsynti);
}

ttype *Rgimpsynts(t)
 struct Simpsyn *t;
{
	if(t -> tag != impsyn)
		fprintf(stderr,"gimpsynts: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpsynts);
}

long *Rgisline(t)
 struct Simpsyn *t;
{
	if(t -> tag != impsyn)
		fprintf(stderr,"gisline: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgisline);
}

/************** impeqtype ******************/

impidt mkimpeqtype(PPgimpeqtype)
 binding PPgimpeqtype;
{
	register struct Simpeqtype *pp =
		(struct Simpeqtype *) malloc(sizeof(struct Simpeqtype));
	pp -> tag = impeqtype;
	pp -> Xgimpeqtype = PPgimpeqtype;
	return((impidt)pp);
}

binding *Rgimpeqtype(t)
 struct Simpeqtype *t;
{
	if(t -> tag != impeqtype)
		fprintf(stderr,"gimpeqtype: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpeqtype);
}

/************** impclass ******************/

impidt mkimpclass(PPgimpclassc, PPgimpclasst, PPgimpclassw, PPgicline)
 list PPgimpclassc;
 ttype PPgimpclasst;
 list PPgimpclassw;
 long PPgicline;
{
	register struct Simpclass *pp =
		(struct Simpclass *) malloc(sizeof(struct Simpclass));
	pp -> tag = impclass;
	pp -> Xgimpclassc = PPgimpclassc;
	pp -> Xgimpclasst = PPgimpclasst;
	pp -> Xgimpclassw = PPgimpclassw;
	pp -> Xgicline = PPgicline;
	return((impidt)pp);
}

list *Rgimpclassc(t)
 struct Simpclass *t;
{
	if(t -> tag != impclass)
		fprintf(stderr,"gimpclassc: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpclassc);
}

ttype *Rgimpclasst(t)
 struct Simpclass *t;
{
	if(t -> tag != impclass)
		fprintf(stderr,"gimpclasst: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpclasst);
}

list *Rgimpclassw(t)
 struct Simpclass *t;
{
	if(t -> tag != impclass)
		fprintf(stderr,"gimpclassw: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpclassw);
}

long *Rgicline(t)
 struct Simpclass *t;
{
	if(t -> tag != impclass)
		fprintf(stderr,"gicline: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgicline);
}

/************** impinst ******************/

impidt mkimpinst(PPgimpinstc, PPgimpinstid, PPgimpinstt, PPgiiline)
 list PPgimpinstc;
 id PPgimpinstid;
 ttype PPgimpinstt;
 long PPgiiline;
{
	register struct Simpinst *pp =
		(struct Simpinst *) malloc(sizeof(struct Simpinst));
	pp -> tag = impinst;
	pp -> Xgimpinstc = PPgimpinstc;
	pp -> Xgimpinstid = PPgimpinstid;
	pp -> Xgimpinstt = PPgimpinstt;
	pp -> Xgiiline = PPgiiline;
	return((impidt)pp);
}

list *Rgimpinstc(t)
 struct Simpinst *t;
{
	if(t -> tag != impinst)
		fprintf(stderr,"gimpinstc: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpinstc);
}

id *Rgimpinstid(t)
 struct Simpinst *t;
{
	if(t -> tag != impinst)
		fprintf(stderr,"gimpinstid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpinstid);
}

ttype *Rgimpinstt(t)
 struct Simpinst *t;
{
	if(t -> tag != impinst)
		fprintf(stderr,"gimpinstt: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpinstt);
}

long *Rgiiline(t)
 struct Simpinst *t;
{
	if(t -> tag != impinst)
		fprintf(stderr,"giiline: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgiiline);
}

/************** impmod ******************/

impidt mkimpmod(PPgimpmodn, PPgimpmodimp, PPgimpmodren, PPgimline)
 id PPgimpmodn;
 list PPgimpmodimp;
 list PPgimpmodren;
 long PPgimline;
{
	register struct Simpmod *pp =
		(struct Simpmod *) malloc(sizeof(struct Simpmod));
	pp -> tag = impmod;
	pp -> Xgimpmodn = PPgimpmodn;
	pp -> Xgimpmodimp = PPgimpmodimp;
	pp -> Xgimpmodren = PPgimpmodren;
	pp -> Xgimline = PPgimline;
	return((impidt)pp);
}

id *Rgimpmodn(t)
 struct Simpmod *t;
{
	if(t -> tag != impmod)
		fprintf(stderr,"gimpmodn: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpmodn);
}

list *Rgimpmodimp(t)
 struct Simpmod *t;
{
	if(t -> tag != impmod)
		fprintf(stderr,"gimpmodimp: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpmodimp);
}

list *Rgimpmodren(t)
 struct Simpmod *t;
{
	if(t -> tag != impmod)
		fprintf(stderr,"gimpmodren: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpmodren);
}

long *Rgimline(t)
 struct Simpmod *t;
{
	if(t -> tag != impmod)
		fprintf(stderr,"gimline: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimline);
}
