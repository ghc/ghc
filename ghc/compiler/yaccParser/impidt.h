#ifndef impidt_defined
#define impidt_defined

#include <stdio.h>

#ifndef PROTO
#ifdef __STDC__
#define PROTO(x) x
#else
#define PROTO(x) /**/
#endif
#endif

typedef enum {
	impid,
	imptype,
	impsyn,
	impeqtype,
	impclass,
	impinst,
	impmod
} Timpidt;

typedef struct { Timpidt tag; } *impidt;

/* Compatibility defines */
extern Timpidt timpidt PROTO((impidt));

struct Simpid {
	Timpidt tag;
	id Xgimpid;
	ttype Xgimptype;
	finfot Xgimpfinfo;
	long Xgivline;
};

struct Simptype {
	Timpidt tag;
	list Xgimptypec;
	ttype Xgimptypet;
	list Xgimptyped;
	long Xgitline;
};

struct Simpsyn {
	Timpidt tag;
	ttype Xgimpsynti;
	ttype Xgimpsynts;
	long Xgisline;
};

struct Simpeqtype {
	Timpidt tag;
	binding Xgimpeqtype;
};

struct Simpclass {
	Timpidt tag;
	list Xgimpclassc;
	ttype Xgimpclasst;
	list Xgimpclassw;
	long Xgicline;
};

struct Simpinst {
	Timpidt tag;
	list Xgimpinstc;
	id Xgimpinstid;
	ttype Xgimpinstt;
	long Xgiiline;
};

struct Simpmod {
	Timpidt tag;
	id Xgimpmodn;
	list Xgimpmodimp;
	list Xgimpmodren;
	long Xgimline;
};

#endif
extern impidt mkimpid PROTO((id, ttype, finfot, long));
extern id *Rgimpid PROTO((struct Simpid *));
#define gimpid(xyzxyz) (*Rgimpid((struct Simpid *) (xyzxyz)))
extern ttype *Rgimptype PROTO((struct Simpid *));
#define gimptype(xyzxyz) (*Rgimptype((struct Simpid *) (xyzxyz)))
extern finfot *Rgimpfinfo PROTO((struct Simpid *));
#define gimpfinfo(xyzxyz) (*Rgimpfinfo((struct Simpid *) (xyzxyz)))
extern long *Rgivline PROTO((struct Simpid *));
#define givline(xyzxyz) (*Rgivline((struct Simpid *) (xyzxyz)))

extern impidt mkimptype PROTO((list, ttype, list, long));
extern list *Rgimptypec PROTO((struct Simptype *));
#define gimptypec(xyzxyz) (*Rgimptypec((struct Simptype *) (xyzxyz)))
extern ttype *Rgimptypet PROTO((struct Simptype *));
#define gimptypet(xyzxyz) (*Rgimptypet((struct Simptype *) (xyzxyz)))
extern list *Rgimptyped PROTO((struct Simptype *));
#define gimptyped(xyzxyz) (*Rgimptyped((struct Simptype *) (xyzxyz)))
extern long *Rgitline PROTO((struct Simptype *));
#define gitline(xyzxyz) (*Rgitline((struct Simptype *) (xyzxyz)))

extern impidt mkimpsyn PROTO((ttype, ttype, long));
extern ttype *Rgimpsynti PROTO((struct Simpsyn *));
#define gimpsynti(xyzxyz) (*Rgimpsynti((struct Simpsyn *) (xyzxyz)))
extern ttype *Rgimpsynts PROTO((struct Simpsyn *));
#define gimpsynts(xyzxyz) (*Rgimpsynts((struct Simpsyn *) (xyzxyz)))
extern long *Rgisline PROTO((struct Simpsyn *));
#define gisline(xyzxyz) (*Rgisline((struct Simpsyn *) (xyzxyz)))

extern impidt mkimpeqtype PROTO((binding));
extern binding *Rgimpeqtype PROTO((struct Simpeqtype *));
#define gimpeqtype(xyzxyz) (*Rgimpeqtype((struct Simpeqtype *) (xyzxyz)))

extern impidt mkimpclass PROTO((list, ttype, list, long));
extern list *Rgimpclassc PROTO((struct Simpclass *));
#define gimpclassc(xyzxyz) (*Rgimpclassc((struct Simpclass *) (xyzxyz)))
extern ttype *Rgimpclasst PROTO((struct Simpclass *));
#define gimpclasst(xyzxyz) (*Rgimpclasst((struct Simpclass *) (xyzxyz)))
extern list *Rgimpclassw PROTO((struct Simpclass *));
#define gimpclassw(xyzxyz) (*Rgimpclassw((struct Simpclass *) (xyzxyz)))
extern long *Rgicline PROTO((struct Simpclass *));
#define gicline(xyzxyz) (*Rgicline((struct Simpclass *) (xyzxyz)))

extern impidt mkimpinst PROTO((list, id, ttype, long));
extern list *Rgimpinstc PROTO((struct Simpinst *));
#define gimpinstc(xyzxyz) (*Rgimpinstc((struct Simpinst *) (xyzxyz)))
extern id *Rgimpinstid PROTO((struct Simpinst *));
#define gimpinstid(xyzxyz) (*Rgimpinstid((struct Simpinst *) (xyzxyz)))
extern ttype *Rgimpinstt PROTO((struct Simpinst *));
#define gimpinstt(xyzxyz) (*Rgimpinstt((struct Simpinst *) (xyzxyz)))
extern long *Rgiiline PROTO((struct Simpinst *));
#define giiline(xyzxyz) (*Rgiiline((struct Simpinst *) (xyzxyz)))

extern impidt mkimpmod PROTO((id, list, list, long));
extern id *Rgimpmodn PROTO((struct Simpmod *));
#define gimpmodn(xyzxyz) (*Rgimpmodn((struct Simpmod *) (xyzxyz)))
extern list *Rgimpmodimp PROTO((struct Simpmod *));
#define gimpmodimp(xyzxyz) (*Rgimpmodimp((struct Simpmod *) (xyzxyz)))
extern list *Rgimpmodren PROTO((struct Simpmod *));
#define gimpmodren(xyzxyz) (*Rgimpmodren((struct Simpmod *) (xyzxyz)))
extern long *Rgimline PROTO((struct Simpmod *));
#define gimline(xyzxyz) (*Rgimline((struct Simpmod *) (xyzxyz)))

