

#include "hspincl.h"
#include "yaccParser/binding.h"

Tbinding tbinding(t)
 binding t;
{
	return(t -> tag);
}


/************** tbind ******************/

binding mktbind(PPgtbindc, PPgtbindid, PPgtbindl, PPgtbindd, PPgtline, PPgtpragma)
 list PPgtbindc;
 ttype PPgtbindid;
 list PPgtbindl;
 list PPgtbindd;
 long PPgtline;
 hpragma PPgtpragma;
{
	register struct Stbind *pp =
		(struct Stbind *) malloc(sizeof(struct Stbind));
	pp -> tag = tbind;
	pp -> Xgtbindc = PPgtbindc;
	pp -> Xgtbindid = PPgtbindid;
	pp -> Xgtbindl = PPgtbindl;
	pp -> Xgtbindd = PPgtbindd;
	pp -> Xgtline = PPgtline;
	pp -> Xgtpragma = PPgtpragma;
	return((binding)pp);
}

list *Rgtbindc(t)
 struct Stbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != tbind)
		fprintf(stderr,"gtbindc: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtbindc);
}

ttype *Rgtbindid(t)
 struct Stbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != tbind)
		fprintf(stderr,"gtbindid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtbindid);
}

list *Rgtbindl(t)
 struct Stbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != tbind)
		fprintf(stderr,"gtbindl: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtbindl);
}

list *Rgtbindd(t)
 struct Stbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != tbind)
		fprintf(stderr,"gtbindd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtbindd);
}

long *Rgtline(t)
 struct Stbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != tbind)
		fprintf(stderr,"gtline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtline);
}

hpragma *Rgtpragma(t)
 struct Stbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != tbind)
		fprintf(stderr,"gtpragma: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgtpragma);
}

/************** nbind ******************/

binding mknbind(PPgnbindid, PPgnbindas, PPgnline, PPgnpragma)
 ttype PPgnbindid;
 ttype PPgnbindas;
 long PPgnline;
 hpragma PPgnpragma;
{
	register struct Snbind *pp =
		(struct Snbind *) malloc(sizeof(struct Snbind));
	pp -> tag = nbind;
	pp -> Xgnbindid = PPgnbindid;
	pp -> Xgnbindas = PPgnbindas;
	pp -> Xgnline = PPgnline;
	pp -> Xgnpragma = PPgnpragma;
	return((binding)pp);
}

ttype *Rgnbindid(t)
 struct Snbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != nbind)
		fprintf(stderr,"gnbindid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnbindid);
}

ttype *Rgnbindas(t)
 struct Snbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != nbind)
		fprintf(stderr,"gnbindas: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnbindas);
}

long *Rgnline(t)
 struct Snbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != nbind)
		fprintf(stderr,"gnline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnline);
}

hpragma *Rgnpragma(t)
 struct Snbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != nbind)
		fprintf(stderr,"gnpragma: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgnpragma);
}

/************** pbind ******************/

binding mkpbind(PPgpbindl, PPgpline)
 list PPgpbindl;
 long PPgpline;
{
	register struct Spbind *pp =
		(struct Spbind *) malloc(sizeof(struct Spbind));
	pp -> tag = pbind;
	pp -> Xgpbindl = PPgpbindl;
	pp -> Xgpline = PPgpline;
	return((binding)pp);
}

list *Rgpbindl(t)
 struct Spbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != pbind)
		fprintf(stderr,"gpbindl: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgpbindl);
}

long *Rgpline(t)
 struct Spbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != pbind)
		fprintf(stderr,"gpline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgpline);
}

/************** fbind ******************/

binding mkfbind(PPgfbindl, PPgfline)
 list PPgfbindl;
 long PPgfline;
{
	register struct Sfbind *pp =
		(struct Sfbind *) malloc(sizeof(struct Sfbind));
	pp -> tag = fbind;
	pp -> Xgfbindl = PPgfbindl;
	pp -> Xgfline = PPgfline;
	return((binding)pp);
}

list *Rgfbindl(t)
 struct Sfbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != fbind)
		fprintf(stderr,"gfbindl: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgfbindl);
}

long *Rgfline(t)
 struct Sfbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != fbind)
		fprintf(stderr,"gfline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgfline);
}

/************** abind ******************/

binding mkabind(PPgabindfst, PPgabindsnd)
 binding PPgabindfst;
 binding PPgabindsnd;
{
	register struct Sabind *pp =
		(struct Sabind *) malloc(sizeof(struct Sabind));
	pp -> tag = abind;
	pp -> Xgabindfst = PPgabindfst;
	pp -> Xgabindsnd = PPgabindsnd;
	return((binding)pp);
}

binding *Rgabindfst(t)
 struct Sabind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != abind)
		fprintf(stderr,"gabindfst: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgabindfst);
}

binding *Rgabindsnd(t)
 struct Sabind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != abind)
		fprintf(stderr,"gabindsnd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgabindsnd);
}

/************** ibind ******************/

binding mkibind(PPgibindc, PPgibindid, PPgibindi, PPgibindw, PPgiline, PPgipragma)
 list PPgibindc;
 unkId PPgibindid;
 ttype PPgibindi;
 binding PPgibindw;
 long PPgiline;
 hpragma PPgipragma;
{
	register struct Sibind *pp =
		(struct Sibind *) malloc(sizeof(struct Sibind));
	pp -> tag = ibind;
	pp -> Xgibindc = PPgibindc;
	pp -> Xgibindid = PPgibindid;
	pp -> Xgibindi = PPgibindi;
	pp -> Xgibindw = PPgibindw;
	pp -> Xgiline = PPgiline;
	pp -> Xgipragma = PPgipragma;
	return((binding)pp);
}

list *Rgibindc(t)
 struct Sibind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ibind)
		fprintf(stderr,"gibindc: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgibindc);
}

unkId *Rgibindid(t)
 struct Sibind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ibind)
		fprintf(stderr,"gibindid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgibindid);
}

ttype *Rgibindi(t)
 struct Sibind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ibind)
		fprintf(stderr,"gibindi: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgibindi);
}

binding *Rgibindw(t)
 struct Sibind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ibind)
		fprintf(stderr,"gibindw: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgibindw);
}

long *Rgiline(t)
 struct Sibind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ibind)
		fprintf(stderr,"giline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgiline);
}

hpragma *Rgipragma(t)
 struct Sibind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ibind)
		fprintf(stderr,"gipragma: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgipragma);
}

/************** dbind ******************/

binding mkdbind(PPgdbindts, PPgdline)
 list PPgdbindts;
 long PPgdline;
{
	register struct Sdbind *pp =
		(struct Sdbind *) malloc(sizeof(struct Sdbind));
	pp -> tag = dbind;
	pp -> Xgdbindts = PPgdbindts;
	pp -> Xgdline = PPgdline;
	return((binding)pp);
}

list *Rgdbindts(t)
 struct Sdbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != dbind)
		fprintf(stderr,"gdbindts: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdbindts);
}

long *Rgdline(t)
 struct Sdbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != dbind)
		fprintf(stderr,"gdline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdline);
}

/************** cbind ******************/

binding mkcbind(PPgcbindc, PPgcbindid, PPgcbindw, PPgcline, PPgcpragma)
 list PPgcbindc;
 ttype PPgcbindid;
 binding PPgcbindw;
 long PPgcline;
 hpragma PPgcpragma;
{
	register struct Scbind *pp =
		(struct Scbind *) malloc(sizeof(struct Scbind));
	pp -> tag = cbind;
	pp -> Xgcbindc = PPgcbindc;
	pp -> Xgcbindid = PPgcbindid;
	pp -> Xgcbindw = PPgcbindw;
	pp -> Xgcline = PPgcline;
	pp -> Xgcpragma = PPgcpragma;
	return((binding)pp);
}

list *Rgcbindc(t)
 struct Scbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != cbind)
		fprintf(stderr,"gcbindc: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcbindc);
}

ttype *Rgcbindid(t)
 struct Scbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != cbind)
		fprintf(stderr,"gcbindid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcbindid);
}

binding *Rgcbindw(t)
 struct Scbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != cbind)
		fprintf(stderr,"gcbindw: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcbindw);
}

long *Rgcline(t)
 struct Scbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != cbind)
		fprintf(stderr,"gcline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcline);
}

hpragma *Rgcpragma(t)
 struct Scbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != cbind)
		fprintf(stderr,"gcpragma: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcpragma);
}

/************** sbind ******************/

binding mksbind(PPgsbindids, PPgsbindid, PPgsline, PPgspragma)
 list PPgsbindids;
 ttype PPgsbindid;
 long PPgsline;
 hpragma PPgspragma;
{
	register struct Ssbind *pp =
		(struct Ssbind *) malloc(sizeof(struct Ssbind));
	pp -> tag = sbind;
	pp -> Xgsbindids = PPgsbindids;
	pp -> Xgsbindid = PPgsbindid;
	pp -> Xgsline = PPgsline;
	pp -> Xgspragma = PPgspragma;
	return((binding)pp);
}

list *Rgsbindids(t)
 struct Ssbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != sbind)
		fprintf(stderr,"gsbindids: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgsbindids);
}

ttype *Rgsbindid(t)
 struct Ssbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != sbind)
		fprintf(stderr,"gsbindid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgsbindid);
}

long *Rgsline(t)
 struct Ssbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != sbind)
		fprintf(stderr,"gsline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgsline);
}

hpragma *Rgspragma(t)
 struct Ssbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != sbind)
		fprintf(stderr,"gspragma: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgspragma);
}

/************** mbind ******************/

binding mkmbind(PPgmbindmodn, PPgmbindimp, PPgmbindren, PPgmline)
 stringId PPgmbindmodn;
 list PPgmbindimp;
 list PPgmbindren;
 long PPgmline;
{
	register struct Smbind *pp =
		(struct Smbind *) malloc(sizeof(struct Smbind));
	pp -> tag = mbind;
	pp -> Xgmbindmodn = PPgmbindmodn;
	pp -> Xgmbindimp = PPgmbindimp;
	pp -> Xgmbindren = PPgmbindren;
	pp -> Xgmline = PPgmline;
	return((binding)pp);
}

stringId *Rgmbindmodn(t)
 struct Smbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != mbind)
		fprintf(stderr,"gmbindmodn: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgmbindmodn);
}

list *Rgmbindimp(t)
 struct Smbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != mbind)
		fprintf(stderr,"gmbindimp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgmbindimp);
}

list *Rgmbindren(t)
 struct Smbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != mbind)
		fprintf(stderr,"gmbindren: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgmbindren);
}

long *Rgmline(t)
 struct Smbind *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != mbind)
		fprintf(stderr,"gmline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgmline);
}

/************** nullbind ******************/

binding mknullbind(void)
{
	register struct Snullbind *pp =
		(struct Snullbind *) malloc(sizeof(struct Snullbind));
	pp -> tag = nullbind;
	return((binding)pp);
}

/************** import ******************/

binding mkimport(PPgiebindmod, PPgiebindexp, PPgiebindren, PPgiebinddef, PPgiebindfile, PPgiebindline)
 stringId PPgiebindmod;
 list PPgiebindexp;
 list PPgiebindren;
 binding PPgiebinddef;
 stringId PPgiebindfile;
 long PPgiebindline;
{
	register struct Simport *pp =
		(struct Simport *) malloc(sizeof(struct Simport));
	pp -> tag = import;
	pp -> Xgiebindmod = PPgiebindmod;
	pp -> Xgiebindexp = PPgiebindexp;
	pp -> Xgiebindren = PPgiebindren;
	pp -> Xgiebinddef = PPgiebinddef;
	pp -> Xgiebindfile = PPgiebindfile;
	pp -> Xgiebindline = PPgiebindline;
	return((binding)pp);
}

stringId *Rgiebindmod(t)
 struct Simport *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != import)
		fprintf(stderr,"giebindmod: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgiebindmod);
}

list *Rgiebindexp(t)
 struct Simport *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != import)
		fprintf(stderr,"giebindexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgiebindexp);
}

list *Rgiebindren(t)
 struct Simport *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != import)
		fprintf(stderr,"giebindren: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgiebindren);
}

binding *Rgiebinddef(t)
 struct Simport *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != import)
		fprintf(stderr,"giebinddef: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgiebinddef);
}

stringId *Rgiebindfile(t)
 struct Simport *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != import)
		fprintf(stderr,"giebindfile: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgiebindfile);
}

long *Rgiebindline(t)
 struct Simport *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != import)
		fprintf(stderr,"giebindline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgiebindline);
}

/************** hiding ******************/

binding mkhiding(PPgihbindmod, PPgihbindexp, PPgihbindren, PPgihbinddef, PPgihbindfile, PPgihbindline)
 stringId PPgihbindmod;
 list PPgihbindexp;
 list PPgihbindren;
 binding PPgihbinddef;
 stringId PPgihbindfile;
 long PPgihbindline;
{
	register struct Shiding *pp =
		(struct Shiding *) malloc(sizeof(struct Shiding));
	pp -> tag = hiding;
	pp -> Xgihbindmod = PPgihbindmod;
	pp -> Xgihbindexp = PPgihbindexp;
	pp -> Xgihbindren = PPgihbindren;
	pp -> Xgihbinddef = PPgihbinddef;
	pp -> Xgihbindfile = PPgihbindfile;
	pp -> Xgihbindline = PPgihbindline;
	return((binding)pp);
}

stringId *Rgihbindmod(t)
 struct Shiding *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != hiding)
		fprintf(stderr,"gihbindmod: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgihbindmod);
}

list *Rgihbindexp(t)
 struct Shiding *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != hiding)
		fprintf(stderr,"gihbindexp: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgihbindexp);
}

list *Rgihbindren(t)
 struct Shiding *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != hiding)
		fprintf(stderr,"gihbindren: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgihbindren);
}

binding *Rgihbinddef(t)
 struct Shiding *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != hiding)
		fprintf(stderr,"gihbinddef: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgihbinddef);
}

stringId *Rgihbindfile(t)
 struct Shiding *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != hiding)
		fprintf(stderr,"gihbindfile: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgihbindfile);
}

long *Rgihbindline(t)
 struct Shiding *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != hiding)
		fprintf(stderr,"gihbindline: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgihbindline);
}

/************** vspec_uprag ******************/

binding mkvspec_uprag(PPgvspec_id, PPgvspec_tys, PPgvspec_line)
 unkId PPgvspec_id;
 list PPgvspec_tys;
 long PPgvspec_line;
{
	register struct Svspec_uprag *pp =
		(struct Svspec_uprag *) malloc(sizeof(struct Svspec_uprag));
	pp -> tag = vspec_uprag;
	pp -> Xgvspec_id = PPgvspec_id;
	pp -> Xgvspec_tys = PPgvspec_tys;
	pp -> Xgvspec_line = PPgvspec_line;
	return((binding)pp);
}

unkId *Rgvspec_id(t)
 struct Svspec_uprag *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != vspec_uprag)
		fprintf(stderr,"gvspec_id: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgvspec_id);
}

list *Rgvspec_tys(t)
 struct Svspec_uprag *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != vspec_uprag)
		fprintf(stderr,"gvspec_tys: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgvspec_tys);
}

long *Rgvspec_line(t)
 struct Svspec_uprag *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != vspec_uprag)
		fprintf(stderr,"gvspec_line: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgvspec_line);
}

/************** vspec_ty_and_id ******************/

binding mkvspec_ty_and_id(PPgvspec_ty, PPgvspec_tyid)
 ttype PPgvspec_ty;
 list PPgvspec_tyid;
{
	register struct Svspec_ty_and_id *pp =
		(struct Svspec_ty_and_id *) malloc(sizeof(struct Svspec_ty_and_id));
	pp -> tag = vspec_ty_and_id;
	pp -> Xgvspec_ty = PPgvspec_ty;
	pp -> Xgvspec_tyid = PPgvspec_tyid;
	return((binding)pp);
}

ttype *Rgvspec_ty(t)
 struct Svspec_ty_and_id *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != vspec_ty_and_id)
		fprintf(stderr,"gvspec_ty: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgvspec_ty);
}

list *Rgvspec_tyid(t)
 struct Svspec_ty_and_id *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != vspec_ty_and_id)
		fprintf(stderr,"gvspec_tyid: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgvspec_tyid);
}

/************** ispec_uprag ******************/

binding mkispec_uprag(PPgispec_clas, PPgispec_ty, PPgispec_line)
 unkId PPgispec_clas;
 ttype PPgispec_ty;
 long PPgispec_line;
{
	register struct Sispec_uprag *pp =
		(struct Sispec_uprag *) malloc(sizeof(struct Sispec_uprag));
	pp -> tag = ispec_uprag;
	pp -> Xgispec_clas = PPgispec_clas;
	pp -> Xgispec_ty = PPgispec_ty;
	pp -> Xgispec_line = PPgispec_line;
	return((binding)pp);
}

unkId *Rgispec_clas(t)
 struct Sispec_uprag *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ispec_uprag)
		fprintf(stderr,"gispec_clas: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgispec_clas);
}

ttype *Rgispec_ty(t)
 struct Sispec_uprag *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ispec_uprag)
		fprintf(stderr,"gispec_ty: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgispec_ty);
}

long *Rgispec_line(t)
 struct Sispec_uprag *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != ispec_uprag)
		fprintf(stderr,"gispec_line: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgispec_line);
}

/************** inline_uprag ******************/

binding mkinline_uprag(PPginline_id, PPginline_howto, PPginline_line)
 unkId PPginline_id;
 list PPginline_howto;
 long PPginline_line;
{
	register struct Sinline_uprag *pp =
		(struct Sinline_uprag *) malloc(sizeof(struct Sinline_uprag));
	pp -> tag = inline_uprag;
	pp -> Xginline_id = PPginline_id;
	pp -> Xginline_howto = PPginline_howto;
	pp -> Xginline_line = PPginline_line;
	return((binding)pp);
}

unkId *Rginline_id(t)
 struct Sinline_uprag *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != inline_uprag)
		fprintf(stderr,"ginline_id: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xginline_id);
}

list *Rginline_howto(t)
 struct Sinline_uprag *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != inline_uprag)
		fprintf(stderr,"ginline_howto: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xginline_howto);
}

long *Rginline_line(t)
 struct Sinline_uprag *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != inline_uprag)
		fprintf(stderr,"ginline_line: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xginline_line);
}

/************** deforest_uprag ******************/

binding mkdeforest_uprag(PPgdeforest_id, PPgdeforest_line)
 unkId PPgdeforest_id;
 long PPgdeforest_line;
{
	register struct Sdeforest_uprag *pp =
		(struct Sdeforest_uprag *) malloc(sizeof(struct Sdeforest_uprag));
	pp -> tag = deforest_uprag;
	pp -> Xgdeforest_id = PPgdeforest_id;
	pp -> Xgdeforest_line = PPgdeforest_line;
	return((binding)pp);
}

unkId *Rgdeforest_id(t)
 struct Sdeforest_uprag *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != deforest_uprag)
		fprintf(stderr,"gdeforest_id: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdeforest_id);
}

long *Rgdeforest_line(t)
 struct Sdeforest_uprag *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != deforest_uprag)
		fprintf(stderr,"gdeforest_line: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdeforest_line);
}

/************** magicuf_uprag ******************/

binding mkmagicuf_uprag(PPgmagicuf_id, PPgmagicuf_str, PPgmagicuf_line)
 unkId PPgmagicuf_id;
 stringId PPgmagicuf_str;
 long PPgmagicuf_line;
{
	register struct Smagicuf_uprag *pp =
		(struct Smagicuf_uprag *) malloc(sizeof(struct Smagicuf_uprag));
	pp -> tag = magicuf_uprag;
	pp -> Xgmagicuf_id = PPgmagicuf_id;
	pp -> Xgmagicuf_str = PPgmagicuf_str;
	pp -> Xgmagicuf_line = PPgmagicuf_line;
	return((binding)pp);
}

unkId *Rgmagicuf_id(t)
 struct Smagicuf_uprag *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != magicuf_uprag)
		fprintf(stderr,"gmagicuf_id: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgmagicuf_id);
}

stringId *Rgmagicuf_str(t)
 struct Smagicuf_uprag *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != magicuf_uprag)
		fprintf(stderr,"gmagicuf_str: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgmagicuf_str);
}

long *Rgmagicuf_line(t)
 struct Smagicuf_uprag *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != magicuf_uprag)
		fprintf(stderr,"gmagicuf_line: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgmagicuf_line);
}

/************** abstract_uprag ******************/

binding mkabstract_uprag(PPgabstract_id, PPgabstract_line)
 unkId PPgabstract_id;
 long PPgabstract_line;
{
	register struct Sabstract_uprag *pp =
		(struct Sabstract_uprag *) malloc(sizeof(struct Sabstract_uprag));
	pp -> tag = abstract_uprag;
	pp -> Xgabstract_id = PPgabstract_id;
	pp -> Xgabstract_line = PPgabstract_line;
	return((binding)pp);
}

unkId *Rgabstract_id(t)
 struct Sabstract_uprag *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != abstract_uprag)
		fprintf(stderr,"gabstract_id: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgabstract_id);
}

long *Rgabstract_line(t)
 struct Sabstract_uprag *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != abstract_uprag)
		fprintf(stderr,"gabstract_line: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgabstract_line);
}

/************** dspec_uprag ******************/

binding mkdspec_uprag(PPgdspec_id, PPgdspec_tys, PPgdspec_line)
 unkId PPgdspec_id;
 list PPgdspec_tys;
 long PPgdspec_line;
{
	register struct Sdspec_uprag *pp =
		(struct Sdspec_uprag *) malloc(sizeof(struct Sdspec_uprag));
	pp -> tag = dspec_uprag;
	pp -> Xgdspec_id = PPgdspec_id;
	pp -> Xgdspec_tys = PPgdspec_tys;
	pp -> Xgdspec_line = PPgdspec_line;
	return((binding)pp);
}

unkId *Rgdspec_id(t)
 struct Sdspec_uprag *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != dspec_uprag)
		fprintf(stderr,"gdspec_id: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdspec_id);
}

list *Rgdspec_tys(t)
 struct Sdspec_uprag *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != dspec_uprag)
		fprintf(stderr,"gdspec_tys: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdspec_tys);
}

long *Rgdspec_line(t)
 struct Sdspec_uprag *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != dspec_uprag)
		fprintf(stderr,"gdspec_line: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgdspec_line);
}
