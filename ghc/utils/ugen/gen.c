#ifdef __STDC__
#define PROTO(x)	x
#else
#define PROTO(x)	()
#endif

#include <stdio.h>
#include "id.h"
#include "tree.h"
#include "funs.h"
extern FILE *fh, *fc, *fhs;

void
ge_typdef(t)
    tree t;
{
	/*
	** Generate to the .h file:
	**
	** 	typdef enum {
	**		constructor1,
	**		constructor2,
	**		...
	**	} *Ttypename;
	*/
	fprintf(fh, "#ifndef %s_defined\n", gtid(t));
	fprintf(fh, "#define %s_defined\n", gtid(t));
	fprintf(fh, "\n#include <stdio.h>\n"); /* for stderr */
	fprintf(fh, "\n#ifndef PROTO\n");
	fprintf(fh, "#ifdef __STDC__\n");
	fprintf(fh, "#define PROTO(x) x\n");
	fprintf(fh, "#else\n");
	fprintf(fh, "#define PROTO(x) /**/\n");
	fprintf(fh, "#endif\n");
	fprintf(fh, "#endif\n\n");
	fprintf(fh, "#ifdef UGEN_DEBUG\n");
	fprintf(fh, "int\tfprintf PROTO((FILE *, const char *, ...));\n");
	fprintf(fh, "#endif /* UGEN_DEBUG */\n\n");
	fprintf(fh, "typedef enum {\n");
	ge_typlist(gtdeflist(t));
	fprintf(fh, "\n} T%s;\n\n", gtid(t));
	/*
	** Generate to the .hs file:
	**
	** 	data U_typename
	**        = U_constructor1 | U_constructor2 | ...
	*/
	/*
	** Generate to the .h file:
	**
	**	typedef struct { Ttypename tag; } *typename;
	*/
	fprintf(fh, "typedef struct { T%s tag; } *%s;\n\n", gtid(t), gtid(t));

	g_tagfun(gtid(t)); /* generate the tag-grabbing function */

	/* Generate the struct definitions (to the .h file). */
	gs_typlist(gtdeflist(t), gtid(t));

	/* Generate a Haskell-equiv data type (to the .hs file) */
	fprintf(fhs, "data U_%s = ", gtid(t));
	hs_typlist(gtdeflist(t));
	fprintf(fhs, "\n\n");
	/* And a type with which to talk about the C-land parse tree */
/*	fprintf(fhs, "data U__%s = U__%s Addr#\n", gtid(t), gtid(t));
	fprintf(fhs, "instance _CCallable U__%s\n", gtid(t));
	fprintf(fhs, "instance _CReturnable U__%s\n\n", gtid(t));
*/
}

void
ge_typlist(t)
    tree t;
{
	switch(ttree(t)) {
	  case deflist:
		ge_typlist(gdeflist(t));
		fprintf(fh, ",\n\t%s", gdid(gdef(t)));
		break;
	  case def:
		fprintf(fh, "\t%s", gdid(t));
		break;
	  default:
		fprintf(stderr,"ge_typlist: funny abstract syntax.\n");
		break;
	}
}

void
gs_typlist(t, tid)
    tree t;
    id tid;
{
	switch(ttree(t)) {
	  case deflist:
		gs_typlist(gdeflist(t), tid);
		gs_def(gdef(t), tid);
		break;
	  case def:
		gs_def(t, tid);
		break;
	  default:
		fprintf(stderr,"gs_typlist: funny abstract syntax.\n");
		break;
	}
}

void
hs_typlist(t)
    tree t;
{
	switch(ttree(t)) {
	  case deflist:
		hs_typlist(gdeflist(t));
		fprintf(fhs, "| ");
		hs_def(gdef(t));
		break;
	  case def:
		hs_def(t);
		break;
	  default:
		fprintf(stderr,"hs_typlist: funny abstract syntax.\n");
		break;
	}
}

void
gs_def(t, tid)
   tree t;
   id tid;
{
	fprintf(fh, "struct S%s {\n", gdid(t));
	fprintf(fh, "\tT%s tag;\n", tid);
	gs_itemlist(gditemlist(t));
	fprintf(fh, "};\n\n");
}

void
hs_def(t)
   tree t;
{
	fprintf(fhs, "U_%s ", gdid(t));
	hs_itemlist(gditemlist(t));
}

void
gs_itemlist(t)
    tree t;
{
	switch(ttree(t)) {
	  case emitemlist:
		break;
	  case itemlist:
		gs_itemlist(gitemlist(t));
		fprintf(fh, "\t%s X%s;\n",
			gitemtypid(gitem(t)), gitemfunid(gitem(t)) );
		break;
	  case item:
		fprintf(fh, "\t%s X%s;\n", 
			gitemtypid(t), gitemfunid(t));
		break;
	  default:
		fprintf(stderr,"gs_itemlist: funny abs. syntax: %d\n.", ttree(t));
		break;
	}
}

void
hs_itemlist(t)
    tree t;
{
	switch(ttree(t)) {
	  case emitemlist:
		break;
	  case itemlist:
		hs_itemlist(gitemlist(t));
		fprintf(fhs, "U_%s ", gitemtypid(gitem(t)));
		break;
	  case item:
		fprintf(fhs, "U_%s ", gitemtypid(t));
		break;
	  default:
		fprintf(stderr,"hs_itemlist: funny abs. syntax: %d\n.", ttree(t));
		break;
	}
}

void
g_tagfun(typid)
    id typid;
{
    fprintf(fh, "#ifdef __GNUC__\n");

    /* to satisfy GCC when in really-picky mode: */
    fprintf(fh, "T%s t%s(%s t);\n", typid, typid, typid);
    /* the real thing: */
    fprintf(fh, "extern __inline__ T%s t%s(%s t)\n{\n\treturn(t -> tag);\n}\n",
		typid, typid, typid);

    fprintf(fh, "#else  /* ! __GNUC__ */\n");

    fprintf(fh, "extern T%s t%s PROTO((%s));\n", typid, typid, typid);
    fprintf(fc, "\nT%s t%s(t)\n %s t;\n{\n\treturn(t -> tag);\n}\n\n",
		typid, typid, typid);

    fprintf(fh, "#endif /* ! __GNUC__ */\n\n");
}
/*******************************************************************/

void
g_consels(t, typid)
    tree t;
    id typid;
{
	switch(ttree(t)) {
	  case deflist:
		g_consels(gdeflist(t), typid);
		g_typconsel(gdef(t), typid);
		break;
	  case def:
		g_typconsel(t, typid);
		break;
	  default:
		fprintf(stderr,"g_consel: funny abstract syntax.\n");
		break;
	}
}

/***********************************************************************/

void
g_typconsel(t, typid)
    tree t;
    id typid;
{
	fprintf(fc, "\n/************** %s ******************/\n\n", gdid(t));
	gencons(typid, t);
	gensels(typid, gdid(t), gditemlist(t));
	fprintf(fh, "\n");
}

void
gencons(typid, t)
  id typid;
  tree t; /* of kind 'def'. */
{
	tree itemlist = gditemlist(t);

	fprintf(fh, "extern %s mk%s PROTO((", typid, gdid(t));
	switch (ttree(itemlist)) {
	  case emitemlist: /* empty list */
	    fprintf(fh, "void");
	    break;
	  default:
	    genmkprotodekl(itemlist);
	    break;
	}
	fprintf(fh, "));\n");

	fprintf(fc, "%s mk%s(", typid, gdid(t));
	switch (ttree(itemlist)) {
	  case emitemlist: /* empty list */
	    fprintf(fc, "void");
	    break;
	  default:
	    genmkparamlist(itemlist);
	    break;
	}
	fprintf(fc, ")\n");

	genmkparamdekl(itemlist);

	fprintf(fc, "{\n\tregister struct S%s *pp =\n", gdid(t));
	fprintf(fc, "\t\t(struct S%s *) malloc(sizeof(struct S%s));\n",
		    gdid(t), gdid(t));
	fprintf(fc, "\tpp -> tag = %s;\n", gdid(t));
	genmkfillin(itemlist);
	fprintf(fc, "\treturn((%s)pp);\n", typid);
	fprintf(fc, "}\n");
}

void
genmkparamlist(t)
   tree t;
{
	switch(ttree(t)) {
	  case emitemlist:
		break;
	  case itemlist:
		genmkparamlist(gitemlist(t));
		fprintf(fc, ", ");
		genmkparamlist(gitem(t));
		break;
	  case item:
		fprintf(fc, "PP%s", gitemfunid(t));
		break;
	  default:
		fprintf(stderr,"genparamlist: funny abs syntax.\n");
		break;
	}
}

void
genmkparamdekl(t)
   tree t; /* of kind 'itemlist' or 'item' */
{
	switch(ttree(t)) {
	  case emitemlist:
		break;
	  case itemlist:
		genmkparamdekl(gitemlist(t));
		genmkparamdekl(gitem(t));
		break;
	  case item:
		fprintf(fc, " %s PP%s;\n", gitemtypid(t), gitemfunid(t));
		break;
	  default:
		fprintf(stderr,"genmkparamdekl: funny abs syntax.\n");
		break;
	}
}

void
genmkprotodekl(t)
   tree t; /* of kind 'itemlist' or 'item' */
{
	switch(ttree(t)) {
	  case emitemlist:
		break;
	  case itemlist:
		genmkprotodekl(gitemlist(t));
		fprintf(fh, ", ");
		genmkprotodekl(gitem(t));
		break;
	  case item:
		fprintf(fh, "%s", gitemtypid(t));
		break;
	  default:
		fprintf(stderr,"genmkprotodekl: funny abs syntax.\n");
		break;
	}
}

void
genmkfillin(t)
    tree t;
{
	switch(ttree(t)) {
	  case emitemlist:
		break;
	  case itemlist:
		genmkfillin(gitemlist(t));
		genmkfillin(gitem(t));
		break;
	  case item:
		fprintf(fc, "\tpp -> X%s = PP%s;\n", 
			gitemfunid(t), gitemfunid(t));
		break;
	  default:
		fprintf(stderr,"genmkfillin: funny abs syntax.\n");
		break;
	}
}

void
gensels(typid, variantid, t)
    id typid;
    id variantid;
    tree t;
{
	switch(ttree(t)) {
	  case emitemlist:
		break;
	  case itemlist:
		gensels(typid, variantid, gitemlist(t));
		gensels(typid, variantid, gitem(t));
		break;
	  case item:
		fprintf(fh, "#ifdef __GNUC__\n");

		/* to satisfy GCC when in extremely-picky mode: */
		fprintf(fh, "\n%s *R%s PROTO((struct S%s *));\n", 
			     gitemtypid(t), gitemfunid(t), variantid);
		/* the real thing: */
		fprintf(fh, "\nextern __inline__ %s *R%s(struct S%s *t)\n{\n", 
			     gitemtypid(t), gitemfunid(t), variantid);
		fprintf(fh, "#ifdef UGEN_DEBUG\n");
		fprintf(fh, "\tif(t -> tag != %s)\n", variantid);
		fprintf(fh, "\t\tfprintf(stderr,\"%s: illegal selection; was %%d\\n\", t -> tag);\n", gitemfunid(t));
		fprintf(fh, "#endif /* UGEN_DEBUG */\n");
		fprintf(fh, "\treturn(& t -> X%s);\n}\n", gitemfunid(t));

		fprintf(fh, "#else  /* ! __GNUC__ */\n");

		fprintf(fh,
		  "extern %s *R%s PROTO((struct S%s *));\n",
		  gitemtypid(t), gitemfunid(t), variantid);

		fprintf(fc, "\n%s *R%s(t)\n struct S%s *t;\n{\n", 
			     gitemtypid(t), gitemfunid(t), variantid);
		fprintf(fc, "#ifdef UGEN_DEBUG\n");
		fprintf(fc, "\tif(t -> tag != %s)\n", variantid);
		fprintf(fc, "\t\tfprintf(stderr,\"%s: illegal selection; was %%d\\n\", t -> tag);\n", gitemfunid(t));
		fprintf(fc, "#endif /* UGEN_DEBUG */\n");
		fprintf(fc, "\treturn(& t -> X%s);\n}\n", gitemfunid(t));

		fprintf(fh, "#endif /* ! __GNUC__ */\n\n");

		fprintf(fh,
		  "#define %s(xyzxyz) (*R%s((struct S%s *) (xyzxyz)))\n",
		  gitemfunid(t), gitemfunid(t), variantid);
		break;
	  default:
		fprintf(stderr,"gensels: funny abs syntax.\n");
		break;
	}

}

/***********************************************************************/

void
gen_hs_reader(typid, deflist)
    id typid;
    tree deflist;
{
	/* signature */
	fprintf(fhs, "rdU_%s :: _Addr -> UgnM U_%s\n", typid, typid);

	/* defn */
	fprintf(fhs, "rdU_%s t\n  = ioToUgnM (_ccall_ t%s t) `thenUgn` \\ tag@(I# _) ->\n", typid, typid);
	fprintf(fhs, "    if ");
	gen_hs_rdalts(typid, deflist);
	fprintf(fhs, "    else\n\terror (\"rdU_%s: bad tag selection:\"++show tag++\"\\n\")\n", typid);
}

void
gen_hs_rdalts(typid, t)
    id   typid;
    tree t;
{
	switch(ttree(t)) {
	  case deflist:
		gen_hs_rdalts(typid, gdeflist(t));
		fprintf(fhs, "    else if ");
		gen_hs_rdalt (typid, gdef(t));
		break;
	  case def:
		gen_hs_rdalt(typid, t);
		break;
	  default:
		fprintf(stderr,"gen_hs_rdalts: funny abstract syntax.\n");
		break;
	}
}

void
gen_hs_rdalt(typid, t)
    id   typid;
    tree t;
{
	fprintf(fhs, "tag == ``%s'' then\n", gdid(t));
	gen_hs_rdcomponents (typid, gdid(t), gditemlist(t));
	fprintf(fhs, "\treturnUgn (U_%s ", gdid(t));
	gen_hs_retcomponents(typid, gdid(t), gditemlist(t));
	fprintf(fhs, ")\n"); /* end of alt */
}

void
gen_hs_rdcomponents(typid, variantid, t)
    id   typid;
    id   variantid;
    tree t;
{
	switch(ttree(t)) {
	  case emitemlist:
		break;
	  case itemlist:
		gen_hs_rdcomponents(typid, variantid, gitemlist(t));
		gen_hs_rdcomponents(typid, variantid, gitem(t));
		break;
	  case item:
		fprintf(fhs, "\tioToUgnM (_ccall_ %s t) `thenUgn` \\ x_%s ->\n",
			     gitemfunid(t), gitemfunid(t));

		fprintf(fhs, "\trdU_%s x_%s `thenUgn` \\ y_%s ->\n",
			     gitemtypid(t), gitemfunid(t), gitemfunid(t));

/*		fprintf(fhs, "\tif(t -> tag != %s)\n", variantid);
		fprintf(fhs, "\t\tfprintf(stderr,\"%s: illegal selection; was %%d\\n\", t -> tag);\n", gitemfunid(t));
		fprintf(fhs, "\treturn(& t -> X%s);\n}\n", gitemfunid(t));
*/		break;

	  default:
		fprintf(stderr,"gen_hs_rdcomponents: funny abs syntax.\n");
		break;
	}
}

void
gen_hs_retcomponents(typid, variantid, t)
    id   typid;
    id   variantid;
    tree t;
{
	switch(ttree(t)) {
	  case emitemlist:
		break;
	  case itemlist:
		gen_hs_retcomponents(typid, variantid, gitemlist(t));
		fprintf(fhs, " ");
		gen_hs_retcomponents(typid, variantid, gitem(t));
		break;
	  case item:
		fprintf(fhs, "y_%s", gitemfunid(t));
		break;

	  default:
		fprintf(stderr,"gen_hs_retcomponents: funny abs syntax.\n");
		break;
	}
}
