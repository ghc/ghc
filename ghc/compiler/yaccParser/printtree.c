/**********************************************************************
*                                                                     *
*                                                                     *
*      Syntax Tree Printing Routines                                  *
*                                                                     *
*                                                                     *
**********************************************************************/


#define	COMPACT	TRUE	/* No spaces in output -- #undef this for debugging */


#include <stdio.h>

#include "hspincl.h"
#include "constants.h"
#include "utils.h"

/* fwd decls, necessary and otherwise */
static void ptree   PROTO( (tree) );
static void plist   PROTO( (void (*)(/*NOT WORTH IT: void * */), list) );
static void pid	    PROTO( (id) );
static void pstr    PROTO( (char *) );
static void pbool   PROTO( (BOOLEAN) );
static void prbind  PROTO( (binding) );
static void pttype  PROTO( (ttype) );
static void patype  PROTO( (atype) );
static void pentid  PROTO( (entidt) );
static void prename PROTO( (list) );
static void pfixes  PROTO( (void) );
static void ppbinding PROTO((pbinding));
static void pgrhses PROTO( (list) );
static void ppragma PROTO( (hpragma) );
static void pcoresyn PROTO((coresyn));

extern char *fixop   PROTO((int));
extern char *fixtype PROTO((int));

extern char *input_filename;
extern BOOLEAN hashIds;

/*	How to print tags	*/

#if COMPACT
#define	PUTTAG(c)	putchar(c);
#define PUTTAGSTR(s)	printf("%s",(s));
#else
#define	PUTTAG(c)	putchar(c); \
  			putchar(' ');
#define PUTTAGSTR(s)	printf("%s",(s)); \
			putchar(' ');
#endif


/* 	Performs a post order walk of the tree
	to print it.
*/

void
pprogram(t)
tree t;
{
  print_hash_table();
  ptree(t);
}

/* print_string: we must escape \t and \\, as described in
   char/string lexer comments.  (WDP 94/11)
*/
static void
print_string(hstring str)
{
    char *gs;
    char c;
    int i, str_length;

    putchar('#');
    str_length = str->len;
    gs = str->bytes;

    for (i = 0; i < str_length; i++) {
	c = gs[i];
	if ( c == '\t' ) {
	    putchar('\\');
	    putchar('t');
	} else if ( c == '\\' ) {
	    putchar('\\');
	    putchar('\\');
	} else {
	    putchar(gs[i]);
	}
    }
    putchar('\t');
}

static int
get_character(hstring str)
{
    int c = (int)((str->bytes)[0]);

    if (str->len != 1) { /* ToDo: assert */
	fprintf(stderr, "get_character: length != 1? (%ld: %s)\n", str->len, str->bytes);
    }

    if (c < 0) {
	c += 256; 	/* "This is not a hack" -- KH */
    }

    return(c);
}

static void
pliteral(literal t)
{
    switch(tliteral(t)) {
      case integer:
		      PUTTAG('4');
		      pstr(ginteger(t));
		      break;
      case intprim:
		      PUTTAG('H');
		      pstr(gintprim(t));
		      break;
      case floatr:
		      PUTTAG('F');
		      pstr(gfloatr(t));
		      break;
      case doubleprim:
		      PUTTAG('J');
		      pstr(gdoubleprim(t));
		      break;
      case floatprim:
		      PUTTAG('K');
		      pstr(gfloatprim(t));
		      break;
      case charr:
		      PUTTAG('C');
		      /* Changed %d to %u, since negative chars
			 make little sense -- KH @ 16/4/91
		      */
		      printf("#%u\t", get_character(gchar(t)));
		      break;
      case charprim:
		      PUTTAG('P');
		      printf("#%u\t", get_character(gcharprim(t)));
		      break;
      case string:
		      PUTTAG('S');
		      print_string(gstring(t));
		      break;
      case stringprim:
		      PUTTAG('V');
		      print_string(gstringprim(t));
		      break;
      case clitlit:
		      PUTTAG('Y');
		      pstr(gclitlit(t));
		      pstr(gclitlit_kind(t));
		      break;

      case norepi:
		      PUTTAG('I');
		      pstr(gnorepi(t));
		      break;
      case norepr:
		      PUTTAG('R');
		      pstr(gnorepr_n(t));
		      pstr(gnorepr_d(t));
		      break;
      case noreps:
		      PUTTAG('s');
		      print_string(gnoreps(t));
		      break;
      default:
		      error("Bad pliteral");
    }
}

static void
ptree(t)
   tree t;
{
again:
    switch(ttree(t)) {
      case par:		t = gpare(t); goto again;
      case hmodule:
		      PUTTAG('M');
		      printf("#%lu\t",ghmodline(t));
		      pid(ghname(t));
		      pstr(input_filename);
		      prbind(ghmodlist(t));
		      pfixes();
		      plist(prbind, ghimplist(t));
		      plist(pentid, ghexplist(t));
		      break;
      case ident: 
		      PUTTAG('i');
		      pid(gident(t));
		      break;
      case lit:
		      PUTTAG('C');
		      pliteral(glit(t));
		      break;

      case ap: 
		      PUTTAG('a');
		      ptree(gfun(t)); 
		      ptree(garg(t)); 
		      break;
      case lsection:
		      PUTTAG('(');
		      ptree(glsexp(t)); 
		      pid(glsop(t)); 
		      break;
      case rsection:
		      PUTTAG(')');
		      pid(grsop(t)); 
		      ptree(grsexp(t)); 
		      break;
      case tinfixop: 
		      PUTTAG('@');
		      ptree(ginarg1((struct Sap *)t)); 
		      pid(gident(ginfun((struct Sap *)t))); 
		      ptree(ginarg2((struct Sap *)t)); 
		      break;

      case lambda: 
		      PUTTAG('l');
		      printf("#%lu\t",glamline(t));
		      plist(ptree,glampats(t));
		      ptree(glamexpr(t));
		      break;

      case let: 
		      PUTTAG('E');
		      prbind(gletvdeflist(t));
		      ptree(gletvexpr(t));
		      break;
      case casee:
		      PUTTAG('c');
		      ptree(gcaseexpr(t));
		      plist(ppbinding, gcasebody(t));
		      break;
      case ife:
		      PUTTAG('b');
		      ptree(gifpred(t));
		      ptree(gifthen(t));
		      ptree(gifelse(t));
		      break;
      case tuple:
		      PUTTAG(',');
		      plist(ptree,gtuplelist(t));
		      break;
      case eenum:
		      PUTTAG('.');
		      ptree(gefrom(t));
		      plist(ptree,gestep(t));
		      plist(ptree,geto(t));
		      break;
      case llist:
		      PUTTAG(':');
		      plist(ptree,gllist(t));
		      break;
      case negate:
		      PUTTAG('-');
		      ptree(gnexp(t));
		      break;
      case comprh:
		      PUTTAG('Z');
		      ptree(gcexp(t));
		      plist(ptree,gcquals(t));
		      break;
      case qual:
		      PUTTAG('G');
		      ptree(gqpat(t));
		      ptree(gqexp(t));
		      break;
      case guard:
		      PUTTAG('g');
		      ptree(ggexp(t));
		      break;
      case def:
		      PUTTAG('=');
		      ptree(ggdef(t)); /* was: prbind (WDP 94/10) */
		      break;
      case as:
		      PUTTAG('s');
		      pid(gasid(t));
		      ptree(gase(t));
		      break;
      case lazyp:
		      PUTTAG('~');
		      ptree(glazyp(t));
		      break;
      case plusp:
		      PUTTAG('+');
		      ptree(gplusp(t));
		      pliteral(gplusi(t));
		      break;
      case wildp:
		      PUTTAG('_');
		      break;
      case restr:
		      PUTTAG('R');
		      ptree(grestre(t));
		      pttype(grestrt(t));
		      break;
      case ccall:
		      PUTTAG('j');
		      pstr(gccid(t));
		      pstr(gccinfo(t));
		      plist(ptree,gccargs(t));
		      break;
      case scc:
		      PUTTAG('k');
		      print_string(gsccid(t));
		      ptree(gsccexp(t));
		      break;
#ifdef DPH
      case parzf:
		      PUTTAG('5');
		      ptree(gpzfexp(t));
		      plist(ptree,gpzfqual(t));
		      break;
      case pod:
		      PUTTAG('6');
		      plist(ptree,gpod(t));
		      break;
      case proc:
		      PUTTAG('O');
		      plist(ptree,gprocid(t));
		      ptree(gprocdata(t));
		      break;
      case pardgen:
		      PUTTAG('0');
		      ptree(gdproc(t));
		      ptree(gdexp(t));
		      break;	
      case parigen:
		      PUTTAG('w');
		      ptree(giproc(t));
		      ptree(giexp(t));
		      break;	
      case parfilt:
		      PUTTAG('I');
		      ptree(gpfilt(t));
		      break;	
#endif /* DPH */

      default:
		      error("Bad ptree");
    }
}

static void
plist(fun, l)
  void (*fun)(/* NOT WORTH IT: void * */);
  list l;
{
	if (tlist(l) == lcons) {
		PUTTAG('L');
		(*fun)(lhd(l));
		plist(fun, ltl(l));
	} else  {
		PUTTAG('N');
	}
}

static void
pid(i)
  id i;
{
  if(hashIds)
	printf("!%lu\t", hash_index(i));
  else
	printf("#%s\t", id_to_string(i));
}

static void
pstr(i)
  char *i;
{
	printf("#%s\t", i);
}

static void
prbind(b)
  binding b;
{
	switch(tbinding(b)) {
	case tbind: 
			  PUTTAG('t');
			  printf("#%lu\t",gtline(b));
			  plist(pttype, gtbindc(b));
			  plist(pid, gtbindd(b));
			  pttype(gtbindid(b));
			  plist(patype, gtbindl(b));
			  ppragma(gtpragma(b));
			  break;
	case nbind	: 
			  PUTTAG('n');
			  printf("#%lu\t",gnline(b));
			  pttype(gnbindid(b));
			  pttype(gnbindas(b));
			  ppragma(gnpragma(b));
			  break;
	case pbind	: 
			  PUTTAG('p');
			  printf("#%lu\t",gpline(b));
			  plist(ppbinding, gpbindl(b));
			  break;
	case fbind	: 
			  PUTTAG('f');
			  printf("#%lu\t",gfline(b));
			  plist(ppbinding, gfbindl(b));
			  break;
	case abind	: 
			  PUTTAG('A');
			  prbind(gabindfst(b));
			  prbind(gabindsnd(b));
			  break;
	case cbind	:
			  PUTTAG('$');
			  printf("#%lu\t",gcline(b));
			  plist(pttype,gcbindc(b));
			  pttype(gcbindid(b));
			  prbind(gcbindw(b));
			  ppragma(gcpragma(b));
			  break;
	case ibind	:
			  PUTTAG('%');
			  printf("#%lu\t",giline(b));
			  plist(pttype,gibindc(b));
			  pid(gibindid(b));
			  pttype(gibindi(b));
			  prbind(gibindw(b));
			  ppragma(gipragma(b));
			  break;
	case dbind	:
			  PUTTAG('D');
			  printf("#%lu\t",gdline(b));
			  plist(pttype,gdbindts(b));
			  break;

	/* signature(-like) things, including user pragmas */
	case sbind	:
			  PUTTAGSTR("St");
			  printf("#%lu\t",gsline(b));
			  plist(pid,gsbindids(b));
			  pttype(gsbindid(b));
			  ppragma(gspragma(b));
			  break;

	case vspec_uprag:
			  PUTTAGSTR("Ss");
			  printf("#%lu\t",gvspec_line(b));
			  pid(gvspec_id(b));
			  plist(pttype,gvspec_tys(b));
			  break;
	case ispec_uprag:
			  PUTTAGSTR("SS");
			  printf("#%lu\t",gispec_line(b));
			  pid(gispec_clas(b));
			  pttype(gispec_ty(b));
			  break;
	case inline_uprag:
			  PUTTAGSTR("Si");
			  printf("#%lu\t",ginline_line(b));
			  pid(ginline_id(b));
			  plist(pid,ginline_howto(b));
			  break;
	case deforest_uprag:
			  PUTTAGSTR("Sd");
			  printf("#%lu\t",gdeforest_line(b));
			  pid(gdeforest_id(b));
			  break;
	case magicuf_uprag:
			  PUTTAGSTR("Su");
			  printf("#%lu\t",gmagicuf_line(b));
			  pid(gmagicuf_id(b));
			  pid(gmagicuf_str(b));
			  break;
	case abstract_uprag:
			  PUTTAGSTR("Sa");
			  printf("#%lu\t",gabstract_line(b));
			  pid(gabstract_id(b));
			  break;
	case dspec_uprag:
			  PUTTAGSTR("Sd");
			  printf("#%lu\t",gdspec_line(b));
			  pid(gdspec_id(b));
			  plist(pttype,gdspec_tys(b));
			  break;

	/* end of signature(-like) things */

	case mbind:	  
			  PUTTAG('7');
			  printf("#%lu\t",gmline(b));
			  pid(gmbindmodn(b));
			  plist(pentid,gmbindimp(b));
			  plist(prename,gmbindren(b));
			  break;
	case import:	  
			  PUTTAG('e');
			  printf("#%lu\t",giebindline(b));
			  pstr(giebindfile(b));
			  pid(giebindmod(b));
			  plist(pentid,giebindexp(b));
			  plist(prename,giebindren(b));
			  prbind(giebinddef(b));
			  break;
	case hiding:	  
			  PUTTAG('h');
			  printf("#%lu\t",gihbindline(b));
			  pstr(gihbindfile(b));
			  pid(gihbindmod(b));
			  plist(pentid,gihbindexp(b));
			  plist(prename,gihbindren(b));
			  prbind(gihbinddef(b));
			  break;
	case nullbind	:
			  PUTTAG('B');
			  break;
	default		: error("Bad prbind");
			  break;
	}
}

static void
pttype(t)
  ttype t;
{
	switch (tttype(t)) {
	case tname	: PUTTAG('T');
			  pid(gtypeid(t));
			  plist(pttype, gtypel(t));
			  break;
	case namedtvar	: PUTTAG('y');
			  pid(gnamedtvar(t));
			  break;
	case tllist	: PUTTAG(':');
			  pttype(gtlist(t));
			  break;
	case ttuple	: PUTTAG(',');
			  plist(pttype,gttuple(t));
			  break;
	case tfun	: PUTTAG('>');
			  pttype(gtfun(t));
			  pttype(gtarg(t));
			  break;
	case context	: PUTTAG('3');
			  plist(pttype,gtcontextl(t));
			  pttype(gtcontextt(t));
			  break;

	case unidict	: PUTTAGSTR("2A");
			  pid(gunidict_clas(t));
			  pttype(gunidict_ty(t));
			  break;
	case unityvartemplate : PUTTAGSTR("2B");
			  pid(gunityvartemplate(t));
			  break;
	case uniforall	: PUTTAGSTR("2C");
			  plist(pid,guniforall_tv(t));
			  pttype(guniforall_ty(t));
			  break;

	case ty_maybe_nothing : PUTTAGSTR("2D");
			  break;
	case ty_maybe_just: PUTTAGSTR("2E");
			  pttype(gty_maybe(t));
			  break;

#ifdef DPH
	case tproc	:
			  PUTTAG('u');
			  plist(pttype,gtpid(t));
			  pttype(gtdata(t));
			  break;
	case tpod	:
			  PUTTAG('v');
			  pttype(gtpod(t));
			  break;
#endif
	default		: error("bad pttype");
	}
}

static void
patype(a)
  atype a;
{
	switch (tatype(a)) {
	case atc	: 
			  PUTTAG('1');
			  printf("#%lu\t",gatcline(a));
			  pid(gatcid(a));
			  plist(pttype, gatctypel(a));
			  break;
	default		: fprintf(stderr, "Bad tag in abstree %d\n", tatype(a));
			  exit(1);
	}
}


static void
pentid(i)
  entidt i;
{
	switch (tentidt(i)) {
	case entid	: PUTTAG('x');
			  pid(gentid(i));
			  break;
	case enttype	: PUTTAG('X');
			  pid(gitentid(i));
			  break;
	case enttypeall	: PUTTAG('z');
			  pid(gatentid(i));
			  break;
	case entmod	: PUTTAG('m');
			  pid(gmentid(i));
			  break;
	case enttypecons: PUTTAG('8');
			  pid(gctentid(i));
			  plist(pid,gctentcons(i));
			  break;
	case entclass	: PUTTAG('9');
			  pid(gcentid(i));
			  plist(pid,gcentops(i));
			  break;
	default	        :
			  error("Bad pentid");
	}
}


static void
prename(l)
  list l;
{
  pid(lhd(l));
  pid(lhd(ltl(l)));
}


static void
pfixes()
{
	int m = nfixes(), i;
	char *s;

	for(i = 0; i < m; i++) {
		s = fixtype(i);
		if (s) {
			PUTTAG('L');
			pstr(fixop(i));
			pstr(fixtype(i));
			printf("#%lu\t",precedence(i));
		}
	}
	PUTTAG('N');
}


static void
ppbinding(p)
  pbinding p;
{
	switch(tpbinding(p)) {
	case pgrhs	: PUTTAG('W');
  			  printf("#%lu\t",ggline(p));
	  		  pid(ggfuncname(p));
			  ptree(ggpat(p));
			  plist(pgrhses,ggdexprs(p));
	  		  prbind(ggbind(p));
	  		  break;
	default	        :
			  error("Bad pbinding");
	}
}


static void
pgrhses(l)
  list l;
{
  ptree(lhd(l));		/* Guard */
  ptree(lhd(ltl(l)));		/* Expression */
}

static void
ppragma(p)
  hpragma p;
{
    switch(thpragma(p)) {
      case no_pragma:		PUTTAGSTR("PN");
				break;
      case idata_pragma:	PUTTAGSTR("Pd");
				plist(patype, gprag_data_constrs(p));
				plist(ppragma, gprag_data_specs(p));
				break;
      case itype_pragma:	PUTTAGSTR("Pt");
				break;
      case iclas_pragma:	PUTTAGSTR("Pc");
				plist(ppragma, gprag_clas(p));
				break;
      case iclasop_pragma:	PUTTAGSTR("Po");
				ppragma(gprag_dsel(p));
				ppragma(gprag_defm(p));
				break;

      case iinst_simpl_pragma:	PUTTAGSTR("Pis");
				pid(gprag_imod_simpl(p));
				ppragma(gprag_dfun_simpl(p));
				break;
      case iinst_const_pragma:	PUTTAGSTR("Pic");
				pid(gprag_imod_const(p));
				ppragma(gprag_dfun_const(p));
				plist(ppragma, gprag_constms(p));
				break;

      case igen_pragma:		PUTTAGSTR("Pg");
				ppragma(gprag_arity(p));
				ppragma(gprag_update(p));
				ppragma(gprag_deforest(p));
				ppragma(gprag_strictness(p));
				ppragma(gprag_unfolding(p));
				plist(ppragma, gprag_specs(p));
				break;
      case iarity_pragma:	PUTTAGSTR("PA");
				pid(gprag_arity_val(p));
				break;
      case iupdate_pragma:	PUTTAGSTR("Pu");
				pid(gprag_update_val(p));
				break;
      case ideforest_pragma:	PUTTAGSTR("PD");
				break;
      case istrictness_pragma:	PUTTAGSTR("PS");
				print_string(gprag_strict_spec(p));
				ppragma(gprag_strict_wrkr(p));
				break;
      case imagic_unfolding_pragma: PUTTAGSTR("PM");
				pid(gprag_magic_str(p));
				break;

      case iunfolding_pragma:	PUTTAGSTR("PU");
				ppragma(gprag_unfold_guide(p));
				pcoresyn(gprag_unfold_core(p));
				break;

      case iunfold_always:	PUTTAGSTR("Px");
				break;
      case iunfold_if_args:	PUTTAGSTR("Py");
				pid(gprag_unfold_if_t_args(p));
				pid(gprag_unfold_if_v_args(p));
				pid(gprag_unfold_if_con_args(p));
				pid(gprag_unfold_if_size(p));
				break;

      case iname_pragma_pr:	PUTTAGSTR("P1");
				pid(gprag_name_pr1(p));
				ppragma(gprag_name_pr2(p));
				break;
      case itype_pragma_pr:	PUTTAGSTR("P2");
				plist(pttype, gprag_type_pr1(p));
				pid(gprag_type_pr2(p));
				ppragma(gprag_type_pr3(p));
				break;

      case idata_pragma_4s:	PUTTAGSTR("P4");
				plist(pttype, gprag_data_spec(p));
	                        break;

      default:           	error("Bad Pragma");
      }
}

static void
pbool(b)
  BOOLEAN b;
{
    if (b) {
      putchar('T');
    } else {
      putchar('F');
    }
}

static void
pcoresyn(p)
  coresyn p;
{
    switch(tcoresyn(p)) {
      case cobinder:		PUTTAGSTR("Fa");
			    	pid(gcobinder_v(p));
				pttype(gcobinder_ty(p));
				break;

      case colit:		PUTTAGSTR("Fb");
				pliteral(gcolit(p));
				break;
      case colocal:		PUTTAGSTR("Fc");
				pcoresyn(gcolocal_v(p));
				break;

      case cononrec:		PUTTAGSTR("Fd");
				pcoresyn(gcononrec_b(p));
				pcoresyn(gcononrec_rhs(p));
				break;
      case corec:		PUTTAGSTR("Fe");
				plist(pcoresyn,gcorec(p));
				break;
      case corec_pair:		PUTTAGSTR("Ff");
				pcoresyn(gcorec_b(p));
				pcoresyn(gcorec_rhs(p));
				break;		

      case covar:		PUTTAGSTR("Fg");
				pcoresyn(gcovar(p));
				break;
      case coliteral:		PUTTAGSTR("Fh");
				pliteral(gcoliteral(p));
				break;
      case cocon:		PUTTAGSTR("Fi");
				pcoresyn(gcocon_con(p));
				plist(pttype, gcocon_tys(p));
				plist(pcoresyn, gcocon_args(p));
				break;
      case coprim:		PUTTAGSTR("Fj");
				pcoresyn(gcoprim_op(p));
				plist(pttype, gcoprim_tys(p));
				plist(pcoresyn, gcoprim_args(p));
				break;
      case colam:		PUTTAGSTR("Fk");
				plist(pcoresyn, gcolam_vars(p));
				pcoresyn(gcolam_body(p));
				break;
      case cotylam:		PUTTAGSTR("Fl");
				plist(pid, gcotylam_tvs(p));
				pcoresyn(gcotylam_body(p));
				break;
      case coapp:		PUTTAGSTR("Fm");
				pcoresyn(gcoapp_fun(p));
				plist(pcoresyn, gcoapp_args(p));
				break;
      case cotyapp:		PUTTAGSTR("Fn");
				pcoresyn(gcotyapp_e(p));
				pttype(gcotyapp_t(p));
				break;
      case cocase:		PUTTAGSTR("Fo");
				pcoresyn(gcocase_s(p));
				pcoresyn(gcocase_alts(p));
				break;
      case colet:		PUTTAGSTR("Fp");
				pcoresyn(gcolet_bind(p));
				pcoresyn(gcolet_body(p));
				break;
      case coscc:		PUTTAGSTR("Fz");	/* out of order! */
				pcoresyn(gcoscc_scc(p));
				pcoresyn(gcoscc_body(p));
				break;

      case coalg_alts:		PUTTAGSTR("Fq");
				plist(pcoresyn, gcoalg_alts(p));
				pcoresyn(gcoalg_deflt(p));
				break;
      case coalg_alt:		PUTTAGSTR("Fr");
				pcoresyn(gcoalg_con(p));
				plist(pcoresyn, gcoalg_bs(p));
				pcoresyn(gcoalg_rhs(p));
				break;
      case coprim_alts:		PUTTAGSTR("Fs");
				plist(pcoresyn, gcoprim_alts(p));
				pcoresyn(gcoprim_deflt(p));
				break;
      case coprim_alt:		PUTTAGSTR("Ft");
				pliteral(gcoprim_lit(p));
				pcoresyn(gcoprim_rhs(p));
				break;
      case conodeflt:		PUTTAGSTR("Fu");
				break;
      case cobinddeflt:		PUTTAGSTR("Fv");
				pcoresyn(gcobinddeflt_v(p));
				pcoresyn(gcobinddeflt_rhs(p));
				break;

      case co_primop:		PUTTAGSTR("Fw");
				pid(gco_primop(p));
				break;
      case co_ccall:		PUTTAGSTR("Fx");
	                        pbool(gco_ccall_may_gc(p));
				pid(gco_ccall(p));
				plist(pttype, gco_ccall_arg_tys(p));
				pttype(gco_ccall_res_ty(p));
				break;
      case co_casm:		PUTTAGSTR("Fy");
	                        pbool(gco_casm_may_gc(p));
				pliteral(gco_casm(p));
				plist(pttype, gco_casm_arg_tys(p));
				pttype(gco_casm_res_ty(p));
				break;

	/* Cost-centre stuff */
      case co_preludedictscc:	PUTTAGSTR("F?a");
				pcoresyn(gco_preludedictscc_dupd(p));
				break;
      case co_alldictscc:	PUTTAGSTR("F?b");
				print_string(gco_alldictscc_m(p));
				print_string(gco_alldictscc_g(p));
				pcoresyn(gco_alldictscc_dupd(p));
				break;
      case co_usercc:		PUTTAGSTR("F?c");
				print_string(gco_usercc_n(p));
				print_string(gco_usercc_m(p));
				print_string(gco_usercc_g(p));
				pcoresyn(gco_usercc_dupd(p));
				pcoresyn(gco_usercc_cafd(p));
				break;
      case co_autocc:		PUTTAGSTR("F?d");
				pcoresyn(gco_autocc_i(p));
				print_string(gco_autocc_m(p));
				print_string(gco_autocc_g(p));
				pcoresyn(gco_autocc_dupd(p));
				pcoresyn(gco_autocc_cafd(p));
				break;
      case co_dictcc:		PUTTAGSTR("F?e");
				pcoresyn(gco_dictcc_i(p));
				print_string(gco_dictcc_m(p));
				print_string(gco_dictcc_g(p));
				pcoresyn(gco_dictcc_dupd(p));
				pcoresyn(gco_dictcc_cafd(p));
				break;

      case co_scc_noncaf:	PUTTAGSTR("F?f");
				break;
      case co_scc_caf:		PUTTAGSTR("F?g");
				break;
      case co_scc_nondupd:	PUTTAGSTR("F?h");
				break;
      case co_scc_dupd:		PUTTAGSTR("F?i");
				break;

	/* Id stuff */
      case co_id:		PUTTAGSTR("F1");
				pid(gco_id(p));
				break;
      case co_orig_id:		PUTTAGSTR("F9");
				pid(gco_orig_id_m(p));
				pid(gco_orig_id_n(p));
				break;
      case co_sdselid:		PUTTAGSTR("F2");
				pid(gco_sdselid_c(p));
				pid(gco_sdselid_sc(p));
				break;
      case co_classopid:	PUTTAGSTR("F3");
				pid(gco_classopid_c(p));
				pid(gco_classopid_o(p));
				break;
      case co_defmid:		PUTTAGSTR("F4");
				pid(gco_defmid_c(p));
				pid(gco_defmid_op(p));
				break;
      case co_dfunid:		PUTTAGSTR("F5");
				pid(gco_dfunid_c(p));
				pttype(gco_dfunid_ty(p));
				break;
      case co_constmid:		PUTTAGSTR("F6");
				pid(gco_constmid_c(p));
				pid(gco_constmid_op(p));
				pttype(gco_constmid_ty(p));
				break;
      case co_specid:		PUTTAGSTR("F7");
				pcoresyn(gco_specid_un(p));
				plist(pttype,gco_specid_tys(p));
				break;
      case co_wrkrid:		PUTTAGSTR("F8");
				pcoresyn(gco_wrkrid_un(p));
				break;
      /* more to come?? */

      default :		    	error("Bad Core syntax");
    }
}
