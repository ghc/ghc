/**************************************************************************
*   File:               hsparser.y                                        *
*                                                                         *
*                       Author:                 Maria M. Gutierrez        *
*                       Modified by:            Kevin Hammond             *
*                       Last date revised:      December 13 1991. KH.     *
*                       Modification:           Haskell 1.1 Syntax.       *
*                                                                         *
*                                                                         *
*   Description:  This file contains the LALR(1) grammar for Haskell.     *
*                                                                         *
*   Entry Point:  module                                                  *
*                                                                         *
*   Problems:     None known.                                             *
*                                                                         *
*                                                                         *
*                 LALR(1) Syntax for Haskell 1.2                          *
*                                                                         *
**************************************************************************/


%{
#ifdef HSP_DEBUG
# define YYDEBUG 1
#endif

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "hspincl.h"
#include "constants.h"
#include "utils.h"

/**********************************************************************
*                                                                     *
*                                                                     *
*     Imported Variables and Functions                                *
*                                                                     *
*                                                                     *
**********************************************************************/

static BOOLEAN expect_ccurly = FALSE; /* Used to signal that a CCURLY could be inserted here */

extern BOOLEAN nonstandardFlag;
extern BOOLEAN etags;

extern VOID find_module_on_imports_dirlist PROTO((char *, BOOLEAN, char *));

extern char *input_filename;
static char *the_module_name;
static char *iface_name;
static char iface_filename[FILENAME_SIZE];

static maybe module_exports;		/* Exported entities */
static list prelude_core_import, prelude_imports;
					/* Entities imported from the Prelude */

extern tree niltree;
extern list Lnil;

extern tree root;

/* For FN, PREVPATT and SAMEFN macros */
extern qid	fns[];
extern BOOLEAN	samefn[];
extern tree	prevpatt[];
extern short	icontexts;

/* Line Numbers */
extern int hsplineno, hspcolno;
extern int modulelineno;
extern int startlineno;
extern int endlineno;

/**********************************************************************
*                                                                     *
*                                                                     *
*      Fixity and Precedence Declarations                             *
*                                                                     *
*                                                                     *
**********************************************************************/

/* OLD 95/08: list fixlist; */
static int Fixity = 0, Precedence = 0;
struct infix;

char *ineg PROTO((char *));

int importlineno = 0;		/* The line number where an import starts */

long	inimport; 		/* Info about current import */
id 	importmod;
long 	importas;
id	asmod;
long 	importqual;
long 	importspec;
long 	importhide;
list 	importlist;

extern BOOLEAN inpat;			/*  True when parsing a pattern */
extern BOOLEAN implicitPrelude;		/*  True when we should read the Prelude if not given */
extern BOOLEAN haskell1_2Flag;		/*  True if we are attempting (proto)Haskell 1.3 */

extern int thisIfacePragmaVersion;
%}

%union {
	tree utree;
	list ulist;
	ttype uttype;
	constr uconstr;
	binding ubinding;
	pbinding upbinding;
	entidt uentid;
	id uid;
	qid uqid;
	literal uliteral;
        maybe umaybe;
        either ueither;
	long ulong;
	float ufloat;
	char *ustring;
	hstring uhstring;
	hpragma uhpragma;
	coresyn ucoresyn;
}


/**********************************************************************
*                                                                     *
*                                                                     *
*     These are lexemes.                                              *
*                                                                     *
*                                                                     *
**********************************************************************/


%token 	VARID 		CONID		QVARID		QCONID
	VARSYM		CONSYM		QVARSYM		QCONSYM

%token 	INTEGER 	FLOAT 		CHAR 		STRING
	CHARPRIM	STRINGPRIM	INTPRIM		FLOATPRIM
	DOUBLEPRIM	CLITLIT



/**********************************************************************
*                                                                     *
*                                                                     *
*      Special Symbols                                                *
*                                                                     *
*                                                                     *
**********************************************************************/

%token	OCURLY		CCURLY		VCCURLY		SEMI
%token	OBRACK		CBRACK		OPAREN		CPAREN
%token	COMMA		BQUOTE


/**********************************************************************
*                                                                     *
*                                                                     *
*     Reserved Operators                                              *
*                                                                     *
*                                                                     *
**********************************************************************/

%token	DOTDOT		DCOLON		EQUAL
%token	LAMBDA		VBAR		RARROW
%token 	LARROW		MINUS


/**********************************************************************
*                                                                     *
*                                                                     *
*     Reserved Identifiers                                            *
*                                                                     *
*                                                                     *
**********************************************************************/

%token  CASE		CLASS		DATA
%token	DEFAULT		DERIVING	DO
%token  ELSE		IF		IMPORT
%token	IN		INFIX		INFIXL
%token  INFIXR		INSTANCE	LET
%token	MODULE		NEWTYPE		OF
%token	THEN		TYPE		WHERE

%token  INTERFACE	SCC
%token	CCALL		CCALL_GC	CASM		CASM_GC


/**********************************************************************
*                                                                     *
*                                                                     *
*     Valid symbols/identifiers which need to be recognised           *
*                                                                     *
*                                                                     *
**********************************************************************/

%token	WILDCARD	AT		LAZY		BANG
%token 	AS		HIDING		QUALIFIED


/**********************************************************************
*                                                                     *
*                                                                     *
*     Special Symbols for the Lexer                                   *
*                                                                     *
*                                                                     *
**********************************************************************/

%token	LEOF
%token  GHC_PRAGMA END_PRAGMA NO_PRAGMA NOINFO_PRAGMA SPECIALISE_PRAGMA
%token  ARITY_PRAGMA UPDATE_PRAGMA STRICTNESS_PRAGMA KIND_PRAGMA
%token  UNFOLDING_PRAGMA MAGIC_UNFOLDING_PRAGMA DEFOREST_PRAGMA
%token  SPECIALISE_UPRAGMA INLINE_UPRAGMA MAGIC_UNFOLDING_UPRAGMA
%token  DEFOREST_UPRAGMA END_UPRAGMA
%token  TYLAMBDA COCON COPRIM COAPP COTYAPP FORALL TYVAR_TEMPLATE_ID
%token  CO_ALG_ALTS CO_PRIM_ALTS CO_NO_DEFAULT CO_LETREC
%token  CO_SDSEL_ID CO_METH_ID CO_DEFM_ID CO_DFUN_ID CO_CONSTM_ID
%token  CO_SPEC_ID CO_WRKR_ID CO_ORIG_NM
%token  UNFOLD_ALWAYS UNFOLD_IF_ARGS
%token  NOREP_INTEGER NOREP_RATIONAL NOREP_STRING
%token  CO_PRELUDE_DICTS_CC CO_ALL_DICTS_CC CO_USER_CC CO_AUTO_CC CO_DICT_CC
%token  CO_CAF_CC CO_DUPD_CC

/**********************************************************************
*                                                                     *
*                                                                     *
*     Precedences of the various tokens                               *
*                                                                     *
*                                                                     *
**********************************************************************/


%left	CASE	LET	IN
  	IF	ELSE	LAMBDA
	SCC	CASM	CCALL	CASM_GC	CCALL_GC

%left	VARSYM	CONSYM	QVARSYM	QCONSYM
	MINUS	BQUOTE	BANG	DARROW

%left	DCOLON

%left	SEMI	COMMA

%left	OCURLY	OBRACK	OPAREN

%left 	EQUAL

%right	RARROW

/**********************************************************************
*                                                                     *
*                                                                     *
*      Type Declarations                                              *
*                                                                     *
*                                                                     *
**********************************************************************/


%type <ulist>   caserest alts alt quals
		dorest stmts stmt
		rbinds rpats list_exps 
		qvarsk qvars_list
		constrs constr1 fields 
		types atypes batypes
		types_and_maybe_ids
  		pats context context_list tyvar_list
		export_list enames
  		import_list inames
 		impdecls maybeimpdecls impdecl
		maybefixes fixes fix ops
		dtyclses dtycls_list
  		gdrhs gdpat valrhs
  		lampats	cexps
		idata_pragma_specs idata_pragma_specslist
		gen_pragma_list type_pragma_pairs
		type_pragma_pairs_maybe name_pragma_pairs
		type_maybes
		core_binders core_tyvars core_tv_templates
		core_types core_type_list
		core_atoms core_atom_list
		core_alg_alts core_prim_alts corec_binds
		core_type_maybes

%type <umaybe>  maybeexports impas maybeimpspec
		type_maybe core_type_maybe


%type <ueither> impspec  

%type <uliteral> lit_constant

%type <utree>	exp oexp dexp kexp fexp aexp rbind texps
		expL oexpL kexpL expLno oexpLno dexpLno kexpLno
		qual gd leftexp
 		apat bpat pat apatc conpat dpat fpat opat aapat
 		dpatk fpatk opatk aapatk rpat


%type <uid>	MINUS DARROW AS LAZY
		VARID CONID VARSYM CONSYM 
		TYVAR_TEMPLATE_ID
  		var con varop conop op
		vark varid varsym varsym_nominus
	        tycon modid impmod ccallid

%type <uqid>	QVARID QCONID QVARSYM QCONSYM 
		qvarid qconid qvarsym qconsym
		qvar qcon qvarop qconop qop
		qvark qconk qtycon qtycls
		gcon gconk gtycon qop1 qvarop1 
		ename iname 

%type <ubinding>  topdecl topdecls letdecls
		  typed datad newtd classd instd defaultd
		  decl decls valdef instdef instdefs
  		  maybeifixes iimport iimports maybeiimports
		  ityped idatad inewtd iclassd iinstd ivarsd
  		  itopdecl itopdecls
  		  maybe_where
  		  interface dointerface readinterface ibody
		  cbody rinst
		  type_and_maybe_id

%type <upbinding> valrhs1 altrest

%type <uttype>    simple ctype type atype btype
		  gtyconapp ntyconapp ntycon gtyconvars
		  bbtype batype btyconapp
		  class restrict_inst general_inst tyvar
		  core_type

%type <uconstr>	  constr field

%type <ustring>   FLOAT INTEGER INTPRIM
		  FLOATPRIM DOUBLEPRIM CLITLIT

%type <uhstring>  STRING STRINGPRIM CHAR CHARPRIM

%type <uentid>	  export import

%type <uhpragma>  idata_pragma inewt_pragma idata_pragma_spectypes
		  iclas_pragma iclasop_pragma
		  iinst_pragma gen_pragma ival_pragma arity_pragma
		  update_pragma strictness_pragma worker_info
		  deforest_pragma
		  unfolding_pragma unfolding_guidance type_pragma_pair
		  name_pragma_pair

%type <ucoresyn>  core_expr core_case_alts core_id core_binder core_atom
		  core_alg_alt core_prim_alt core_default corec_bind
		  co_primop co_scc co_caf co_dupd

%type <ulong>     commas impqual

/**********************************************************************
*                                                                     *
*                                                                     *
*      Start Symbol for the Parser                                    *
*                                                                     *
*                                                                     *
**********************************************************************/

%start pmodule


%%

pmodule	:  	{
		  inimport   = 1;
		  importmod  = install_literal("Prelude");
		  importas   = 0;
		  asmod      = NULL;
		  importqual = 0;
		  importspec = 0;
		  importhide = 0;
		  importlist = Lnil;
		}
	   readpreludecore readprelude
		{
		  inimport   = 0;
		  importmod  = NULL;

	  	  modulelineno = 0;
		}
	   module
	;

module	:  modulekey modid maybeexports
		{
		  the_module_name = $2;
		  module_exports = $3;
		}
	   WHERE body
	|	{ 
		  the_module_name = install_literal("Main");
		  module_exports = mknothing();
                }
	   body
	;

body	:  ocurly { setstartlineno(); } orestm
	|  vocurly vrestm
	;

orestm  :  maybeimpdecls maybefixes topdecls ccurly
	       {
		 root = mkhmodule(the_module_name,lconc(prelude_imports,$1),module_exports,$2,$3,modulelineno);
	       }
	|  impdecls ccurly
	       {
		 root = mkhmodule(the_module_name,lconc(prelude_imports,$1),module_exports,Lnil,mknullbind(),modulelineno);
	       }

vrestm  :  maybeimpdecls maybefixes topdecls vccurly
	       {
		 root = mkhmodule(the_module_name,lconc(prelude_imports,$1),module_exports,$2,$3,modulelineno);
	       }
	|  impdecls vccurly
	       {
		 root = mkhmodule(the_module_name,lconc(prelude_imports,$1),module_exports,Lnil,mknullbind(),modulelineno);
	       }


maybeexports :	/* empty */			{ $$ = mknothing(); }
	|  OPAREN export_list CPAREN		{ $$ = mkjust($2); }
	|  OPAREN export_list COMMA CPAREN	{ $$ = mkjust($2); }
	;

export_list:
	   export				{ $$ = lsing($1); }
  	|  export_list COMMA export		{ $$ = lapp($1, $3); }
	;

export	:  qvar					{ $$ = mkentid($1); }
	|  gtycon				{ $$ = mkenttype($1); }
	|  gtycon OPAREN DOTDOT CPAREN		{ $$ = mkenttypeall($1); }
	|  gtycon OPAREN CPAREN		        { $$ = mkenttypenamed($1,Lnil); }
	|  gtycon OPAREN enames CPAREN		{ $$ = mkenttypenamed($1,$3); }
	|  MODULE modid				{ $$ = mkentmod($2); }
	;

enames  :  ename				{ $$ = lsing($1); }
	|  enames COMMA ename			{ $$ = lapp($1,$3); }
	;
ename   :  qvar
	|  qcon
	;


maybeimpdecls : /* empty */			{ $$ = Lnil; }
	|  impdecls SEMI			{ $$ = $1; }
	;

impdecls:  impdecl				{ $$ = $1; }
	|  impdecls SEMI impdecl		{ $$ = lconc($1,$3); }
	;


impdecl	:  importkey
		{ 
		  inimport = 1;
		  importlineno = startlineno;
		}
	   impqual impmod dointerface impas maybeimpspec
		{ 
		  $$ = lsing(mkimport(iface_name,xstrdup(iface_filename),$5,
				      $4,$3,$6,$7,importlineno));
		  inimport   = 0;
		  importmod  = NULL;	
		  importas   = 0;
		  asmod      = NULL;
		  importqual = 0;
		  importspec = 0;
		  importhide = 0;
		  importlist = Lnil;
	        }
	;

impmod  : modid					{ $$ = importmod = $1; }
	;

impqual :  /* noqual */				{ $$ = importqual = 0; }
	|  QUALIFIED 				{ $$ = importqual = 1; }
	;

impas   :  /* noas */				{ $$ = mknothing(); importas = 0; asmod = NULL; }
	|  AS modid				{ $$ = mkjust($2);  importas = 1; asmod = $2;   }
	;

maybeimpspec :	/* empty */			{ $$ = mknothing(); importspec = 0; }
	|  impspec				{ $$ = mkjust($1);  importspec = 1; }
	;

impspec	:  OPAREN CPAREN			  { $$ = mkleft(Lnil); importhide = 0; importlist = Lnil; }
	|  OPAREN import_list CPAREN		  { $$ = mkleft($2);   importhide = 0; importlist = $2; }
	|  OPAREN import_list COMMA CPAREN	  { $$ = mkleft($2);   importhide = 0; importlist = $2; }
	|  HIDING OPAREN import_list CPAREN	  { $$ = mkright($3);  importhide = 1; importlist = $3; }
	|  HIDING OPAREN import_list COMMA CPAREN { $$ = mkright($3);  importhide = 1; importlist = $3; }
  	;

import_list:
	   import				{ $$ = lsing($1); }
	|  import_list COMMA import		{ $$ = lapp($1, $3); }
	;

import	:  var					{ $$ = mkentid(mknoqual($1)); }
	|  tycon				{ $$ = mkenttype(mknoqual($1)); }
	|  tycon OPAREN DOTDOT CPAREN		{ $$ = mkenttypeall(mknoqual($1)); }
	|  tycon OPAREN CPAREN			{ $$ = mkenttypenamed(mknoqual($1),Lnil); }
	|  tycon OPAREN inames CPAREN		{ $$ = mkenttypenamed(mknoqual($1),$3); }
	;

inames  :  iname				{ $$ = lsing($1); }
	|  inames COMMA iname			{ $$ = lapp($1,$3); }
	;
iname   :  var					{ $$ = mknoqual($1); }
	|  con					{ $$ = mknoqual($1); }
	;


/**********************************************************************
*                                                                     *
*                                                                     *
*      Reading interface files					      *
*                                                                     *
*                                                                     *
**********************************************************************/

dointerface :	{ /* filename returned in "iface_filename" */
		  char *module_name = id_to_string(importmod);
		  if ( ! etags ) {
		      find_module_on_imports_dirlist(
			(haskell1_2Flag && strcmp(module_name, "Prelude") == 0)
			    ? "Prel12" : module_name,
			FALSE, iface_filename);
		  } else {
		     find_module_on_imports_dirlist("PreludeNull_",TRUE,iface_filename);
		  }
		  if (strcmp(module_name,"PreludeCore")==0) {
			    hsperror("Cannot explicitly import `PreludeCore'");

		  } else if (strcmp(module_name,"Prelude")==0) {
		    prelude_imports = prelude_core_import; /* unavoidable */
		  }
		  thisIfacePragmaVersion = 0;
		  setyyin(iface_filename);
		}
	readinterface
		{ $$ = $2; }
	;

readpreludecore:{
		  if ( implicitPrelude && !etags ) {
		     /* we try to avoid reading interfaces when etagging */
		     find_module_on_imports_dirlist(
			(haskell1_2Flag) ? "PrelCore12" : "PreludeCore",
			TRUE,iface_filename);
		  } else {
		     find_module_on_imports_dirlist("PreludeNull_",TRUE,iface_filename);
		  }
		  thisIfacePragmaVersion = 0;
		  setyyin(iface_filename);
		}
	   readinterface
		{
		  binding prelude_core = mkimport(iface_name,xstrdup(iface_filename),$2,
				                  install_literal("PreludeCore"),
						  0,mknothing(),mknothing(),0);
		  prelude_core_import = (! implicitPrelude) ? Lnil : lsing(prelude_core);
		}
	;

readprelude :   {
		  if ( implicitPrelude && !etags ) {
		     find_module_on_imports_dirlist(
			( haskell1_2Flag ) ? "Prel12" : "Prelude",
			TRUE,iface_filename);
		  } else {
		     find_module_on_imports_dirlist("PreludeNull_",TRUE,iface_filename);
		  }
		  thisIfacePragmaVersion = 0;
		  setyyin(iface_filename);
		}
	   readinterface
		{
		  binding prelude = mkimport(iface_name,xstrdup(iface_filename),$2,
				             install_literal("Prelude"),
					     0,mknothing(),mknothing(),0);
		  prelude_imports = (! implicitPrelude) ? Lnil
					: lconc(prelude_core_import,lsing(prelude));
		}
	;

readinterface:
	   interface LEOF
		{
		  $$ = $1;
		}
	;

interface:
  	   INTERFACE modid
		{ 
		  iface_name = $2;
		}
	   WHERE ibody
		{
		  $$ = $5;
		}
	;

ibody	:  ocurly maybeiimports maybeifixes itopdecls ccurly
		{
		  $$ = mkabind($2,mkabind($3,$4));
		}
	|  ocurly iimports ccurly
		{
		  $$ = $2;
		}
	|  vocurly maybeiimports maybeifixes itopdecls vccurly
		{
		  $$ = mkabind($2,mkabind($3,$4));
		}
	|  vocurly iimports vccurly
		{
		  $$ = $2;
		}
  	;

maybeifixes:  /* empty */			{ $$ = mknullbind(); }
	|  fixes SEMI				{ $$ = mkmfbind($1); }
	;

maybeiimports : /* empty */			{ $$ = mknullbind(); }
	|  iimports SEMI			{ $$ = $1; }
	;

iimports : iimport				{ $$ = $1; }
	 | iimports SEMI iimport		{ $$ = mkabind($1,$3); }
	 ;

iimport :  importkey modid OPAREN import_list CPAREN
		{ $$ = mkmbind($2,$4,startlineno); }
	;


itopdecls : itopdecl				{ $$ = $1; }
	| itopdecls SEMI itopdecl		{ $$ = mkabind($1,$3); }
  	;

itopdecl:  ityped	 			{ $$ = $1; }
	|  idatad 				{ $$ = $1; }
	|  inewtd 				{ $$ = $1; }
	|  iclassd 				{ $$ = $1; }
	|  iinstd 				{ $$ = $1; }
	|  ivarsd				{ $$ = $1; }
	|  /* empty */				{ $$ = mknullbind(); }
	;

ivarsd	:  qvarsk DCOLON ctype ival_pragma
		{ $$ = mksbind($1,$3,startlineno,$4); }
	;

ityped	:  typekey simple EQUAL type
		{ $$ = mknbind($2,$4,startlineno); }
	;

idatad	:  datakey simple idata_pragma
		{ $$ = mktbind(Lnil,$2,Lnil,mknothing(),startlineno,$3); }
	|  datakey simple EQUAL constrs idata_pragma
		{ $$ = mktbind(Lnil,$2,$4,mknothing(),startlineno,$5); }
	|  datakey context DARROW simple idata_pragma
		{ $$ = mktbind($2,$4,Lnil,mknothing(),startlineno,$5); }
	|  datakey context DARROW simple EQUAL constrs idata_pragma
		{ $$ = mktbind($2,$4,$6,mknothing(),startlineno,$7); }
	;

inewtd	:  newtypekey simple inewt_pragma
		{ $$ = mkntbind(Lnil,$2,Lnil,mknothing(),startlineno,$3); }
	|  newtypekey simple EQUAL constr1 inewt_pragma
		{ $$ = mkntbind(Lnil,$2,$4,mknothing(),startlineno,$5); }
	|  newtypekey context DARROW simple inewt_pragma
		{ $$ = mkntbind($2,$4,Lnil,mknothing(),startlineno,$5); }
	|  newtypekey context DARROW simple EQUAL constr1 inewt_pragma
		{ $$ = mkntbind($2,$4,$6,mknothing(),startlineno,$7); }
	;

iclassd	:  classkey context DARROW class iclas_pragma cbody
		{ $$ = mkcbind($2,$4,$6,startlineno,$5); }
	|  classkey class iclas_pragma cbody
		{ $$ = mkcbind(Lnil,$2,$4,startlineno,$3); }
	;

iinstd	:  instkey modid context DARROW gtycon general_inst iinst_pragma
		{ $$ = mkibind(0/*not source*/,$2,$3,$5,$6,mknullbind(),startlineno,$7); }
	|  instkey modid gtycon general_inst iinst_pragma
		{ $$ = mkibind(0/*not source*/,$2,Lnil,$3,$4,mknullbind(),startlineno,$5); }
	;


/**********************************************************************
*                                                                     *
*                                                                     *
*     Interface pragma stuff					      *
*                                                                     *
*                                                                     *
**********************************************************************/

idata_pragma:
	   GHC_PRAGMA constrs idata_pragma_specs END_PRAGMA
						{ $$ = mkidata_pragma($2, $3); }
	|  GHC_PRAGMA idata_pragma_specs END_PRAGMA
						{ $$ = mkidata_pragma(Lnil, $2); }
	|  /* empty */			    	{ $$ = mkno_pragma(); }
	;

inewt_pragma:
	   GHC_PRAGMA constr1 idata_pragma_specs END_PRAGMA
						{ $$ = mkidata_pragma($2, $3); }
	|  GHC_PRAGMA idata_pragma_specs END_PRAGMA
						{ $$ = mkidata_pragma(Lnil, $2); }
	|  /* empty */			    	{ $$ = mkno_pragma(); }
	;

idata_pragma_specs : 
	   SPECIALISE_PRAGMA idata_pragma_specslist
						{ $$ = $2; }
	|  /* empty */			    	{ $$ = Lnil; }
	;

idata_pragma_specslist:
	   idata_pragma_spectypes		{ $$ = lsing($1); }
	|  idata_pragma_specslist COMMA idata_pragma_spectypes
						{ $$ = lapp($1, $3); }
	;

idata_pragma_spectypes:
	   OBRACK type_maybes CBRACK		{ $$ = mkidata_pragma_4s($2); }
	;

iclas_pragma:
	   GHC_PRAGMA gen_pragma_list END_PRAGMA { $$ = mkiclas_pragma($2); }
	|  /* empty */				 { $$ = mkno_pragma(); }
	;

iclasop_pragma:
	   GHC_PRAGMA gen_pragma gen_pragma END_PRAGMA
		{ $$ = mkiclasop_pragma($2, $3); }
	|  /* empty */
		{ $$ = mkno_pragma(); }
	;

iinst_pragma:
	   GHC_PRAGMA gen_pragma END_PRAGMA
		{ $$ = mkiinst_simpl_pragma($2); }

	|  GHC_PRAGMA gen_pragma name_pragma_pairs END_PRAGMA
		{ $$ = mkiinst_const_pragma($2, $3); }

	|  /* empty */
		{ $$ = mkno_pragma(); }
	;

ival_pragma:
	   GHC_PRAGMA gen_pragma END_PRAGMA
		{ $$ = $2; }
	|  /* empty */
		{ $$ = mkno_pragma(); }
	;

gen_pragma:
	   NOINFO_PRAGMA
		{ $$ = mkno_pragma(); }
	|  arity_pragma update_pragma deforest_pragma strictness_pragma unfolding_pragma type_pragma_pairs_maybe
		{ $$ = mkigen_pragma($1, $2, $3, $4, $5, $6); }
	;

arity_pragma:
	   NO_PRAGMA		    { $$ = mkno_pragma(); }
	|  ARITY_PRAGMA INTEGER	    { $$ = mkiarity_pragma($2); }
	;

update_pragma:
	   NO_PRAGMA		    { $$ = mkno_pragma(); }
	|  UPDATE_PRAGMA INTEGER    { $$ = mkiupdate_pragma($2); }
	;

deforest_pragma:
           NO_PRAGMA                { $$ = mkno_pragma(); }
        |  DEFOREST_PRAGMA          { $$ = mkideforest_pragma(); }
        ;

strictness_pragma:
	   NO_PRAGMA		    { $$ = mkno_pragma(); }
	|  STRICTNESS_PRAGMA COCON  { $$ = mkistrictness_pragma(installHstring(1, "B"),
				      /* _!_ = COCON = bottom */ mkno_pragma());
				    }
	|  STRICTNESS_PRAGMA STRING worker_info
				    { $$ = mkistrictness_pragma($2, $3); }
	;

worker_info:
	   OCURLY gen_pragma CCURLY { $$ = $2; }
	|  /* empty */		    { $$ = mkno_pragma(); }

unfolding_pragma:
	   NO_PRAGMA		    { $$ = mkno_pragma(); }
	|  MAGIC_UNFOLDING_PRAGMA vark
				    { $$ = mkimagic_unfolding_pragma($2); }
	|  UNFOLDING_PRAGMA unfolding_guidance core_expr
				    { $$ = mkiunfolding_pragma($2, $3); }
	;

unfolding_guidance:
	   UNFOLD_ALWAYS
				    { $$ = mkiunfold_always(); }
	|  UNFOLD_IF_ARGS INTEGER INTEGER CONID INTEGER
				    { $$ = mkiunfold_if_args($2, $3, $4, $5); }
	;

gen_pragma_list:
	   gen_pragma				{ $$ = lsing($1); }
	|  gen_pragma_list COMMA gen_pragma	{ $$ = lapp($1, $3); }
	;

type_pragma_pairs_maybe:
	  NO_PRAGMA				{ $$ = Lnil; }
	| SPECIALISE_PRAGMA type_pragma_pairs	{ $$ = $2; }
	;

/* 1 S/R conflict at COMMA -> shift */
type_pragma_pairs:
	   type_pragma_pair			    { $$ = lsing($1); }
	|  type_pragma_pairs COMMA type_pragma_pair { $$ = lapp($1, $3); }
	;

type_pragma_pair:
	   OBRACK type_maybes CBRACK INTEGER worker_info
		{ $$ = mkitype_pragma_pr($2, $4, $5); }
	;

type_maybes:
	   type_maybe			{ $$ = lsing($1); }
	|  type_maybes COMMA type_maybe	{ $$ = lapp($1, $3); }
	;

type_maybe:
	   NO_PRAGMA			{ $$ = mknothing(); }
    	|  type				{ $$ = mkjust($1); }
	;

name_pragma_pairs:
	   name_pragma_pair			    { $$ = lsing($1); }
	|  name_pragma_pairs COMMA name_pragma_pair { $$ = lapp($1, $3); }
	;

name_pragma_pair:
	   /* if the gen_pragma concludes with a *comma*-separated SPECs list,
	      we get a parse error --- we have to bracket the gen_pragma
	   */

	   var EQUAL OCURLY gen_pragma CCURLY
		{ $$ = mkiname_pragma_pr($1, $4); }
	;

/**********************************************************************
*                                                                     *
*                                                                     *
*     Core syntax stuff 					      *
*                                                                     *
*                                                                     *
**********************************************************************/

core_expr:
	   LAMBDA core_binders RARROW core_expr
			{ $$ = mkcolam($2, $4); }
	|  TYLAMBDA core_tyvars RARROW core_expr
			{ $$ = mkcotylam($2, $4); }
	|  COCON con core_types core_atoms
			{ $$ = mkcocon(mkco_id($2), $3, $4); }
	|  COCON CO_ORIG_NM modid con core_types core_atoms
			{ $$ = mkcocon(mkco_orig_id($3,$4), $5, $6); }
	|  COPRIM co_primop core_types core_atoms
			{ $$ = mkcoprim($2, $3, $4); }
	|  COAPP core_expr core_atoms
			{ $$ = mkcoapp($2, $3); }
	|  COTYAPP core_expr OCURLY core_type CCURLY
			{ $$ = mkcotyapp($2, $4); }
	|  CASE core_expr OF OCURLY core_case_alts CCURLY
			{ $$ = mkcocase($2, $5); }
	|  LET OCURLY core_binder EQUAL core_expr CCURLY IN core_expr
			{ $$ = mkcolet(mkcononrec($3, $5), $8); }
	|  CO_LETREC OCURLY corec_binds CCURLY IN core_expr
			{ $$ = mkcolet(mkcorec($3), $6); }
	|  SCC OCURLY co_scc CCURLY core_expr
			{ $$ = mkcoscc($3, $5); }
	|  lit_constant { $$ = mkcoliteral($1); }
	|  core_id	{ $$ = mkcovar($1); }
	;

core_case_alts :
	   CO_ALG_ALTS  core_alg_alts  core_default
			{ $$ = mkcoalg_alts($2, $3); }
	|  CO_PRIM_ALTS core_prim_alts core_default
			{ $$ = mkcoprim_alts($2, $3); }
	;

core_alg_alts :
	   /* empty */			{ $$ = Lnil; }
	|  core_alg_alts core_alg_alt	{ $$ = lapp($1, $2); }
	;

core_alg_alt:
	   core_id core_binders RARROW core_expr SEMI { $$ = mkcoalg_alt($1, $2, $4); }
	   /* core_id is really too generous */
	;

core_prim_alts :
	   /* empty */			{ $$ = Lnil; }
	|  core_prim_alts core_prim_alt	{ $$ = lapp($1, $2); }
	;

core_prim_alt:
	   lit_constant RARROW core_expr SEMI { $$ = mkcoprim_alt($1, $3); }
	;

core_default:
	   CO_NO_DEFAULT		{ $$ = mkconodeflt(); }
	|  core_binder RARROW core_expr	{ $$ = mkcobinddeflt($1, $3); }
	;

corec_binds:
	   corec_bind			{ $$ = lsing($1); }
	|  corec_binds SEMI corec_bind	{ $$ = lapp($1, $3); }
	;

corec_bind:
	   core_binder EQUAL core_expr	{ $$ = mkcorec_pair($1, $3); }
	;

co_scc	:
	   CO_PRELUDE_DICTS_CC co_dupd		 { $$ = mkco_preludedictscc($2); }
	|  CO_ALL_DICTS_CC STRING STRING co_dupd { $$ = mkco_alldictscc($2,$3,$4); }
	|  CO_USER_CC STRING  STRING STRING co_dupd co_caf
						{ $$ = mkco_usercc($2,$3,$4,$5,$6); }
	|  CO_AUTO_CC core_id STRING STRING co_dupd co_caf
						{ $$ = mkco_autocc($2,$3,$4,$5,$6); }
	|  CO_DICT_CC core_id STRING STRING co_dupd co_caf
						{ $$ = mkco_dictcc($2,$3,$4,$5,$6); }

co_caf	:  NO_PRAGMA	{ $$ = mkco_scc_noncaf(); }
	|  CO_CAF_CC	{ $$ = mkco_scc_caf(); }

co_dupd	:  NO_PRAGMA	{ $$ = mkco_scc_nondupd(); }
	|  CO_DUPD_CC	{ $$ = mkco_scc_dupd(); }

core_id: /* more to come?? */
	   CO_SDSEL_ID  tycon tycon	{ $$ = mkco_sdselid($2, $3); }
	|  CO_METH_ID   tycon var	{ $$ = mkco_classopid($2, $3); }
	|  CO_DEFM_ID   tycon var	{ $$ = mkco_defmid($2, $3); }
	|  CO_DFUN_ID   tycon OPAREN core_type CPAREN
					{ $$ = mkco_dfunid($2, $4); }
	|  CO_CONSTM_ID tycon var OPAREN core_type CPAREN
					{ $$ = mkco_constmid($2, $3, $5); }
	|  CO_SPEC_ID	core_id OBRACK core_type_maybes CBRACK
					{ $$ = mkco_specid($2, $4); }
	|  CO_WRKR_ID	core_id		{ $$ = mkco_wrkrid($2); }
	|  CO_ORIG_NM   modid var	{ $$ = mkco_orig_id($2, $3); }
	|  CO_ORIG_NM   modid con	{ $$ = mkco_orig_id($2, $3); }
	|  var				{ $$ = mkco_id($1); }
	|  con				{ $$ = mkco_id($1); }
	;

co_primop :
	   OPAREN CCALL ccallid	     OCURLY core_types core_type CCURLY CPAREN
					{ $$ = mkco_ccall($3,0,$5,$6); }
	|  OPAREN CCALL_GC ccallid   OCURLY core_types core_type CCURLY CPAREN
					{ $$ = mkco_ccall($3,1,$5,$6); }
	|  OPAREN CASM  lit_constant OCURLY core_types core_type CCURLY CPAREN
					{ $$ = mkco_casm($3,0,$5,$6); }
	|  OPAREN CASM_GC lit_constant OCURLY core_types core_type CCURLY CPAREN
					{ $$ = mkco_casm($3,1,$5,$6); }
	|  VARID			{ $$ = mkco_primop($1); }
	;

core_binders :
	   /* empty */			{ $$ = Lnil; }
	|  core_binders core_binder	{ $$ = lapp($1, $2); }
	;

core_binder :
	   OPAREN VARID DCOLON core_type CPAREN	{ $$ = mkcobinder($2, $4); }

core_atoms :
	   OBRACK CBRACK		{ $$ = Lnil; }
	|  OBRACK core_atom_list CBRACK	{ $$ = $2; }
	;

core_atom_list :
	   core_atom			    { $$ = lsing($1); }
	|  core_atom_list COMMA core_atom   { $$ = lapp($1, $3); }
	;

core_atom :
	   lit_constant		{ $$ = mkcolit($1); }
	|  core_id		{ $$ = mkcolocal($1); }
	;

core_tyvars :
	   VARID		{ $$ = lsing($1); }
	|  core_tyvars VARID  	{ $$ = lapp($1, $2); }
	;

core_tv_templates :
	   TYVAR_TEMPLATE_ID				{ $$ = lsing($1); }
	|  core_tv_templates COMMA TYVAR_TEMPLATE_ID 	{ $$ = lapp($1, $3); }
	;

core_types :
	   OBRACK CBRACK		{ $$ = Lnil; }
	|  OBRACK core_type_list CBRACK	{ $$ = $2; }
	;

core_type_list :
	   core_type			    { $$ = lsing($1); }
	|  core_type_list COMMA core_type   { $$ = lapp($1, $3); }
	;

core_type :
	   type { $$ = $1; }
	;

/*
core_type :
	   FORALL core_tv_templates DARROW core_type
		{ $$ = mkuniforall($2, $4); }
	|  OCURLY OCURLY CONID core_type CCURLY CCURLY RARROW core_type
		{ $$ = mktfun(mkunidict($3, $4), $8); }
	|  OCURLY OCURLY CONID core_type CCURLY CCURLY
		{ $$ = mkunidict($3, $4); }
	|  OPAREN OCURLY OCURLY CONID core_type CCURLY CCURLY COMMA core_type_list CPAREN RARROW core_type
		{ $$ = mktfun(mkttuple(mklcons(mkunidict($4, $5), $9)), $12); }
	|  OPAREN OCURLY OCURLY CONID core_type CCURLY CCURLY COMMA core_type_list CPAREN
		{ $$ = mkttuple(mklcons(mkunidict($4,$5), $9)); }
	|  type { $$ = $1; }
	;
*/

core_type_maybes:
	   core_type_maybe			    { $$ = lsing($1); }
	|  core_type_maybes COMMA core_type_maybe   { $$ = lapp($1, $3); }
	;

core_type_maybe:
	   NO_PRAGMA			{ $$ = mknothing(); }
    	|  core_type			{ $$ = mkjust($1); }
	;


/**********************************************************************
*                                                                     *
*                                                                     *
*     Fixes and Decls etc 					      *
*                                                                     *
*                                                                     *
**********************************************************************/

maybefixes:  /* empty */		{ $$ = Lnil; }
	|  fixes SEMI			{ $$ = $1; }
	;

fixes	:  fix				{ $$ = $1; }
	|  fixes SEMI fix		{ $$ = lconc($1,$3); }
	;

fix	:  INFIXL INTEGER	{ Precedence = checkfixity($2); Fixity = INFIXL; }
	   ops  		{ $$ = $4; }
	|  INFIXR INTEGER	{ Precedence = checkfixity($2); Fixity = INFIXR; }
	   ops  		{ $$ = $4; }
	|  INFIX  INTEGER	{ Precedence = checkfixity($2); Fixity = INFIX; }
	   ops  		{ $$ = $4; }
	|  INFIXL		{ Fixity = INFIXL; Precedence = 9; }
	   ops  		{ $$ = $3; }
	|  INFIXR		{ Fixity = INFIXR; Precedence = 9; }
	   ops  		{ $$ = $3; }
	|  INFIX		{ Fixity = INFIX; Precedence = 9; }
	   ops  		{ $$ = $3; }
	;

ops	:  op		 { makeinfix($1,Fixity,Precedence,the_module_name,
				     inimport,importas,importmod,asmod,importqual,
				     importspec,importhide,importlist);
			   $$ = lsing(mkfixop($1,infixint(Fixity),Precedence));
			 }
	|  ops COMMA op  { makeinfix($3,Fixity,Precedence,the_module_name,
				     inimport,importas,importmod,asmod,importqual,
				     importspec,importhide,importlist);
			   $$ = lapp($1,mkfixop($3,infixint(Fixity),Precedence));
			 }
	;

topdecls:  topdecl
	|  topdecls SEMI topdecl
		{
		  if($1 != NULL)
		    if($3 != NULL)
		      if(SAMEFN)
			{
			  extendfn($1,$3);
			  $$ = $1;
			}
		      else
			$$ = mkabind($1,$3);
		    else
		      $$ = $1;
		  else
		    $$ = $3;
		  SAMEFN = 0;
		}
	;

topdecl	:  typed				{ $$ = $1; }
	|  datad 				{ $$ = $1; }
	|  newtd				{ $$ = $1; }
	|  classd 				{ $$ = $1; }
	|  instd 				{ $$ = $1; }
	|  defaultd 				{ $$ = $1; }
	|  decl 				{ $$ = $1; }
	;

typed	:  typekey simple EQUAL type		{ $$ = mknbind($2,$4,startlineno); }
	;


datad	:  datakey simple EQUAL constrs
		{ $$ = mktbind(Lnil,$2,$4,mknothing(),startlineno,mkno_pragma()); }
	|  datakey simple EQUAL constrs DERIVING dtyclses
		{ $$ = mktbind(Lnil,$2,$4,mkjust($6),startlineno,mkno_pragma()); }
	|  datakey context DARROW simple EQUAL constrs
		{ $$ = mktbind($2,$4,$6,mknothing(),startlineno,mkno_pragma()); }
	|  datakey context DARROW simple EQUAL constrs DERIVING dtyclses
		{ $$ = mktbind($2,$4,$6,mkjust($8),startlineno,mkno_pragma()); }
	;

newtd	:  newtypekey simple EQUAL constr1
		{ $$ = mkntbind(Lnil,$2,$4,mknothing(),startlineno,mkno_pragma()); }
	|  newtypekey simple EQUAL constr1 DERIVING dtyclses
		{ $$ = mkntbind(Lnil,$2,$4,mkjust($6),startlineno,mkno_pragma()); }
	|  newtypekey context DARROW simple EQUAL constr1
		{ $$ = mkntbind($2,$4,$6,mknothing(),startlineno,mkno_pragma()); }
	|  newtypekey context DARROW simple EQUAL constr1 DERIVING dtyclses
		{ $$ = mkntbind($2,$4,$6,mkjust($8),startlineno,mkno_pragma()); }
	;

classd	:  classkey context DARROW class cbody	{ $$ = mkcbind($2,$4,$5,startlineno,mkno_pragma()); }
	|  classkey class cbody		 	{ $$ = mkcbind(Lnil,$2,$3,startlineno,mkno_pragma()); }
	;

cbody	:  /* empty */				{ $$ = mknullbind(); }
	|  WHERE ocurly decls ccurly		{ checkorder($3); $$ = $3; }
	|  WHERE vocurly decls vccurly		{ checkorder($3); $$ = $3; }
	;

instd	:  instkey context DARROW gtycon restrict_inst rinst
		{ $$ = mkibind(1/*source*/,the_module_name,$2,$4,$5,$6,startlineno,mkno_pragma()); }
	|  instkey gtycon general_inst rinst
	 	{ $$ = mkibind(1/*source*/,the_module_name,Lnil,$2,$3,$4,startlineno,mkno_pragma()); }
	;

rinst	:  /* empty */			  	{ $$ = mknullbind(); }
	|  WHERE ocurly  instdefs ccurly  	{ $$ = $3; }
	|  WHERE vocurly instdefs vccurly 	{ $$ = $3; }
	;

restrict_inst : gtycon				{ $$ = mktname($1); }
	|  OPAREN gtyconvars CPAREN		{ $$ = $2; }
	|  OPAREN tyvar COMMA tyvar_list CPAREN	{ $$ = mkttuple(mklcons($2,$4)); }
	|  OBRACK tyvar CBRACK			{ $$ = mktllist($2); }
	|  OPAREN tyvar RARROW tyvar CPAREN	{ $$ = mktfun($2,$4); }
	;

general_inst : gtycon				{ $$ = mktname($1); }
	|  OPAREN gtyconapp CPAREN		{ $$ = $2; }
	|  OPAREN type COMMA types CPAREN	{ $$ = mkttuple(mklcons($2,$4)); }
	|  OBRACK type CBRACK			{ $$ = mktllist($2); }
	|  OPAREN btype RARROW type CPAREN	{ $$ = mktfun($2,$4); }
	;

defaultd:  defaultkey OPAREN types CPAREN       { $$ = mkdbind($3,startlineno); }
	|  defaultkey OPAREN CPAREN		{ $$ = mkdbind(Lnil,startlineno); }
	;

decls	:  decl
	|  decls SEMI decl
		{
		  if(SAMEFN)
		    {
		      extendfn($1,$3);
		      $$ = $1;
		    }
		  else
		    $$ = mkabind($1,$3);
		}
	;


/*
    Note: if there is an iclasop_pragma here, then we must be
    doing a class-op in an interface -- unless the user is up
    to real mischief (ugly, but likely to work).
*/

decl	:  qvarsk DCOLON ctype iclasop_pragma
		{ $$ = mksbind($1,$3,startlineno,$4);
		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}
	/* User-specified pragmas come in as "signatures"...
	   They are similar in that they can appear anywhere in the module,
	   and have to be "joined up" with their related entity.

	   Have left out the case specialising to an overloaded type.
	   Let's get real, OK?  (WDP)
	*/
	|  SPECIALISE_UPRAGMA qvark DCOLON types_and_maybe_ids END_UPRAGMA
		{
		  $$ = mkvspec_uprag($2, $4, startlineno);
		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}

	|  SPECIALISE_UPRAGMA INSTANCE gtycon general_inst END_UPRAGMA
		{
		  $$ = mkispec_uprag($3, $4, startlineno);
		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}

	|  SPECIALISE_UPRAGMA DATA gtycon atypes END_UPRAGMA
		{
		  $$ = mkdspec_uprag($3, $4, startlineno);
		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}

	|  INLINE_UPRAGMA qvark END_UPRAGMA
		{
		  $$ = mkinline_uprag($2, startlineno);
		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}

	|  MAGIC_UNFOLDING_UPRAGMA qvark vark END_UPRAGMA
		{
		  $$ = mkmagicuf_uprag($2, $3, startlineno);
		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}

        |  DEFOREST_UPRAGMA qvark END_UPRAGMA
                {
		  $$ = mkdeforest_uprag($2, startlineno);
 		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}

	/* end of user-specified pragmas */

	|  valdef
	|  /* empty */ { $$ = mknullbind(); PREVPATT = NULL; FN = NULL; SAMEFN = 0; }
  	;

qvarsk	:  qvark COMMA qvars_list		{ $$ = mklcons($1,$3); }
	|  qvark				{ $$ = lsing($1); }
	;

qvars_list: qvar				{ $$ = lsing($1); }
	|   qvars_list COMMA qvar		{ $$ = lapp($1,$3); }
	;

types_and_maybe_ids :
	   type_and_maybe_id				{ $$ = lsing($1); }
	|  types_and_maybe_ids COMMA type_and_maybe_id	{ $$ = lapp($1,$3); }
	;

type_and_maybe_id :
	   type					{ $$ = mkvspec_ty_and_id($1,mknothing()); }
	|  type EQUAL qvark			{ $$ = mkvspec_ty_and_id($1,mkjust($3)); }


/**********************************************************************
*                                                                     *
*                                                                     *
*     Types etc     	 					      *
*                                                                     *
*                                                                     *
**********************************************************************/

/*  "DCOLON context => type" vs "DCOLON type" is a problem,
    because you can't distinguish between

	foo :: (Baz a, Baz a)
	bar :: (Baz a, Baz a) => [a] -> [a] -> [a]

    with one token of lookahead.  The HACK is to have "DCOLON ttype"
    [tuple type] in the first case, then check that it has the right
    form C a, or (C1 a, C2 b, ... Cn z) and convert it into a
    context.  Blaach!
*/

	/* 1 S/R conflict at DARROW -> shift */
ctype   : type DARROW type			{ $$ = mkcontext(type2context($1),$3); }
	| type
	;

	/* 1 S/R conflict at RARROW -> shift */
type	:  btype				{ $$ = $1; }
	|  btype RARROW type			{ $$ = mktfun($1,$3); }

	|  FORALL core_tv_templates DARROW type { $$ = mkuniforall($2, $4); }
	;

/* btype is split so we can parse gtyconapp without S/R conflicts */
btype	:  gtyconapp				{ $$ = $1; }
	|  ntyconapp				{ $$ = $1; }
	;

ntyconapp: ntycon				{ $$ = $1; }
	|  ntyconapp atype			{ $$ = mktapp($1,$2); }
	;

gtyconapp: gtycon				{ $$ = mktname($1); }
	|  gtyconapp atype			{ $$ = mktapp($1,$2); }
	;


atype  	:  gtycon				{ $$ = mktname($1); }
	|  ntycon				{ $$ = $1; }
	;

ntycon	:  tyvar				{ $$ = $1; }
	|  OPAREN type COMMA types CPAREN	{ $$ = mkttuple(mklcons($2,$4)); }
	|  OBRACK type CBRACK			{ $$ = mktllist($2); }
	|  OPAREN type CPAREN			{ $$ = $2; }

	|  OCURLY OCURLY gtycon type CCURLY CCURLY { $$ = mkunidict($3, $4); }
	|  TYVAR_TEMPLATE_ID			{ $$ = mkunityvartemplate($1); }
	;

gtycon	:  qtycon
	|  OPAREN RARROW CPAREN			{ $$ = creategid(-2); }
	|  OBRACK CBRACK			{ $$ = creategid(-1); }         
	|  OPAREN CPAREN			{ $$ = creategid(0); }         
	|  OPAREN commas CPAREN 		{ $$ = creategid($2); }
	;

atypes	:  atype				{ $$ = lsing($1); }
	|  atypes atype				{ $$ = lapp($1,$2); }
	;

types	:  type					{ $$ = lsing($1); }
	|  types COMMA type			{ $$ = lapp($1,$3); }
	;

commas	: COMMA					{ $$ = 1; }
	| commas COMMA				{ $$ = $1 + 1; }
	;

/**********************************************************************
*                                                                     *
*                                                                     *
*     Declaration stuff 					      *
*                                                                     *
*                                                                     *
**********************************************************************/

simple	:  gtycon				{ $$ = mktname($1); }
	|  gtyconvars	  			{ $$ = $1; }
	;

gtyconvars: gtycon tyvar			{ $$ = mktapp(mktname($1),$2); }
	|  gtyconvars tyvar			{ $$ = mktapp($1,$2); }
	;

context	:  OPAREN context_list CPAREN		{ $$ = $2; }
	|  class				{ $$ = lsing($1); }
	;

context_list:  class				{ $$ = lsing($1); }
	|  context_list COMMA class	 	{ $$ = lapp($1,$3); }
	;

class	:  gtycon tyvar 			{ $$ = mktapp(mktname($1),$2); }
	;

constrs	:  constr				{ $$ = lsing($1); }
	|  constrs VBAR constr			{ $$ = lapp($1,$3); }
	;

constr	:  btyconapp				{ qid tyc; list tys;
						  splittyconapp($1, &tyc, &tys);
					          $$ = mkconstrpre(tyc,tys,hsplineno); }
	|  OPAREN qconsym CPAREN		{ $$ = mkconstrpre($2,Lnil,hsplineno); }
	|  OPAREN qconsym CPAREN batypes	{ $$ = mkconstrpre($2,$4,hsplineno); }
	|  btyconapp qconop bbtype		{ checknobangs($1);
						  $$ = mkconstrinf($1,$2,$3,hsplineno); }
	|  ntyconapp qconop bbtype		{ $$ = mkconstrinf($1,$2,$3,hsplineno); }
	|  BANG atype qconop bbtype		{ $$ = mkconstrinf(mktbang($2),$3,$4,hsplineno); }

	/* 1 S/R conflict on OCURLY -> shift */
	|  gtycon OCURLY fields CCURLY		{ $$ = mkconstrrec($1,$3,hsplineno); }
	;

btyconapp: gtycon				{ $$ = mktname($1); }
	|  btyconapp batype			{ $$ = mktapp($1,$2); }
	;

bbtype	:  btype				{ $$ = $1; }
	|  BANG atype				{ $$ = mktbang($2); }
	;

batype	:  atype				{ $$ = $1; }
	|  BANG atype				{ $$ = mktbang($2); }
	;

batypes	:  batype				{ $$ = lsing($1); }
	|  batypes batype			{ $$ = lapp($1,$2); }
	;


fields	: field					{ $$ = lsing($1); }
	| fields COMMA field			{ $$ = lapp($1,$3); }
	;

field	:  qvars_list DCOLON type		{ $$ = mkfield($1,$3); }
	|  qvars_list DCOLON BANG atype		{ $$ = mkfield($1,mktbang($4)); }
	; 

constr1 :  gtycon atype				{ $$ = lsing(mkconstrnew($1,$2,hsplineno)); }
	;


dtyclses:  OPAREN dtycls_list CPAREN		{ $$ = $2; }
	|  OPAREN CPAREN			{ $$ = Lnil; }
	|  qtycls				{ $$ = lsing($1); }
	;

dtycls_list:  qtycls				{ $$ = lsing($1); }
	|  dtycls_list COMMA qtycls		{ $$ = lapp($1,$3); }
	;

instdefs : /* empty */				{ $$ = mknullbind(); }
	 | instdef				{ $$ = $1; }
	 | instdefs SEMI instdef
		{
		  if(SAMEFN)
		    {
		      extendfn($1,$3);
		      $$ = $1;
		    }
		  else
		    $$ = mkabind($1,$3);
		}
	;

/* instdef: same as valdef, except certain user-pragmas may appear */
instdef :
	   SPECIALISE_UPRAGMA qvark DCOLON types_and_maybe_ids END_UPRAGMA
		{
		  $$ = mkvspec_uprag($2, $4, startlineno);
		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}

	|  INLINE_UPRAGMA qvark END_UPRAGMA
		{
		  $$ = mkinline_uprag($2, startlineno);
		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}

	|  MAGIC_UNFOLDING_UPRAGMA qvark vark END_UPRAGMA
		{
		  $$ = mkmagicuf_uprag($2, $3, startlineno);
		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}

	|  valdef
  	;


valdef	:  opatk
		{
		  tree fn = function($1);
		  PREVPATT = $1;

		  if(ttree(fn) == ident)
		    {
		      qid fun_id = gident((struct Sident *) fn);
		      checksamefn(fun_id);
		      FN = fun_id;
		    }

		  else if (ttree(fn) == infixap)
		    {
		      qid fun_id = ginffun((struct Sinfixap *) fn); 
		      checksamefn(fun_id);
		      FN = fun_id;
		    }

		  else if(etags)
#if 1/*etags*/
		    printf("%u\n",startlineno);
#else
		    fprintf(stderr,"%u\tvaldef\n",startlineno);
#endif
		}
	   valrhs
	  	{
		  if ( lhs_is_patt($1) )
		    {
		      $$ = mkpbind($3, startlineno);
		      FN = NULL;
		      SAMEFN = 0;
		    }
		  else /* lhs is function */
		    $$ = mkfbind($3,startlineno);

		  PREVPATT = NULL;
		}
	;

valrhs	:  valrhs1 maybe_where			{ $$ = lsing(createpat($1, $2)); }
	;

valrhs1	:  gdrhs				{ $$ = mkpguards($1); }
	|  EQUAL exp				{ $$ = mkpnoguards($2); }
	;

gdrhs	:  gd EQUAL exp				{ $$ = lsing(mkpgdexp($1,$3)); }
	|  gd EQUAL exp gdrhs			{ $$ = mklcons(mkpgdexp($1,$3),$4); }
	;

maybe_where:
	   WHERE ocurly decls ccurly		{ $$ = $3; }
	|  WHERE vocurly decls vccurly		{ $$ = $3; }
	|  /* empty */				{ $$ = mknullbind(); }
	;

gd	:  VBAR oexp				{ $$ = $2; }
	;


/**********************************************************************
*                                                                     *
*                                                                     *
*     Expressions						      *
*                                                                     *
*                                                                     *
**********************************************************************/

exp	:  oexp DCOLON ctype			{ $$ = mkrestr($1,$3); }
	|  oexp
	;

/*
  Operators must be left-associative at the same precedence for
  precedence parsing to work.
*/
	/* 9 S/R conflicts on qop -> shift */
oexp	:  oexp qop oexp %prec MINUS		{ $$ = mkinfixap($2,$1,$3); precparse($$); }
	|  dexp
	;

/*
  This comes here because of the funny precedence rules concerning
  prefix minus.
*/
dexp	:  MINUS kexp				{ $$ = mknegate($2,NULL,NULL); }
	|  kexp
	;

/*
  We need to factor out a leading let expression so we can set
  inpat=TRUE when parsing (non let) expressions inside stmts and quals
*/
expLno 	:  oexpLno DCOLON ctype			{ $$ = mkrestr($1,$3); }
	|  oexpLno
	;
oexpLno	:  oexpLno qop oexp %prec MINUS		{ $$ = mkinfixap($2,$1,$3); precparse($$); }
	|  dexpLno
	;
dexpLno	:  MINUS kexp				{ $$ = mknegate($2,NULL,NULL); }
	|  kexpLno
	;

expL 	:  oexpL DCOLON ctype			{ $$ = mkrestr($1,$3); }
	|  oexpL
	;
oexpL	:  oexpL qop oexp %prec MINUS		{ $$ = mkinfixap($2,$1,$3); precparse($$); }
	|  kexpL
	;

/*
  let/if/lambda/case have higher precedence than infix operators.
*/

kexp	:  kexpL
	|  kexpLno
	;

kexpL	:  letdecls IN exp			{ $$ = mklet($1,$3); }
	;

kexpLno	:  LAMBDA
		{ hsincindent();        /* push new context for FN = NULL;        */
		  FN = NULL; 	        /* not actually concerned about indenting */
		  $<ulong>$ = hsplineno; /* remember current line number           */
		}
	   lampats
  		{ hsendindent();
		}
	   RARROW exp		        /* lambda abstraction */
		{
		  $$ = mklambda($3, $6, $<ulong>2);
		}

	/* If Expression */
	|  IF {$<ulong>$ = hsplineno;}
	   exp THEN exp ELSE exp		{ $$ = mkife($3,$5,$7,$<ulong>2); }

	/* Case Expression */
	|  CASE {$<ulong>$ = hsplineno;}
	   exp OF caserest			{ $$ = mkcasee($3,$5,$<ulong>2); }

	/* Do Expression */
	|  DO {$<ulong>$ = hsplineno;}
	   dorest				{ $$ = mkdoe($3,$<ulong>2); }

	/* CCALL/CASM Expression */
	|  CCALL ccallid cexps			{ $$ = mkccall($2,install_literal("n"),$3); }
	|  CCALL ccallid			{ $$ = mkccall($2,install_literal("n"),Lnil); }
	|  CCALL_GC ccallid cexps		{ $$ = mkccall($2,install_literal("p"),$3); }
	|  CCALL_GC ccallid 			{ $$ = mkccall($2,install_literal("p"),Lnil); }
	|  CASM CLITLIT cexps			{ $$ = mkccall($2,install_literal("N"),$3); }
	|  CASM CLITLIT				{ $$ = mkccall($2,install_literal("N"),Lnil); }
	|  CASM_GC CLITLIT cexps		{ $$ = mkccall($2,install_literal("P"),$3); }
	|  CASM_GC CLITLIT 			{ $$ = mkccall($2,install_literal("P"),Lnil); }

	/* SCC Expression */
	|  SCC STRING exp
		{ if (ignoreSCC) {
		    $$ = $3;
		  } else {
		    $$ = mkscc($2, $3);
		  }
		}
	|  fexp
  	;

fexp	:  fexp aexp				{ $$ = mkap($1,$2); }
	|  aexp
	;

	/* simple expressions */
aexp	:  qvar					{ $$ = mkident($1); }
	|  gcon					{ $$ = mkident($1); }
	|  lit_constant				{ $$ = mklit($1); }
	|  OPAREN exp CPAREN			{ $$ = mkpar($2); }	  /* mkpar: stop infix parsing at ()'s */
	|  qcon OCURLY CCURLY			{ $$ = mkrecord($1,Lnil); }
	|  qcon OCURLY rbinds CCURLY		{ $$ = mkrecord($1,$3); } /* 1 S/R conflict on OCURLY -> shift */
	|  OBRACK list_exps CBRACK		{ $$ = mkllist($2); }
	|  OPAREN exp COMMA texps CPAREN	{ if (ttree($4) == tuple)
			     			     $$ = mktuple(mklcons($2, gtuplelist((struct Stuple *) $4)));
						  else
						     $$ = mktuple(ldub($2, $4)); }

	/* only in expressions ... */
	|  aexp OCURLY rbinds CCURLY		{ $$ = mkrupdate($1,$3); }
	|  OBRACK exp VBAR quals CBRACK		{ $$ = mkcomprh($2,$4); }
	|  OBRACK exp COMMA exp DOTDOT exp CBRACK {$$= mkeenum($2,mkjust($4),mkjust($6)); }
	|  OBRACK exp COMMA exp DOTDOT CBRACK	{ $$ = mkeenum($2,mkjust($4),mknothing()); }
	|  OBRACK exp DOTDOT exp CBRACK		{ $$ = mkeenum($2,mknothing(),mkjust($4)); }
	|  OBRACK exp DOTDOT CBRACK		{ $$ = mkeenum($2,mknothing(),mknothing()); }
	|  OPAREN oexp qop CPAREN		{ $$ = mklsection($2,$3); }
	|  OPAREN qop1 oexp CPAREN		{ $$ = mkrsection($2,$3); }

	/* only in patterns ... */
	/* these add 2 S/R conflict with with  aexp . OCURLY rbinds CCURLY */
	|  qvar AT aexp				{ checkinpat(); $$ = mkas($1,$3); }
	|  LAZY aexp				{ checkinpat(); $$ = mklazyp($2); }
	|  WILDCARD				{ checkinpat(); $$ = mkwildp();   }
	;

	/* ccall arguments */
cexps	:  cexps aexp				{ $$ = lapp($1,$2); }
	|  aexp					{ $$ = lsing($1); }
	;

caserest:  ocurly alts ccurly			{ $$ = $2; }
	|  vocurly alts vccurly			{ $$ = $2; }

dorest  :  ocurly stmts ccurly			{ checkdostmts($2); $$ = $2; }
	|  vocurly stmts vccurly		{ checkdostmts($2); $$ = $2; }
	;

rbinds	:  rbind				{ $$ = lsing($1); }
	|  rbinds COMMA rbind			{ $$ = lapp($1,$3); }
	;

rbind  	:  qvar					{ $$ = mkrbind($1,mknothing()); }
	|  qvar EQUAL exp			{ $$ = mkrbind($1,mkjust($3)); }
	;

texps	:  exp	{ $$ = mkpar($1); }	/* mkpar: so we don't flatten last element in tuple */
	|  exp COMMA texps
		{ if (ttree($3) == tuple)
		    $$ = mktuple(mklcons($1, gtuplelist((struct Stuple *) $3)));
		  else
		    $$ = mktuple(ldub($1, $3));
		}
	/* right recursion? WDP */
	;


list_exps :
	   exp					{ $$ = lsing($1); }
	|  exp COMMA list_exps		{ $$ = mklcons($1, $3); }
	/* right recursion? (WDP)

	   It has to be this way, though, otherwise you
	   may do the wrong thing to distinguish between...

	   [ e1 , e2 .. ]	-- an enumeration ...
	   [ e1 , e2 , e3 ]	-- a list

	   (In fact, if you change the grammar and throw yacc/bison
	   at it, it *will* do the wrong thing [WDP 94/06])
	*/
	;

letdecls:  LET ocurly decls ccurly		{ $$ = $3 }
	|  LET vocurly decls vccurly		{ $$ = $3 }
	;

quals	:  qual					{ $$ = lsing($1); }
	|  quals COMMA qual			{ $$ = lapp($1,$3); }
	;

qual	:  letdecls				{ $$ = mkseqlet($1); }
	|  expL					{ $$ = $1; }
	|  {inpat=TRUE;} expLno {inpat=FALSE;}leftexp
		{ if ($4 == NULL) {
		      expORpat(LEGIT_EXPR,$2);
		      $$ = mkguard($2);
		  } else {
		      expORpat(LEGIT_PATT,$2);
		      $$ = mkqual($2,$4);
		  }
		}
	;

alts	:  alt					{ $$ = $1; }
	|  alts SEMI alt			{ $$ = lconc($1,$3); }
	;

alt	:  pat { PREVPATT = $1; } altrest	{ $$ = lsing($3); PREVPATT = NULL; }
	|  /* empty */				{ $$ = Lnil; }
	;

altrest	:  gdpat maybe_where	 		{ $$ = createpat(mkpguards($1), $2); }
	|  RARROW exp maybe_where		{ $$ = createpat(mkpnoguards($2),$3); }
	;

gdpat	:  gd RARROW exp			{ $$ = lsing(mkpgdexp($1,$3)); }
	|  gd RARROW exp gdpat			{ $$ = mklcons(mkpgdexp($1,$3),$4);  }
	;

stmts	:  stmt					{ $$ = $1; }
	|  stmts SEMI stmt			{ $$ = lconc($1,$3); }
	;

stmt	:  /* empty */				{ $$ = Lnil; }
	|  letdecls				{ $$ = lsing(mkseqlet($1)); }
	|  expL					{ $$ = lsing($1); }
	|  {inpat=TRUE;} expLno {inpat=FALSE;} leftexp
		{ if ($4 == NULL) {
		      expORpat(LEGIT_EXPR,$2);
		      $$ = lsing(mkdoexp($2,endlineno));
		  } else {
		      expORpat(LEGIT_PATT,$2);
		      $$ = lsing(mkdobind($2,$4,endlineno));
		  }
		}
	;

leftexp	:  LARROW exp				{ $$ = $2; }
        |  /* empty */				{ $$ = NULL; }
	;

/**********************************************************************
*                                                                     *
*                                                                     *
*     Patterns							      *
*                                                                     *
*                                                                     *
**********************************************************************/

/*
	The xpatk business is to do with accurately recording
	the starting line for definitions.
*/

opatk	:  dpatk
	|  opatk qop opat %prec MINUS
		{
		  $$ = mkinfixap($2,$1,$3);

		  if (isconstr(qid_to_string($2)))
		    precparse($$);
		  else
		    {
		      checkprec($1,$2,FALSE);	/* Check the precedence of the left pattern */
		      checkprec($3,$2,TRUE);	/* then check the right pattern */
		    }
		}
	;

opat	:  dpat
	|  opat qop opat %prec MINUS
		{
		  $$ = mkinfixap($2,$1,$3);

		  if(isconstr(qid_to_string($2)))
		    precparse($$);
		  else
		    {
		      checkprec($1,$2,FALSE);	/* Check the precedence of the left pattern */
		      checkprec($3,$2,TRUE);	/* then check the right pattern */
		    }
		}
	;

/*
  This comes here because of the funny precedence rules concerning
  prefix minus.
*/


dpat	:  MINUS fpat				{ $$ = mknegate($2,NULL,NULL); }
	|  fpat
	;

	/* Function application */
fpat	:  fpat aapat				{ $$ = mkap($1,$2); }
	|  aapat
	;

dpatk	:  minuskey fpat			{ $$ = mknegate($2,NULL,NULL); }
	|  fpatk
	;

	/* Function application */
fpatk	:  fpatk aapat				{ $$ = mkap($1,$2); }
	|  aapatk
	;

aapat	:  qvar		 			{ $$ = mkident($1); }
	|  qvar AT apat			 	{ $$ = mkas($1,$3); }
	|  gcon		 			{ $$ = mkident($1); }
	|  qcon OCURLY rpats CCURLY		{ $$ = mkrecord($1,$3); }
	|  lit_constant				{ $$ = mklit($1); }
	|  WILDCARD				{ $$ = mkwildp(); }
	|  OPAREN opat CPAREN			{ $$ = mkpar($2); }
	|  OPAREN opat COMMA pats CPAREN 	{ $$ = mktuple(mklcons($2,$4)); }
	|  OBRACK pats CBRACK			{ $$ = mkllist($2); }
	|  LAZY apat				{ $$ = mklazyp($2); }
	;


aapatk	:  qvark	 			{ $$ = mkident($1); }
	|  qvark AT apat	 		{ $$ = mkas($1,$3); }
	|  gconk	 			{ $$ = mkident($1); }
	|  qconk OCURLY rpats CCURLY		{ $$ = mkrecord($1,$3); }
	|  lit_constant				{ $$ = mklit($1); setstartlineno(); }
	|  WILDCARD				{ $$ = mkwildp(); setstartlineno(); }
	|  oparenkey opat CPAREN		{ $$ = mkpar($2); }
	|  oparenkey opat COMMA pats CPAREN 	{ $$ = mktuple(mklcons($2,$4)); }
	|  obrackkey pats CBRACK		{ $$ = mkllist($2); }
	|  lazykey apat				{ $$ = mklazyp($2); }
	;

gcon	:  qcon
	|  OBRACK CBRACK			{ $$ = creategid(-1); }
	|  OPAREN CPAREN			{ $$ = creategid(0); }
	|  OPAREN commas CPAREN			{ $$ = creategid($2); }
	;

gconk	:  qconk		 		
	|  obrackkey CBRACK			{ $$ = creategid(-1); }
	|  oparenkey CPAREN			{ $$ = creategid(0); }
	|  oparenkey commas CPAREN		{ $$ = creategid($2); }
	;

lampats	:  apat lampats				{ $$ = mklcons($1,$2); }
	|  apat					{ $$ = lsing($1); }
	/* right recursion? (WDP) */
	;

pats	:  pat COMMA pats			{ $$ = mklcons($1, $3); }
	|  pat					{ $$ = lsing($1); }
    	/* right recursion? (WDP) */
	;

pat	:  pat qconop bpat			{ $$ = mkinfixap($2,$1,$3); precparse($$); }
	|  bpat
	;

bpat	:  apatc
 	|  conpat
	|  qcon OCURLY rpats CCURLY		{ $$ = mkrecord($1,$3); }
	|  MINUS INTEGER			{ $$ = mklit(mkinteger(ineg($2))); }
	|  MINUS FLOAT				{ $$ = mklit(mkfloatr(ineg($2))); }
	;

conpat	:  gcon					{ $$ = mkident($1); }
	|  conpat apat				{ $$ = mkap($1,$2); }
	;

apat	:  gcon		 			{ $$ = mkident($1); }
	|  qcon OCURLY rpats CCURLY		{ $$ = mkrecord($1,$3); }
	|  apatc
	;

apatc	:  qvar		 			{ $$ = mkident($1); }
	|  qvar AT apat			 	{ $$ = mkas($1,$3); }
	|  lit_constant				{ $$ = mklit($1); }
	|  WILDCARD				{ $$ = mkwildp(); }
	|  OPAREN pat CPAREN			{ $$ = mkpar($2); }
	|  OPAREN pat COMMA pats CPAREN 	{ $$ = mktuple(mklcons($2,$4)); }
	|  OBRACK pats CBRACK			{ $$ = mkllist($2); }
	|  LAZY apat				{ $$ = mklazyp($2); }
	;

lit_constant:
	   INTEGER				{ $$ = mkinteger($1); }
	|  FLOAT				{ $$ = mkfloatr($1); }
	|  CHAR					{ $$ = mkcharr($1); }
	|  STRING				{ $$ = mkstring($1); }
	|  CHARPRIM				{ $$ = mkcharprim($1); }
	|  STRINGPRIM				{ $$ = mkstringprim($1); }
	|  INTPRIM				{ $$ = mkintprim($1); }
	|  FLOATPRIM				{ $$ = mkfloatprim($1); }
	|  DOUBLEPRIM				{ $$ = mkdoubleprim($1); }
	|  CLITLIT /* yurble yurble */		{ $$ = mkclitlit($1, ""); }
	|  CLITLIT KIND_PRAGMA CONID		{ $$ = mkclitlit($1, $3); }
	|  NOREP_INTEGER  INTEGER		{ $$ = mknorepi($2); }
	|  NOREP_RATIONAL INTEGER INTEGER	{ $$ = mknorepr($2, $3); }
	|  NOREP_STRING   STRING		{ $$ = mknoreps($2); }
	;

rpats	: rpat					{ $$ = lsing($1); }
	| rpats COMMA rpat			{ $$ = lapp($1,$3); }
	;

rpat	:  qvar					{ $$ = mkrbind($1,mknothing()); }
	|  qvar EQUAL pat			{ $$ = mkrbind($1,mkjust($3)); }
	;


/**********************************************************************
*                                                                     *
*                                                                     *
*     Keywords which record the line start			      *
*                                                                     *
*                                                                     *
**********************************************************************/

importkey:  IMPORT	{ setstartlineno(); }
	;

datakey	:   DATA	{ setstartlineno();
			  if(etags)
#if 1/*etags*/
			    printf("%u\n",startlineno);
#else
			    fprintf(stderr,"%u\tdata\n",startlineno);
#endif
			}
	;

typekey	:   TYPE	{ setstartlineno();
			  if(etags)
#if 1/*etags*/
			    printf("%u\n",startlineno);
#else
			    fprintf(stderr,"%u\ttype\n",startlineno);
#endif
			}
	;

newtypekey : NEWTYPE	{ setstartlineno();
			  if(etags)
#if 1/*etags*/
			    printf("%u\n",startlineno);
#else
			    fprintf(stderr,"%u\tnewtype\n",startlineno);
#endif
			}
	;

instkey	:   INSTANCE	{ setstartlineno();
#if 1/*etags*/
/* OUT:			  if(etags)
			    printf("%u\n",startlineno);
*/
#else
			    fprintf(stderr,"%u\tinstance\n",startlineno);
#endif
			}
	;

defaultkey: DEFAULT	{ setstartlineno(); }
	;

classkey:   CLASS	{ setstartlineno();
			  if(etags)
#if 1/*etags*/
			    printf("%u\n",startlineno);
#else
			    fprintf(stderr,"%u\tclass\n",startlineno);
#endif
			}
	;

minuskey:   MINUS	{ setstartlineno(); }
	;

modulekey:  MODULE	{ setstartlineno();
			  if(etags)
#if 1/*etags*/
			    printf("%u\n",startlineno);
#else
			    fprintf(stderr,"%u\tmodule\n",startlineno);
#endif
			}
	;

oparenkey:  OPAREN	{ setstartlineno(); }
	;

obrackkey:  OBRACK	{ setstartlineno(); }
	;

lazykey	:   LAZY	{ setstartlineno(); }
	;


/**********************************************************************
*                                                                     *
*                                                                     *
*     Basic qualified/unqualified ids/ops                             *
*                                                                     *
*                                                                     *
**********************************************************************/

qvar	:  qvarid
	|  OPAREN qvarsym CPAREN	{ $$ = $2; }
	;
qcon    :  qconid
	|  OPAREN qconsym CPAREN	{ $$ = $2; }
	;
qvarop	:  qvarsym
	|  BQUOTE qvarid BQUOTE		{ $$ = $2; }
	;
qconop	:  qconsym
	|  BQUOTE qconid BQUOTE		{ $$ = $2; }
	;
qop	:  qconop
	|  qvarop
  	;

/* Non "-" op, used in right sections */
qop1	:  qconop
	|  qvarop1
  	;

/* Non "-" varop, used in right sections */
qvarop1	:  QVARSYM
	|  varsym_nominus		{ $$ = mknoqual($1); }
	|  BQUOTE qvarid BQUOTE		{ $$ = $2; }
	;


var	:  varid
	|  OPAREN varsym CPAREN		{ $$ = $2; }
	;
con	:  tycon			/* using tycon removes conflicts */
	|  OPAREN CONSYM CPAREN		{ $$ = $2; }
	;
varop	:  varsym
	|  BQUOTE varid BQUOTE		{ $$ = $2; }
	;
conop	:  CONSYM
	|  BQUOTE CONID BQUOTE		{ $$ = $2; }
	;
op	:  conop
	|  varop
	;

qvark	:  qvarid			{ setstartlineno(); $$ = $1; }
	|  oparenkey qvarsym CPAREN	{ $$ = $2; }
	;
qconk	:  qconid			{ setstartlineno(); $$ = $1; }
	|  oparenkey qconsym CPAREN	{ $$ = $2; }
	;
vark	:  varid			{ setstartlineno(); $$ = $1; }
	|  oparenkey varsym CPAREN	{ $$ = $2; }
	;

qvarid	:  QVARID
	|  varid			{ $$ = mknoqual($1); }
	;
qvarsym	:  QVARSYM
	|  varsym			{ $$ = mknoqual($1); }
	;
qconid	:  QCONID
	|  tycon			{ $$ = mknoqual($1); } /* using tycon removes conflicts */
	;
qconsym	:  QCONSYM
	|  CONSYM			{ $$ = mknoqual($1); }
	;
qtycon	:  QCONID
	|  tycon			{ $$ = mknoqual($1); } /* using tycon removes conflicts */
	;
qtycls  :  QCONID
	|  tycon			{ $$ = mknoqual($1); } /* using tycon removes conflicts */
	;

varsym	:  varsym_nominus
	|  MINUS			{ $$ = install_literal("-"); }
	;

/* AS HIDING QUALIFIED are valid varids */
varid   :  VARID
	|  AS				{ $$ = install_literal("as"); }
	|  HIDING			{ $$ = install_literal("hiding"); }
	|  QUALIFIED			{ $$ = install_literal("qualified"); }
	|  INTERFACE			{ $$ = install_literal("interface"); }
	;

/* DARROW BANG are valid varsyms */
varsym_nominus : VARSYM
	|  DARROW			{ $$ = install_literal("=>"); }
	|  BANG				{ $$ = install_literal("!"); }	
	;

ccallid	:  VARID
	|  CONID
	;

tyvar	:  varid			{ $$ = mknamedtvar($1); }
	;
tycon	:  CONID
	;
modid	:  CONID
	;

tyvar_list: tyvar			{ $$ = lsing($1); }
	|  tyvar_list COMMA tyvar 	{ $$ = lapp($1,$3); }
	;

/**********************************************************************
*                                                                     *
*                                                                     *
*     Stuff to do with layout                                         *
*                                                                     *
*                                                                     *
**********************************************************************/

ocurly	: layout OCURLY				{ hsincindent(); }

vocurly	: layout				{ hssetindent(); }
	;

layout	: 					{ hsindentoff(); }
	;

ccurly	:
	 CCURLY
		{
		  FN = NULL; SAMEFN = 0; PREVPATT = NULL;
		  hsendindent();
		}
	;

vccurly	:  { expect_ccurly = 1; }  vccurly1  { expect_ccurly = 0; }
	;

vccurly1:
	 VCCURLY
		{
		  FN = NULL; SAMEFN = 0; PREVPATT = NULL;
		  hsendindent();
		}
	| error
		{
		  yyerrok;
		  FN = NULL; SAMEFN = 0; PREVPATT = NULL;
		  hsendindent();
		}
	;

%%

/**********************************************************************
*                                                                     *
*      Error Processing and Reporting                                 *
*                                                                     *
*  (This stuff is here in case we want to use Yacc macros and such.)  *
*                                                                     *
**********************************************************************/

/* The parser calls "hsperror" when it sees a
   `report this and die' error.  It sets the stage
   and calls "yyerror".

   There should be no direct calls in the parser to
   "yyerror", except for the one from "hsperror".  Thus,
   the only other calls will be from the error productions
   introduced by yacc/bison/whatever.

   We need to be able to recognise the from-error-production
   case, because we sometimes want to say, "Oh, never mind",
   because the layout rule kicks into action and may save
   the day.  [WDP]
*/

static BOOLEAN error_and_I_mean_it = FALSE;

void
hsperror(s)
  char *s;
{
    error_and_I_mean_it = TRUE;
    yyerror(s);
}

extern char *yytext;
extern int yyleng;

void
yyerror(s)
  char *s;
{
    /* We want to be able to distinguish 'error'-raised yyerrors
       from yyerrors explicitly coded by the parser hacker.
    */
    if (expect_ccurly && ! error_and_I_mean_it ) {
	/*NOTHING*/;

    } else {
	fprintf(stderr, "\"%s\", line %d, column %d: %s on input: ",
	  input_filename, hsplineno, hspcolno + 1, s);

	if (yyleng == 1 && *yytext == '\0')
	    fprintf(stderr, "<EOF>");

	else {
	    fputc('"', stderr);
	    format_string(stderr, (unsigned char *) yytext, yyleng);
	    fputc('"', stderr);
	}
	fputc('\n', stderr);

	/* a common problem */
	if (strcmp(yytext, "#") == 0)
	    fprintf(stderr, "\t(Perhaps you forgot a `-cpp' or `-fglasgow-exts' flag?)\n");

	exit(1);
    }
}

void
format_string(fp, s, len)
  FILE *fp;
  unsigned char *s;
  int len;
{
    while (len-- > 0) {
	switch (*s) {
	case '\0':    fputs("\\NUL", fp);   break;
	case '\007':  fputs("\\a", fp);	    break;
	case '\010':  fputs("\\b", fp);	    break;
	case '\011':  fputs("\\t", fp);	    break;
	case '\012':  fputs("\\n", fp);	    break;
	case '\013':  fputs("\\v", fp);	    break;
	case '\014':  fputs("\\f", fp);	    break;
	case '\015':  fputs("\\r", fp);	    break;
	case '\033':  fputs("\\ESC", fp);   break;
	case '\034':  fputs("\\FS", fp);    break;
	case '\035':  fputs("\\GS", fp);    break;
	case '\036':  fputs("\\RS", fp);    break;
	case '\037':  fputs("\\US", fp);    break;
	case '\177':  fputs("\\DEL", fp);   break;
	default:
    	    if (*s >= ' ')
    		fputc(*s, fp);
	    else
		fprintf(fp, "\\^%c", *s + '@');
	    break;
	}
	s++;
    }
}
