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
static char iface_name[MODNAME_SIZE];
static char interface_filename[FILENAME_SIZE];

static list module_exports;		/* Exported entities */
static list prelude_core_import, prelude_imports;
					/* Entities imported from the Prelude */

extern list all;			/* All valid deriving classes */

extern tree niltree;
extern list Lnil;

extern tree root;

/* For FN, PREVPATT and SAMEFN macros */
extern tree fns[];
extern short samefn[];
extern tree prevpatt[];
extern short icontexts;

/* Line Numbers */
extern int hsplineno, hspcolno;
extern int startlineno;


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

static BOOLEAN hidden = FALSE;		/*  Set when HIDING used        */

extern BOOLEAN inpat;			/*  True when parsing a pattern */
extern BOOLEAN implicitPrelude;		/*  True when we should read the Prelude if not given */
extern BOOLEAN haskell1_3Flag;		/*  True if we are attempting (proto)Haskell 1.3 */

extern int thisIfacePragmaVersion;
%}

%union {
	tree utree;
	list ulist;
	ttype uttype;
	atype uatype;
	binding ubinding;
	pbinding upbinding;
	finfot ufinfo;
	entidt uentid;
	id uid;
	literal uliteral;
	int uint;
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


%token 	VARID 		CONID
	VARSYM		CONSYM		MINUS

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

%token	RARROW
%token	VBAR		EQUAL		DARROW		DOTDOT
%token	DCOLON		LARROW
%token	WILDCARD	AT		LAZY		LAMBDA


/**********************************************************************
*                                                                     *
*                                                                     *
*     Reserved Identifiers                                            *
*                                                                     *
*                                                                     *
**********************************************************************/

%token  LET		IN
%token	WHERE		CASE		OF
%token	TYPE		DATA		CLASS		INSTANCE	DEFAULT
%token	INFIX		INFIXL		INFIXR
%token	MODULE		IMPORT		INTERFACE	HIDING
%token	CCALL		CCALL_GC	CASM		CASM_GC		SCC

%token	IF		THEN		ELSE
%token	RENAMING	DERIVING	TO

/**********************************************************************
*                                                                     *
*                                                                     *
*     Special Symbols for the Lexer                                   *
*                                                                     *
*                                                                     *
**********************************************************************/

%token	LEOF
%token  GHC_PRAGMA END_PRAGMA NO_PRAGMA NOINFO_PRAGMA
%token	ABSTRACT_PRAGMA SPECIALISE_PRAGMA MODNAME_PRAGMA
%token  ARITY_PRAGMA UPDATE_PRAGMA STRICTNESS_PRAGMA KIND_PRAGMA
%token  UNFOLDING_PRAGMA MAGIC_UNFOLDING_PRAGMA DEFOREST_PRAGMA
%token  SPECIALISE_UPRAGMA INLINE_UPRAGMA MAGIC_UNFOLDING_UPRAGMA
%token  ABSTRACT_UPRAGMA DEFOREST_UPRAGMA END_UPRAGMA
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


%left	CASE		LET	IN		LAMBDA
  	IF		ELSE	CCALL		CCALL_GC
	CASM		CASM_GC	SCC		AT

%left	VARSYM		CONSYM	PLUS		MINUS		BQUOTE

%left	DCOLON

%left	SEMI		COMMA

%left	OCURLY		OBRACK		OPAREN

%left 	EQUAL

%right	DARROW
%right	RARROW



/**********************************************************************
*                                                                     *
*                                                                     *
*      Type Declarations                                              *
*                                                                     *
*                                                                     *
**********************************************************************/


%type <ulist>   alt alts altrest quals vars varsrest cons
  		tyvars constrs dtypes types atypes
		types_and_maybe_ids
  		list_exps pats context context_list tyvar_list
		maybeexports export_list
  		impspec maybeimpspec import_list
 		impdecls maybeimpdecls impdecl
  		renaming renamings renaming_list
		tyclses tycls_list
  		gdrhs gdpat valrhs valrhs1
  		lampats
  		upto
		cexp
		idata_pragma_specs idata_pragma_specslist
		gen_pragma_list type_pragma_pairs
		type_pragma_pairs_maybe name_pragma_pairs
		type_maybes
		howto_inline_maybe
		core_binders core_tyvars core_tv_templates
		core_types core_type_list
		core_atoms core_atom_list
		core_alg_alts core_prim_alts corec_binds
		core_type_maybes

%type <uliteral> lit_constant

%type <utree>	exp dexp fexp kexp oexp aexp
		tuple list sequence comprehension qual qualrest
  		gd
 		apat bpat pat apatc conpat dpat fpat opat aapat
 		dpatk fpatk opatk aapatk
  		texps

%type <uid>	MINUS VARID CONID VARSYM CONSYM TYVAR_TEMPLATE_ID
  		var vark con conk varop varop1 conop op op1
		varsym minus plus
		tycls tycon modid ccallid modname_pragma

%type <ubinding>  topdecl topdecls
		  typed datad classd instd defaultd
		  decl decls valdef instdef instdefs
  		  iimport iimports maybeiimports
		  ityped idatad iclassd iinstd ivarsd
  		  itopdecl itopdecls
  		  maybe_where
  		  interface readinterface ibody
		  cbody rinst
  		  impdecl_rest
		  type_and_maybe_id

%type <uttype>    simple type atype btype ttype ntatype
		  class restrict_inst general_inst tyvar type_maybe
		  core_type core_type_maybe

%type <uatype>	  constr

%type <ustring>   FLOAT INTEGER INTPRIM
		  FLOATPRIM DOUBLEPRIM CLITLIT
%type <uhstring>  STRING STRINGPRIM CHAR CHARPRIM
%type <uentid>	  export import

%type <uhpragma>  idata_pragma idata_pragma_spectypes
		  itype_pragma iclas_pragma iclasop_pragma
		  iinst_pragma gen_pragma ival_pragma arity_pragma
		  update_pragma strictness_pragma worker_info
		  deforest_pragma
		  unfolding_pragma unfolding_guidance type_pragma_pair
		  name_pragma_pair

%type <ucoresyn>  core_expr core_case_alts core_id core_binder core_atom
		  core_alg_alt core_prim_alt core_default corec_bind
		  co_primop co_scc co_caf co_dupd

/**********************************************************************
*                                                                     *
*                                                                     *
*      Start Symbol for the Parser                                    *
*                                                                     *
*                                                                     *
**********************************************************************/

%start pmodule


%%

pmodule	:  readpreludecore readprelude module
	;

module	:  modulekey modid maybeexports
		{ the_module_name = $2; module_exports = $3; }
	   WHERE body
	|	{ the_module_name = install_literal("Main"); module_exports = Lnil; }
	   body
	;

	/* all the startlinenos in mkhmodules are bogus (WDP) */
body	:  ocurly maybeimpdecls maybefixes topdecls ccurly
	       {
		 root = mkhmodule(the_module_name,lconc(prelude_imports,$2),module_exports,$4,startlineno);
	       }
	|  vocurly maybeimpdecls maybefixes topdecls vccurly
	       {
		 root = mkhmodule(the_module_name,lconc(prelude_imports,$2),module_exports,$4,startlineno);
	       }

	|  vocurly impdecls vccurly
	       {
		 root = mkhmodule(the_module_name,lconc(prelude_imports,$2),module_exports,mknullbind(),startlineno);
	       }
	|  ocurly impdecls ccurly
	       {
		 root = mkhmodule(the_module_name,lconc(prelude_imports,$2),module_exports,mknullbind(),startlineno);
	       }

/* Adds 1 S/R, 2 R/R conflicts, alternatives add 3 R/R conflicts */
	|  vocurly maybeimpdecls vccurly
	       {
		 root = mkhmodule(the_module_name,lconc(prelude_imports,$2),module_exports,mknullbind(),startlineno);
	       }
	|  ocurly maybeimpdecls ccurly
	       {
		 root = mkhmodule(the_module_name,lconc(prelude_imports,$2),module_exports,mknullbind(),startlineno);
	       }
	;


maybeexports :	/* empty */			{ $$ = Lnil; }
	|  OPAREN export_list CPAREN		{ $$ = $2; }
	;

export_list:
	   export				{ $$ = lsing($1); }
  	|  export_list COMMA export		{ $$ = lapp($1, $3); }
	;

export	:
	   var					{ $$ = mkentid($1); }
	|  tycon				{ $$ = mkenttype($1); }
	|  tycon OPAREN DOTDOT CPAREN		{ $$ = mkenttypeall($1); }
	|  tycon OPAREN cons CPAREN
		{ $$ = mkenttypecons($1,$3);
		  /* should be a datatype with cons representing all constructors */
		}
	|  tycon OPAREN vars CPAREN
		{ $$ = mkentclass($1,$3);
		  /* should be a class with vars representing all Class operations */
		}
	|  tycon OPAREN CPAREN
    	    	{ $$ = mkentclass($1,Lnil);
    	    	  /* "tycon" should be a class with no operations */
		}
	|  tycon DOTDOT
		{ $$ = mkentmod($1);
		  /* "tycon" is a module id (but "modid" is bad for your identifier's health [KH])  */
		}
	;


impspec	:  OPAREN import_list CPAREN		{ $$ = $2; hidden = FALSE; }
	|  HIDING OPAREN import_list CPAREN	{ $$ = $3; hidden = TRUE; }
	|  OPAREN CPAREN			{ $$ = Lnil; hidden = FALSE; }
  	;

maybeimpspec :	/* empty */			{ $$ = Lnil; }
	|  impspec				{ $$ = $1; }
	;

import_list:
	   import				{ $$ = lsing($1); }
	|  import_list COMMA import		{ $$ = lapp($1, $3); }
	;

import	:
	   var					{ $$ = mkentid($1); }
	|  tycon				{ $$ = mkenttype($1); }
	|  tycon OPAREN DOTDOT CPAREN		{ $$ = mkenttypeall($1); }
	|  tycon OPAREN cons CPAREN
		{ $$ = mkenttypecons($1,$3);
		  /* should be a datatype with cons representing all constructors */
		}
	|  tycon OPAREN vars CPAREN
		{ $$ = mkentclass($1,$3);
		  /* should be a class with vars representing all Class operations */
		}
	|  tycon OPAREN CPAREN
    	    	{ $$ = mkentclass($1,Lnil);
    	    	  /* "tycon" should be a class with no operations */
		}
	;

/* -- interface pragma stuff: ------------------------------------- */

idata_pragma:
	   GHC_PRAGMA constrs idata_pragma_specs END_PRAGMA
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

itype_pragma:
	   GHC_PRAGMA ABSTRACT_PRAGMA END_PRAGMA    { $$ = mkitype_pragma(); }
	|  /* empty */			    	    { $$ = mkno_pragma(); }
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
	   GHC_PRAGMA modname_pragma gen_pragma END_PRAGMA
		{ $$ = mkiinst_simpl_pragma($2, $3); }

	|  GHC_PRAGMA modname_pragma gen_pragma name_pragma_pairs END_PRAGMA
		{ $$ = mkiinst_const_pragma($2, $3, $4); }

	|  /* empty */
		{ $$ = mkno_pragma(); }
	;

modname_pragma:
	  MODNAME_PRAGMA modid
		{ $$ = $2; }
	| /* empty */
		{ $$ = install_literal(""); }
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
	   NO_PRAGMA			{ $$ = mkty_maybe_nothing(); }
    	|  type				{ $$ = mkty_maybe_just($1); }
	;

name_pragma_pairs:
	   name_pragma_pair			    { $$ = lsing($1); }
	|  name_pragma_pairs COMMA name_pragma_pair { $$ = lapp($1, $3); }
	;

name_pragma_pair:
	   /* if the gen_pragma concludes with a *comma*- */
	   /* separated SPECs list, we get a parse error; */
	   /* we have to bracket the gen_pragma           */

	   var EQUAL OCURLY gen_pragma CCURLY
		{ $$ = mkiname_pragma_pr($1, $4); }

	   /* we keep the old form for backwards compatability */
	   /* ToDo: rm */

	|  var EQUAL gen_pragma
		{ $$ = mkiname_pragma_pr($1, $3); }

	   /* need bracketed form when we have spec pragmas to avoid list confusion */
	;

/* -- end of interface pragma stuff ------------------------------- */

/* -- core syntax stuff ------------------------------------------- */

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
	   NO_PRAGMA			{ $$ = mkty_maybe_nothing(); }
    	|  core_type			{ $$ = mkty_maybe_just($1); }
	;

/* -- end of core syntax stuff ------------------------------------ */

readpreludecore :
	        {
		  if ( implicitPrelude && !etags ) {
		     /* we try to avoid reading interfaces when etagging */
		     find_module_on_imports_dirlist(
			(haskell1_3Flag) ? "PrelCore13" : "PreludeCore",
			TRUE,interface_filename);
		  } else {
		     find_module_on_imports_dirlist("PreludeNull_",TRUE,interface_filename);
		  }
		  thisIfacePragmaVersion = 0;
		  setyyin(interface_filename);
		  enteriscope();
		}
	   readinterface
		{
		  binding prelude_core = mkimport(installid(iface_name),Lnil,Lnil,$2,xstrdup(interface_filename),hsplineno);
		  prelude_core_import = implicitPrelude? lsing(prelude_core): Lnil;
		  
		}
	;

readprelude :
	        {
		  if ( implicitPrelude && !etags ) {
		     find_module_on_imports_dirlist(
			( haskell1_3Flag ) ? "Prel13" : "Prelude",
			TRUE,interface_filename);
		  } else {
		     find_module_on_imports_dirlist("PreludeNull_",TRUE,interface_filename);
		  }
		  thisIfacePragmaVersion = 0;
		  setyyin(interface_filename);
		  enteriscope();
		}
	   readinterface
		{
		  binding prelude = mkimport(installid(iface_name),Lnil,Lnil,$2,xstrdup(interface_filename),hsplineno);
		  prelude_imports = (! implicitPrelude) ? Lnil
					: lconc(prelude_core_import,lsing(prelude));
		}
	;

maybeimpdecls : /* empty */			{ $$ = Lnil; }
	|  impdecls SEMI			{ $$ = $1; }
	;

impdecls:  impdecl				{ $$ = $1; }
	|  impdecls SEMI impdecl		{ $$ = lconc($1,$3); }
	;

impdecl	:  IMPORT modid
		{ /* filename returned in "interface_filename" */
		  char *module_name = id_to_string($2);
		  if ( ! etags ) {
		      find_module_on_imports_dirlist(
			(haskell1_3Flag && strcmp(module_name, "Prelude") == 0)
			    ? "Prel13" : module_name,
			FALSE, interface_filename);
		  } else {
		     find_module_on_imports_dirlist("PreludeNull_",TRUE,interface_filename);
		  }
		  thisIfacePragmaVersion = 0;
		  setyyin(interface_filename);
		  enteriscope();
		  if (strcmp(module_name,"PreludeCore")==0) {
		    hsperror("Cannot explicitly import `PreludeCore'");

		  } else if (strcmp(module_name,"Prelude")==0) {
		    prelude_imports = prelude_core_import; /* unavoidable */
		  }
		}
	   impdecl_rest
		{
		  if (hidden)
		    $4->tag = hiding;
		  $$ = lsing($4);
		}

impdecl_rest:
	   readinterface maybeimpspec
		{ $$ = mkimport(installid(iface_name),$2,Lnil,$1,xstrdup(interface_filename),hsplineno); }
		/* WDP: uncertain about those hsplinenos */
	|  readinterface maybeimpspec RENAMING renamings
		{ $$ = mkimport(installid(iface_name),$2,$4,$1,xstrdup(interface_filename),hsplineno); }
	;

readinterface:
	   interface LEOF
		{
		  exposeis(); /* partain: expose infix ops at level i+1 to level i */
		  $$ = $1;
		}
	;

renamings: OPAREN renaming_list CPAREN		{ $$ = $2; }
	;

renaming_list:
	   renaming				{ $$ = lsing($1); }
	|  renaming_list COMMA renaming		{ $$ = lapp($1, $3); }
	;

renaming:  var TO var				{ $$ = ldub($1,$3); }
	|  con TO con				{ $$ = ldub($1,$3); }
	;

maybeiimports : /* empty */			{ $$ = mknullbind(); }
	|  iimports SEMI			{ $$ = $1; }
	;

iimports : iimport				{ $$ = $1; }
	 | iimports SEMI iimport		{ $$ = mkabind($1,$3); }
	 ;

iimport :  importkey modid OPAREN import_list CPAREN
		{ $$ = mkmbind($2,$4,Lnil,startlineno); }
	|  importkey modid OPAREN import_list CPAREN RENAMING renamings
		{ $$ = mkmbind($2,$4,$7,startlineno); }
	;


interface:
  	   INTERFACE modid
		{ /* OLD 95/08: fixlist = Lnil; */
		  strcpy(iface_name, id_to_string($2));
		}
	   WHERE ibody
		{
		/* WDP: not only do we not check the module name
		   but we take the one in the interface to be what we really want
		   -- we need this for Prelude jiggery-pokery.  (Blech.  KH)
		   ToDo: possibly revert....
		   checkmodname(modname,id_to_string($2));
		*/
		  $$ = $5;
		}
	;


ibody	:  ocurly maybeiimports maybefixes itopdecls ccurly
		{
		  $$ = mkabind($2,$4);
		}
	|  ocurly iimports ccurly
		{
		  $$ = $2;
		}
	|  vocurly maybeiimports maybefixes itopdecls vccurly
		{
		  $$ = mkabind($2,$4);
		}
	|  vocurly iimports vccurly
		{
		  $$ = $2;
		}
  	;

maybefixes:  /* empty */
	|  fixes SEMI
	;


fixes	:  fix
	|  fixes SEMI fix
	;

fix	:  INFIXL INTEGER
  		{ Precedence = checkfixity($2); Fixity = INFIXL; }
	   ops
	|  INFIXR INTEGER
  		{ Precedence = checkfixity($2); Fixity = INFIXR; }
	   ops
	|  INFIX  INTEGER
  		{ Precedence = checkfixity($2); Fixity = INFIX; }
	   ops
	|  INFIXL
  		{ Fixity = INFIXL; Precedence = 9; }
	   ops
	|  INFIXR
  		{ Fixity = INFIXR; Precedence = 9; }
	   ops
	|  INFIX
  		{ Fixity = INFIX; Precedence = 9; }
	   ops
	;

ops	:  op				{ makeinfix(id_to_string($1),Fixity,Precedence); }
	|  ops COMMA op			{ makeinfix(id_to_string($3),Fixity,Precedence); }
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
	|  classd 				{ $$ = $1; }
	|  instd 				{ $$ = $1; }
	|  defaultd 				{ $$ = $1; }
	|  decl 				{ $$ = $1; }
	;

typed	:  typekey simple EQUAL type		{ $$ = mknbind($2,$4,startlineno,mkno_pragma()); }
	;


datad	:  datakey context DARROW simple EQUAL constrs
		{ $$ = mktbind($2,$4,$6,all,startlineno,mkno_pragma()); }
	|  datakey simple EQUAL constrs
		{ $$ = mktbind(Lnil,$2,$4,all,startlineno,mkno_pragma()); }
	|  datakey context DARROW simple EQUAL constrs DERIVING tyclses
		{ $$ = mktbind($2,$4,$6,$8,startlineno,mkno_pragma()); }
	|  datakey simple EQUAL constrs DERIVING tyclses
		{ $$ = mktbind(Lnil,$2,$4,$6,startlineno,mkno_pragma()); }
	;

classd	:  classkey context DARROW class cbody	{ $$ = mkcbind($2,$4,$5,startlineno,mkno_pragma()); }
	|  classkey class cbody		 	{ $$ = mkcbind(Lnil,$2,$3,startlineno,mkno_pragma()); }
	;

cbody	:  /* empty */				{ $$ = mknullbind(); }
	|  WHERE ocurly decls ccurly		{ checkorder($3); $$ = $3; }
	|  WHERE vocurly decls vccurly		{ checkorder($3); $$ =$3; }
	;

instd	:  instkey context DARROW tycls restrict_inst rinst	{ $$ = mkibind($2,$4,$5,$6,startlineno,mkno_pragma()); }
	|  instkey tycls general_inst rinst		 	{ $$ = mkibind(Lnil,$2,$3,$4,startlineno,mkno_pragma()); }
	;

rinst	:  /* empty */			  { $$ = mknullbind(); }
	|  WHERE ocurly  instdefs ccurly  { $$ = $3; }
	|  WHERE vocurly instdefs vccurly { $$ = $3; }
	;

restrict_inst : tycon				{ $$ = mktname($1,Lnil); }
	|  OPAREN tycon tyvars	CPAREN		{ $$ = mktname($2,$3); }
	|  OPAREN tyvar COMMA tyvar_list CPAREN	{ $$ = mkttuple(mklcons($2,$4)); }
	|  OPAREN CPAREN			{ $$ = mkttuple(Lnil); }
	|  OBRACK tyvar CBRACK			{ $$ = mktllist($2); }
	|  OPAREN tyvar RARROW tyvar CPAREN	{ $$ = mktfun($2,$4); }
	;

general_inst : tycon				{ $$ = mktname($1,Lnil); }
	|  OPAREN tycon atypes CPAREN		{ $$ = mktname($2,$3); }
	|  OPAREN type COMMA types CPAREN	{ $$ = mkttuple(mklcons($2,$4)); }
	|  OPAREN CPAREN			{ $$ = mkttuple(Lnil); }
	|  OBRACK type CBRACK			{ $$ = mktllist($2); }
	|  OPAREN btype RARROW type CPAREN	{ $$ = mktfun($2,$4); }
	;

defaultd:  defaultkey dtypes { $$ = mkdbind($2,startlineno); }
	;

dtypes	:  OPAREN type COMMA types CPAREN	{ $$ = mklcons($2,$4); }
	|  ttype				{ $$ = lsing($1); }
/*	Omitting the next forces () to be the *type* (), which never defaults.
    	This is a KLUDGE.  (Putting this in adds piles of r/r conflicts.)
*/
/*	|  OPAREN CPAREN			{ $$ = Lnil; }*/
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

/* partain: this "DCOLON context" vs "DCOLON type" is a problem,
    because you can't distinguish between

	foo :: (Baz a, Baz a)
	bar :: (Baz a, Baz a) => [a] -> [a] -> [a]

    with one token of lookahead.  The HACK is to have "DCOLON ttype"
    [tuple type] in the first case, then check that it has the right
    form C a, or (C1 a, C2 b, ... Cn z) and convert it into a
    context.  Blaach!
    (FIXED 90/06/06)

    Note: if there is an iclasop_pragma there, then we must be
    doing a class-op in an interface -- unless the user is up
    to real mischief (ugly, but likely to work).
*/

decl	:  vars DCOLON type DARROW type iclasop_pragma
		{ /* type2context.c for code */
		  $$ = mksbind($1,mkcontext(type2context($3),$5),startlineno,$6);
		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}
	|  vars DCOLON type iclasop_pragma
		{
		  $$ = mksbind($1,$3,startlineno,$4);
		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}

	/* User-specified pragmas come in as "signatures"...
	   They are similar in that they can appear anywhere in the module,
	   and have to be "joined up" with their related entity.

	   Have left out the case specialising to an overloaded type.
	   Let's get real, OK?  (WDP)
	*/
	|  SPECIALISE_UPRAGMA vark DCOLON types_and_maybe_ids END_UPRAGMA
		{
		  $$ = mkvspec_uprag($2, $4, startlineno);
		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}

	|  SPECIALISE_UPRAGMA INSTANCE CONID general_inst END_UPRAGMA
		{
		  $$ = mkispec_uprag($3, $4, startlineno);
		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}

	|  SPECIALISE_UPRAGMA DATA tycon atypes END_UPRAGMA
		{
		  $$ = mkdspec_uprag($3, $4, startlineno);
		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}

	|  INLINE_UPRAGMA vark howto_inline_maybe END_UPRAGMA
		{
		  $$ = mkinline_uprag($2, $3, startlineno);
		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}

	|  MAGIC_UNFOLDING_UPRAGMA vark vark END_UPRAGMA
		{
		  $$ = mkmagicuf_uprag($2, $3, startlineno);
		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}

        |  DEFOREST_UPRAGMA vark END_UPRAGMA
                {
		  $$ = mkdeforest_uprag($2, startlineno);
 		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}

	|  ABSTRACT_UPRAGMA tycon END_UPRAGMA
		{
		  $$ = mkabstract_uprag($2, startlineno);
		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}

	/* end of user-specified pragmas */

	|  valdef
	|  /* empty */ { $$ = mknullbind(); PREVPATT = NULL; FN = NULL; SAMEFN = 0; }
  	;

howto_inline_maybe :
	  /* empty */	{ $$ = Lnil; }
	|  CONID	{ $$ = lsing($1); }

types_and_maybe_ids :
	   type_and_maybe_id				{ $$ = lsing($1); }
	|  types_and_maybe_ids COMMA type_and_maybe_id	{ $$ = lapp($1,$3); }
	;

type_and_maybe_id :
	   type					{ $$ = mkvspec_ty_and_id($1,Lnil); }
	|  type EQUAL vark			{ $$ = mkvspec_ty_and_id($1,lsing($3)); }

itopdecls : itopdecl				{ $$ = $1; }
	| itopdecls SEMI itopdecl		{ $$ = mkabind($1,$3); }
  	;

itopdecl:  ityped	 			{ $$ = $1; }
	|  idatad 				{ $$ = $1; }
	|  iclassd 				{ $$ = $1; }
	|  iinstd 				{ $$ = $1; }
	|  ivarsd				{ $$ = $1; }
	|  /* empty */				{ $$ = mknullbind(); }
	;

   	/* partain: see comment elsewhere about why "type", not "context" */
ivarsd	:  vars DCOLON type DARROW type ival_pragma
		{ $$ = mksbind($1,mkcontext(type2context($3),$5),startlineno,$6); }
	|  vars DCOLON type ival_pragma
		{ $$ = mksbind($1,$3,startlineno,$4); }
	;

ityped	:  typekey simple EQUAL type itype_pragma
		{ $$ = mknbind($2,$4,startlineno,$5); }
	;

idatad	:  datakey context DARROW simple idata_pragma
		{ $$ = mktbind($2,$4,Lnil,Lnil,startlineno,$5); }
  	|  datakey simple idata_pragma
		{ $$ = mktbind(Lnil,$2,Lnil,Lnil,startlineno,$3); }
	|  datakey context DARROW simple EQUAL constrs idata_pragma
		{ $$ = mktbind($2,$4,$6,Lnil,startlineno,$7); }
	|  datakey simple EQUAL constrs idata_pragma
		{ $$ = mktbind(Lnil,$2,$4,Lnil,startlineno,$5); }
	|  datakey context DARROW simple EQUAL constrs DERIVING tyclses
		{ $$ = mktbind($2,$4,$6,$8,startlineno,mkno_pragma()); }
	|  datakey simple EQUAL constrs DERIVING tyclses
		{ $$ = mktbind(Lnil,$2,$4,$6,startlineno,mkno_pragma()); }
	;

iclassd	:  classkey context DARROW class iclas_pragma cbody
		{ $$ = mkcbind($2,$4,$6,startlineno,$5); }
	|  classkey class iclas_pragma cbody
		{ $$ = mkcbind(Lnil,$2,$4,startlineno,$3); }
	;

iinstd	:  instkey context DARROW tycls general_inst iinst_pragma
		{ $$ = mkibind($2,$4,$5,mknullbind(),startlineno,$6); }
	|  instkey tycls general_inst iinst_pragma
		{ $$ = mkibind(Lnil,$2,$3,mknullbind(),startlineno,$4); }
	;


/* obsolete: "(C a, ...)" cause r/r conflict, resolved in favour of context rather than type */

class	:  tycon tyvar 				{ $$ = mktname($1,lsing($2)); }
    	    /* partain: changed "tycls" to "tycon" */
	;

types	:  type					{ $$ = lsing($1); }
	|  types COMMA type			{ $$ = lapp($1,$3); }
	;

type	:  btype				{ $$ = $1; }
	|  btype RARROW type			{ $$ = mktfun($1,$3); }

	|  FORALL core_tv_templates DARROW type
		{ $$ = mkuniforall($2, $4); }

btype	:  atype				{ $$ = $1; }
	|  tycon atypes				{ $$ = mktname($1,$2); }
	;

atypes	:  atypes atype				{ $$ = lapp($1,$2); }
	|  atype				{ $$ = lsing($1); }
	;

/* The split with ntatype allows us to use the same syntax for defaults as for types */
ttype	:  ntatype				{ $$ = $1; }
	|  btype RARROW type			{ $$ = mktfun($1,$3); }
	|  tycon atypes				{ $$ = mktname($1,$2); }
	;

atype	:  ntatype
	|  OPAREN type COMMA types CPAREN	{ $$ = mkttuple(mklcons($2,$4)); }
	;

ntatype	:  tyvar		{ $$ = $1; }
  	|  tycon		{ $$ = mktname($1,Lnil); }
	|  OPAREN CPAREN	{ $$ = mkttuple(Lnil); }
	|  OPAREN type CPAREN	{ $$ = $2; }
	|  OBRACK type CBRACK	{ $$ = mktllist($2); }

	|  OCURLY OCURLY CONID type CCURLY CCURLY
				{ $$ = mkunidict($3, $4); }
	|  TYVAR_TEMPLATE_ID	{ $$ = mkunityvartemplate($1); }
	;


simple	:  tycon		{ $$ = mktname($1,Lnil); }
	|  tycon tyvars		{ $$ = mktname($1,$2); }
	;

constrs	:  constr		{ $$ = lsing($1); }
	|  constrs VBAR constr	{ $$ = lapp($1,$3); }
	;

/* Using tycon rather than con avoids 5 S/R errors */
constr	:  tycon atypes				{ $$ = mkatc($1,$2,hsplineno); }
	|  OPAREN CONSYM CPAREN atypes		{ $$ = mkatc($2,$4,hsplineno); }
	|  tycon				{ $$ = mkatc($1,Lnil,hsplineno); }
	|  OPAREN CONSYM CPAREN			{ $$ = mkatc($2,Lnil,hsplineno); }
	|  btype conop btype			{ $$ = mkatc($2, ldub($1,$3),hsplineno); }
	;

tyclses	:  OPAREN tycls_list CPAREN		{ $$ = $2; }
	|  OPAREN CPAREN			{ $$ = Lnil; }
	|  tycls				{ $$ = lsing($1); }
	;

tycls_list:  tycls				{ $$ = lsing($1); }
	|  tycls_list COMMA tycls		{ $$ = lapp($1,$3); }
	;

context	:  OPAREN context_list CPAREN		{ $$ = $2; }
	|  class				{ $$ = lsing($1); }
	;

context_list:  class				{ $$ = lsing($1); }
	|  context_list COMMA class	 	{ $$ = lapp($1,$3); }
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
	   SPECIALISE_UPRAGMA vark DCOLON types_and_maybe_ids END_UPRAGMA
		{
		  $$ = mkvspec_uprag($2, $4, startlineno);
		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}

	|  INLINE_UPRAGMA vark howto_inline_maybe END_UPRAGMA
		{
		  $$ = mkinline_uprag($2, $3, startlineno);
		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}

	|  MAGIC_UNFOLDING_UPRAGMA vark vark END_UPRAGMA
		{
		  $$ = mkmagicuf_uprag($2, $3, startlineno);
		  PREVPATT = NULL; FN = NULL; SAMEFN = 0;
		}

	|  valdef
  	;


vars	:  vark COMMA varsrest			{ $$ = mklcons($1,$3); }
	|  vark					{ $$ = lsing($1); }
	;

varsrest:  var					{ $$ = lsing($1); }
	|  varsrest COMMA var	 		{ $$ = lapp($1,$3); }
	;

cons	:  con					{ $$ = lsing($1); }
	|  cons COMMA con	 		{ $$ = lapp($1,$3); }
	;


valdef	:  opatk
		{
		  tree fn = function($1);

		  PREVPATT = $1;

		  if(ttree(fn) == ident)
		    {
		      checksamefn(gident((struct Sident *) fn));
		      FN = fn;
		    }

		  else if (ttree(fn) == tinfixop && ttree(ginfun((struct Sap *) fn)) == ident)
		    {
		      checksamefn(gident((struct Sident *) (ginfun((struct Sap *) fn))));
		      FN = ginfun((struct Sap *) fn);
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

valrhs1	:  gdrhs
	|  EQUAL exp				{ $$ = lsing(mktruecase($2)); }
	;

gdrhs	:  gd EQUAL exp				{ $$ = lsing(ldub($1,$3)); }
	|  gd EQUAL exp gdrhs			{ $$ = mklcons(ldub($1,$3),$4); }
	;

maybe_where:
	   WHERE ocurly decls ccurly		{ $$ = $3; }
	|  WHERE vocurly decls vccurly		{ $$ = $3; }
	|  /* empty */				{ $$ = mknullbind(); }
	;

gd	:  VBAR oexp				{ $$ = $2; }
	;


lampats	:  apat lampats				{ $$ = mklcons($1,$2); }
	|  apat					{ $$ = lsing($1); }
	/* right recursion? (WDP) */
	;


/*
  	Changed as above to allow for contexts!
	KH@21/12/92
*/

exp	:  oexp DCOLON type DARROW type		{ $$ = mkrestr($1,mkcontext(type2context($3),$5)); }
	|  oexp DCOLON type	  		{ $$ = mkrestr($1,$3); }
	|  oexp
	;

/*
  Operators must be left-associative  at the same precedence
  for prec. parsing to work.
*/

 	/* Infix operator application */
oexp	:  dexp
	|  oexp op oexp %prec PLUS
		{ $$ = mkinfixop($2,$1,$3); precparse($$); }
	;

/*
  This comes here because of the funny precedence rules concerning
  prefix minus.
*/


dexp	:  MINUS kexp				{ $$ = mknegate($2); }
	|  kexp
	;

/*
  let/if/lambda/case have higher precedence than infix operators.
*/

kexp	:  LAMBDA
		{ /* enteriscope(); /? I don't understand this -- KH */
		  hsincindent();    /* added by partain; push new context for */
				    /* FN = NULL; not actually concerned about */
		  FN = NULL;	    /* indenting */
		  $<uint>$ = hsplineno; /* remember current line number */
		}
	   lampats
  		{ hsendindent();    /* added by partain */
		  /* exitiscope();	    /? Also not understood */
		}
	   RARROW exp	/* lambda abstraction */
		{
		  $$ = mklambda($3, $6, $<uint>2);
		}

	/* Let Expression */
	|  LET ocurly decls ccurly IN exp	{ $$ = mklet($3,$6); }
	|  LET vocurly decls vccurly IN exp	{ $$ = mklet($3,$6); }

	/* If Expression */
	|  IF exp THEN exp ELSE exp		{ $$ = mkife($2,$4,$6); }

	/* Case Expression */
	|  CASE exp OF ocurly alts ccurly	{ $$ = mkcasee($2,$5); }
	|  CASE exp OF vocurly alts vccurly	{ $$ = mkcasee($2,$5); }

	/* CCALL/CASM Expression */
	|  CCALL ccallid cexp			{ $$ = mkccall($2,installid("n"),$3); }
	|  CCALL ccallid			{ $$ = mkccall($2,installid("n"),Lnil); }
	|  CCALL_GC ccallid cexp		{ $$ = mkccall($2,installid("p"),$3); }
	|  CCALL_GC ccallid 			{ $$ = mkccall($2,installid("p"),Lnil); }
	|  CASM CLITLIT cexp			{ $$ = mkccall($2,installid("N"),$3); }
	|  CASM CLITLIT				{ $$ = mkccall($2,installid("N"),Lnil); }
	|  CASM_GC CLITLIT cexp			{ $$ = mkccall($2,installid("P"),$3); }
	|  CASM_GC CLITLIT 			{ $$ = mkccall($2,installid("P"),Lnil); }

	/* SCC Expression */
	|  SCC STRING exp
		{ if (ignoreSCC) {
		    if (warnSCC)
			fprintf(stderr,
				"\"%s\", line %d: _scc_ (`set [profiling] cost centre') ignored\n",
			    	input_filename, hsplineno);
		    $$ = $3;
		  } else {
		    $$ = mkscc($2, $3);
		  }
		}
	|  fexp
  	;


	/* Function application */
fexp	:  fexp aexp				{ $$ = mkap($1,$2); }
	|  aexp
	;

cexp	:  cexp aexp				{ $$ = lapp($1,$2); }
	|  aexp					{ $$ = lsing($1); }
	;

/*
   The mkpars are so that infix parsing doesn't get confused.

   KH.
*/

	/* Simple Expressions */
aexp	:  var					{ $$ = mkident($1); }
	|  con		 			{ $$ = mkident($1); }
	|  lit_constant				{ $$ = mklit($1); }
	|  OPAREN exp CPAREN			{ $$ = mkpar($2); }
	|  OPAREN oexp op CPAREN		{ checkprec($2,$3,FALSE); $$ = mklsection($2,$3); }
	|  OPAREN op1 oexp CPAREN		{ checkprec($3,$2,TRUE);  $$ = mkrsection($2,$3); }

	/* structures */
	|  tuple
	|  list					{ $$ = mkpar($1); }
	|  sequence				{ $$ = mkpar($1); }
	|  comprehension			{ $$ = mkpar($1); }

	/* These only occur in patterns */
	|  var AT aexp				{ checkinpat();  $$ = mkas($1,$3); }
	|  WILDCARD				{ checkinpat();  $$ = mkwildp();   }
	|  LAZY aexp				{ checkinpat();  $$ = mklazyp($2); }
	;


/*
	LHS patterns are parsed in a similar way to
	expressions.  This avoids the horrible non-LRness
	which occurs with the 1.1 syntax.

	The xpatk business is to do with accurately recording
	the starting line for definitions.
*/

opatk	:  dpatk
	|  opatk op opat %prec PLUS
		{
		  $$ = mkinfixop($2,$1,$3);

		  if(isconstr(id_to_string($2)))
		    precparse($$);
		  else
		    {
		      checkprec($1,$2,FALSE);	/* Check the precedence of the left pattern */
		      checkprec($3,$2,TRUE);	/* then check the right pattern */
		    }
		}
	;

opat	:  dpat
	|  opat op opat %prec PLUS
		{
		  $$ = mkinfixop($2,$1,$3);

		  if(isconstr(id_to_string($2)))
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


dpat	:  MINUS fpat					{ $$ = mknegate($2); }
	|  fpat
	;

	/* Function application */
fpat	:  fpat aapat					{ $$ = mkap($1,$2); }
	|  aapat
	;

dpatk	:  minuskey fpat				{ $$ = mknegate($2); }
	|  fpatk
	;

	/* Function application */
fpatk	:  fpatk aapat					{ $$ = mkap($1,$2); }
	|  aapatk
	;

aapat	:  con		 				{ $$ = mkident($1); }
	|  var		 				{ $$ = mkident($1); }
	|  var AT apat			 		{ $$ = mkas($1,$3); }
	|  lit_constant					{ $$ = mklit($1); }
	|  WILDCARD					{ $$ = mkwildp(); }
	|  OPAREN CPAREN				{ $$ = mktuple(Lnil); }
	|  OPAREN var PLUS INTEGER CPAREN		{ $$ = mkplusp(mkident($2),mkinteger($4)); }
/* GHC cannot do these anyway. WDP 93/11/15
	|  OPAREN WILDCARD PLUS INTEGER CPAREN		{ $$ = mkplusp(mkwildp(),mkinteger($4)); }
*/
	|  OPAREN opat CPAREN				{ $$ = mkpar($2); }
	|  OPAREN opat COMMA pats CPAREN 		{ $$ = mktuple(mklcons($2,$4)); }
	|  OBRACK pats CBRACK				{ $$ = mkllist($2); }
	|  OBRACK CBRACK				{ $$ = mkllist(Lnil); }
	|  LAZY apat					{ $$ = mklazyp($2); }
	;

aapatk	:  conk		 				{ $$ = mkident($1); }
	|  vark		 				{ $$ = mkident($1); }
	|  vark AT apat			 		{ $$ = mkas($1,$3); }
	|  lit_constant					{ $$ = mklit($1); setstartlineno(); }
	|  WILDCARD					{ $$ = mkwildp(); setstartlineno(); }
	|  oparenkey CPAREN				{ $$ = mktuple(Lnil); }
	|  oparenkey var PLUS INTEGER CPAREN		{ $$ = mkplusp(mkident($2),mkinteger($4)); }
/* GHC no cannae do (WDP 95/05)
	|  oparenkey WILDCARD PLUS INTEGER CPAREN	{ $$ = mkplusp(mkwildp(),mkinteger($4)); }
*/
	|  oparenkey opat CPAREN			{ $$ = mkpar($2); }
	|  oparenkey opat COMMA pats CPAREN 		{ $$ = mktuple(mklcons($2,$4)); }
	|  obrackkey pats CBRACK			{ $$ = mkllist($2); }
	|  obrackkey CBRACK				{ $$ = mkllist(Lnil); }
	|  lazykey apat					{ $$ = mklazyp($2); }
	;


tuple	:  OPAREN exp COMMA texps CPAREN
  		{ if (ttree($4) == tuple)
		    $$ = mktuple(mklcons($2, gtuplelist((struct Stuple *) $4)));
		else
		  $$ = mktuple(ldub($2, $4));
		}
	|  OPAREN CPAREN
		{ $$ = mktuple(Lnil); }
	;

/*
   The mkpar is so that infix parsing doesn't get confused.

   KH.
*/
texps	:  exp	{ $$ = mkpar($1); }
	|  exp COMMA texps
		{ if (ttree($3) == tuple)
		    $$ = mktuple(mklcons($1, gtuplelist((struct Stuple *) $3)));
		else
		  $$ = mktuple(ldub($1, $3));
		}
	/* right recursion? WDP */
	;


list	:  OBRACK CBRACK  			{ $$ = mkllist(Lnil); }
  	|  OBRACK list_exps CBRACK		{ $$ = mkllist($2); }
	;

list_exps :
	   exp					{ $$ = lsing($1); }
	|  exp COMMA list_exps			{ $$ = mklcons($1, $3); }
	/* right recursion? (WDP)

	   It has to be this way, though, otherwise you
	   may do the wrong thing to distinguish between...

	   [ e1 , e2 .. ]	-- an enumeration ...
	   [ e1 , e2 , e3 ]	-- a list

	   (In fact, if you change the grammar and throw yacc/bison
	   at it, it *will* do the wrong thing [WDP 94/06])
	*/
	;


sequence:  OBRACK exp COMMA exp DOTDOT upto CBRACK	{$$ = mkeenum($2,lsing($4),$6);}
	|  OBRACK exp DOTDOT upto CBRACK	{ $$ = mkeenum($2,Lnil,$4); }
	;

comprehension:  OBRACK exp VBAR quals CBRACK	{ $$ = mkcomprh($2,$4); }
	;

quals	:  qual					{ $$ = lsing($1); }
	|  quals COMMA qual			{ $$ = lapp($1,$3); }
	;

qual	:  	{ inpat = TRUE; } exp { inpat = FALSE; } qualrest
		{ if ($4 == NULL) {
		    patternOrExpr(/*wanted:*/ LEGIT_EXPR,$2);
		    $$ = mkguard($2);
		  } else {
		    patternOrExpr(/*wanted:*/ LEGIT_PATT,$2);
		    $$ = mkqual($2,$4);
/* OLD: WDP 95/08
		      if(ttree($4)==def)
			{
			  tree prevpatt_save = PREVPATT;
			  PREVPATT = $2;
			  $$ = mkdef((tree) mkpbind(lsing(createpat(lsing(mktruecase(ggdef((struct Sdef *) $4))),mknullbind())),hsplineno));
			  PREVPATT = prevpatt_save;
			}
		      else
*/
		  }
		}
	;

qualrest:  LARROW exp				{ $$ = $2; }
        |  /* empty */				{ $$ = NULL; }
	;

alts	:  alt					{ $$ = $1; }
	|  alts SEMI alt			{ $$ = lconc($1,$3); }
	;

alt	:  pat
		{ PREVPATT = $1; }
	   altrest
		{ $$ = $3;
		  PREVPATT = NULL;
		}
	|  /* empty */				{ $$ = Lnil; }
	;

altrest	:  gdpat maybe_where	 		{ $$ = lsing(createpat($1, $2)); }
	|  RARROW exp maybe_where		{ $$ = lsing(createpat(lsing(mktruecase($2)), $3)); }
	;

gdpat	:  gd RARROW exp gdpat			{ $$ = mklcons(ldub($1,$3),$4);  }
	|  gd RARROW exp			{ $$ = lsing(ldub($1,$3)); }
	;

upto	:  /* empty */				{ $$ = Lnil; }
	|  exp					{ $$ = lsing($1); }
	;

pats	:  pat COMMA pats			{ $$ = mklcons($1, $3); }
	|  pat					{ $$ = lsing($1); }
    	/* right recursion? (WDP) */
	;

pat	:  bpat
	|  pat conop bpat			{ $$ = mkinfixop($2,$1,$3); precparse($$); }
	;

bpat	:  apatc
 	|  conpat
	|  MINUS INTEGER			{ $$ = mklit(mkinteger(ineg($2))); }
	|  MINUS FLOAT				{ $$ = mklit(mkfloatr(ineg($2))); }
	;

conpat	:  con					{ $$ = mkident($1); }
	|  conpat apat				{ $$ = mkap($1,$2); }
	;

apat	:  con		 			{ $$ = mkident($1); }
	|  apatc
	;

apatc	:  var		 			{ $$ = mkident($1); }
	|  var AT apat			 	{ $$ = mkas($1,$3); }
	|  lit_constant				{ $$ = mklit($1); }
	|  WILDCARD				{ $$ = mkwildp(); }
	|  OPAREN CPAREN			{ $$ = mktuple(Lnil); }
	|  OPAREN var PLUS INTEGER CPAREN	{ $$ = mkplusp(mkident($2),mkinteger($4)); }
/* GHC no cannae do (WDP 95/05)
	|  OPAREN WILDCARD PLUS INTEGER CPAREN	{ $$ = mkplusp(mkwildp(),mkinteger($4)); }
*/
	|  OPAREN pat CPAREN			{ $$ = mkpar($2); }
	|  OPAREN pat COMMA pats CPAREN 	{ $$ = mktuple(mklcons($2,$4)); }
	|  OBRACK pats CBRACK			{ $$ = mkllist($2); }
	|  OBRACK CBRACK			{ $$ = mkllist(Lnil); }
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


/* Keywords which record the line start */

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



/* Non "-" op, used in right sections -- KH */
op1	:  conop
	|  varop1
  	;

op	:  conop
	|  varop
  	;

varop	:  varsym
	|  BQUOTE VARID BQUOTE		{ $$ = $2; }
	;

/*	Non-minus varop, used in right sections */
varop1	:  VARSYM
	|  plus
	|  BQUOTE VARID BQUOTE		{ $$ = $2; }
	;

conop	:  CONSYM
	|  BQUOTE CONID BQUOTE		{ $$ = $2; }
	;

varsym	:  VARSYM
	|  plus
	|  minus
	;

minus	:  MINUS			{ $$ = install_literal("-"); }
	;

plus	:  PLUS 			{ $$ = install_literal("+"); }
	;

var	:  VARID
	|  OPAREN varsym CPAREN		{ $$ = $2; }
	;

vark	:  VARID			{ setstartlineno(); $$ = $1; }
	|  oparenkey varsym CPAREN	{ $$ = $2; }
	;

/* tycon used here to eliminate 11 spurious R/R errors -- KH */
con	:  tycon
	|  OPAREN CONSYM CPAREN		{ $$ = $2; }
	;

conk	:  tycon			{ setstartlineno(); $$ = $1; }
	|  oparenkey CONSYM CPAREN	{ $$ = $2; }
	;

ccallid	:  VARID
	|  CONID
	;

tyvar_list: tyvar			{ $$ = lsing($1); }
	|  tyvar_list COMMA tyvar 	{ $$ = lapp($1,$3); }
	;

tyvars	:  tyvar			{ $$ = lsing($1); }
	|  tyvars tyvar			{ $$ = lapp($1, $2); }
	;

tyvar	:  VARID			{ $$ = mknamedtvar($1); }
	;

tycls	:  tycon
		/* partain: "aconid"->"tycon" got rid of a r/r conflict
		    (and introduced >= 2 s/r's ...)
	         */
	;

tycon	:  CONID
	;

modid	:  CONID
	;


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
