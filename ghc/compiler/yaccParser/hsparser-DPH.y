/**************************************************************************
*   File:               hsparser.y                                        *
*                                                                         *
*                       Author:                 Maria M. Gutierrez        *
*                       Modified by:            Kevin Hammond             *
*                       Last date revised:      December 13 1991. KH.     *
*                       Modification:           o Haskell 1.1 Syntax.     *
*						o Data Parallel Syntax.   *
*                                                                         *
*                                                                         *
*   Description:  This file contains the LALR(1) grammar for Haskell.     *
*                                                                         *
*   Entry Point:  module                                                  *
*                                                                         *
*   Problems:     None known.                                             *
*                                                                         *
*                                                                         *
*                 LALR(1) Syntax for Haskell 1.2 + Data Parallelism       *
*                                                                         *
**************************************************************************/


%{
#ifdef DEBUG
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

extern BOOLEAN nonstandardFlag;
extern BOOLEAN expect_ccurly;
extern BOOLEAN etags;

extern BOOLEAN  ispatt PROTO((tree, BOOLEAN));
extern tree 	function PROTO((tree));

static char modname[MODNAME_SIZE];
static char *the_module_name;
static char iface_name[MODNAME_SIZE];
static char interface_filename[FILENAME_SIZE];

static list module_exports;		/* Exported entities */
static list prelude_imports;		/* Entities imported from the Prelude */

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
extern int hsplineno;
extern int startlineno;

/**********************************************************************
*                                                                     *
*                                                                     *
*      Fixity and Precedence Declarations                             *
*                                                                     *
*                                                                     *
**********************************************************************/

list fixlist;
static int Fixity = 0, Precedence = 0;
struct infix;

char *ineg();

static BOOLEAN hidden = FALSE;		/*  Set when HIDING used        */

extern BOOLEAN inpat;			/*  True when parsing a pattern */
extern BOOLEAN implicitPrelude;		/*  True when we should read the Prelude if not given */

%}

%union {
	tree utree;
	list ulist;
	ttype uttype;
	atype uatype;
	binding ubinding;
	pbinding upbinding;
	finfot ufinfo;
	impidt uimpid;
	entidt uentid;
	id uid;
	int uint;
	float ufloat;
	char *ustring;
	hpragma uhpragma;
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
	CHARPRIM	INTPRIM		FLOATPRIM	DOUBLEPRIM
	CLITLIT		VOIDPRIM



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
%token  OPOD		CPOD 		OPROC 		CPROC


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
%token 	DRAWNFROM	INDEXFROM


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
%token	CCALL		CCALL_DANGEROUS	CASM		CASM_DANGEROUS	SCC

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
%token	ARITY_PRAGMA SPECIALIZE_PRAGMA STRICTNESS_PRAGMA UPDATE_PRAGMA
%token  END_PRAGMA

/**********************************************************************
*                                                                     *
*                                                                     *
*     Precedences of the various tokens                               *
*                                                                     *
*                                                                     *
**********************************************************************/


%left	CASE		LET		IN		LAMBDA		
  	IF		ELSE		CCALL		CCALL_DANGEROUS
	CASM		CASM_DANGEROUS	SCC		AT

%left	VARSYM		CONSYM		PLUS		MINUS		BQUOTE

%left	DCOLON

%left	SEMI		COMMA

%left	OCURLY		OBRACK		OPAREN

%left	OPOD		OPROC

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
  		exps pats context context_list tyvar_list
		maybeexports export_list
  		impspec maybeimpspec import_list
 		impdecls maybeimpdecls impdecl
  		renaming renamings renaming_list
		tyclses tycls_list
  		gdrhs gdpat valrhs valrhs1
  		lampats
  		upto
		cexp
		tyvar_pids
		parquals
  		pragmas


%type <utree>	exp dexp fexp kexp oexp aexp literal
		tuple list sequence comprehension qual qualrest
  		gd
 		apat bpat pat apatc conpat dpat fpat opat aapat
 		dpatk fpatk opatk aapatk
  		texps
 		processor parqual

%type <uid>	MINUS VARID CONID VARSYM CONSYM
  		var vark con conk varop varop1 conop op op1
		varid conid varsym consym minus plus
		tycls tycon modid ccallid

%type <ubinding>  topdecl topdecls
		  typed datad classd instd defaultd
		  decl decls valdef valdefs sign
  		  iimport iimports maybeiimports
		  ityped idatad iclassd iinstd ivarsd
  		  itopdecl itopdecls
  		  maybe_where
  		  interface readinterface ibody
		  cbody rinst
  		  impdecl_rest

%type <uttype>  simple simple_long type atype btype ttype ntatype inst class
		tyvar	

%type <uatype>	constr

%type <ustring> STRING FLOAT INTEGER CHARPRIM INTPRIM FLOATPRIM DOUBLEPRIM CLITLIT VOIDPRIM
%type <uint>	CHAR
%type <uentid>	export import
%type <uhpragma>  pragma


/**********************************************************************
*                                                                     *
*                                                                     *
*      Start Symbol for the Parser                                    *
*                                                                     *
*                                                                     *
**********************************************************************/

%start pmodule


%%

pmodule	:  readprelude module
	;

module	:  MODULE modid maybeexports 
		{ the_module_name = $2; module_exports = $3; }
	   WHERE body
	|	{ the_module_name = install_literal("Main"); module_exports = Lnil; }
	   body
	;

body	:  ocurly maybeimpdecls maybefixes topdecls ccurly
	       {
		 root = mkhmodule(the_module_name,lconc(prelude_imports,$2),module_exports,$4);
	       }
	|  vocurly maybeimpdecls maybefixes topdecls vccurly
	       {
		 root = mkhmodule(the_module_name,lconc(prelude_imports,$2),module_exports,$4);
	       }

	|  vocurly impdecls vccurly
	       { 		 
		 root = mkhmodule(the_module_name,lconc(prelude_imports,$2),module_exports,mknullbind());
	       }
	|  ocurly impdecls ccurly 
	       { 		 
		 root = mkhmodule(the_module_name,lconc(prelude_imports,$2),module_exports,mknullbind());
	       }

/* Adds 1 S/R, 2 R/R conflicts, alternatives add 3 R/R conflicts */
	|  vocurly maybeimpdecls vccurly 
	       { 		 
		 root = mkhmodule(the_module_name,lconc(prelude_imports,$2),module_exports,mknullbind());
	       }
	|  ocurly maybeimpdecls ccurly
	       { 		 
		 root = mkhmodule(the_module_name,lconc(prelude_imports,$2),module_exports,mknullbind());
	       }
	;


maybeexports :	/* empty */			{ $$ = Lnil; }
	|  OPAREN export_list CPAREN		{ $$ = $2; }
	;

export_list:
	   export				{ $$ = lsing($1); }
  	|  export_list COMMA export		{ $$ = lapp($1,$3); }
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
  	|  import_list COMMA import		{ $$ = lapp($1,$3); }
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


pragmas:
	   pragma { $$ = lsing($1); }
	|  pragmas pragma { $$ = lapp($1,$2); }
	|  /* empty */ { $$ = Lnil; }
	;

pragma:
	   ARITY_PRAGMA	     var EQUAL INTEGER END_PRAGMA
		{ $$ = mkarity_pragma($2,$4); }

	|  SPECIALIZE_PRAGMA var EQUAL ivarsd END_PRAGMA
		{ $$ = mkspecialize_pragma($2, $4); }

	|  STRICTNESS_PRAGMA var EQUAL STRING pragmas END_PRAGMA
		{ $$ = mkstrictness_pragma($2, $4, $5); }

	|  UPDATE_PRAGMA     var EQUAL INTEGER END_PRAGMA
		{ $$ = mkupdate_pragma($2, $4); }
	;


readprelude :   
	        {
		  if ( implicitPrelude ) {
		     find_module_on_imports_dirlist("Prelude",TRUE,interface_filename);
		  } else {
		     find_module_on_imports_dirlist("PreludeNull_",TRUE,interface_filename);
		  }
		  setyyin(interface_filename);
		  enteriscope();
		}
	   readinterface
		{
		  binding prelude = mkimport(installid(iface_name),Lnil,Lnil,$2,xstrdup(interface_filename),hsplineno);
		  prelude_imports = implicitPrelude? lsing(prelude): Lnil; 
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
		  find_module_on_imports_dirlist(module_name,FALSE,interface_filename);
		  setyyin(interface_filename);
		  enteriscope();
		  if(strcmp(module_name,"Prelude")==0)
		    prelude_imports = Lnil;
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

renaming_list: renaming				{ $$ = lsing($1); }
	|  renaming_list COMMA renaming		{ $$ = lapp($1,$3); }
	;

renaming:  var TO var				{ $$ = ldub($1,$3); }
	|  con TO con				{ $$ = ldub($1,$3); }
	;

maybeiimports : /* empty */			{ $$ = mknullbind(); }
	|  iimports SEMI			{ $$ = $1; }
	;

iimports : iimports SEMI iimport		{ $$ = mkabind($1,$3); }
	| iimport				{ $$ = $1; }
	;

iimport :  importkey modid OPAREN import_list CPAREN
		{ $$ = mkmbind($2,$4,Lnil,startlineno); }
	|  importkey modid OPAREN import_list CPAREN RENAMING renamings  
		{ $$ = mkmbind($2,$4,$7,startlineno); }
	;


interface:
  	   INTERFACE modid
		{ fixlist = Lnil;
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


fixes	:  fixes SEMI fix		
	|  fix
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

topdecls:  topdecls SEMI topdecl
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
	|  topdecl
	;

topdecl	:  typed				{ $$ = $1; }
	|  datad 				{ $$ = $1; }
	|  classd 				{ $$ = $1; }
	|  instd 				{ $$ = $1; }
	|  defaultd 				{ $$ = $1; }
	|  decl 				{ $$ = $1; }
	;

typed	:  typekey simple EQUAL type		{ $$ = mknbind($2,$4,startlineno,mkno_pramga()); }
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

classd	:  classkey context DARROW class cbody	{ $$ = mkcbind($2,$4,$5,startlineno,Lnil); }
	|  classkey class cbody		 	{ $$ = mkcbind(Lnil,$2,$3,startlineno,Lnil); }
	;

cbody	:  /* empty */				{ $$ = mknullbind(); }
	|  WHERE ocurly decls ccurly		{ checkorder($3); $$ = $3; }
	|  WHERE vocurly decls vccurly		{ checkorder($3); $$ =$3; }
	;


instd	:  instkey context DARROW tycls inst rinst	{ $$ = mkibind($2,$4,$5,$6,startlineno,Lnil); }
	|  instkey tycls inst rinst		 	{ $$ = mkibind(Lnil,$2,$3,$4,startlineno,Lnil); }
	;

rinst	:  /* empty */				{ $$ = mknullbind(); }
	|  WHERE ocurly valdefs ccurly		{ $$ = $3; }
	|  WHERE vocurly valdefs vccurly	{ $$ = $3; }
	;

inst	:  tycon				{ $$ = mktname($1,Lnil); }
	|  OPAREN simple_long CPAREN		{ $$ = $2; }
    /* partain?: "simple" requires k >= 0, not k > 0 (hence "simple_long" hack) */
	|  OPAREN tyvar_list CPAREN		{ $$ = mkttuple($2); }
	|  OPAREN CPAREN			{ $$ = mkttuple(Lnil); }
	|  OBRACK tyvar CBRACK			{ $$ = mktllist($2); }
	|  OPAREN tyvar RARROW tyvar CPAREN	{ $$ = mktfun($2,$4); }
	|  OPOD tyvar CPOD			{ $$ = mktpod($2); }
	|  OPROC tyvar_pids SEMI tyvar CPROC	{ $$ = mktproc($2,$4); }
	|  OPOD tyvar_pids SEMI tyvar CPOD	{ $$ = mktpod(mktproc($2,$4));}
	|  OPOD OPROC tyvar_pids SEMI tyvar CPROC CPOD	
			{ $$ = mktpod(mktproc($3,$5)); }
	;

/* Note (hilly) : Similar to tyvar_list except k>=1 not k>=2 */

tyvar_pids 	: tyvar COMMA tyvar_pids	{ $$ = mklcons($1,$3); }
		|  tyvar 			{ $$ = lsing($1); }
		;

defaultd:  defaultkey dtypes
	 	{ 
		  $$ = mkdbind($2,startlineno); 
		}
	;

dtypes	:  OPAREN type COMMA types CPAREN	{ $$ = mklcons($2,$4); }
	|  ttype				{ $$ = lsing($1); }
/*	Omitting this forces () to be the *type* (), which never defaults.  This is a KLUDGE */
/*	|  OPAREN CPAREN			{ $$ = Lnil; }*/
	;

decls	:  decls SEMI decl
		{
		  if(SAMEFN)
		    {
		      extendfn($1,$3);
		      $$ = $1;
		    }
		  else
		    $$ = mkabind($1,$3);
		}
	|  decl
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
*/

decl	:  vars DCOLON type DARROW type iclasop_pragma
		{ /* type2context.c for code */
		  $$ = mksbind($1,mkcontext(type2context($3),$5),startlineno,$6);
		  PREVPATT = NULL;
		  FN = NULL;
		  SAMEFN = 0;
		}
	|  sign
	|  valdef
	|  /* empty */			{ $$ = mknullbind(); PREVPATT = NULL; FN = NULL; SAMEFN = 0; }
  	;

sign	:  vars DCOLON type iclasop_pragma
		{
		  $$ = mksbind($1,$3,startlineno,$4);
		  PREVPATT = NULL;
		  FN = NULL;
		  SAMEFN = 0;
		}
	;



itopdecls : itopdecls SEMI itopdecl		{ $$ = mkabind($1,$3); }
	| itopdecl				{ $$ = $1; }
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

ityped	:  typekey simple EQUAL type itype_pragma { $$ = mknbind($2,$4,startlineno,$5); }
	;
	
idatad	:  datakey context DARROW simple idata_pragma			{ $$ = mktbind($2,$4,Lnil,Lnil,startlineno,$5); }
  	|  datakey simple idata_pragma					{ $$ = mktbind(Lnil,$2,Lnil,Lnil,startlineno,$3); }
	|  datakey context DARROW simple EQUAL constrs 			{ $$ = mktbind($2,$4,$6,Lnil,startlineno,mk_nopragma()); }
	|  datakey simple EQUAL constrs					{ $$ = mktbind(Lnil,$2,$4,Lnil,startlineno,mk_nopragma()); }
	|  datakey context DARROW simple EQUAL constrs DERIVING tyclses	{ $$ = mktbind($2,$4,$6,$8,startlineno,mk_nopragma()); }
	|  datakey simple EQUAL constrs DERIVING tyclses		{ $$ = mktbind(Lnil,$2,$4,$6,startlineno,mk_nopragma()); }
	;


iclassd	:  classkey context DARROW class cbody pragmas
		{ $$ = mkcbind($2,$4,$5,startlineno,$6); }
	|  classkey class cbody	pragmas
		{ $$ = mkcbind(Lnil,$2,$3,startlineno,$4); }
	;

iinstd	:  instkey context DARROW tycls inst pragmas
		{ $$ = mkibind($2,$4,$5,mknullbind(),startlineno,$6); }
	|  instkey tycls inst pragmas
		{ $$ = mkibind(Lnil,$2,$3,mknullbind(),startlineno,$4); }
	;


/* obsolete: "(C a, ...)" cause r/r conflict, resolved in favour of context rather than type */

class	:  tycon tyvar 				{ $$ = mktname($1,lsing($2)); }
    	    /* partain: changed "tycls" to "tycon" */
	;

types	:  types COMMA type			{ $$ = lapp($1,$3); }
	|  type					{ $$ = lsing($1); }
	;

type	:  btype				{ $$ = $1; }
	|  btype RARROW type			{ $$ = mktfun($1,$3); }

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

ntatype	:  tyvar				{ $$ = $1; }
  	|  tycon				{ $$ = mktname($1,Lnil); }
	|  OPAREN CPAREN			{ $$ = mkttuple(Lnil); }
	|  OPAREN type CPAREN			{ $$ = $2; }
	|  OBRACK type CBRACK			{ $$ = mktllist($2); }
	|  OPOD type CPOD			{ $$ = mktpod($2); }
	|  OPROC types SEMI type CPROC		{ $$ = mktproc($2,$4); }
	|  OPOD types SEMI type CPOD		{ $$ = mktpod(mktproc($2,$4));}
	;
	

simple	:  tycon		 		{ $$ = mktname($1,Lnil); }
	|  tycon tyvars		 		{ $$ = mktname($1,$2); }
	;


simple_long : tycon tyvars	 		{ $$ = mktname($1,$2); }
	; /* partain: see comment in "inst" */


constrs	:  constrs VBAR constr			{ $$ = lapp($1,$3); }
	|  constr				{ $$ = lsing($1); }
	;

/* Using tycon rather than con avoids 5 S/R errors */
constr	:  tycon atypes				{ $$ = mkatc($1,$2,hsplineno); }
	|  OPAREN consym CPAREN atypes		{ $$ = mkatc($2,$4,hsplineno); }
	|  tycon				{ $$ = mkatc($1,Lnil,hsplineno); }
	|  OPAREN consym CPAREN			{ $$ = mkatc($2,Lnil,hsplineno); }
	|  btype conop btype			{ $$ = mkatc($2, ldub($1,$3), hsplineno); }
	;

tyclses	:  OPAREN tycls_list CPAREN		{ $$ = $2; }
	|  OPAREN CPAREN			{ $$ = Lnil; }
	|  tycls				{ $$ = lsing($1); }
	;

tycls_list:  tycls COMMA tycls_list		{ $$ = mklcons($1,$3); }
	|  tycls				{ $$ = lsing($1); }
	;

context	:  OPAREN context_list CPAREN		{ $$ = $2; }
	|  class				{ $$ = lsing($1); }
	;

context_list:  class COMMA context_list		{ $$ = mklcons($1,$3); }
	|  class				{ $$ = lsing($1); }
	;

valdefs	:  valdefs SEMI valdef
		{
		  if(SAMEFN)
		    {
		      extendfn($1,$3);
		      $$ = $1;
		    }
		  else
		    $$ = mkabind($1,$3);
		}
	|  valdef				{ $$ = $1; }
	|  /* empty */				{ $$ = mknullbind(); }
	;


vars	:  vark COMMA varsrest			{ $$ = mklcons($1,$3); }
	|  vark					{ $$ = lsing($1); }
	;

varsrest:  varsrest COMMA var	 		{ $$ = lapp($1,$3); }
	|  var					{ $$ = lsing($1); }
	;

cons	:  cons COMMA con	 		{ $$ = lapp($1,$3); }
	|  con					{ $$ = lsing($1); }
	;


valdef	:  opatk
		{
		  tree fn = function($1);

		  PREVPATT = $1;

		  if(ttree(fn) == ident)
		    {
		      checksamefn(gident(fn));
		      FN = fn;
		    }

		  else if (ttree(fn) == tinfixop && ttree(ginfun((struct Sap *) fn)) == ident)
		    {
		      checksamefn(gident(ginfun((struct Sap *) fn)));
		      FN = ginfun((struct Sap *) fn);
		    }

		  else if(etags)
		    printf("%u\n",startlineno);
		}
	   valrhs
	  	{
		  if(ispatt($1,TRUE))
		    {
		      $$ = mkpbind($3, startlineno);
		      FN = NULL;
		      SAMEFN = 0;
		    }
		  else
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
	|  CCALL_DANGEROUS ccallid cexp		{ $$ = mkccall($2,installid("p"),$3); }
	|  CCALL_DANGEROUS ccallid		{ $$ = mkccall($2,installid("p"),Lnil); }
	|  CASM CLITLIT cexp			{ $$ = mkccall($2,installid("N"),$3); }
	|  CASM CLITLIT				{ $$ = mkccall($2,installid("N"),Lnil); }
	|  CASM_DANGEROUS CLITLIT cexp		{ $$ = mkccall($2,installid("P"),$3); }
	|  CASM_DANGEROUS CLITLIT		{ $$ = mkccall($2,installid("P"),Lnil); }

	/* SCC Expression */
	|  SCC STRING exp
		{ extern BOOLEAN ignoreSCC;
		  extern BOOLEAN warnSCC;
		  extern char * input_filename;

		  if (ignoreSCC) {
		    if (warnSCC) 
			fprintf(stderr,
				"\"%s\", line %d: scc (`set [profiling] cost centre') ignored\n",
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
	|  literal	
	|  OPAREN exp CPAREN			{ $$ = mkpar($2); }
	|  OPAREN oexp op CPAREN		{ checkprec($2,$3,FALSE); $$ = mklsection($2,$3); }
	|  OPAREN op1 oexp CPAREN		{ checkprec($3,$2,TRUE);  $$ = mkrsection($2,$3); }

	/* structures */
	|  tuple
	|  list					{ $$ = mkpar($1); }
	|  sequence				{ $$ = mkpar($1); }
	|  comprehension			{ $$ = mkpar($1); }
	|  OPOD exp VBAR parquals CPOD		{ $$ = mkparzf($2,$4); }
	|  OPOD exps CPOD			{ $$ = mkpod($2); }
	|  processor				{ $$ = mkpar($1); }

	/* These only occur in patterns */
	|  var AT aexp				{ checkinpat();  $$ = mkas($1,$3); } 
	|  WILDCARD				{ checkinpat();  $$ = mkwildp();   }
	|  LAZY aexp				{ checkinpat();  $$ = mklazyp($2); }
	;


processor :  OPROC exps SEMI exp CPROC		{ $$ = mkproc($2,$4); }
	  ;

parquals  :  parquals COMMA parqual		{ $$ = lapp($1,$3); }
	  |  parqual				{ $$ = lsing($1); }
	  ;

parqual  : exp					{ $$ = mkparfilt($1); }
	  | processor  DRAWNFROM exp	
		{	$$ = mkpardgen($1,$3); 
			checkpatt($1);
		}
	  | processor  INDEXFROM exp	
		{       $$ = mkparigen($1,$3); 
			checkpatt(gprocdata($1));
		}
	  ;


/*
	LHS patterns are parsed in a similar way to
	expressions.  This avoids the horrible non-LRness
	which occurs with the 1.1 syntax.

	The xpatk business is to do with accurately recording
	the starting line for definitions.
*/

/*TESTTEST
bind	:  opatk
	|  vark lampats
		{ $$ = mkap($1,$2); }
	|  opatk varop opat %prec PLUS
		{
		  $$ = mkinfixop($2,$1,$3);
		}
	;

opatk	:  dpatk
	|  opatk conop opat %prec PLUS
		{
		  $$ = mkinfixop($2,$1,$3);
		  precparse($$);
		}
	;

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
	|  literal					{ $$ = $1; }
	|  WILDCARD					{ $$ = mkwildp(); }
	|  OPAREN CPAREN				{ $$ = mktuple(Lnil); }
	|  OPAREN var PLUS INTEGER CPAREN		{ $$ = mkplusp(mkident($2),mkinteger($4)); }
	|  OPAREN WILDCARD PLUS INTEGER CPAREN		{ $$ = mkplusp(mkwildp(),mkinteger($4)); }
	|  OPAREN opat CPAREN				{ $$ = mkpar($2); }
	|  OPAREN opat COMMA pats CPAREN 		{ $$ = mktuple(mklcons($2,$4)); }
	|  OBRACK pats CBRACK				{ $$ = mkllist($2); }
	|  OBRACK CBRACK				{ $$ = mkllist(Lnil); }
	|  LAZY apat					{ $$ = mklazyp($2); }
        |  OPROC pats SEMI apat CPROC			{ $$ = mkproc($2,$4); }
	;

aapatk	:  conk		 				{ $$ = mkident($1); }
	|  vark		 				{ $$ = mkident($1); }
	|  vark AT apat			 		{ $$ = mkas($1,$3); }
	|  literal					{ $$ = $1; setstartlineno(); }
	|  WILDCARD					{ $$ = mkwildp(); setstartlineno(); }
	|  oparenkey CPAREN				{ $$ = mktuple(Lnil); }
	|  oparenkey var PLUS INTEGER CPAREN		{ $$ = mkplusp(mkident($2),mkinteger($4)); }
	|  oparenkey WILDCARD PLUS INTEGER CPAREN	{ $$ = mkplusp(mkwildp(),mkinteger($4)); }
	|  oparenkey opat CPAREN			{ $$ = mkpar($2); }
	|  oparenkey opat COMMA pats CPAREN 		{ $$ = mktuple(mklcons($2,$4)); }
	|  obrackkey pats CBRACK			{ $$ = mkllist($2); }
	|  obrackkey CBRACK				{ $$ = mkllist(Lnil); }
	|  lazykey apat					{ $$ = mklazyp($2); }
        |  oprockey pats SEMI opat CPROC		{ $$ = mkproc($2,$4); }
	;


/*
   The mkpars are so that infix parsing doesn't get confused.

   KH.
*/

tuple	:  OPAREN exp COMMA texps CPAREN
  		{ if (ttree($4) == tuple)
		    $$ = mktuple(mklcons($2, gtuplelist($4)));
		else
		  $$ = mktuple(ldub($2, $4));
		}
	|  OPAREN CPAREN
		{ $$ = mktuple(Lnil); }
	;

texps	:  exp COMMA texps
		{ if (ttree($3) == tuple)
		    $$ = mktuple(mklcons($1, gtuplelist($3)));
		else
		  $$ = mktuple(ldub($1, $3));
		}
	|  exp					{ $$ = mkpar($1); }
	;


list	:  OBRACK CBRACK  			{ $$ = mkllist(Lnil); }
  	|  OBRACK exps CBRACK			{ $$ = mkllist($2); }
	;

exps	:  exp COMMA exps			{ $$ = mklcons($1,$3); }
	|  exp					{ $$ = lsing($1); }
	;


sequence:  OBRACK exp COMMA exp DOTDOT upto CBRACK	{$$ = mkeenum($2,lsing($4),$6);}
	|  OBRACK exp DOTDOT upto CBRACK	{ $$ = mkeenum($2,Lnil,$4); }
	;

comprehension:  OBRACK exp VBAR quals CBRACK	{ $$ = mkcomprh($2,$4); }
	;

quals	:  quals COMMA qual			{ $$ = lapp($1,$3); }
	|  qual					{ $$ = lsing($1); }
	;

qual	:  	{ inpat = TRUE; } exp { inpat = FALSE; } qualrest
		{ if ($4 == NULL)
		    $$ = mkguard($2);
		  else 
		    {
		      checkpatt($2);
		      if(ttree($4)==def)
			{
			  tree prevpatt_save = PREVPATT;
			  PREVPATT = $2;
			  $$ = mkdef(mkpbind(lsing(createpat(lsing(mktruecase((tree)(ggdef($4)))),mknullbind())),hsplineno));
			  PREVPATT = prevpatt_save;
			}
		      else
			$$ = mkqual($2,$4);
		    }
		}
	;

qualrest:  LARROW exp				{ $$ = $2; }
/* OLD:
	|  EQUAL exp
		{ if(nonstandardFlag)
		    $$ = mkdef($2); 
		  else
		    hsperror("Definitions in comprehensions are not standard Haskell");
		}
*/
        |  /* empty */				{ $$ = NULL; }
	;


alts	:  alts SEMI alt			{ $$ = lconc($1,$3); }
	|  alt					{ $$ = $1; }
	;

alt	:  pat
		{ PREVPATT = $1; }
	   altrest
		{ $$ = $3;
		  PREVPATT = NULL;
		}
	|  /* empty */				{ $$ = Lnil; }
	;

altrest	:  gdpat maybe_where	 		{ $$ = lsing(createpat($1,$2)); }
	|  RARROW exp maybe_where		{ $$ = lsing(createpat(lsing(mktruecase($2)),$3)); }
	;

gdpat	:  gd RARROW exp gdpat			{ $$ = mklcons(ldub($1,$3),$4);  }
	|  gd RARROW exp			{ $$ = lsing(ldub($1,$3)); }
	;

upto	:  /* empty */				{ $$ = Lnil; }
	|  exp					{ $$ = lsing($1); }
	;

pats	:  pat COMMA pats			{ $$ = mklcons($1,$3); }
	|  pat					{ $$ = lsing($1); }
	;

pat	:  bpat
	|  pat conop bpat			{ $$ = mkinfixop($2,$1,$3); precparse($$); }
	;

bpat	:  apatc
 	|  conpat
	|  MINUS INTEGER			{ $$ = mkinteger(ineg($2)); }
	|  MINUS FLOAT				{ $$ = mkfloatr(ineg($2)); }
	;

conpat	:  con					{ $$ = mkident($1); }
	|  conpat apat				{ $$ = mkap($1,$2); }
	;

apat	:  con		 			{ $$ = mkident($1); }
	|  apatc
	;

apatc	:  var		 			{ $$ = mkident($1); }
	|  var AT apat			 	{ $$ = mkas($1,$3); }
	|  literal				{ $$ = $1; }
	|  WILDCARD				{ $$ = mkwildp(); }
	|  OPAREN CPAREN			{ $$ = mktuple(Lnil); }
	|  OPAREN var PLUS INTEGER CPAREN	{ $$ = mkplusp(mkident($2),mkinteger($4)); }
	|  OPAREN WILDCARD PLUS INTEGER CPAREN	{ $$ = mkplusp(mkwildp(),mkinteger($4)); }
	|  OPAREN pat CPAREN			{ $$ = mkpar($2); }
	|  OPAREN pat COMMA pats CPAREN 	{ $$ = mktuple(mklcons($2,$4)); }
	|  OBRACK pats CBRACK			{ $$ = mkllist($2); }
	|  OBRACK CBRACK			{ $$ = mkllist(Lnil); }
	|  LAZY apat				{ $$ = mklazyp($2); }
        |  OPROC pats SEMI apat CPROC		{ $$ = mkproc($2,$4); }
	;

/*
patk	:  bpatk
	|  patk conop bpat			{ $$ = mkinfixop($2,$1,$3); precparse($$); }
	;

bpatk	:  apatck
 	|  conpatk
	|  minuskey INTEGER			{ $$ = mkinteger(ineg($2)); }
	|  minuskey FLOAT			{ $$ = mkfloatr(ineg($2)); }
	;

conpatk	:  conk					{ $$ = mkident($1); }
	|  conpatk apat				{ $$ = mkap($1,$2); }
	;

apatck	:  vark		 			{ $$ = mkident($1); }
	|  vark AT apat			 	{ $$ = mkas($1,$3); }
	|  literal				{ $$ = $1; setstartlineno(); }
	|  WILDCARD				{ $$ = mkwildp(); setstartlineno(); }
	|  oparenkey CPAREN			{ $$ = mktuple(Lnil); }
	|  oparenkey var PLUS INTEGER CPAREN	{ $$ = mkplusp(mkident($2),mkinteger($4)); }
	|  oparenkey WILDCARD PLUS INTEGER CPAREN	{ $$ = mkplusp(mkwildp(),mkinteger($4)); }
	|  oparenkey pat CPAREN			{ $$ = mkpar($2); }
	|  oparenkey pat COMMA pats CPAREN 	{ $$ = mktuple(mklcons($2,$4)); }
	|  obrackkey pats CBRACK			{ $$ = mkllist($2); }
	|  obrackkey CBRACK			{ $$ = mkllist(Lnil); }
	|  lazykey apat				{ $$ = mklazyp($2); }
        |  oprockey pats SEMI opat CPROC		{ $$ = mkproc($2,$4); }
	;
*/

literal	:  INTEGER				{ $$ = mkinteger($1); }
	|  FLOAT				{ $$ = mkfloatr($1); }
	|  CHAR					{ $$ = mkcharr($1); }
	|  STRING				{ $$ = mkstring($1); }
	|  CHARPRIM				{ $$ = mkcharprim($1); }
	|  INTPRIM				{ $$ = mkintprim($1); }
	|  FLOATPRIM				{ $$ = mkfloatprim($1); }
	|  DOUBLEPRIM				{ $$ = mkdoubleprim($1); }
	|  CLITLIT				{ $$ = mkclitlit($1); }
	|  VOIDPRIM				{ $$ = mkvoidprim(); }
	;


/* Keywords which record the line start */

importkey:  IMPORT	{ setstartlineno(); }
	;

datakey	:   DATA	{ setstartlineno();
			  if(etags)
			    printf("%u\n",startlineno);
			} 
	;

typekey	:   TYPE	{ setstartlineno();
			  if(etags)
			    printf("%u\n",startlineno);
			} 
	;

instkey	:   INSTANCE	{ setstartlineno();
			  if(etags)
			    printf("%u\n",startlineno);
			} 
	;

defaultkey: DEFAULT	{ setstartlineno(); }
	;

classkey:   CLASS	{ setstartlineno();
			  if(etags)
			    printf("%u\n",startlineno);
			}
	;

minuskey:   MINUS	{ setstartlineno(); }
	;

oparenkey:  OPAREN	{ setstartlineno(); }
	;

obrackkey:  OBRACK	{ setstartlineno(); }
	;

lazykey	:   LAZY	{ setstartlineno(); }
	;

oprockey:   OPROC	{ setstartlineno(); }
	;


/* Non "-" op, used in right sections -- KH */
op1	:  conop
	|  varop1
  	;

op	:  conop
	|  varop
  	;

varop	:  varsym
	|  BQUOTE varid BQUOTE		{ $$ = $2; }
	;

/*	Non-minus varop, used in right sections */
varop1	:  VARSYM
	|  plus
	|  BQUOTE varid BQUOTE		{ $$ = $2; }
	;

conop	:  consym
	|  BQUOTE conid BQUOTE		{ $$ = $2; }
	;

consym	:  CONSYM
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
	|  OPAREN consym CPAREN		{ $$ = $2; }
	;

conk	:  tycon			{ setstartlineno(); $$ = $1; }
	|  oparenkey consym CPAREN	{ $$ = $2; }
	;

varid	:  VARID
	;

conid	:  CONID
	;

ccallid	:  varid
	|  conid
	;

/* partain: "tyvar_list" must be at least 2 elements long (defn of "inst") */
tyvar_list: tyvar COMMA tyvar_list		{ $$ = mklcons($1,$3); }
	|  tyvar COMMA tyvar			{ $$ = mklcons($1,lsing($3)); }
	;

tyvars	:  tyvar tyvars				{ $$ = mklcons($1,$2); }
	|  tyvar				{ $$ = lsing($1); }
	;

tyvar	:  VARID				{ $$ = mknamedtvar($1); }
	;

tycls	:  tycon
		/* partain: "aconid"->"tycon" got rid of a r/r conflict
		    (and introduced >= 2 s/r's ...)
	         */
	;

tycon	:  conid
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
