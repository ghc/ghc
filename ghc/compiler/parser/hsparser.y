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
extern BOOLEAN etags;

extern char *input_filename;
static char *the_module_name;
static maybe module_exports;

extern list Lnil;
extern list reverse_list();
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

static int Fixity = 0, Precedence = 0;

char *ineg PROTO((char *));

long    source_version = 0;

BOOLEAN inpat;
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

%token	OCURLY		CCURLY		VCCURLY	
%token  COMMA		SEMI		OBRACK		CBRACK
%token	WILDCARD	BQUOTE		OPAREN		CPAREN


/**********************************************************************
*                                                                     *
*                                                                     *
*     Reserved Operators                                              *
*                                                                     *
*                                                                     *
**********************************************************************/

%token	DOTDOT		DCOLON		EQUAL		LAMBDA		
%token	VBAR		RARROW	 	LARROW
%token	AT		LAZY		DARROW


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

%token  SCC
%token	CCALL		CCALL_GC	CASM		CASM_GC


/**********************************************************************
*                                                                     *
*                                                                     *
*     Special symbols/identifiers which need to be recognised         *
*                                                                     *
*                                                                     *
**********************************************************************/

%token	MINUS		BANG		PLUS
%token 	AS		HIDING		QUALIFIED


/**********************************************************************
*                                                                     *
*                                                                     *
*     Special Symbols for the Lexer                                   *
*                                                                     *
*                                                                     *
**********************************************************************/

%token  INTERFACE_UPRAGMA SPECIALISE_UPRAGMA
%token  INLINE_UPRAGMA MAGIC_UNFOLDING_UPRAGMA
%token  END_UPRAGMA 
%token  SOURCE_UPRAGMA

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
	MINUS	BQUOTE	BANG	DARROW	PLUS

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
		rbinds rbinds1 rpats rpats1 list_exps list_rest
		qvarsk qvars_list
		constrs constr1 fields 
		types atypes batypes
		types_and_maybe_ids
  		pats context context_list /* tyvar_list */
		export_list enames
  		import_list inames
 		impdecls maybeimpdecls impdecl
		maybefixes fixes fix ops
		dtyclses dtycls_list
  		gdrhs gdpat valrhs
  		lampats	cexps gd

%type <umaybe>  maybeexports impspec deriving

%type <uliteral> lit_constant

%type <utree>	exp oexp dexp kexp fexp aexp rbind texps
		expL oexpL kexpL expLno oexpLno dexpLno kexpLno
		vallhs funlhs qual leftexp
 		pat cpat bpat apat apatc conpat rpat
          		patk bpatk apatck conpatk


%type <uid>	MINUS PLUS DARROW AS LAZY
		VARID CONID VARSYM CONSYM 
  		var con varop conop op
		vark varid varsym varsym_nominus
	        tycon modid ccallid

%type <uqid>	QVARID QCONID QVARSYM QCONSYM 
		qvarid qconid qvarsym qconsym
		qvar qcon qvarop qconop qop
		qvark qconk qtycon qtycls
		gcon gconk gtycon itycon qop1 qvarop1 
		ename iname 

%type <ubinding>  topdecl topdecls letdecls
		  typed datad newtd classd instd defaultd
		  decl decls valdef instdef instdefs
 		  maybe_where cbody rinst type_and_maybe_id

%type <upbinding> valrhs1 altrest

%type <uttype>    simple ctype sigtype sigarrowtype type atype bigatype btype
		  gtyconvars 
		  bbtype batype bxtype wierd_atype
		  class tyvar contype

%type <uconstr>	  constr constr_after_context field

%type <ustring>   FLOAT INTEGER INTPRIM
		  FLOATPRIM DOUBLEPRIM CLITLIT

%type <uhstring>  STRING STRINGPRIM CHAR CHARPRIM

%type <uentid>	  export import

%type <ulong>     commas importkey

/**********************************************************************
*                                                                     *
*                                                                     *
*      Start Symbol for the Parser                                    *
*                                                                     *
*                                                                     *
**********************************************************************/

%start module

%%
module	:  modulekey modid maybeexports
		{
		  modulelineno = startlineno;
		  the_module_name = $2;
		  module_exports = $3;
		}
	   WHERE body
	|	{ 
		  modulelineno = 0;
		  the_module_name = install_literal("Main");
		  module_exports = mknothing();
                }
	   body
	;

body	:  ocurly { setstartlineno(); } interface_pragma orestm
	|  vocurly interface_pragma vrestm
	;

interface_pragma : /* empty */
	| INTERFACE_UPRAGMA INTEGER END_UPRAGMA SEMI
	       {
		 source_version = atoi($2);
	       }
        ;

orestm  :  maybeimpdecls maybefixes topdecls ccurly
	       {
		 root = mkhmodule(the_module_name,$1,module_exports,
				  $2,$3,source_version,modulelineno);
	       }
	|  impdecls ccurly
	       {
		 root = mkhmodule(the_module_name,$1,module_exports,
			          Lnil,mknullbind(),source_version,modulelineno);
	       }

vrestm  :  maybeimpdecls maybefixes topdecls vccurly
	       {
		 root = mkhmodule(the_module_name,$1,module_exports,
				  $2,$3,source_version,modulelineno);
	       }
	|  impdecls vccurly
	       {
		 root = mkhmodule(the_module_name,$1,module_exports,
				  Lnil,mknullbind(),source_version,modulelineno);
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


impdecl	:  importkey modid impspec
		{ $$ = lsing(mkimport($2,0,mknothing(),$3,$1,startlineno)); }
	|  importkey QUALIFIED modid impspec
		{ $$ = lsing(mkimport($3,1,mknothing(),$4,$1,startlineno)); }
	|  importkey QUALIFIED modid AS modid impspec
		{ $$ = lsing(mkimport($3,1,mkjust($5),$6,$1,startlineno)); }
	;

impspec	:  /* empty */				  { $$ = mknothing(); }
	|  OPAREN CPAREN			  { $$ = mkjust(mkleft(Lnil)); }
	|  OPAREN import_list CPAREN		  { $$ = mkjust(mkleft($2));   }
	|  OPAREN import_list COMMA CPAREN	  { $$ = mkjust(mkleft($2));   }
	|  HIDING OPAREN import_list CPAREN	  { $$ = mkjust(mkright($3));  }
	|  HIDING OPAREN import_list COMMA CPAREN { $$ = mkjust(mkright($3));  }
  	;

import_list:
	   import				{ $$ = lsing($1); }
	|  import_list COMMA import		{ $$ = lapp($1, $3); }
	;

import	:  var					{ $$ = mkentid(mknoqual($1)); }
	|  itycon				{ $$ = mkenttype($1); }
	|  itycon OPAREN DOTDOT CPAREN		{ $$ = mkenttypeall($1); }
	|  itycon OPAREN CPAREN			{ $$ = mkenttypenamed($1,Lnil);}
	|  itycon OPAREN inames CPAREN		{ $$ = mkenttypenamed($1,$3); }
	;

itycon	:  tycon				{ $$ = mknoqual($1); }
	|  OBRACK CBRACK			{ $$ = creategid(-1); }         
	|  OPAREN CPAREN			{ $$ = creategid(0); }         
	|  OPAREN commas CPAREN 		{ $$ = creategid($2); }
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

ops	:  op		 { $$ = lsing(mkfixop(mknoqual($1),infixint(Fixity),Precedence)); }
	|  ops COMMA op  { $$ = lapp($1,mkfixop(mknoqual($3),infixint(Fixity),Precedence)); }
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

topdecl	:  typed				{ $$ = $1; FN = NULL; SAMEFN = 0; }
	|  datad 				{ $$ = $1; FN = NULL; SAMEFN = 0; }
	|  newtd				{ $$ = $1; FN = NULL; SAMEFN = 0; }
	|  classd 				{ $$ = $1; FN = NULL; SAMEFN = 0; }
	|  instd 				{ $$ = $1; FN = NULL; SAMEFN = 0; }
	|  defaultd 				{ $$ = $1; FN = NULL; SAMEFN = 0; }
	|  decl 				{ $$ = $1; }
	;

typed	:  typekey simple EQUAL type		{ $$ = mknbind($2,$4,startlineno); }
	;


datad	:  datakey simple EQUAL constrs deriving
		{ $$ = mktbind(Lnil,$2,$4,$5,startlineno); }
	|  datakey context DARROW simple EQUAL constrs deriving
		{ $$ = mktbind($2,$4,$6,$7,startlineno); }
	;

newtd	:  newtypekey simple EQUAL constr1 deriving
		{ $$ = mkntbind(Lnil,$2,$4,$5,startlineno); }
	|  newtypekey context DARROW simple EQUAL constr1 deriving
		{ $$ = mkntbind($2,$4,$6,$7,startlineno); }
	;

deriving: /* empty */				{ $$ = mknothing(); }
        | DERIVING dtyclses                     { $$ = mkjust($2); }
	;

classd	:  classkey context DARROW class cbody
		{ $$ = mkcbind($2,$4,$5,startlineno); }
	|  classkey class cbody		 	
		{ $$ = mkcbind(Lnil,$2,$3,startlineno); }
	;

cbody	:  /* empty */				{ $$ = mknullbind(); }
	|  WHERE ocurly decls ccurly		{ checkorder($3); $$ = $3; }
	|  WHERE vocurly decls vccurly		{ checkorder($3); $$ = $3; }
	;

instd	:  instkey context DARROW gtycon atype rinst
		{ $$ = mkibind($2,$4,$5,$6,startlineno); }
	|  instkey gtycon atype rinst
	 	{ $$ = mkibind(Lnil,$2,$3,$4,startlineno); }
	;

rinst	:  /* empty */			  			{ $$ = mknullbind(); }
	|  WHERE ocurly  instdefs ccurly  			{ $$ = $3; }
	|  WHERE vocurly instdefs vccurly 			{ $$ = $3; }
	;

/*	I now allow a general type in instance declarations, relying
	on the type checker to reject instance decls which are ill-formed.
	Some (non-standard) extensions of Haskell may allow more general
	types than the Report syntax permits, and in any case not all things
	can be checked in the syntax (eg repeated type variables).
		SLPJ Jan 97

restrict_inst : gtycon				{ $$ = mktname($1); }
	|  OPAREN gtyconvars CPAREN		{ $$ = $2; }
	|  OPAREN tyvar COMMA tyvar_list CPAREN	{ $$ = mkttuple(mklcons($2,$4)); }
	|  OBRACK tyvar CBRACK			{ $$ = mktllist($2); }
	|  OPAREN tyvar RARROW tyvar CPAREN	{ $$ = mktfun($2,$4); }
	;

general_inst : gtycon				{ $$ = mktname($1); }
	|  OPAREN gtyconapp1 CPAREN		{ $$ = $2; }
	|  OPAREN type COMMA types CPAREN	{ $$ = mkttuple(mklcons($2,$4)); }
	|  OBRACK type CBRACK			{ $$ = mktllist($2); }
	|  OPAREN btype RARROW type CPAREN	{ $$ = mktfun($2,$4); }
	;
*/

defaultd:  defaultkey OPAREN types CPAREN       { $$ = mkdbind($3,startlineno); }
	|  defaultkey OPAREN CPAREN		{ $$ = mkdbind(Lnil,startlineno); }
	;

decls	: decl
	| decls SEMI decl
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

decl	: qvarsk DCOLON sigtype
		{ $$ = mksbind($1,$3,startlineno);
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

	|  SPECIALISE_UPRAGMA INSTANCE gtycon atype END_UPRAGMA
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

/* A sigtype is a rank 2 type; it can have for-alls as function args:
  	f :: All a => (All b => ...) -> Int
*/
sigtype	: type DARROW sigarrowtype		{ $$ = mkcontext(type2context($1),$3); }
	| sigarrowtype 
	;

sigarrowtype : bigatype RARROW sigarrowtype	{ $$ = mktfun($1,$3); }
	     | btype RARROW sigarrowtype	{ $$ = mktfun($1,$3); }
	     | btype
	     ;

/* A "big" atype can be a forall-type in brackets.  */
bigatype: OPAREN type DARROW type CPAREN	{ $$ = mkcontext(type2context($2),$4); }
	;

	/* 1 S/R conflict at DARROW -> shift */
ctype   : type DARROW type			{ $$ = mkcontext(type2context($1),$3); }
	| type
	;

	/* 1 S/R conflict at RARROW -> shift */
type	:  btype RARROW type			{ $$ = mktfun($1,$3); }
	|  btype				{ $$ = $1; }
	;

btype	:  btype atype				{ $$ = mktapp($1,$2); }
	|  atype				{ $$ = $1; }
	;

atype  	:  gtycon				{ $$ = mktname($1); }
	|  tyvar				{ $$ = $1; }
	|  OPAREN type COMMA types CPAREN	{ $$ = mkttuple(mklcons($2,$4)); }
	|  OBRACK type CBRACK			{ $$ = mktllist($2); }
	|  OPAREN type CPAREN			{ $$ = $2; }
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

constr	:  constr_after_context
	|  type DARROW constr_after_context	{ $$ = mkconstrcxt ( type2context($1), $3 ); }
	;

constr_after_context :

	/* We have to parse the constructor application as a *type*, else we get
	   into terrible ambiguity problems.  Consider the difference between

		data T = S Int Int Int `R` Int
	   and
		data T = S Int Int Int
	
	   It isn't till we get to the operator that we discover that the "S" is
	   part of a type in the first, but part of a constructor application in the
	   second.
	*/

/* Con !Int (Tree a) */
	   contype				{ qid tyc; list tys;
						  splittyconapp($1, &tyc, &tys);
					          $$ = mkconstrpre(tyc,tys,hsplineno); }

/* !Int `Con` Tree a */
	|  bbtype qconop bbtype			{ $$ = mkconstrinf($1,$2,$3,hsplineno); }

/* (::) (Tree a) Int */
	|  OPAREN qconsym CPAREN batypes	{ $$ = mkconstrpre($2,$4,hsplineno); }

/* Con { op1 :: Int } */
	|  gtycon OCURLY fields CCURLY		{ $$ = mkconstrrec($1,$3,hsplineno); }
		/* 1 S/R conflict on OCURLY -> shift */
	;


/* contype has to reduce to a btype unless there are !'s, so that
   we don't get reduce/reduce conflicts with the second production of constr.
   But as soon as we see a ! we must switch to using bxtype. */

contype : btype					{ $$ = $1 }
	| bxtype				{ $$ = $1 }
	;

/* S !Int Bool; at least one ! */
bxtype	: btype wierd_atype			{ $$ = mktapp($1, $2); }
	| bxtype batype				{ $$ = mktapp($1, $2); }
	;

bbtype	:  btype				{ $$ = $1; }
	|  wierd_atype				{ $$ = $1; }
	;

batype	:  atype				{ $$ = $1; }
	|  wierd_atype				{ $$ = $1; }
	;

/* A wierd atype is one that isn't a regular atype;
   it starts with a "!", or with a forall. */
wierd_atype : BANG bigatype			{ $$ = mktbang( $2 ) }
	    | BANG atype			{ $$ = mktbang( $2 ) }
	    | bigatype 
	    ;

batypes	:  					{ $$ = Lnil; }
	|  batypes batype			{ $$ = lapp($1,$2); }
	;


fields	: field					{ $$ = lsing($1); }
	| fields COMMA field			{ $$ = lapp($1,$3); }
	;

field	:  qvars_list DCOLON ctype		{ $$ = mkfield($1,$3); }
	|  qvars_list DCOLON BANG atype		{ $$ = mkfield($1,mktbang($4)); }
 	|  qvars_list DCOLON BANG bigatype	{ $$ = mkfield($1,mktbang($4)); }
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


valdef	:  vallhs
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
		  else
		    $$ = mkfbind($3,startlineno);

		  PREVPATT = NULL;
		}
	;

vallhs  : patk					{ $$ = $1; }
	| patk qvarop pat			{ $$ = mkinfixap($2,$1,$3); }
	| funlhs				{ $$ = $1; }
	;

funlhs	:  qvark apat				{ $$ = mkap(mkident($1),$2); }
	|  funlhs apat				{ $$ = mkap($1,$2); }
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
           /* A where containing no decls is OK */
	|  WHERE SEMI				{ $$ = mknullbind(); }
	|  /* empty */				{ $$ = mknullbind(); }
	;

gd	:  VBAR quals				{ $$ = $2; }
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
	/* 8 S/R conflicts on qop -> shift */
oexp	:  oexp qop oexp %prec MINUS		{ $$ = mkinfixap($2,$1,$3); }
	|  dexp
	;

/*
  This comes here because of the funny precedence rules concerning
  prefix minus.
*/
dexp	:  MINUS kexp				{ $$ = mknegate($2); }
	|  kexp
	;

/*
  We need to factor out a leading let expression so we can set
  inpat=TRUE when parsing (non let) expressions inside stmts and quals
*/
expLno 	:  oexpLno DCOLON ctype			{ $$ = mkrestr($1,$3); }
	|  oexpLno
	;
oexpLno	:  oexpLno qop oexp %prec MINUS		{ $$ = mkinfixap($2,$1,$3); }
	|  dexpLno
	;
dexpLno	:  MINUS kexp				{ $$ = mknegate($2); }
	|  kexpLno
	;

expL 	:  oexpL DCOLON ctype			{ $$ = mkrestr($1,$3); }
	|  oexpL
	;
oexpL	:  oexpL qop oexp %prec MINUS		{ $$ = mkinfixap($2,$1,$3); }
	|  kexpL
	;

/*
  let/if/lambda/case have higher precedence than infix operators.
*/

kexp	:  kexpL
	|  kexpLno
	;

/* kexpL = a let expression */
kexpL	:  letdecls IN exp			{ $$ = mklet($1,$3); }
	;

/* kexpLno = any other expression more tightly binding than operator application */
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
	|  OPAREN exp CPAREN			{ $$ = mkpar($2); }	    /* mkpar: stop infix parsing at ()'s */
	|  qcon OCURLY rbinds CCURLY		{ $$ = mkrecord($1,$3); }   /* 1 S/R conflict on OCURLY -> shift */
	|  OBRACK list_exps CBRACK		{ $$ = mkllist($2); }
	|  OPAREN exp COMMA texps CPAREN	{ if (ttree($4) == tuple)
			     			     $$ = mktuple(mklcons($2, gtuplelist((struct Stuple *) $4)));
						  else
						     $$ = mktuple(ldub($2, $4)); }

	/* only in expressions ... */
	|  aexp OCURLY rbinds1 CCURLY		{ $$ = mkrupdate($1,$3); }
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

rbinds	:  /* empty */				{ $$ = Lnil; }
	|  rbinds1
	;

rbinds1	:  rbind				{ $$ = lsing($1); }
	|  rbinds1 COMMA rbind			{ $$ = lapp($1,$3); }
	;

rbind  	:  qvar					{ $$ = mkrbind($1,mknothing()); }
	|  qvar EQUAL exp			{ $$ = mkrbind($1,mkjust($3)); }
	;

texps	:  exp	{ $$ = mkpar($1); }	/* mkpar: so we don't flatten last element in tuple */
	|  exp COMMA texps
		{ if (ttree($3) == tuple)
		    $$ = mktuple(mklcons($1, gtuplelist((struct Stuple *) $3)));
		  else if (ttree($3) == par)
		    $$ = mktuple(ldub($1, gpare((struct Spar *) $3)));
		  else
		    hsperror("hsparser:texps: panic");
		}
	/* right recursion? WDP */
	;

list_exps :
	   exp					{ $$ = lsing($1); }
	|  exp COMMA exp			{ $$ = mklcons( $1, lsing($3) ); }
	|  exp COMMA exp COMMA list_rest	{ $$ = mklcons( $1, mklcons( $3, reverse_list( $5 ))); }
	;

/* Use left recusion for list_rest, because we sometimes get programs with
   very long explicit lists. */
list_rest : 	exp				{ $$ = lsing($1); }
	  | list_rest COMMA exp			{ $$ = mklcons( $3, $1 ); }
	  ;

/* 
	   exp					{ $$ = lsing($1); }
	|  exp COMMA list_exps		{ $$ = mklcons($1, $3); }
*/
	/* right recursion? (WDP)

	   It has to be this way, though, otherwise you
	   may do the wrong thing to distinguish between...

	   [ e1 , e2 .. ]	-- an enumeration ...
	   [ e1 , e2 , e3 ]	-- a list

	   (In fact, if you change the grammar and throw yacc/bison
	   at it, it *will* do the wrong thing [WDP 94/06])
	*/

letdecls:  LET ocurly decls ccurly		{ $$ = $3 }
	|  LET vocurly decls vccurly		{ $$ = $3 }
	;

quals	:  qual					{ $$ = lsing($1); }
	|  quals COMMA qual			{ $$ = lapp($1,$3); }
	;

qual	:  letdecls				{ $$ = mkseqlet($1); }
	|  expL					{ $$ = $1; }
	|  {inpat=TRUE;} expLno 
	   {inpat=FALSE;} leftexp
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
	|  expL					{ $$ = lsing(mkdoexp($1,hsplineno)); }
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

pat	:  qvar PLUS INTEGER			{ $$ = mkplusp($1, mkinteger($3)); }
	|  cpat
	;

cpat	:  cpat qconop bpat			{ $$ = mkinfixap($2,$1,$3); }
	|  bpat
	;

bpat	:  apatc
 	|  conpat
 	|  qcon OCURLY rpats CCURLY		{ $$ = mkrecord($1,$3); }
	|  MINUS INTEGER			{ $$ = mknegate(mklit(mkinteger($2))); }
	|  MINUS FLOAT				{ $$ = mknegate(mklit(mkfloatr($2))); }
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
	|  CLITLIT /* yurble yurble */		{ $$ = mkclitlit($1); }
	;

lampats	:  apat lampats				{ $$ = mklcons($1,$2); }
	|  apat					{ $$ = lsing($1); }
	/* right recursion? (WDP) */
	;

pats	:  pat COMMA pats			{ $$ = mklcons($1, $3); }
	|  pat					{ $$ = lsing($1); }
    	/* right recursion? (WDP) */
	;

rpats	: /* empty */				{ $$ = Lnil; }
	| rpats1
	;

rpats1	: rpat					{ $$ = lsing($1); }
	| rpats1 COMMA rpat			{ $$ = lapp($1,$3); }
	;

rpat	:  qvar					{ $$ = mkrbind($1,mknothing()); }
	|  qvar EQUAL pat			{ $$ = mkrbind($1,mkjust($3)); }
	;


patk	:  patk qconop bpat			{ $$ = mkinfixap($2,$1,$3); }
	|  bpatk
	;

bpatk	:  apatck
 	|  conpatk
	|  qconk OCURLY rpats CCURLY		{ $$ = mkrecord($1,$3); }
	|  minuskey INTEGER			{ $$ = mknegate(mklit(mkinteger($2))); }
	|  minuskey FLOAT			{ $$ = mknegate(mklit(mkfloatr($2))); }
	;

conpatk	:  gconk				{ $$ = mkident($1); }
	|  conpatk apat				{ $$ = mkap($1,$2); }
	;

apatck	:  qvark		 		{ $$ = mkident($1); }
	|  qvark AT apat			{ $$ = mkas($1,$3); }
	|  lit_constant				{ $$ = mklit($1); setstartlineno(); }
	|  WILDCARD				{ $$ = mkwildp(); setstartlineno(); }
	|  oparenkey pat CPAREN			{ $$ = mkpar($2); }
	|  oparenkey pat COMMA pats CPAREN 	{ $$ = mktuple(mklcons($2,$4)); }
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

/**********************************************************************
*                                                                     *
*                                                                     *
*     Keywords which record the line start			      *
*                                                                     *
*                                                                     *
**********************************************************************/

importkey: IMPORT	         { setstartlineno(); $$ = 0; }
        |  IMPORT SOURCE_UPRAGMA { setstartlineno(); $$ = 1; }
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

minuskey:   MINUS	{ setstartlineno(); }
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

/* PLUS, BANG are valid varsyms */
varsym_nominus : VARSYM
	|  PLUS				{ $$ = install_literal("+"); }
	|  BANG				{ $$ = install_literal("!"); }	
	;

/* AS HIDING QUALIFIED are valid varids */
varid   :  VARID
	|  AS				{ $$ = install_literal("as"); }
	|  HIDING			{ $$ = install_literal("hiding"); }
	|  QUALIFIED			{ $$ = install_literal("qualified"); }
	;


ccallid	:  VARID
	|  CONID
	;

tyvar	:  varid			{ $$ = mknamedtvar(mknoqual($1)); }
	;
tycon	:  CONID
	;
modid	:  CONID
	;

/*
tyvar_list: tyvar			{ $$ = lsing($1); }
	|  tyvar_list COMMA tyvar 	{ $$ = lapp($1,$3); }
	;
*/

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

void
checkinpat()
{
  if(!inpat)
    hsperror("pattern syntax used in expression");
}


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
	fprintf(stderr, "%s:%d:%d: %s on input: ",
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
