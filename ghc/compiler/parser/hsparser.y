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

/* For FN, SAMEFN macros */
extern qid	fns[];
extern BOOLEAN	samefn[];
extern short	icontexts;

/* Line Numbers */
extern int hsplineno, hspcolno;
extern int modulelineno;
extern int startlineno;
extern int endlineno;

/* Local helper functions */
static void checkinpat        PROTO((void));
static void punningNowIllegal PROTO((void));


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
BOOLEAN pat_check=TRUE;

%}

%union {
	tree utree;
	list ulist;
	ttype uttype;
	constr uconstr;
	binding ubinding;
        match umatch;
        gdexp ugdexp;
        grhsb ugrhsb;
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
%token	BQUOTE		OPAREN		CPAREN
%token  OUNBOXPAREN     CUNBOXPAREN


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

%token	DOT		FORALL
%token  EXPORT          UNSAFE          STDCALL		C_CALL   LABEL
%token  PASCAL		FASTCALL	FOREIGN         DYNAMIC

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
%token  INLINE_UPRAGMA NOINLINE_UPRAGMA MAGIC_UNFOLDING_UPRAGMA
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


%type <ulist>   caserest alts quals
		dorest stmts stmt
		rbinds rbinds1 rpats rpats1 list_exps list_rest
		qvarsk qvars_list
		constrs fields conargatypes
		tautypes atypes
		types_and_maybe_ids
  		pats simple_context simple_context_list
		export_list enames
  		import_list inames
 		impdecls maybeimpdecls impdecl
		dtyclses dtycls_list
  		gdrhs gdpat 
  		lampats	cexps gd texps
		tyvars1 constr_context forall

%type <umatch>  alt

%type <ugrhsb>  valrhs altrhs

%type <umaybe>  maybeexports impspec deriving 
		ext_name opt_sig opt_asig

%type <uliteral> lit_constant

%type <utree>	exp oexp dexp kexp fexp aexp rbind
		expL oexpL kexpL expLno oexpLno dexpLno kexpLno
		funlhs funlhs1 funlhs2 funlhs3 qual leftexp
 		pat dpat cpat bpat apat apatc conpat rpat
          	patk bpatk apatck conpatk


%type <uid>	MINUS PLUS DARROW AS LAZY
		VARID CONID VARSYM CONSYM 
  		var con varop conop op
		vark varid varsym varsym_nominus
	        tycon modid ccallid tyvar
		varid_noforall

%type <uqid>	QVARID QCONID QVARSYM QCONSYM 
		qvarid qconid qvarsym qconsym
		qvar qcon qvarop qconop qop
		qvark qconk qtycon qtycls
		gcon gconk gtycon itycon qop1 qvarop1 
		ename iname

%type <ubinding>  topdecl topdecls topdecls1 letdecls
		  typed datad newtd classd instd defaultd foreignd
		  decl decls fixdecl fix_op fix_ops valdef
 		  maybe_where with_where where_body type_and_maybe_id

%type <uttype>    polytype
		  conargatype conapptype
		  tautype
		  apptype
		  atype polyatype
		  simple_con_app simple_con_app1 inst_type

%type <uconstr>	  constr constr_after_context field constr1

%type <ustring>   FLOAT INTEGER INTPRIM
		  FLOATPRIM DOUBLEPRIM CLITLIT

%type <uhstring>  STRING STRINGPRIM CHAR CHARPRIM

%type <uentid>	  export import

%type <ulong>     commas importkey get_line_no
		  unsafe_flag callconv

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

body	:  ocurly { setstartlineno(); } main_body ccurly
        |  vocurly                      main_body vccurly
	;

main_body  :  interface_pragma maybeimpdecls topdecls
	       {
		 root = mkhmodule(the_module_name, $2, module_exports,
				  $3, source_version,modulelineno);
	       }
	   |  interface_pragma impdecls
	       {
		 root = mkhmodule(the_module_name, $2, module_exports,
			          mknullbind(), source_version, modulelineno);
	       }

interface_pragma : /* empty */
	| INTERFACE_UPRAGMA INTEGER END_UPRAGMA SEMI
	       {
		 source_version = atoi($2);
	       }
        ;

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
	|  gcon
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
	|  importkey modid AS modid impspec
		{ $$ = lsing(mkimport($3,1,mkjust($4),$5,$1,startlineno)); }
	;

impspec	:  /* empty */				  { $$ = mknothing(); }
	|  OPAREN CPAREN			  { $$ = mkjust(mkleft(Lnil));  }
	|  OPAREN import_list CPAREN		  { $$ = mkjust(mkleft($2));    }
	|  OPAREN import_list COMMA CPAREN	  { $$ = mkjust(mkleft($2));    }
	|  HIDING OPAREN CPAREN	  		  { $$ = mkjust(mkright(Lnil)); }
	|  HIDING OPAREN import_list CPAREN	  { $$ = mkjust(mkright($3));   }
	|  HIDING OPAREN import_list COMMA CPAREN { $$ = mkjust(mkright($3));   }
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
	|  OBRACK CBRACK			{ $$ = creategid(NILGID); }
	|  OPAREN CPAREN			{ $$ = creategid(UNITGID); }         
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

topdecls : /* empty */                  { $$ = mknullbind(); }
         | topdecls1
	 ;

topdecls1:  topdecl
	 |  topdecls1 SEMI  		{ $$ = $1; }
	 |  topdecls1 SEMI topdecl
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
	|  foreignd				{ $$ = $1; FN = NULL; SAMEFN = 0; }
	|  decl 				{ $$ = $1; }
	;

typed	:  typekey simple_con_app EQUAL tautype		{ $$ = mknbind($2,$4,startlineno); }
	;


datad	:  datakey simple_con_app EQUAL constrs deriving
		{ $$ = mktbind(Lnil,$2,$4,$5,startlineno); }
	|  datakey simple_context DARROW simple_con_app EQUAL constrs deriving
		{ $$ = mktbind($2,$4,$6,$7,startlineno); }
	;

newtd	:  newtypekey simple_con_app EQUAL constr1 deriving
		{ $$ = mkntbind(Lnil,$2,lsing($4),$5,startlineno); }
	|  newtypekey simple_context DARROW simple_con_app EQUAL constr1 deriving
		{ $$ = mkntbind($2,$4,lsing($6),$7,startlineno); }
	;

deriving: /* empty */				{ $$ = mknothing(); }
        | DERIVING dtyclses                     { $$ = mkjust($2); }
	;

classd	:  classkey apptype DARROW simple_con_app1 maybe_where
		/* Context can now be more than simple_context */
		{ $$ = mkcbind(type2context($2),$4,$5,startlineno); }
	|  classkey apptype maybe_where
		/* We have to say apptype rather than simple_con_app1, else
		   we get reduce/reduce errs */
		{ check_class_decl_head($2);
		  $$ = mkcbind(Lnil,$2,$3,startlineno); }
	;

instd	:  instkey inst_type maybe_where	{ $$ = mkibind($2,$3,startlineno); }
	;

/* Compare polytype */
/* [July 98: first production was tautype DARROW tautype, but I can't see why.] */
inst_type : apptype DARROW apptype		{ is_context_format( $3, 0 );   /* Check the instance head */
						  $$ = mkforall(Lnil,type2context($1),$3); }
	  | apptype				{ is_context_format( $1, 0 );   /* Check the instance head */
						  $$ = $1; }
	  ;


defaultd:  defaultkey OPAREN tautypes CPAREN       { $$ = mkdbind($3,startlineno); }
	|  defaultkey OPAREN CPAREN		{ $$ = mkdbind(Lnil,startlineno); }
	;

/* FFI primitive declarations - GHC/Hugs specific */
foreignd:  foreignkey IMPORT callconv ext_name unsafe_flag qvarid DCOLON tautype
                   { $$ = mkfobind($6,$8,$4,$5,$3,FOREIGN_IMPORT,startlineno); }
        |  foreignkey EXPORT callconv ext_name qvarid DCOLON tautype
                   { $$ = mkfobind($5,$7,$4,0,$3,FOREIGN_EXPORT,startlineno); }
        |  foreignkey LABEL ext_name qvarid DCOLON tautype
                   { $$ = mkfobind($4,$6,$3,0,-1,FOREIGN_LABEL,startlineno); }
	;

callconv: STDCALL 	{ $$ = CALLCONV_STDCALL;  }
	| C_CALL        { $$ = CALLCONV_CCALL;    }
	| PASCAL        { $$ = CALLCONV_PASCAL;   }
	| FASTCALL      { $$ = CALLCONV_FASTCALL; }
/* If you leave out the specification of a calling convention, you'll (probably) get C's. */
        | /*empty*/     { $$ = CALLCONV_NONE;    }
	;

ext_name: STRING	{ $$ = mkjust(lsing($1)); }
	| STRING STRING { $$ = mkjust(mklcons ($1,lsing($2))); }
        | DYNAMIC       { $$ = mknothing();   }

unsafe_flag: UNSAFE	{ $$ = 1; }
	   | /*empty*/  { $$ = 0; }
	   ;

decls	: decl
	| decls SEMI		{ $$ = $1; }
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

decl	: fixdecl

        | qvarsk DCOLON polytype
		{ $$ = mksbind($1,$3,startlineno);
		  FN = NULL; SAMEFN = 0;
		}

        | qvark DCOLON polytype
		{ $$ = mksbind(lsing($1),$3,startlineno);
		  FN = NULL; SAMEFN = 0;
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
		  FN = NULL; SAMEFN = 0;
		}

	|  SPECIALISE_UPRAGMA INSTANCE gtycon atype END_UPRAGMA
		{
		  $$ = mkispec_uprag($3, $4, startlineno);
		  FN = NULL; SAMEFN = 0;
		}

	|  SPECIALISE_UPRAGMA DATA gtycon atypes END_UPRAGMA
		{
		  $$ = mkdspec_uprag($3, $4, startlineno);
		  FN = NULL; SAMEFN = 0;
		}

	|  INLINE_UPRAGMA qvark END_UPRAGMA
		{
		  $$ = mkinline_uprag($2, startlineno);
		  FN = NULL; SAMEFN = 0;
		}

	|  NOINLINE_UPRAGMA qvark END_UPRAGMA
		{
		  $$ = mknoinline_uprag($2, startlineno);
		  FN = NULL; SAMEFN = 0;
		}

	|  MAGIC_UNFOLDING_UPRAGMA qvark vark END_UPRAGMA
		{
		  $$ = mkmagicuf_uprag($2, $3, startlineno);
		  FN = NULL; SAMEFN = 0;
		}

	/* end of user-specified pragmas */

	|  valdef
  	;

fixdecl	:  INFIXL INTEGER	{ Precedence = checkfixity($2); Fixity = INFIXL; }
	   fix_ops  		{ $$ = $4; }
	|  INFIXR INTEGER	{ Precedence = checkfixity($2); Fixity = INFIXR; }
	   fix_ops  		{ $$ = $4; }
	|  INFIX  INTEGER	{ Precedence = checkfixity($2); Fixity = INFIX; }
	   fix_ops  		{ $$ = $4; }
	|  INFIXL		{ Fixity = INFIXL; Precedence = 9; }
	   fix_ops  		{ $$ = $3; }
	|  INFIXR		{ Fixity = INFIXR; Precedence = 9; }
	   fix_ops  		{ $$ = $3; }
	|  INFIX		{ Fixity = INFIX; Precedence = 9; }
	   fix_ops  		{ $$ = $3; }
	;

/* Grotesque global-variable hack to
   make a separate fixity decl for each op */
fix_ops	:  fix_op
        |  fix_ops COMMA fix_op { $$ = mkabind($1,$3); }
	;

fix_op  : op                    { $$ = mkfixd(mknoqual($1),infixint(Fixity),Precedence,startlineno); }
        ;

qvarsk	:  qvark COMMA qvars_list		{ $$ = mklcons($1,$3); }
	;

qvars_list: qvar				{ $$ = lsing($1); }
	|   qvars_list COMMA qvar		{ $$ = lapp($1,$3); }
	;

types_and_maybe_ids :
	   type_and_maybe_id				{ $$ = lsing($1); }
	|  types_and_maybe_ids COMMA type_and_maybe_id	{ $$ = lapp($1,$3); }
	;

type_and_maybe_id :
	   tautype				{ $$ = mkvspec_ty_and_id($1,mknothing()); }
	|  tautype EQUAL qvark			{ $$ = mkvspec_ty_and_id($1,mkjust($3)); }


/**********************************************************************
*                                                                     *
*                                                                     *
*     Types etc     	 					      *
*                                                                     *
*                                                                     *
**********************************************************************/

/*  "DCOLON context => tautype" vs "DCOLON tautype" is a problem,
    because you can't distinguish between

	foo :: (Baz a, Baz a)
	bar :: (Baz a, Baz a) => [a] -> [a] -> [a]

    with one token of lookahead.  The HACK is to have "DCOLON apptype"
    in the first case, then check that it has the right
    form C a, or (C1 a, C2 b, ... Cn z) and convert it into a
    context.  Blaach!
*/

/* --------------------------- */

polyatype : atype
          ;

polytype : FORALL tyvars1 DOT
                  apptype DARROW tautype	{ $$ = mkforall($2,   type2context($4), $6); }
         | FORALL tyvars1 DOT tautype           { $$ = mkforall($2,   Lnil,             $4); }
         |        apptype DARROW tautype	{ $$ = mkforall(Lnil, type2context($1), $3); }
         | tautype
	 ;

/* --------------------------- */
/* tautype is just a monomorphic type.
   But it may have nested for-alls if we're in a rank-2 type */

tautype :  apptype RARROW tautype		{ $$ = mktfun($1,$3); }
	|  apptype				{ $$ = $1; }
	;

tautypes :  tautype				{ $$ = lsing($1); }
	 |  tautypes COMMA tautype		{ $$ = lapp($1,$3); }
	 ;

/* --------------------------- */
/* apptype: type application */

apptype	:  apptype atype			{ $$ = mktapp($1,$2); }
	|  atype				{ $$ = $1; }
	;

/* --------------------------- */
/* atype: an atomic or bracketed type: T, x, [ty], tuple ty */

atypes :  atype				        { $$ = lsing($1); }
	  |  atype atypes		        { $$ = mklcons($1,$2); }
	  ;

atype   :  gtycon				{ $$ = mktname($1); }
	|  tyvar				{ $$ = mknamedtvar($1); }

	|  OPAREN tautype COMMA
		  tautypes CPAREN		{ $$ = mkttuple(mklcons($2,$4)); }

	|  OUNBOXPAREN tautype COMMA 
		       tautypes CUNBOXPAREN	{ $$ = mktutuple(mklcons($2,$4)); }

	|  OBRACK tautype CBRACK		{ $$ = mktllist($2); }
        |  OPAREN polytype CPAREN		{ $$ = $2; }
	;

/* --------------------------- */
gtycon	:  qtycon
	|  OPAREN RARROW CPAREN			{ $$ = creategid(ARROWGID); }
	|  OBRACK CBRACK			{ $$ = creategid(NILGID); }         
	|  OPAREN CPAREN			{ $$ = creategid(UNITGID); }         
	|  OPAREN commas CPAREN 		{ $$ = creategid($2); }
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

/* C a b c, where a,b,c are type variables */
/* C can be a class or tycon */

/* simple_con_app can have no args; simple_con_app1 must have at least one */
simple_con_app: gtycon                          { $$ = mktname($1); }
        |  simple_con_app1                      { $$ = $1; }
	;
   
simple_con_app1:  gtycon tyvar			{ $$ = mktapp(mktname($1),mknamedtvar($2)); }
	|  simple_con_app1 tyvar		{ $$ = mktapp($1, mknamedtvar($2)); } 
	;

simple_context	:  OPAREN simple_context_list CPAREN		{ $$ = $2; }
	| OPAREN CPAREN						{ $$ = Lnil; }
	|  simple_con_app1					{ $$ = lsing($1); }
	;

simple_context_list :  simple_con_app1				{ $$ = lsing($1); }
	|  simple_context_list COMMA simple_con_app1		{ $$ = lapp($1,$3); }
	;

constrs	:  constr				{ $$ = lsing($1); }
	|  constrs VBAR constr			{ $$ = lapp($1,$3); }
	;

constr	:  forall constr_context DARROW constr_after_context	{ $$ = mkconstrex ( $1, $2, $4 ); }
        |  forall constr_after_context	                        { $$ = mkconstrex ( $1, Lnil, $2 ); }
	;

forall :                                                { $$ = Lnil }
       | FORALL tyvars1 DOT                             { $$ = $2; }
       ;

constr_context
	: conapptype conargatype 	{ $$ = type2context( mktapp($1,$2) ); }
   	| conargatype 			{ $$ = type2context( $1 ); }
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
	   conapptype				{ qid tyc; list tys;
						  splittyconapp($1, &tyc, &tys);
					          $$ = mkconstrpre(tyc,tys,hsplineno); }

/* (::) (Tree a) Int */
	|  OPAREN qconsym CPAREN conargatypes	{ $$ = mkconstrpre($2,$4,hsplineno); }

/* !Int `Con` Tree a */
	|  conargatype qconop conargatype	{ $$ = mkconstrinf($1,$2,$3,hsplineno); }

/* Con { op1 :: Int } */
	| qtycon OCURLY CCURLY			{ $$ = mkconstrrec($1,Lnil,hsplineno); }
	| qtycon OCURLY fields CCURLY		{ $$ = mkconstrrec($1,$3,hsplineno); }
	| OPAREN qconsym CPAREN OCURLY fields CCURLY { $$ = mkconstrrec($2,$5,hsplineno); }
	;
		/* 1 S/R conflict on OCURLY -> shift */


conapptype : gtycon				{ $$ = mktname($1); }
	   | conapptype conargatype		{ $$ = mktapp($1, $2); }
	   ;

conargatype : polyatype				{ $$ = $1; }
	    | BANG polyatype			{ $$ = mktbang( $2 ); }
	    ;

conargatypes :  				{ $$ = Lnil; }
	  |  conargatype conargatypes		{ $$ = mklcons($1,$2); }
	  ;

fields	: field					{ $$ = lsing($1); }
	| fields COMMA field			{ $$ = lapp($1,$3); }
	;

field	:  qvars_list DCOLON polytype		{ $$ = mkfield($1,$3); }
 	|  qvars_list DCOLON BANG polyatype	{ $$ = mkfield($1,mktbang($4)); }
	; 

constr1 : gtycon conargatype			    { $$ = mkconstrnew($1,$2,mknothing(),hsplineno); }
	| gtycon OCURLY qvar DCOLON polytype CCURLY { $$ = mkconstrnew($1,$5,mkjust($3),hsplineno); }
	;


dtyclses:  OPAREN dtycls_list CPAREN		{ $$ = $2; }
	|  OPAREN CPAREN			{ $$ = Lnil; }
	|  qtycls				{ $$ = lsing($1); }
	;

dtycls_list:  qtycls				{ $$ = lsing($1); }
	|  dtycls_list COMMA qtycls		{ $$ = lapp($1,$3); }
	;

valdef	:  funlhs opt_sig	{ checksamefn($1); }	
	   get_line_no valrhs  	{ $$ = mkfbind( lsing(mkpmatch( lsing($1), $2, $5 )), $4); }

/* Special case for  f :: type = e
   We treat it as a special kind of pattern binding */
        |  qvark DCOLON tautype 
           get_line_no valrhs   { $$ = mkpbind( mkrestr( mkident($1), $3 ), $5, $4 ); 
                                  FN = NULL; SAMEFN = 0; }

        |  patk                 
           get_line_no valrhs   { $$ = mkpbind($1, $3, $2);
                        	  FN = NULL; SAMEFN = 0; }

get_line_no : 					{ $$ = hsplineno; /* startlineno; */ }
	    ;
/* This grammar still isn't quite right
   If you say
      (x + 2) y = e
   you should get a function binding, but actually the (x+3) will
   parse as a pattern, and you'll get a parse error. */

funlhs  : patk qvarop cpat			{ $$ = mkinfixap($2,$1,$3); }
        | funlhs1 apat                          { $$ = mkap( $1, $2 ); }

funlhs1 : oparenkey funlhs2 CPAREN              { $$ = mkpar($2); }
        | funlhs1 apat                          { $$ = mkap( $1, $2 ); }
        | qvark                                 { $$ = mkident($1); }
        ;

funlhs2 : cpat qvarop cpat			{ $$ = mkinfixap($2,$1,$3); }
        | funlhs3 apat                          { $$ = mkap( $1, $2 ); }

funlhs3 : OPAREN funlhs2 CPAREN                 { $$ = mkpar($2); }
        | funlhs3 apat                          { $$ = mkap( $1, $2 ); }
        | qvar                                  { $$ = mkident($1); }
        ;

opt_sig :                                       { $$ = mknothing(); }
        |  DCOLON tautype                       { $$ = mkjust($2); }
        ;

/* opt_asig is the same, but with a parenthesised type */
opt_asig :                                       { $$ = mknothing(); }
         |  DCOLON atype                         { $$ = mkjust($2); }
         ;

valrhs	:  EQUAL get_line_no exp maybe_where	{ $$ = mkpnoguards($2, $3, $4); }
        |  gdrhs maybe_where		        { $$ = mkpguards($1, $2); }
	;

gdrhs	:  gd EQUAL get_line_no exp		{ $$ = lsing(mkpgdexp($1,$3,$4)); }
	|  gd EQUAL get_line_no exp gdrhs	{ $$ = mklcons(mkpgdexp($1,$3,$4),$5); }
	;

maybe_where: /* empty */			{ $$ = mknullbind(); }
	   | WHERE with_where			{ $$ = $2; }
	   ;

with_where : /* empty */			{ $$ = mknullbind(); }
	   | where_body				{ $$ = $1; }
	   ;

where_body : ocurly  decls ccurly		{ $$ = $2; }
	   | vocurly decls vccurly		{ $$ = $2; }
	   | ocurly ccurly			{ $$ = mknullbind(); }
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

exp	:  oexp DCOLON polytype			{ $$ = mkrestr($1,$3); }
	|  oexp
	;

/*
  Operators must be left-associative at the same precedence for
  precedence parsing to work.
*/
	/* 10 S/R conflicts on qop -> shift */
oexp	:  oexp qop dexp %prec MINUS		{ $$ = mkinfixap($2,$1,$3); }
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
  pat_check=FALSE when parsing (non let) expressions inside stmts and quals
*/
expLno 	: oexpLno DCOLON polytype		{ $$ = mkrestr($1,$3); }
	| oexpLno
	;
oexpLno	:  oexpLno qop oexp %prec MINUS		{ $$ = mkinfixap($2,$1,$3); }
	|  dexpLno
	;
dexpLno	:  MINUS kexp				{ $$ = mknegate($2); }
	|  kexpLno
	;

expL 	:  oexpL DCOLON polytype		{ $$ = mkrestr($1,$3); }
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
		}
	   lampats opt_asig
  		{ hsendindent(); }

	   RARROW get_line_no exp	/* lambda abstraction */
		{ $$ = mklambda( mkpmatch( $3, $4, mkpnoguards( $7, $8, mknullbind() ) ) ); }

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
		    if (warnSCC) {
		        fprintf(stderr,
			        "\"%s\":%d: _scc_ (`set [profiling] cost centre') ignored\n",
			        input_filename, hsplineno);
		    }
		    $$ = mkpar($3);	/* Note the mkpar().  If we don't have it, then
					   (x >> _scc_ y >> z) parses as (x >> (y >> z)),
					   right associated.  But the precedence reorganiser expects
					   the parser to *left* associate all operators unless there
					   are explicit parens.  The _scc_ acts like an explicit paren,
					   so if we omit it we'd better add explicit parens instead. */
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
	|  OPAREN exp COMMA texps CPAREN	{ $$ = mktuple(mklcons($2,$4)); }
        /* unboxed tuples */
	|  OUNBOXPAREN exp COMMA texps CUNBOXPAREN 
						{ $$ = mkutuple(mklcons($2,$4)); }

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
	;

	/* ccall arguments */
cexps	:  cexps aexp				{ $$ = lapp($1,$2); }
	|  aexp					{ $$ = lsing($1); }
	;

caserest:  ocurly alts ccurly			{ $$ = $2; }
	|  vocurly alts vccurly			{ $$ = $2; }

dorest  :  ocurly stmts ccurly		        { checkdostmts($2); $$ = $2; }
	|  vocurly stmts vccurly		{ checkdostmts($2); $$ = $2; }
	;

rbinds	:  /* empty */				{ $$ = Lnil; }
	|  rbinds1
	;

rbinds1	:  rbind				{ $$ = lsing($1); }
	|  rbinds1 COMMA rbind			{ $$ = lapp($1,$3); }
	;

rbind  	: qvar					{ punningNowIllegal();	       }
	| qvar EQUAL exp			{ $$ = mkrbind($1,mkjust($3)); }
	;	

texps	:  exp					{ $$ = lsing($1); }
	|  exp COMMA texps			{ $$ = mklcons($1, $3) }
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

letdecls:  LET { pat_check = TRUE; }  ocurly decls ccurly		{ $$ = $4; }
	|  LET { pat_check = TRUE; } vocurly decls vccurly		{ $$ = $4; }
	|  LET /* empty */						{ $$ = mknullbind(); }
	;

/*
 When parsing patterns inside do stmt blocks or quals, we have
 to tentatively parse them as expressions, since we don't know at
 the time of parsing `p' whether it will be part of "p <- e" (pat)
 or "p" (expr). When we eventually can tell the difference, the parse
 of `p' is examined to see if it consitutes a syntactically legal pattern
 or expression.

 The expr rule used to parse the pattern/expression do contain
 pattern-special productions (e.g., _ , a@pat, etc.), which are
 illegal in expressions. Since we don't know whether what
 we're parsing is an expression rather than a pattern, we turn off
 the check and instead do it later.
 
 The rather clumsy way that this check is turned on/off is there
 to work around a Bison feature/shortcoming. Turning the flag 
 on/off just around the relevant nonterminal by decorating it
 with simple semantic actions, e.g.,

    {pat_check = FALSE; } expLNo { pat_check = TRUE; }

 causes Bison to generate a parser where in one state it either
 has to reduce/perform a semantic action ( { pat_check = FALSE; })
 or reduce an error (the error production used to implement
 vccurly.) Bison picks the semantic action, which it ideally shouldn't.
 The work around is to lift out the setting of { pat_check = FALSE; }
 and then later reset pat_check. Not pretty.

*/


quals	:  { pat_check = FALSE;} qual 	           { pat_check = TRUE; $$ = lsing($2); }
	|  quals COMMA { pat_check = FALSE; } qual { pat_check = TRUE; $$ = lapp($1,$4); }
	;

qual	:  letdecls	                        { $$ = mkseqlet($1); }
	|  expL					{ expORpat(LEGIT_EXPR,$1); $$ = $1; }
 	|  expLno { pat_check = TRUE; } leftexp
 						{ if ($3 == NULL) {
 		      				     expORpat(LEGIT_EXPR,$1);
 		      				     $$ = mkguard($1);
		  				  } else {
		      				     expORpat(LEGIT_PATT,$1);
 		      				     $$ = mkqual($1,$3);
		  				  }
						}
	;

alts	:  /* empty */				{ $$ = Lnil; }
        |  alt                		        { $$ = lsing($1); }
	|  alt SEMI alts			{ $$ = mklcons($1,$3); }
        |  SEMI alts                            { $$ = $2; }
	;

alt	:  dpat opt_sig altrhs	                { $$ = mkpmatch( lsing($1), $2, $3 ); }
	;

altrhs	:  RARROW get_line_no exp maybe_where	{ $$ = mkpnoguards($2, $3, $4); }
	|  gdpat maybe_where	 		{ $$ = mkpguards($1, $2); }
	;  

gdpat	:  gd RARROW get_line_no exp		{ $$ = lsing(mkpgdexp($1,$3,$4)); }
	|  gd RARROW get_line_no exp gdpat	{ $$ = mklcons(mkpgdexp($1,$3,$4),$5);  }
	;

stmts	:  {pat_check = FALSE;} stmt 	      {pat_check=TRUE; $$ = $2; }
	|  stmts SEMI {pat_check=FALSE;} stmt {pat_check=TRUE; $$ = lconc($1,$4); }
	;

stmt	: /* empty */				{ $$ = Lnil; } 
	| letdecls		                { $$ = lsing(mkseqlet($1)); }
	| expL					{ expORpat(LEGIT_EXPR,$1); $$ = lsing(mkdoexp($1,hsplineno)); }
 	| expLno {pat_check=TRUE;} leftexp
 						{ if ($3 == NULL) {
 		      				     expORpat(LEGIT_EXPR,$1);
 		      				     $$ = lsing(mkdoexp($1,endlineno));
		  				  } else {
		      				     expORpat(LEGIT_PATT,$1);
		      				     $$ = lsing(mkdobind($1,$3,endlineno));
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

pat     :  dpat DCOLON tautype                  { $$ = mkrestr($1,$3); }
        |  dpat
        ;

dpat	:  qvar PLUS INTEGER			{ $$ = mkplusp($1, mkinteger($3)); }
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
	|  OPAREN pat CPAREN			{ $$ = mkpar($2); }
	|  OPAREN pat COMMA pats CPAREN 	{ $$ = mktuple(mklcons($2,$4)); }
	|  OUNBOXPAREN pat COMMA pats CUNBOXPAREN { $$ = mkutuple(mklcons($2,$4)); }
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

/* Sequence of apats for a lambda abstraction */
lampats	:  apat lampats				{ $$ = mklcons($1,$2); }
	|  apat					{ $$ = lsing($1); }
	/* right recursion? (WDP) */
	;

/* Comma-separated sequence of pats */
pats	:  pat COMMA pats			{ $$ = mklcons($1, $3); }
	|  pat					{ $$ = lsing($1); }
    	/* right recursion? (WDP) */
	;

/* Comma separated sequence of record patterns, each of form 'field=pat' */
rpats	: /* empty */				{ $$ = Lnil; }
	| rpats1
	;

rpats1	: rpat					{ $$ = lsing($1); }
	| rpats1 COMMA rpat			{ $$ = lapp($1,$3); }
	;

rpat	: qvar					{ punningNowIllegal();         } 
	| qvar EQUAL pat			{ $$ = mkrbind($1,mkjust($3)); }
	;


/* I can't figure out just what these ...k patterns are for.
   It seems to have something to do with recording the line number */

/* Corresponds to a cpat */
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
	|  oparenkey pat CPAREN			{ $$ = mkpar($2); }
	|  oparenkey pat COMMA pats CPAREN 	{ $$ = mktuple(mklcons($2,$4)); }
	|  ounboxparenkey pat COMMA pats CUNBOXPAREN
						{ $$ = mkutuple(mklcons($2,$4)); }
	|  obrackkey pats CBRACK		{ $$ = mkllist($2); }
	|  lazykey apat				{ $$ = mklazyp($2); }
	;


gcon	:  qcon
	|  OBRACK CBRACK			{ $$ = creategid(NILGID); }
	|  OPAREN CPAREN			{ $$ = creategid(UNITGID); }
	|  OPAREN commas CPAREN			{ $$ = creategid($2); }
	;

gconk	:  qconk
	|  obrackkey CBRACK			{ $$ = creategid(NILGID); }
	|  oparenkey CPAREN			{ $$ = creategid(UNITGID); }
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

foreignkey: FOREIGN	         { setstartlineno();  }
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

ounboxparenkey: OUNBOXPAREN { setstartlineno(); }
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
	|  DOT			        { $$ = install_literal("."); }
	;

/* AS HIDING QUALIFIED are valid varids */
varid   :  varid_noforall
        |  FORALL                       { $$ = install_literal("forall"); }
	;

varid_noforall
	:  VARID
	|  AS				{ $$ = install_literal("as"); }
	|  HIDING			{ $$ = install_literal("hiding"); }
	|  QUALIFIED			{ $$ = install_literal("qualified"); }
/* The rest of these guys are used by the FFI decls, a ghc (and hugs) extension. */
	|  EXPORT			{ $$ = install_literal("export"); }
	|  UNSAFE			{ $$ = install_literal("unsafe"); }
	|  DYNAMIC			{ $$ = install_literal("dynamic"); }
	|  LABEL			{ $$ = install_literal("label"); }
	|  C_CALL			{ $$ = install_literal("ccall"); }
	|  STDCALL			{ $$ = install_literal("stdcall"); }
	|  PASCAL			{ $$ = install_literal("pascal"); }
	;

ccallid	:  VARID
	|  CONID
	;

tycon	:  CONID
	;
modid	:  CONID
	;

/* ---------------------------------------------- */
tyvar	:  varid_noforall		{ $$ = $1; }
	;

/* tyvars1: At least one tyvar */
tyvars1 : tyvar				{ $$ = lsing($1); }
	| tyvar tyvars1  		{ $$ = mklcons($1,$2); }
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
		  FN = NULL; SAMEFN = 0;
		  hsendindent();
		}
	;

vccurly	:  { expect_ccurly = 1; }  vccurly1  { expect_ccurly = 0; }
	;

vccurly1:
	 VCCURLY
		{
		  FN = NULL; SAMEFN = 0;
		  hsendindent();
		}
	| error
		{
		  yyerrok;
		  FN = NULL; SAMEFN = 0;
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


static void checkinpat()
{
  if(pat_check)
    hsperror("pattern syntax used in expression");
}

static void punningNowIllegal()
{
  hsperror("Haskell 98 does not support 'punning' on records");
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
    if ( expect_ccurly && ! error_and_I_mean_it ) {
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
