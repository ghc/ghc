%{
/**********************************************************************
*                                                                     *
*                                                                     *
*       LEX grammar for Haskell.                                      *
*       ------------------------                                      *
*                                                                     *
*       (c) Copyright K. Hammond, University of Glasgow,              *
*               10th. February 1989                                   *
*                                                                     *
*       Modification History                                          *
*       --------------------                                          *
*                                                                     *
*       22/08/91 kh             Initial Haskell 1.1 version.          *
*       18/10/91 kh             Added 'ccall'.                        *
*       19/11/91 kh             Tidied generally.                     *
*       04/12/91 kh             Added Int#.                           *
*       31/01/92 kh             Haskell 1.2 version.                  *
*       24/04/92 ps             Added 'scc'.                          *
*       03/06/92 kh             Changed Infix/Prelude Handling.       *
*   	23/08/93 jsm	    	Changed to support flex	    	      *
*                                                                     *
*                                                                     *
*       Known Problems:                                               *
*                                                                     *
*               None, any more.                                       *
*                                                                     *
**********************************************************************/

#include "../../includes/config.h"

#include <stdio.h>

#if defined(STDC_HEADERS) || defined(HAVE_STRING_H)
#include <string.h>
/* An ANSI string.h and pre-ANSI memory.h might conflict.  */
#if !defined(STDC_HEADERS) && defined(HAVE_MEMORY_H)
#include <memory.h>
#endif /* not STDC_HEADERS and HAVE_MEMORY_H */
#define index strchr
#define rindex strrchr
#define bcopy(s, d, n) memcpy ((d), (s), (n))
#define bcmp(s1, s2, n) memcmp ((s1), (s2), (n))
#define bzero(s, n) memset ((s), 0, (n))
#else /* not STDC_HEADERS and not HAVE_STRING_H */
#include <strings.h>
/* memory.h and strings.h conflict on some systems.  */
#endif /* not STDC_HEADERS and not HAVE_STRING_H */

#include "hspincl.h"
#include "hsparser.tab.h"
#include "constants.h"
#include "utils.h"

/* Our substitute for <ctype.h> */

#define NCHARS  256
#define _S      0x1
#define _D      0x2
#define _H      0x4
#define _O      0x8
#define _C 	0x10

#define _isconstr(s) 	(CharTable[*s]&(_C))
BOOLEAN isconstr PROTO((char *)); /* fwd decl */

static unsigned char CharTable[NCHARS] = {
/* nul */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/* bs  */    	0,  	_S,  	_S,  	_S,  	_S,  	0,  	0,  	0,
/* dle */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/* can */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/* sp  */    	_S,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/* '(' */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/* '0' */	_D|_H|_O,_D|_H|_O,_D|_H|_O,_D|_H|_O,_D|_H|_O,_D|_H|_O,_D|_H|_O,_D|_H|_O,
/* '8' */    	_D|_H,	_D|_H,	_C,  	0,  	0,  	0,  	0,  	0,
/* '@' */    	0,  	_H|_C,	_H|_C,	_H|_C,	_H|_C,	_H|_C,	_H|_C,	_C,
/* 'H' */    	_C,  	_C,  	_C,  	_C,  	_C,  	_C,  	_C,  	_C,
/* 'P' */    	_C,  	_C,  	_C,  	_C,  	_C,  	_C,  	_C,  	_C,
/* 'X' */    	_C,  	_C,  	_C,  	0,  	0,  	0,  	0,  	0,
/* '`' */    	0,  	_H,  	_H,  	_H,  	_H,  	_H,  	_H,  	0,
/* 'h' */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/* 'p' */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/* 'x' */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,

/*     */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/*     */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/*     */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/*     */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/*     */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/*     */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/*     */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/*     */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/*     */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/*     */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/*     */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/*     */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/*     */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/*     */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/*     */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/*     */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
};

/**********************************************************************
*                                                                     *
*                                                                     *
*      Declarations                                                   *
*                                                                     *
*                                                                     *
**********************************************************************/

char *input_filename = NULL;	/* Always points to a dynamically allocated string */

/*
 * For my own sanity, things that are not part of the flex skeleton
 * have been renamed as hsXXXXX rather than yyXXXXX.  --JSM
 */

static int hslineno = 0;	/* Line number at end of token */
int hsplineno = 0;		/* Line number at end of previous token */

static int hscolno = 0;		/* Column number at end of token */
int hspcolno = 0;		/* Column number at end of previous token */
static int hsmlcolno = 0;	/* Column number for multiple-rule lexemes */

int startlineno = 0;		/* The line number where something starts */
int endlineno = 0;		/* The line number where something ends */

static BOOLEAN noGap = TRUE;	/* For checking string gaps */
static BOOLEAN forgetindent = FALSE;	/* Don't bother applying indentation rules */

static int nested_comments; 	/* For counting comment nesting depth */

/* Hacky definition of yywrap: see flex doc.

   If we don't do this, then we'll have to get the default
   yywrap from the flex library, which is often something
   we are not good at locating.  This avoids that difficulty.
   (Besides which, this is the way old flexes (pre 2.4.x) did it.)
   WDP 94/09/05
*/
#define yywrap() 1

/* Essential forward declarations */

static void hsnewid 	 PROTO((char *, int));
static void layout_input PROTO((char *, int));
static void cleartext	 (NO_ARGS);
static void addtext 	 PROTO((char *, unsigned));
static void addchar	 PROTO((char));
static char *fetchtext	 PROTO((unsigned *));
static void new_filename PROTO((char *));
static int  Return	 PROTO((int));
static void hsentercontext PROTO((int));

/* Special file handling for IMPORTS */
/*  Note: imports only ever go *one deep* (hence no need for a stack) WDP 94/09 */

static YY_BUFFER_STATE hsbuf_save = NULL;	/* Saved input buffer	 */
static char *filename_save;	    	/* File Name            	 */
static int hslineno_save = 0,	    	/* Line Number          	 */
 hsplineno_save = 0,	    		/* Line Number of Prev. token	 */
 hscolno_save = 0,		    	/* Indentation          	 */
 hspcolno_save = 0;		    	/* Left Indentation	 	 */
static short icontexts_save = 0;    	/* Indent Context Level 	 */

static BOOLEAN etags_save; /* saved: whether doing etags stuff or not */
extern BOOLEAN etags;	   /* that which is saved */

extern BOOLEAN nonstandardFlag;	/* Glasgow extensions allowed */

static BOOLEAN in_interface = FALSE; /* TRUE if we are reading a .hi file */

extern BOOLEAN ignorePragmas;		/* True when we should ignore pragmas */
extern int minAcceptablePragmaVersion;	/* see documentation in main.c */
extern int maxAcceptablePragmaVersion;
extern int thisIfacePragmaVersion;

static int hssttok = -1;	/* Stacked Token: -1   -- no token; -ve  -- ";"
				 * inserted before token +ve  -- "}" inserted before
				 * token */

short icontexts = 0;		/* Which context we're in */



/*
	Table of indentations:  right bit indicates whether to use
	  indentation rules (1 = use rules; 0 = ignore)

    partain:
    push one of these "contexts" at every "case" or "where"; the right bit says
    whether user supplied braces, etc., or not.  pop appropriately (hsendindent).

    ALSO, a push/pop when enter/exit a new file (e.g., on importing).  A -1 is
    pushed (the "column" for "module", "interface" and EOF).  The -1 from the initial
    push is shown just below.

*/


static short indenttab[MAX_CONTEXTS] = {-1};

#define INDENTPT (indenttab[icontexts]>>1)
#define INDENTON (indenttab[icontexts]&1)

#define RETURN(tok) return(Return(tok))

#undef YY_DECL
#define YY_DECL int yylex1()

/* We should not peek at yy_act, but flex calls us even for the internal action
   triggered on 'end-of-buffer' (This is not true of flex 2.4.4 and up, but
   to support older versions of flex, we'll continue to peek for now.
 */
#define YY_USER_ACTION \
    if (yy_act != YY_END_OF_BUFFER) layout_input(yytext, yyleng);

#if 0/*debug*/
#undef YY_BREAK
#define YY_BREAK if (etags) fprintf(stderr,"%d %d / %d %d / %d\n",hsplineno,hspcolno,hslineno,hscolno,startlineno); break;
#endif

/* Each time we enter a new start state, we push it onto the state stack.
   Note that the rules do not allow us to underflow or overflow the stack.
   (At least, they shouldn't.)  The maximum expected depth is 4:
   0: Code -> 1: String -> 2: StringEsc -> 3: Comment
*/
static int StateStack[5];
static int StateDepth = -1;

#ifdef HSP_DEBUG
#define PUSH_STATE(n)   do {\
    fprintf(stderr,"Pushing %d (%d)\n", n, StateDepth + 1);\
    StateStack[++StateDepth] = (n); BEGIN(n);} while(0)
#define POP_STATE       do {--StateDepth;\
    fprintf(stderr,"Popping %d (%d)\n", StateStack[StateDepth], StateDepth);\
    BEGIN(StateStack[StateDepth]);} while(0)
#else
#define PUSH_STATE(n)   do {StateStack[++StateDepth] = (n); BEGIN(n);} while(0)
#define POP_STATE       do {--StateDepth; BEGIN(StateStack[StateDepth]);} while(0)
#endif

%}

/* The start states are:
   Code -- normal Haskell code (principal lexer)
   GlaExt -- Haskell code with Glasgow extensions
   Comment -- Nested comment processing
   String -- Inside a string literal with backslashes
   StringEsc -- Immediately following a backslash in a string literal
   Char -- Inside a character literal with backslashes
   CharEsc -- Immediately following a backslash in a character literal 

   Note that the INITIAL state is unused.  Also note that these states
   are _exclusive_.  All rules should be prefixed with an appropriate
   list of start states.
 */

%x Char CharEsc Code Comment GlaExt GhcPragma UserPragma String StringEsc

D			[0-9]
O			[0-7]
H			[0-9A-Fa-f]
N			{D}+
F   	    	    	{N}"."{N}(("e"|"E")("+"|"-")?{N})?
S			[!#$%&*+./<=>?@\\^|~:]
SId			({S}|~|-){S}*
CHAR			[ !#$%&()*+,\-./0-9:;<=>?@A-Z\[\]^_`a-z{|}~]
L			[A-Z]
I			[A-Za-z]
i			[A-Za-z0-9'_]
Id			{I}({i})*
WS			[ \t\n\r\f\v]
CNTRL	    	    	[@A-Z\[\\\]^_]
NL  	    	    	[\n\r]

%%

%{
    /* 
     * Special GHC pragma rules.  Do we need a start state for interface files,
     * so these won't be matched in source files? --JSM
     */
%}

<Code,GlaExt>^"# ".*{NL}    {
    	    	    	  char tempf[FILENAME_SIZE];
			  sscanf(yytext+1, "%d \"%[^\"]", &hslineno, tempf); 
			  new_filename(tempf);
			  hsplineno = hslineno; hscolno = 0; hspcolno = 0;
			}

<Code,GlaExt>^"#line ".*{NL}    {
    	    	    	  char tempf[FILENAME_SIZE];
			  sscanf(yytext+5, "%d \"%[^\"]", &hslineno, tempf); 
			  new_filename(tempf); 
			  hsplineno = hslineno; hscolno = 0; hspcolno = 0;
			}

<Code,GlaExt>"{-# LINE ".*"-}"{NL} { 
    	    	    	  /* partain: pragma-style line directive */
			  char tempf[FILENAME_SIZE];
			  sscanf(yytext+9, "%d \"%[^\"]", &hslineno, tempf); 
			  new_filename(tempf);
			  hsplineno = hslineno; hscolno = 0; hspcolno = 0;
			}
<Code,GlaExt>"{-# GHC_PRAGMA INTERFACE VERSION "{D}+" #-}"   {
			  sscanf(yytext+33,"%d ",&thisIfacePragmaVersion);
			}
<Code,GlaExt>"{-# GHC_PRAGMA "   { 
    	    	    	  if ( ignorePragmas ||
			       thisIfacePragmaVersion < minAcceptablePragmaVersion || 
			       thisIfacePragmaVersion > maxAcceptablePragmaVersion) {
			     nested_comments = 1;
			     PUSH_STATE(Comment);
			  } else {
			     PUSH_STATE(GhcPragma);
			     RETURN(GHC_PRAGMA);
			  }
			}
<GhcPragma>"_N_"	    { RETURN(NO_PRAGMA); }
<GhcPragma>"_NI_"	    { RETURN(NOINFO_PRAGMA); }
<GhcPragma>"_ABSTRACT_"	    { RETURN(ABSTRACT_PRAGMA); }
<GhcPragma>"_DEFOREST_"	    { RETURN(DEFOREST_PRAGMA); }
<GhcPragma>"_SPECIALISE_"   { RETURN(SPECIALISE_PRAGMA); }
<GhcPragma>"_M_"	    { RETURN(MODNAME_PRAGMA); }
<GhcPragma>"_A_"	    { RETURN(ARITY_PRAGMA); }
<GhcPragma>"_U_"	    { RETURN(UPDATE_PRAGMA); }
<GhcPragma>"_S_"	    { RETURN(STRICTNESS_PRAGMA); }
<GhcPragma>"_K_"	    { RETURN(KIND_PRAGMA); }
<GhcPragma>"_MF_"	    { RETURN(MAGIC_UNFOLDING_PRAGMA); }
<GhcPragma>"_F_"	    { RETURN(UNFOLDING_PRAGMA); }

<GhcPragma>"_!_"	    { RETURN(COCON); }
<GhcPragma>"_#_"	    { RETURN(COPRIM); }
<GhcPragma>"_APP_"	    { RETURN(COAPP); }
<GhcPragma>"_TYAPP_"	    { RETURN(COTYAPP); }
<GhcPragma>"_ALG_"	    { RETURN(CO_ALG_ALTS); }
<GhcPragma>"_PRIM_"	    { RETURN(CO_PRIM_ALTS); }
<GhcPragma>"_NO_DEFLT_"	    { RETURN(CO_NO_DEFAULT); }
<GhcPragma>"_LETREC_"	    { RETURN(CO_LETREC); }

<GhcPragma>"_PRELUDE_DICTS_CC_" { RETURN(CO_PRELUDE_DICTS_CC); }
<GhcPragma>"_ALL_DICTS_CC_" { RETURN(CO_ALL_DICTS_CC); }
<GhcPragma>"_USER_CC_"	    { RETURN(CO_USER_CC); }
<GhcPragma>"_AUTO_CC_"	    { RETURN(CO_AUTO_CC); }
<GhcPragma>"_DICT_CC_"	    { RETURN(CO_DICT_CC); }

<GhcPragma>"_DUPD_CC_"	    { RETURN(CO_DUPD_CC); }
<GhcPragma>"_CAF_CC_"	    { RETURN(CO_CAF_CC); }

<GhcPragma>"_SDSEL_"	    { RETURN(CO_SDSEL_ID); }
<GhcPragma>"_METH_"	    { RETURN(CO_METH_ID); }
<GhcPragma>"_DEFM_"	    { RETURN(CO_DEFM_ID); }
<GhcPragma>"_DFUN_"	    { RETURN(CO_DFUN_ID); }
<GhcPragma>"_CONSTM_"	    { RETURN(CO_CONSTM_ID); }
<GhcPragma>"_SPEC_"	    { RETURN(CO_SPEC_ID); }
<GhcPragma>"_WRKR_"	    { RETURN(CO_WRKR_ID); }
<GhcPragma>"_ORIG_"	    { RETURN(CO_ORIG_NM); /* fully-qualified original name*/ }

<GhcPragma>"_ALWAYS_"	    { RETURN(UNFOLD_ALWAYS); }
<GhcPragma>"_IF_ARGS_"      { RETURN(UNFOLD_IF_ARGS); }

<GhcPragma>"_NOREP_I_"	    { RETURN(NOREP_INTEGER); }
<GhcPragma>"_NOREP_R_"	    { RETURN(NOREP_RATIONAL); }
<GhcPragma>"_NOREP_S_"	    { RETURN(NOREP_STRING); }

<GhcPragma>" #-}"	    { POP_STATE; RETURN(END_PRAGMA); }

<Code,GlaExt>"{-#"{WS}*"SPECIALI"[SZ]E {
			      PUSH_STATE(UserPragma);
			      RETURN(SPECIALISE_UPRAGMA);
			    }
<Code,GlaExt>"{-#"{WS}*"INLINE" {
			      PUSH_STATE(UserPragma);
			      RETURN(INLINE_UPRAGMA);
			    }
<Code,GlaExt>"{-#"{WS}*"MAGIC_UNFOLDING" {
			      PUSH_STATE(UserPragma);
			      RETURN(MAGIC_UNFOLDING_UPRAGMA);
			    }
<Code,GlaExt>"{-#"{WS}*"DEFOREST" {
                              PUSH_STATE(UserPragma);
                              RETURN(DEFOREST_UPRAGMA);
			    }
<Code,GlaExt>"{-#"{WS}*"ABSTRACT" {
			      PUSH_STATE(UserPragma);
			      RETURN(ABSTRACT_UPRAGMA);
			    }
<Code,GlaExt>"{-#"{WS}*[A-Z_]+ {
    	    	    	      fprintf(stderr, "Warning: \"%s\", line %d: Unrecognised pragma '",
    	    	    	        input_filename, hsplineno);
    	    	    	      format_string(stderr, (unsigned char *) yytext, yyleng);
    	    	    	      fputs("'\n", stderr);
			      nested_comments = 1;
			      PUSH_STATE(Comment);
			    }
<UserPragma>"#-}"	    { POP_STATE; RETURN(END_UPRAGMA); }

%{
    /*
     * Haskell keywords.  `scc' is actually a Glasgow extension, but it is
     * intentionally accepted as a keyword even for normal <Code>.
     */
%}

<Code,GlaExt,GhcPragma>"case" 	{ RETURN(CASE); }
<Code,GlaExt>"class"		{ RETURN(CLASS); }
<Code,GlaExt,UserPragma>"data"	{ RETURN(DATA); }
<Code,GlaExt>"default"  	{ RETURN(DEFAULT); }
<Code,GlaExt>"deriving" 	{ RETURN(DERIVING); }
<Code,GlaExt>"else"		{ RETURN(ELSE); }
<Code,GlaExt>"hiding"		{ RETURN(HIDING); }
<Code,GlaExt>"if"		{ RETURN(IF); }
<Code,GlaExt>"import"		{ RETURN(IMPORT); }
<Code,GlaExt>"infix"		{ RETURN(INFIX); }
<Code,GlaExt>"infixl"		{ RETURN(INFIXL); }
<Code,GlaExt>"infixr"		{ RETURN(INFIXR); }
<Code,GlaExt,UserPragma>"instance" { RETURN(INSTANCE); }
<Code,GlaExt>"interface"    	{ RETURN(INTERFACE); }
<Code,GlaExt>"module"		{ RETURN(MODULE); }
<Code,GlaExt,GhcPragma>"of"	{ RETURN(OF); }
<Code,GlaExt>"renaming"		{ RETURN(RENAMING); }
<Code,GlaExt>"then"		{ RETURN(THEN); }
<Code,GlaExt>"to"		{ RETURN(TO); }
<Code,GlaExt>"type"		{ RETURN(TYPE); }
<Code,GlaExt>"where"		{ RETURN(WHERE); }
<Code,GlaExt,GhcPragma>"in"	{ RETURN(IN); }
<Code,GlaExt,GhcPragma>"let"	{ RETURN(LET); }
<GlaExt,GhcPragma>"_ccall_"	{ RETURN(CCALL); }
<GlaExt,GhcPragma>"_ccall_GC_"	{ RETURN(CCALL_GC); }
<GlaExt,GhcPragma>"_casm_"	{ RETURN(CASM); }
<GlaExt,GhcPragma>"_casm_GC_"	{ RETURN(CASM_GC); }
<Code,GlaExt,GhcPragma>"_scc_"	{ RETURN(SCC); }
<GhcPragma>"_forall_"		{ RETURN(FORALL); }

%{
    /* 
     * Haskell operators.  Nothing special about these.
     */
%}

<Code,GlaExt>".."			{ RETURN(DOTDOT); }
<Code,GlaExt,GhcPragma>";"		{ RETURN(SEMI); }
<Code,GlaExt,GhcPragma,UserPragma>","	{ RETURN(COMMA); }
<Code,GlaExt,GhcPragma>"|"		{ RETURN(VBAR); }
<Code,GlaExt,GhcPragma,UserPragma>"="	{ RETURN(EQUAL); }
<Code,GlaExt>"<-"			{ RETURN(LARROW); }
<Code,GlaExt,GhcPragma,UserPragma>"->"	{ RETURN(RARROW); }
<Code,GlaExt,GhcPragma,UserPragma>"=>"	{ RETURN(DARROW); }
<Code,GlaExt,GhcPragma,UserPragma>"::"	{ RETURN(DCOLON); }
<Code,GlaExt,GhcPragma,UserPragma>"("	{ RETURN(OPAREN); }
<Code,GlaExt,GhcPragma,UserPragma>")"	{ RETURN(CPAREN); }
<Code,GlaExt,GhcPragma,UserPragma>"["	{ RETURN(OBRACK); }
<Code,GlaExt,GhcPragma,UserPragma>"]"	{ RETURN(CBRACK); }
<Code,GlaExt,GhcPragma>"{"		{ RETURN(OCURLY); }
<Code,GlaExt,GhcPragma>"}"		{ RETURN(CCURLY); }
<Code,GlaExt>"+"			{ RETURN(PLUS); }
<Code,GlaExt>"@"			{ RETURN(AT); }
<Code,GlaExt,GhcPragma>"\\"		{ RETURN(LAMBDA); }
<GhcPragma>"_/\\_"			{ RETURN(TYLAMBDA); }
<Code,GlaExt>"_"			{ RETURN(WILDCARD); }
<Code,GlaExt,GhcPragma>"`"		{ RETURN(BQUOTE); }
<Code,GlaExt>"~"    			{ RETURN(LAZY); }
<Code,GlaExt>"-"    			{ RETURN(MINUS); }

%{
    /*
     * Integers and (for Glasgow extensions) primitive integers.  Note that
     * we pass all of the text on to the parser, because flex/C can't handle
     * arbitrary precision numbers.
     */
%}

<GlaExt>("-")?"0o"{O}+"#" { /* octal */
			 yylval.uid = xstrndup(yytext, yyleng - 1);
			 RETURN(INTPRIM);
			}
<Code,GlaExt>"0o"{O}+	{ /* octal */
			 yylval.uid = xstrndup(yytext, yyleng);
			 RETURN(INTEGER);
			}
<GlaExt>("-")?"0x"{H}+"#" { /* hexadecimal */
			 yylval.uid = xstrndup(yytext, yyleng - 1);
			 RETURN(INTPRIM);
			}
<Code,GlaExt>"0x"{H}+	{ /* hexadecimal */
			 yylval.uid = xstrndup(yytext, yyleng);
			 RETURN(INTEGER);
			}
<GlaExt,GhcPragma>("-")?{N}"#"	{
			 yylval.uid = xstrndup(yytext, yyleng - 1);
			 RETURN(INTPRIM);
			}
<Code,GlaExt,GhcPragma>{N} {
			 yylval.uid = xstrndup(yytext, yyleng);
			 RETURN(INTEGER);
			}

%{
    /*
     * Floats and (for Glasgow extensions) primitive floats/doubles.
     */
%}

<GlaExt,GhcPragma>("-")?{F}"##" {
			 yylval.uid = xstrndup(yytext, yyleng - 2);
			 RETURN(DOUBLEPRIM);
			}
<GlaExt,GhcPragma>("-")?{F}"#" {
			 yylval.uid = xstrndup(yytext, yyleng - 1);
			 RETURN(FLOATPRIM);
			}
<Code,GlaExt>{F}        {
			 yylval.uid = xstrndup(yytext, yyleng);
			 RETURN(FLOAT);
			}

%{
    /*
     * Funky ``foo'' style C literals for Glasgow extensions
     */
%}

<GlaExt,GhcPragma>"``"[^']+"''"	{
			 hsnewid(yytext + 2, yyleng - 4);
			 RETURN(CLITLIT);
			}

%{
    /*
     * Identifiers, both variables and operators.  The trailing hash is allowed
     * for Glasgow extensions.
     */
%}

<GhcPragma>"_NIL_"		{ hsnewid(yytext, yyleng); RETURN(CONID); }
<GhcPragma>"_TUP_"{D}+		{ hsnewid(yytext, yyleng); RETURN(CONID); }
<GhcPragma>[a-z]{i}*"$"[a-z]{i}* { hsnewid(yytext, yyleng); RETURN(TYVAR_TEMPLATE_ID); }

%{
/* These SHOULDNAE work in "Code" (sigh) */
%}
<Code,GlaExt,GhcPragma,UserPragma>{Id}"#" { 
			 if (! (nonstandardFlag || in_interface)) {
			    char errbuf[ERR_BUF_SIZE];
			    sprintf(errbuf, "Non-standard identifier (trailing `#'): %s\n", yytext);
			    hsperror(errbuf);
			 }
    	    	    	 hsnewid(yytext, yyleng);
    	    	    	 RETURN(_isconstr(yytext) ? CONID : VARID);
			}
<Code,GlaExt,GhcPragma,UserPragma>_+{Id} { 
			 if (! (nonstandardFlag || in_interface)) {
			    char errbuf[ERR_BUF_SIZE];
			    sprintf(errbuf, "Non-standard identifier (leading underscore): %s\n", yytext);
			    hsperror(errbuf);
			 }
    	    	    	 hsnewid(yytext, yyleng);
    	    	    	 RETURN(isconstr(yytext) ? CONID : VARID);
			 /* NB: ^^^^^^^^ : not the macro! */
			}
<Code,GlaExt,GhcPragma,UserPragma>{Id}	{
    	    	         hsnewid(yytext, yyleng);
			 RETURN(_isconstr(yytext) ? CONID : VARID);
			}
<Code,GlaExt,GhcPragma,UserPragma>{SId}	{
    	    		 hsnewid(yytext, yyleng);
			 RETURN(_isconstr(yytext) ? CONSYM : VARSYM);
			}

%{
    /* Why is `{Id}#` matched this way, and `{Id}` lexed as three tokens? --JSM */

    /* Because we can make the former well-behaved (we defined them).

       Sadly, the latter is defined by Haskell, which allows such
       la-la land constructs as `{-a 900-line comment-} foo`.  (WDP 94/12)
    */
%}

<GlaExt,GhcPragma,UserPragma>"`"{Id}"#`"	{	
    	    	    	 hsnewid(yytext + 1, yyleng - 2);
			 RETURN(_isconstr(yytext+1) ? CONSYM : VARSYM);
			}

%{
    /*
     * Character literals.  The first form is the quick form, for character
     * literals that don't contain backslashes.  Literals with backslashes are
     * lexed through multiple rules.  First, we match the open ' and as many
     * normal characters as possible.  This puts us into the <Char> state, where
     * a backslash is legal.  Then, we match the backslash and move into the 
     * <CharEsc> state.  When we drop out of <CharEsc>, we collect more normal
     * characters and the close '.  We may end up with too many characters, but
     * this allows us to easily share the lex rules with strings.  Excess characters
     * are ignored with a warning.
     */
%}

<GlaExt,GhcPragma>'({CHAR}|"\"")"'#" {
    	    	    	 yylval.uhstring = installHstring(1, yytext+1);
    	    	    	 RETURN(CHARPRIM);
    	    	    	}
<Code,GlaExt>'({CHAR}|"\"")'	{
    	    	    	 yylval.uhstring = installHstring(1, yytext+1);
    	    	    	 RETURN(CHAR);
    	    	    	}
<Code,GlaExt>''		{char errbuf[ERR_BUF_SIZE];
			 sprintf(errbuf, "'' is not a valid character (or string) literal\n");
			 hsperror(errbuf);
			}
<Code,GlaExt,GhcPragma>'({CHAR}|"\"")* {
    	    	    	 hsmlcolno = hspcolno;
    	    	    	 cleartext();
    	    	    	 addtext(yytext+1, yyleng-1);
    	    	    	 PUSH_STATE(Char);
    	    	    	}
<Char>({CHAR}|"\"")*'#	{
    	    	    	 unsigned length;
    	    	    	 char *text;

    	    	    	 addtext(yytext, yyleng - 2);
    	    	    	 text = fetchtext(&length);

			 if (! (nonstandardFlag || in_interface)) {
			    char errbuf[ERR_BUF_SIZE];
			    sprintf(errbuf, "`Char-hash' literals are non-standard: %s\n", text);
			    hsperror(errbuf);
			 }

    	    	    	 if (length > 1) {
    	    	    	    fprintf(stderr, "\"%s\", line %d, column %d: Unboxed character literal '",
    	    	    	      input_filename, hsplineno, hspcolno + 1);
    	    	    	    format_string(stderr, (unsigned char *) text, length);
    	    	    	    fputs("' too long\n", stderr);
			    hsperror("");
    	    	    	 }
			 yylval.uhstring = installHstring(1, text);
    	    	    	 hspcolno = hsmlcolno;
    	    	    	 POP_STATE;
			 RETURN(CHARPRIM); 
			}
<Char>({CHAR}|"\"")*'	{
    	    	    	 unsigned length;
    	    	    	 char *text;

    	    	    	 addtext(yytext, yyleng - 1);
    	    	    	 text = fetchtext(&length);

    	    	    	 if (length > 1) {
    	    	    	    fprintf(stderr, "\"%s\", line %d, column %d: Character literal '",
    	    	    	      input_filename, hsplineno, hspcolno + 1);
    	    	    	    format_string(stderr, (unsigned char *) text, length);
    	    	    	    fputs("' too long\n", stderr);
			    hsperror("");
    	    	    	 }
			 yylval.uhstring = installHstring(1, text);
    	    	    	 hspcolno = hsmlcolno;
    	    	    	 POP_STATE;
			 RETURN(CHAR); 
			}
<Char>({CHAR}|"\"")+	{ addtext(yytext, yyleng); }


%{
    /*
     * String literals.  The first form is the quick form, for string literals
     * that don't contain backslashes.  Literals with backslashes are lexed
     * through multiple rules.  First, we match the open " and as many normal
     * characters as possible.  This puts us into the <String> state, where
     * a backslash is legal.  Then, we match the backslash and move into the 
     * <StringEsc> state.  When we drop out of <StringEsc>, we collect more normal
     * characters, moving back and forth between <String> and <StringEsc> as more
     * backslashes are encountered.  (We may even digress into <Comment> mode if we
     * find a comment in a gap between backslashes.)  Finally, we read the last chunk
     * of normal characters and the close ".
     */
%}

<GlaExt,GhcPragma>"\""({CHAR}|"'")*"\""#  {
			 yylval.uhstring = installHstring(yyleng-3, yytext+1);
			    /* the -3 accounts for the " on front, "# on the end */
			 RETURN(STRINGPRIM); 
    	    	    	}
<Code,GlaExt,GhcPragma>"\""({CHAR}|"'")*"\""  {
			 yylval.uhstring = installHstring(yyleng-2, yytext+1);
			 RETURN(STRING); 
    	    	    	}
<Code,GlaExt,GhcPragma>"\""({CHAR}|"'")* {
    	    	    	 hsmlcolno = hspcolno;
    	    	    	 cleartext();
    	    	    	 addtext(yytext+1, yyleng-1);
    	    	    	 PUSH_STATE(String);
    	    	    	}
<String>({CHAR}|"'")*"\"#"   {
    	    	    	 unsigned length;
    	    	    	 char *text;

    	    	    	 addtext(yytext, yyleng-2);
    	    	    	 text = fetchtext(&length);

			 if (! (nonstandardFlag || in_interface)) {
			    char errbuf[ERR_BUF_SIZE];
			    sprintf(errbuf, "`String-hash' literals are non-standard: %s\n", text);
			    hsperror(errbuf);
			 }

			 yylval.uhstring = installHstring(length, text);
    	    	    	 hspcolno = hsmlcolno;
    	    	    	 POP_STATE;
			 RETURN(STRINGPRIM);
			}
<String>({CHAR}|"'")*"\""   {
    	    	    	 unsigned length;
    	    	    	 char *text;

    	    	    	 addtext(yytext, yyleng-1);
    	    	    	 text = fetchtext(&length);

			 yylval.uhstring = installHstring(length, text);
    	    	    	 hspcolno = hsmlcolno;
    	    	    	 POP_STATE;
			 RETURN(STRING); 
			}
<String>({CHAR}|"'")+   { addtext(yytext, yyleng); }

%{
    /*
     * Character and string escapes are roughly the same, but strings have the
     * extra `\&' sequence which is not allowed for characters.  Also, comments
     * are allowed in the <StringEsc> state.  (See the comment section much
     * further down.)
     *
     * NB: Backslashes and tabs are stored in strings as themselves.
     * But if we print them (in printtree.c), they must go out as
     * "\\\\" and "\\t" respectively.  (This is because of the bogus
     * intermediate format that the parser produces.  It uses '\t' fpr end of
     * string, so it needs to be able to escape tabs, which means that it
     * also needs to be able to escape the escape character ('\\').  Sigh.
     */
%}

<Char>\\    	    	{ PUSH_STATE(CharEsc); }
<String>\\&	    	/* Ignore */ ;
<String>\\    	    	{ PUSH_STATE(StringEsc); noGap = TRUE; }

<CharEsc>\\    	    	{ addchar(*yytext); POP_STATE; }
<StringEsc>\\	    	{ if (noGap) { addchar(*yytext); } POP_STATE; }

<CharEsc,StringEsc>["']	{ addchar(*yytext); POP_STATE; }
<CharEsc,StringEsc>NUL 	{ addchar('\000'); POP_STATE; }
<CharEsc,StringEsc>SOH 	{ addchar('\001'); POP_STATE; }
<CharEsc,StringEsc>STX 	{ addchar('\002'); POP_STATE; }
<CharEsc,StringEsc>ETX 	{ addchar('\003'); POP_STATE; }
<CharEsc,StringEsc>EOT  { addchar('\004'); POP_STATE; }
<CharEsc,StringEsc>ENQ	{ addchar('\005'); POP_STATE; }
<CharEsc,StringEsc>ACK	{ addchar('\006'); POP_STATE; }
<CharEsc,StringEsc>BEL 	|
<CharEsc,StringEsc>a	{ addchar('\007'); POP_STATE; }
<CharEsc,StringEsc>BS 	|
<CharEsc,StringEsc>b	{ addchar('\010'); POP_STATE; }
<CharEsc,StringEsc>HT 	|
<CharEsc,StringEsc>t 	{ addchar('\011'); POP_STATE; }
<CharEsc,StringEsc>LF 	|
<CharEsc,StringEsc>n	{ addchar('\012'); POP_STATE; }
<CharEsc,StringEsc>VT 	|
<CharEsc,StringEsc>v	{ addchar('\013'); POP_STATE; }
<CharEsc,StringEsc>FF 	|
<CharEsc,StringEsc>f	{ addchar('\014'); POP_STATE; }
<CharEsc,StringEsc>CR 	|
<CharEsc,StringEsc>r	{ addchar('\015'); POP_STATE; }
<CharEsc,StringEsc>SO	{ addchar('\016'); POP_STATE; }
<CharEsc,StringEsc>SI	{ addchar('\017'); POP_STATE; }
<CharEsc,StringEsc>DLE	{ addchar('\020'); POP_STATE; }
<CharEsc,StringEsc>DC1	{ addchar('\021'); POP_STATE; }
<CharEsc,StringEsc>DC2	{ addchar('\022'); POP_STATE; }
<CharEsc,StringEsc>DC3	{ addchar('\023'); POP_STATE; }
<CharEsc,StringEsc>DC4	{ addchar('\024'); POP_STATE; }
<CharEsc,StringEsc>NAK	{ addchar('\025'); POP_STATE; }
<CharEsc,StringEsc>SYN	{ addchar('\026'); POP_STATE; }
<CharEsc,StringEsc>ETB	{ addchar('\027'); POP_STATE; }
<CharEsc,StringEsc>CAN	{ addchar('\030'); POP_STATE; }
<CharEsc,StringEsc>EM	{ addchar('\031'); POP_STATE; }
<CharEsc,StringEsc>SUB	{ addchar('\032'); POP_STATE; }
<CharEsc,StringEsc>ESC	{ addchar('\033'); POP_STATE; }
<CharEsc,StringEsc>FS	{ addchar('\034'); POP_STATE; }
<CharEsc,StringEsc>GS	{ addchar('\035'); POP_STATE; }
<CharEsc,StringEsc>RS	{ addchar('\036'); POP_STATE; }
<CharEsc,StringEsc>US	{ addchar('\037'); POP_STATE; }
<CharEsc,StringEsc>SP	{ addchar('\040'); POP_STATE; }
<CharEsc,StringEsc>DEL	{ addchar('\177'); POP_STATE; }
<CharEsc,StringEsc>"^"{CNTRL} { char c = yytext[1] - '@'; addchar(c); POP_STATE; }
<CharEsc,StringEsc>{D}+	 {
    	    	    	  int i = strtol(yytext, NULL, 10);
			  if (i < NCHARS) {
			     addchar((char) i);
			  } else {
			     char errbuf[ERR_BUF_SIZE];
			     sprintf(errbuf, "Numeric escape \"\\%s\" out of range\n", 
				yytext);
			     hsperror(errbuf);
			  }
    	    	    	  POP_STATE;
			}
<CharEsc,StringEsc>o{O}+ {
    	    	   	  int i = strtol(yytext + 1, NULL, 8);
			  if (i < NCHARS) {
			     addchar((char) i);
			  } else {
			     char errbuf[ERR_BUF_SIZE];
			     sprintf(errbuf, "Numeric escape \"\\%s\" out of range\n", 
				yytext);
			     hsperror(errbuf);
			  }
    	    	    	  POP_STATE;
			}
<CharEsc,StringEsc>x{H}+ {
    	    	    	  int i = strtol(yytext + 1, NULL, 16);
			  if (i < NCHARS) {
			     addchar((char) i);
			  } else {
			     char errbuf[ERR_BUF_SIZE];
			     sprintf(errbuf, "Numeric escape \"\\%s\" out of range\n", 
				yytext);
			     hsperror(errbuf);
			  }
    	    	    	  POP_STATE;
			}

%{
    /*
     * Simple comments and whitespace.  Normally, we would just ignore these, but
     * in case we're processing a string escape, we need to note that we've seen
     * a gap.
     *
     * Note that we cater for a comment line that *doesn't* end in a newline.
     * This is incorrect, strictly speaking, but seems like the right thing
     * to do.  Reported by Rajiv Mirani.  (WDP 95/08)
     */
%}

<Code,GlaExt,StringEsc>"--".*{NL}?{WS}* |
<Code,GlaExt,GhcPragma,UserPragma,StringEsc>{WS}+	{ noGap = FALSE; }

%{
    /*
     * Nested comments.  The major complication here is in trying to match the
     * longest lexemes possible, for better performance.  (See the flex document.)
     * That's why the rules look so bizarre.
     */
%}

<Code,GlaExt,GhcPragma,UserPragma,StringEsc>"{-"	{ 
    	    	    	  noGap = FALSE; nested_comments = 1; PUSH_STATE(Comment); 
    	    	    	}

<Comment>[^-{]*     	|
<Comment>"-"+[^-{}]+ 	|
<Comment>"{"+[^-{}]+	;
<Comment>"{-"		{ nested_comments++; }
<Comment>"-}"	    	{ if (--nested_comments == 0) POP_STATE; }
<Comment>(.|\n)	    	;

%{
    /*
     * Illegal characters.  This used to be a single rule, but we might as well
     * pass on as much information as we have, so now we indicate our state in
     * the error message.
     */
%}

<INITIAL,Code,GlaExt,GhcPragma,UserPragma>(.|\n)	{ 
    	    	    	 fprintf(stderr, "\"%s\", line %d, column %d: Illegal character: `", 
    	    	    	    input_filename, hsplineno, hspcolno + 1); 
    	    	    	 format_string(stderr, (unsigned char *) yytext, 1);
    	    	    	 fputs("'\n", stderr);
			 hsperror("");
			}
<Char>(.|\n)		{ 
    	    	    	 fprintf(stderr, "\"%s\", line %d, column %d: Illegal character: `",
    	    	    	    input_filename, hsplineno, hspcolno + 1); 
    	    	    	 format_string(stderr, (unsigned char *) yytext, 1);
    	    	    	 fputs("' in a character literal\n", stderr);
			 hsperror("");
			}
<CharEsc>(.|\n)		{
    	    	    	 fprintf(stderr, "\"%s\", line %d, column %d: Illegal character escape: `\\",
    	    	    	    input_filename, hsplineno, hspcolno + 1); 
    	    	    	 format_string(stderr, (unsigned char *) yytext, 1);
    	    	    	 fputs("'\n", stderr);
			 hsperror("");
    	    	    	}
<String>(.|\n)		{ if (nonstandardFlag) {
                             addtext(yytext, yyleng);
                          } else { 
                                fprintf(stderr, "\"%s\", line %d, column %d: Illegal character: `", 
                                input_filename, hsplineno, hspcolno + 1); 
                                format_string(stderr, (unsigned char *) yytext, 1);
                                fputs("' in a string literal\n", stderr);
                                hsperror("");
			  }
			}
<StringEsc>(.|\n)	{
    	    	    	 if (noGap) {
    	    	    	     fprintf(stderr, "\"%s\", line %d, column %d: Illegal string escape: `\\", 
    	    	    	    	input_filename, hsplineno, hspcolno + 1); 
    	    	    	     format_string(stderr, (unsigned char *) yytext, 1);
    	    	    	     fputs("'\n", stderr);
			     hsperror("");
    	    	    	 } else {
    	    	    	     fprintf(stderr, "\"%s\", line %d, column %d: Illegal character: `",
    	    	    	    	input_filename, hsplineno, hspcolno + 1);
    	    	    	     format_string(stderr, (unsigned char *) yytext, 1);
    	    	    	     fputs("' in a string gap\n", stderr);
			     hsperror("");
    	    	    	 }
    	    	    	}

%{
    /*
     * End of file.  In any sub-state, this is an error.  However, for the primary
     * <Code> and <GlaExt> states, this is perfectly normal.  We just return an EOF
     * and let the yylex() wrapper deal with whatever has to be done next (e.g.
     * adding virtual close curlies, or closing an interface and returning to the
     * primary source file.
     *
     * Note that flex does not call YY_USER_ACTION for <<EOF>> rules.  Hence the
     * line/column advancement has to be done by hand.
     */
%}

<Char,CharEsc><<EOF>>  	{ 
    	    	    	  hsplineno = hslineno; hspcolno = hscolno;
    	    	    	  hsperror("unterminated character literal");
    	    	    	}
<Comment><<EOF>>    	{ 
    	    	    	  hsplineno = hslineno; hspcolno = hscolno;
    	    	    	  hsperror("unterminated comment"); 
    	    	    	}
<String,StringEsc><<EOF>>   { 
    	    	    	  hsplineno = hslineno; hspcolno = hscolno;
    	    	    	  hsperror("unterminated string literal"); 
    	    	    	}
<GhcPragma><<EOF>>  	{
    	    	    	  hsplineno = hslineno; hspcolno = hscolno;
    	    	    	  hsperror("unterminated interface pragma"); 
			}
<UserPragma><<EOF>>  	{
    	    	    	  hsplineno = hslineno; hspcolno = hscolno;
    	    	    	  hsperror("unterminated user-specified pragma"); 
			}
<Code,GlaExt><<EOF>>   	{ hsplineno = hslineno; hspcolno = hscolno; return(EOF); }

%%

/**********************************************************************
*                                                                     *
*                                                                     *
*     YACC/LEX Initialisation etc.                                    *
*                                                                     *
*                                                                     *
**********************************************************************/

/*
   We initialise input_filename to "<stdin>".
   This allows unnamed sources to be piped into the parser.
*/

extern BOOLEAN acceptPrim;

void
yyinit(void)
{
    input_filename = xstrdup("<stdin>");

    /* We must initialize the input buffer _now_, because we call
       setyyin _before_ calling yylex for the first time! */
    yy_switch_to_buffer(yy_create_buffer(stdin, YY_BUF_SIZE));

    if (acceptPrim)
	PUSH_STATE(GlaExt);
    else
	PUSH_STATE(Code);
}

static void
new_filename(char *f) /* This looks pretty dodgy to me (WDP) */
{
    if (input_filename != NULL)
	free(input_filename);
    input_filename = xstrdup(f);
}

/**********************************************************************
*                                                                     *
*                                                                     *
*     Layout Processing                                               *
*                                                                     *
*                                                                     *
**********************************************************************/

/*
	The following section deals with Haskell Layout conventions
	forcing insertion of ; or } as appropriate
*/

static BOOLEAN
hsshouldindent(void)
{
    return (!forgetindent && INDENTON);
}


/* Enter new context and set new indentation level */
void
hssetindent(void)
{
#ifdef HSP_DEBUG
    fprintf(stderr, "hssetindent:hscolno=%d,hspcolno=%d,INDENTPT[%d]=%d\n", hscolno, hspcolno, icontexts, INDENTPT);
#endif

    /*
     * partain: first chk that new indent won't be less than current one; this code
     * doesn't make sense to me; hscolno tells the position of the _end_ of the
     * current token; what that has to do with indenting, I don't know.
     */


    if (hscolno - 1 <= INDENTPT) {
	if (INDENTPT == -1)
	    return;		/* Empty input OK for Haskell 1.1 */
	else {
	    char errbuf[ERR_BUF_SIZE];

	    sprintf(errbuf, "Layout error -- indentation should be > %d cols", INDENTPT);
	    hsperror(errbuf);
	}
    }
    hsentercontext((hspcolno << 1) | 1);
}


/* Enter a new context without changing the indentation level */
void
hsincindent(void)
{
#ifdef HSP_DEBUG
    fprintf(stderr, "hsincindent:hscolno=%d,hspcolno=%d,INDENTPT[%d]=%d\n", hscolno, hspcolno, icontexts, INDENTPT);
#endif
    hsentercontext(indenttab[icontexts] & ~1);
}


/* Turn off indentation processing, usually because an explicit "{" has been seen */
void
hsindentoff(void)
{
    forgetindent = TRUE;
}


/* Enter a new layout context. */
static void
hsentercontext(int indent)
{
    /* Enter new context and set indentation as specified */
    if (++icontexts >= MAX_CONTEXTS) {
	char errbuf[ERR_BUF_SIZE];

	sprintf(errbuf, "`wheres' and `cases' nested too deeply (>%d)", MAX_CONTEXTS - 1);
	hsperror(errbuf);
    }
    forgetindent = FALSE;
    indenttab[icontexts] = indent;
#ifdef HSP_DEBUG
    fprintf(stderr, "hsentercontext:indent=%d,hscolno=%d,hspcolno=%d,INDENTPT[%d]=%d\n", indent, hscolno, hspcolno, icontexts, INDENTPT);
#endif
}


/* Exit a layout context */
void
hsendindent(void)
{
    --icontexts;
#ifdef HSP_DEBUG
    fprintf(stderr, "hsendindent:hscolno=%d,hspcolno=%d,INDENTPT[%d]=%d\n", hscolno, hspcolno, icontexts, INDENTPT);
#endif
}

/*
 * 	Return checks the indentation level and returns ;, } or the specified token.
 */

static int
Return(int tok)
{
#ifdef HSP_DEBUG
    extern int yyleng;
#endif

    if (hsshouldindent()) {
	if (hspcolno < INDENTPT) {
#ifdef HSP_DEBUG
	    fprintf(stderr, "inserted '}' before %d (%d:%d:%d:%d)\n", tok, hspcolno, hscolno, yyleng, INDENTPT);
#endif
	    hssttok = tok;
	    return (VCCURLY);
	} else if (hspcolno == INDENTPT) {
#ifdef HSP_DEBUG
	    fprintf(stderr, "inserted ';' before %d (%d:%d)\n", tok, hspcolno, INDENTPT);
#endif
	    hssttok = -tok;
	    return (SEMI);
	}
    }
    hssttok = -1;
#ifdef HSP_DEBUG
    fprintf(stderr, "returning %d (%d:%d)\n", tok, hspcolno, INDENTPT);
#endif
    return (tok);
}


/*
 *	Redefine yylex to check for stacked tokens, yylex1() is the original yylex()
 */
int
yylex()
{
    int tok;
    static BOOLEAN eof = FALSE;

    if (!eof) {
	if (hssttok != -1) {
	    if (hssttok < 0) {
		tok = -hssttok;
		hssttok = -1;
		return tok;
	    }
	    RETURN(hssttok);
	} else {
    	    endlineno = hslineno;
	    if ((tok = yylex1()) != EOF)
		return tok;
	    else
		eof = TRUE;
	}
    }
    if (icontexts > icontexts_save) {
	if (INDENTON) {
	    eof = TRUE;
	    indenttab[icontexts] = 0;
	    return (VCCURLY);
	} else
	    hsperror("missing '}' at end of file");
    } else if (hsbuf_save != NULL) {
	fclose(yyin);
	yy_delete_buffer(YY_CURRENT_BUFFER);
	yy_switch_to_buffer(hsbuf_save);
	hsbuf_save = NULL;
	new_filename(filename_save);
    	free(filename_save);
	hslineno = hslineno_save;
	hsplineno = hsplineno_save;
	hscolno = hscolno_save;
	hspcolno = hspcolno_save;
	etags = etags_save;
	in_interface = FALSE;
	icontexts = icontexts_save - 1;
	icontexts_save = 0;
#ifdef HSP_DEBUG
	fprintf(stderr, "finished reading interface (%d:%d:%d)\n", hscolno, hspcolno, INDENTPT);
#endif
	eof = FALSE;
	RETURN(LEOF);
    } else {
	yyterminate();
    }
    abort(); /* should never get here! */
    return(0);
}

/**********************************************************************
*                                                                     *
*                                                                     *
*     Input Processing for Interfaces                                 *
*                                                                     *
*                                                                     *
**********************************************************************/

/* setyyin(file)	open file as new lex input buffer */
extern FILE *yyin;

void
setyyin(char *file)
{
    hsbuf_save = YY_CURRENT_BUFFER;
    if ((yyin = fopen(file, "r")) == NULL) {
	char errbuf[ERR_BUF_SIZE];

	sprintf(errbuf, "can't read \"%-.50s\"", file);
	hsperror(errbuf);
    }
    yy_switch_to_buffer(yy_create_buffer(yyin, YY_BUF_SIZE));

    hslineno_save = hslineno;
    hsplineno_save = hsplineno;
    hslineno = hsplineno = 1;

    filename_save = input_filename;
    input_filename = NULL;
    new_filename(file);
    hscolno_save = hscolno;
    hspcolno_save = hspcolno;
    hscolno = hspcolno = 0;
    in_interface = TRUE;
    etags_save = etags; /* do not do "etags" stuff in interfaces */
    etags = 0;		/* We remember whether we are doing it in
			   the module, so we can restore it later [WDP 94/09] */
    hsentercontext(-1);		/* partain: changed this from 0 */
    icontexts_save = icontexts;
#ifdef HSP_DEBUG
    fprintf(stderr, "reading %s (%d:%d:%d)\n", input_filename, hscolno_save, hspcolno_save, INDENTPT);
#endif
}

static void
layout_input(char *text, int len)
{
#ifdef HSP_DEBUG
    fprintf(stderr, "Scanning \"%s\"\n", text);
#endif

    hsplineno = hslineno;
    hspcolno = hscolno;

    while (len-- > 0) {
	switch (*text++) {
	case '\n':
	case '\r':
	case '\f':
	    hslineno++;
	    hscolno = 0;
	    break;
	case '\t':
	    hscolno += 8 - (hscolno % 8);	/* Tabs stops are 8 columns apart */
	    break;
	case '\v':
	    break;
	default:
	    ++hscolno;
	    break;
	}
    }
}

void
setstartlineno(void)
{
    startlineno = hsplineno;
#if 1/*etags*/
#else
    if (etags)
	fprintf(stderr,"%u\tsetstartlineno (col %u)\n",startlineno,hscolno);
#endif
}

/**********************************************************************
*                                                                     *
*                                                                     *
*                      Text Caching                                   *
*                                                                     *
*                                                                     *
**********************************************************************/

#define CACHE_SIZE YY_BUF_SIZE

static struct {
    unsigned allocated;
    unsigned next;
    char *text;
} textcache = { 0, 0, NULL };

static void
cleartext(void)
{
/*  fprintf(stderr, "cleartext\n"); */
    textcache.next = 0;
    if (textcache.allocated == 0) {
	textcache.allocated = CACHE_SIZE;
	textcache.text = xmalloc(CACHE_SIZE);
    }
}

static void
addtext(char *text, unsigned length)
{
/*  fprintf(stderr, "addtext: %d %s\n", length, text); */

    if (length == 0)
	return;

    if (textcache.next + length + 1 >= textcache.allocated) {
	textcache.allocated += length + CACHE_SIZE;
	textcache.text = xrealloc(textcache.text, textcache.allocated);
    }
    bcopy(text, textcache.text + textcache.next, length);
    textcache.next += length;
}

static void
addchar(char c)
{
/*  fprintf(stderr, "addchar: %c\n", c); */

    if (textcache.next + 2 >= textcache.allocated) {
	textcache.allocated += CACHE_SIZE;
	textcache.text = xrealloc(textcache.text, textcache.allocated);
    }
    textcache.text[textcache.next++] = c;
}

static char *
fetchtext(unsigned *length)
{
/*  fprintf(stderr, "fetchtext: %d\n", textcache.next); */

    *length = textcache.next;
    textcache.text[textcache.next] = '\0';
    return textcache.text;
}

/**********************************************************************
*                                                                     *
*                                                                     *
*    Identifier Processing                                             *
*                                                                     *
*                                                                     *
**********************************************************************/

/*
	hsnewid		Enters an id of length n into the symbol table.
*/

static void
hsnewid(char *name, int length)
{
    char save = name[length];

    name[length] = '\0';
    yylval.uid = installid(name);
    name[length] = save;
}

BOOLEAN 
isconstr(char *s) /* walks past leading underscores before using the macro */
{
    char *temp = s;

    for ( ; temp != NULL && *temp == '_' ; temp++ );

    return _isconstr(temp);
}
