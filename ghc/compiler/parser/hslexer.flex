%{
/**********************************************************************
*                                                                     *
*                                                                     *
*       FLEX for Haskell.                                             *
*       -----------------                                             *
*                                                                     *
**********************************************************************/

/* The includes/config.h one */
#include "config.h"

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

static unsigned char CharTable[NCHARS] = {
/* nul */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/* bs  */    	0,  	_S,  	_S,  	_S,  	_S,  	0,  	0,  	0,
/* dle */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/* can */    	0,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/* sp  */    	_S,  	0,  	0,  	0,  	0,  	0,  	0,  	0,
/* '(' */       _C, 	0,  	0,  	0,  	0,  	0,  	0,  	0,
/* '0' */	_D|_H|_O,_D|_H|_O,_D|_H|_O,_D|_H|_O,_D|_H|_O,_D|_H|_O,_D|_H|_O,_D|_H|_O,
/* '8' */    	_D|_H,	_D|_H,	_C,  	0,  	0,  	0,  	0,  	0,
/* '@' */    	0,  	_H|_C,	_H|_C,	_H|_C,	_H|_C,	_H|_C,	_H|_C,	_C,
/* 'H' */    	_C,  	_C,  	_C,  	_C,  	_C,  	_C,  	_C,  	_C,
/* 'P' */    	_C,  	_C,  	_C,  	_C,  	_C,  	_C,  	_C,  	_C,
/* 'X' */    	_C,  	_C,  	_C,     _C,	0,  	0,  	0,  	0,
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

BOOLEAN
isconstr (char *s)
{
    return(CharTable[*s]&(_C));
}

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

int modulelineno = -1;		/* The line number where the module starts */
int startlineno = 0;		/* The line number where something starts */
int endlineno = 0;		/* The line number where something ends */

static BOOLEAN noGap = TRUE;	/* For checking string gaps */
static BOOLEAN forgetindent = FALSE;	/* Don't bother applying indentation rules */

static int nested_comments; 	/* For counting comment nesting depth */
static int comment_start;

/* OLD: Hacky definition of yywrap: see flex doc.

   If we don't do this, then we'll have to get the default
   yywrap from the flex library, which is often something
   we are not good at locating.  This avoids that difficulty.
   (Besides which, this is the way old flexes (pre 2.4.x) did it.)
   WDP 94/09/05
#define yywrap() 1
*/

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

static BOOLEAN is_commment PROTO((char*, int));

/* Special file handling for IMPORTS */
/*  Note: imports only ever go *one deep* (hence no need for a stack) WDP 94/09 */

static YY_BUFFER_STATE hsbuf_save = NULL;	/* Saved input buffer	 */
static char *filename_save;	    	/* File Name            	 */
static int hslineno_save = 0,	    	/* Line Number          	 */
 hsplineno_save = 0,	    		/* Line Number of Prev. token	 */
 hscolno_save = 0,		    	/* Indentation          	 */
 hspcolno_save = 0;		    	/* Left Indentation	 	 */
static short icontexts_save = 0;    	/* Indent Context Level 	 */

static BOOLEAN etags_save;              /* saved: whether doing etags stuff or not */
extern BOOLEAN etags;	                /* that which is saved */

extern BOOLEAN nonstandardFlag;	        /* Glasgow extensions allowed */

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
*/
#define PUSH_STATE(n)   yy_push_state(n)
#define POP_STATE       yy_pop_state()

%}
/* Options:
    8bit (8-bit input)
    noyywrap (do not call yywrap on end of file; avoid use of -lfl)
    never-interactive (to go a bit faster)
    stack (use a start-condition stack)
*/
%option 8bit
%option noyywrap
%option never-interactive
%option stack

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

%x Char CharEsc Code Comment GlaExt UserPragma String StringEsc

isoS			[\xa1-\xbf\xd7\xf7]
isoL			[\xc0-\xd6\xd8-\xde]
isol			[\xdf-\xf6\xf8-\xff]
isoA			[\xa1-\xff]

D			[0-9]
O			[0-7]
H			[0-9A-Fa-f]
N			{D}+
F   	    	    	{N}"."{N}(("e"|"E")("+"|"-")?{N})?
S			[!#$%&*+./<=>?@\\^|\-~:\xa1-\xbf\xd7\xf7]
SId			{S}{S}*
L			[A-Z\xc0-\xd6\xd8-\xde]
l			[a-z_\xdf-\xf6\xf8-\xff]
I			{L}|{l}
i			{L}|{l}|[0-9'_]
Id			{I}{i}*
Mod			{L}{i}*
CHAR			[ !#$%&()*+,\-./0-9:;<=>?@A-Z\[\]^_`a-z{|}~\xa1-\xff]
CNTRL	    	    	[@A-Z\[\\\]^_]
WS			[ \t\n\r\f\v]
NL  	    	    	[\n\r]

%%

%{
    /*
     * Simple comments and whitespace.  Normally, we would just ignore these, but
     * in case we're processing a string escape, we need to note that we've seen
     * a gap.
     *
     * Note that we cater for a comment line that *doesn't* end in a newline.
     * This is incorrect, strictly speaking, but seems like the right thing
     * to do.  Reported by Rajiv Mirani.  (WDP 95/08)
     *
     * Hackily moved up here so that --<<EOF>> will match     -- SOF 5/97
     */
%}

<Code,GlaExt,UserPragma,StringEsc>{WS}+	{ noGap = FALSE; }

%{
    /* 
     * Special GHC pragma rules.  Do we need a start state for interface files,
     * so these won't be matched in source files? --JSM
     */

%}

%{
/* I believe the next rule is not ever matched.

   The '#line ' rule is un-cool, recognising a cpp directive inside hs source.
   Driver has now been modified to output `standard' {-# LINE ..-} pragmas
   where possible, so the lexer should now never see cpp directives
   like '# ' and '#line'.

   -- SOF

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
*/
%}

<Code,GlaExt>"{-# LINE ".*"-}"{NL} { 
    	    	    	  /* partain: pragma-style line directive */
			  char tempf[FILENAME_SIZE];
			  sscanf(yytext+9, "%d \"%[^\"]", &hslineno, tempf); 
			  new_filename(tempf);
			  hsplineno = hslineno; hscolno = 0; hspcolno = 0;
			}

<Code,GlaExt>"{-#"{WS}*"INTERFACE" {
			      PUSH_STATE(UserPragma);
			      RETURN(INTERFACE_UPRAGMA);
			    }
<Code,GlaExt>"{-#"{WS}*"SPECIALI"[SZ]E {
			      PUSH_STATE(UserPragma);
			      RETURN(SPECIALISE_UPRAGMA);
			    }
<Code,GlaExt>"{-#"{WS}*"INLINE" {
			      PUSH_STATE(UserPragma);
			      RETURN(INLINE_UPRAGMA);
			    }
<Code,GlaExt>"{-#"{WS}*"NOINLINE" {
			      PUSH_STATE(UserPragma);
			      RETURN(NOINLINE_UPRAGMA);
			    }
<Code,GlaExt>"{-#"{WS}*"MAGIC_UNFOLDING" {
			      PUSH_STATE(UserPragma);
			      RETURN(MAGIC_UNFOLDING_UPRAGMA);
			    }
<Code,GlaExt>"{-#"{WS}*"GENERATE_SPECS" {
			      /* these are handled by hscpp */
			      nested_comments =1; comment_start = hsplineno;
                              PUSH_STATE(Comment);
			    }
<Code,GlaExt>"{-#"{WS}*"OPTIONS" {
			      /* these are for the driver! */
			      nested_comments =1; comment_start = hsplineno;
                              PUSH_STATE(Comment);
			    }
<Code,GlaExt>"{-#"{WS}*"SOURCE"{WS}*"#"?"-}" {
			      /* these are used by `make depend' and the
			         compiler to indicate that a module should
				 be imported from source */
			      nested_comments =1; comment_start = hsplineno; 
                              RETURN(SOURCE_UPRAGMA); 
			    }

<Code,GlaExt>"{-#"{WS}*[A-Z_]+ {
    	    	    	      fprintf(stderr, "%s:%d: Warning: Unrecognised pragma '",
    	    	    	        input_filename, hsplineno);
    	    	    	      format_string(stderr, (unsigned char *) yytext, yyleng);
    	    	    	      fputs("'\n", stderr);
			      nested_comments = 1; comment_start = hsplineno;
			      PUSH_STATE(Comment);
			    }
<UserPragma>"#-}"	    { POP_STATE; RETURN(END_UPRAGMA); }

%{
    /*
     * Haskell keywords.  `scc' is actually a Glasgow extension, but it is
     * intentionally accepted as a keyword even for normal <Code>.
     */
%}

<Code,GlaExt>"case" 		{ RETURN(CASE); }
<Code,GlaExt>"class"		{ RETURN(CLASS); }
<Code,GlaExt,UserPragma>"data"	{ RETURN(DATA); }
<Code,GlaExt>"default"  	{ RETURN(DEFAULT); }
<Code,GlaExt>"deriving" 	{ RETURN(DERIVING); }
<Code,GlaExt>"do" 		{ RETURN(DO); }
<Code,GlaExt>"else"		{ RETURN(ELSE); }
<Code,GlaExt>"if"		{ RETURN(IF); }
<Code,GlaExt>"import"		{ RETURN(IMPORT); }
<Code,GlaExt>"in"		{ RETURN(IN); }
<Code,GlaExt>"infix"		{ RETURN(INFIX); }
<Code,GlaExt>"infixl"		{ RETURN(INFIXL); }
<Code,GlaExt>"infixr"		{ RETURN(INFIXR); }
<Code,GlaExt,UserPragma>"instance" { RETURN(INSTANCE); }
<Code,GlaExt>"let"		{ RETURN(LET); }
<Code,GlaExt>"module"		{ RETURN(MODULE); }
<Code,GlaExt>"newtype" 		{ RETURN(NEWTYPE); }
<Code,GlaExt>"of"		{ RETURN(OF); }
<Code,GlaExt>"then"		{ RETURN(THEN); }
<Code,GlaExt>"type"		{ RETURN(TYPE); }
<Code,GlaExt>"where"		{ RETURN(WHERE); }

<Code,GlaExt>"as"		{ RETURN(AS); }
<Code,GlaExt>"hiding"		{ RETURN(HIDING); }
<Code,GlaExt>"qualified"	{ RETURN(QUALIFIED); }

<Code,GlaExt>"forall"		{ RETURN(FORALL); }

<Code,GlaExt>"_scc_"		{ RETURN(SCC); }
<GlaExt>"_ccall_"		{ RETURN(CCALL); }
<GlaExt>"_ccall_GC_"		{ RETURN(CCALL_GC); }
<GlaExt>"_casm_"		{ RETURN(CASM); }
<GlaExt>"_casm_GC_"		{ RETURN(CASM_GC); }
<GlaExt>"(#"			{ RETURN(OUNBOXPAREN); }
<GlaExt>"#)"			{ RETURN(CUNBOXPAREN); }
<GlaExt>"foreign"		{ RETURN(FOREIGN); }
<GlaExt>"export"		{ RETURN(EXPORT); }
<GlaExt>"label"			{ RETURN(LABEL); }
<GlaExt>"unsafe"		{ RETURN(UNSAFE); }
<GlaExt>"_stdcall"		{ RETURN(STDCALL); }
<GlaExt>"_ccall"		{ RETURN(C_CALL); }
<GlaExt>"_pascal"	        { RETURN(PASCAL); }
<GlaExt>"stdcall"		{ RETURN(STDCALL); }
<GlaExt>"ccall"			{ RETURN(C_CALL); }
<GlaExt>"pascal"	        { RETURN(PASCAL); }
<GlaExt>"dynamic"	        { RETURN(DYNAMIC); }

%{
    /* 
     * Haskell operators: special, reservedops and useful varsyms
     */
%}

<Code,GlaExt,UserPragma>"("	{ RETURN(OPAREN); }
<Code,GlaExt,UserPragma>")"	{ RETURN(CPAREN); }
<Code,GlaExt,UserPragma>"["	{ RETURN(OBRACK); }
<Code,GlaExt,UserPragma>"]"	{ RETURN(CBRACK); }
<Code,GlaExt>"{"		{ RETURN(OCURLY); }
<Code,GlaExt>"}"		{ RETURN(CCURLY); }
<Code,GlaExt,UserPragma>","	{ RETURN(COMMA); }
<Code,GlaExt>";"		{ RETURN(SEMI); }
<Code,GlaExt>"`"		{ RETURN(BQUOTE); }

<Code,GlaExt>"."		{ RETURN(DOT); }
<Code,GlaExt>".."		{ RETURN(DOTDOT); }
<Code,GlaExt,UserPragma>"::"	{ RETURN(DCOLON); }
<Code,GlaExt,UserPragma>"="	{ RETURN(EQUAL); }
<Code,GlaExt>"\\"		{ RETURN(LAMBDA); }
<Code,GlaExt>"|"		{ RETURN(VBAR); }
<Code,GlaExt>"<-"		{ RETURN(LARROW); }
<Code,GlaExt,UserPragma>"->"	{ RETURN(RARROW); }
<Code,GlaExt>"-"    		{ RETURN(MINUS); }
<Code,GlaExt>"+"    		{ RETURN(PLUS); }

<Code,GlaExt,UserPragma>"=>"	{ RETURN(DARROW); }
<Code,GlaExt>"@"		{ RETURN(AT); }
<Code,GlaExt>"!"		{ RETURN(BANG); }
<Code,GlaExt>"~"    		{ RETURN(LAZY); }

%{
    /*
     * Integers and (for Glasgow extensions) primitive integers.  Note that
     * we pass all of the text on to the parser, because flex/C can't handle
     * arbitrary precision numbers.
     */
%}

<GlaExt>("-")?"0"[Oo]{O}+"#"  { /* octal */
			 yylval.uid = xstrndup(yytext, yyleng - 1);
			 RETURN(INTPRIM);
			}
<Code,GlaExt>"0"[Oo]{O}+  { /* octal */
			 yylval.uid = xstrndup(yytext, yyleng);
			 RETURN(INTEGER);
			}
<GlaExt>("-")?"0"[Xx]{H}+"#"  { /* hexadecimal */
			 yylval.uid = xstrndup(yytext, yyleng - 1);
			 RETURN(INTPRIM);
			}
<Code,GlaExt>"0"[Xx]{H}+  { /* hexadecimal */
			 yylval.uid = xstrndup(yytext, yyleng);
			 RETURN(INTEGER);
			}
<GlaExt>("-")?{N}"#"	{
			 yylval.uid = xstrndup(yytext, yyleng - 1);
			 RETURN(INTPRIM);
			}
<Code,GlaExt,UserPragma>{N} {
			 yylval.uid = xstrndup(yytext, yyleng);
			 RETURN(INTEGER);
			}

%{
    /*
     * Floats and (for Glasgow extensions) primitive floats/doubles.
     */
%}

<GlaExt>("-")?{F}"##" 	{
			 yylval.uid = xstrndup(yytext, yyleng - 2);
			 RETURN(DOUBLEPRIM);
			}
<GlaExt>("-")?{F}"#" 	{
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

<GlaExt>"``"[^']+"''"	{
			 hsnewid(yytext + 2, yyleng - 4);
			 RETURN(CLITLIT);
			}

%{
    /*
     * Identifiers, both variables and operators.  The trailing hash is allowed
     * for Glasgow extensions.
     */
%}


%{
/* These SHOULDNAE work in "Code" (sigh) */
%}
<GlaExt,UserPragma>{Id}"#" { 
			if (! nonstandardFlag) {
			    char errbuf[ERR_BUF_SIZE];
			    sprintf(errbuf, "Non-standard identifier (trailing `#'): %s\n", yytext);
			    hsperror(errbuf);
			 }
    	    	    	 hsnewid(yytext, yyleng);
    	    	    	 RETURN(isconstr(yytext) ? CONID : VARID);
			}
<Code,GlaExt,UserPragma>{Id}	{
    	    	         hsnewid(yytext, yyleng);
			 RETURN(isconstr(yytext) ? CONID : VARID);
			}
<Code,GlaExt,UserPragma>{SId}	{
			 if (is_commment(yytext,yyleng)) {
				int c;
				while ((c = input()) != '\n' && c != '\r' && c!= EOF )
					;
				if (c != EOF)
				   unput(c);
			 } else {
	    	    	    hsnewid(yytext, yyleng);
			    RETURN(isconstr(yytext) ? CONSYM : VARSYM);
			 }
			}
<Code,GlaExt,UserPragma>{Mod}"."{Id}"#"	{
			 BOOLEAN is_constr;
			 if (! nonstandardFlag) {
			    char errbuf[ERR_BUF_SIZE];
			    sprintf(errbuf, "Non-standard identifier (trailing `#'): %s\n", yytext);
			    hsperror(errbuf);
			 }
			 is_constr = hsnewqid(yytext, yyleng);
			 RETURN(is_constr ? QCONID : QVARID);
			}
<Code,GlaExt,UserPragma>{Mod}"."{Id}	{
			 BOOLEAN is_constr = hsnewqid(yytext, yyleng);
			 RETURN(is_constr ? QCONID : QVARID);
			}
<Code,GlaExt,UserPragma>{Mod}"."{SId}	{
			 BOOLEAN is_constr = hsnewqid(yytext, yyleng);
			 RETURN(is_constr ? QCONSYM : QVARSYM);
			}

%{
    /* Why is `{Id}#` matched this way, and `{Id}` lexed as three tokens? --JSM */

    /* Because we can make the former well-behaved (we defined them).

       Sadly, the latter is defined by Haskell, which allows such
       la-la land constructs as `{-a 900-line comment-} foo`.  (WDP 94/12)
    */
%}

<GlaExt,UserPragma>"`"{Id}"#`"	{	
    	    	    	 hsnewid(yytext + 1, yyleng - 2);
			 RETURN(isconstr(yytext+1) ? CONSYM : VARSYM);
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

<GlaExt>'({CHAR}|"\"")"'#" {
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
<Code,GlaExt>'({CHAR}|"\"")* {
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

			 if (! nonstandardFlag) {
			    char errbuf[ERR_BUF_SIZE];
			    sprintf(errbuf, "`Char-hash' literals are non-standard: %s\n", text);
			    hsperror(errbuf);
			 }

    	    	    	 if (length > 1) {
    	    	    	    fprintf(stderr, "%s:%d:%d: Unboxed character literal '",
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
    	    	    	    fprintf(stderr, "%s:%d:%d: Character literal '",
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

<GlaExt>"\""({CHAR}|"'")*"\""#  {
			 yylval.uhstring = installHstring(yyleng-3, yytext+1);
			    /* the -3 accounts for the " on front, "# on the end */
			 RETURN(STRINGPRIM); 
    	    	    	}
<Code,GlaExt>"\""({CHAR}|"'")*"\""  {
			 yylval.uhstring = installHstring(yyleng-2, yytext+1);
			 RETURN(STRING); 
    	    	    	}
<Code,GlaExt>"\""({CHAR}|"'")* {
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

			 if (! nonstandardFlag) {
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

%{
/*
 Not 100% correct, tokenizes "foo \  --<>--
                                 \ bar"

 as "foo  bar", but this is not correct as per Haskell 98 report and its
 maximal munch rule for "--"-style comments.

 For the moment, not deemed worthy to fix.
*/
%}
<StringEsc>"--"[^\n\r]*{NL}?{WS}*  { noGap=FALSE; }

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
     * Nested comments.  The major complication here is in trying to match the
     * longest lexemes possible, for better performance.  (See the flex document.)
     * That's why the rules look so bizarre.
     */
%}

<Code,GlaExt,UserPragma,StringEsc>"{-"	{ 
    	    	    	  noGap = FALSE; nested_comments = 1; comment_start = hsplineno; PUSH_STATE(Comment); 
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

<INITIAL,Code,GlaExt,UserPragma>(.|\n)	{ 
    	    	    	 fprintf(stderr, "%s:%d:%d: Illegal character: `", 
    	    	    	    input_filename, hsplineno, hspcolno + 1); 
    	    	    	 format_string(stderr, (unsigned char *) yytext, 1);
    	    	    	 fputs("'\n", stderr);
			 hsperror("");
			}
<Char>(.|\n)		{ 
    	    	    	 fprintf(stderr, "%s:%d:%d: Illegal character: `",
    	    	    	    input_filename, hsplineno, hspcolno + 1); 
    	    	    	 format_string(stderr, (unsigned char *) yytext, 1);
    	    	    	 fputs("' in a character literal\n", stderr);
			 hsperror("");
			}
<CharEsc>(.|\n)		{
    	    	    	 fprintf(stderr, "%s:%d:%d: Illegal character escape: `\\",
    	    	    	    input_filename, hsplineno, hspcolno + 1); 
    	    	    	 format_string(stderr, (unsigned char *) yytext, 1);
    	    	    	 fputs("'\n", stderr);
			 hsperror("");
    	    	    	}
<String>(.|\n)		{ if (nonstandardFlag) {
                             addtext(yytext, yyleng);
                          } else { 
                                fprintf(stderr, "%s:%d:%d: Illegal character: `", 
                                input_filename, hsplineno, hspcolno + 1); 
                                format_string(stderr, (unsigned char *) yytext, 1);
                                fputs("' in a string literal\n", stderr);
                                hsperror("");
			  }
			}
<StringEsc>(.|\n)	{
    	    	    	 if (noGap) {
    	    	    	     fprintf(stderr, "%s:%d:%d: Illegal string escape: `\\", 
    	    	    	    	input_filename, hsplineno, hspcolno + 1); 
    	    	    	     format_string(stderr, (unsigned char *) yytext, 1);
    	    	    	     fputs("'\n", stderr);
			     hsperror("");
    	    	    	 } else {
    	    	    	     fprintf(stderr, "%s:%d:%d: Illegal character: `",
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
			  char errbuf[ERR_BUF_SIZE];
    	    	    	  hsplineno = hslineno; hspcolno = hscolno;
 		          sprintf(errbuf, "unterminated comment (which started on line %d)", comment_start);
    	    	    	  hsperror(errbuf); 
    	    	    	}
<String,StringEsc><<EOF>>   { 
    	    	    	  hsplineno = hslineno; hspcolno = hscolno;
    	    	    	  hsperror("unterminated string literal"); 
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

void
yyinit(void)
{
    input_filename = xstrdup("<stdin>");

    /* We must initialize the input buffer _now_, because we call
       setyyin _before_ calling yylex for the first time! */
    yy_switch_to_buffer(yy_create_buffer(stdin, YY_BUF_SIZE));

    if (nonstandardFlag)
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

#ifdef HSP_DEBUG
#define LAYOUT_DEBUG
#endif


static BOOLEAN
hsshouldindent(void)
{
    return (!forgetindent && INDENTON);
}


/* Enter new context and set new indentation level */
void
hssetindent(void)
{
#ifdef LAYOUT_DEBUG
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
#ifdef LAYOUT_DEBUG
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
#ifdef LAYOUT_DEBUG
    fprintf(stderr, "hsentercontext:indent=%d,hscolno=%d,hspcolno=%d,INDENTPT[%d]=%d\n", indent, hscolno, hspcolno, icontexts, INDENTPT);
#endif
}


/* Exit a layout context */
void
hsendindent(void)
{
    --icontexts;
#ifdef LAYOUT_DEBUG
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
	icontexts = icontexts_save - 1;
	icontexts_save = 0;
#ifdef HSP_DEBUG
	fprintf(stderr, "finished reading interface (%d:%d:%d)\n", hscolno, hspcolno, INDENTPT);
#endif
	eof = FALSE;

	/* RETURN(LEOF); */
        hsperror("No longer using yacc to parse interface files");

    } else {
	yyterminate();
    }
    abort(); /* should never get here! */
    return(0);
}

/**********************************************************************
*                                                                     *
*                                                                     *
*     Input Processing for Interfaces -- Not currently used !!!       *
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

    if (modulelineno == 0) {
	modulelineno = startlineno;
    }

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
hsnewqid(char *name, int length)
{
    char* dot;
    char save = name[length];
    name[length] = '\0';

    dot = strchr(name, '.');
    *dot = '\0';
    yylval.uqid = mkaqual(installid(name),installid(dot+1));
    *dot = '.';
    name[length] = save;

    return isconstr(dot+1);
}

static
BOOLEAN
is_commment(char* lexeme, int len)
{
   char* ptr;
   int i;
   	
   if (len < 2) {
      return FALSE;
   }

   for(i=0;i<len;i++) {
     if (lexeme[i] != '-') return FALSE;
   }        
   return TRUE;
}
   
