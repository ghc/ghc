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
*	19/03/92 Jon Hill	Added Data Parallel Notation	      *
*       24/04/92 ps             Added 'scc'.                          *
*       03/06/92 kh             Changed Infix/Prelude Handling.       *
*                                                                     *
*                                                                     *
*       Known Problems:                                               *
*                                                                     *
*               None, any more.                                       *
*                                                                     *
**********************************************************************/

#include "include.h"
#include "hsparser-DPH.tab.h"
#include <stdio.h>
#include <ctype.h>
#include "constants.h"

char 	*input_filename = NULL;

#include "utils.h"


/**********************************************************************
*                                                                     *
*                                                                     *
*      Declarations                                                   *
*                                                                     *
*                                                                     *
**********************************************************************/


extern int yylineno;
unsigned yylastlineno = 0;	/* Line number of previous token */
unsigned startlineno = 0;	/* temp; used to save the line no where something starts */
int yylastposn = 0;		/* Absolute position of last token */
int yylinestart = 0;		/* Absolute position of line start */

static int yyposn = 0;

/* Essential forward declarations */

static int readstring(), readasciiname(), readcomment(),
           lookupascii(), yynewid() /* OLD:, parse_pragma()*/;
static char escval();

static BOOLEAN incomment = FALSE;
static unsigned commentdepth = 0;

static BOOLEAN indenteof = FALSE;

/* Pragmas */
/* OLD: char *pragmatype, *pragmaid, *pragmavalue; */

/* Special file handling for IMPORTS */

static  FILE  *yyin_save = NULL;		/*  Saved File Pointer   	*/
static  char  *filename_save;			/*  File Name            	*/
static  int   yylineno_save = 0,                /*  Line Number          	*/
	      yyposn_save = 0,			/*  This Token		 	*/
	      yylastposn_save = 0,		/*  Last Token		 	*/
	      yyindent_save,			/*  Indentation          	*/
	      yylindent_save,     		/*  Left Indentation	 	*/
	      yytchar_save = 0,			/*  Next Input Character 	*/ 
	      icontexts_save = 0;		/*  Indent Context Level 	*/
static unsigned yylastlineno_save = 0;		/*  Line Number of Prev. token	*/

static BOOLEAN leof = FALSE;			/*  EOF for interfaces		*/


extern BOOLEAN ignorePragmas;		/*  True when we should ignore pragmas */
extern BOOLEAN ignoreArityPragmas;	/*  And various specific flavors... */
extern BOOLEAN ignoreSpecializePragmas;
extern BOOLEAN ignoreStrictnessPragmas;
extern BOOLEAN ignoreUpdatePragmas;



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


static short 
	yyindent = 0,		/* Current indentation */
  	yylindent = 0,		/* Indentation of the leftmost char in the current lexeme */
  	yyslindent = -1,	/* Indentation of the leftmost char in a string */
	yytabindent = 0,	/* Indentation before a tab in case we have to backtrack */
  	forgetindent = FALSE;	/* Don't bother applying indentation rules */

static int yysttok = -1;	/* Stacked Token:
					-1   -- no token;
					-ve  -- ";" inserted before token
					+ve  -- "}" inserted before token
				*/

short icontexts = 0;		/* Which context we're in */



/*
	Table of indentations:  right bit indicates whether to use
	  indentation rules (1 = use rules; 0 = ignore)

    partain:
    push one of these "contexts" at every "case" or "where"; the right bit says
    whether user supplied braces,etc., or not.  pop appropriately (yyendindent).

    ALSO, a push/pop when enter/exit a new file (e.g., on importing).  A -1 is
    pushed (the "column" for "module", "interface" and EOF).  The -1 from the initial
    push is shown just below.

*/


static short indenttab[MAX_CONTEXTS] = { -1 };

#define INDENTPT (indenttab[icontexts]>>1)
#define INDENTON (indenttab[icontexts]&1)


yyshouldindent()
{
  return(!leof && !forgetindent && INDENTON);
}


/* Enter new context and set new indentation level */
yysetindent()
{
#ifdef DEBUG
	  fprintf(stderr,"yysetindent:yyindent=%d,yylindent=%d,INDENTPT[%d]=%d\n",yyindent,yylindent,icontexts,INDENTPT);
#endif

  /* partain: first chk that new indent won't be less than current one;
    	this code doesn't make sense to me; yyindent tells the position of the _end_
	of the current token; what that has to do with indenting, I don't know.
  */


  if(yyindent-1 <= INDENTPT)
    {
      if (INDENTPT == -1)
	  return;	/* Empty input OK for Haskell 1.1 */
      else
	{
	  char errbuf[ERR_BUF_SIZE];
	  sprintf(errbuf,"Layout error -- indentation should be > %d cols",INDENTPT);
	  yyerror(errbuf);
	}
    }
  yyentercontext((yylindent << 1) | 1);
}


/* Enter a new context without changing the indentation level */

yyincindent()
{
#ifdef DEBUG
	  fprintf(stderr,"yyincindent:yyindent=%d,yylindent=%d,INDENTPT[%d]=%d\n",yyindent,yylindent,icontexts,INDENTPT);
#endif
  yyentercontext(indenttab[icontexts] & ~1);
}


/* Turn off indentation processing, usually because an explicit "{" has been seen */

yyindentoff()
{
  forgetindent = TRUE;
}


/* Enter a new layout context. */

yyentercontext(indent)
int indent;
{
  /* Enter new context and set indentation as specified */
  if(++icontexts >= MAX_CONTEXTS)
    {
      char errbuf[ERR_BUF_SIZE];
      sprintf(errbuf,"'wheres' and 'cases' nested too deeply (>%d)", MAX_CONTEXTS-1);
      yyerror(errbuf);
    }

  forgetindent = FALSE;
  indenttab[icontexts] = indent;
#ifdef DEBUG
	  fprintf(stderr,"yyentercontext:indent=%d,yyindent=%d,yylindent=%d,INDENTPT[%d]=%d\n",indent,yyindent,yylindent,icontexts,INDENTPT);
#endif
}


/* Exit a layout context */

yyendindent()
{
  --icontexts;
#ifdef DEBUG
	  fprintf(stderr,"yyendindent:yyindent=%d,yylindent=%d,INDENTPT[%d]=%d\n",yyindent,yylindent,icontexts,INDENTPT);
#endif
}




/* 
 * 	Return checks the indentation level and returns ;, } or the specified token.
 */

#define RETURN(tok) return(Return(tok))

Return(tok)
int tok;
{
  if(yyslindent != -1)
    {
      yylindent = yyslindent;
      yyslindent = -1;
    }
  else
    yylindent = yyindent-yyleng;

  if (yyshouldindent())
    {
      if (yylindent < INDENTPT)
	{
#ifdef DEBUG
	  fprintf(stderr,"inserted '}' before %d (%d:%d:%d:%d)\n",tok,yylindent,yyindent,yyleng,INDENTPT);
#endif
	  yysttok=tok;
	  return(VCCURLY);
	}

      else if (yylindent == INDENTPT)
	{
#ifdef DEBUG
	  fprintf(stderr,"inserted ';' before %d (%d:%d)\n",tok,yylindent,INDENTPT);
#endif
	  yysttok = -tok;
	  return (SEMI);
	}
    }
  yysttok = -1;
  leof = FALSE;
#ifdef DEBUG
	  fprintf(stderr,"returning %d (%d:%d)\n",tok,yylindent,INDENTPT);
#endif
  return(tok);
}


/**********************************************************************
*                                                                     *
*                                                                     *
*     Input Processing for Interfaces                                 *
*                                                                     *
*                                                                     *
**********************************************************************/


/* setyyin(file)	open file as new yyin */
/* partain: got rid of .ext stuff */
setyyin(file)
char *file;
{
  char fbuf[FILENAME_SIZE];

  strcpy(fbuf,file);

  yyin_save = yyin;

  if((yyin=fopen(fbuf,"r"))==NULL)
    {
      char errbuf[ERR_BUF_SIZE];
      sprintf(errbuf,"can't read \"%-.50s\"", fbuf);
      yyerror(errbuf);
    }

  yylineno_save = yylineno;
  yylastlineno_save = yylastlineno;
  yylineno = yylastlineno = 0;

  yylastposn_save = yylastposn;
  yyposn_save = yyposn;
  yyposn = yylastposn = -1;

  filename_save = xmalloc(strlen(input_filename)+1);
  strcpy(filename_save,input_filename);
  new_filename(fbuf);
  yyindent_save = yyindent;
  yylindent_save = yylindent;
  yyindent = yylindent = 0;
  yyentercontext(-1);		/* partain: changed this from 0 */
  icontexts_save = icontexts;
  yytchar_save = yytchar;
#ifdef DEBUG
  fprintf(stderr,"yytchar = %c(%d)\n",yytchar,(int)yytchar);
#endif
  yysptr = yysbuf;
#ifdef DEBUG
  fprintf(stderr,"reading %s (%d:%d:%d)\n",input_filename,yyindent_save,yylindent_save,INDENTPT);
#endif
}
  
    

/*
	input() is the raw input routine used by yylex()
*/

#undef input			/*  so we can define our own versions to handle layout */
#undef unput


static 
input()
{
  if(yytchar==10)
    yyindent = 0;			/* Avoid problems with backtracking over EOL */

  yytchar=yytchar==EOF?EOF:(++yyposn,yysptr>yysbuf?U(*--yysptr):getc(yyin));

  if(yytchar==10)
    {
      yylinestart = yyposn;
      yylineno++;
    }

  if (yytchar == '\t')
    {
      yytabindent = yyindent;		/* Remember TAB indentation - only 1, though! */
      yyindent += 8 - (yyindent % 8);	/* Tabs stops are 8 columns apart */
    }
  else
    ++yyindent;


  /* Special EOF processing inserts all missing '}'s into the input stream */

  if(yytchar==EOF)
    {
      if(icontexts>icontexts_save && !incomment)
	{
	  if(INDENTON)
	    {
	      indenttab[icontexts] = 0;
	      indenteof = TRUE;
	      return('\002');
	    }
	  else
	      yyerror("missing '}' at end of file");
	}

      else if (yyin_save != NULL)
	  {
	    fclose(yyin);
	    yyin = yyin_save;
	    yyin_save = NULL;
	    new_filename(filename_save);
	    free(filename_save);
	    yylineno = yylineno_save;
	    yylastlineno = yylastlineno_save;
	    yyindent = 0;
	    yylindent = 0;
	    yyindent = yyindent_save;
	    yylindent = yylindent_save;
	    yyslindent = -1;
	    icontexts = icontexts_save -1;
	    icontexts_save = 0;
	    leof = TRUE;
	    yyposn = yyposn_save;
	    yylastposn = yylastposn_save;
#ifdef DEBUG
  fprintf(stderr,"finished reading interface (%d:%d:%d)\n",yyindent,yylindent,INDENTPT);
#endif
	    return('\001');	/* YUCK */
	  }
	else
	  return(0);
    }
  else
    return(yytchar);
}

setstartlineno()
{
  if(yytchar == 10)
    startlineno = yylineno -1;
  else
    startlineno = yylineno;
}


/*
 *	unput() backtracks over a character.  With luck it will never backtrack over
 *		multiple EOLs and TABs (since these are lexical delimiters).
 */

static
unput(c) 
char c;
{
  /* fprintf(stderr,"Unputting %c\n",c); */

  yytchar= (c);

  if(yytchar=='\n' || yytchar=='\r')
    yylineno--;

  *yysptr++=yytchar;
  if(c == '\t')
    yyindent = yytabindent;
  else
    --yyindent;

  --yyposn;
}


/* 
 *	Redefine yylex to check for stacked tokens, yylex1() is the original yylex()
 */

yylex()
{
  if(yysttok != -1)
    {
      if(yysttok < 0)
	{
	  int tok = -yysttok;
	  yysttok = -1;
	  return(tok);
	}
      RETURN(yysttok);
    }
  else
    {
      /* not quite right, and should take account of stacking */
      yylastlineno = yylineno;
      yylastposn = yyposn;
      return(yylex1());
    }
}

#define yylex() yylex1()
%}

%start PRIM

D			[0-9]
O			[0-7]
H			[0-9A-Fa-f]
N			{D}+
S			[!#$%&*+./<=>?@\\^|~:]
NS			[^!#$%&*+./<=>?@\\^|~:]
SId			({S}|~|-){S}*
Char			[ !\"#$%&()*+,\-./0-9:;<=>?@A-Z\[\]^_`a-z{|}~]
L			[A-Z]
I			[A-Za-z]
i			[A-Za-z0-9'_]
Id			{I}({i})*
A			(NUL|SOH|STX|ETX|EOT|ENQ|ACK|BEL|BS|HT|LF|VT|FF|CR|SO|SI|DLE|DC1|DC2|DC3|DC4|NAK|SYN|ETB|CAN|EM|SUB|ESC|FS|GS|RS|US|SP|DEL)
WS			[ \t\n\r\f]*

%e 1000
%o 2100
%a 2100
%p 3600
%n 490
%k 350

%%

^"# ".*[\n\r]		{  char tempf[FILENAME_SIZE];
			   sscanf(yytext+1, "%d \"%[^\"]", &yylineno, tempf); 
			   new_filename(tempf); 
			}

^"{-# LINE ".*"-}"[\n\r] { /* partain: pragma-style line directive */
			  char tempf[FILENAME_SIZE];
			  sscanf(yytext+9, "%d \"%[^\"]", &yylineno, tempf); 
			  new_filename(tempf);
			}

"{-# ARITY "		{ if ( ignorePragmas || ignoreArityPragmas ) {
			     incomment = 1;
			     readcomment();
			     incomment = 0;
			  } else {
			     RETURN(ARITY_PRAGMA);
			  }
			}
"{-# SPECIALIZE "	{ if ( ignorePragmas || ignoreSpecializePragmas ) {
			     incomment = 1;
			     readcomment();
			     incomment = 0;
			  } else {
			     RETURN(SPECIALIZE_PRAGMA);
			  }
			}
"{-# STRICTNESS "	{ if ( ignorePragmas || ignoreStrictnessPragmas ) {
			     incomment = 1;
			     readcomment();
			     incomment = 0;
			  } else {
			     RETURN(STRICTNESS_PRAGMA);
			  }
			}
"{-# UPDATE "		{ if ( ignorePragmas || ignoreUpdatePragmas ) {
			     incomment = 1;
			     readcomment();
			     incomment = 0;
			  } else {
			     RETURN(UPDATE_PRAGMA);
			  }
			}

" #-}"			{ RETURN(END_PRAGMA); }

<PRIM>"void#"		{ RETURN(VOIDPRIM); }
<PRIM>{Id}"#"		{       yynewid(yytext,yyleng);
				RETURN(isconstr(yytext)? CONID: VARID);
				/* Must appear before keywords -- KH */
			}

"case"			{ RETURN(CASE); }
"class"			{ RETURN(CLASS); }
"data"			{ RETURN(DATA); }
"default"		{ RETURN(DEFAULT); }
"deriving"		{ RETURN(DERIVING); }
"else"			{ RETURN(ELSE); }
"hiding"		{ RETURN(HIDING); }
"if"			{ RETURN(IF); }
"import"		{ RETURN(IMPORT); }
"infix"			{ RETURN(INFIX); }
"infixl"		{ RETURN(INFIXL); }
"infixr"		{ RETURN(INFIXR); }
"instance"		{ RETURN(INSTANCE); }
"interface"		{ RETURN(INTERFACE); }
"module"		{ RETURN(MODULE); }
"of"			{ RETURN(OF); }
"renaming"		{ RETURN(RENAMING); }
"then"			{ RETURN(THEN); }
"to"			{ RETURN(TO); }
"type"			{ RETURN(TYPE); }
"where"			{ RETURN(WHERE); }
"in"			{ RETURN(IN); }
"let"			{ RETURN(LET); }
"ccall"			{ RETURN(CCALL); }
"veryDangerousCcall"	{ RETURN(CCALL_DANGEROUS); }
"casm"			{ RETURN(CASM); }
"veryDangerousCasm"	{ RETURN(CASM_DANGEROUS); }
"scc"			{ RETURN(SCC); }

".."			{ RETURN(DOTDOT); }
";"			{ RETURN(SEMI); }
","			{ RETURN(COMMA); }
"|"			{ RETURN(VBAR); }
"="			{ RETURN(EQUAL); }
"<-"			{ RETURN(LARROW); }
"->"			{ RETURN(RARROW); }
"=>"			{ RETURN(DARROW); }
"::"			{ RETURN(DCOLON); }
"("			{ RETURN(OPAREN); }
")"			{ RETURN(CPAREN); }
"["			{ RETURN(OBRACK); }
"]"			{ RETURN(CBRACK); }
"{"			{ RETURN(OCURLY); }
"}"			{ RETURN(CCURLY); }
"+"			{ RETURN(PLUS); }
"@"			{ RETURN(AT); }
"\\"			{ RETURN(LAMBDA); }
"_"			{ RETURN(WILDCARD); }
"`"			{ RETURN(BQUOTE); }
"<<"			{ RETURN(OPOD); }
">>"			{ RETURN(CPOD); }
"(|"			{ RETURN(OPROC); }
"|)"			{ RETURN(CPROC); }
"<<-"			{ RETURN(DRAWNFROM); }
"<<="			{ RETURN(INDEXFROM); }

<PRIM>("-")?{N}"#"	{
				yytext[yyleng-1] = '\0';	/* clobber the # first */
				yylval.uid = xstrdup(yytext);
				RETURN(INTPRIM);
			}
{N}			{
				yylval.uid = xstrdup(yytext);
				RETURN(INTEGER);
			}

<PRIM>{N}"."{N}(("e"|"E")("+"|"-")?{N})?"##"    {
				yytext[yyleng-2] = '\0';	/* clobber the # first */
				yylval.uid = xstrdup(yytext);
				RETURN(DOUBLEPRIM);
			}

<PRIM>{N}"."{N}(("e"|"E")("+"|"-")?{N})?"#"    {
				yytext[yyleng-1] = '\0';	/* clobber the # first */
				yylval.uid = xstrdup(yytext);
				RETURN(FLOATPRIM);
			}

{N}"."{N}(("e"|"E")("+"|"-")?{N})?    {
				yylval.uid = xstrdup(yytext);
				RETURN(FLOAT);
			}


<PRIM>"``"[^']+"''"	{       yytext[yyleng-2] = '\0';	/* clobber '' first */
				yynewid(yytext+2,yyleng-2);
				RETURN(CLITLIT);
			}

{Id} 			{       yynewid(yytext,yyleng);
				RETURN(isconstr(yytext)? CONID: VARID);
			}

{SId}			{	yynewid(yytext,yyleng);
				if(yyleng == 1)
				  if (*yytext == '~')
				    return( LAZY );
				  else if ( *yytext == '-' )
				    return( MINUS );
				RETURN(isconstr(yytext)? CONSYM: VARSYM);
			}

<PRIM>"`"{Id}"#`"	{	yynewid(yytext+1,yyleng-2);
				RETURN(isconstr(yytext+1)? CONSYM: VARSYM);
			}

'{Char}'		{
				yytext[2] = '\0';
				yylval.uid = xstrdup(yytext);
				RETURN(CHAR); 

			  /* WDP note:
				we don't yet return CHARPRIMs
				(ToDo)
			  */
			}

'\\(a|b|f|n|r|t|v)' 	{
				yytext[1] = escval(yytext[2]);
				yytext[2] = '\0';
				yylval.uid = xstrdup(yytext);
				RETURN(CHAR);
			}

'\\(\"|\'|\\)'	 	{
				yytext[1] = yytext[2];
				yytext[2] = '\0';
				yylval.uid = xstrdup(yytext);
				RETURN(CHAR);
			}

'\\{A}'			{	yytext[yyleng-1] = '\0';
				if(strcmp(yytext+2,"DEL")==0)
				  {
				    yylval.uid = xstrdup("'\177");
				    RETURN(CHAR);
				  }
				else
				  {
				    int a = lookupascii(yytext+2);
				    if(a >= 0)
				      {
					yytext[1] = a;
					yytext[2] = '\0';
					yylval.uid = xstrdup(yytext);
					RETURN(CHAR);
				      }
				    else
				      {
					char errbuf[ERR_BUF_SIZE];
					sprintf(errbuf,"invalid ASCII name in character constant: %s",yytext);
					yyerror(errbuf);
				      }
				  }
			}

'\\{D}+'		{	if(convchar(yytext+2,yyleng-3,10))
				  RETURN(CHAR);
			}

'\\o{O}+'		{	if(convchar(yytext+3,yyleng-4,8))
				  RETURN(CHAR);
			}

'\\x{H}+'		{	if(convchar(yytext+3,yyleng-4,16))
				  RETURN(CHAR);
			}

'\\\^[A-Z\[\\\]^_]'	{	yytext[1] = yytext[3]-'A'+ 1;
				yytext[2] = '\0';
				yylval.uid = xstrdup(yytext);
				RETURN(CHAR); 
			}

'\\\^@'			{	yytext[1] = '\0'; /* partain: most doubtful... */
				yytext[2] = '\0';
				yylval.uid = xstrdup(yytext);
				RETURN(CHAR); 
			}

"\""			{
  				readstring();
				yylval.uid = installString(yyleng, yytext);
			  	RETURN(STRING); 
			}


"--".*[\n\r]		;	/* hm-hm -style comment */

"\001"			{	if (leof)
				  {
				    unput(yytchar_save);
				    RETURN(LEOF);
				  }

				fprintf(stderr, "illegal char: %c (%d) in line %d\n",
					yytext[0], yytext[0], yylineno); 
			}

"\002"			{	if (indenteof)
				  {
				    indenteof = FALSE;
				    RETURN(VCCURLY);
				  }

				fprintf(stderr, "illegal char: %c (%d) in line %d\n",
					yytext[0], yytext[0], yylineno); 
			}

[\r\n \t\v\f]		;

.			{ fprintf(stderr, "illegal char: %c (%d) in line %d\n",
					yytext[0], yytext[0], yylineno); 
			}

"{-"			{
  				incomment = 1;
				readcomment();
  				incomment = 0;
			}
%%


/**********************************************************************
*                                                                     *
*                                                                     *
*     YACC/LEX Initialisation etc.                                    *
*                                                                     *
*                                                                     *
**********************************************************************/


/* 
   We initialise input_filename to "<NONAME>".
   This allows unnamed sources to be piped into the parser. 
*/

yyinit()
{
  extern BOOLEAN acceptPrim;

  input_filename = xstrdup("<NONAME>");

  yytchar = '\n';

  if(acceptPrim)
    BEGIN PRIM;
}


new_filename(f)
char *f;
{
  if(input_filename != NULL)
    free(input_filename);
  input_filename = xstrdup(f);
}
  


yywrap()
{
	return(1);
}


/**********************************************************************
*                                                                     *
*                                                                     *
*                      Comment Handling                               *
*                                                                     *
*                                                                     *
**********************************************************************/



/*
	readcomment()	reads Haskell nested comments {- ... -}
	  		Indentation is automatically taken care of since input() is used.

			While in principle this could be done using Lex rules, in
			practice it's easier and neater to use special code for this
			and for strings.
*/

static readcomment()
{
  int c;

  do {
    while ((c = input()) != '-' && !eof(c))
      {
	if(c=='{')
	  if ((c=input()) == '-')
	    readcomment();
	
	  else if (eof(c))
	    {
	      yyerror("comment not terminated by end of file");
	    }
      }

    while (c == '-')
      c = input();

    if (c == '}')
      break;

    if (eof(c))
      {
	yyerror("comment not terminated by end of file");
      }

  } while (1);
}


/*
    eof(c)	Returns TRUE when EOF read.
*/

eof(c)
int c;
{
  return (c == 0 || c == 1 && leof);
}



/**********************************************************************
*                                                                     *
*                                                                     *
*    Identifier Processing                                             *
*                                                                     *
*                                                                     *
**********************************************************************/


/*
	yynewid		Enters an id of length n into the symbol table.
*/

static yynewid(yyt,len)
char *yyt;
int len;
{
  char yybuf[1024];
  strcpy(yybuf,yyt);
  yybuf[len] = '\0';
  yylval.uid = installid(yybuf);
}


/*
  	isconstr(s)	True iff s is a constructor id.
*/

isconstr(s)
char *s;
{
  return(*s == ':' || isupper(*s));
}




/**********************************************************************
*                                                                     *
*                                                                     *
*     Character Kind Predicates                                       *
*                                                                     *
*                                                                     *
**********************************************************************/


/*
 * ishspace(ch)	determines whether ch is a valid Haskell space character
 */


static int ishspace(ch)
char ch;
{
  return(ch == '\n' || ch == ' ' || ch == '\t' || ch == '\v' || ch == '\f');
}


/*
 * isddigit(ch) determines whether ch is a valid Decimal digit
 */


static int isddigit(ch)
char ch;
{
 return (isdigit(ch));
}


/*
 * ishexdigit(ch) determines whether ch is a valid Hexadecimal digit
 */


static int ishexdigit(ch)
char ch;
{
 return (isdigit(ch) || (ch >= 'A' && ch <= 'F') || (ch >= 'a' && ch <= 'f'));
}

/*
 * isodigit(ch) determines whether ch is a valid Octal digit
 */


static int isodigit(ch)
char ch;
{
 return ((ch >= '0' && ch <= '7'));
}


/**********************************************************************
*                                                                     *
*                                                                     *
*       Lexical Analysis of Strings  -- Gaps and escapes mean that    *
*            lex isn't (wo)man enough for this job.                   *
*                                                                     *
*                                                                     *
**********************************************************************/


/*
 * readstring()		reads a string constant and places it in yytext
 */

static readstring()
{
  int ch, c;
  
  yyslindent = yyindent-1;

  yyleng = 1;
  yytext[1] = '\0';

  do
    {
      ch = input();

      if (ch == '\\')
	{
	  ch = input();

	  if(isdigit(ch))
	      ch = readescnum(isddigit,10,ch);

	  else if (ch == 'o')
	    {
	      ch = input();
	      if(isodigit(ch))
		ch = readescnum(isodigit,8,ch);
	      else
		{
		  char errbuf[ERR_BUF_SIZE];
		  sprintf(errbuf,"strange Octal character code (%c) in string",ch);
		  yyerror(errbuf);
		}
	    }

	  else if (ch == 'x')
	    {
	      ch = input();
	      if(ishexdigit(ch))
		ch = readescnum(ishexdigit,16,ch);
	      else
		{
		  char errbuf[ERR_BUF_SIZE];
		  sprintf(errbuf,"strange Hexadecimal character code (%c) in string",ch);
		  yyerror(errbuf);
		}
	    }

	  else if(ch == '"' || ch == '\\' || ch == '\'')
	    /* SKIP */;

	  else if (isupper(ch))
	    {
	      if((ch = readasciiname(ch)) == -1)
		yyerror("invalid ASCII name in string");
	    }
	  
	  else if (ch == '^')
	    {
	      if(isupper(ch = input()) || (ch >= '[' && ch <= '_'))
		ch = ch - 'A' + 1;
    	      else if (ch == '@')
    	    	ch = '\0';
	      else
		{
		  char errbuf[ERR_BUF_SIZE];
		  sprintf(errbuf,"strange control sequence (^%c) in string",ch);
		  yyerror(errbuf);
		}
	    }

	  else if (ishspace(ch))
	    {
    	      /* partain: we may want clearer error msgs if \v, \f seen */

	      while (ch == '\t' || ch == ' ')
		ch = input();

	      if (ch != '\n' && ch != '\r')
		yyerror("newline not seen when expected in string gap");
	      else
		ch = input();
		
	      while (ch == '\t' || ch == ' ')
		ch = input();

	      if(ch != '\\')
		yyerror("trailing \\ not seen when expected in string gap");
	      
	      ch = -1;
	    }

	  else if (ch == 'a')
	    ch = '\007';

	  else if (ch == 'b')
	    ch = '\b';

	  else if (ch == 'f')
	    ch = '\f';

	  else if (ch == 'n')
	    ch = '\n';

	  else if (ch == 'r')
	    ch = '\r';

	  else if (ch == 't')
	    ch = '\t';

	  else if (ch == 'v')
	    ch = '\v';

	  else if (ch == '&')
	    ch = -1;

	  else
	    {
	      char errbuf[ERR_BUF_SIZE];
	      sprintf(errbuf,"invalid escape sequence (\\%c) in string",ch);
	      yyerror(errbuf);
	    }
	}

      else if (ch == '\n' || ch == '\r' || ch == '\f' || ch == '\v' || ch == 0 || ch == '"')
	break;

      else if (!isprint(ch) && !ishspace(ch))
	{
	  char errbuf[ERR_BUF_SIZE];
	  sprintf(errbuf,"invalid character (%c) in string",ch);
	  yyerror(errbuf);
	}

      if((yyleng < YYLMAX-3 && ch != -1) || (yyleng == YYLMAX-3 && (ch == '\t' || ch == '\\')))
	{
	  /* The LML back-end treats \\ and \t specially in strings... */

	  if(ch == '\t' || ch == '\\')
	    {
	      yytext[yyleng++] = '\\';
	      if (ch == '\t')
		ch = 't';
	    }
	  if(yyleng<YYLMAX-2)
	    {
	      yytext[yyleng++] = ch;
	      yytext[yyleng] = '\0';
	    }
	}
      else if (ch != -1)
	{
	  char errbuf[ERR_BUF_SIZE];
	  sprintf(errbuf,"string too long (> %d characters)",YYLMAX-3-2);
	  yyerror(errbuf);
	}
    }
  while(1);

  if (ch != '"')
    yyerror("string incorrectly terminated");

  else
    {
      yytext[yyleng++] = '"';
      yytext[yyleng] = '\0';
    }
#ifdef DEBUG
  fprintf(stderr,"string: %s (%d chars)\n",yytext,yyleng-2);
#endif
}



/**********************************************************************
*                                                                     *
*                                                                     *
*      Haskell String and Character Escape Codes                      *
*                                                                     *
*                                                                     *
**********************************************************************/


/* Names of ASCII control characters, used in strings and character constants */

static char *asciinames[] =
  {
    "NUL",	"SOH",	"STX",	"ETX",	"EOT",	"ENQ",	"ACK",	"BEL",	"BS",	"HT",
    "LF",	"VT",	"FF",	"CR",	"SO",	"SI",	"DLE",	"DC1",	"DC2",	"DC3",
    "DC4",	"NAK",	"SYN",	"ETB",	"CAN",	"EM",	"SUB",	"ESC",	"FS",	"GS",
    "RS",	"US",	"SP",	"DEL"
    };


/*
 * readasciiname()	read ASCII name and translate to an ASCII code
 *			-1 indicates invalid name
 */

static int readasciiname(ch)
int ch;
{
  char asciiname[4];

  asciiname[0] = ch;
  if(!isupper(asciiname[1]= input()))
    {
      unput(asciiname[1]);
      return(-1);
    }

  if(!isupper(asciiname[2]=input()))
    {
      /* partain: have to have something extra for DC[1-4] */
      if (asciiname[0] == 'D' && asciiname[1] == 'C' && isdigit(asciiname[2])) {
	  asciiname[3] = '\0';
      } else {
	  unput(asciiname[2]);
	  asciiname[2] = '\0';
      }
    }
  else
    asciiname[3] = '\0';

  if (strcmp(asciiname,"DEL") == 0)
    return('\177');

  else
    return(lookupascii(asciiname));
}


/*
   lookupascii(ascii)	look up ascii in asciinames[]

   returns -1 if ascii is not found, otherwise its index.
*/

static int lookupascii(ascii)
char *ascii;
{
  int i;
  for(i='\0'; i <= ' '; ++i)
    if(strcmp(ascii,asciinames[i])==0)
      return(i);
  return(-1);
}


/**********************************************************************
*                                                                     *
*                                                                     *
*      Numeric Escapes in Characters/Strings                          *
*                                                                     *
*                                                                     *
**********************************************************************/

int convnum(num,numlen,base)
char *num;
int numlen, base;
{
  int i, res = 0, mul;

  for (i = numlen-1, mul = 1; i >= 0; --i, mul *= base)
    {
      if(isdigit(num[i]))
	res += (num[i] - '0') * mul;
      else if (isupper(num[i]))
	res += (num[i] - 'A' + 10) * mul;
      else if (islower(num[i]))
	res += (num[i] - 'a' + 10) * mul;
    }
  return(res);
}

convchar(num,numlen,base)
char *num;
int numlen, base;
{
  int n = convnum(num,numlen,base);
  if (n <= MAX_ESC_CHAR)
    {
      yytext[1] = n;
      yytext[2] = '\0';
      yylval.uid = xstrdup(yytext);
      return(1);
    }
  else
    {
      char errbuf[ERR_BUF_SIZE];
      sprintf(errbuf,"ASCII code > %d in character constant",MAX_ESC_CHAR);
      yyerror(errbuf);
    }
}

readescnum(isadigit,mulbase,ch)
int (*isadigit)();
int mulbase;
int ch;
{
  char digit[MAX_ESC_DIGITS];
  int digcount;

  digcount = 1;
  digit[0] = ch;
  
  while((*isadigit)(ch=input()))
    {
      if(digcount < MAX_ESC_DIGITS)
	digit[digcount] = ch;
      ++digcount;
    }

  unput(ch);

  if(digcount > MAX_ESC_DIGITS)
    {
      char errbuf[ERR_BUF_SIZE];
      sprintf(errbuf,"numeric character code too long (> %d characters) in string",MAX_ESC_DIGITS);
      yyerror(errbuf);
    }

  ch = convnum(digit,digcount,mulbase);
  
  if (ch > MAX_ESC_CHAR)
    {
      char errbuf[ERR_BUF_SIZE];
      sprintf(errbuf,"character code > ASCII %d in string",MAX_ESC_CHAR);
      yyerror(errbuf);
    }

  return(ch);
}


/*
  escval(c)	return the value of an escaped character.

  		\a 	BELL
		\b	BACKSPACE
		\f	FORMFEED
		\n	NEWLINE
		\r	CARRIAGE RETURN
		\t	TAB
		\v	VERTICAL TAB

   These definitions are standard ANSI C values.
*/

static char escval(c)
char c;
{
  return(c == 'a'? '\007': c == 'b'? '\b': c == 'f'? '\f': c == 'n'? '\n':
	 c == 'r'? '\r': c == 't'? '\t': c == 'v'? '\v': '\0');
}

/*
  OLD: Lexical analysis for Haskell pragmas.
*/

#if 0
static parse_pragma(s,len)
char *s;
int len;
{
  char pragma_name[1024];
  char identifier[1024];
  char value[1024];
  int i;

  *(s+len) = '\0';

  while(isspace(*s))
    s++;

  /* Pragma name */
  for(i=0; !isspace(*s); ++i, ++s)
    pragma_name[i] = *s;
  pragma_name[i] = '\0';

  while(isspace(*s))
    s++;

  /* Identifier */
  for(i=0; !isspace(*s); ++i, ++s)
    identifier[i] = *s;
  identifier[i] = '\0';

  while(isspace(*s))
    s++;

  /* equals */
  s++;

  while(isspace(*s))
    s++;

  /* Value */
  for(i=0; !isspace(*s); ++i, ++s)
    value[i] = *s;
  value[i] = '\0';

  pragmatype = installid(pragma_name);
  pragmaid = installid(identifier);
  pragmavalue = xstrdup(value);
}

#endif /* 0 */
