%{
#define YYSTYPE long

#ifdef __STDC__
#define PROTO(x)	x
#else
#define PROTO(x)	()
#endif

# include "syntax.tab.h"
# include <stdio.h>
# include "id.h"
# include "tree.h"
# include "funs.h"
extern	YYSTYPE yylval;
extern  FILE *fc, *fhs;
int lineno;

void countNewlines (char *s)
{
  while (*s) if (*s++ == '\n') lineno += 1;
}

%}

%%
";"			{ return(SEMICOL); }
":"			{ return(COLON); }
"<"			{ return(STDEF); }
">"			{ return(ENDDEF); }
"type"	 		{ return(TYPE); }
"end"	 		{ return(END); }
[A-Za-z][A-Za-z0-9_]*	{ yylval = (YYSTYPE) installid(yytext); return(ID); }
"\n"			{ lineno += 1; }
.			{ }
"/*"([^*]|"*"[^/]|\n)*"*/" { countNewlines(yytext); }
"%{{"([^%]|"%"[^}]|\n)*"%}}" {
				/* For Haskell includes */
				countNewlines(yytext);
				yytext[yyleng-3] = '\0';
				fprintf(fhs, "\n%s", yytext+3);
			      }
"%{"([^%]|"%"[^}]|\n)*"%}"  {
			      countNewlines(yytext);
			      yytext[yyleng-2] = '\0';
			      fprintf(fc, "\n%s",  &yytext[2]);
			    }
%%
int
yywrap()
{
	return(1);
}
