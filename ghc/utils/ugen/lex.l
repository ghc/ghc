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
#undef ECHO /* partain */
#define ECHO /*fprintf(stderr, "%s", yytext)*/

%}
%%
";"			{ ECHO; return(SEMICOL); }
":"			{ ECHO; return(COLON); }
"<"			{ ECHO; return(STDEF); }
">"			{ ECHO; return(ENDDEF); }
"type"	 		{ ECHO; return(TYPE); }
"end"	 		{ ECHO; return(END); }
[A-Za-z][A-Za-z0-9_]*	{
				ECHO;
				yylval = (YYSTYPE) installid(yytext);
				return(ID);
			}
.			ECHO;
"\n"			ECHO; 
"/*"([^*]|"*"[^/]|\n)*"*/" ECHO;
"%{{"([^%]|"%"[^}]|\n)*"%}}" { /* partain: for Haskell includes */
				ECHO;
				yytext[yyleng-3] = '\0';
				fprintf(fhs, "\n%s",  &yytext[3]);
			}
"%{"([^%]|"%"[^}]|\n)*"%}" {
				ECHO;
				yytext[yyleng-2] = '\0';
				fprintf(fc, "\n%s",  &yytext[2]);
			}
%%
int
yywrap()
{
	return(1);
}
