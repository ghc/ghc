#include <stdio.h>
extern int yylineno;

void
yyerror(s)
    char *s;
{
	extern int yychar;
	extern char yytext[1];

	fprintf(stderr, "\n%s", s);
	if(yylineno)
		fprintf(stderr, ", line %d, ", yylineno);
	fprintf(stderr, "on input: ");
	if( yychar >= 0400 )
		fprintf(stderr, "%s\n", &yytext[0]);
	else
		switch(yychar) {
		  case '\t' : fprintf(stderr, "\\t\n"); break;
		  case '\n' : fprintf(stderr, "\\n\n"); break;
		  case '\0' : fprintf(stderr, "$end\n"); break;
		  default   : fprintf(stderr, "%c\n", yychar); break;
		}
}
