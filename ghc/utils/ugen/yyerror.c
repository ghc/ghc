#include <stdio.h>
extern int lineno;
extern char *yytext;

void yyerror(s)
     char *s;
{
  fprintf(stderr, "\n%s", s);
  if (lineno) fprintf(stderr, ", line %d, ", lineno);
  fprintf(stderr, "on input: ");
  fprintf(stderr, "%s\n", yytext);
}
