
  /*	This Lex script acts as a filter to pre-process Latex files.

	It surrounds groups of lines beginning with a ">" sign, and
	preceded and followed by a blank line, with \begin{verbatim} 
	and \end{verbatim}.  The ">" may be preceded by a digit or digit
	range (eg 4>, 2-5>, 3->); in this case the digits are removed.  
	They are meant to be used for filtering out versions.

	It takes words surrounded with @ signs (thus @letrec@) and makes them
	come out in typewriter font, regardless of the current mode.
  */

%START  NORM  VERB  MIRANDA VERBATIM VERBATIMSIM
sp			[ \t]*
nl			{sp}\n{sp}
miranda			([0-9]+(\-([0-9]+)?)?)?>
%{
#define PUSH		states[top++] =
#define POP		BEGIN states[--top]
#define yywrap() 	1
%}
%%
			int states[256];
			int top;
			BEGIN NORM;
			top = 0;
<NORM>@@		{ printf ("@"); }
<NORM>@			{ printf ("\\mbox{\\tt "); PUSH NORM;  BEGIN VERB; }
<VERB>@			{ printf ("}");  POP; }
<VERB>\n		{ printf ("}\\\\{}\n\\mbox{\\tt "); }
<VERB>" "		{ printf ("\\ "); }
<VERB>@@		{ printf ("@"); }
<VERB>\#		{ printf ("{\\char'43}"); }
<VERB>\$		{ printf ("{\\char'44}"); }
<VERB>\%		{ printf ("{\\char'45}"); }
<VERB>\&		{ printf ("{\\char'46}"); }
<VERB>\~		{ printf ("{\\char'176}"); }
<VERB>\_		{ printf ("{\\char'137}"); }
<VERB>\^		{ printf ("{\\char'136}"); }
<VERB>\\		{ printf ("{\\char'134}"); }
<VERB>\{		{ printf ("{\\char'173}"); }
<VERB>\}		{ printf ("{\\char'175}"); }

<NORM>^@{sp}\n		{ printf( "\\begin{verbatim}\n" ); 
			  PUSH NORM; BEGIN VERBATIMSIM; }
<VERBATIMSIM>^@{sp}\n	{ printf( "\\end{verbatim}\n" ); POP; }

<NORM>\\"begin{verbatim}"	{ printf( "\\begin{verbatim}" ); 
				  PUSH NORM; BEGIN VERBATIM; }
<VERBATIM>\\"end{verbatim}"	{ printf( "\\end{verbatim}" ); POP; }

<NORM>^\n{miranda}	{ printf ("\\begin{verbatim}\n>" ); 
			  PUSH NORM; BEGIN MIRANDA; }
<MIRANDA>\n{miranda}	{ printf( "\n>" ); }
<MIRANDA>^\n		{ printf ("\\end{verbatim}\n"); POP; }
%%
int
main()
{
    yylex();
    return(0);
}
