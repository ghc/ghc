
  /*	This Lex script acts as a filter to pre-process Latex files.

	It surrounds groups of lines beginning with a ">" sign, and
	preceded and followed by a blank line, with \begin{verbatim} 
	and \end{verbatim}.  The ">" may be preceded by a digit or digit
	range (eg 4>, 2-5>, 3->); in this case the digits are removed.  
	They are meant to be used for filtering out versions.

	It takes words surrounded with @ signs (thus @letrec@) and makes them
	come out in typewriter font, regardless of the current mode.
  */

%START  NORM  VERB  VERBENV
sp			[ \t]*
nl			{sp}\n{sp}
comment                 \%.*$
miranda			([0-9]+(\-([0-9]+)?)?)?>
%{
#define PUSH		states[top++] =
#define POP		BEGIN states[--top]
#define yywrap() 	1
#define YY_SKIP_YYWRAP
%}
%%
			int states[256];
			int top;
			BEGIN NORM;
			top = 0;
<NORM>@@		{ printf ("@"); }
<NORM>@			{ printf ("<tt>"); PUSH NORM;  BEGIN VERB; }
<NORM>\\%               { printf ("&percnt;"); }
<NORM>{comment}         { }
<VERB>@			{ printf ("</tt>");  POP; }
<VERB>@@		{ printf ("@"); }
<VERB>\>		{ printf ("&gt;"); }
<VERB>\<		{ printf ("&lt;"); }
<VERB>\#		{ printf ("&num;"); }
<VERB>\$		{ printf ("&dollar;"); }
<VERB>\%		{ printf ("&percnt;"); }
<VERB>\&		{ printf ("&amp;"); }
<VERB>\~		{ printf ("&tilde;"); }
<VERB>\^		{ printf ("&circ;"); }

<NORM>\<verb\>		 { printf ("<verb>"); PUSH NORM; BEGIN VERBENV; }
<NORM>\<code\>		 { printf ("<code>"); PUSH NORM; BEGIN VERBENV; }
<NORM>\\begin\{code\}	 { printf ("<code>"); PUSH NORM; BEGIN VERBENV; }
<VERBENV>\<\/verb\>	 { printf ("</verb>"); POP; }
<VERBENV>\<\/code\>	 { printf ("</code>"); POP; }
<VERBENV>\<\\end\{code\} { printf ("</code>"); POP; }
<VERBENV>\&\&		 { printf ("&"); }
<VERBENV>\&		 { printf ("&ero;"); }
<VERBENV>\<\/		 { printf ("&etago;"); }

%%
int
main()
{
    yylex();
    return(0);
}

/*
<VERB>\_		{ printf ("{\\char'137}"); }
<VERB>\\		{ printf ("{\\char'134}"); }
<VERB>\{		{ printf ("{\\char'173}"); }
<VERB>\}		{ printf ("{\\char'175}"); }
*/
