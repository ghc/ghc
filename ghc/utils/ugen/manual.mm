.nr N 1 
.nr L 72
.so /usr/lib/tmac/tmac.m
.SA 1
.ce
\fIRecursive Data Types Made Simple with Ugen\fR
.sp
.ce
Thomas Johnsson
.sp 2
.ce
\*(DT
.sp 2
.H 1 "Introduction"
Recursive datatypes in an important class of data structures
we often use in, for instance, abstract syntax trees in compilers.
An example of a recursive data type is shown below
(written in some hypothetical language):
.DS
	\fItype\fR bintree =
		\fIunion\fR
			interior: (bintree, bintree);
			leaf:     (int );
		\fIend union\fR;
.DE
The type bintree is a union of two variants: 'interior' which consists
of two bintrees, and 'leaf' which has an integer value associated to it.
.P
The program \fIugen\fR is yet another tool which relieves the 
the C-programmer from the burden of implementing the
constructor-, selector- and variant test functions associated to
such a type.
.H 1 "How to use ugen"
Suppose the specification below is in a file called 'treedef.u'.
.DS
    type bintree;
	interior : < getleft: bintree; getright: bintree; >;
	leaf	 : < getint: int; >;
    end;
.DE
The command
.DS
	ugen treedef.u
.DE
creates two files: 'treedef.c' and 'treedef.h'.
The file 'treedef.h' will contain the following definitions:
.DS
    typedef enum{ interior, leaf } Tbintree;
    typedef    ....                *bintree;
.DE
The type 'Tbintree' is an enumerated type with the same identifiers as
the variants of the recursive data type,
the type 'bintree' is implemented as a pointer to something.
This file must be included in all files where the type 'bintree'
is used.
Furthermore, the file treedef.h also contains macro definitions for
the selector functions; these macroes simply use the corresponding function
in treedefs.c that returns a pointer to that intended field.
In this manner, updating of a field can be done by simple assignment, 
by for example
.DS
    getleft(x) = .....
.DE
The file 'treedef.c' will contain the following definitions.
.sp
.nf
.in +4
#include "treedef.h"
/* The function tbintree returns the variant of the
 * bintree parameter.
 */
Tbintree tbintree(t) bintree t; { ... }

/* Constructor function for variant interior.
 */
bintree mkinterior(t1, t2) bintree t1, t2; { ... }

/* Its selector functions, returns pointers to a field in the node.
 */
bintree *Xgetleft(t) bintree t; { ... }
bintree *Xgetright(t) bintree t; { ... }


/* Constructor function for variant leaf.
 */
bintree mkleaf(i) int i; { ... }

/* Its selector function.
 */
int getint(t) bintree t; { ... }
.in -4
.sp
.fi
The pointers returned by the constructor functions are 
returned by the memory allocation function \fImalloc\fR,
so one may use \fIfree\fR to reclaim storage, if that is desired.
.P
The appendix contains the file listings of a complete program
that reads an expression on normal infix form and prints 
it in prefix form, eg:
.DS
    input:   12 + 3 * 5
    output:  +(12, *(3, 5))
.DE
Lex and yacc has been used for lexical- and syntax analysis,
ugen for the intermediate tree form, and make maintains it all.
.HU "Appendix - Example of use of ugen"
.nf
.sp
syntax.y:
.in +4
.sp
%{
#include "tree.h"
extern tree root;
%}
%token PLUS TIMES LPAR RPAR INT
%left  PLUS
%right TIMES
%start top
%%
top :	expr		{ root = $1; }

expr :	expr PLUS expr	{ $$ = mkplus($1, $3); } |
	expr TIMES expr	{ $$ = mktimes($1, $3); } |
	LPAR expr RPAR	{ $$ = $2; } |
	INT		{ $$ = mkinteger($1);}
%%
yyerror(s) char *s; {
	printf("%s\n", s);
}
.sp
.in -4
lexicals.l:
.in +4
.sp
%{
#include <stdio.h>
#include "y.tab.h"
extern int yylval;
%}
%%
"*"		return(TIMES);
"+"		return(PLUS);
"("		return(LPAR);
")"		return(RPAR);
[0-9]+		{ sscanf(yytext, "%d", &yylval);
		  return(INT);
		}
.		;
"\\n"		;
%%
int yywrap(){ return(1); }
.sp
.in -4
main.c:
.in +4
.sp
#include "tree.h"
tree root;

main() {
	if(! yyparse()) /* if no syntax errors .. */
		prefixprint(root);
}

prefixprint(t)
   tree t;
{
	switch(ttree(t)) {
	  case plus:
		printf("+(");
		prefixprint(gplusleft(t));
		printf(", ");
		prefixprint(gplusright(t));
		printf(")");
		break;
	  case times:
		printf("*(");
		prefixprint(gtimesleft(t));
		printf(", ");
		prefixprint(gtimesright(t));
		printf(")");
		break;
	  case integer:
		printf("%d", getint(t));
		break;
	}
}
.sp
.in -4
.SK
tree.u:
.sp
.in +4
type tree;
	plus	:< gplusleft	: tree;
		   gplusright	: tree;
		>;
	times	:< gtimesleft	: tree;
		   gtimesright  : tree;
		>;
	integer	:< getint	: int;
		>;
end;
.sp
.in -4
makefile:
.sp
.in +4
pre	: main.o y.tab.o lex.yy.o tree.o
	cc  main.o y.tab.o lex.yy.o tree.o -o pre
main.o	: main.c tree.h
	cc -c main.c
y.tab.o	: y.tab.c
	cc -c y.tab.c
lex.yy.o: lex.yy.c y.tab.h
	cc -c lex.yy.c
tree.o	: tree.c tree.h
	cc -c tree.c
y.tab.c	: syntax.y
	yacc -d syntax.y
lex.yy.c: lexicals.l
	lex lexicals.l
tree.c tree.h : tree.u
	ugen tree.u
