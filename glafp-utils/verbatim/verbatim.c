# include "stdio.h"
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX BUFSIZ
# define output(c) putc(c,yyout)
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
# define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng; extern char yytext[];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
FILE *yyin = {stdin}, *yyout = {stdout};
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;
  /*	This Lex script acts as a filter to pre-process Latex files.
	It surrounds groups of lines beginning with a ">" sign, and
	preceded and followed by a blank line, with \begin{verbatim} 
	and \end{verbatim}.  The ">" may be preceded by a digit or digit
	range (eg 4>, 2-5>, 3->); in this case the digits are removed.  
	They are meant to be used for filtering out versions.
	It takes words surrounded with @ signs (thus @letrec@) and makes them
	come out in typewriter font, regardless of the current mode.
  */
# define NORM 2
# define VERB 4
# define MIRANDA 6
# define VERBATIM 8
# define VERBATIMSIM 10
#define PUSH		states[top++] =
#define POP		BEGIN states[--top]
#define yywrap() 	1
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
		int states[256];
		int top;
		BEGIN NORM;
		top = 0;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
	{ printf ("@"); }
break;
case 2:
		{ printf ("\\mbox{\\tt "); PUSH NORM;  BEGIN VERB; }
break;
case 3:
		{ printf ("}");  POP; }
break;
case 4:
	{ printf ("}\\\\{}\n\\mbox{\\tt "); }
break;
case 5:
	{ printf ("\\ "); }
break;
case 6:
	{ printf ("@"); }
break;
case 7:
	{ printf ("{\\char'43}"); }
break;
case 8:
	{ printf ("{\\char'44}"); }
break;
case 9:
	{ printf ("{\\char'45}"); }
break;
case 10:
	{ printf ("{\\char'46}"); }
break;
case 11:
	{ printf ("{\\char'176}"); }
break;
case 12:
	{ printf ("{\\char'137}"); }
break;
case 13:
	{ printf ("{\\char'136}"); }
break;
case 14:
	{ printf ("{\\char'134}"); }
break;
case 15:
	{ printf ("{\\char'173}"); }
break;
case 16:
	{ printf ("{\\char'175}"); }
break;
case 17:
	{ printf( "\\begin{verbatim}\n" ); 
			  PUSH NORM; BEGIN VERBATIMSIM; }
break;
case 18:
{ printf( "\\end{verbatim}\n" ); POP; }
break;
case 19:
{ printf( "\\begin{verbatim}" ); 
				  PUSH NORM; BEGIN VERBATIM; }
break;
case 20:
{ printf( "\\end{verbatim}" ); POP; }
break;
case 21:
{ printf ("\\begin{verbatim}\n>" ); 
			  PUSH NORM; BEGIN MIRANDA; }
break;
case 22:
{ printf( "\n>" ); }
break;
case 23:
	{ printf ("\\end{verbatim}\n"); POP; }
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */
int
main()
{
    yylex();
    return(0);
}
int yyvstop[] = {
0,

2,
0,

2,
0,

4,
0,

5,
0,

7,
0,

8,
0,

9,
0,

10,
0,

3,
0,

14,
0,

13,
0,

12,
0,

15,
0,

16,
0,

11,
0,

23,
0,

1,
0,

21,
0,

17,
0,

6,
0,

22,
0,

18,
0,

20,
0,

19,
0,
0};
# define YYTYPE char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
4,15,	5,17,	7,30,	8,31,	
33,43,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	16,38,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	5,18,	
0,0,	0,0,	5,19,	5,20,	
5,21,	5,22,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
36,45,	40,46,	0,0,	45,45,	
45,45,	45,45,	45,45,	45,45,	
45,45,	45,45,	45,45,	45,45,	
45,45,	0,0,	0,0,	0,0,	
0,0,	3,13,	4,16,	5,23,	
12,33,	13,34,	15,36,	15,36,	
15,36,	15,36,	15,36,	15,36,	
15,36,	15,36,	15,36,	15,36,	
16,34,	23,39,	0,0,	0,0,	
15,37,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	3,14,	4,14,	5,24,	
9,32,	5,25,	5,26,	0,0,	
0,0,	14,35,	30,40,	30,40,	
30,40,	30,40,	30,40,	30,40,	
30,40,	30,40,	30,40,	30,40,	
32,42,	35,44,	42,47,	44,48,	
30,41,	47,49,	48,50,	53,55,	
56,58,	57,59,	59,61,	60,62,	
50,52,	62,64,	5,27,	63,65,	
5,28,	5,29,	46,46,	46,46,	
46,46,	46,46,	46,46,	46,46,	
46,46,	46,46,	46,46,	46,46,	
49,51,	51,53,	52,54,	54,56,	
55,57,	58,60,	61,63,	64,66,	
65,67,	66,68,	67,69,	68,70,	
70,71,	0,0,	0,0,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+0,	0,		0,	
yycrank+0,	0,		0,	
yycrank+1,	0,		0,	
yycrank+2,	0,		0,	
yycrank+3,	0,		0,	
yycrank+0,	yysvec+5,	0,	
yycrank+4,	0,		0,	
yycrank+5,	0,		0,	
yycrank+4,	0,		0,	
yycrank+0,	yysvec+9,	0,	
yycrank+0,	0,		0,	
yycrank+4,	0,		0,	
yycrank+5,	0,		yyvstop+1,
yycrank+3,	0,		0,	
yycrank+22,	0,		0,	
yycrank+16,	0,		yyvstop+3,
yycrank+0,	0,		yyvstop+5,
yycrank+0,	0,		yyvstop+7,
yycrank+0,	0,		yyvstop+9,
yycrank+0,	0,		yyvstop+11,
yycrank+0,	0,		yyvstop+13,
yycrank+0,	0,		yyvstop+15,
yycrank+17,	0,		yyvstop+17,
yycrank+0,	0,		yyvstop+19,
yycrank+0,	0,		yyvstop+21,
yycrank+0,	0,		yyvstop+23,
yycrank+0,	0,		yyvstop+25,
yycrank+0,	0,		yyvstop+27,
yycrank+0,	0,		yyvstop+29,
yycrank+54,	0,		0,	
yycrank+0,	yysvec+30,	yyvstop+31,
yycrank+11,	0,		0,	
yycrank+6,	0,		0,	
yycrank+0,	0,		yyvstop+33,
yycrank+12,	0,		0,	
yycrank+3,	yysvec+15,	0,	
yycrank+0,	0,		yyvstop+35,
yycrank+0,	0,		yyvstop+37,
yycrank+0,	0,		yyvstop+39,
yycrank+4,	yysvec+30,	0,	
yycrank+0,	0,		yyvstop+41,
yycrank+4,	0,		0,	
yycrank+0,	0,		yyvstop+43,
yycrank+12,	0,		0,	
yycrank+3,	yysvec+15,	0,	
yycrank+82,	yysvec+30,	0,	
yycrank+17,	0,		0,	
yycrank+13,	0,		0,	
yycrank+17,	0,		0,	
yycrank+14,	0,		0,	
yycrank+23,	0,		0,	
yycrank+19,	0,		0,	
yycrank+18,	0,		0,	
yycrank+25,	0,		0,	
yycrank+30,	0,		0,	
yycrank+19,	0,		0,	
yycrank+23,	0,		0,	
yycrank+31,	0,		0,	
yycrank+25,	0,		0,	
yycrank+25,	0,		0,	
yycrank+30,	0,		0,	
yycrank+28,	0,		0,	
yycrank+22,	0,		0,	
yycrank+31,	0,		0,	
yycrank+39,	0,		0,	
yycrank+44,	0,		0,	
yycrank+25,	0,		0,	
yycrank+42,	0,		0,	
yycrank+0,	0,		yyvstop+45,
yycrank+27,	0,		0,	
yycrank+0,	0,		yyvstop+47,
0,	0,	0};
struct yywork *yytop = yycrank+152;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
0};
char yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
#ifndef lint
static	char ncform_sccsid[] = "@(#)ncform 1.6 88/02/08 SMI"; /* from S5R2 1.2 */
#endif

int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
char yytext[YYLMAX];
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
char yysbuf[YYLMAX];
char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
yylook(){
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
			*yylastch++ = yych = input();
			yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( (int)yyt > (int)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((int)yyt < (int)yycrank) {		/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
					sprint(yytext);
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
yyback(p, m)
	int *p;
{
if (p==0) return(0);
while (*p)
	{
	if (*p++ == m)
		return(1);
	}
return(0);
}
	/* the following are only used in the lex library */
yyinput(){
	return(input());
	}
yyoutput(c)
  int c; {
	output(c);
	}
yyunput(c)
   int c; {
	unput(c);
	}
