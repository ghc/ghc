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

# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
		{ ECHO; return(SEMICOL); }
break;
case 2:
		{ ECHO; return(COLON); }
break;
case 3:
		{ ECHO; return(STDEF); }
break;
case 4:
		{ ECHO; return(ENDDEF); }
break;
case 5:
 		{ ECHO; return(TYPE); }
break;
case 6:
 		{ ECHO; return(END); }
break;
case 7:
{
				ECHO;
				yylval = (YYSTYPE) installid(yytext);
				return(ID);
			}
break;
case 8:
		ECHO;
break;
case 9:
		ECHO;
break;
case 10:
ECHO;
break;
case 11:
{ /* partain: for Haskell includes */
				ECHO;
				yytext[yyleng-3] = '\0';
				fprintf(fhs, "\n%s",  &yytext[3]);
			}
break;
case 12:
{
				ECHO;
				yytext[yyleng-2] = '\0';
				fprintf(fc, "\n%s",  &yytext[2]);
			}
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */
int
yywrap()
{
	return(1);
}
int yyvstop[] = {
0,

8,
0,

9,
0,

8,
0,

8,
0,

2,
8,
0,

1,
8,
0,

3,
8,
0,

4,
8,
0,

7,
8,
0,

7,
8,
0,

7,
8,
0,

7,
0,

7,
0,

7,
0,

6,
7,
0,

7,
0,

12,
0,

10,
0,

5,
7,
0,

12,
0,

11,
0,
0};
# define YYTYPE char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	1,4,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,5,	0,0,	
0,0,	0,0,	0,0,	1,3,	
6,15,	0,0,	0,0,	15,15,	
1,6,	1,3,	20,19,	26,21,	
0,0,	0,0,	0,0,	0,0,	
15,15,	22,15,	0,0,	1,7,	
1,8,	1,9,	22,27,	1,10,	
0,0,	0,0,	1,11,	2,7,	
2,8,	2,9,	0,0,	2,10,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	15,15,	
0,0,	0,0,	0,0,	0,0,	
15,22,	0,0,	0,0,	0,0,	
0,0,	15,15,	15,15,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,12,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	2,12,	15,15,	
17,23,	24,28,	0,0,	0,0,	
0,0,	1,13,	0,0,	0,0,	
12,17,	0,0,	0,0,	18,24,	
5,14,	2,13,	1,3,	11,16,	
11,16,	11,16,	11,16,	11,16,	
11,16,	11,16,	11,16,	11,16,	
11,16,	13,18,	20,25,	26,29,	
29,30,	0,0,	0,0,	0,0,	
11,16,	11,16,	11,16,	11,16,	
11,16,	11,16,	11,16,	11,16,	
11,16,	11,16,	11,16,	11,16,	
11,16,	11,16,	11,16,	11,16,	
11,16,	11,16,	11,16,	11,16,	
11,16,	11,16,	11,16,	11,16,	
11,16,	11,16,	0,0,	15,15,	
0,0,	0,0,	11,16,	0,0,	
11,16,	11,16,	11,16,	11,16,	
11,16,	11,16,	11,16,	11,16,	
11,16,	11,16,	11,16,	11,16,	
11,16,	11,16,	11,16,	11,16,	
11,16,	11,16,	11,16,	11,16,	
11,16,	11,16,	11,16,	11,16,	
11,16,	11,16,	14,19,	0,0,	
19,19,	0,0,	21,21,	0,0,	
0,0,	0,0,	0,0,	14,19,	
0,0,	19,19,	0,0,	21,21,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	14,20,	0,0,	
19,20,	0,0,	21,26,	14,19,	
0,0,	19,19,	0,0,	21,21,	
14,19,	14,19,	19,19,	19,19,	
21,21,	21,21,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	14,19,	0,0,	
19,19,	0,0,	21,21,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
14,21,	0,0,	14,19,	0,0,	
19,19,	0,0,	21,21,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-9,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+0,	0,		yyvstop+3,
yycrank+1,	0,		yyvstop+5,
yycrank+2,	0,		yyvstop+7,
yycrank+0,	0,		yyvstop+9,
yycrank+0,	0,		yyvstop+12,
yycrank+0,	0,		yyvstop+15,
yycrank+0,	0,		yyvstop+18,
yycrank+79,	0,		yyvstop+21,
yycrank+10,	yysvec+11,	yyvstop+24,
yycrank+16,	yysvec+11,	yyvstop+27,
yycrank+-201,	0,		0,	
yycrank+-46,	0,		0,	
yycrank+0,	yysvec+11,	yyvstop+30,
yycrank+12,	yysvec+11,	yyvstop+32,
yycrank+11,	yysvec+11,	yyvstop+34,
yycrank+-203,	0,		0,	
yycrank+-13,	yysvec+19,	0,	
yycrank+-205,	0,		0,	
yycrank+-15,	yysvec+15,	0,	
yycrank+0,	yysvec+11,	yyvstop+36,
yycrank+12,	yysvec+11,	yyvstop+39,
yycrank+0,	0,		yyvstop+41,
yycrank+-14,	yysvec+21,	0,	
yycrank+0,	0,		yyvstop+43,
yycrank+0,	yysvec+11,	yyvstop+45,
yycrank+15,	0,		yyvstop+48,
yycrank+0,	0,		yyvstop+50,
0,	0,	0};
struct yywork *yytop = yycrank+330;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,'%' ,01  ,01  ,
01  ,01  ,'*' ,01  ,01  ,01  ,01  ,'/' ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,01  ,01  ,01  ,01  ,'0' ,
01  ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,01  ,01  ,'}' ,01  ,01  ,
0};
char yyextra[] = {
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
