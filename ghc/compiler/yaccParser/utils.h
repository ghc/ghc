/*
	Utility Definitions.
*/

#ifndef __UTILS_H
#define __UTILS_H

/* stuff from util.c */
extern tree root;
extern list Lnil;
extern list all;

extern BOOLEAN nonstandardFlag;
extern BOOLEAN hashIds;
extern BOOLEAN acceptPrim;
extern BOOLEAN etags;
				  
extern BOOLEAN ignoreSCC;
extern BOOLEAN warnSCC;
				  
extern BOOLEAN implicitPrelude;
extern BOOLEAN ignorePragmas;

extern int minAcceptablePragmaVersion;
extern int maxAcceptablePragmaVersion;
extern int thisIfacePragmaVersion;

extern unsigned hash_table_size;
extern char *input_file_dir;

extern list imports_dirlist;
extern list sys_imports_dirlist;

extern char HiSuffix[];
extern char PreludeHiSuffix[];

void process_args PROTO((int, char **));

/* end of util.c stuff */

list mklcons	PROTO((void *h, list t)); /* if we have PROTO, we have "void *" */
list lapp	PROTO((list l1, void *l2));
list lconc	PROTO((list l1, list l2));
list mktruecase	PROTO((tree t));

#define lsing(l) mklcons(l, Lnil)		/* Singleton Lists */
#define ldub(l1, l2) mklcons(l1, lsing(l2))	/* Two-element Lists */

#define FN fns[icontexts]
#define SAMEFN samefn[icontexts]
#define PREVPATT prevpatt[icontexts]

tree *Rginfun PROTO((struct Sap *));
tree *Rginarg1 PROTO((struct Sap *));
tree *Rginarg2 PROTO((struct Sap *));

#define ginfun(xx) *Rginfun(xx)
#define ginarg1(xx) *Rginarg1(xx)
#define ginarg2(xx) *Rginarg2(xx)

id installid PROTO((char *));		   /* Create a new identifier */
hstring installHstring PROTO((int, char *)); /* Create a new literal string */

id	install_literal PROTO((char *));
char   *id_to_string PROTO((id));

struct infix *infixlookup PROTO((id));

/* partain additions */

char	*xmalloc PROTO((unsigned)); /* just a GNU-style error-checking malloc */
int	 printf  PROTO((const char *, ...));
int	 fprintf PROTO((FILE *, const char *, ...));
/*varies (sun/alpha): int fputc   PROTO((char, FILE *)); */
int	 fputs	 PROTO((const char *, FILE *));
int  	 sscanf	 PROTO((const char *, const char *, ...));
long	 strtol  PROTO((const char *, char **, int));
size_t	 fread	 PROTO((void *, size_t, size_t, FILE *));
int	 fclose  PROTO((FILE *));
int	 isatty	 PROTO((int));
/*extern ???       _filbuf */
/*extern ???  	 _flsbuf */

void	 format_string PROTO((FILE *, unsigned char *, int));
tree	 mkbinop PROTO((char *, tree, tree));
tree	 mkinfixop PROTO((char *, tree, tree));
list	 type2context PROTO((ttype));
pbinding createpat PROTO((list, binding));
void	 process_args PROTO((int, char **));
void	 hash_init PROTO((void));
void	 print_hash_table PROTO((void));
long int hash_index PROTO((id));
void	 yyinit PROTO((void));
int	 yyparse PROTO((void));
int	 yylex PROTO((void));
void	 setyyin PROTO((char *));
void	 yyerror PROTO((char *));
void	 error PROTO((char *));
void	 hsperror PROTO((char *));
void	 enteriscope PROTO((void));
void	 exposeis PROTO((void));
void	 makeinfix PROTO((id, int, int));
int	 nfixes PROTO((void));
long int precedence PROTO((int));
int	 pprecedence PROTO((struct infix *));
int	 pfixity PROTO((struct infix *));
void	 pprogram PROTO((tree));
void 	 hsincindent PROTO((void));
void	 hssetindent PROTO((void));
void	 hsendindent PROTO((void));
void	 hsindentoff PROTO((void));

int	 checkfixity PROTO((char *));
void	 checksamefn PROTO((char *));
void	 checkinpat PROTO((void));

void	 patternOrExpr PROTO((int,tree));
/* the "int" arg says what we want; it is one of: */
#define LEGIT_PATT 1
#define LEGIT_EXPR 2

BOOLEAN	lhs_is_patt PROTO((tree));
tree	function PROTO((tree));
void	extendfn PROTO((binding, binding));
void	precparse PROTO((tree));
void	checkorder PROTO((binding));
void	checkprec PROTO((tree, id, BOOLEAN));
BOOLEAN	isconstr PROTO((char *));
void	setstartlineno PROTO((void));
void	find_module_on_imports_dirlist PROTO((char *, BOOLEAN, char *));
char   *fixop PROTO((int));
char   *fixtype PROTO((int));

/* mattson additions */
char *xstrdup PROTO((char *));	    	  /* Duplicate a string */
char *xstrndup PROTO((char *, unsigned)); /* Duplicate a substring */
char *xrealloc PROTO((char *, unsigned)); /* Re-allocate a string */

#endif /* __UTILS_H */
