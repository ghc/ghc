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

#define lsing(l) mklcons(l, Lnil)		/* Singleton Lists */
#define ldub(l1, l2) mklcons(l1, lsing(l2))	/* Two-element Lists */

#define FN fns[icontexts]
#define SAMEFN samefn[icontexts]
#define PREVPATT prevpatt[icontexts]

id installid PROTO((char *));		     /* Create a new identifier */
hstring installHstring PROTO((int, char *)); /* Create a new literal string */

id	install_literal PROTO((char *));
char   *id_to_string PROTO((id));

id      qid_to_id PROTO((qid));
char   *qid_to_string PROTO((qid));
char   *qid_to_mod PROTO((qid));	     /* NULL if unqual */
char   *qid_to_pmod PROTO((qid));	     /* "?"  if unqual */
qid	creategid PROTO((long));

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

void	 pprogram PROTO((tree));

void	 format_string PROTO((FILE *, unsigned char *, int));
list	 type2context PROTO((ttype));
pbinding createpat PROTO((pbinding, binding));
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

void	 makeinfix PROTO((id, int, int, id, long, long, id, id, long, long, long, list));
struct infix *infixlookup PROTO((qid));
int	 pprecedence PROTO((struct infix *));
int	 pfixity PROTO((struct infix *));
char *   infixstr PROTO((int));
long     infixint PROTO((int));

void 	 hsincindent PROTO((void));
void	 hssetindent PROTO((void));
void	 hsendindent PROTO((void));
void	 hsindentoff PROTO((void));

int	 checkfixity PROTO((char *));
void	 checksamefn PROTO((qid));
void	 checkinpat PROTO((void));

void	 expORpat PROTO((int,tree));
/* the "int" arg says what we want; it is one of: */
#define LEGIT_PATT 1
#define LEGIT_EXPR 2

BOOLEAN	lhs_is_patt PROTO((tree));
tree	function PROTO((tree));
void	extendfn PROTO((binding, binding));
void	checkorder PROTO((binding));

void	precparse PROTO((tree));
void	checkprec PROTO((tree, qid, BOOLEAN));
void    checkdostmts PROTO((list));
void	checknobangs PROTO((ttype));
void	splittyconapp PROTO((ttype, qid *, list *));

BOOLEAN	isconstr PROTO((char *));
void	setstartlineno PROTO((void));
void	find_module_on_imports_dirlist PROTO((char *, BOOLEAN, char *));

/* mattson additions */
char *xstrdup PROTO((char *));	    	  /* Duplicate a string */
char *xstrndup PROTO((char *, unsigned)); /* Duplicate a substring */
char *xrealloc PROTO((char *, unsigned)); /* Re-allocate a string */

#endif /* __UTILS_H */
