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

extern void process_args PROTO((int, char **));

/* end of util.c stuff */

extern list mklcons	PROTO((void *h, list t)); /* if we have PROTO, we have "void *" */
extern list lapp	PROTO((list l1, void *l2));
extern list lconc	PROTO((list l1, list l2));
extern list mktruecase	PROTO((tree t));

#define lsing(l) mklcons(l, Lnil)		/* Singleton Lists */
#define ldub(l1, l2) mklcons(l1, lsing(l2))	/* Two-element Lists */

#define FN fns[icontexts]
#define SAMEFN samefn[icontexts]
#define PREVPATT prevpatt[icontexts]

extern tree *Rginfun PROTO((struct Sap *));
extern tree *Rginarg1 PROTO((struct Sap *));
extern tree *Rginarg2 PROTO((struct Sap *));

#define ginfun(xx) *Rginfun(xx)
#define ginarg1(xx) *Rginarg1(xx)
#define ginarg2(xx) *Rginarg2(xx)

extern id installid PROTO((char *));		   /* Create a new identifier */
extern hstring installHstring PROTO((int, char *)); /* Create a new literal string */

extern id install_literal PROTO((char *));
extern char	*id_to_string PROTO((id));

extern struct infix *infixlookup();

/* partain additions */

extern char	*xmalloc PROTO((unsigned)); /* just a GNU-style error-checking malloc */
extern int	 printf  PROTO((const char *, ...));
extern int	 fprintf PROTO((FILE *, const char *, ...));
/*varies (sun/alpha): extern int	 fputc   PROTO((char, FILE *)); */
extern int	 fputs	 PROTO((const char *, FILE *));
extern int  	 sscanf	 PROTO((const char *, const char *, ...));
extern long	 strtol  PROTO((const char *, char **, int));
extern size_t	 fread	 PROTO((void *, size_t, size_t, FILE *));
extern int	 fclose  PROTO((FILE *));
extern int	 isatty	 PROTO((int));
/*extern ???       _filbuf */
/*extern ???  	 _flsbuf */

extern void	 format_string PROTO((FILE *, unsigned char *, int));
extern tree	 mkbinop PROTO((char *, tree, tree));
extern tree	 mkinfixop PROTO((char *, tree, tree));
extern list	 type2context PROTO((ttype));
extern pbinding  createpat PROTO((list, binding));
extern void	 process_args PROTO((int, char **));
extern void	 hash_init PROTO((void));
extern void	 print_hash_table PROTO((void));
extern long int	 hash_index PROTO((id));
extern void	 yyinit PROTO((void));
extern int	 yyparse PROTO((void));
extern int	 yylex PROTO((void));
extern void	 setyyin PROTO((char *));
extern void	 yyerror PROTO((char *));
extern void	 error PROTO((char *));
extern void	 hsperror PROTO((char *));
extern void	 enteriscope PROTO((void));
extern void	 exposeis PROTO((void));
extern void	 makeinfix PROTO((id, int, int));
extern int	 nfixes PROTO((void));
extern long int	 precedence PROTO((int));
extern int	 pprecedence PROTO((struct infix *));
extern void	 rearrangeprec PROTO((tree, tree));
extern int	 pfixity PROTO((struct infix *));
extern void 	 hsincindent PROTO((void));
extern void	 hssetindent PROTO((void));
extern void	 hsentercontext PROTO((int));
extern void	 hsendindent PROTO((void));
extern void	 hsindentoff PROTO((void));

extern int	 checkfixity PROTO((char *));
extern void	 checksamefn PROTO((char *));
extern void	 checkcontext PROTO((list));
extern void	 checkinpat PROTO((void));
extern void	 checkpatt PROTO((tree));
extern BOOLEAN	 lhs_is_patt PROTO((tree));
extern tree	 function PROTO((tree));
extern void	 checkconap PROTO((tree, tree));
extern void	 extendfn PROTO((binding, binding));
extern void	 precparse PROTO((tree));
extern void	 checkorder PROTO((binding));
extern BOOLEAN	 checkorder2 PROTO((binding, BOOLEAN));
extern BOOLEAN	 checksig PROTO((BOOLEAN, binding));
extern void	 checkprec PROTO((tree, id, BOOLEAN));
extern BOOLEAN	 isconstr PROTO((char *));
extern void	 setstartlineno PROTO((void));
extern void	 pprogram PROTO((tree));
extern void	 who_am_i PROTO((void));
extern void	 new_filename PROTO((char *));
extern int	 Return PROTO((int));

/* mattson additions */
extern char *xstrdup PROTO((char *));	    	/* Duplicate a string */
extern char *xstrndup PROTO((char *, unsigned));/* Duplicate a substring */
extern char *xrealloc PROTO((char *, unsigned));/* Re-allocate a string */

#endif /* __UTILS_H */
