/**********************************************************************
*                                                                     *
*                                                                     *
*      Declarations                                                   *
*                                                                     *
*                                                                     *
**********************************************************************/

#include "hspincl.h"
#include "constants.h"
#include "utils.h"

#define PARSER_VERSION "0.27"

tree root; 		/* The root of the built syntax tree. */
list Lnil;
list all;

BOOLEAN nonstandardFlag = FALSE;  /* Set if non-std Haskell extensions to be used. */
BOOLEAN acceptPrim = FALSE;	  /* Set if Int#, etc., may be used		   */
BOOLEAN haskell1_3Flag = FALSE;	  /* Set if we are doing (proto?) Haskell 1.3	   */
BOOLEAN etags = FALSE;		  /* Set if we're parsing only to produce tags.	   */
BOOLEAN hashIds = FALSE; 	  /* Set if Identifiers should be hashed.          */
				  
BOOLEAN ignoreSCC = TRUE;         /* Set if we ignore/filter scc expressions.      */
BOOLEAN warnSCC = FALSE;          /* Set if we warn about ignored scc expressions. */
				  
BOOLEAN implicitPrelude = TRUE;   /* Set if we implicitly import the Prelude.      */
BOOLEAN ignorePragmas = FALSE;    /* Set if we want to ignore pragmas		   */

/* From time to time, the format of interface files may change.

   So that we don't get gratuitous syntax errors or silently slurp in
   junk info, two things: (a) the compiler injects a "this is a
   version N interface":

	{-# GHC_PRAGMA INTERFACE VERSION <n> #-}

   (b) this parser has a "minimum acceptable version", below which it
   refuses to parse the pragmas (it just considers them as comments).
   It also has a "maximum acceptable version", above which...

   The minimum is so a new parser won't try to grok overly-old
   interfaces; the maximum (usually the current version number when
   the parser was released) is so an old parser will not try to grok
   since-upgraded interfaces.

   If an interface has no INTERFACE VERSION line, it is taken to be
   version 0.
*/
int minAcceptablePragmaVersion = 5;  /* 0.26 or greater ONLY */
int maxAcceptablePragmaVersion = 6;  /* 0.28+ */
int thisIfacePragmaVersion = 0;

static char *input_file_dir; /* The directory where the input file is. */

char HiSuffix[64] = ".hi";		/* can be changed with -h flag */
char PreludeHiSuffix[64] = ".hi";	/* can be changed with -g flag */

/* OLD 95/08: BOOLEAN ExplicitHiSuffixGiven = 0; */
static BOOLEAN verbose = FALSE;		/* Set for verbose messages. */

/* Forward decls */
static void who_am_i PROTO((void));

/**********************************************************************
*                                                                     *
*                                                                     *
*     Utility Functions                                               *
*                                                                     *
*                                                                     *
**********************************************************************/

# include <stdio.h>
# include "constants.h"
# include "hspincl.h"
# include "utils.h"

void
process_args(argc,argv)
  int argc;
  char **argv;
{
    BOOLEAN keep_munging_option = FALSE;

/*OLD: progname = argv[0]; */
    imports_dirlist     = mklnil();
    sys_imports_dirlist = mklnil();

    argc--, argv++;

    while (argc && argv[0][0] == '-') {

	keep_munging_option = TRUE;

	while (keep_munging_option && *++*argv != '\0') {
	    switch(**argv) {

	    /* -I dir */
	    case 'I':
		    imports_dirlist = lapp(imports_dirlist,*argv+1);
		    keep_munging_option = FALSE;
		    break;

	    /* -J dir (for system imports) */
	    case 'J':
		    sys_imports_dirlist = lapp(sys_imports_dirlist,*argv+1);
		    keep_munging_option = FALSE;
		    break;

	    case 'g':
		    strcpy(PreludeHiSuffix, *argv+1);
		    keep_munging_option = FALSE;
		    break;

	    case 'h':
		    strcpy(HiSuffix, *argv+1);
/*OLD 95/08:	    ExplicitHiSuffixGiven = 1; */
		    keep_munging_option = FALSE;
		    break;

	    case 'v':
		    who_am_i(); /* identify myself */
		    verbose = TRUE;
		    break;

	    case 'N':
		    nonstandardFlag = TRUE;
		    acceptPrim = TRUE;
		    break;

	    case '3':
		    haskell1_3Flag = TRUE;
		    break;

	    case 'S':
		    ignoreSCC = FALSE;
		    break;

	    case 'W':
		    warnSCC = TRUE;
		    break;

	    case 'p':
		    ignorePragmas = TRUE;
		    break;

	    case 'P':
		    implicitPrelude = FALSE;
		    break;

	    case 'D':
#ifdef HSP_DEBUG
		    { extern int yydebug;
		      yydebug = 1;
		    }
#endif
		    break;

	    /* -Hn -- Use Hash Table, Size n (if given) */
	    case 'H':
		    hashIds = TRUE;
		    if(*(*argv+1)!= '\0')
		      hash_table_size = atoi(*argv+1);
		    break;
	    case 'E':
		    etags = TRUE;
		    break;
	    }
	}
	argc--, argv++;
    }

    if(argc >= 1 && freopen(argv[0], "r", stdin) == NULL) {
	    fprintf(stderr, "Cannot open %s.\n", argv[0]);
	    exit(1);
    }

    if(argc >= 2 && freopen(argv[1], "w", stdout) == NULL) {
	    fprintf(stderr, "Cannot open %s.\n", argv[1]);
	    exit(1);
    }


    /* By default, imports come from the directory of the source file */
    if ( argc >= 1 ) 
      { 
	char *endchar;

	input_file_dir = xmalloc (strlen(argv[0]) + 1);
	strcpy(input_file_dir, argv[0]);
#ifdef macintosh
	endchar = rindex(input_file_dir, (int) ':');
#else
	endchar = rindex(input_file_dir, (int) '/');
#endif /* ! macintosh */

	if ( endchar == NULL ) 
	  {
	    free(input_file_dir);
	    input_file_dir = ".";
	  } 
	else
	  *endchar = '\0';
      } 

    /* No input file -- imports come from the current directory first */
    else
      input_file_dir = ".";

    imports_dirlist = mklcons( input_file_dir, imports_dirlist );

    if (verbose)
      {
	fprintf(stderr,"Hash Table Contains %d entries\n",hash_table_size);
	if(acceptPrim)
	  fprintf(stderr,"Allowing special syntax for Unboxed Values\n");
      }
}

void
error(s)
  char *s;
{
/*OLD:	fprintf(stderr, "%s: Error %s\n", progname, s); */
	fprintf(stderr, "PARSER: Error %s\n", s);
	exit(1);
}

static void
who_am_i(void)
{
  fprintf(stderr,"Glasgow Haskell parser, version %s\n", PARSER_VERSION);
}

tree
mkbinop(s, l, r)
  char *s;
  tree l, r;
{
	return mkap(mkap(mkident(s), l), r);
}

list
lconc(l1, l2)
  list l1;
  list l2;
{
	list t;

	if (tlist(l1) == lnil)
		return(l2);
	for(t = l1; tlist(ltl(t)) != lnil; t = ltl(t))
		;
	ltl(t) = l2;
	return(l1);
}

list
lapp(list l1, VOID_STAR l2)
{
	list t;

	if (tlist(l1) == lnil)
		return(mklcons(l2, mklnil()));
	for(t = l1; tlist(ltl(t)) != lnil; t = ltl(t))
		;
	ltl(t) = mklcons(l2, mklnil());
	return(l1);
}


/************** Haskell Infix ops, built on mkap ******************/

tree mkinfixop(s, l, r)
  char *s;
  tree l, r;
{
  tree ap = mkap(mkap(mkident(s), l), r);
  ap -> tag = tinfixop;
  return ap;
}

tree *
Rginfun(t)
 struct Sap *t;
{
	if(t -> tag != tinfixop)
		fprintf(stderr, "ginfun: illegal selection; was %d\n", t -> tag);
	return(Rgfun((struct Sap *) (t -> Xgfun)));
}

tree *
Rginarg1(t)
 struct Sap *t;
{
	if(t -> tag != tinfixop)
		fprintf(stderr, "ginarg1: illegal selection; was %d\n", t -> tag);
	return(Rgarg((struct Sap *) (t -> Xgfun)));
}

tree *
Rginarg2(t)
 struct Sap *t;
{
	if(t -> tag != tinfixop)
		fprintf(stderr, "ginarg2: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgarg);
}
