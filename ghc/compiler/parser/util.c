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

#define PARSER_VERSION "1.3-???"

tree root; 		/* The root of the built syntax tree. */
list Lnil;

BOOLEAN nonstandardFlag = FALSE;  /* Set if non-std Haskell extensions to be used. */
BOOLEAN acceptPrim = FALSE;	  /* Set if Int#, etc., may be used		   */
BOOLEAN haskell1_2Flag = FALSE;	  /* Set if we are compiling for 1.2    	   */
BOOLEAN etags = FALSE;		  /* Set if we're parsing only to produce tags.	   */
BOOLEAN hashIds = FALSE; 	  /* Set if Identifiers should be hashed.          */
				  
BOOLEAN ignoreSCC = TRUE;         /* Set if we ignore/filter scc expressions.      */
				  
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

    argc--, argv++;

    while (argc > 0 && argv[0][0] == '-') {

	keep_munging_option = TRUE;

	while (keep_munging_option && *++*argv != '\0') {
	    switch(**argv) {

	    case 'v':
		    who_am_i(); /* identify myself */
		    verbose = TRUE;
		    break;

	    case 'N':
		    nonstandardFlag = TRUE;
		    acceptPrim = TRUE;
		    break;

	    case '2':
		    haskell1_2Flag = TRUE;
		    break;

	    case 'S':
		    ignoreSCC = FALSE;
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

    if (verbose) {
	fprintf(stderr,"Hash Table Contains %d entries\n",hash_table_size);
	if(acceptPrim)
	  fprintf(stderr,"Allowing special syntax for Unboxed Values\n");
    }
}

void
error(s)
  char *s;
{
	fprintf(stderr, "PARSER: Error %s\n", s);
	exit(1);
}

static void
who_am_i(void)
{
  fprintf(stderr,"Glasgow Haskell parser, version %s\n", PARSER_VERSION);
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
