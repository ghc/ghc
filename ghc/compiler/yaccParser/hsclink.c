/* This is the "top-level" file for the *linked-into-the-compiler* parser.
   See also main.c.  (WDP 94/10)
*/

#include <stdio.h>

#include "hspincl.h"
#include "constants.h"
#include "utils.h"

/**********************************************************************
*                                                                     *
*                                                                     *
*     The main program                                                *
*                                                                     *
*                                                                     *
**********************************************************************/

extern long  prog_argc;	
extern char  **prog_argv;

#define MAX_HSP_ARGS 64
long hsp_argc;
char *hsp_argv[MAX_HSP_ARGS];	/* sigh */

tree
hspmain()
{
    int hsp_i, prog_i;

    Lnil = mklnil();	/* The null list -- used in lsing, etc. */
    all = mklnil();	/* This should be the list of all derivable types */

    /* copy the args we're interested in (first char: comma)
	to hsp_argv; arrange to point after the comma!
    */
    hsp_i = 0;
    for (prog_i = 0; prog_i < prog_argc; prog_i++) {
	if (prog_argv[prog_i][0] == ',') {
	    hsp_argv[hsp_i] = &(prog_argv[prog_i][1]);
	    hsp_i++;
	}
    }
    hsp_argc = hsp_i; /* set count */

    process_args(hsp_argc, hsp_argv); /* HACK */

    hash_init();

#ifdef HSP_DEBUG
    fprintf(stderr,"input_file_dir=%s\n",input_file_dir);
#endif

    yyinit();

    if (yyparse() != 0) {
	/* There was a syntax error. */
	printf("\n");
	exit(1);
    }

    return(root);
}
