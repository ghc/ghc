#ifdef __STDC__
#define PROTO(x)	x
#else
#define PROTO(x)	()
#endif

#include <stdio.h>
#include "id.h"
#include "tree.h"

#include "funs.h"

FILE *fh, *fc, *fhs;

tree root; /* The root of the built syntax tree. */

main(argc, argv)
    int argc;
    char **argv;
{
	int i = 0;

	if(argc != 2) {
		printf("Missing input file.\n");
		exit(1);
	}

	if(freopen(argv[1], "r", stdin) == NULL) {
		fprintf(stderr, "Cannot open %s.\n", argv[1]);
		exit(1);
	}

	while(argv[1][i+1] != 0)
		i++;
	if(! (argv[1][i-3] == '.' &&
	      argv[1][i-2] == 'u' &&
	      argv[1][i-1] == 'g' &&
	      argv[1][i]   == 'n')) {
		fprintf(stderr, "Not a .ugn file\n");
		exit(1);
	}

	argv[1][i-2] = 'c';
	argv[1][i-1] = '\0';
	fc = fopen(argv[1], "w"); /* .c file */
	argv[1][i-2] = 'h';
	fh = fopen(argv[1], "w"); /* .h file */
	argv[1][i-1] = 's';
	argv[1][i]   = '\0';
	fhs = fopen(argv[1], "w"); /* .hs file */
	argv[1][i-1] = '\0';

	if(yyparse() == 0) {
		/* No syntax errors. */

		fprintf(fc, "#include \"%s\"\n", argv[1]);
		gentype(root);
		exit(0);

	} else {
		/* There was a syntax error. */
/* ToDo: this stuff is now *WWRROONNGG* (WDP 94/10) */
		unlink(argv[1][i]);
		argv[i][i] = 'c';
		unlink(argv[1][i]);
		fprintf(stderr, "Nothing generated.\n");
		exit(1);
	}
}

void
gentype(t)
   tree t;
{
	ge_typdef(t); /* Generate the .h - file. */

	/* Generate the struct definitions. */
/*partain:moved		gs_typlist(gtdeflist(t), gtid(t));
*/
	/* Generate constructors and selectors. */
	g_consels(gtdeflist(t), gtid(t));

	fprintf(fh, "#endif\n"); /* for .h multi-slurp protector */
	
	/* Generate Haskell reader */
	gen_hs_reader(gtid(t), gtdeflist(t));
}
