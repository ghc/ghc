#define VERSION 	"24-Jan-94"
#define PROGNAME	"ICount"

#define SHADE

#include <stdio.h>

#include <IHASH.h>
#include <ITYPES.h>
#include <instr.h>
#include <inames.h>

#include <shade.h>
#define TR_REGS
#include <trace.h>
#include <stdtr.h>
#include <trctl.h>

static long long info[NIHASH];

#define STATS_FILE	"ICNT"

/* fwd decls */
void print_results();

#define CHECKPOINT 1000000	/* reporting frequency */
static long countdown = CHECKPOINT;

char	*anal_usage = "";
char	*anal_version = VERSION;

initialize(argc,argv,envp)
  int    argc;
  char **argv, envp;
{
    unsigned i, j;

    /* Setup the trace */
    shade_trctl_trsize(sizeof(Trace));

    shade_trctl_it (IT_ANY, 1, 0, TC_IH);

    /* init table */
    for (j = 0; j < NIHASH; j++)
    	info[j] = 0LL;
}

int analyze(argc,argv,envp)
  int    argc;
  char **argv, envp;
{
    Trace *tr;
    int i;

    for (i = 0; tr = shade_step(); i++) {

	info[tr->tr_ih] += 1LL;

	if (countdown-- < 0) {
	  print_results("Intermediate:");
	  countdown = CHECKPOINT;
	}
    }
    return(0);
}

void
terminate()
{
    print_results("Final:");
}

void
print_results(header)
  char *header;
{
    int i, j;
    static FILE *statf = NULL;

    if ((statf = fopen("ICNT", "w")) == NULL) {
      fprintf(stderr, "Cannot open statistics file ICNT\n");
      exit(1);
    }
    fprintf(statf, "%s\n\n", header);

    for (i = 0; i < NIHASH; i++) {
	fprintf(statf, "%8x: %8ld\n", i, (long) info[i]);
    }

    fclose(statf);
}
