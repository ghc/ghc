#include <stdio.h>
#include <sparc.h>

#include "StgRegAddrs.h"

#define CHECKPOINT 1000000	/* reporting frequency */
static int countdown = CHECKPOINT;

struct regcount {
    char *str;
    int load;
    int store;
} info[] = {
  {"SpA", 0, 0},
  {"SpB", 0, 0},
  {"Hp", 0, 0},
  {"HpLim", 0, 0},
  {"SuA", 0, 0},
  {"SuB", 0, 0},
  {"UpdReg", 0, 0},
  {"RetVecReg", 0, 0},
  {"TagReg", 0, 0},
  {"Ret1", 0, 0},
  {"Ret2", 0, 0},
  {"Ret3", 0, 0},
  {"Ret4", 0, 0},
  {"Ret5", 0, 0},
  {"Ret6", 0, 0},
  {"Ret7", 0, 0},
  {"Ret8", 0, 0},
  {0, 0, 0}
};    

void
printregs(msg)
char *msg;
{
    FILE *output;
    int i;
    if ((output = fopen("REGSTATS", "w")) == 0)
	syserr("cannot open statistics file REGSTATS\n");

    fprintf(output, "%s\n", msg);
    for (i = 0; info[i].str; i++) {
	fprintf(output, "%-16.16s %8d %8d\n",
		info[i].str, info[i].load, info[i].store);
    }
    fclose(output);
}

#define RECORD(i)               \
    if ( (OP3(t->iw)&014) == 004) { \
        info[i].store++;        \
    } else {                    \
        info[i].load++;         \
    }                           \
    /* fprintf(stderr, "%s\n", info[i].str); */ \
    break

void
analyze (t, tend)
	TRACE	*t, *tend;
{
    countdown -= tend-t;

    for (; t < tend; t++) {
	if (OP(t->iw) == 3 &&	/* Load/store; (OP3(t->iw)&014)==004) => store */
	    !(t->flags & ANNULLED)) {
	    unsigned a = (unsigned)t->ea;
	    switch (a) {
	      case SpA:
		RECORD(0);
	      case SpB:
		RECORD(1);
	      case Hp:
		RECORD(2);
	      case HpLim:
		RECORD(3);
	      case SuA:
		RECORD(4);
	      case SuB:
		RECORD(5);
	      case UpdReg:
		RECORD(6);
	      case RetVecReg:
		RECORD(7);
	      case TagReg:
		RECORD(8);
	      case Ret1:
		RECORD(9);
	      case Ret2:
		RECORD(10);
	      case Ret3:
		RECORD(11);
	      case Ret4:
		RECORD(12);
	      case Ret5:
		RECORD(13);
	      case Ret6:
		RECORD(14);
	      case Ret7:
		RECORD(15);
	      case Ret8:
		RECORD(16);
	      deafualt:
		break;
	    }
	}
    }

    if (countdown <= 0) {
	printregs("Intermediate:");
	countdown = CHECKPOINT;
    }
}

void
terminate()
{
    printregs("Final:");
}
