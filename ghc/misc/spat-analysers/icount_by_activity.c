#define VERSION 	"24-Jan-95"
#define PROGNAME	"ICountByActivity"

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

int shade_run(Trace *, int);

#define DO_SPAT_PROFILING
#define __STG_USING_ULLONG__
#include "stgdefs.h"	/* GHC ticky-counting stuff */
#define ACTIVITY_REG I_REG_g5
#define SpA_REG	     I_REG_i0
#define SpB_REG	     I_REG_i2
#define Hp_REG	     I_REG_i4
#define RET_REG	     I_REG_l0
#define NODE_REG     I_REG_l1
#define INFO_REG     I_REG_l2
#define R3_REG	     I_REG_l3
#define R7_REG       I_REG_l7

/* Activity register and current activity */

#define EACT_CALL	(ACTIVITIES+0)
#define EACT_STKADJ	(ACTIVITIES+1)
#define EACT_ASTK	(ACTIVITIES+2)
#define EACT_BSTK	(ACTIVITIES+3)
#define EACT_RETREG	(ACTIVITIES+4)
#define EACT_ARGREGS	(ACTIVITIES+5)

#define EACT_TAILCALL	(ACT_TAILCALL - ACT_BASE) /* use the TAILCALL slot */
#define EACT_OVERHEAD	(ACT_OVERHEAD - ACT_BASE) /* only used herein */

#define EXTRA_ACTIVITIES 6

#define TOTAL_ACTIVITIES (ACTIVITIES+EXTRA_ACTIVITIES)

static ullong info[TOTAL_ACTIVITIES][NIHASH];
/*static ullong annulled_insns = 0;*/

#define STATS_FILE	"ICNT_BY_ACTIVITY"

/* fwd decls */
void print_results(char *);
void fprintf_ullong(FILE *, ullong);

#define CHECKPOINT (1024*1024)	/* reporting frequency */
static long countdown = CHECKPOINT;

char	*anal_usage = "";
char	*anal_version = VERSION;

void
initialize(argc,argv,envp)
  int    argc;
  char **argv, envp;
{
    unsigned i, j;

    /* Setup the trace */
    shade_trctl_trsize(sizeof(Trace));

    shade_trctl_it (IT_ANY,   1, 0, TC_I | TC_IH);
    shade_trctl_ih (IH_OR,    1, 0, TC_I | TC_IH | TC_RD);
    shade_trctl_ih (IH_ADD,   1, 0, TC_I | TC_IH | TC_RD | TC_RS1);
    shade_trctl_ih (IH_SETHI, 1, 0, TC_I | TC_IH | TC_RD);
    shade_trctl_it (IT_LOAD,  1, 0, TC_I | TC_IH | TC_RD | TC_RS1);
    shade_trctl_it (IT_ILOAD, 1, 0, TC_I | TC_IH | TC_RD | TC_RS1);
    shade_trctl_it (IT_STORE, 1, 0, TC_I | TC_IH | TC_RD | TC_RS1);
    shade_trctl_it (IT_ISTORE,1, 0, TC_I | TC_IH | TC_RD | TC_RS1);
	/* trace all non-annulled instructions (... 1, 0, ...); For
	   them, we want the instruction text (TC_I), and the hashed
	   opcode (TC_IH).  For "or" instructions, we also want the
	   contents the destination register that was written into
	   (TC_RD).  Etc.
	*/

    /* init table */
    for (i = 0; i < TOTAL_ACTIVITIES; i++)
	for (j = 0; j < NIHASH; j++)
	    info[i][j] = 0LL;
}

int analyze(argc,argv,envp)
  int    argc;
  char **argv, envp;
{
    Trace *tr;
    ullong i;
    uint16 ih;
    int32  rd, rs1;

    unsigned activity = (ACT_UNKNOWN - ACT_BASE);
    ullong pending_sethi = 0LL;
    ullong pending_or    = 0LL;
    ullong activity_chgs = 0LL;
    int acctd_for;
#define ACCT_FOR() acctd_for = 1

    for (i = 0LL; tr = shade_step(); i += 1LL) {
	acctd_for = 0;
	ih = tr->tr_ih;

	if ( ih == IH_OR && tr->tr_i.i_rd == ACTIVITY_REG) {
	    rd = tr->tr_rd;

	    info[EACT_TAILCALL][IH_OR] += pending_or;
	    if ( pending_sethi ) {
		fprintf(stderr, "pending_sethi still set!\n");
	    }

	    if (activity == (ACT_GC - ACT_BASE)) { /* only GC_STOP will stop it */
		if (rd == ACT_GC_STOP) {
		    activity = ACT_UNKNOWN - ACT_BASE;
		    info[EACT_OVERHEAD][IH_OR] += 1LL;
		    ACCT_FOR();
		} else {
		    info[activity][IH_OR] += 1LL;
		    ACCT_FOR();
		}
	    } else {
		if (rd < ACT_BASE || rd >= (ACT_BASE+ACTIVITIES)) {
		    info[activity][IH_OR] += 1LL;
		    ACCT_FOR();
		} else {
		    activity = rd - ACT_BASE; /* reset! */
		    info[EACT_OVERHEAD][IH_OR] += 1LL;
		    ACCT_FOR();
		}
	    }
	    activity_chgs += 1LL;
	    pending_sethi  = 0LL;
	    pending_or	   = 0LL;
	    /* reset other things? */

	} else if ( activity != EACT_TAILCALL ) { /* ordinary instruction */
	    info[activity][ih] += 1LL;
	    ACCT_FOR();

	} else { /* TAILCALLing */
/*	    fprintf(stderr, "op=%d\n", ih); */

	    switch (ih) {
	      case IH_SETHI:
/*			if ( pending_sethi ) {
			    fprintf(stderr, "pending_sethi already set!\n");
			}
*/			pending_sethi += 1LL;
			ACCT_FOR();
			break;
	      case IH_JMPL:
	      case IH_CALL:
	      case IH_NOP:
			info[EACT_CALL][ih]       += 1LL;
			info[EACT_CALL][IH_SETHI] += pending_sethi; /* really mystery? */
			info[EACT_CALL][IH_OR]    += pending_or;    /* ditto? */
			pending_sethi = 0LL;
			pending_or    = 0LL;
			ACCT_FOR();
			break;
	    
	      case IH_ADD:
	      case IH_ADDCC:
	      case IH_SUB:
	      case IH_SUBCC:
			rd  = tr->tr_i.i_rd;
			rs1 = tr->tr_i.i_rs1;
			if ( rd == NODE_REG || rd == INFO_REG ) {
			    info[EACT_CALL][ih]       += 1LL;
			    info[EACT_CALL][IH_SETHI] += pending_sethi;
			    info[EACT_CALL][IH_OR]    += pending_or;
			    pending_sethi = 0LL;
			    pending_or    = 0LL;
			    ACCT_FOR();

			} else if (rd >= R3_REG && rd <= R7_REG) {
			    info[EACT_ARGREGS][ih]       += 1LL;
			    info[EACT_ARGREGS][IH_SETHI] += pending_sethi;
			    info[EACT_ARGREGS][IH_OR]    += pending_or;
			    pending_sethi = 0LL;
			    pending_or    = 0LL;
			    ACCT_FOR();

			} else {
			    info[EACT_TAILCALL][IH_SETHI] += pending_sethi;
			    info[EACT_TAILCALL][IH_OR]    += pending_or;
			    pending_sethi = 0LL;
			    pending_or    = 0LL;

			    if ( (rd == SpA_REG && rs1 == SpA_REG)
			      || (rd == SpB_REG && rs1 == SpB_REG) ) {
				info[EACT_STKADJ][ih] += 1LL;
				ACCT_FOR();

			    } else if ( rd >= I_REG_o0 && rd <= I_REG_o7 ) {
				info[EACT_TAILCALL][ih] += 1LL;
				ACCT_FOR();

			    } else if ( rd == I_REG_g0
				    && rs1 >= I_REG_o0 && rs1 <= I_REG_o7 ) {
				info[EACT_TAILCALL][ih] += 1LL;
				ACCT_FOR();

			    } else if ( rd == I_REG_g3 && rs1 == I_REG_g3 ) {
				info[EACT_TAILCALL][ih] += 1LL;
				ACCT_FOR();

			    } else {
			      fprintf(stderr, "IH_ADD: mystery op (%d) rd=%d rs1=%d\n",
				    ih, rd, rs1);
			    }
			}
			break;

	      case IH_OR:
	      case IH_ORCC:
			rd  = tr->tr_i.i_rd;
			if ( rd == RET_REG ) {
			    info[EACT_RETREG][ih]       += 1LL + pending_or;
			    info[EACT_RETREG][IH_SETHI] += pending_sethi;
			    pending_sethi = 0LL;
			    pending_or    = 0LL;
			    ACCT_FOR();

			} else if ( rd == NODE_REG || rd == INFO_REG ) {
			    info[EACT_CALL][ih]       += 1LL + pending_or;
			    info[EACT_CALL][IH_SETHI] += pending_sethi;
			    pending_sethi = 0LL;
			    pending_or    = 0LL;
			    ACCT_FOR();

			} else {
			    pending_or += 1LL;
			    ACCT_FOR();
			}
			break;

	      case IH_LD:
	      case IH_LDUB: /* ??? */
	      case IH_ST:
			rs1 = tr->tr_i.i_rs1;
			if ( rs1 == SpA_REG ) {
			    info[EACT_ASTK][ih]       += 1LL;
			    info[EACT_ASTK][IH_SETHI] += pending_sethi;
			    info[EACT_ASTK][IH_OR]    += pending_or;
			    pending_sethi = 0LL;
			    pending_or    = 0LL;
			    ACCT_FOR();

			} else if ( rs1 == SpB_REG ) {
			    info[EACT_BSTK][ih]       += 1LL;
			    info[EACT_BSTK][IH_SETHI] += pending_sethi;
			    info[EACT_BSTK][IH_OR]    += pending_or;
			    pending_sethi = 0LL;
			    pending_or    = 0LL;
			    ACCT_FOR();

			} else if ( rs1 == NODE_REG ) {
			    info[EACT_CALL][ih]       += 1LL;
			    info[EACT_CALL][IH_SETHI] += pending_sethi;
			    info[EACT_CALL][IH_OR]    += pending_or;
			    pending_sethi = 0LL;
			    pending_or    = 0LL;
			    ACCT_FOR();

			} else { /* random ld/st */
			    info[EACT_TAILCALL][ih]       += 1LL;
			    info[EACT_TAILCALL][IH_SETHI] += pending_sethi;
			    info[EACT_TAILCALL][IH_OR]    += pending_or;
			    pending_sethi = 0LL;
			    pending_or    = 0LL;
			    ACCT_FOR();
			}
			break;

	      case IH_AND:	/* ??? */
	      case IH_BA:	/* ??? */
	      case IH_BAA:
	      case IH_BCC:
	      case IH_BCS:
	      case IH_BE:
	      case IH_BGE:
	      case IH_BL:
	      case IH_BLA:
	      case IH_BLEU:
	      case IH_BNE:
	      case IH_SLL:
	      case IH_SRL:
	      case IH_XOR:
			info[EACT_TAILCALL][ih]	  += 1LL;
			info[EACT_TAILCALL][IH_SETHI] += pending_sethi;
			info[EACT_TAILCALL][IH_OR]    += pending_or;
			pending_sethi = 0LL;
			pending_or    = 0LL;
			ACCT_FOR();
			break;

	      default:
			fprintf(stderr, "mystery TAIL op = %d\n", ih);
			break;
	    }
    	}

	if (countdown-- < 0) {
	  print_results("Intermediate:");
	  countdown = CHECKPOINT;
	}
	if ( ! acctd_for ) {
	  fprintf(stderr, "insn op=%d not acctd for!\n", ih);
	}
    }
    fprintf(stderr,"\n");
    fprintf_ullong(stderr,i);
    fprintf(stderr," iterations; ");
    fprintf_ullong(stderr,activity_chgs);
    fprintf(stderr," activity changes\n");
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
    long total_slots = 0;
    ullong total_instrs = 0;
    static FILE *statf = NULL;

/*  fprintf(stderr, "Printing %s\n", header); */

    unlink(STATS_FILE);
    if ((statf = fopen(STATS_FILE, "w")) == NULL) {
      fprintf(stderr, "Cannot open statistics file %s\n",STATS_FILE);
      exit(1);
    }
    fprintf(statf, "%s\n\n", header);
/*  fprintf(statf, "annulled insns = ");
    fprintf_ullong(statf, annulled_insns);
*/  fprintf(statf, "\n\n");

    for (i = 0; i < NIHASH; i++) {
	fprintf(statf, "%8d:", i);
	for (j = 0; j < TOTAL_ACTIVITIES; j++) {
	    fprintf(statf, " ");
	    fprintf_ullong(statf, info[j][i]);
	    total_slots++;
	    total_instrs += info[j][i];
	}
	fprintf(statf, "\n");
    }
    fprintf(statf, "total slots=%ld, total instructions=", total_slots);
    fprintf_ullong(statf, total_instrs);
    fprintf(statf, "\n");

    fclose(statf);
}

void
fprintf_ullong(FILE *filep, ullong x)
{
    if (x < (ullong)1000) 
	fprintf(filep, "%ld", (I_)x);
    else if (x < (ullong)1000000)
	fprintf(filep, "%ld%3.3ld",
		(I_)((x)/(ullong)1000),
		(I_)((x)%(ullong)1000));
    else if (x < (ullong)1000000000)
	fprintf(filep, "%ld%3.3ld%3.3ld",
		(I_)((x)/(ullong)1000000),
		(I_)((x)/(ullong)1000%(ullong)1000),
		(I_)((x)%(ullong)1000));
    else
	fprintf(filep, "%ld%3.3ld%3.3ld%3.3ld",
		(I_)((x)/(ullong)1000000000),
		(I_)((x)/(ullong)1000000%(ullong)1000),
		(I_)((x)/(ullong)1000%(ullong)1000), 
		(I_)((x)%(ullong)1000));
}
