/* -----------------------------------------------------------------------------
 *
 * (c) The AQUA project, Glasgow University, 1992-1997
 * (c) The GHC Team, 1998-1999
 *
 * Ticky-ticky profiling
 *-------------------------------------------------------------------------- */

#define TICKY_C                 /* define those variables */
#include "rts/PosixSource.h"
#include "Rts.h"

#include "eventlog/EventLog.h"

/* Catch-all top-level counter struct.  Allocations from CAFs will go
 * here.
 */
StgEntCounter top_ct
        = { 0, 0, 0,
            "TOP", "", "",NULL,
            0, 0, NULL };

/* Data structure used in ``registering'' one of these counters. */

StgEntCounter *ticky_entry_ctrs = NULL; /* root of list of them */

/* We want Haskell code compiled with -ticky to be linkable with any
 * version of the RTS, so we have to make sure all the symbols that
 * ticky-compiled code may refer to are defined by every RTS. (#3439)
 * Hence the #if defined(is) here, rather than up above.
 */
#if defined(TICKY_TICKY)

#include "Ticky.h"

/* -----------------------------------------------------------------------------
   Print out all the counters
   -------------------------------------------------------------------------- */

static void printRegisteredCounterInfo (FILE *); /* fwd decl */

#define INTAVG(a,b) ((b == 0) ? 0.0 : ((double) (a) / (double) (b)))
#define PC(a)       (100.0 * a)

#define AVG(thing) \
        StgDouble avg##thing  = INTAVG(tot##thing,ctr##thing)

void
PrintTickyInfo(void)
{
  if (RtsFlags.TraceFlags.ticky) {
      barf("Ticky eventlog output can't be used with +RTS -r<file>");
  }

  unsigned long i;

  unsigned long tot_thk_enters = ENT_STATIC_THK_MANY_ctr + ENT_DYN_THK_MANY_ctr
                               + ENT_STATIC_THK_SINGLE_ctr + ENT_DYN_THK_SINGLE_ctr;
  unsigned long tot_con_enters = ENT_STATIC_CON_ctr + ENT_DYN_CON_ctr;

  unsigned long tot_fun_direct_enters = ENT_STATIC_FUN_DIRECT_ctr + ENT_DYN_FUN_DIRECT_ctr;
  unsigned long tot_ind_enters = ENT_STATIC_IND_ctr + ENT_DYN_IND_ctr;

  unsigned long tot_known_calls =
      KNOWN_CALL_ctr + KNOWN_CALL_TOO_FEW_ARGS_ctr +
      + KNOWN_CALL_EXTRA_ARGS_ctr;
  unsigned long tot_tail_calls =
      UNKNOWN_CALL_ctr + tot_known_calls;

  unsigned long tot_enters =
      tot_con_enters + tot_fun_direct_enters +
        tot_ind_enters + ENT_PERM_IND_ctr + ENT_PAP_ctr + tot_thk_enters;
  unsigned long jump_direct_enters =
        tot_enters - ENT_VIA_NODE_ctr;


  unsigned long tot_returns =
      RET_NEW_ctr + RET_OLD_ctr + RET_UNBOXED_TUP_ctr;

  unsigned long tot_returns_of_new = RET_NEW_ctr;

  unsigned long con_updates = UPD_CON_IN_NEW_ctr + UPD_CON_IN_PLACE_ctr;
  unsigned long pap_updates = UPD_PAP_IN_NEW_ctr + UPD_PAP_IN_PLACE_ctr;

  unsigned long tot_updates = UPD_SQUEEZED_ctr + pap_updates + con_updates;

  unsigned long tot_new_updates   = UPD_NEW_IND_ctr + UPD_NEW_PERM_IND_ctr;
  unsigned long tot_old_updates   = UPD_OLD_IND_ctr + UPD_OLD_PERM_IND_ctr;
  unsigned long tot_gengc_updates = tot_new_updates + tot_old_updates;

  // Number of times tag inference predicted tagged/untagged correctly
  // allowing us to skip a tag check (when ticky is disabled)
  unsigned long tot_tag_preds = TAG_UNTAGGED_pred + TAG_TAGGED_pred;

  FILE *tf = RtsFlags.TickyFlags.tickyFile;

  /* If tf = NULL, that means the user passed in stderr for the ticky stats
     file. According to a comment in RtsFlags.c, this means to use
     debugBelch to print out messages. But this function prints out a lot
     of stuff so in order to avoid changing a lot of code, we just dump
     the same output to stderr (for now). */
  if( tf == NULL )
    tf = stderr;

  fprintf(tf,"\nSTACK USAGE:\n"); /* NB: some bits are direction sensitive */


  fprintf(tf,"\nENTERS: %lu  of which %lu (%.1f%%) direct to the entry code\n\t\t  [the rest indirected via Node's info ptr]\n",
        tot_enters,
        jump_direct_enters,
        PC(INTAVG(jump_direct_enters,tot_enters)));
  fprintf(tf,"%11lu (%5.1f%%) thunks\n",
        tot_thk_enters,
        PC(INTAVG(tot_thk_enters,tot_enters)));
  fprintf(tf, "%11lu (%5.1f%%) data values\n",
        tot_con_enters,
        PC(INTAVG(tot_con_enters,tot_enters)));
  fprintf(tf, "%11lu (%5.1f%%) normal indirections\n",
        tot_ind_enters,
        PC(INTAVG(tot_ind_enters,tot_enters)));
  fprintf(tf,"%11" FMT_Int " (%5.1f%%) permanent indirections\n",
        ENT_PERM_IND_ctr,
        PC(INTAVG(ENT_PERM_IND_ctr,tot_enters)));


  fprintf(tf, "\nFUNCTION ENTRIES: %lu\n", tot_fun_direct_enters);

  fprintf(tf, "\nTAIL CALLS: %lu, of which %lu (%.lf%%) were to known functions\n",
          tot_tail_calls, tot_known_calls,
          PC(INTAVG(tot_known_calls,tot_tail_calls)));

  fprintf(tf, "\nSLOW APPLICATIONS: %" FMT_Int " evaluated, %" FMT_Int " unevaluated\n",
          SLOW_CALL_ctr, SLOW_CALL_UNEVALD_ctr);
  fprintf(tf, "\n");
  fprintf(tf, "         Too few args   Correct args   Too many args\n");
  fprintf(tf, "   FUN     %8" FMT_Int "       %8" FMT_Int "        %8" FMT_Int "\n",
          SLOW_CALL_FUN_TOO_FEW_ctr, SLOW_CALL_FUN_CORRECT_ctr, SLOW_CALL_FUN_TOO_MANY_ctr);
  fprintf(tf, "   PAP     %8" FMT_Int "       %8" FMT_Int "        %8" FMT_Int "\n",
          SLOW_CALL_PAP_TOO_FEW_ctr, SLOW_CALL_PAP_CORRECT_ctr, SLOW_CALL_PAP_TOO_MANY_ctr);
  fprintf(tf, "\n");

  fprintf(tf, "\nRETURNS: %lu\n", tot_returns);
  fprintf(tf, "%11lu (%5.1f%%) from entering a new constructor\n\t\t  [the rest from entering an existing constructor]\n",
        tot_returns_of_new,
        PC(INTAVG(tot_returns_of_new,tot_returns)));


  fprintf(tf, "\nRET_NEW:         %11" FMT_Int ": ", RET_NEW_ctr);
  for (i = 0; i < TICKY_BIN_COUNT; i++) {
    fprintf(tf, "%5.1f%%", PC(INTAVG(RET_NEW_hst[i], RET_NEW_ctr)));
  }
  fprintf(tf, "\n");

  fprintf(tf, "RET_OLD:         %11" FMT_Int ": ", RET_OLD_ctr);
  for (i = 0; i < TICKY_BIN_COUNT; i++) {
    fprintf(tf, "%5.1f%%", PC(INTAVG(RET_OLD_hst[i], RET_OLD_ctr)));
  }
  fprintf(tf, "\n");

  fprintf(tf, "RET_UNBOXED_TUP: %11" FMT_Int ": ", RET_UNBOXED_TUP_ctr);
  for (i = 0; i < TICKY_BIN_COUNT; i++) {
    fprintf(tf, "%5.1f%%", PC(INTAVG(RET_UNBOXED_TUP_hst[i],
                           RET_UNBOXED_TUP_ctr)));
  }
  fprintf(tf, "\n");

  fprintf(tf,"\nUPDATE FRAMES: %" FMT_Int " (%" FMT_Int " omitted from thunks)",
        UPDF_PUSHED_ctr,
        UPDF_OMITTED_ctr);

  fprintf(tf,"\nCATCH FRAMES:  %" FMT_Int "", CATCHF_PUSHED_ctr);

  if (UPDF_RCC_PUSHED_ctr != 0) {
     fprintf(tf,"%11" FMT_Int " restore cost centre frames (%" FMT_Int " omitted)\n",
        UPDF_RCC_PUSHED_ctr,
        UPDF_RCC_OMITTED_ctr);
  }

  fprintf(tf,"\nUPDATES: %ld\n", tot_updates);
  fprintf(tf, "%11lu (%5.1f%%) data values\n\t\t  [%" FMT_Int " in place, %" FMT_Int " allocated new space]\n",
        con_updates,
        PC(INTAVG(con_updates,tot_updates)),
        UPD_CON_IN_PLACE_ctr, UPD_CON_IN_NEW_ctr);

  fprintf(tf, "%11lu (%5.1f%%) partial applications\n\t\t  [%" FMT_Int " in place, %" FMT_Int " allocated new space]\n",
        pap_updates,
        PC(INTAVG(pap_updates,tot_updates)),
        UPD_PAP_IN_PLACE_ctr, UPD_PAP_IN_NEW_ctr);

  fprintf(tf,"%11" FMT_Int " (%5.1f%%) updates by squeezing\n",
        UPD_SQUEEZED_ctr,
        PC(INTAVG(UPD_SQUEEZED_ctr, tot_updates)));

  if (tot_gengc_updates != 0) {
      fprintf(tf, "\nNEW GEN UPDATES: %9lu (%5.1f%%)\n",
              tot_new_updates,
              PC(INTAVG(tot_new_updates,tot_gengc_updates)));
      fprintf(tf, "OLD GEN UPDATES: %9lu (%5.1f%%)\n",
              tot_old_updates,
              PC(INTAVG(tot_old_updates,tot_gengc_updates)));
  }

  if (tot_tag_preds != 0) {
      fprintf(tf, "\nTOTAL TAG PREDICTIONS MADE: %9" FMT_Word64 " \n",
              (StgWord64) tot_tag_preds);
      fprintf(tf, "TAGGED PREDICTIONS HIT:     %9" FMT_Word64 " \n",
              (StgWord64) TAG_TAGGED_pred);
      fprintf(tf, "UNTAGGED PREDICTIONS HIT:   %9" FMT_Word64 " \n",
              (StgWord64) (TAG_UNTAGGED_pred - TAG_UNTAGGED_miss));
      fprintf(tf, "UNTAGGED PREDICTIONS MISS:  %9" FMT_Word64 " \n",
              (StgWord64) TAG_UNTAGGED_miss);
  }

  printRegisteredCounterInfo(tf);

  fprintf(tf,"\n**************************************************\n");

  /* here, we print out all the raw numbers; these are really
    more useful when we want to snag them for subsequent
    rdb-etc processing. WDP 95/11
  */

#define PR_CTR(ctr) \
  do { fprintf(tf,"%11" FMT_Int " " #ctr "\n", ctr); } while(0)

/* COND_PR_CTR takes a boolean; if false then msg is the printname rather than ctr */
#define COND_PR_CTR(ctr,b,msg) \
    if (b) { fprintf(tf,"%11" FMT_Int " " #ctr "\n", ctr); } else { fprintf(tf,"%11" FMT_Int " " msg "\n", ctr); }

#define PR_HST(hst,i) \
  do { fprintf(tf,"%11" FMT_Int " " #hst "_" #i "\n", hst[i]); } while(0)

  ALLOC_HEAP_ctr = (StgInt)ALLOC_HEAP_ctr + (StgInt)ALLOC_RTS_ctr;
  ALLOC_HEAP_tot = (StgInt)ALLOC_HEAP_tot + (StgInt)ALLOC_RTS_tot;

  PR_CTR(ALLOC_HEAP_ctr);
  PR_CTR(ALLOC_HEAP_tot);

  PR_CTR(HEAP_CHK_ctr);
  PR_CTR(STK_CHK_ctr);

  PR_CTR(ALLOC_RTS_ctr);
  PR_CTR(ALLOC_RTS_tot);

  PR_CTR(ALLOC_FUN_ctr);
  PR_CTR(ALLOC_FUN_gds);

  PR_CTR(ALLOC_PAP_ctr);
  PR_CTR(ALLOC_PAP_adm);
  PR_CTR(ALLOC_PAP_gds);

  PR_CTR(ALLOC_UP_THK_ctr);
  PR_CTR(ALLOC_SE_THK_ctr);
  PR_CTR(ALLOC_THK_gds);

  PR_CTR(ALLOC_CON_ctr);
  PR_CTR(ALLOC_CON_gds);

  PR_CTR(ALLOC_PRIM_ctr);
  PR_CTR(ALLOC_PRIM_gds);
  PR_CTR(ALLOC_PRIM_slp);

  PR_CTR(ALLOC_TSO_ctr);
  PR_CTR(ALLOC_TSO_tot);

  PR_CTR(ALLOC_STACK_ctr);
  PR_CTR(ALLOC_STACK_tot);

  PR_CTR(ENT_VIA_NODE_ctr);
  PR_CTR(ENT_STATIC_CON_ctr);
  PR_CTR(ENT_DYN_CON_ctr);
  PR_CTR(ENT_STATIC_FUN_DIRECT_ctr);
  PR_CTR(ENT_DYN_FUN_DIRECT_ctr);
  PR_CTR(ENT_LNE_ctr);
  PR_CTR(ENT_STATIC_IND_ctr);
  PR_CTR(ENT_DYN_IND_ctr);

/* The counters ENT_PERM_IND and UPD_{NEW,OLD}_PERM_IND are not dumped
 * at the end of execution unless update squeezing is turned off (+RTS
 * -Z =RtsFlags.GcFlags.squeezeUpdFrames), as they will be wrong
 * otherwise.  Why?  Because for each update frame squeezed out, we
 * count an UPD_NEW_PERM_IND *at GC time* (i.e., too early).  And
 * further, when we enter the closure that has been updated, we count
 * the ENT_PERM_IND, but we then enter the PERM_IND that was built for
 * the next update frame below, and so on down the chain until we
 * finally reach the value.  Thus we count many new ENT_PERM_INDs too
 * early.
 *
 * This of course refers to the -ticky version that uses PERM_INDs to
 * determine the number of closures entered 0/1/>1.  KSW 1999-04.  */
  COND_PR_CTR(ENT_PERM_IND_ctr,RtsFlags.GcFlags.squeezeUpdFrames == false,"ENT_PERM_IND_ctr requires +RTS -Z");

  PR_CTR(ENT_AP_ctr);
  PR_CTR(ENT_PAP_ctr);
  PR_CTR(ENT_AP_STACK_ctr);
  PR_CTR(ENT_CONTINUATION_ctr);
  PR_CTR(ENT_BH_ctr);
  PR_CTR(ENT_STATIC_THK_SINGLE_ctr);
  PR_CTR(ENT_STATIC_THK_MANY_ctr);
  PR_CTR(ENT_DYN_THK_SINGLE_ctr);
  PR_CTR(ENT_DYN_THK_MANY_ctr);
  PR_CTR(UPD_CAF_BH_UPDATABLE_ctr);
  PR_CTR(UPD_CAF_BH_SINGLE_ENTRY_ctr);

  PR_CTR(SLOW_CALL_fast_v16_ctr);
  PR_CTR(SLOW_CALL_fast_v_ctr);
  PR_CTR(SLOW_CALL_fast_f_ctr);
  PR_CTR(SLOW_CALL_fast_d_ctr);
  PR_CTR(SLOW_CALL_fast_l_ctr);
  PR_CTR(SLOW_CALL_fast_n_ctr);
  PR_CTR(SLOW_CALL_fast_p_ctr);
  PR_CTR(SLOW_CALL_fast_pv_ctr);
  PR_CTR(SLOW_CALL_fast_pp_ctr);
  PR_CTR(SLOW_CALL_fast_ppv_ctr);
  PR_CTR(SLOW_CALL_fast_ppp_ctr);
  PR_CTR(SLOW_CALL_fast_pppv_ctr);
  PR_CTR(SLOW_CALL_fast_pppp_ctr);
  PR_CTR(SLOW_CALL_fast_ppppp_ctr);
  PR_CTR(SLOW_CALL_fast_pppppp_ctr);
  PR_CTR(VERY_SLOW_CALL_ctr);

  PR_CTR(UNKNOWN_CALL_ctr);
  PR_CTR(KNOWN_CALL_ctr);
  PR_CTR(KNOWN_CALL_TOO_FEW_ARGS_ctr);
  PR_CTR(KNOWN_CALL_EXTRA_ARGS_ctr);
  PR_CTR(MULTI_CHUNK_SLOW_CALL_ctr);
  PR_CTR(MULTI_CHUNK_SLOW_CALL_CHUNKS_ctr);
  PR_CTR(SLOW_CALL_ctr);
  PR_CTR(SLOW_CALL_FUN_TOO_FEW_ctr);
  PR_CTR(SLOW_CALL_FUN_CORRECT_ctr);
  PR_CTR(SLOW_CALL_FUN_TOO_MANY_ctr);
  PR_CTR(SLOW_CALL_PAP_TOO_FEW_ctr);
  PR_CTR(SLOW_CALL_PAP_CORRECT_ctr);
  PR_CTR(SLOW_CALL_PAP_TOO_MANY_ctr);
  PR_CTR(SLOW_CALL_UNEVALD_ctr);

  PR_CTR(RET_NEW_ctr);
  PR_CTR(RET_OLD_ctr);
  PR_CTR(RET_UNBOXED_TUP_ctr);

#define PR_HST_BINS(hst) for (i = 0; i < TICKY_BIN_COUNT; i++) \
  { fprintf(tf,"%11" FMT_Int " " #hst "_%lu\n", hst[i], i); }

  PR_HST_BINS(RET_NEW_hst);
  PR_HST_BINS(RET_OLD_hst);
  PR_HST_BINS(RET_UNBOXED_TUP_hst);

#undef PR_HST_BINS

  PR_CTR(UPDF_OMITTED_ctr);
  PR_CTR(UPDF_PUSHED_ctr);
  PR_CTR(CATCHF_PUSHED_ctr);

  PR_CTR(UPDF_RCC_PUSHED_ctr);
  PR_CTR(UPDF_RCC_OMITTED_ctr);

  PR_CTR(UPD_SQUEEZED_ctr);
  PR_CTR(UPD_CON_IN_NEW_ctr);
  PR_CTR(UPD_CON_IN_PLACE_ctr);
  PR_CTR(UPD_PAP_IN_NEW_ctr);
  PR_CTR(UPD_PAP_IN_PLACE_ctr);

  PR_CTR(UPD_NEW_IND_ctr);
  /* see comment on ENT_PERM_IND_ctr */
  COND_PR_CTR(UPD_NEW_PERM_IND_ctr,RtsFlags.GcFlags.squeezeUpdFrames == false,"UPD_NEW_PERM_IND_ctr requires +RTS -Z");
  PR_CTR(UPD_OLD_IND_ctr);
  /* see comment on ENT_PERM_IND_ctr */
  COND_PR_CTR(UPD_OLD_PERM_IND_ctr,RtsFlags.GcFlags.squeezeUpdFrames == false,"UPD_OLD_PERM_IND_ctr requires +RTS -Z");

  PR_CTR(GC_SEL_ABANDONED_ctr);
  PR_CTR(GC_SEL_MINOR_ctr);
  PR_CTR(GC_SEL_MAJOR_ctr);
  PR_CTR(GC_FAILED_PROMOTION_ctr);
}

/* To print out all the registered-counter info: */

static void
printRegisteredCounterInfo (FILE *tf)
{
    StgEntCounter *p;

    if ( ticky_entry_ctrs != NULL ) {
      fprintf(tf,"\nThe following table is explained by https://gitlab.haskell.org/ghc/ghc/wikis/debugging/ticky-ticky\nAll allocation numbers are in bytes.\n");
      fprintf(tf,"\n**************************************************\n\n");
    }
    fprintf(tf, "%11s%12s%12s  %-63s %s\n",
            "Entries", "Alloc", "Alloc'd", "Non-void Arguments", "STG Name");
    fprintf(tf, "--------------------------------------------------------------------------------\n");
    /* Function name at the end so it doesn't mess up the tabulation */

    for (p = ticky_entry_ctrs; p != NULL; p = p->link) {
        fprintf(tf, "%11" FMT_Int "%12" FMT_Int "%12" FMT_Int " %3lu %-60.60s %s",
                p->entry_count,
                p->allocs,
                p->allocd,
                (unsigned long)p->arity,
                p->arg_kinds,
                p->str);

        fprintf(tf, "\n");

    }
}

void emitTickyCounterDefs(void)
{
#if defined(TRACING)
    postTickyCounterDefs(ticky_entry_ctrs);
#endif
}

void emitTickyCounterSamples(void)
{
#if defined(TRACING)
    postTickyCounterSamples(ticky_entry_ctrs);
#endif
}

#endif /* TICKY_TICKY */

void requestTickyCounterSamples(void)
{
#if defined(TICKY_TICKY) && defined(TRACING)
    if (RtsFlags.TraceFlags.ticky) {
        emitTickyCounterSamples();
    }
#endif
}
