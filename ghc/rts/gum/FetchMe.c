/* -----------------------------------------------------------------------------
 * $Id: FetchMe.c,v 1.2 1998/12/02 13:29:03 simonm Exp $
 *
 * Entry code for a FETCH_ME closure
 *
 * ---------------------------------------------------------------------------*/
 
#ifdef PAR /* all of it */

#include "Rts.h"
#include "FetchMe.h"
#include "HLC.h"

/* -----------------------------------------------------------------------------
   FETCH_ME closures.

   A FETCH_ME closure represents data that currently resides on
   another PE.  We issue a fetch message, and wait for the data to be
   retrieved.
   -------------------------------------------------------------------------- */

INFO_TABLE(FETCH_ME_info, FETCH_ME_entry, 0,2, FETCH_ME, const, EF_,0,0);

STGFUN(FETCH_ME_entry)
{
    globalAddr *rGA;
    globalAddr *lGA;
    globalAddr fmbqGA;

# if defined(GRAN)
    STGCALL0(void,(),GranSimBlock);	/* Do this before losing its TSO_LINK */
# endif

    rGA = FETCHME_GA(R1);
    ASSERT(rGA->loc.gc.gtid != mytid);

    /* Turn the FETCH_ME into a FETCH_ME_BQ, and place the current thread
     * on the blocking queue.
     */
    R1.cl->header.info = FETCH_ME_BQ_info;
    CurrentTSO->link = END_TSO_QUEUE;
    ((StgBlackHole *)R1.cl)->blocking_queue = CurrentTSO;

#ifdef 0 /* unknown junk... needed? --SDM */
    if (DO_QP_PROF) {
    	QP_Event1("GR", CurrentTSO);
    }

    if (RTSflags.ParFlags.granSimStats) {
        /* Note that CURRENT_TIME may perform an unsafe call */
	TIME now = CURRENT_TIME;
        TSO_EXECTIME(CurrentTSO) += now - TSO_BLOCKEDAT(CurrentTSO);
        TSO_FETCHCOUNT(CurrentTSO)++;
	TSO_QUEUE(CurrentTSO) = Q_FETCHING;
        TSO_BLOCKEDAT(CurrentTSO) = now;
        /* DumpGranEventAndNode(GR_FETCH, CurrentTSO, (SAVE_R1).p, 
           taskIDtoPE(rGA->loc.gc.gtid)); */
	DumpRawGranEvent(CURRENT_PROC,taskIDtoPE(rGA->loc.gc.gtid),GR_FETCH,
			 CurrentTSO,(SAVE_R1).p,0);
    }

    /* Phil T. claims that this was a workaround for a hard-to-find
     * bug, hence I'm leaving it out for now --SDM 
     */
    /* Assign a brand-new global address to the newly created FMBQ */
    lGA = MakeGlobal((SAVE_R1).p, rtsFalse);
    splitWeight(&fmbqGA, lGA);
    ASSERT(fmbqGA.weight == 1L << (BITS_IN(unsigned) - 1));
#endif

    /* I *hope* it's ok to call this from STG land. --SDM */
    STGCALL3(sendFetch, rGA, &fmbqGA, 0/*load*/);

    BLOCK_NP(1); /* back to the scheduler */

    FE_
}

/* -----------------------------------------------------------------------------
   FETCH_ME_BQ
   
   On the first entry of a FETCH_ME closure, we turn the closure into
   a FETCH_ME_BQ, which behaves just like a black hole.  Any thread
   entering the FETCH_ME_BQ will be placed in the blocking queue.
   When the data arrives from the remote PE, all waiting threads are
   woken up and the FETCH_ME_BQ is overwritten with the fetched data.
   -------------------------------------------------------------------------- */

INFO_TABLE(FETCH_ME_BQ_info, BLACKHOLE_entry,0,2,BLACKHOLE,const,EF_,0,0);

#endif /* PAR */
