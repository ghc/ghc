/* ----------------------------------------------------------------------------
 Time-stamp: <Wed Jan 12 2000 13:39:33 Stardate: [-30]4193.88 hwloidl>
 $Id: FetchMe.hc,v 1.2 2000/01/13 14:34:06 hwloidl Exp $

 Entry code for a FETCH_ME closure

 This module defines routines for handling remote pointers (@FetchMe@s)
 in GUM.  It is threaded (@.hc@) because @FetchMe_entry@ will be
 called during evaluation.

 * --------------------------------------------------------------------------*/
 
#ifdef PAR /* all of it */

//@menu
//* Includes::			
//* Info tables::		
//* Index::			
//@end menu

//@node Includes, Info tables
//@subsection Includes

#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Storage.h"
#include "GranSim.h"
#include "GranSimRts.h"
#include "Parallel.h"
#include "ParallelRts.h"
#include "FetchMe.h"
#include "HLC.h"
#include "StgRun.h"	/* for StgReturn and register saving */

/* --------------------------------------------------------------------------
   FETCH_ME closures.

   A FETCH_ME closure represents data that currently resides on
   another PE.  We issue a fetch message, and wait for the data to be
   retrieved.

   About the difference between std and PAR in returning to the RTS:
   in PAR we call RTS functions from within the entry code (see also
   BLACKHOLE_entry and friends in StgMiscClosures.hc); therefore, we
   have to save the thread state before calling these functions --- 
   this is done via SAVE_THREAD_STATE; we then just load the return
   code into R1 before jumping into the RTS --- this is done via
   THREAD_RETURN; so, in short we have something like
     SAVE_THREAD_STATE + THREAD_RETURN = BLOCK_NP
   
   ------------------------------------------------------------------------ */

//@node Info tables, Index, Includes
//@subsection Info tables

//@cindex FETCH_ME_info
INFO_TABLE(FETCH_ME_info, FETCH_ME_entry, 0,2, FETCH_ME, const, EF_,0,0);
//@cindex FETCH_ME_entry
STGFUN(FETCH_ME_entry)
{
  extern globalAddr *rga_GLOBAL;
  extern globalAddr *lga_GLOBAL;
  extern globalAddr fmbqga_GLOBAL;
  extern StgClosure *p_GLOBAL;
  /* 
  globalAddr *rga;
  globalAddr *lga;
  globalAddr fmbqga;
  StgClosure *p;
  */

  rga_GLOBAL = ((StgFetchMe *)R1.p)->ga;
  ASSERT(rga->payload.gc.gtid != mytid);

  /* Turn the FETCH_ME into a FETCH_ME_BQ, and place the current thread
   * on the blocking queue.
   */
  // R1.cl->header.info = FETCH_ME_BQ_info;
  SET_INFO((StgClosure *)R1.cl, &FETCH_ME_BQ_info);

  CurrentTSO->link = END_BQ_QUEUE;
  ((StgFetchMeBlockingQueue *)R1.cl)->blocking_queue = (StgBlockingQueueElement *)CurrentTSO;

  /* record onto which closure the current thread is blcoking */
  CurrentTSO->block_info.closure = R1.cl;
  //recordMutable((StgMutClosure *)R1.cl);
  p_GLOBAL = R1.cl;

  /* Save the Thread State here, before calling RTS routines below! */
  //BLOCK_NP_NO_JUMP(1);
  SAVE_THREAD_STATE(1);

  /* unknown junk... needed? --SDM  yes, want to see what's happening -- HWL */
  if (RtsFlags.ParFlags.ParStats.Full) {
    /* Note that CURRENT_TIME may perform an unsafe call */
    //rtsTime now = CURRENT_TIME; /* Now */
    CurrentTSO->par.exectime += CURRENT_TIME - CurrentTSO->par.blockedat;
    CurrentTSO->par.fetchcount++;
    /* TSO_QUEUE(CurrentTSO) = Q_FETCHING; */
    CurrentTSO->par.blockedat = CURRENT_TIME;
    /* we are about to send off a FETCH message, so dump a FETCH event */
    DumpRawGranEvent(CURRENT_PROC, taskIDtoPE(rga_GLOBAL->payload.gc.gtid),
		     GR_FETCH, CurrentTSO, (StgClosure *)R1.p, 0);
  }

  /* Phil T. claims that this was a workaround for a hard-to-find
   * bug, hence I'm leaving it out for now --SDM 
   */
  /* Assign a brand-new global address to the newly created FMBQ */
  lga_GLOBAL = makeGlobal(p_GLOBAL, rtsFalse);
  splitWeight(&fmbqga_GLOBAL, lga_GLOBAL);
  ASSERT(fmbqga_GLOBAL.weight == 1L << (BITS_IN(unsigned) - 1));

  /* I *hope* it's ok to call this from STG land. --SDM */
  STGCALL3(sendFetch, rga_GLOBAL, &fmbqga_GLOBAL, 0/*load*/);

  // sendFetch now called from processTheRealFetch, to make SDM happy
  //theGlobalFromGA.payload.gc.gtid = rga->payload.gc.gtid;
  //theGlobalFromGA.payload.gc.slot = rga->payload.gc.slot;
  //theGlobalFromGA.weight = rga->weight;
  //theGlobalToGA.payload.gc.gtid = fmbqga.payload.gc.gtid;
  //theGlobalToGA.payload.gc.slot = fmbqga.payload.gc.slot;
  //theGlobalToGA.weight = fmbqga.weight;

  // STGCALL6(fprintf,stderr,"%% Fetching %p from remote PE ((%x,%d,%x))\n",R1.p,rga->payload.gc.gtid, rga->payload.gc.slot, rga->weight);

  THREAD_RETURN(1); /* back to the scheduler */  
  // was: BLOCK_NP(1); 
  FE_
}

/* ---------------------------------------------------------------------------
   FETCH_ME_BQ
   
   On the first entry of a FETCH_ME closure, we turn the closure into
   a FETCH_ME_BQ, which behaves just like a BLACKHOLE_BQ.  Any thread
   entering the FETCH_ME_BQ will be placed in the blocking queue.
   When the data arrives from the remote PE, all waiting threads are
   woken up and the FETCH_ME_BQ is overwritten with the fetched data.

   FETCH_ME_BQ_entry is a copy of BLACKHOLE_BQ_entry -- HWL
   ------------------------------------------------------------------------ */

INFO_TABLE(FETCH_ME_BQ_info, FETCH_ME_BQ_entry,0,2,FETCH_ME_BQ,const,EF_,0,0);
//@cindex FETCH_ME_BQ_info
STGFUN(FETCH_ME_BQ_entry)
{
  FB_
    TICK_ENT_BH();

    /* Put ourselves on the blocking queue for this black hole */
    CurrentTSO->block_info.closure = R1.cl;
    CurrentTSO->link = ((StgBlockingQueue *)R1.p)->blocking_queue;
    ((StgBlockingQueue *)R1.p)->blocking_queue = CurrentTSO;

#if defined(PAR)
    /* Save the Thread State here, before calling RTS routines below! */
    SAVE_THREAD_STATE(1);

    if (RtsFlags.ParFlags.ParStats.Full) {
      /* Note that CURRENT_TIME may perform an unsafe call */
      //rtsTime now = CURRENT_TIME; /* Now */
      CurrentTSO->par.exectime += CURRENT_TIME - CurrentTSO->par.blockedat;
      CurrentTSO->par.blockcount++;
      CurrentTSO->par.blockedat = CURRENT_TIME;
      DumpRawGranEvent(CURRENT_PROC, thisPE,
		       GR_BLOCK, CurrentTSO, (StgClosure *)R1.p, 0);
    }

    THREAD_RETURN(1);  /* back to the scheduler */  
#else
    /* stg_gen_block is too heavyweight, use a specialised one */
    BLOCK_NP(1);
#endif
  FE_
}

/* ---------------------------------------------------------------------------
   BLOCKED_FETCH_BQ
   
   A BLOCKED_FETCH closure only ever exists in the blocking queue of a
   globally visible closure i.e. one with a GA. A BLOCKED_FETCH closure
   indicates that a TSO on another PE is waiting for the result of this
   computation. Thus, when updating the closure, the result has to be sent
   to that PE. The relevant routines handling that are awaken_blocked_queue
   and blockFetch (for putting BLOCKED_FETCH closure into a BQ).
*/

//@cindex BLOCKED_FETCH_info
INFO_TABLE(BLOCKED_FETCH_info, BLOCKED_FETCH_entry,0,2,BLOCKED_FETCH,const,EF_,0,0);
//@cindex BLOCKED_FETCH_entry
STGFUN(BLOCKED_FETCH_entry)
{
  FB_
    /* see NON_ENTERABLE_ENTRY_CODE in StgMiscClosures.hc */
    fprintf(stderr,"Qagh: BLOCKED_FETCH entered!\n");
    STGCALL1(raiseError, errorHandler);
    stg_exit(EXIT_FAILURE); /* not executed */
  FE_
}

#endif /* PAR */

//@node Index,  , Info tables
//@subsection Index

//@index
//* BLOCKED_FETCH_entry::  @cindex\s-+BLOCKED_FETCH_entry
//* BLOCKED_FETCH_info::  @cindex\s-+BLOCKED_FETCH_info
//* FETCH_ME_BQ_info::  @cindex\s-+FETCH_ME_BQ_info
//* FETCH_ME_entry::  @cindex\s-+FETCH_ME_entry
//* FETCH_ME_info::  @cindex\s-+FETCH_ME_info
//@end index
