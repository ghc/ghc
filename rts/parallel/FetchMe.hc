/* ----------------------------------------------------------------------------
 Time-stamp: <Tue Mar 06 2001 17:01:46 Stardate: [-30]6288.54 hwloidl>

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

#include "Stg.h"
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

   A word on the ptr/nonptr fields in the macros: they are unused at the
   moment; all closures defined here have constant size (ie. no payload
   that varies from closure to closure). Therefore, all routines that 
   need to know the size of these closures have to do a sizeofW(StgFetchMe) 
   etc to get the closure size. See get_closure_info(), evacuate() and
   checkClosure() (using the same fcts for determining the size of the 
   closures would be a good idea; at least it would be a nice step towards
   making this code bug free).
   ------------------------------------------------------------------------ */

//@node Info tables, Index, Includes
//@subsection Info tables

//@cindex FETCH_ME_info
INFO_TABLE(stg_FETCH_ME_info, stg_FETCH_ME_entry, 0,2, FETCH_ME,, EF_,"FETCH_ME","FETCH_ME");
//@cindex FETCH_ME_entry
STGFUN(stg_FETCH_ME_entry)
{
  FB_
    TICK_ENT_BH();

    ASSERT(((StgFetchMe *)R1.p)->ga->payload.gc.gtid != mytid);
  
    /* Turn the FETCH_ME into a FETCH_ME_BQ, and place the current thread
     * on the blocking queue.
     */
    // ((StgFetchMeBlockingQueue *)R1.cl)->header.info = &FETCH_ME_BQ_info; // does the same as SET_INFO
    SET_INFO((StgClosure *)R1.cl, &stg_FETCH_ME_BQ_info);
  
    /* Remember GA as a global var (used in blockThread); NB: not thread safe! */
    ASSERT(theGlobalFromGA.payload.gc.gtid == (GlobalTaskId)0);
    theGlobalFromGA = *((StgFetchMe *)R1.p)->ga; 

    /* Put ourselves on the blocking queue for this black hole */
    ASSERT(looks_like_ga(((StgFetchMe *)R1.p)->ga));
    CurrentTSO->link = END_BQ_QUEUE;
    ((StgFetchMeBlockingQueue *)R1.cl)->blocking_queue = (StgBlockingQueueElement *)CurrentTSO;
  
    /* jot down why and on what closure we are blocked */
    CurrentTSO->why_blocked = BlockedOnGA;
    CurrentTSO->block_info.closure = R1.cl;
    /* closure is mutable since something has just been added to its BQ */
    //recordMutable((StgMutClosure *)R1.cl);

    /* sendFetch etc is now done in blockThread, which is called from the
       scheduler -- HWL */

    BLOCK_NP(1); 
  FE_
}

/* ---------------------------------------------------------------------------
   FETCH_ME_BQ
   
   On the first entry of a FETCH_ME closure, we turn the closure into
   a FETCH_ME_BQ, which behaves just like a BLACKHOLE_BQ.  Any thread
   entering the FETCH_ME_BQ will be placed in the blocking queue.
   When the data arrives from the remote PE, all waiting threads are
   woken up and the FETCH_ME_BQ is overwritten with the fetched data.

   FETCH_ME_BQ_entry is almost identical to BLACKHOLE_BQ_entry -- HWL
   ------------------------------------------------------------------------ */

INFO_TABLE(stg_FETCH_ME_BQ_info, stg_FETCH_ME_BQ_entry,0,2,FETCH_ME_BQ,,EF_,"FETCH_ME_BQ","FETCH_ME_BQ");
//@cindex FETCH_ME_BQ_info
STGFUN(stg_FETCH_ME_BQ_entry)
{
  FB_
    TICK_ENT_BH();

    /* Put ourselves on the blocking queue for this node */
    CurrentTSO->link = (StgTSO*)((StgBlockingQueue *)R1.p)->blocking_queue;
    ((StgBlockingQueue *)R1.p)->blocking_queue = (StgBlockingQueueElement *)CurrentTSO;

    /* jot down why and on what closure we are blocked */
    CurrentTSO->why_blocked = BlockedOnGA_NoSend;
    CurrentTSO->block_info.closure = R1.cl;

    /* stg_gen_block is too heavyweight, use a specialised one */
    BLOCK_NP(1);
  FE_
}

/* ---------------------------------------------------------------------------
   BLOCKED_FETCH_BQ
   
   A BLOCKED_FETCH closure only ever exists in the blocking queue of a
   globally visible closure i.e. one with a GA. A BLOCKED_FETCH closure
   indicates that a TSO on another PE is waiting for the result of this
   computation. Thus, when updating the closure, the result has to be sent
   to that PE. The relevant routines handling that are awakenBlockedQueue
   and blockFetch (for putting BLOCKED_FETCH closure into a BQ).
   ------------------------------------------------------------------------ */

//@cindex BLOCKED_FETCH_info
INFO_TABLE(stg_BLOCKED_FETCH_info, stg_BLOCKED_FETCH_entry,0,2,BLOCKED_FETCH,,EF_,"BLOCKED_FETCH","BLOCKED_FETCH");
//@cindex BLOCKED_FETCH_entry
STGFUN(stg_BLOCKED_FETCH_entry)
{
  FB_
    /* see NON_ENTERABLE_ENTRY_CODE in StgMiscClosures.hc */
    STGCALL2(fprintf,stderr,"BLOCKED_FETCH object entered!\n");
    STGCALL1(shutdownHaskellAndExit, EXIT_FAILURE);
  FE_
}


/* ---------------------------------------------------------------------------
   REMOTE_REF
   
   A REMOTE_REF closure is generated whenever we wish to refer to a sticky
   object on another PE.
   ------------------------------------------------------------------------ */

//@cindex REMOTE_REF_info
INFO_TABLE(stg_REMOTE_REF_info, stg_REMOTE_REF_entry,0,2,REMOTE_REF,,EF_,"REMOTE_REF","REMOTE_REF");
//@cindex REMOTE_REF_entry
STGFUN(stg_REMOTE_REF_entry)
{
  FB_
    /* see NON_ENTERABLE_ENTRY_CODE in StgMiscClosures.hc */
    STGCALL2(fprintf,stderr,"REMOTE REF object entered!\n");
    STGCALL1(shutdownHaskellAndExit, EXIT_FAILURE);
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
