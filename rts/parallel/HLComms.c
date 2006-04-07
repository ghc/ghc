/* ----------------------------------------------------------------------------
 * Time-stamp: <Wed Mar 21 2001 16:34:41 Stardate: [-30]6363.45 hwloidl>
 *
 * High Level Communications Routines (HLComms.lc)
 *
 * Contains the high-level routines (i.e. communication
 * subsystem independent) used by GUM
 * 
 * GUM 0.2x: Phil Trinder, Glasgow University, 12 December 1994
 * GUM 3.xx: Phil Trinder, Simon Marlow July 1998
 * GUM 4.xx: H-W. Loidl, Heriot-Watt University, November 1999 -
 * 
 * ------------------------------------------------------------------------- */

#ifdef PAR /* whole file */

//@node High Level Communications Routines, , ,
//@section High Level Communications Routines

//@menu
//* Macros etc::		
//* Includes::			
//* GUM Message Sending and Unpacking Functions::  
//* Message-Processing Functions::  
//* GUM Message Processor::	
//* Miscellaneous Functions::	
//* Index::			
//@end menu

//@node Macros etc, Includes, High Level Communications Routines, High Level Communications Routines
//@subsection Macros etc

/* Evidently not Posix */
/* #include "PosixSource.h" */

//@node Includes, GUM Message Sending and Unpacking Functions, Macros etc, High Level Communications Routines
//@subsection Includes

#include "Rts.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "Storage.h"   // for recordMutable
#include "HLC.h"
#include "Parallel.h"
#include "GranSimRts.h"
#include "ParallelRts.h"
#include "Sparks.h"
#include "FetchMe.h"     // for BLOCKED_FETCH_info etc
#if defined(DEBUG)
# include "ParallelDebug.h"
#endif
#include "StgMacros.h" // inlined IS_... fcts

#ifdef DIST
#include "SchedAPI.h" //for createIOThread
extern unsigned int context_switch; 
#endif /* DIST */

//@node GUM Message Sending and Unpacking Functions, Message-Processing Functions, Includes, High Level Communications Routines
//@subsection GUM Message Sending and Unpacking Functions

/*
 * GUM Message Sending and Unpacking Functions
 */

/*
 * Allocate space for message processing
 */

//@cindex gumPackBuffer
static rtsPackBuffer *gumPackBuffer;

//@cindex initMoreBuffers
rtsBool
initMoreBuffers(void)
{
  if ((gumPackBuffer = (rtsPackBuffer *)stgMallocWords(RtsFlags.ParFlags.packBufferSize, 
					     "initMoreBuffers")) == NULL)
    return rtsFalse;
  return rtsTrue;
}

/*
 * SendFetch packs the two global addresses and a load into a message +
 * sends it.  

//@cindex FETCH

   Structure of a FETCH message:

         |    GA 1     |        GA 2          |
         +------------------------------------+------+
	 | gtid | slot | weight | gtid | slot | load |
	 +------------------------------------+------+
 */

//@cindex sendFetch
void
sendFetch(globalAddr *rga, globalAddr *lga, int load)
{
  ASSERT(rga->weight > 0 && lga->weight > 0);
  IF_PAR_DEBUG(fetch,
	       belch("~^** Sending Fetch for ((%x, %d, 0)); locally ((%x, %d, %x)), load = %d", 
		     rga->payload.gc.gtid, rga->payload.gc.slot, 
		     lga->payload.gc.gtid, lga->payload.gc.slot, lga->weight,
		     load));


  /* ToDo: Dump event
  DumpRawGranEvent(CURRENT_PROC, taskIDtoPE(rga->payload.gc.gtid), 
		   GR_FETCH, CurrentTSO, (StgClosure *)(lga->payload.gc.slot),
		   0, spark_queue_len(ADVISORY_POOL));
  */

  sendOpV(PP_FETCH, rga->payload.gc.gtid, 6,
	  (StgWord) rga->payload.gc.gtid, (StgWord) rga->payload.gc.slot, 
	  (StgWord) lga->weight, (StgWord) lga->payload.gc.gtid, 
	  (StgWord) lga->payload.gc.slot, (StgWord) load);
}

/*
 * unpackFetch unpacks a FETCH message into two Global addresses and a load
 * figure.  
*/

//@cindex unpackFetch
static void
unpackFetch(globalAddr *lga, globalAddr *rga, int *load)
{
  long buf[6];

  GetArgs(buf, 6); 

  IF_PAR_DEBUG(fetch,
	       belch("~^** Unpacking Fetch for ((%x, %d, 0)) to ((%x, %d, %x)), load = %d", 
		     (GlobalTaskId) buf[0], (int) buf[1], 
		     (GlobalTaskId) buf[3], (int) buf[4], buf[2], buf[5]));

  lga->weight = 1;
  lga->payload.gc.gtid = (GlobalTaskId) buf[0];
  lga->payload.gc.slot = (int) buf[1];

  rga->weight = (unsigned) buf[2];
  rga->payload.gc.gtid = (GlobalTaskId) buf[3];
  rga->payload.gc.slot = (int) buf[4];

  *load = (int) buf[5];

  ASSERT(rga->weight > 0);
}

/*
 * SendResume packs the remote blocking queue's GA and data into a message 
 * and sends it.

//@cindex RESUME

   Structure of a RESUME message:

      -------------------------------
      | weight | slot | n | data ...
      -------------------------------

   data is a packed graph represented as an rtsPackBuffer
   n is the size of the graph (as returned by PackNearbyGraph) + packet hdr size
 */

//@cindex sendResume
void
sendResume(globalAddr *rga, int nelem, rtsPackBuffer *packBuffer)
{
  IF_PAR_DEBUG(fetch,
	       belch("~^[] Sending Resume (packet <<%d>> with %d elems) for ((%x, %d, %x)) to [%x]", 
		     packBuffer->id, nelem,
		     rga->payload.gc.gtid, rga->payload.gc.slot, rga->weight,
		     rga->payload.gc.gtid));
  IF_PAR_DEBUG(packet,
	       PrintPacket(packBuffer));

  ASSERT(nelem==packBuffer->size);
  /* check for magic end-of-buffer word */
  IF_DEBUG(sanity, ASSERT(*(packBuffer->buffer+nelem) == END_OF_BUFFER_MARKER));

  sendOpNV(PP_RESUME, rga->payload.gc.gtid, 
	   nelem + PACK_BUFFER_HDR_SIZE + DEBUG_HEADROOM, (StgPtr)packBuffer, 
	   2, (rtsWeight) rga->weight, (StgWord) rga->payload.gc.slot);
}

/*
 * unpackResume unpacks a Resume message into two Global addresses and
 * a data array.
 */

//@cindex unpackResume
static void
unpackResume(globalAddr *lga, int *nelem, rtsPackBuffer *packBuffer)
{
    long buf[3];

    GetArgs(buf, 3); 

    /*
      RESUME event is written in awaken_blocked_queue
    DumpRawGranEvent(CURRENT_PROC, taskIDtoPE(lga->payload.gc.gtid), 
		     GR_RESUME, END_TSO_QUEUE, (StgClosure *)NULL, 0, 0);
    */

    lga->weight = (unsigned) buf[0];
    lga->payload.gc.gtid = mytid;
    lga->payload.gc.slot = (int) buf[1];

    *nelem = (int) buf[2] - PACK_BUFFER_HDR_SIZE - DEBUG_HEADROOM;
    GetArgs(packBuffer, *nelem + PACK_BUFFER_HDR_SIZE + DEBUG_HEADROOM);

    IF_PAR_DEBUG(fetch,
		 belch("~^[] Unpacking Resume (packet <<%d>> with %d elems) for ((%x, %d, %x))", 
		       packBuffer->id, *nelem, mytid, (int) buf[1], (unsigned) buf[0]));

    /* check for magic end-of-buffer word */
    IF_DEBUG(sanity, ASSERT(*(packBuffer->buffer+*nelem) == END_OF_BUFFER_MARKER));
}

/*
 * SendAck packs the global address being acknowledged, together with
 * an array of global addresses for any closures shipped and sends them.

//@cindex ACK

   Structure of an ACK message:

      |        GA 1          |        GA 2          | 
      +---------------------------------------------+-------
      | weight | gtid | slot | weight | gtid | slot |  .....  ngas times
      + --------------------------------------------+------- 

 */

//@cindex sendAck
void
sendAck(GlobalTaskId task, int ngas, globalAddr *gagamap)
{
  static long *buffer;
  long *p;
  int i;

  if(ngas==0)
    return; //don't send unnecessary messages!!
  
  buffer = (long *) gumPackBuffer;

  for(i = 0, p = buffer; i < ngas; i++, p += 6) {
    ASSERT(gagamap[1].weight > 0);
    p[0] = (long) gagamap->weight;
    p[1] = (long) gagamap->payload.gc.gtid;
    p[2] = (long) gagamap->payload.gc.slot;
    gagamap++;
    p[3] = (long) gagamap->weight;
    p[4] = (long) gagamap->payload.gc.gtid;
    p[5] = (long) gagamap->payload.gc.slot;
    gagamap++;
  }
  IF_PAR_DEBUG(schedule,
	       belch("~^,, Sending Ack (%d pairs) to [%x]\n", 
		     ngas, task));

  sendOpN(PP_ACK, task, p - buffer, (StgPtr)buffer);
}

/*
 * unpackAck unpacks an Acknowledgement message into a Global address,
 * a count of the number of global addresses following and a map of 
 * Global addresses
 */

//@cindex unpackAck
static void
unpackAck(int *ngas, globalAddr *gagamap)
{
  long GAarraysize;
  long buf[6];
  
  GetArgs(&GAarraysize, 1);
  
  *ngas = GAarraysize / 6;
  
  IF_PAR_DEBUG(schedule,
	       belch("~^,, Unpacking Ack (%d pairs) on [%x]\n", 
		     *ngas, mytid));

  while (GAarraysize > 0) {
    GetArgs(buf, 6);
    gagamap->weight = (rtsWeight) buf[0];
    gagamap->payload.gc.gtid = (GlobalTaskId) buf[1];
    gagamap->payload.gc.slot = (int) buf[2];
    gagamap++;
    gagamap->weight = (rtsWeight) buf[3];
    gagamap->payload.gc.gtid = (GlobalTaskId) buf[4];
    gagamap->payload.gc.slot = (int) buf[5];
    ASSERT(gagamap->weight > 0);
    gagamap++;
    GAarraysize -= 6;
  }
}

/*
 * SendFish packs the global address being acknowledged, together with
 * an array of global addresses for any closures shipped and sends them.

//@cindex FISH

 Structure of a FISH message:

     +----------------------------------+
     | orig PE | age | history | hunger |
     +----------------------------------+
 */

//@cindex sendFish
void
sendFish(GlobalTaskId destPE, GlobalTaskId origPE, 
	 int age, int history, int hunger)
{
  IF_PAR_DEBUG(fish,
	       belch("~^$$ Sending Fish to [%x] (%d outstanding fishes)", 
		     destPE, outstandingFishes));

  sendOpV(PP_FISH, destPE, 4, 
	  (StgWord) origPE, (StgWord) age, (StgWord) history, (StgWord) hunger);

  if (origPE == mytid) {
    //fishing = rtsTrue;
    outstandingFishes++;
  }
}

/*
 * unpackFish unpacks a FISH message into the global task id of the
 * originating PE and 3 data fields: the age, history and hunger of the
 * fish. The history + hunger are not currently used.

 */

//@cindex unpackFish
static void
unpackFish(GlobalTaskId *origPE, int *age, int *history, int *hunger)
{
  long buf[4];
  
  GetArgs(buf, 4);
  
  IF_PAR_DEBUG(fish,
	       belch("~^$$ Unpacking Fish from [%x] (age=%d)", 
		     (GlobalTaskId) buf[0], (int) buf[1]));

  *origPE = (GlobalTaskId) buf[0];
  *age = (int) buf[1];
  *history = (int) buf[2];
  *hunger = (int) buf[3];
}

/*
 * SendFree sends (weight, slot) pairs for GAs that we no longer need
 * references to.  

//@cindex FREE

   Structure of a FREE message:
   
       +-----------------------------
       | n | weight_1 | slot_1 | ...
       +-----------------------------
 */
//@cindex sendFree
void
sendFree(GlobalTaskId pe, int nelem, StgPtr data)
{
    IF_PAR_DEBUG(free,
		 belch("~^!! Sending Free (%d GAs) to [%x]", 
		       nelem/2, pe));

    sendOpN(PP_FREE, pe, nelem, data);
}

/*
 * unpackFree unpacks a FREE message into the amount of data shipped and
 * a data block.
 */
//@cindex unpackFree
static void
unpackFree(int *nelem, StgWord *data)
{
  long buf[1];
  
  GetArgs(buf, 1);
  *nelem = (int) buf[0];

  IF_PAR_DEBUG(free,
	       belch("~^!! Unpacking Free (%d GAs)", 
		     *nelem/2));

  GetArgs(data, *nelem);
}

/*
 * SendSchedule sends a closure to be evaluated in response to a Fish
 * message. The message is directed to the PE that originated the Fish
 * (origPE), and includes the packed closure (data) along with its size
 * (nelem).

//@cindex SCHEDULE

   Structure of a SCHEDULE message:

       +------------------------------------
       | PE | n | pack buffer of a graph ...
       +------------------------------------
 */
//@cindex sendSchedule
void
sendSchedule(GlobalTaskId origPE, int nelem, rtsPackBuffer *packBuffer) 
{
  IF_PAR_DEBUG(schedule,
	       belch("~^-- Sending Schedule (packet <<%d>> with %d elems) to [%x]\n", 
		     packBuffer->id, nelem, origPE));
  IF_PAR_DEBUG(packet,
	       PrintPacket(packBuffer));

  ASSERT(nelem==packBuffer->size);
  /* check for magic end-of-buffer word */
  IF_DEBUG(sanity, ASSERT(*(packBuffer->buffer+nelem) == END_OF_BUFFER_MARKER));

  sendOpN(PP_SCHEDULE, origPE, 
	  nelem + PACK_BUFFER_HDR_SIZE + DEBUG_HEADROOM, (StgPtr)packBuffer);
}

/*
 * unpackSchedule unpacks a SCHEDULE message into the Global address of
 * the closure shipped, the amount of data shipped (nelem) and the data
 * block (data).
 */

//@cindex unpackSchedule
static void
unpackSchedule(int *nelem, rtsPackBuffer *packBuffer)
{
  long buf[1];

  /* first, just unpack 1 word containing the total size (including header) */
  GetArgs(buf, 1);
  /* no. of elems, not counting the header of the pack buffer */
  *nelem = (int) buf[0] - PACK_BUFFER_HDR_SIZE - DEBUG_HEADROOM;

  /* automatic cast of flat pvm-data to rtsPackBuffer */
  GetArgs(packBuffer, *nelem + PACK_BUFFER_HDR_SIZE + DEBUG_HEADROOM);

  IF_PAR_DEBUG(schedule,
	       belch("~^-- Unpacking Schedule (packet <<%d>> with %d elems) on [%x]\n", 
		     packBuffer->id, *nelem, mytid));

  ASSERT(*nelem==packBuffer->size);
  /* check for magic end-of-buffer word */
  IF_DEBUG(sanity, ASSERT(*(packBuffer->buffer+*nelem) == END_OF_BUFFER_MARKER));
}

#ifdef DIST
/* sendReval is almost identical to the Schedule version, so we can unpack with unpackSchedule */
void
sendReval(GlobalTaskId origPE, int nelem, rtsPackBuffer *packBuffer) 
{  
  IF_PAR_DEBUG(schedule,
	       belch("~^-- Sending Reval (packet <<%d>> with %d elems) to [%x]\n", 
		     packBuffer->id, nelem, origPE));
  IF_PAR_DEBUG(packet,
	       PrintPacket(packBuffer));

  ASSERT(nelem==packBuffer->size);
  /* check for magic end-of-buffer word */
  IF_DEBUG(sanity, ASSERT(*(packBuffer->buffer+nelem) == END_OF_BUFFER_MARKER));

  sendOpN(PP_REVAL, origPE, 
	  nelem + PACK_BUFFER_HDR_SIZE + DEBUG_HEADROOM, (StgPtr)packBuffer);
}

void FinishReval(StgTSO *t)
{ StgClosure *res;
  globalAddr ga;
  nat size;
  rtsPackBuffer *buffer=NULL;
  
  ga.payload.gc.slot = t->revalSlot;
  ga.payload.gc.gtid = t->revalTid;
  ga.weight = 0; 
  
  //find where the reval result is
  res = GALAlookup(&ga);
  ASSERT(res);
  
  IF_PAR_DEBUG(schedule,
    printGA(&ga);
    belch(" needs the result %08x\n",res));       
  
  //send off the result
  buffer = PackNearbyGraph(res, END_TSO_QUEUE, &size,ga.payload.gc.gtid);
  ASSERT(buffer != (rtsPackBuffer *)NULL);
  sendResume(&ga, size, buffer);

  IF_PAR_DEBUG(schedule,
    belch("@;~) Reval Finished"));
}

#endif /* DIST */

//@node Message-Processing Functions, GUM Message Processor, GUM Message Sending and Unpacking Functions, High Level Communications Routines
//@subsection Message-Processing Functions

/*
 * Message-Processing Functions
 *
 * The following routines process incoming GUM messages. Often reissuing
 * messages in response.
 *
 * processFish unpacks a fish message, reissuing it if it's our own,
 * sending work if we have it or sending it onwards otherwise.
 */

/*
 * processFetches constructs and sends resume messages for every
 * BlockedFetch which is ready to be awakened.
 * awaken_blocked_queue (in Schedule.c) is responsible for moving 
 * BlockedFetches from a blocking queue to the PendingFetches queue.
 */
void GetRoots(void);
extern StgBlockedFetch *PendingFetches;

nat
pending_fetches_len(void)
{
  StgBlockedFetch *bf;
  nat n;

  for (n=0, bf=PendingFetches; bf != END_BF_QUEUE; n++, bf = (StgBlockedFetch *)(bf->link)) {
    ASSERT(get_itbl(bf)->type==BLOCKED_FETCH);
  }
  return n;
}

//@cindex processFetches
void
processFetches(void) {
  StgBlockedFetch *bf, *next;
  StgClosure *closure;
  StgInfoTable *ip;
  globalAddr rga;
  static rtsPackBuffer *packBuffer;
    
  IF_PAR_DEBUG(verbose,
	       belch("____ processFetches: %d pending fetches (root @ %p)",
		     pending_fetches_len(), PendingFetches));
  
  for (bf = PendingFetches; 
       bf != END_BF_QUEUE;
       bf=next) {
    /* the PendingFetches list contains only BLOCKED_FETCH closures */
    ASSERT(get_itbl(bf)->type==BLOCKED_FETCH);
    /* store link (we might overwrite it via blockFetch later on */
    next = (StgBlockedFetch *)(bf->link);

    /*
     * Find the target at the end of the indirection chain, and
     * process it in much the same fashion as the original target
     * of the fetch.  Though we hope to find graph here, we could
     * find a black hole (of any flavor) or even a FetchMe.
     */
    closure = bf->node;
    /*
      We evacuate BQs and update the node fields where necessary in GC.c
      So, if we find an EVACUATED closure, something has gone Very Wrong
      (and therefore we let the RTS crash most ungracefully).
    */
    ASSERT(get_itbl(closure)->type != EVACUATED);
      //  closure = ((StgEvacuated *)closure)->evacuee;

    closure = UNWIND_IND(closure);
    //while ((ind = IS_INDIRECTION(closure)) != NULL) { closure = ind; }

    ip = get_itbl(closure);
    if (ip->type == FETCH_ME) {
      /* Forward the Fetch to someone else */
      rga.payload.gc.gtid = bf->ga.payload.gc.gtid;
      rga.payload.gc.slot = bf->ga.payload.gc.slot;
      rga.weight = bf->ga.weight;
      
      sendFetch(((StgFetchMe *)closure)->ga, &rga, 0 /* load */);

      // Global statistics: count no. of fetches
      if (RtsFlags.ParFlags.ParStats.Global &&
	  RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
	globalParStats.tot_fetch_mess++;
      }

      IF_PAR_DEBUG(fetch,
		   belch("__-> processFetches: Forwarding fetch from %lx to %lx",
			 mytid, rga.payload.gc.gtid));

    } else if (IS_BLACK_HOLE(closure)) {
      IF_PAR_DEBUG(verbose,
		   belch("__++ processFetches: trying to send a BLACK_HOLE => doing a blockFetch on closure %p (%s)",
			 closure, info_type(closure)));
      bf->node = closure;
      blockFetch(bf, closure);
    } else {
      /* We now have some local graph to send back */
      nat size;

      packBuffer = gumPackBuffer;
      IF_PAR_DEBUG(verbose,
		   belch("__*> processFetches: PackNearbyGraph of closure %p (%s)",
			 closure, info_type(closure)));

      if ((packBuffer = PackNearbyGraph(closure, END_TSO_QUEUE, &size, bf->ga.payload.gc.gtid)) == NULL) {
	// Put current BF back on list
	bf->link = (StgBlockingQueueElement *)PendingFetches;
	PendingFetches = (StgBlockedFetch *)bf;
	// ToDo: check that nothing more has to be done to prepare for GC!
	barf("processFetches: out of heap while packing graph; ToDo: call GC here");
	GarbageCollect(GetRoots, rtsFalse); 
	bf = PendingFetches;
	PendingFetches = (StgBlockedFetch *)(bf->link);
	closure = bf->node;
	packBuffer = PackNearbyGraph(closure, END_TSO_QUEUE, &size, bf->ga.payload.gc.gtid);
	ASSERT(packBuffer != (rtsPackBuffer *)NULL);
      }
      rga.payload.gc.gtid = bf->ga.payload.gc.gtid;
      rga.payload.gc.slot = bf->ga.payload.gc.slot;
      rga.weight = bf->ga.weight;
      
      sendResume(&rga, size, packBuffer);

      // Global statistics: count no. of fetches
      if (RtsFlags.ParFlags.ParStats.Global &&
	  RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
	globalParStats.tot_resume_mess++;
      }
    }
  }
  PendingFetches = END_BF_QUEUE;
}

#if 0
/*
  Alternatively to sending fetch messages directly from the FETCH_ME_entry
  code we could just store the data about the remote data in a global
  variable and send the fetch request from the main scheduling loop (similar
  to processFetches above). This would save an expensive STGCALL in the entry 
  code because we have to go back to the scheduler anyway.
*/
//@cindex processFetches
void
processTheRealFetches(void) {
  StgBlockedFetch *bf;
  StgClosure *closure, *next;
    
  IF_PAR_DEBUG(verbose,
	       belch("__ processTheRealFetches: ");
	       printGA(&theGlobalFromGA);
	       printGA(&theGlobalToGA));

  ASSERT(theGlobalFromGA.payload.gc.gtid != 0 &&
	 theGlobalToGA.payload.gc.gtid != 0);

  /* the old version did this in the FETCH_ME entry code */
  sendFetch(&theGlobalFromGA, &theGlobalToGA, 0/*load*/);
  
}
#endif


/* 
   Way of dealing with unwanted fish.
   Used during startup/shutdown, or from unknown PEs 
*/
void
bounceFish(void) { 
  GlobalTaskId origPE;
  int age, history, hunger;
  
  /* IF_PAR_DEBUG(verbose, */
	       belch(".... [%x] Bouncing unwanted FISH",mytid);

  unpackFish(&origPE, &age, &history, &hunger);
	  
  if (origPE == mytid) {
    //fishing = rtsFalse;                   // fish has come home
    outstandingFishes--;
    last_fish_arrived_at = CURRENT_TIME;  // remember time (see schedule fct)
    return;                               // that's all
  }

  /* otherwise, send it home to die */
  sendFish(origPE, origPE, (age + 1), NEW_FISH_HISTORY, NEW_FISH_HUNGER);
  // Global statistics: count no. of fetches
      if (RtsFlags.ParFlags.ParStats.Global &&
	  RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
	globalParStats.tot_fish_mess++;
      }
}
   
/*
 * processFish unpacks a fish message, reissuing it if it's our own,
 * sending work if we have it or sending it onwards otherwise.
 */
//@cindex processFish
static void
processFish(void)
{
  GlobalTaskId origPE;
  int age, history, hunger;
  rtsSpark spark;
  static rtsPackBuffer *packBuffer; 

  unpackFish(&origPE, &age, &history, &hunger);

  if (origPE == mytid) {
    //fishing = rtsFalse;                   // fish has come home
    outstandingFishes--;
    last_fish_arrived_at = CURRENT_TIME;  // remember time (see schedule fct)
    return;                               // that's all
  }

  ASSERT(origPE != mytid);
  IF_PAR_DEBUG(fish,
	       belch("$$__ processing fish; %d sparks available",
		     spark_queue_len(&(MainRegTable.rSparks))));
  while ((spark = findSpark(rtsTrue/*for_export*/)) != NULL) {
    nat size;
    // StgClosure *graph;

    packBuffer = gumPackBuffer; 
    ASSERT(closure_SHOULD_SPARK((StgClosure *)spark));
    if ((packBuffer = PackNearbyGraph(spark, END_TSO_QUEUE, &size,origPE)) == NULL) {
      IF_PAR_DEBUG(fish,
		   belch("$$ GC while trying to satisfy FISH via PackNearbyGraph of node %p",
			 (StgClosure *)spark));
      barf("processFish: out of heap while packing graph; ToDo: call GC here");
      GarbageCollect(GetRoots, rtsFalse);
      /* Now go back and try again */
    } else {
      IF_PAR_DEBUG(verbose,
		   if (RtsFlags.ParFlags.ParStats.Sparks)
		     belch("==== STEALING spark %x; sending to %x", spark, origPE));
      
      IF_PAR_DEBUG(fish,
		   belch("$$-- Replying to FISH from %x by sending graph @ %p (%s)",
			 origPE, 
			 (StgClosure *)spark, info_type((StgClosure *)spark)));
      sendSchedule(origPE, size, packBuffer);
      disposeSpark(spark);
      // Global statistics: count no. of fetches
      if (RtsFlags.ParFlags.ParStats.Global &&
	  RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
	globalParStats.tot_schedule_mess++;
      }

      break;
    }
  }
  if (spark == (rtsSpark)NULL) {
    IF_PAR_DEBUG(fish,
		 belch("$$^^ No sparks available for FISH from %x",
		       origPE));
    /* We have no sparks to give */
    if (age < FISH_LIFE_EXPECTANCY) {
      /* and the fish is atill young, send it to another PE to look for work */
      sendFish(choosePE(), origPE,
	       (age + 1), NEW_FISH_HISTORY, NEW_FISH_HUNGER);

      // Global statistics: count no. of fetches
      if (RtsFlags.ParFlags.ParStats.Global &&
	  RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
	globalParStats.tot_fish_mess++;
      }
    } else { /* otherwise, send it home to die */
      sendFish(origPE, origPE, (age + 1), NEW_FISH_HISTORY, NEW_FISH_HUNGER);
      // Global statistics: count no. of fetches
      if (RtsFlags.ParFlags.ParStats.Global &&
	  RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
	globalParStats.tot_fish_mess++;
      }
    }
  }
}  /* processFish */

/*
 * processFetch either returns the requested data (if available) 
 * or blocks the remote blocking queue on a black hole (if not).
 */

//@cindex processFetch
static void
processFetch(void)
{
  globalAddr ga, rga;
  int load;
  StgClosure *closure;
  StgInfoTable *ip;

  unpackFetch(&ga, &rga, &load);
  IF_PAR_DEBUG(fetch,
	       belch("%%%%__ Rcvd Fetch for ((%x, %d, 0)), Resume ((%x, %d, %x)) (load %d) from %x",
		     ga.payload.gc.gtid, ga.payload.gc.slot,
		     rga.payload.gc.gtid, rga.payload.gc.slot, rga.weight, load,
		     rga.payload.gc.gtid));

  closure = GALAlookup(&ga);
  ASSERT(closure != (StgClosure *)NULL);
  ip = get_itbl(closure);
  if (ip->type == FETCH_ME) {
    /* Forward the Fetch to someone else */
    sendFetch(((StgFetchMe *)closure)->ga, &rga, load);

    // Global statistics: count no. of fetches
    if (RtsFlags.ParFlags.ParStats.Global &&
	RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
      globalParStats.tot_fetch_mess++;
    }
  } else if (rga.payload.gc.gtid == mytid) {
    /* Our own FETCH forwarded back around to us */
    StgFetchMeBlockingQueue *fmbq = (StgFetchMeBlockingQueue *)GALAlookup(&rga);
    
    IF_PAR_DEBUG(fetch,
		 belch("%%%%== Fetch returned to sending PE; closure=%p (%s); receiver=%p (%s)",
		       closure, info_type(closure), fmbq, info_type((StgClosure*)fmbq)));
    /* We may have already discovered that the fetch target is our own. */
    if ((StgClosure *)fmbq != closure) 
      CommonUp((StgClosure *)fmbq, closure);
    (void) addWeight(&rga);
  } else if (IS_BLACK_HOLE(closure)) {
    /* This includes RBH's and FMBQ's */
    StgBlockedFetch *bf;

    /* Can we assert something on the remote GA? */
    ASSERT(GALAlookup(&rga) == NULL);

    /* If we're hitting a BH or RBH or FMBQ we have to put a BLOCKED_FETCH
       closure into the BQ in order to denote that when updating this node
       the result should be sent to the originator of this fetch message. */
    bf = (StgBlockedFetch *)createBlockedFetch(ga, rga);
    IF_PAR_DEBUG(fetch,
		 belch("%%++ Blocking Fetch ((%x, %d, %x)) on %p (%s)",
		       rga.payload.gc.gtid, rga.payload.gc.slot, rga.weight, 
		       closure, info_type(closure)));
    blockFetch(bf, closure);
  } else {			
    /* The target of the FetchMe is some local graph */
    nat size;
    // StgClosure *graph;
    rtsPackBuffer *buffer = (rtsPackBuffer *)NULL;

    if ((buffer = PackNearbyGraph(closure, END_TSO_QUEUE, &size, rga.payload.gc.gtid)) == NULL) {
      barf("processFetch: out of heap while packing graph; ToDo: call GC here");
      GarbageCollect(GetRoots, rtsFalse); 
      closure = GALAlookup(&ga);
      buffer = PackNearbyGraph(closure, END_TSO_QUEUE, &size, rga.payload.gc.gtid);
      ASSERT(buffer != (rtsPackBuffer *)NULL);
    }
    sendResume(&rga, size, buffer);

    // Global statistics: count no. of fetches
    if (RtsFlags.ParFlags.ParStats.Global &&
	RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
      globalParStats.tot_resume_mess++;
    }
  }
}

/* 
   The list of pending fetches must be a root-list for GC.
   This routine is called from GC.c (same as marking GAs etc).
*/
void
markPendingFetches(rtsBool major_gc) {

  /* No need to traverse the list; this is done via the scavenge code
     for a BLOCKED_FETCH closure, which evacuates the link field */

  if (PendingFetches != END_BF_QUEUE ) {
    IF_PAR_DEBUG(tables,
		 fprintf(stderr, "@@@@ PendingFetches is root; evaced from %p to",
			 PendingFetches));

    PendingFetches = MarkRoot((StgClosure*)PendingFetches);

    IF_PAR_DEBUG(verbose,
		 fprintf(stderr, " %p\n", PendingFetches));

  } else {
    IF_PAR_DEBUG(tables,
		 fprintf(stderr, "@@@@ PendingFetches is empty; no need to mark it\n"));
  }
}

/*
 * processFree unpacks a FREE message and adds the weights to our GAs.
 */
//@cindex processFree
static void
processFree(void)
{
  int nelem;
  static StgWord *buffer;
  int i;
  globalAddr ga;

  buffer = (StgWord *)gumPackBuffer;
  unpackFree(&nelem, buffer);
  IF_PAR_DEBUG(free,
	       belch("!!__ Rcvd Free (%d GAs)", nelem / 2));

  ga.payload.gc.gtid = mytid;
  for (i = 0; i < nelem;) {
    ga.weight = (rtsWeight) buffer[i++];
    ga.payload.gc.slot = (int) buffer[i++];
    IF_PAR_DEBUG(free,
		 fprintf(stderr, "!!-- Processing free "); 
		 printGA(&ga);
		 fputc('\n', stderr);
		 );
    (void) addWeight(&ga);
  }
}

/*
 * processResume unpacks a RESUME message into the graph, filling in
 * the LA -> GA, and GA -> LA tables. Threads blocked on the original
 * FetchMe (now a blocking queue) are awakened, and the blocking queue
 * is converted into an indirection.  Finally it sends an ACK in response
 * which contains any newly allocated GAs.
 */

//@cindex processResume
static void
processResume(GlobalTaskId sender)
{
  int nelem;
  nat nGAs;
  static rtsPackBuffer *packBuffer;
  StgClosure *newGraph, *old;
  globalAddr lga;
  globalAddr *gagamap;
  
  packBuffer = (rtsPackBuffer *)gumPackBuffer;
  unpackResume(&lga, &nelem, packBuffer);

  IF_PAR_DEBUG(fetch,
	       fprintf(stderr, "[]__ Rcvd Resume for "); 
	       printGA(&lga);
	       fputc('\n', stderr));
  IF_PAR_DEBUG(packet,
	       PrintPacket((rtsPackBuffer *)packBuffer));
  
  /* 
   * We always unpack the incoming graph, even if we've received the
   * requested node in some other data packet (and already awakened
   * the blocking queue).
  if (SAVE_Hp + packBuffer[0] >= SAVE_HpLim) {
    ReallyPerformThreadGC(packBuffer[0], rtsFalse);
    SAVE_Hp -= packBuffer[0];
  }
   */

  // ToDo: Check for GC here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  /* Do this *after* GC; we don't want to release the object early! */

  if (lga.weight > 0)
    (void) addWeight(&lga);

  old = GALAlookup(&lga);

  /* ToDo:  The closure that requested this graph must be one of these two?*/
  ASSERT(get_itbl(old)->type == FETCH_ME_BQ || 
	 get_itbl(old)->type == RBH);

  if (RtsFlags.ParFlags.ParStats.Full) {
    StgBlockingQueueElement *bqe, *last_bqe;

    IF_PAR_DEBUG(fetch,
		 belch("[]-- Resume is REPLY to closure %lx", old));

    /* Write REPLY events to the log file, indicating that the remote
       data has arrived 
       NB: we emit a REPLY only for the *last* elem in the queue; this is
           the one that triggered the fetch message; all other entries
	   have just added themselves to the queue, waiting for the data 
	   they know that has been requested (see entry code for FETCH_ME_BQ)
    */
    if ((get_itbl(old)->type == FETCH_ME_BQ ||
	 get_itbl(old)->type == RBH)) {
      for (bqe = ((StgFetchMeBlockingQueue *)old)->blocking_queue,
	   last_bqe = END_BQ_QUEUE;
	     get_itbl(bqe)->type==TSO || 
	     get_itbl(bqe)->type==BLOCKED_FETCH;
	   last_bqe = bqe, bqe = bqe->link) { /* nothing */ }

      ASSERT(last_bqe==END_BQ_QUEUE || 
	     get_itbl((StgClosure *)last_bqe)->type == TSO);

      /* last_bqe now points to the TSO that triggered the FETCH */ 
      if (get_itbl((StgClosure *)last_bqe)->type == TSO)
	DumpRawGranEvent(CURRENT_PROC, taskIDtoPE(sender), 
			 GR_REPLY, ((StgTSO *)last_bqe), ((StgTSO *)last_bqe)->block_info.closure,
			 0, spark_queue_len(&(MainRegTable.rSparks)));
    }
  }

  newGraph = UnpackGraph(packBuffer, &gagamap, &nGAs);
  ASSERT(newGraph != NULL);

  /* 
   * Sometimes, unpacking will common up the resumee with the
   * incoming graph, but if it hasn't, we'd better do so now.
   */
   
  if (get_itbl(old)->type == FETCH_ME_BQ)
    CommonUp(old, newGraph);

  IF_PAR_DEBUG(fetch,
	       belch("[]-- Ready to resume unpacked graph at %p (%s)",
		     newGraph, info_type(newGraph)));

  IF_PAR_DEBUG(tables,
	       DebugPrintGAGAMap(gagamap, nGAs));
  
  sendAck(sender, nGAs, gagamap);
}

/*
 * processSchedule unpacks a SCHEDULE message into the graph, filling
 * in the LA -> GA, and GA -> LA tables. The root of the graph is added to
 * the local spark queue.  Finally it sends an ACK in response
 * which contains any newly allocated GAs.
 */
//@cindex processSchedule
static void
processSchedule(GlobalTaskId sender)
{
  nat nelem, nGAs;
  rtsBool success;
  static rtsPackBuffer *packBuffer;
  StgClosure *newGraph;
  globalAddr *gagamap;
  
  packBuffer = gumPackBuffer;		/* HWL */
  unpackSchedule(&nelem, packBuffer);

  IF_PAR_DEBUG(schedule,
	       belch("--__ Rcvd Schedule (%d elems)", nelem));
  IF_PAR_DEBUG(packet,
	       PrintPacket(packBuffer));

  /*
   * For now, the graph is a closure to be sparked as an advisory
   * spark, but in future it may be a complete spark with
   * required/advisory status, priority etc.
   */

  /*
  space_required = packBuffer[0];
  if (SAVE_Hp + space_required >= SAVE_HpLim) {
    ReallyPerformThreadGC(space_required, rtsFalse);
    SAVE_Hp -= space_required;
  }
  */
  // ToDo: check whether GC is necessary !!!!!!!!!!!!!!!!!!!!!
  newGraph = UnpackGraph(packBuffer, &gagamap, &nGAs);
  ASSERT(newGraph != NULL);
  success = add_to_spark_queue(newGraph, &(MainRegTable.rSparks));

  if (RtsFlags.ParFlags.ParStats.Full && 
      RtsFlags.ParFlags.ParStats.Sparks && 
      success) 
    DumpRawGranEvent(CURRENT_PROC, CURRENT_PROC, 
		     GR_STOLEN, ((StgTSO *)NULL), newGraph, 
		     0, 0 /* spark_queue_len(ADVISORY_POOL) */);

  IF_PAR_DEBUG(schedule,
	       if (success)
  	         belch("--^^  added spark to unpacked graph %p (%s); %d sparks available on [%x] (%s)", 
		     newGraph, info_type(newGraph), spark_queue_len(&(MainRegTable.rSparks)), mytid);
	       else
                 belch("--^^  received non-sparkable closure %p (%s); nothing added to spark pool; %d sparks available on [%x]", 
		     newGraph, info_type(newGraph), spark_queue_len(&(MainRegTable.rSparks)), mytid));
  IF_PAR_DEBUG(packet,
	       belch("*<    Unpacked graph with root at %p (%s):", 
		     newGraph, info_type(newGraph));
	       PrintGraph(newGraph, 0));

  IF_PAR_DEBUG(tables,
  	       DebugPrintGAGAMap(gagamap, nGAs));

  sendAck(sender, nGAs, gagamap);

  //fishing = rtsFalse;
  ASSERT(outstandingFishes>0);
  outstandingFishes--;
}

/*
 * processAck unpacks an ACK, and uses the GAGA map to convert RBH's
 * (which represent shared thunks that have been shipped) into fetch-mes
 * to remote GAs.
 */
//@cindex processAck
static void
processAck(void)
{
  nat nGAs;
  globalAddr *gaga;
  globalAddr gagamap[256]; // ToDo: elim magic constant!!   MAX_GAS * 2];??

  unpackAck(&nGAs, gagamap);

  IF_PAR_DEBUG(tables,
	       belch(",,,, Rcvd Ack (%d pairs)", nGAs);
	       DebugPrintGAGAMap(gagamap, nGAs));

  IF_DEBUG(sanity,
	   checkGAGAMap(gagamap, nGAs));

  /*
   * For each (oldGA, newGA) pair, set the GA of the corresponding
   * thunk to the newGA, convert the thunk to a FetchMe, and return
   * the weight from the oldGA.
   */
  for (gaga = gagamap; gaga < gagamap + nGAs * 2; gaga += 2) {
    StgClosure *old_closure = GALAlookup(gaga);
    StgClosure *new_closure = GALAlookup(gaga + 1);

    ASSERT(old_closure != NULL);
    if (new_closure == NULL) {
      /* We don't have this closure, so we make a fetchme for it */
      globalAddr *ga = setRemoteGA(old_closure, gaga + 1, rtsTrue);
      
      /* convertToFetchMe should be done unconditionally here.
	 Currently, we assign GAs to CONSTRs, too, (a bit of a hack),
	 so we have to check whether it is an RBH before converting

	 ASSERT(get_itbl(old_closure)==RBH);
      */
      if (get_itbl(old_closure)->type==RBH)
	convertToFetchMe((StgRBH *)old_closure, ga);
    } else {
      /* 
       * Oops...we've got this one already; update the RBH to
       * point to the object we already know about, whatever it
       * happens to be.
       */
      CommonUp(old_closure, new_closure);
      
      /* 
       * Increase the weight of the object by the amount just
       * received in the second part of the ACK pair.
       */
      (void) addWeight(gaga + 1);
    }
    (void) addWeight(gaga);
  }

  /* check the sanity of the LAGA and GALA tables after mincing them */
  IF_DEBUG(sanity, checkLAGAtable(rtsFalse));
}

#ifdef DIST

void
bounceReval(void) {  
  barf("Task %x: TODO: should send NACK in response to REVAL",mytid);	  
}

static void
processReval(GlobalTaskId sender) //similar to schedule...
{ nat nelem, space_required, nGAs;
  static rtsPackBuffer *packBuffer;
  StgClosure *newGraph;
  globalAddr *gagamap;
  StgTSO*     tso;
  globalAddr *ga;
  
  packBuffer = gumPackBuffer;		/* HWL */
  unpackSchedule(&nelem, packBuffer); /* okay, since the structure is the same */

  IF_PAR_DEBUG(packet,
	       belch("@;~) [%x] Rcvd Reval (%d elems)", mytid, nelem);
	       PrintPacket(packBuffer));

  /*
  space_required = packBuffer[0];
  if (SAVE_Hp + space_required >= SAVE_HpLim) {
    ReallyPerformThreadGC(space_required, rtsFalse);
    SAVE_Hp -= space_required;
  }
  */
  
  // ToDo: check whether GC is necessary !!!!!!!!!!!!!!!!!!!!!
  newGraph = UnpackGraph(packBuffer, &gagamap, &nGAs);
  ASSERT(newGraph != NULL);
  
  IF_PAR_DEBUG(packet,
	       belch("@;~)  Unpacked graph with root at %p (%s):", 
		     newGraph, info_type(newGraph));
	       PrintGraph(newGraph, 0));

  IF_PAR_DEBUG(tables,
  	       DebugPrintGAGAMap(gagamap, nGAs));

  IF_PAR_DEBUG(tables, 
    printLAGAtable();   
    DebugPrintGAGAMap(gagamap, nGAs));   

  //We don't send an Ack to the head!!!!
  ASSERT(nGAs>0);  
  sendAck(sender, nGAs-1, gagamap+2);
  
  IF_PAR_DEBUG(verbose,
	       belch("@;~)  About to create Reval thread on behalf of %x", 
		     sender));
  
  tso=createGenThread(RtsFlags.GcFlags.initialStkSize,newGraph);
  tso->priority=RevalPriority;
  tso->revalSlot=gagamap->payload.gc.slot;//record who sent the reval
  tso->revalTid =gagamap->payload.gc.gtid;
  scheduleThread(tso);
  context_switch = 1; // switch at the earliest opportunity
} 
#endif


//@node GUM Message Processor, Miscellaneous Functions, Message-Processing Functions, High Level Communications Routines
//@subsection GUM Message Processor

/*
 * GUM Message Processor

 * processMessages processes any messages that have arrived, calling
 * appropriate routines depending on the message tag
 * (opcode). N.B. Unless profiling it assumes that there {\em ARE} messages
 * present and performs a blocking receive! During profiling it
 * busy-waits in order to record idle time.
 */

//@cindex processMessages
rtsBool
processMessages(void)
{
  rtsPacket packet;
  OpCode opcode;
  GlobalTaskId task;
  rtsBool receivedFinish = rtsFalse;

  do {
    packet = GetPacket();  /* Get next message; block until one available */
    getOpcodeAndSender(packet, &opcode, &task);

    if (task==SysManTask) { 
      switch (opcode) { 
      case PP_PETIDS:
	processPEtids();
	break;
	  
      case PP_FINISH:
	IF_PAR_DEBUG(verbose,
		     belch("==== received FINISH [%p]", mytid));
	/* this boolean value is returned and propagated to the main 
	   scheduling loop, thus shutting-down this PE */
	receivedFinish = rtsTrue;
	break;  
	  
      default:  
	barf("Task %x: received unknown opcode %x from SysMan",mytid, opcode);
      }
    } else if (taskIDtoPE(task)==0) { 
      /* When a new PE joins then potentially FISH & REVAL message may
	 reach PES before they are notified of the new PEs existance.  The
	 only solution is to bounce/fail these messages back to the sender.
	 But we will worry about it once we start seeing these race
	 conditions!  */
      switch (opcode) { 
      case PP_FISH:
	bounceFish();
	break;
#ifdef DIST	  
      case PP_REVAL:
	bounceReval();
	break;	  
#endif          
      case PP_PETIDS:
	belch("Task %x: Ignoring PVM session opened by another SysMan %x",mytid,task);
	break;
        
      case PP_FINISH:   
	break;
	
      default:  
	belch("Task %x: Ignoring opcode %x from unknown PE %x",mytid, opcode, task);
      }
    } else
      switch (opcode) {
      case PP_FETCH:
	processFetch();
	// Global statistics: count no. of fetches
	if (RtsFlags.ParFlags.ParStats.Global &&
	    RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
	  globalParStats.rec_fetch_mess++;
	}
	break;

      case PP_RESUME:
	processResume(task);
	// Global statistics: count no. of fetches
	if (RtsFlags.ParFlags.ParStats.Global &&
	    RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
	  globalParStats.rec_resume_mess++;
	}
	break;

      case PP_ACK:
	processAck();
	break;

      case PP_FISH:
	processFish();
	// Global statistics: count no. of fetches
	if (RtsFlags.ParFlags.ParStats.Global &&
	    RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
	  globalParStats.rec_fish_mess++;
	}
	break;

      case PP_FREE:
	processFree();
	break;
      
      case PP_SCHEDULE:
	processSchedule(task);
	// Global statistics: count no. of fetches
	if (RtsFlags.ParFlags.ParStats.Global &&
	    RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
	  globalParStats.rec_schedule_mess++;
	}
	break;
      
#ifdef DIST      
      case PP_REVAL:
	processReval(task);
	// Global statistics: count no. of fetches
	if (RtsFlags.ParFlags.ParStats.Global &&
	    RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
	  globalParStats.rec_reval_mess++;
	}
	break;
#endif
      
      default:
	/* Anything we're not prepared to deal with. */
	barf("Task %x: Unexpected opcode %x from %x",
	     mytid, opcode, task);
      } /* switch */

  } while (PacketsWaiting());	/* While there are messages: process them */
  return receivedFinish;
}				/* processMessages */

//@node Miscellaneous Functions, Index, GUM Message Processor, High Level Communications Routines
//@subsection Miscellaneous Functions

/*
 * blockFetch blocks a BlockedFetch node on some kind of black hole.
 */
//@cindex blockFetch
void
blockFetch(StgBlockedFetch *bf, StgClosure *bh) {
  bf->node = bh;
  switch (get_itbl(bh)->type) {
  case BLACKHOLE:
    bf->link = END_BQ_QUEUE;
    //((StgBlockingQueue *)bh)->header.info = &stg_BLACKHOLE_BQ_info;
    SET_INFO(bh, &stg_BLACKHOLE_BQ_info); // turn closure into a blocking queue
    ((StgBlockingQueue *)bh)->blocking_queue = (StgBlockingQueueElement *)bf;
    
    // put bh on the mutables list
    recordMutable((StgMutClosure *)bh);
    break;
    
  case BLACKHOLE_BQ:
    /* enqueue bf on blocking queue of closure bh */
    bf->link = ((StgBlockingQueue *)bh)->blocking_queue;
    ((StgBlockingQueue *)bh)->blocking_queue = (StgBlockingQueueElement *)bf;

    // put bh on the mutables list; ToDo: check
    recordMutable((StgMutClosure *)bh);
    break;

  case FETCH_ME_BQ:
    /* enqueue bf on blocking queue of closure bh */
    bf->link = ((StgFetchMeBlockingQueue *)bh)->blocking_queue;
    ((StgFetchMeBlockingQueue *)bh)->blocking_queue = (StgBlockingQueueElement *)bf;

    // put bh on the mutables list; ToDo: check
    recordMutable((StgMutClosure *)bh);
    break;
    
  case RBH:
    /* enqueue bf on blocking queue of closure bh */
    bf->link = ((StgRBH *)bh)->blocking_queue;
    ((StgRBH *)bh)->blocking_queue = (StgBlockingQueueElement *)bf;

    // put bh on the mutables list; ToDo: check
    recordMutable((StgMutClosure *)bh);
    break;
    
  default:
    barf("blockFetch: thought %p was a black hole (IP %#lx, %s)",
	 (StgClosure *)bh, get_itbl((StgClosure *)bh), 
	 info_type((StgClosure *)bh));
  }
  IF_PAR_DEBUG(bq,
	       belch("##++ blockFetch: after block the BQ of %p (%s) is:",
		     bh, info_type(bh));
	       print_bq(bh));
}


/*
  @blockThread@ is called from the main scheduler whenever tso returns with
  a ThreadBlocked return code; tso has already been added to a blocking
  queue (that's done in the entry code of the closure, because it is a 
  cheap operation we have to do in any case); the main purpose of this
  routine is to send a Fetch message in case we are blocking on a FETCHME(_BQ)
  closure, which is indicated by the tso.why_blocked field;
  we also write an entry into the log file if we are generating one

  Should update exectime etc in the entry code already; but we don't have
  something like ``system time'' in the log file anyway, so this should
  even out the inaccuracies.
*/

//@cindex blockThread
void
blockThread(StgTSO *tso)
{
  globalAddr *remote_ga=NULL;
  globalAddr *local_ga;
  globalAddr fmbq_ga;

  // ASSERT(we are on some blocking queue)
  ASSERT(tso->block_info.closure != (StgClosure *)NULL);

  /*
    We have to check why this thread has been blocked.
  */
  switch (tso->why_blocked) {
    case BlockedOnGA:
      /* the closure must be a FETCH_ME_BQ; tso came in here via 
	 FETCH_ME entry code */
      ASSERT(get_itbl(tso->block_info.closure)->type==FETCH_ME_BQ);

      /* HACK: the link field is used to hold the GA between FETCH_ME_entry
	 end this point; if something (eg. GC) happens inbetween the whole
	 thing will blow up 
	 The problem is that the ga field of the FETCH_ME has been overwritten
	 with the head of the blocking queue (which is tso). 
      */
      ASSERT(looks_like_ga(&theGlobalFromGA));
      // ASSERT(tso->link!=END_TSO_QUEUE && tso->link!=NULL);
      remote_ga = &theGlobalFromGA; //tso->link;
      tso->link = (StgTSO*)END_BQ_QUEUE;
      /* it was tso which turned node from FETCH_ME into FETCH_ME_BQ =>
	 we have to send a Fetch message here! */
      if (RtsFlags.ParFlags.ParStats.Full) {
	/* Note that CURRENT_TIME may perform an unsafe call */
	tso->par.exectime += CURRENT_TIME - tso->par.blockedat;
	tso->par.fetchcount++;
	tso->par.blockedat = CURRENT_TIME;
	/* we are about to send off a FETCH message, so dump a FETCH event */
	DumpRawGranEvent(CURRENT_PROC, 
			 taskIDtoPE(remote_ga->payload.gc.gtid),
			 GR_FETCH, tso, tso->block_info.closure, 0, 0);
      }
      /* Phil T. claims that this was a workaround for a hard-to-find
       * bug, hence I'm leaving it out for now --SDM 
       */
      /* Assign a brand-new global address to the newly created FMBQ  */
      local_ga = makeGlobal(tso->block_info.closure, rtsFalse);
      splitWeight(&fmbq_ga, local_ga);
      ASSERT(fmbq_ga.weight == 1U << (BITS_IN(unsigned) - 1));
      
      sendFetch(remote_ga, &fmbq_ga, 0/*load*/);

      // Global statistics: count no. of fetches
      if (RtsFlags.ParFlags.ParStats.Global &&
	  RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
	globalParStats.tot_fetch_mess++;
      }

      IF_DEBUG(sanity,
	       theGlobalFromGA.payload.gc.gtid = (GlobalTaskId)0);
      break;

    case BlockedOnGA_NoSend:
      /* the closure must be a FETCH_ME_BQ; tso came in here via 
	 FETCH_ME_BQ entry code */
      ASSERT(get_itbl(tso->block_info.closure)->type==FETCH_ME_BQ);

      /* Fetch message has been sent already */
      if (RtsFlags.ParFlags.ParStats.Full) {
	/* Note that CURRENT_TIME may perform an unsafe call */
	tso->par.exectime += CURRENT_TIME - tso->par.blockedat;
	tso->par.blockcount++;
	tso->par.blockedat = CURRENT_TIME;
	/* dump a block event, because fetch has been sent already */
	DumpRawGranEvent(CURRENT_PROC, thisPE,
			 GR_BLOCK, tso, tso->block_info.closure, 0, 0);
      }
      break;

    case BlockedOnMVar:
    case BlockedOnBlackHole:
      /* the closure must be a BLACKHOLE_BQ or an RBH; tso came in here via 
	 BLACKHOLE(_BQ) or CAF_BLACKHOLE or RBH entry code */
      ASSERT(get_itbl(tso->block_info.closure)->type==MVAR ||
	     get_itbl(tso->block_info.closure)->type==BLACKHOLE_BQ ||
	     get_itbl(tso->block_info.closure)->type==RBH);

      /* if collecting stats update the execution time etc */
      if (RtsFlags.ParFlags.ParStats.Full) {
	/* Note that CURRENT_TIME may perform an unsafe call */
	tso->par.exectime += CURRENT_TIME - tso->par.blockedat;
	tso->par.blockcount++;
	tso->par.blockedat = CURRENT_TIME;
	DumpRawGranEvent(CURRENT_PROC, thisPE,
			 GR_BLOCK, tso, tso->block_info.closure, 0, 0);
      }
      break;

    case BlockedOnDelay:
      /* Whats sort of stats shall we collect for an explicit threadDelay? */
      IF_PAR_DEBUG(verbose,
	       belch("##++ blockThread: TSO %d blocked on ThreadDelay",
		     tso->id));
      break;

    /* Check that the following is impossible to happen, indeed
    case BlockedOnException:
    case BlockedOnRead:
    case BlockedOnWrite:
    */
    default:
      barf("blockThread: impossible why_blocked code %d for TSO %d",
	   tso->why_blocked, tso->id);
  }

  IF_PAR_DEBUG(verbose,
	       belch("##++ blockThread: TSO %d blocked on closure %p (%s); %s",
		     tso->id, tso->block_info.closure, info_type(tso->block_info.closure),
		     (tso->why_blocked==BlockedOnGA) ? "Sent FETCH for GA" : ""));
  
  IF_PAR_DEBUG(bq,
	       print_bq(tso->block_info.closure));
}

/*
 * ChoosePE selects a GlobalTaskId from the array of PEs 'at random'.
 * Important properties:
 *   - it varies during execution, even if the PE is idle
 *   - it's different for each PE
 *   - we never send a fish to ourselves
 */
extern long lrand48 (void);

//@cindex choosePE
GlobalTaskId
choosePE(void)
{
  long temp;

  temp = lrand48() % nPEs;
  if (allPEs[temp] == mytid) {	/* Never send a FISH to yourself */
    temp = (temp + 1) % nPEs;
  }
  return allPEs[temp];
}

/* 
 * allocate a BLOCKED_FETCH closure and fill it with the relevant fields
 * of the ga argument; called from processFetch when the local closure is
 * under evaluation
 */
//@cindex createBlockedFetch
StgClosure *
createBlockedFetch (globalAddr ga, globalAddr rga)
{
  StgBlockedFetch *bf;
  StgClosure *closure;

  closure = GALAlookup(&ga);
  if ((bf = (StgBlockedFetch *)allocate(_HS + sizeofW(StgBlockedFetch))) == NULL) {
    barf("createBlockedFetch: out of heap while allocating heap for a BlocekdFetch; ToDo: call GC here");
    GarbageCollect(GetRoots, rtsFalse); 
    closure = GALAlookup(&ga);
    bf = (StgBlockedFetch *)allocate(_HS + sizeofW(StgBlockedFetch));
    // ToDo: check whether really guaranteed to succeed 2nd time around
  }

  ASSERT(bf != (StgBlockedFetch *)NULL);
  SET_INFO((StgClosure *)bf, &stg_BLOCKED_FETCH_info);
  // ToDo: check whether other header info is needed
  bf->node = closure;
  bf->ga.payload.gc.gtid = rga.payload.gc.gtid;
  bf->ga.payload.gc.slot = rga.payload.gc.slot;
  bf->ga.weight = rga.weight;
  // bf->link = NULL;  debugging

  IF_PAR_DEBUG(schedule,
	       fprintf(stderr, "%%%%// created BF: bf=%p (%s) of closure , GA: ",
		       bf, info_type((StgClosure*)bf));
	       printGA(&(bf->ga));
	       fputc('\n',stderr));
  return (StgClosure *)bf;
}

/*
 * waitForTermination enters a loop ignoring spurious messages while
 * waiting for the termination sequence to be completed.  
 */
//@cindex waitForTermination
void
waitForTermination(void)
{
  do {
    rtsPacket p = GetPacket();
    processUnexpectedMessage(p);
  } while (rtsTrue);
}

#ifdef DEBUG
//@cindex DebugPrintGAGAMap
void
DebugPrintGAGAMap(globalAddr *gagamap, int nGAs)
{
  nat i;
  
  for (i = 0; i < nGAs; ++i, gagamap += 2)
    fprintf(stderr, "__ gagamap[%d] = ((%x, %d, %x)) -> ((%x, %d, %x))\n", i,
	    gagamap[0].payload.gc.gtid, gagamap[0].payload.gc.slot, gagamap[0].weight,
	    gagamap[1].payload.gc.gtid, gagamap[1].payload.gc.slot, gagamap[1].weight);
}

//@cindex checkGAGAMap
void
checkGAGAMap(globalAddr *gagamap, int nGAs)
{
  nat i;
  
  for (i = 0; i < (nat)nGAs; ++i, gagamap += 2) {
    ASSERT(looks_like_ga(gagamap));
    ASSERT(looks_like_ga(gagamap+1));
  }
}
#endif

//@cindex freeMsgBuffer
static StgWord **freeMsgBuffer = NULL;
//@cindex freeMsgIndex
static nat      *freeMsgIndex  = NULL;

//@cindex prepareFreeMsgBuffers
void
prepareFreeMsgBuffers(void)
{
  nat i;
  
  /* Allocate the freeMsg buffers just once and then hang onto them. */
  if (freeMsgIndex == NULL) {
    freeMsgIndex = (nat *) stgMallocBytes(nPEs * sizeof(nat), 
					  "prepareFreeMsgBuffers (Index)");
    freeMsgBuffer = (StgWord **) stgMallocBytes(nPEs * sizeof(long *), 
					  "prepareFreeMsgBuffers (Buffer)");
    
    for(i = 0; i < nPEs; i++) 
      if (i != (thisPE-1)) 
	freeMsgBuffer[i] = (StgPtr) stgMallocWords(RtsFlags.ParFlags.packBufferSize,
					       "prepareFreeMsgBuffers (Buffer #i)");
      else
	freeMsgBuffer[i] = 0;
  }
  
  /* Initialize the freeMsg buffer pointers to point to the start of their
     buffers */
  for (i = 0; i < nPEs; i++)
    freeMsgIndex[i] = 0;
}

//@cindex freeRemoteGA
void
freeRemoteGA(int pe, globalAddr *ga)
{
  nat i;
  
  ASSERT(GALAlookup(ga) == NULL);
  
  if ((i = freeMsgIndex[pe]) + 2 >= RtsFlags.ParFlags.packBufferSize) {
    IF_PAR_DEBUG(free,
		 belch("!! Filled a free message buffer (sending remaining messages indivisually)"));	

    sendFree(ga->payload.gc.gtid, i, freeMsgBuffer[pe]);
    i = 0;
  }
  freeMsgBuffer[pe][i++] = (StgWord) ga->weight;
  freeMsgBuffer[pe][i++] = (StgWord) ga->payload.gc.slot;
  freeMsgIndex[pe] = i;

  IF_DEBUG(sanity,
	   ga->weight = 0xdead0add;
	   ga->payload.gc.gtid = 0xbbbbbbbb;
	   ga->payload.gc.slot = 0xbbbbbbbb;);
}

//@cindex sendFreeMessages
void
sendFreeMessages(void)
{
  nat i;
  
  for (i = 0; i < nPEs; i++) 
    if (freeMsgIndex[i] > 0)
      sendFree(allPEs[i], freeMsgIndex[i], freeMsgBuffer[i]);
}

/* synchronises with the other PEs. Receives and records in a global
 * variable the task-id of SysMan. If this is the main thread (discovered
 * in main.lc), identifies itself to SysMan. Finally it receives
 * from SysMan an array of the Global Task Ids of each PE, which is
 * returned as the value of the function.
 */

#if defined(PAR_TICKY)
/* Has to see freeMsgIndex, so must be defined here not in ParTicky.c */
//@cindex stats_CntFreeGA
void
stats_CntFreeGA (void) {  // stats only

  // Global statistics: residency of thread and spark pool
  if (RtsFlags.ParFlags.ParStats.Global &&
      RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
    nat i, s;
  
    globalParStats.cnt_free_GA++;
    for (i = 0, s = 0; i < nPEs; i++) 
      s += globalParStats.tot_free_GA += freeMsgIndex[i]/2;

    if ( s > globalParStats.res_free_GA )
      globalParStats.res_free_GA = s;
  }
}
#endif /* PAR_TICKY */

#endif /* PAR -- whole file */

//@node Index,  , Miscellaneous Functions, High Level Communications Routines
//@subsection Index

//@index
//* ACK::  @cindex\s-+ACK
//* DebugPrintGAGAMap::  @cindex\s-+DebugPrintGAGAMap
//* FETCH::  @cindex\s-+FETCH
//* FISH::  @cindex\s-+FISH
//* FREE::  @cindex\s-+FREE
//* RESUME::  @cindex\s-+RESUME
//* SCHEDULE::  @cindex\s-+SCHEDULE
//* blockFetch::  @cindex\s-+blockFetch
//* choosePE::  @cindex\s-+choosePE
//* freeMsgBuffer::  @cindex\s-+freeMsgBuffer
//* freeMsgIndex::  @cindex\s-+freeMsgIndex
//* freeRemoteGA::  @cindex\s-+freeRemoteGA
//* gumPackBuffer::  @cindex\s-+gumPackBuffer
//* initMoreBuffers::  @cindex\s-+initMoreBuffers
//* prepareFreeMsgBuffers::  @cindex\s-+prepareFreeMsgBuffers
//* processAck::  @cindex\s-+processAck
//* processFetch::  @cindex\s-+processFetch
//* processFetches::  @cindex\s-+processFetches
//* processFish::  @cindex\s-+processFish
//* processFree::  @cindex\s-+processFree
//* processMessages::  @cindex\s-+processMessages
//* processResume::  @cindex\s-+processResume
//* processSchedule::  @cindex\s-+processSchedule
//* sendAck::  @cindex\s-+sendAck
//* sendFetch::  @cindex\s-+sendFetch
//* sendFish::  @cindex\s-+sendFish
//* sendFree::  @cindex\s-+sendFree
//* sendFreeMessages::  @cindex\s-+sendFreeMessages
//* sendResume::  @cindex\s-+sendResume
//* sendSchedule::  @cindex\s-+sendSchedule
//* unpackAck::  @cindex\s-+unpackAck
//* unpackFetch::  @cindex\s-+unpackFetch
//* unpackFish::  @cindex\s-+unpackFish
//* unpackFree::  @cindex\s-+unpackFree
//* unpackResume::  @cindex\s-+unpackResume
//* unpackSchedule::  @cindex\s-+unpackSchedule
//* waitForTermination::  @cindex\s-+waitForTermination
//@end index
