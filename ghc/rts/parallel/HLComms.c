/* ----------------------------------------------------------------------------
 * Time-stamp: <Wed Jan 12 2000 13:32:25 Stardate: [-30]4193.86 hwloidl>
 * $Id: HLComms.c,v 1.2 2000/01/13 14:34:07 hwloidl Exp $
 *
 * High Level Communications Routines (HLComms.lc)
 *
 * Contains the high-level routines (i.e. communication
 * subsystem independent) used by GUM
 * 
 * Phil Trinder, Glasgow University, 12 December 1994
 * Adapted for new RTS
 * Phil Trinder, Simon Marlow July 1998
 * H-W. Loidl, Heriot-Watt University, November 1999
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

# ifndef _AIX
# define NON_POSIX_SOURCE /* so says Solaris */
# endif

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
#include "FetchMe.h"     // for BLOCKED_FETCH_info etc
#if defined(DEBUG)
# include "ParallelDebug.h"
#endif
#include "StgMacros.h" // inlined IS_... fcts

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
	       belch("** [%x] Sending Fetch for ((%x, %d, 0)); locally ((%x, %d, %x)), load = %d", 
		     mytid,
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
	       belch("** [%x] Unpacking Fetch for ((%x, %d, 0)) to ((%x, %d, %x)), load = %d", 
		     mytid,
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
sendResume(globalAddr *rga, int nelem, rtsPackBuffer *data) // StgPtr data)
{
  IF_PAR_DEBUG(resume,
	       PrintPacket(data);
	       belch("[] [%x] Sending Resume for ((%x, %d, %x))", 
		     mytid,
		     rga->payload.gc.gtid, rga->payload.gc.slot, rga->weight));

  sendOpNV(PP_RESUME, rga->payload.gc.gtid, 
	   nelem + PACK_BUFFER_HDR_SIZE, (StgPtr)data, 
	   2, (rtsWeight) rga->weight, (StgWord) rga->payload.gc.slot);
}

/*
 * unpackResume unpacks a Resume message into two Global addresses and
 * a data array.
 */

//@cindex unpackResume
static void
unpackResume(globalAddr *lga, int *nelem, rtsPackBuffer *data)
{
    long buf[3];

    GetArgs(buf, 3); 

    IF_PAR_DEBUG(resume,
		 belch("[] [%x] Unpacking Resume for ((%x, %d, %x))", 
		       mytid, mytid,
		       (int) buf[1], (unsigned) buf[0]));

    /*
      RESUME event is written in awaken_blocked_queue
    DumpRawGranEvent(CURRENT_PROC, taskIDtoPE(lga->payload.gc.gtid), 
		     GR_RESUME, END_TSO_QUEUE, (StgClosure *)NULL, 0, 0);
    */

    lga->weight = (unsigned) buf[0];
    lga->payload.gc.gtid = mytid;
    lga->payload.gc.slot = (int) buf[1];

    *nelem = (int) buf[2]; // includes PACK_BUFFER_HDR_SIZE;
    GetArgs(data, *nelem);
    *nelem -= PACK_BUFFER_HDR_SIZE;
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
  IF_PAR_DEBUG(ack,
	       belch(",, [%x] Sending Ack (%d pairs) to PE %x\n", 
		     mytid, ngas, task));

  sendOpN(PP_ACK, task, p - buffer, buffer);
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
  
  IF_PAR_DEBUG(ack,
	       belch(",, [%x] Unpacking Ack (%d pairs) on %x\n", 
		     mytid, *ngas, mytid));

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
	       belch("$$ [%x] Sending Fish to %x (%d outstanding fishes)", 
		     mytid, destPE, outstandingFishes));

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
	       belch("$$ [%x] Unpacking Fish from PE %x (age=%d)", 
		     mytid, (GlobalTaskId) buf[0], (int) buf[1]));

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
		 belch("!! [%x] Sending Free (%d GAs) to %x", 
		       mytid, nelem/2, pe));

    sendOpN(PP_FREE, pe, nelem, data);
}

/*
 * unpackFree unpacks a FREE message into the amount of data shipped and
 * a data block.
 */
//@cindex unpackFree
static void
unpackFree(int *nelem, rtsPackBuffer *data)
{
  long buf[1];
  
  GetArgs(buf, 1);
  *nelem = (int) buf[0];

  IF_PAR_DEBUG(free,
	       belch("!! [%x] Unpacking Free (%d GAs)", 
		     mytid, *nelem/2));

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
sendSchedule(GlobalTaskId origPE, int nelem, rtsPackBuffer *data) // StgPtr data)
{
  IF_PAR_DEBUG(schedule,
	       PrintPacket(data);
	       belch("-- [%x] Sending Schedule (%d elems) to %x\n", 
		     mytid, nelem, origPE));

  sendOpN(PP_SCHEDULE, origPE, nelem + PACK_BUFFER_HDR_SIZE, (StgPtr)data);
}

/*
 * unpackSchedule unpacks a SCHEDULE message into the Global address of
 * the closure shipped, the amount of data shipped (nelem) and the data
 * block (data).
 */

//@cindex unpackSchedule
static void
unpackSchedule(int *nelem, rtsPackBuffer *data)
{
    long buf[1];

    GetArgs(buf, 1);
    /* no. of elems, not counting the header of the pack buffer */
    *nelem = (int) buf[0] - PACK_BUFFER_HDR_SIZE;

    IF_PAR_DEBUG(schedule,
		 belch("-- [%x] Unpacking Schedule (%d elems) on %x\n", 
		       mytid, *nelem));

    /* automatic cast of flat pvm-data to rtsPackBuffer */
    GetArgs(data, *nelem + PACK_BUFFER_HDR_SIZE);
}

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
 * blockFetch blocks a BlockedFetch node on some kind of black hole.
 */
//@cindex blockFetch
static void
blockFetch(StgBlockedFetch *bf, StgClosure *bh) {
  bf->node = bh;
  switch (get_itbl(bh)->type) {
  case BLACKHOLE:
    bf->link = END_BQ_QUEUE;
    //((StgBlockingQueue *)bh)->header.info = &BLACKHOLE_BQ_info;
    SET_INFO(bh, &BLACKHOLE_BQ_info);  // turn closure into a blocking queue
    ((StgBlockingQueue *)bh)->blocking_queue = (StgBlockingQueueElement *)bf;
    
    // put bh on the mutables list
    recordMutable((StgMutClosure *)bh);

# if 0
    /*
     * If we modify a black hole in the old generation, we have to
     * make sure it goes on the mutables list
     */
    
    if (bh <= StorageMgrInfo.OldLim) {
      MUT_LINK(bh) = (StgWord) StorageMgrInfo.OldMutables;
      StorageMgrInfo.OldMutables = bh;
    } else
      MUT_LINK(bh) = MUT_NOT_LINKED;
# endif
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
    barf("Panic (blockFetch): thought %p was a black hole (IP %#lx, %s)",
	 (StgClosure *)bh, get_itbl((StgClosure *)bh), 
	 info_type((StgClosure *)bh));
  }
  IF_PAR_DEBUG(verbose,
	       belch("## blockFetch: after block the BQ of %p (%s) is:",
		     bh, info_type(bh));
	       print_bq(bh));
}


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
  StgBlockedFetch *bf;
  StgClosure *closure, *next;
  StgInfoTable *ip;
  globalAddr rga;
  static rtsPackBuffer *packBuffer;
    
  IF_PAR_DEBUG(verbose,
	       belch("__ processFetches: %d  pending fetches",
		     pending_fetches_len()));
  
  for (bf = PendingFetches; 
       bf != END_BF_QUEUE;
       bf=(StgBlockedFetch *)(bf->link)) {
    /* the PendingFetches list contains only BLOCKED_FETCH closures */
    ASSERT(get_itbl(bf)->type==BLOCKED_FETCH);

    /*
     * Find the target at the end of the indirection chain, and
     * process it in much the same fashion as the original target
     * of the fetch.  Though we hope to find graph here, we could
     * find a black hole (of any flavor) or even a FetchMe.
     */
    closure = bf->node;
    /*
      HACK 312: bf->node may have been evacuated since filling it; follow
       the evacuee in this case; the proper way to handle this is to
       traverse the blocking queue and update the node fields of
       BLOCKED_FETCH entries when evacuating an BLACKHOLE_BQ, FETCH_ME_BQ
       or RBH (but it's late and I'm tired) 
    */
    if (get_itbl(closure)->type == EVACUATED)
      closure = ((StgEvacuated *)closure)->evacuee;

    while ((next = IS_INDIRECTION(closure)) != NULL) { closure = next; }

    ip = get_itbl(closure);
    if (ip->type == FETCH_ME) {
      /* Forward the Fetch to someone else */
      rga.payload.gc.gtid = bf->ga.payload.gc.gtid;
      rga.payload.gc.slot = bf->ga.payload.gc.slot;
      rga.weight = bf->ga.weight;
      
      sendFetch(((StgFetchMe *)closure)->ga, &rga, 0 /* load */);

      IF_PAR_DEBUG(forward,
		   belch("__ processFetches: Forwarding fetch from %lx to %lx",
			 mytid, rga.payload.gc.gtid));

    } else if (IS_BLACK_HOLE(closure)) {
      IF_PAR_DEBUG(verbose,
		   belch("__ processFetches: trying to send a BLACK_HOLE => doign a blockFetch on closure %p (%s)",
			 closure, info_type(closure)));
      bf->node = closure;
      blockFetch(bf, closure);
    } else {
      /* We now have some local graph to send back */
      nat size;

      packBuffer = gumPackBuffer;
      IF_PAR_DEBUG(verbose,
		   belch("__ processFetches: PackNearbyGraph of closure %p (%s)",
			 closure, info_type(closure)));

      if ((packBuffer = PackNearbyGraph(closure, END_TSO_QUEUE, &size)) == NULL) {
	// Put current BF back on list
	bf->link = (StgBlockingQueueElement *)PendingFetches;
	PendingFetches = (StgBlockedFetch *)bf;
	// ToDo: check that nothing more has to be done to prepare for GC!
	GarbageCollect(GetRoots); 
	bf = PendingFetches;
	PendingFetches = (StgBlockedFetch *)(bf->link);
	closure = bf->node;
	packBuffer = PackNearbyGraph(closure, END_TSO_QUEUE, &size);
	ASSERT(packBuffer != (rtsPackBuffer *)NULL);
      }
      rga.payload.gc.gtid = bf->ga.payload.gc.gtid;
      rga.payload.gc.slot = bf->ga.payload.gc.slot;
      rga.weight = bf->ga.weight;
      
      sendResume(&rga, size, packBuffer);
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
  
#if DEBUG
  theGlobalFromGA.payload.gc.gtid = 0;
  theGlobalToGA.payload.gc.gtid = 0;
#endif DEBUG
}
#endif


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
	       belch("$$ [%x] processing fish; %d sparks available",
		     mytid, spark_queue_len(ADVISORY_POOL)));
  while ((spark = findLocalSpark(rtsTrue)) != NULL) {
    nat size;
    // StgClosure *graph;

    packBuffer = gumPackBuffer; 
    ASSERT(closure_SHOULD_SPARK((StgClosure *)spark));
    if ((packBuffer = PackNearbyGraph(spark, END_TSO_QUEUE, &size)) == NULL) {
      IF_PAR_DEBUG(fish,
		   belch("$$ GC while trying to satisfy FISH via PackNearbyGraph of node %p",
			 (StgClosure *)spark));
      GarbageCollect(GetRoots);
      /* Now go back and try again */
    } else {
      IF_PAR_DEBUG(fish,
		   belch("$$ [%x] Replying to FISH from %x by sending graph @ %p (%s)",
			 mytid, origPE, 
			 (StgClosure *)spark, info_type((StgClosure *)spark)));
      sendSchedule(origPE, size, packBuffer);
      disposeSpark(spark);
      break;
    }
  }
  if (spark == (rtsSpark)NULL) {
    IF_PAR_DEBUG(fish,
		 belch("$$ [%x] No sparks available for FISH from %x",
		       mytid, origPE));
    /* We have no sparks to give */
    if (age < FISH_LIFE_EXPECTANCY)
      /* and the fish is atill young, send it to another PE to look for work */
      sendFish(choosePE(), origPE,
	       (age + 1), NEW_FISH_HISTORY, NEW_FISH_HUNGER);

    /* otherwise, send it home to die */
    else
      sendFish(origPE, origPE, (age + 1), NEW_FISH_HISTORY, NEW_FISH_HUNGER);
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
	       belch("%% [%x] Rcvd Fetch for ((%x, %d, 0)), Resume ((%x, %d, %x)) (load %d) from %x",
		     mytid, 
		     ga.payload.gc.gtid, ga.payload.gc.slot,
		     rga.payload.gc.gtid, rga.payload.gc.slot, rga.weight, load,
		     rga.payload.gc.gtid));

  closure = GALAlookup(&ga);
  ASSERT(closure != (StgClosure *)NULL);
  ip = get_itbl(closure);
  if (ip->type == FETCH_ME) {
    /* Forward the Fetch to someone else */
    sendFetch(((StgFetchMe *)closure)->ga, &rga, load);
  } else if (rga.payload.gc.gtid == mytid) {
    /* Our own FETCH forwarded back around to us */
    StgFetchMeBlockingQueue *fmbq = (StgFetchMeBlockingQueue *)GALAlookup(&rga);
    
    IF_PAR_DEBUG(fetch,
		 belch("%% [%x] Fetch returned to sending PE; closure=%p (%s); receiver=%p (%s)",
		       mytid, closure, info_type(closure), fmbq, info_type(fmbq)));
    /* We may have already discovered that the fetch target is our own. */
    if ((StgClosure *)fmbq != closure) 
      CommonUp((StgClosure *)fmbq, closure);
    (void) addWeight(&rga);
  } else if (IS_BLACK_HOLE(closure)) {
    /* This includes RBH's and FMBQ's */
    StgBlockedFetch *bf;

    ASSERT(GALAlookup(&rga) == NULL);

    /* If we're hitting a BH or RBH or FMBQ we have to put a BLOCKED_FETCH
       closure into the BQ in order to denote that when updating this node
       the result should be sent to the originator of this fetch message. */
    bf = (StgBlockedFetch *)createBlockedFetch(ga, rga);
    blockFetch(bf, closure);

    IF_PAR_DEBUG(fetch,
		 belch("%% [%x] Blocking Fetch ((%x, %d, %x)) on %p (%s)",
		       mytid, 
		       rga.payload.gc.gtid, rga.payload.gc.slot, rga.weight, 
		       closure, info_type(closure)));
    } else {			
      /* The target of the FetchMe is some local graph */
      nat size;
      // StgClosure *graph;
      rtsPackBuffer *buffer = (rtsPackBuffer *)NULL;

      if ((buffer = PackNearbyGraph(closure, END_TSO_QUEUE, &size)) == NULL) {
	GarbageCollect(GetRoots); 
	closure = GALAlookup(&ga);
	buffer = PackNearbyGraph(closure, END_TSO_QUEUE, &size);
	ASSERT(buffer != (rtsPackBuffer *)NULL);
      }
      sendResume(&rga, size, buffer);
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
	       belch("!! [%x] Rcvd Free (%d GAs)", mytid, nelem / 2));

  ga.payload.gc.gtid = mytid;
  for (i = 0; i < nelem;) {
    ga.weight = (rtsWeight) buffer[i++];
    ga.payload.gc.slot = (int) buffer[i++];
    IF_PAR_DEBUG(free,
		 fprintf(stderr, "!! [%x] Processing free ", mytid); 
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
  
  packBuffer = gumPackBuffer;
  unpackResume(&lga, &nelem, (StgPtr)packBuffer);

  IF_PAR_DEBUG(resume,
	       fprintf(stderr, "[] [%x] Rcvd Resume for ", mytid); 
	       printGA(&lga);
	       fputc('\n', stderr);
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

  if (RtsFlags.ParFlags.ParStats.Full) {
    // StgTSO *tso = END_TSO_QUEUE;
    StgBlockingQueueElement *bqe;

    /* Write REPLY events to the log file, indicating that the remote
       data has arrived */
    if (get_itbl(old)->type == FETCH_ME_BQ ||
	get_itbl(old)->type == RBH) 
      for (bqe = ((StgFetchMeBlockingQueue *)old)->blocking_queue;
	   bqe->link != END_BQ_QUEUE;
	   bqe = bqe->link)
	if (get_itbl((StgClosure *)bqe)->type == TSO)
	  DumpRawGranEvent(CURRENT_PROC, taskIDtoPE(sender), 
			   GR_REPLY, ((StgTSO *)bqe), ((StgTSO *)bqe)->block_info.closure,
			   0, spark_queue_len(ADVISORY_POOL));
  }

  newGraph = UnpackGraph(packBuffer, &gagamap, &nGAs);
  ASSERT(newGraph != NULL);

  /* 
   * Sometimes, unpacking will common up the resumee with the
   * incoming graph, but if it hasn't, we'd better do so now.
   */
   
  if (get_itbl(old)->type == FETCH_ME_BQ)
    CommonUp(old, newGraph);

  IF_PAR_DEBUG(resume,
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
  nat nelem, space_required, nGAs;
  rtsBool success;
  static rtsPackBuffer *packBuffer;
  StgClosure *newGraph;
  globalAddr *gagamap;
  
  packBuffer = gumPackBuffer;		/* HWL */
  unpackSchedule(&nelem, packBuffer);

  IF_PAR_DEBUG(schedule,
	       belch("-- [%x] Rcvd Schedule (%d elems)", mytid, nelem);
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
  // ToDo: check whether GC is necessary !!!!!!!!!!!!!!!!!!!!!1
  newGraph = UnpackGraph(packBuffer, &gagamap, &nGAs);
  ASSERT(newGraph != NULL);
  success = add_to_spark_queue(newGraph, rtsFalse);

  IF_PAR_DEBUG(pack,
	       if (success)
  	         belch("+* added spark to unpacked graph %p; %d sparks available on [%x]", 
		     newGraph, spark_queue_len(ADVISORY_POOL), mytid);
	       else
                 belch("+* received non-sparkable closure %p; nothing added to spark pool; %d sparks available on [%x]", 
		     newGraph, spark_queue_len(ADVISORY_POOL), mytid);
	       belch("-* Unpacked graph with root at %p (%s):", 
		     newGraph, info_type(newGraph));
	       PrintGraph(newGraph, 0));

  IF_PAR_DEBUG(pack,
  	       DebugPrintGAGAMap(gagamap, nGAs));

  if (nGAs > 0)
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

  IF_PAR_DEBUG(ack,
	       belch(",, [%x] Rcvd Ack (%d pairs)", mytid, nGAs);
	       DebugPrintGAGAMap(gagamap, nGAs));

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
	convertToFetchMe(old_closure, ga);
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
}

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
void
processMessages(void)
{
  rtsPacket packet;
  OpCode opcode;
  GlobalTaskId task;
    
  do {
    packet = GetPacket();  /* Get next message; block until one available */
    getOpcodeAndSender(packet, &opcode, &task);

    switch (opcode) {
    case PP_FINISH:
      IF_PAR_DEBUG(verbose,
		   belch("== [%x] received FINISH", mytid));
      /* setting this global variables eventually terminates the main
         scheduling loop for this PE and causes a shut-down, sending 
	 PP_FINISH to SysMan */
      GlobalStopPending = rtsTrue;
      break;

    case PP_FETCH:
      processFetch();
      break;

    case PP_RESUME:
      processResume(task);
      break;

    case PP_ACK:
      processAck();
      break;

    case PP_FISH:
      processFish();
      break;

    case PP_FREE:
      processFree();
      break;
      
    case PP_SCHEDULE:
      processSchedule(task);
      break;

    default:
      /* Anything we're not prepared to deal with. */
      barf("Task %x: Unexpected opcode %x from %x",
	   mytid, opcode, task);
    } /* switch */

  } while (PacketsWaiting());	/* While there are messages: process them */
}				/* processMessages */

//@node Miscellaneous Functions, Index, GUM Message Processor, High Level Communications Routines
//@subsection Miscellaneous Functions

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
  if ((bf = (StgBlockedFetch *)allocate(FIXED_HS + sizeofW(StgBlockedFetch))) == NULL) {
    GarbageCollect(GetRoots); 
    closure = GALAlookup(&ga);
    bf = (StgBlockedFetch *)allocate(FIXED_HS + sizeofW(StgBlockedFetch));
    // ToDo: check whether really guaranteed to succeed 2nd time around
  }

  ASSERT(bf != (StgClosure *)NULL);
  SET_INFO((StgClosure *)bf, &BLOCKED_FETCH_info);
  // ToDo: check whether other header info is needed
  bf->node = closure;
  bf->ga.payload.gc.gtid = rga.payload.gc.gtid;
  bf->ga.payload.gc.slot = rga.payload.gc.slot;
  bf->ga.weight = rga.weight;
  // bf->link = NULL;  debugging

  IF_PAR_DEBUG(fetch,
	       fprintf(stderr, "%% [%x] created BF: closure=%p (%s), GA: ",
		       mytid, closure, info_type(closure));
	       printGA(&(bf->ga));
	       fputc('\n',stderr));
  return bf;
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
    processUnexpected(p);
  } while (rtsTrue);
}

#ifdef DEBUG
//@cindex DebugPrintGAGAMap
void
DebugPrintGAGAMap(globalAddr *gagamap, int nGAs)
{
  int i;
  
  for (i = 0; i < nGAs; ++i, gagamap += 2)
    fprintf(stderr, "gagamap[%d] = ((%x, %d, %x)) -> ((%x, %d, %x))\n", i,
	    gagamap[0].payload.gc.gtid, gagamap[0].payload.gc.slot, gagamap[0].weight,
	    gagamap[1].payload.gc.gtid, gagamap[1].payload.gc.slot, gagamap[1].weight);
}
#endif

//@cindex freeMsgBuffer
static StgWord **freeMsgBuffer = NULL;
//@cindex freeMsgIndex
static int      *freeMsgIndex  = NULL;

//@cindex prepareFreeMsgBuffers
void
prepareFreeMsgBuffers(void)
{
  int i;
  
  /* Allocate the freeMsg buffers just once and then hang onto them. */
  if (freeMsgIndex == NULL) {
    freeMsgIndex = (int *) stgMallocBytes(nPEs * sizeof(int), 
					  "prepareFreeMsgBuffers (Index)");
    freeMsgBuffer = (StgWord **) stgMallocBytes(nPEs * sizeof(long *), 
					  "prepareFreeMsgBuffers (Buffer)");
    
    for(i = 0; i < nPEs; i++) 
      if (i != thisPE) 
	freeMsgBuffer[i] = (StgPtr) stgMallocWords(RtsFlags.ParFlags.packBufferSize,
					       "prepareFreeMsgBuffers (Buffer #i)");
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
  int i;
  
  ASSERT(GALAlookup(ga) == NULL);
  
  if ((i = freeMsgIndex[pe]) + 2 >= RtsFlags.ParFlags.packBufferSize) {
    IF_PAR_DEBUG(free,
		 belch("Filled a free message buffer (sending remaining messages indivisually)"));	

    sendFree(ga->payload.gc.gtid, i, freeMsgBuffer[pe]);
    i = 0;
  }
  freeMsgBuffer[pe][i++] = (StgWord) ga->weight;
  freeMsgBuffer[pe][i++] = (StgWord) ga->payload.gc.slot;
  freeMsgIndex[pe] = i;

#ifdef DEBUG
  ga->weight = 0x0f0f0f0f;
  ga->payload.gc.gtid = 0x666;
  ga->payload.gc.slot = 0xdeaddead;
#endif
}

//@cindex sendFreeMessages
void
sendFreeMessages(void)
{
  int i;
  
  for (i = 0; i < nPEs; i++) 
    if (freeMsgIndex[i] > 0)
      sendFree(allPEs[i], freeMsgIndex[i], freeMsgBuffer[i]);
}

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
