/* -----------------------------------------------------------------------------
 * 
 * $Id: HLComms.c,v 1.3 1999/02/15 14:30:56 simonm Exp $
 *
 * High Level Communications Routines (HLComms.lc)
 *
 *  Contains the high-level routines (i.e. communication
 *  subsystem independent) used by GUM
 *
 *  Phil Trinder, Glasgow University, 12 December 1994
 *  Adapted for new RTS
 *  Phil Trinder, Simon Marlow July 1998
 * 
 * -------------------------------------------------------------------------- */

#ifdef PAR /* whole file */

#ifndef _AIX
#define NON_POSIX_SOURCE /* so says Solaris */
#endif

#include "Rts.h"
#include "RtsUtils.h"
#include "RtsFlags.h"

#include "HLC.h"
#include "Parallel.h"

/*
 * GUM Message Sending and Unpacking Functions
 * ********************************************
 */

/*
 * Allocate space for message processing
 */

static W_ *gumPackBuffer;

void 
InitMoreBuffers(void)
{
    gumPackBuffer
      = (W_ *) stgMallocWords(RtsFlags.ParFlags.packBufferSize, "initMoreBuffers");
}

/*
 *SendFetch packs the two global addresses and a load into a message +
 *sends it.  
 */

void
sendFetch(globalAddr *rga, globalAddr *lga, int load)
{

    ASSERT(rga->weight > 0 && lga->weight > 0);
#ifdef FETCH_DEBUG    
    fprintf(stderr, "Sending Fetch (%x, %d, 0), load = %d\n", 
      rga->loc.gc.gtid, rga->loc.gc.slot, load);
#endif
    SendOpV(PP_FETCH, rga->loc.gc.gtid, 6,
      (W_) rga->loc.gc.gtid, (W_) rga->loc.gc.slot, 
      (W_) lga->weight, (W_) lga->loc.gc.gtid, (W_) lga->loc.gc.slot, (W_) load);
}

/*
 *unpackFetch unpacks a FETCH message into two Global addresses and a load figure.
 */

static void
unpackFetch(globalAddr *lga, globalAddr *rga, int *load)
{
    long buf[6];

    GetArgs(buf, 6); 
    lga->weight = 1;
    lga->loc.gc.gtid = (GLOBAL_TASK_ID) buf[0];
    lga->loc.gc.slot = (int) buf[1];

    rga->weight = (unsigned) buf[2];
    rga->loc.gc.gtid = (GLOBAL_TASK_ID) buf[3];
    rga->loc.gc.slot = (int) buf[4];

    *load = (int) buf[5];

    ASSERT(rga->weight > 0);
}

/*
 * SendResume packs the remote blocking queue's GA and data into a message 
 * and sends it.
 */

void
sendResume(globalAddr *rga, int nelem, StgPtr data)
{

#ifdef RESUME_DEBUG
    PrintPacket(data);
    fprintf(stderr, "Sending Resume for (%x, %d, %x)\n", 
      rga->loc.gc.gtid, rga->loc.gc.slot, rga->weight);
#endif

    SendOpNV(PP_RESUME, rga->loc.gc.gtid, nelem, data, 2,
      (W_) rga->weight, (W_) rga->loc.gc.slot);

}

/*
 * blockFetch blocks a BlockedFetch node on some kind of black hole.
 */
static void
blockFetch(StgPtr bf, StgPtr bh)
{}

#if 0 
 Empty until Blocked fetches etc defined 
    switch (INFO_TYPE(INFO_PTR(bh))) {
    case INFO_BH_TYPE:
	BF_LINK(bf) = PrelBase_Z91Z93_closure;
	SET_INFO_PTR(bh, BQ_info);
	BQ_ENTRIES(bh) = (W_) bf;

#ifdef GC_MUT_REQUIRED
	/*
	 * If we modify a black hole in the old generation, we have to
	 * make sure it goes on the mutables list
	 */

	if (bh <= StorageMgrInfo.OldLim) {
	    MUT_LINK(bh) = (W_) StorageMgrInfo.OldMutables;
	    StorageMgrInfo.OldMutables = bh;
	} else
	    MUT_LINK(bh) = MUT_NOT_LINKED;
#endif
	break;
    case INFO_BQ_TYPE:
	BF_LINK(bf) = (P_) BQ_ENTRIES(bh);
	BQ_ENTRIES(bh) = (W_) bf;
	break;
    case INFO_FMBQ_TYPE:
	BF_LINK(bf) = (P_) FMBQ_ENTRIES(bh);
	FMBQ_ENTRIES(bh) = (W_) bf;
	break;
    case INFO_SPEC_RBH_TYPE:
	BF_LINK(bf) = (P_) SPEC_RBH_BQ(bh);
	SPEC_RBH_BQ(bh) = (W_) bf;
	break;
    case INFO_GEN_RBH_TYPE:
	BF_LINK(bf) = (P_) GEN_RBH_BQ(bh);
	GEN_RBH_BQ(bh) = (W_) bf;
	break;
    default:
	fprintf(stderr, "Panic: thought %#lx was a black hole (IP %#lx)\n",
	  (W_) bh, INFO_PTR(bh));
	EXIT(EXIT_FAILURE);
    }
}
#endif

/*
 * processFetches constructs and sends resume messages for every
 * BlockedFetch which is ready to be awakened.
 */
extern P_ PendingFetches;

void
processFetches()
{}
 
#if 0
 Empty till closure defined 
    P_ bf;
    P_ next;
    P_ closure;
    P_ ip;
    globalAddr rga;
    
    for (bf = PendingFetches; bf != PrelBase_Z91Z93_closure; bf = next) {
	next = BF_LINK(bf);

	/*
	 * Find the target at the end of the indirection chain, and
	 * process it in much the same fashion as the original target
	 * of the fetch.  Though we hope to find graph here, we could
	 * find a black hole (of any flavor) or even a FetchMe.
	 */
	closure = BF_NODE(bf);
	while (IS_INDIRECTION(INFO_PTR(closure)))
	    closure = (P_) IND_CLOSURE_PTR(closure);
        ip = (P_) INFO_PTR(closure);

	if (INFO_TYPE(ip) == INFO_FETCHME_TYPE) {
            /* Forward the Fetch to someone else */
	    rga.loc.gc.gtid = (GLOBAL_TASK_ID) BF_GTID(bf);
	    rga.loc.gc.slot = (int) BF_SLOT(bf);
	    rga.weight = (unsigned) BF_WEIGHT(bf);

	    sendFetch(FETCHME_GA(closure), &rga, 0 /* load */);
	} else if (IS_BLACK_HOLE(ip)) {
	    BF_NODE(bf) = closure;
	    blockFetch(bf, closure);
	} else {
	    /* We now have some local graph to send back */
	    W_ size;
	    P_ graph;

	    if ((graph = PackNearbyGraph(closure, &size)) == NULL) {
		PendingFetches = bf;
		ReallyPerformThreadGC(PACK_HEAP_REQUIRED, rtsFalse);
		SAVE_Hp -= PACK_HEAP_REQUIRED;
		bf = PendingFetches;
		next = BF_LINK(bf);
		closure = BF_NODE(bf);
		graph = PackNearbyGraph(closure, &size);
		ASSERT(graph != NULL);
	    }
	    rga.loc.gc.gtid = (GLOBAL_TASK_ID) BF_GTID(bf);
	    rga.loc.gc.slot = (int) BF_SLOT(bf);
	    rga.weight = (unsigned) BF_WEIGHT(bf);

	    sendResume(&rga, size, graph);
	}
    }
    PendingFetches = PrelBase_Z91Z93_closure;
}
#endif

/*
 * unpackResume unpacks a Resume message into two Global addresses and
 * a data array.
 */

static void
unpackResume(globalAddr *lga, int *nelem, W_ *data)
{
    long buf[3];

    GetArgs(buf, 3); 
    lga->weight = (unsigned) buf[0];
    lga->loc.gc.gtid = mytid;
    lga->loc.gc.slot = (int) buf[1];

    *nelem = (int) buf[2];
    GetArgs(data, *nelem);
}

/*
 *SendAck packs the global address being acknowledged, together with
 *an array of global addresses for any closures shipped and sends them.
 */

void
sendAck(GLOBAL_TASK_ID task, int ngas, globalAddr *gagamap)
{
    static long *buffer;
    long *p;
    int i;

    buffer = (long *) gumPackBuffer;

    for(i = 0, p = buffer; i < ngas; i++, p += 6) {
        ASSERT(gagamap[1].weight > 0);
	p[0] = (long) gagamap->weight;
	p[1] = (long) gagamap->loc.gc.gtid;
	p[2] = (long) gagamap->loc.gc.slot;
	gagamap++;
	p[3] = (long) gagamap->weight;
	p[4] = (long) gagamap->loc.gc.gtid;
	p[5] = (long) gagamap->loc.gc.slot;
	gagamap++;
    }
#ifdef ACK_DEBUG    
    fprintf(stderr,"Sending Ack (%d pairs) to %x\n", ngas, task);
#endif
    SendOpN(PP_ACK, task, p - buffer, buffer);

}

/*
 *unpackAck unpacks an Acknowledgement message into a Global address,
 *a count of the number of global addresses following and a map of 
 *Global addresses
 */

static void
unpackAck(int *ngas, globalAddr *gagamap)
{
    long GAarraysize;
    long buf[6];

    GetArgs(&GAarraysize, 1);

    *ngas = GAarraysize / 6;

    while (GAarraysize > 0) {
	GetArgs(buf, 6);
	gagamap->weight = (unsigned) buf[0];
	gagamap->loc.gc.gtid = (GLOBAL_TASK_ID) buf[1];
	gagamap->loc.gc.slot = (int) buf[2];
	gagamap++;
	gagamap->weight = (unsigned) buf[3];
	gagamap->loc.gc.gtid = (GLOBAL_TASK_ID) buf[4];
	gagamap->loc.gc.slot = (int) buf[5];
        ASSERT(gagamap->weight > 0);
	gagamap++;
	GAarraysize -= 6;
    }
}

/*
 *SendFish packs the global address being acknowledged, together with
 *an array of global addresses for any closures shipped and sends them.
 */

void
sendFish(GLOBAL_TASK_ID destPE, GLOBAL_TASK_ID origPE, 
	 int age, int history, int hunger)
{

#ifdef FISH_DEBUG
    fprintf(stderr,"Sending Fish to %lx\n", destPE);
#endif
    SendOpV(PP_FISH, destPE, 4, (W_) origPE, (W_) age, (W_) history, (W_) hunger);
    if (origPE == mytid)
	fishing = rtsTrue;

}

/*
 *unpackFish unpacks a FISH message into the global task id of the
 *originating PE and 3 data fields: the age, history and hunger of the
 *fish. The history + hunger are not currently used.
 */

static void
unpackFish(GLOBAL_TASK_ID *origPE, int *age, int *history, int *hunger)
{
    long buf[4];

    GetArgs(buf, 4);

    *origPE = (GLOBAL_TASK_ID) buf[0];
    *age = (int) buf[1];
    *history = (int) buf[2];
    *hunger = (int) buf[3];
}

/*
 *SendFree sends (weight, slot) pairs for GAs that we no longer need references
 *to.
 */
void
sendFree(GLOBAL_TASK_ID pe, int nelem, StgPtr data)
{
#ifdef FREE_DEBUG
    fprintf(stderr, "Sending Free (%d GAs) to %x\n", nelem / 2, pe);
#endif
    SendOpN(PP_FREE, pe, nelem, data);

}


/*
 *unpackFree unpacks a FREE message into the amount of data shipped and
 *a data block.
 */

static void
unpackFree(int *nelem, W_ *data)
{
    long buf[1];

    GetArgs(buf, 1);
    *nelem = (int) buf[0];
    GetArgs(data, *nelem);
}

/*
 *SendSchedule sends a closure to be evaluated in response to a Fish
 *message. The message is directed to the PE that originated the Fish
 *(origPE), and includes the packed closure (data) along with its size
 *(nelem).
 */

void
sendSchedule(GLOBAL_TASK_ID origPE, int nelem, StgPtr data)
{
#ifdef SCHEDULE_DEBUG
    PrintPacket(data);
    fprintf(stderr, "Sending Schedule to %x\n", origPE);
#endif

    SendOpN(PP_SCHEDULE, origPE, nelem, data);
}

/*
 *unpackSchedule unpacks a SCHEDULE message into the Global address of
 *the closure shipped, the amount of data shipped (nelem) and the data
 *block (data).
 */

static void
unpackSchedule(int *nelem, W_ *data)
{
    long buf[1];

    GetArgs(buf, 1);
    *nelem = (int) buf[0];
    GetArgs(data, *nelem);
}

/*
 *Message-Processing Functions
 *
 *The following routines process incoming GUM messages. Often reissuing
 *messages in response.
 *
 *processFish unpacks a fish message, reissuing it if it's our own,
 *sending work if we have it or sending it onwards otherwise.
 *
 * Only stubs now. Real stuff in HLCommsRest PWT
 */
static void
processFish(void)
{}				/* processFish */

/*
 * processFetch either returns the requested data (if available) 
 * or blocks the remote blocking queue on a black hole (if not).
 */
static void
processFetch(void)
{}

/*
 * processFree unpacks a FREE message and adds the weights to our GAs.
 */
static void
processFree(void)
{}

/*
 * processResume unpacks a RESUME message into the graph, filling in
 * the LA -> GA, and GA -> LA tables. Threads blocked on the original
 * FetchMe (now a blocking queue) are awakened, and the blocking queue
 * is converted into an indirection.  Finally it sends an ACK in response
 * which contains any newly allocated GAs.
 */

static void
processResume(GLOBAL_TASK_ID sender)
{}

/*
 * processSchedule unpacks a SCHEDULE message into the graph, filling
 * in the LA -> GA, and GA -> LA tables. The root of the graph is added to
 * the local spark queue.  Finally it sends an ACK in response
 * which contains any newly allocated GAs.
 */
static void
processSchedule(GLOBAL_TASK_ID sender)
{
}

/*
 * processAck unpacks an ACK, and uses the GAGA map to convert RBH's
 * (which represent shared thunks that have been shipped) into fetch-mes
 * to remote GAs.
 */
static void
processAck(void)
{}

/*
 * GUM Message Processor

 * processMessages processes any messages that have arrived, calling
 * appropriate routines depending on the message tag
 * (opcode). N.B. Unless profiling it assumes that there {\em ARE} messages
 * present and performs a blocking receive! During profiling it
 * busy-waits in order to record idle time.
 */

void
processMessages(void)
{
    PACKET packet;
    OPCODE opcode;
    GLOBAL_TASK_ID task;
    
    do {

	packet = GetPacket();	/* Get next message; block until one available */

	get_opcode_and_sender(packet, &opcode, &task);

	switch (opcode) {

	case PP_FINISH:
	    stg_exit(EXIT_SUCCESS);	/* The computation has been completed by someone
				 * else */
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
	    fprintf(stderr, "Task %x: Unexpected opcode %x from %x\n",
	      mytid, opcode, task);

	    stg_exit(EXIT_FAILURE);
	}			/* switch */

    } while (PacketsWaiting());	/* While there are messages: process them */
}				/* processMessages */

/*
 * Miscellaneous Functions
 * 
 *
 * ChoosePE selects a GlobalTaskId from the array of PEs 'at random'.
 * Important properties:
 *   - it varies during execution, even if the PE is idle
 *   - it's different for each PE
 *   - we never send a fish to ourselves
 */
extern long lrand48 (void);

GLOBAL_TASK_ID
choosePE(void)
{
    long temp;

    temp = lrand48() % nPEs;
    if (PEs[temp] == mytid) {	/* Never send a FISH to yourself */
	temp = (temp + 1) % nPEs;
    }
    return PEs[temp];
}

/*
 *WaitForTermination enters a loop ignoring spurious messages while waiting for the
 *termination sequence to be completed.
 */
void
WaitForTermination(void)
{
  do {
    PACKET p = GetPacket();
    ProcessUnexpected(p);
  } while (rtsTrue);
}

#ifdef DEBUG
void
DebugPrintGAGAMap(globalAddr *gagamap, int nGAs)
{
    int i;

    for (i = 0; i < nGAs; ++i, gagamap += 2)
	fprintf(stderr, "gagamap[%d] = (%x, %d, %x) -> (%x, %d, %x)\n", i,
	  gagamap[0].loc.gc.gtid, gagamap[0].loc.gc.slot, gagamap[0].weight,
	  gagamap[1].loc.gc.gtid, gagamap[1].loc.gc.slot, gagamap[1].weight);
}
#endif

static PP_ freeMsgBuffer = NULL;
static int *freeMsgIndex = NULL;

void
prepareFreeMsgBuffers(void)
{
    int i;

    /* Allocate the freeMsg buffers just once and then hang onto them. */

    if (freeMsgIndex == NULL) {

	freeMsgIndex = (int *) stgMallocBytes(nPEs * sizeof(int), "prepareFreeMsgBuffers (Index)");
	freeMsgBuffer = (PP_)  stgMallocBytes(nPEs * sizeof(long *), "prepareFreeMsgBuffers (Buffer)");

	for(i = 0; i < nPEs; i++) {
	    if (i != thisPE) {
	      freeMsgBuffer[i] = (P_) stgMallocWords(RtsFlags.ParFlags.packBufferSize,
					"prepareFreeMsgBuffers (Buffer #i)");
	    }
	}
    }

    /* Initialize the freeMsg buffer pointers to point to the start of their buffers */
    for (i = 0; i < nPEs; i++)
	freeMsgIndex[i] = 0;
}

void
freeRemoteGA(int pe, globalAddr *ga)
{
    int i;

    ASSERT(GALAlookup(ga) == NULL);

    if ((i = freeMsgIndex[pe]) + 2 >= RtsFlags.ParFlags.packBufferSize) {
#ifdef FREE_DEBUG
	fprintf(stderr, "Filled a free message buffer\n");	
#endif
	sendFree(ga->loc.gc.gtid, i, freeMsgBuffer[pe]);
	i = 0;
    }
    freeMsgBuffer[pe][i++] = (W_) ga->weight;
    freeMsgBuffer[pe][i++] = (W_) ga->loc.gc.slot;
    freeMsgIndex[pe] = i;
#ifdef DEBUG
    ga->weight = 0x0f0f0f0f;
    ga->loc.gc.gtid = 0x666;
    ga->loc.gc.slot = 0xdeaddead;
#endif
}

void
sendFreeMessages(void)
{
    int i;

    for (i = 0; i < nPEs; i++) {
	if (freeMsgIndex[i] > 0)
	    sendFree(PEs[i], freeMsgIndex[i], freeMsgBuffer[i]);
    }
}

/* Process messaging code ripped out for the time being -- SDM & PWT */

#ifdef 0
/* These are the remaining message-processing functions from HLComms*/


/*
 *Message-Processing Functions
 *
 *The following routines process incoming GUM messages. Often reissuing
 *messages in response.
 *
 *processFish unpacks a fish message, reissuing it if it's our own,
 *sending work if we have it or sending it onwards otherwise.
 */
static void
processFish(void)
{
    GLOBAL_TASK_ID origPE;
    int age, history, hunger;

    unpackFish(&origPE, &age, &history, &hunger);

    if (origPE == mytid) {
        fishing = rtsFalse;
    } else {
	P_ spark;

	while ((spark = FindLocalSpark(rtsTrue)) != NULL) {
	    W_ size;
	    P_ graph;

	    if ((graph = PackNearbyGraph(spark, &size)) == NULL) {
		ReallyPerformThreadGC(PACK_HEAP_REQUIRED, rtsFalse);
		SAVE_Hp -= PACK_HEAP_REQUIRED;
		/* Now go back and try again */
	    } else {
		sendSchedule(origPE, size, graph);
		DisposeSpark(spark);
		break;
	    }
	}
	if (spark == NULL) {
	    /* We have no sparks to give */
	    if (age < FISH_LIFE_EXPECTANCY)
		sendFish(choosePE(), origPE,
		  (age + 1), NEW_FISH_HISTORY, NEW_FISH_HUNGER);

	    /* Send it home to die */
	    else
		sendFish(origPE, origPE, (age + 1), NEW_FISH_HISTORY, NEW_FISH_HUNGER);
	}
    }
}				/* processFish */

/*
 *processFetch either returns the requested data (if available) 
 *or blocks the remote blocking queue on a black hole (if not).
 */
static void
processFetch(void)
{
    globalAddr ga, rga;
    int load;

    P_ closure;
    P_ ip;

    unpackFetch(&ga, &rga, &load);
#ifdef FETCH_DEBUG
    fprintf(stderr, "Rcvd Fetch for (%x, %d, 0), Resume (%x, %d, %x) (load %d) \n",
      ga.loc.gc.gtid, ga.loc.gc.slot,
      rga.loc.gc.gtid, rga.loc.gc.slot, rga.weight, load);
#endif

    closure = GALAlookup(&ga);
    ip = (P_) INFO_PTR(closure);

    if (INFO_TYPE(ip) == INFO_FETCHME_TYPE) {
	/* Forward the Fetch to someone else */
	sendFetch(FETCHME_GA(closure), &rga, load);
    } else if (rga.loc.gc.gtid == mytid) {
	/* Our own FETCH forwarded back around to us */
	P_ fmbq = GALAlookup(&rga);

	/* We may have already discovered that the fetch target is our own. */
	if (fmbq != closure) 
	    CommonUp(fmbq, closure);
	(void) addWeight(&rga);
    } else if (IS_BLACK_HOLE(ip)) {
	/* This includes RBH's and FMBQ's */
	P_ bf;

	if ((bf = AllocateHeap(FIXED_HS + BF_CLOSURE_SIZE(dummy))) == NULL) {
	    ReallyPerformThreadGC(FIXED_HS + BF_CLOSURE_SIZE(dummy), rtsFalse);
	    closure = GALAlookup(&ga);
	    bf = SAVE_Hp - (FIXED_HS + BF_CLOSURE_SIZE(dummy)) + 1;
	}
	ASSERT(GALAlookup(&rga) == NULL);

	SET_BF_HDR(bf, BF_info, bogosity);
	BF_NODE(bf) = closure;
	BF_GTID(bf) = (W_) rga.loc.gc.gtid;
	BF_SLOT(bf) = (W_) rga.loc.gc.slot;
	BF_WEIGHT(bf) = (W_) rga.weight;
	blockFetch(bf, closure);

#ifdef FETCH_DEBUG
	fprintf(stderr, "Blocking Fetch (%x, %d, %x) on %#lx\n",
	  rga.loc.gc.gtid, rga.loc.gc.slot, rga.weight, closure);
#endif

    } else {			
	/* The target of the FetchMe is some local graph */
	W_ size;
	P_ graph;

	if ((graph = PackNearbyGraph(closure, &size)) == NULL) {
	    ReallyPerformThreadGC(PACK_HEAP_REQUIRED, rtsFalse);
	    SAVE_Hp -= PACK_HEAP_REQUIRED;
	    closure = GALAlookup(&ga);
	    graph = PackNearbyGraph(closure, &size);
	    ASSERT(graph != NULL);
	}
	sendResume(&rga, size, graph);
    }
}

/*
 *processFree unpacks a FREE message and adds the weights to our GAs.
 */
static void
processFree(void)
{
    int nelem;
    static W_ *freeBuffer;
    int i;
    globalAddr ga;

    freeBuffer = gumPackBuffer;
    unpackFree(&nelem, freeBuffer);
#ifdef FREE_DEBUG
    fprintf(stderr, "Rcvd Free (%d GAs)\n", nelem / 2);
#endif
    ga.loc.gc.gtid = mytid;
    for (i = 0; i < nelem;) {
	ga.weight = (unsigned) freeBuffer[i++];
	ga.loc.gc.slot = (int) freeBuffer[i++];
#ifdef FREE_DEBUG
	fprintf(stderr,"Processing free (%x, %d, %x)\n", ga.loc.gc.gtid, 
	  ga.loc.gc.slot, ga.weight);
#endif
	(void) addWeight(&ga);
    }
}

/*
 *processResume unpacks a RESUME message into the graph, filling in
 *the LA -> GA, and GA -> LA tables. Threads blocked on the original
 *FetchMe (now a blocking queue) are awakened, and the blocking queue
 *is converted into an indirection.  Finally it sends an ACK in response
 *which contains any newly allocated GAs.
 */

static void
processResume(GLOBAL_TASK_ID sender)
{
    int nelem;
    W_ nGAs;
    static W_ *packBuffer;
    P_ newGraph;
    P_ old;
    globalAddr lga;
    globalAddr *gagamap;

    packBuffer = gumPackBuffer;
    unpackResume(&lga, &nelem, packBuffer);

#ifdef RESUME_DEBUG
    fprintf(stderr, "Rcvd Resume for (%x, %d, %x)\n",
      lga.loc.gc.gtid, lga.loc.gc.slot, lga.weight);
    PrintPacket(packBuffer);
#endif

    /* 
     * We always unpack the incoming graph, even if we've received the
     * requested node in some other data packet (and already awakened
     * the blocking queue).
     */
    if (SAVE_Hp + packBuffer[0] >= SAVE_HpLim) {
	ReallyPerformThreadGC(packBuffer[0], rtsFalse);
	SAVE_Hp -= packBuffer[0];
    }

    /* Do this *after* GC; we don't want to release the object early! */

    if (lga.weight > 0)
	(void) addWeight(&lga);

    old = GALAlookup(&lga);

    if (RtsFlags.ParFlags.granSimStats) {
	P_ tso = NULL;

	if (INFO_TYPE(INFO_PTR(old)) == INFO_FMBQ_TYPE) {
	    for(tso = (P_) FMBQ_ENTRIES(old); 
              TSO_LINK(tso) != PrelBase_Z91Z93_closure; 
              tso = TSO_LINK(tso))
		;
	}
        /* DumpGranEventAndNode(GR_REPLY, tso, old, taskIDtoPE(sender)); */
	DumpRawGranEvent(CURRENT_PROC,taskIDtoPE(sender),GR_REPLY,
			 tso,old,0);
    }

    newGraph = UnpackGraph(packBuffer, &gagamap, &nGAs);
    ASSERT(newGraph != NULL);

    /* 
     * Sometimes, unpacking will common up the resumee with the
     * incoming graph, but if it hasn't, we'd better do so now.
     */
   
    if (INFO_TYPE(INFO_PTR(old)) == INFO_FMBQ_TYPE)
        CommonUp(old, newGraph);

#ifdef RESUME_DEBUG
    DebugPrintGAGAMap(gagamap, nGAs);
#endif

    sendAck(sender, nGAs, gagamap);
}

/*
 *processSchedule unpacks a SCHEDULE message into the graph, filling
 *in the LA -> GA, and GA -> LA tables. The root of the graph is added to
 *the local spark queue.  Finally it sends an ACK in response
 *which contains any newly allocated GAs.
 */
static void
processSchedule(GLOBAL_TASK_ID sender)
{
    int nelem;
    int space_required;
    rtsBool success;
    static W_ *packBuffer;
    W_ nGAs;
    P_ newGraph;
    globalAddr *gagamap;

    packBuffer = gumPackBuffer;		/* HWL */
    unpackSchedule(&nelem, packBuffer);

#ifdef SCHEDULE_DEBUG
    fprintf(stderr, "Rcvd Schedule\n");
    PrintPacket(packBuffer);
#endif

    /*
     * For now, the graph is a closure to be sparked as an advisory
     * spark, but in future it may be a complete spark with
     * required/advisory status, priority etc.
     */

    space_required = packBuffer[0];
    if (SAVE_Hp + space_required >= SAVE_HpLim) {
	ReallyPerformThreadGC(space_required, rtsFalse);
	SAVE_Hp -= space_required;
    }
    newGraph = UnpackGraph(packBuffer, &gagamap, &nGAs);
    ASSERT(newGraph != NULL);
    success = Spark(newGraph, rtsFalse);
    ASSERT(success);

#ifdef SCHEDULE_DEBUG
    DebugPrintGAGAMap(gagamap, nGAs);
#endif

    if (nGAs > 0)
        sendAck(sender, nGAs, gagamap);

    fishing = rtsFalse;
}

/*
 *processAck unpacks an ACK, and uses the GAGA map to convert RBH's
 *(which represent shared thunks that have been shipped) into fetch-mes
 *to remote GAs.
 */
static void
processAck(void)
{
    int nGAs;
    globalAddr *gaga;

    globalAddr gagamap[MAX_GAS * 2];

    unpackAck(&nGAs, gagamap);

#ifdef ACK_DEBUG
    fprintf(stderr, "Rcvd Ack (%d pairs)\n", nGAs);
    DebugPrintGAGAMap(gagamap, nGAs);
#endif

    /*
     * For each (oldGA, newGA) pair, set the GA of the corresponding
     * thunk to the newGA, convert the thunk to a FetchMe, and return
     * the weight from the oldGA.
     */
    for (gaga = gagamap; gaga < gagamap + nGAs * 2; gaga += 2) {
	P_ old = GALAlookup(gaga);
	P_ new = GALAlookup(gaga + 1);

	if (new == NULL) {
	    /* We don't have this closure, so we make a fetchme for it */
	    globalAddr *ga = setRemoteGA(old, gaga + 1, rtsTrue);

	    convertToFetchMe(old, ga);
	} else {
	    /* 
             * Oops...we've got this one already; update the RBH to
             * point to the object we already know about, whatever it
             * happens to be.
             */
	    CommonUp(old, new);

	    /* 
             * Increase the weight of the object by the amount just
             * received in the second part of the ACK pair.
             */
	    (void) addWeight(gaga + 1);
	}
	(void) addWeight(gaga);
    }
}

#endif

#endif /* PAR -- whole file */

