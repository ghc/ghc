/* 
   Time-stamp: <2009-12-02 12:26:34 simonmar>

   Graph packing and unpacking code for sending it to another processor
   and retrieving the original graph structure from the packet.
   In the old RTS the code was split into Pack.c and Unpack.c (now deceased)
   Used in GUM and GrAnSim.

   The GrAnSim version of the code defines routines for *simulating* the
   packing of closures in the same way it is done in the parallel runtime
   system. Basically GrAnSim only puts the addresses of the closures to be
   transferred into a buffer. This buffer will then be associated with the
   event of transferring the graph. When this event is scheduled, the
   @UnpackGraph@ routine is called and the buffer can be discarded
   afterwards.

   Note that in GranSim we need many buffers, not just one per PE.
*/

//@node Graph packing, , ,
//@section Graph packing

#if defined(PAR) || defined(GRAN)   /* whole file */

//@menu
//* Includes::			
//* Prototypes::		
//* Global variables::		
//* ADT of Closure Queues::	
//* Initialisation for packing::  
//* Packing Functions::		
//* Low level packing routines::  
//* Unpacking routines::	
//* Aux fcts for packing::	
//* Printing Packet Contents::	
//* End of file::		
//@end menu
//*/

//@node Includes, Prototypes, Graph packing, Graph packing
//@subsection Includes

#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "ClosureTypes.h"
#include "Storage.h"
#include "Hash.h"
#include "Parallel.h"
#include "GranSimRts.h"
#include "ParallelRts.h"
# if defined(DEBUG)
# include "sm/Sanity.h"
# include "Printer.h"
# include "ParallelDebug.h"
# endif
#include "FetchMe.h"

/* Which RTS flag should be used to get the size of the pack buffer ? */
# if defined(PAR)
#  define RTS_PACK_BUFFER_SIZE   RtsFlags.ParFlags.packBufferSize
# else   /* GRAN */
#  define RTS_PACK_BUFFER_SIZE   RtsFlags.GranFlags.packBufferSize
# endif

//@node Prototypes, Global variables, Includes, Graph packing
//@subsection Prototypes
/* 
   Code declarations. 
*/

//@node ADT of closure queues, Init for packing, Prototypes, Prototypes
//@subsubsection ADT of closure queues

static inline void    	  InitClosureQueue(void);
static inline rtsBool 	  QueueEmpty(void);
static inline void    	  QueueClosure(StgClosure *closure);
static inline StgClosure *DeQueueClosure(void);

//@node Init for packing, Packing routines, ADT of closure queues, Prototypes
//@subsubsection Init for packing

static void     InitPacking(rtsBool unpack);
# if defined(PAR)
rtsBool         InitPackBuffer(void);
# elif defined(GRAN)
rtsPackBuffer  *InstantiatePackBuffer (void);
static void     reallocPackBuffer (void);
# endif

//@node Packing routines, Low level packing fcts, Init for packing, Prototypes
//@subsubsection Packing routines

static void    PackClosure (StgClosure *closure);

//@node Low level packing fcts, Unpacking routines, Packing routines, Prototypes
//@subsubsection Low level packing fcts

# if defined(GRAN)
static  void    Pack (StgClosure *data);
# else
static  void    Pack (StgWord data);

static void    PackGeneric(StgClosure *closure);
static void    PackArray(StgClosure *closure);
static void    PackPLC (StgPtr addr);
static void    PackOffset (int offset);
static void    PackPAP(StgPAP *pap);
static rtsPackBuffer *PackTSO(StgTSO *tso, nat *packBufferSize);
static rtsPackBuffer *PackStkO(StgPtr stko, nat *packBufferSize);
static void           PackFetchMe(StgClosure *closure);

static void    GlobaliseAndPackGA (StgClosure *closure);
# endif

//@node Unpacking routines, Aux fcts for packing, Low level packing fcts, Prototypes
//@subsubsection Unpacking routines

# if defined(PAR)
void        InitPendingGABuffer(nat size); 
void        CommonUp(StgClosure *src, StgClosure *dst);
static StgClosure *SetGAandCommonUp(globalAddr *gaP, StgClosure *closure, 
				  rtsBool hasGA);
static nat         FillInClosure(StgWord ***bufptrP, StgClosure *graph);
static void        LocateNextParent(StgClosure **parentP,
				    nat *pptrP, nat *pptrsP, nat *sizeP);
StgClosure        *UnpackGraph(rtsPackBuffer *packBuffer,
			       globalAddr **gamap,
			       nat *nGAs);
static  StgClosure *UnpackClosure (StgWord ***bufptrP, StgClosure **graphP, 
				   globalAddr *ga);
static  StgWord   **UnpackGA(StgWord **bufptr, globalAddr *ga);
static  StgClosure *UnpackOffset(globalAddr *ga);
static  StgClosure *UnpackPLC(globalAddr *ga);
static  void        UnpackArray(StgWord ***bufptrP, StgClosure *graph);
static  nat         UnpackPAP(StgWord ***bufptrP, StgClosure *graph);

# elif defined(GRAN)
void        CommonUp(StgClosure *src, StgClosure *dst);
StgClosure *UnpackGraph(rtsPackBuffer* buffer);
#endif

//@node Aux fcts for packing,  , Unpacking routines, Prototypes
//@subsubsection Aux fcts for packing

# if defined(PAR)
static void 	DonePacking(void);
static void 	AmPacking(StgClosure *closure);
static int  	OffsetFor(StgClosure *closure);
static rtsBool  NotYetPacking(int offset);
static inline rtsBool  RoomToPack (nat size, nat ptrs);
static inline rtsBool  isOffset(globalAddr *ga);
static inline rtsBool  isFixed(globalAddr *ga);
static inline rtsBool  isConstr(globalAddr *ga);
static inline rtsBool  isUnglobalised(globalAddr *ga);
# elif defined(GRAN)
static void     DonePacking(void);
static rtsBool  NotYetPacking(StgClosure *closure);
# endif

//@node Global variables, ADT of Closure Queues, Prototypes, Graph packing
//@subsection Global variables
/*
  Static data declarations
*/

static nat     pack_locn,           /* ptr to first free loc in pack buffer */
               clq_size, clq_pos,
               buf_id = 1;          /* identifier for buffer */
static nat     unpacked_size;
static rtsBool roomInBuffer;
#if defined(PAR)
static GlobalTaskId dest_gtid=0;    /* destination for message to send */
#endif

/* 
   The pack buffer
   To be pedantic: in GrAnSim we're packing *addresses* of closures,
   not the closures themselves.
*/
static rtsPackBuffer *globalPackBuffer = NULL,    /* for packing a graph */
                     *globalUnpackBuffer = NULL;  /* for unpacking a graph */


/*
  Bit of a hack for testing if a closure is the root of the graph. This is
  set in @PackNearbyGraph@ and tested in @PackClosure@.  
*/

static nat          packed_thunks = 0;
static StgClosure  *graph_root;

# if defined(PAR)
/*
  The offset hash table is used during packing to record the location in
  the pack buffer of each closure which is packed.
*/
//@cindex offsetTable
static HashTable *offsetTable;

//@cindex PendingGABuffer
static globalAddr *PendingGABuffer, *gaga;

# endif /* PAR */


//@node ADT of Closure Queues, Initialisation for packing, Global variables, Graph packing
//@subsection ADT of Closure Queues

//@menu
//* Closure Queues::		
//* Init routines::		
//* Basic routines::		
//@end menu

//@node Closure Queues, Init routines, ADT of Closure Queues, ADT of Closure Queues
//@subsubsection Closure Queues
/*
  Closure Queues

  These routines manage the closure queue.
*/

static nat clq_pos, clq_size;

static StgClosure **ClosureQueue = NULL;   /* HWL: init in main */

#if defined(DEBUG)
static char graphFingerPrint[MAX_FINGER_PRINT_LEN];
#endif

//@node Init routines, Basic routines, Closure Queues, ADT of Closure Queues
//@subsubsection Init routines

/* @InitClosureQueue@ allocates and initialises the closure queue. */

//@cindex InitClosureQueue
static inline void
InitClosureQueue(void)
{
  clq_pos = clq_size = 0;

  if (ClosureQueue==NULL)
    ClosureQueue = (StgClosure**) stgMallocWords(RTS_PACK_BUFFER_SIZE, 
						 "InitClosureQueue");
}

//@node Basic routines, Types of Global Addresses, Init routines, ADT of Closure Queues
//@subsubsection Basic routines

/*
  QueueEmpty returns rtsTrue if the closure queue is empty; rtsFalse otherwise.
*/

//@cindex QueueEmpty
static inline rtsBool
QueueEmpty(void)
{
  return(clq_pos >= clq_size);
}

/* QueueClosure adds its argument to the closure queue. */

//@cindex QueueClosure
static inline void
QueueClosure(closure)
StgClosure *closure;
{
  if(clq_size < RTS_PACK_BUFFER_SIZE ) {
    IF_PAR_DEBUG(paranoia,
		 belch(">__> <<%d>> Q: %p (%s); %d elems in q",
		       globalPackBuffer->id, closure, info_type(closure), clq_size-clq_pos));
    ClosureQueue[clq_size++] = closure;
  } else { 
    barf("Closure Queue Overflow (EnQueueing %p (%s))", 
	 closure, info_type(closure));
  }
}

/* DeQueueClosure returns the head of the closure queue. */

//@cindex DeQueueClosure
static inline StgClosure * 
DeQueueClosure(void)
{
  if(!QueueEmpty()) {
    IF_PAR_DEBUG(paranoia,
		 belch(">__> <<%d>> DeQ: %p (%s); %d elems in q",
		       globalPackBuffer->id, ClosureQueue[clq_pos], info_type(ClosureQueue[clq_pos]), 
		       clq_size-clq_pos));
    return(ClosureQueue[clq_pos++]);
  } else {
    return((StgClosure*)NULL);
  }
}

/* DeQueueClosure returns the head of the closure queue. */

#if defined(DEBUG)
//@cindex PrintQueueClosure
static void
PrintQueueClosure(void)
{
  nat i;

  fputs("Closure queue:", stderr);
  for (i=clq_pos; i < clq_size; i++)
    fprintf(stderr, "%p (%s), ", 
	    (StgClosure *)ClosureQueue[clq_pos++], 
	    info_type(ClosureQueue[clq_pos++]));
  fputc('\n', stderr);
}
#endif

//@node Types of Global Addresses,  , Basic routines, ADT of Closure Queues
//@subsubsection Types of Global Addresses

/*
  Types of Global Addresses

  These routines determine whether a GA is one of a number of special types
  of GA.
*/

# if defined(PAR)
//@cindex isOffset
static inline rtsBool 
isOffset(globalAddr *ga)
{
    return (ga->weight == 1U && ga->payload.gc.gtid == (GlobalTaskId)0);
}

//@cindex isFixed
static inline rtsBool
isFixed(globalAddr *ga)
{
    return (ga->weight == 0U);
}

//@cindex isConstr
static inline rtsBool
isConstr(globalAddr *ga)
{
    return (ga->weight == 2U);
}

//@cindex isUnglobalised
static inline rtsBool
isUnglobalised(globalAddr *ga)
{
    return (ga->weight == 2U);
}
# endif

//@node Initialisation for packing, Packing Functions, ADT of Closure Queues, Graph packing
//@subsection Initialisation for packing
/*
  Simple Packing Routines

  About packet sizes in GrAnSim: In GrAnSim we use a malloced block of
  gransim_pack_buffer_size words to simulate a packet of pack_buffer_size
  words.  In the simulated PackBuffer we only keep the addresses of the
  closures that would be packed in the parallel system (see Pack). To
  decide if a packet overflow occurs pack_buffer_size must be compared
  versus unpacked_size (see RoomToPack).  Currently, there is no multi
  packet strategy implemented, so in the case of an overflow we just stop
  adding closures to the closure queue.  If an overflow of the simulated
  packet occurs, we just realloc some more space for it and carry on as
  usual.  -- HWL
*/

# if defined(GRAN)
rtsPackBuffer *
InstantiatePackBuffer (void) {
  extern rtsPackBuffer *globalPackBuffer;

  globalPackBuffer = (rtsPackBuffer *) stgMallocWords(sizeofW(rtsPackBuffer), 
			 "InstantiatePackBuffer: failed to alloc packBuffer");
  globalPackBuffer->size = RtsFlags.GranFlags.packBufferSize_internal;
  globalPackBuffer->buffer = (StgWord **) stgMallocWords(RtsFlags.GranFlags.packBufferSize_internal,
				 "InstantiatePackBuffer: failed to alloc GranSim internal packBuffer");
  /* NB: gransim_pack_buffer_size instead of pack_buffer_size -- HWL */
  /* stgMallocWords is now simple allocate in Storage.c */

  return (globalPackBuffer);
}

/* 
   Reallocate the GranSim internal pack buffer to make room for more closure
   pointers. This is independent of the check for packet overflow as in GUM
*/
static void
reallocPackBuffer (void) {

  ASSERT(pack_locn >= (int)globalPackBuffer->size+sizeofW(rtsPackBuffer));

  IF_GRAN_DEBUG(packBuffer,
		belch("** Increasing size of PackBuffer %p to %d words (PE %u @ %d)\n",
		      globalPackBuffer, globalPackBuffer->size+REALLOC_SZ,
		      CurrentProc, CurrentTime[CurrentProc]));
  
  globalPackBuffer = (rtsPackBuffer*)realloc(globalPackBuffer, 
				  sizeof(StgClosure*)*(REALLOC_SZ +
						       (int)globalPackBuffer->size +
						       sizeofW(rtsPackBuffer))) ;
  if (globalPackBuffer==(rtsPackBuffer*)NULL) 
    barf("Failing to realloc %d more words for PackBuffer %p (PE %u @ %d)\n", 
	 REALLOC_SZ, globalPackBuffer, CurrentProc, CurrentTime[CurrentProc]);
  
  globalPackBuffer->size += REALLOC_SZ;

  ASSERT(pack_locn < globalPackBuffer->size+sizeofW(rtsPackBuffer));
}
# endif

# if defined(PAR)
/* @initPacking@ initialises the packing buffer etc. */
//@cindex InitPackBuffer
rtsBool
InitPackBuffer(void)
{
  if (globalPackBuffer==(rtsPackBuffer*)NULL) {
    if ((globalPackBuffer = (rtsPackBuffer *) 
	 stgMallocWords(sizeofW(rtsPackBuffer)+RtsFlags.ParFlags.packBufferSize+DEBUG_HEADROOM,
			"InitPackBuffer")) == NULL)
      return rtsFalse;
  }
  return rtsTrue;
}

# endif 
//@cindex InitPacking
static void
InitPacking(rtsBool unpack)
{
# if defined(GRAN)
  globalPackBuffer = InstantiatePackBuffer();     /* for GrAnSim only -- HWL */
                                       /* NB: free in UnpackGraph */
# elif defined(PAR)
  if (unpack) {
    /* allocate a GA-to-GA map (needed for ACK message) */
    InitPendingGABuffer(RtsFlags.ParFlags.packBufferSize);
  } else {
    /* allocate memory to pack the graph into */
    InitPackBuffer();
  }
# endif
  /* init queue of closures seen during packing */
  InitClosureQueue();

  if (unpack) 
    return;

  globalPackBuffer->id = buf_id++;  /* buffer id are only used for debugging! */
  pack_locn = 0;         /* the index into the actual pack buffer */
  unpacked_size = 0;     /* the size of the whole graph when unpacked */
  roomInBuffer = rtsTrue;
  packed_thunks = 0;   /* total number of thunks packed so far */
# if defined(PAR)
  offsetTable = allocHashTable();
# endif
}

//@node Packing Functions, Low level packing routines, Initialisation for packing, Graph packing
//@subsection Packing Functions

//@menu
//* Packing Sections of Nearby Graph::	
//* Packing Closures::		
//@end menu

//@node Packing Sections of Nearby Graph, Packing Closures, Packing Functions, Packing Functions
//@subsubsection Packing Sections of Nearby Graph
/*
  Packing Sections of Nearby Graph

  @PackNearbyGraph@ packs a closure and associated graph into a static
  buffer (@PackBuffer@).  It returns the address of this buffer and the
  size of the data packed into the buffer (in its second parameter,
  @packBufferSize@).  The associated graph is packed in a depth first
  manner, hence it uses an explicit queue of closures to be packed rather
  than simply using a recursive algorithm.  Once the packet is full,
  closures (other than primitive arrays) are packed as FetchMes, and their
  children are not queued for packing.  */

//@cindex PackNearbyGraph

/* NB: this code is shared between GranSim and GUM;
       tso only used in GranSim */
rtsPackBuffer *
PackNearbyGraph(closure, tso, packBufferSize, dest)
StgClosure* closure;
StgTSO* tso;
nat *packBufferSize;
GlobalTaskId dest;
{
  IF_PAR_DEBUG(resume,
	       graphFingerPrint[0] = '\0');

  ASSERT(RTS_PACK_BUFFER_SIZE > 0);
  ASSERT(_HS==1);  // HWL HACK; compile time constant

#if defined(PAR_TICKY) // HWL HAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACK
  PAR_TICKY_PACK_NEARBY_GRAPH_START();
#endif

  /* ToDo: check that we have enough heap for the packet
     ngoq ngo'
     if (Hp + PACK_HEAP_REQUIRED > HpLim) 
     return NULL;
  */
  InitPacking(rtsFalse);
# if defined(PAR)
  dest_gtid=dest; //-1 to disable
# elif defined(GRAN)
  graph_root = closure;
# endif

  IF_GRAN_DEBUG(pack,
		belch(">>> Packing <<%d>> (buffer @ %p); graph root @ %p [PE %d]\n    demanded by TSO %d (%p) [PE %u]",
		      globalPackBuffer->id, globalPackBuffer, closure, where_is(closure), 
		      tso->id, tso, where_is((StgClosure*)tso)));

  IF_GRAN_DEBUG(pack,
		belch("** PrintGraph of %p is:", closure); 
		PrintGraph(closure,0));

  IF_PAR_DEBUG(resume,
	       GraphFingerPrint(closure, graphFingerPrint);
	       ASSERT(strlen(graphFingerPrint)<=MAX_FINGER_PRINT_LEN);
	       belch(">>> Packing <<%d>> (buffer @ %p); graph root @ %p [%x]\n    demanded by TSO %d (%p); Finger-print is\n    {%s}",
		     globalPackBuffer->id, globalPackBuffer, closure, mytid,
		     tso->id, tso, graphFingerPrint)); 

  IF_PAR_DEBUG(packet,
	       belch("** PrintGraph of %p is:", closure); 
	       belch("** pack_locn=%d", pack_locn);
	       PrintGraph(closure,0));

  QueueClosure(closure);
  do {
    PackClosure(DeQueueClosure());
  } while (!QueueEmpty());
  
# if defined(PAR)

  /* Record how much space the graph needs in packet and in heap */
  globalPackBuffer->tso = tso;       // currently unused, I think (debugging?)
  globalPackBuffer->unpacked_size = unpacked_size;
  globalPackBuffer->size = pack_locn;

  /* Check for buffer overflow (again) */
  ASSERT(pack_locn <= RtsFlags.ParFlags.packBufferSize+DEBUG_HEADROOM);
  IF_DEBUG(sanity,                           // write magic end-of-buffer word
	   globalPackBuffer->buffer[pack_locn] = END_OF_BUFFER_MARKER);
  *packBufferSize = pack_locn;

# else  /* GRAN */

  /* Record how much space is needed to unpack the graph */
  // PackBuffer[PACK_FLAG_LOCN] = (P_) MAGIC_PACK_FLAG;  for testing
  globalPackBuffer->tso = tso;
  globalPackBuffer->unpacked_size = unpacked_size;

  // ASSERT(pack_locn <= PackBuffer[PACK_SIZE_LOCN]+PACK_HDR_SIZE);
  /* ToDo: Print an earlier, more meaningful message */
  if (pack_locn==0)   /* i.e. packet is empty */
    barf("EMPTY PACKET! Can't transfer closure %p at all!!\n",
	 closure);
  globalPackBuffer->size = pack_locn;
  *packBufferSize = pack_locn;

# endif

  DonePacking();                               /* {GrAnSim}vaD 'ut'Ha' */

# if defined(GRAN)
  IF_GRAN_DEBUG(pack ,
		belch("** Finished <<%d>> packing graph %p; closures packed: %d; thunks packed: %d; size of graph: %d",
		      globalPackBuffer->id, closure, globalPackBuffer->size, packed_thunks, globalPackBuffer->unpacked_size));
  if (RtsFlags.GranFlags.GranSimStats.Global) {
    globalGranStats.tot_packets++; 
    globalGranStats.tot_packet_size += pack_locn; 
  }
  
  IF_GRAN_DEBUG(pack, PrintPacket(globalPackBuffer));
# elif defined(PAR)
  IF_PAR_DEBUG(packet,
		belch("** Finished <<%d>> packing graph %p (%s); closures packed: %d; thunks packed: %d; size of graph: %d",
		      globalPackBuffer->id, closure, info_type(closure),
		      globalPackBuffer->size, packed_thunks, 
		      globalPackBuffer->unpacked_size));;

  IF_DEBUG(sanity, // do a sanity check on the packet just constructed 
	   checkPacket(globalPackBuffer));
# endif   /* GRAN */

#if defined(PAR_TICKY) // HWL HAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACK
  PAR_TICKY_PACK_NEARBY_GRAPH_END(globalPackBuffer->size, packed_thunks);
#endif
  
  return (globalPackBuffer);
}

//@cindex PackOneNode

# if defined(GRAN)
/* This version is used when the node is already local */

rtsPackBuffer *
PackOneNode(closure, tso, packBufferSize)
StgClosure* closure;
StgTSO* tso;
nat *packBufferSize;
{
  extern rtsPackBuffer *globalPackBuffer;
  int i, clpack_locn;

  InitPacking(rtsFalse);

  IF_GRAN_DEBUG(pack,
		belch("** PackOneNode: %p (%s)[PE %d] requested by TSO %d (%p) [PE %d]",
		      closure, info_type(closure),
		      where_is(closure), tso->id, tso, where_is((StgClosure *)tso)));

  Pack(closure);

  /* Record how much space is needed to unpack the graph */
  globalPackBuffer->tso = tso;
  globalPackBuffer->unpacked_size = unpacked_size;

  /* Set the size parameter */
  ASSERT(pack_locn <= RTS_PACK_BUFFER_SIZE);
  globalPackBuffer->size =  pack_locn;
  *packBufferSize = pack_locn;

  if (RtsFlags.GranFlags.GranSimStats.Global) {
    globalGranStats.tot_packets++; 
    globalGranStats.tot_packet_size += pack_locn; 
  }
  IF_GRAN_DEBUG(pack,
    PrintPacket(globalPackBuffer));

  return (globalPackBuffer);
}
# endif  /* GRAN */

#if defined(GRAN)

/*
   PackTSO and PackStkO are entry points for two special kinds of closure
   which are used in the parallel RTS.  Compared with other closures they
   are rather awkward to pack because they don't follow the normal closure
   layout (where all pointers occur before all non-pointers).  Luckily,
   they're only needed when migrating threads between processors.  */

//@cindex PackTSO
rtsPackBuffer*
PackTSO(tso, packBufferSize)
StgTSO *tso;
nat *packBufferSize;
{
  extern rtsPackBuffer *globalPackBuffer;
  IF_GRAN_DEBUG(pack,
		belch("** Packing TSO %d (%p)", tso->id, tso));
  *packBufferSize = 0;
  // PackBuffer[0] = PackBuffer[1] = 0; ???
  return(globalPackBuffer);
}

//@cindex PackStkO
static rtsPackBuffer*
PackStkO(stko, packBufferSize)
StgPtr stko;
nat *packBufferSize;
{
  extern rtsPackBuffer *globalPackBuffer;
  IF_GRAN_DEBUG(pack,
		belch("** Packing STKO %p", stko));
  *packBufferSize = 0;
  // PackBuffer[0] = PackBuffer[1] = 0;
  return(globalPackBuffer);
}

static void
PackFetchMe(StgClosure *closure)
{
  barf("{PackFetchMe}Daq Qagh: no FetchMe closures in GRAN!");
}

#elif defined(PAR)

static rtsPackBuffer*
PackTSO(tso, packBufferSize)
StgTSO *tso;
nat *packBufferSize;
{
  barf("{PackTSO}Daq Qagh: trying to pack a TSO %d (%p) of size %d; thread migrations not supported, yet",
       tso->id, tso, packBufferSize);
}

rtsPackBuffer*
PackStkO(stko, packBufferSize)
StgPtr stko;
nat *packBufferSize;
{
  barf("{PackStkO}Daq Qagh: trying to pack a STKO (%p) of size %d; thread migrations not supported, yet",
       stko, packBufferSize);
}

//@cindex PackFetchMe
static void
PackFetchMe(StgClosure *closure)
{
  StgInfoTable *ip;
  nat i;
  int offset;
#if defined(DEBUG)
  nat x = pack_locn;
#endif

#if defined(GRAN)
  barf("{PackFetchMe}Daq Qagh: no FetchMe closures in GRAN!");
#else
  offset = OffsetFor(closure);
  if (!NotYetPacking(offset)) {
    IF_PAR_DEBUG(pack,
		 belch("*>.. Packing FETCH_ME for closure %p (s) as offset to %d",
		       closure, info_type(closure), offset));
    PackOffset(offset);
    // unpacked_size += 0;   // unpacked_size unchanged (closure is shared!!)
    return;
  }

  /* Need a GA even when packing a constructed FETCH_ME (cruel world!) */
  AmPacking(closure);
  /* FMs must be always globalised */
  GlobaliseAndPackGA(closure);

  IF_PAR_DEBUG(pack,
	       belch("*>.. Packing FETCH_ME for closure %p (%s) with GA: ((%x, %d, %x))",
		     closure, info_type(closure), 
		     globalPackBuffer->buffer[pack_locn-2],
		     globalPackBuffer->buffer[pack_locn-1],
		     globalPackBuffer->buffer[pack_locn-3]));

  /* Pack a FetchMe closure instead of closure */
  ip = &stg_FETCH_ME_info;
  /* this assumes that the info ptr is always the first word in a closure*/
  Pack((StgWord)ip);
  for (i = 1; i < _HS; ++i)               // pack rest of fixed header
    Pack((StgWord)*(((StgPtr)closure)+i));
  
  unpacked_size += sizeofW(StgFetchMe);
  /* size of FETCHME in packed is the same as that constant */
  ASSERT(pack_locn-x==PACK_FETCHME_SIZE);
  /* In the pack buffer the pointer to a GA (in the FetchMe closure) 
     is expanded to the full GA; this is a compile-time const */
  //ASSERT(PACK_FETCHME_SIZE == sizeofW(StgFetchMe)-1+PACK_GA_SIZE);  
#endif
}

#endif

#ifdef DIST
static void
PackRemoteRef(StgClosure *closure)
{
  StgInfoTable *ip;
  nat i;
  int offset;

  offset = OffsetFor(closure);
  if (!NotYetPacking(offset)) {
    PackOffset(offset);
    unpacked_size += 2;
    return;
  }

  /* Need a GA even when packing a constructed REMOTE_REF (cruel world!) */
  AmPacking(closure);
  
  /* basically we just Globalise, but for sticky things we can't have multiple GAs,
     so we must prevent the GAs being split.
     
     In returning things to the true sticky owner, this case is already handled, but for
     anything else we just give up at the moment... This needs to be fixed! 
  */
  { globalAddr *ga;
    ga = LAGAlookup(closure); // surely this ga must exist?
    
    // ***************************************************************************
    // ***************************************************************************
    // REMOTE_REF HACK - dual is in SetGAandCommonUp
    // - prevents the weight from ever reaching zero
    if(ga != NULL) 
      ga->weight=0x06660666; //anything apart from 0 really...
    // ***************************************************************************
    // ***************************************************************************
    
    if((ga != NULL)&&(ga->weight / 2 <= 2))
      barf("Cant split the weight any further when packing REMOTE_REF for closure %p (%s) with GA: ((%x, %d, %x))",
		closure, info_type(closure), 
	   	ga->payload.gc.gtid, ga->payload.gc.slot, ga->weight);   			     
  } 
  GlobaliseAndPackGA(closure);
      
  IF_PAR_DEBUG(pack,
	       belch("*>.. Packing REMOTE_REF for closure %p (%s) with GA: ((%x, %d, %x))",
		     closure, info_type(closure), 
		     globalPackBuffer->buffer[pack_locn-2],
		     globalPackBuffer->buffer[pack_locn-1],
		     globalPackBuffer->buffer[pack_locn-3]));

  /* Pack a REMOTE_REF closure instead of closure */
  ip = &stg_REMOTE_REF_info;
  /* this assumes that the info ptr is always the first word in a closure*/
  Pack((StgWord)ip);
  for (i = 1; i < _HS; ++i)               // pack rest of fixed header
    Pack((StgWord)*(((StgPtr)closure)+i));
  
  unpacked_size += PACK_FETCHME_SIZE;
}
#endif /* DIST */

//@node Packing Closures,  , Packing Sections of Nearby Graph, Packing Functions
//@subsubsection Packing Closures
/*
  Packing Closures

  @PackClosure@ is the heart of the normal packing code.  It packs a single
  closure into the pack buffer, skipping over any indirections and
  globalising it as necessary, queues any child pointers for further
  packing, and turns it into a @FetchMe@ or revertible black hole (@RBH@)
  locally if it was a thunk.  Before the actual closure is packed, a
  suitable global address (GA) is inserted in the pack buffer.  There is
  always room to pack a fetch-me to the closure (guaranteed by the
  RoomToPack calculation), and this is packed if there is no room for the
  entire closure.

  Space is allocated for any primitive array children of a closure, and
  hence a primitive array can always be packed along with it's parent
  closure.  */

//@cindex PackClosure

# if defined(PAR)

void
PackClosure(closure)
StgClosure *closure;
{
  StgInfoTable *info;
  nat clpack_locn;

  ASSERT(LOOKS_LIKE_GHC_INFO(get_itbl(closure)));

  closure = UNWIND_IND(closure);
  /* now closure is the thing we want to pack */
  info = get_itbl(closure);

  clpack_locn = OffsetFor(closure);

  /* If the closure has been packed already, just pack an indirection to it
     to guarantee that the graph doesn't become a tree when unpacked */
  if (!NotYetPacking(clpack_locn)) {
    PackOffset(clpack_locn);
    return;
  }

  switch (info->type) {

  case CONSTR_CHARLIKE:
    IF_PAR_DEBUG(pack,
		 belch("*>^^ Packing a charlike closure %d", 
		       ((StgIntCharlikeClosure*)closure)->data));
    
    PackPLC((StgPtr)CHARLIKE_CLOSURE(((StgIntCharlikeClosure*)closure)->data));
    // NB: unpacked_size of a PLC is 0
    return;
      
  case CONSTR_INTLIKE:
    {
      StgInt val = ((StgIntCharlikeClosure*)closure)->data;

      if ((val <= MAX_INTLIKE) && (val >= MIN_INTLIKE)) {
	IF_PAR_DEBUG(pack,
		     belch("*>^^ Packing a small intlike %d as a PLC", 
			   val));
	PackPLC((StgPtr)INTLIKE_CLOSURE(val));
	// NB: unpacked_size of a PLC is 0
	return;
      } else {
	IF_PAR_DEBUG(pack,
		     belch("*>^^ Packing a big intlike %d as a normal closure", 
			   val));
	PackGeneric(closure);
	return;
      }
    }

  case CONSTR:
  case CONSTR_1_0:
  case CONSTR_0_1:
  case CONSTR_2_0:
  case CONSTR_1_1:
  case CONSTR_0_2:
    /* it's a constructor (i.e. plain data) */
    IF_PAR_DEBUG(pack,
		 belch("*>^^ Packing a CONSTR %p (%s) using generic packing", 
		       closure, info_type(closure)));
    PackGeneric(closure);
    return;

  case THUNK_STATIC:       // ToDo: check whether that's ok
  case FUN_STATIC:       // ToDo: check whether that's ok
  case CONSTR_STATIC:
  case CONSTR_NOCAF_STATIC:// For now we ship indirections to CAFs: They are
			   // evaluated on each PE if needed
    IF_PAR_DEBUG(pack,
		 belch("*>~~ Packing a %p (%s) as a PLC", 
		       closure, info_type(closure)));

    PackPLC((StgPtr)closure);
    // NB: unpacked_size of a PLC is 0
    return;

  case THUNK_SELECTOR: 
    {
      StgClosure *selectee = ((StgSelector *)closure)->selectee;

      IF_PAR_DEBUG(pack,
		   belch("*>** Found THUNK_SELECTOR at %p (%s) pointing to %p (%s); using PackGeneric", 
			 closure, info_type(closure), 
			 selectee, info_type(selectee)));
      PackGeneric(closure);
      /* inlined code; probably could use PackGeneric
      Pack((StgWord)(*(StgPtr)closure));  
      Pack((StgWord)(selectee));
      QueueClosure(selectee);
      unpacked_size += 2;
      */
    }
    return;

  case  FUN:
  case	FUN_1_0:
  case	FUN_0_1:
  case	FUN_2_0:
  case	FUN_1_1:
  case	FUN_0_2:
  case  THUNK:
  case	THUNK_1_0:
  case	THUNK_0_1:
  case	THUNK_2_0:
  case	THUNK_1_1:
  case	THUNK_0_2:
    PackGeneric(closure);
    return;

  case AP_UPD:
  case PAP:
    /* 
    barf("*>   Packing of PAP not implemented %p (%s)",
		       closure, info_type(closure));
	 
       Currently we don't pack PAPs; we pack a FETCH_ME to the closure, 
       instead. Note that since PAPs contain a chunk of stack as payload,
       implementing packing of PAPs is a first step towards thread migration.
    IF_PAR_DEBUG(pack,
		 belch("*>.. Packing a PAP closure at %p (%s) as a FETCH_ME", 
		       closure, info_type(closure)));
    PackFetchMe(closure);
    */
    PackPAP((StgPAP *)closure);
    return;

  case CAF_BLACKHOLE:
  case BLACKHOLE:
  case BLACKHOLE_BQ:
  case SE_BLACKHOLE:
  case SE_CAF_BLACKHOLE:
  case RBH:
  case FETCH_ME:
  case FETCH_ME_BQ:

    /* If it's a (revertible) black-hole, pack a FetchMe closure to it */
    //ASSERT(pack_locn > PACK_HDR_SIZE);
    
    IF_PAR_DEBUG(pack,
		 belch("*>.. Packing a BH-like closure at %p (%s) as a FETCH_ME", 
		       closure, info_type(closure)));
    /* NB: in case of a FETCH_ME this might build up a chain of FETCH_MEs;
           phps short-cut the GA here */
    PackFetchMe(closure);
    return;

#ifdef DIST    
  case REMOTE_REF:
    IF_PAR_DEBUG(pack,
		 belch("*>.. Packing %p (%s) as a REMOTE_REF", 
		       closure, info_type(closure)));
    PackRemoteRef(closure);
    /* we hopefully don't end up with a chain of REMOTE_REFs!!!!!!!!!! */

    return;
#endif  
    
  case TSO:
  case MVAR:
#ifdef DIST
          IF_PAR_DEBUG(pack,
		 belch("*>.. Packing %p (%s) as a RemoteRef", 
		       closure, info_type(closure)));
    PackRemoteRef(closure);
#else
    barf("{Pack}Daq Qagh: Only GdH can pack %p (%s)", 
	 closure, info_type(closure));
#endif    
    return;
    
  case ARR_WORDS:
    PackArray(closure);
    return;

  case MUT_ARR_PTRS:
  case MUT_ARR_PTRS_FROZEN:
  case MUT_VAR:
    /* 
       Eventually, this should use the same packing routine as ARR_WRODS

       GlobaliseAndPackGA(closure);
       PackArray(closure);
       return;
    */
    barf("Qagh{Pack}Doq: packing of mutable closures not yet implemented: %p (%s)",
	 closure, info_type(closure));

#  ifdef DEBUG
  case BCO:
    barf("{Pack}Daq Qagh: found BCO closure %p (%s); GUM hates interpreted code", 
	 closure, info_type(closure));
    /* never reached */
    
    // check error cases only in a debugging setup
  case RET_BCO:
  case RET_SMALL:
  case RET_VEC_SMALL:
  case RET_BIG:
  case RET_VEC_BIG:
  case RET_DYN:
    barf("{Pack}Daq Qagh: found return vector %p (%s) when packing (thread migration not implemented)", 
	 closure, info_type(closure));
    /* never reached */
    
  case UPDATE_FRAME:
  case STOP_FRAME:
  case CATCH_FRAME:
  case SEQ_FRAME:
    barf("{Pack}Daq Qagh: found stack frame %p (%s) when packing (thread migration not implemented)", 
	 closure, info_type(closure));
    /* never reached */

  case BLOCKED_FETCH:
  case EVACUATED:
    /* something's very wrong */
    barf("{Pack}Daq Qagh: found %s (%p) when packing", 
	 info_type(closure), closure);
    /* never reached */

  case IND:
  case IND_OLDGEN:
  case IND_PERM:
  case IND_OLDGEN_PERM:
  case IND_STATIC:
    barf("Pack: found IND_... after shorting out indirections %d (%s)", 
	 (nat)(info->type), info_type(closure));

  case WEAK:
  case FOREIGN:
  case STABLE_NAME:
    barf("Pack: found foreign thingy; not yet implemented in %d (%s)", 
	 (nat)(info->type), info_type(closure));
#endif

  default:
    barf("Pack: strange closure %d", (nat)(info->type));
  } /* switch */
}

/*
  Pack a constructor of unknown size.
  Similar to PackGeneric but without creating GAs.
*/
#if 0
//@cindex PackConstr
static void
PackConstr(StgClosure *closure)
{
  StgInfoTable *info;
  nat size, ptrs, nonptrs, vhs, i;
  char str[80];

  ASSERT(LOOKS_LIKE_GHC_INFO(closure->header.info));

  /* get info about basic layout of the closure */
  info = get_closure_info(closure, &size, &ptrs, &nonptrs, &vhs, str);

  ASSERT(info->type == CONSTR ||
         info->type == CONSTR_1_0 ||
         info->type == CONSTR_0_1 ||
         info->type == CONSTR_2_0 ||
         info->type == CONSTR_1_1 ||
         info->type == CONSTR_0_2);

  IF_PAR_DEBUG(pack,
	       fprintf(stderr, "*>^^ packing a constructor at %p (%s) (size=%d, ptrs=%d, nonptrs=%d)\n",
		       closure, info_type(closure), size, ptrs, nonptrs));

  /* Primitive arrays have gone; now we have (MUT_)ARR_WORDS etc */

  if (!RoomToPack(PACK_GA_SIZE + _HS + vhs + nonptrs, ptrs)) {
    IF_PAR_DEBUG(pack,
		 belch("*>&& pack buffer is full; packing FETCH_ME for closure %p (%s)",
		       closure, info_type(closure)));
    PackFetchMe(closure);
    return;
  }

  /* Record the location of the GA */
  AmPacking(closure);

  /* Pack Constructor marker */
  Pack((StgWord)2);

  /* pack fixed and variable header */
  for (i = 0; i < _HS + vhs; ++i)
    Pack((StgWord)*(((StgPtr)closure)+i));
      
  /* register all ptrs for further packing */
  for (i = 0; i < ptrs; ++i)
    QueueClosure(((StgClosure *) *(((StgPtr)closure)+(_HS+vhs)+i)));

  /* pack non-ptrs */
  for (i = 0; i < nonptrs; ++i)
    Pack((StgWord)*(((StgPtr)closure)+(_HS+vhs)+ptrs+i));
}
#endif

/*
  Generic packing code.
  This code is performed for `ordinary' closures such as CONSTR, THUNK etc.
*/
//@cindex PackGeneric
static void
PackGeneric(StgClosure *closure)
{
  StgInfoTable *info;
  StgClosure *rbh;
  nat size, ptrs, nonptrs, vhs, i, m;
  char str[80];

  ASSERT(LOOKS_LIKE_COOL_CLOSURE(closure));

  /* get info about basic layout of the closure */
  info = get_closure_info(closure, &size, &ptrs, &nonptrs, &vhs, str);

  ASSERT(!IS_BLACK_HOLE(closure));

  IF_PAR_DEBUG(pack,
	       fprintf(stderr, "*>== %p (%s): generic packing (size=%d, ptrs=%d, nonptrs=%d)\n",
		       closure, info_type(closure), size, ptrs, nonptrs));

  /* packing strategies: how many thunks to add to a packet; 
     default is infinity i.e. RtsFlags.ParFlags.thunksToPack==0 */
  if (RtsFlags.ParFlags.thunksToPack &&
      packed_thunks >= RtsFlags.ParFlags.thunksToPack &&
      closure_THUNK(closure)) {
    IF_PAR_DEBUG(pack,
		 belch("*>&& refusing to pack more than %d thunks per packet; packing FETCH_ME for closure %p (%s)",
		       packed_thunks, closure, info_type(closure)));
    PackFetchMe(closure);
    return;
  }

  /* Primitive arrays have gone; now we have (MUT_)ARR_WORDS etc */

  if (!RoomToPack(PACK_GA_SIZE + _HS + vhs + nonptrs, ptrs)) {
    IF_PAR_DEBUG(pack,
		 belch("*>&& pack buffer is full; packing FETCH_ME for closure %p (%s)",
		       closure, info_type(closure)));
    PackFetchMe(closure);
    return;
  }

  /* Record the location of the GA */
  AmPacking(closure);
  /* Allocate a GA for this closure and put it into the buffer */
  /* Checks for globalisation scheme; default: globalise everything thunks */
  if ( RtsFlags.ParFlags.globalising == 0 || 
       (closure_THUNK(closure) && !closure_UNPOINTED(closure)) )
    GlobaliseAndPackGA(closure);
  else
    Pack((StgWord)2);  // marker for unglobalised closure


  ASSERT(!(info->type == ARR_WORDS || info->type == MUT_ARR_PTRS ||
	   info->type == MUT_ARR_PTRS_FROZEN || info->type == MUT_VAR));

  /* At last! A closure we can actually pack! */
  if (ip_MUTABLE(info) && ((info->type != FETCH_ME)||(info->type != REMOTE_REF)))
    barf("*>// %p (%s) PackClosure: trying to replicate a Mutable closure!",
	 closure, info_type(closure));
      
  /* 
     Remember, the generic closure layout is as follows:
        +-------------------------------------------------+
	| FIXED HEADER | VARIABLE HEADER | PTRS | NON-PRS |
        +-------------------------------------------------+
  */
  /* pack fixed and variable header */
  for (i = 0; i < _HS + vhs; ++i)
    Pack((StgWord)*(((StgPtr)closure)+i));
      
  /* register all ptrs for further packing */
  for (i = 0; i < ptrs; ++i)
    QueueClosure(((StgClosure *) *(((StgPtr)closure)+(_HS+vhs)+i)));

  /* pack non-ptrs */
  for (i = 0; i < nonptrs; ++i)
    Pack((StgWord)*(((StgPtr)closure)+(_HS+vhs)+ptrs+i));
      
  // ASSERT(_HS+vhs+ptrs+nonptrs==size);
  if ((m=_HS+vhs+ptrs+nonptrs)<size) {
    IF_PAR_DEBUG(pack,
		 belch("*>** WARNING: slop in closure %p (%s); filling %d words; SHOULD NEVER HAPPEN",
		       closure, info_type(closure), size-m));
    for (i=m; i<size; i++) 
      Pack((StgWord)*(((StgPtr)closure)+i));
  }

  unpacked_size += size;
  //unpacked_size += (size < MIN_UPD_SIZE) ? MIN_UPD_SIZE : size;

  /*
   * Record that this is a revertable black hole so that we can fill in
   * its address from the fetch reply.  Problem: unshared thunks may cause
   * space leaks this way, their GAs should be deallocated following an
   * ACK.
   */
      
  if (closure_THUNK(closure) && !closure_UNPOINTED(closure)) { 
    rbh = convertToRBH(closure);
    ASSERT(size>=_HS+MIN_UPD_SIZE); // min size for any updatable closure
    ASSERT(rbh == closure);         // rbh at the same position (minced version)
    packed_thunks++;
  } else if ( closure==graph_root ) {
    packed_thunks++;                // root of graph is counted as a thunk
  }
}
/*
  Pack an array of words.
  ToDo: implement packing of MUT_ARRAYs
*/

//@cindex PackArray
static void
PackArray(StgClosure *closure)
{
  StgInfoTable *info;
  nat size, ptrs, nonptrs, vhs;
  nat i, n;
  char str[80];

  /* get info about basic layout of the closure */
  info = get_closure_info(closure, &size, &ptrs, &nonptrs, &vhs, str);

  ASSERT(info->type == ARR_WORDS || info->type == MUT_ARR_PTRS ||
	 info->type == MUT_ARR_PTRS_FROZEN || info->type == MUT_VAR);

  n = arr_words_words(((StgArrWords *)closure));
  // this includes the header!: arr_words_sizeW(stgCast(StgArrWords*,q)); 

  IF_PAR_DEBUG(pack,
	       belch("*>== %p (%s): packing an array of %d words (size=%d)\n",
		     closure, info_type(closure), n,
		     arr_words_sizeW((StgArrWords *)closure)));

  /* check that we have enough room in the pack buffer */
  if (!RoomToPack(PACK_GA_SIZE + _HS + vhs + nonptrs, ptrs)) {
    IF_PAR_DEBUG(pack,
		 belch("*>&& pack buffer is full; packing FETCH_ME for closure %p (%s)",
		       closure, info_type(closure)));
    PackFetchMe(closure);
    return;
  }

  /* global stats about arrays sent */
  if (RtsFlags.ParFlags.ParStats.Global &&
      RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
    globalParStats.tot_arrs++;
    globalParStats.tot_arr_size += arr_words_words(((StgArrWords *)closure));
  }

  /* record offset of the closure and allocate a GA */
  AmPacking(closure);
  /* Checks for globalisation scheme; default: globalise everything thunks */
  if ( RtsFlags.ParFlags.globalising == 0 || 
       (closure_THUNK(closure) && !closure_UNPOINTED(closure)) )
    GlobaliseAndPackGA(closure);
  else
    Pack((StgWord)2);  // marker for unglobalised closure

  /* Pack the header (2 words: info ptr and the number of words to follow) */
  Pack((StgWord)*(StgPtr)closure);
  Pack(arr_words_words(((StgArrWords *)closure)));

  /* pack the payload of the closure (all non-ptrs) */
  for (i=0; i<n; i++)
    Pack((StgWord)((StgArrWords *)closure)->payload[i]);

  unpacked_size += arr_words_sizeW((StgArrWords *)closure);
}

/*
   Pack a PAP closure.
   Note that the representation of a PAP in the buffer is different from
   its representation in the heap. In particular, pointers to local
   closures are packed directly as FETCHME closures, using
   PACK_FETCHME_SIZE words to represent q 1 word pointer in the orig graph
   structure. To account for the difference in size we store the packed
   size of the closure as part of the PAP's variable header in the buffer.
*/

//@cindex PackPAP
static void
PackPAP(StgPAP *pap) {
  nat n, i, j, pack_start;
  StgPtr p, q;
  const StgInfoTable* info;
  StgWord bitmap;
  /* debugging only */
  StgPtr end;
  nat size, ptrs, nonptrs, vhs;
  char str[80];
  nat unpacked_size_before_PAP, FMs_in_PAP=0; // debugging only

  /* This is actually a setup invariant; checked here 'cause it affects PAPs*/
  //ASSERT(PACK_FETCHME_SIZE == sizeofW(StgFetchMe)-1+PACK_GA_SIZE);
  ASSERT(NotYetPacking(OffsetFor((StgClosure *)pap)));
  IF_DEBUG(sanity,
	   unpacked_size_before_PAP = unpacked_size);

  n = (nat)(pap->n_args);

  /* get info about basic layout of the closure */
  info = get_closure_info((StgClosure *)pap, &size, &ptrs, &nonptrs, &vhs, str);
  ASSERT(ptrs==0 && nonptrs==0 && size==pap_sizeW(pap));

  IF_PAR_DEBUG(pack,
	       belch("*>**  %p (%s): PackPAP: packing PAP with %d words (size=%d; ptrs=%d; nonptrs=%d:", 
		     (StgClosure *)pap, info_type((StgClosure *)pap),
		     n, size, ptrs, nonptrs);
               printClosure((StgClosure *)pap));

  /* check that we have enough room in the pack buffer */
  if (!RoomToPack(PACK_GA_SIZE + _HS + vhs + nonptrs, ptrs)) {
    IF_PAR_DEBUG(pack,
		 belch("*>&& pack buffer is full; packing FETCH_ME for closure %p (%s)",
		       (StgClosure *)pap, info_type((StgClosure *)pap)));
    PackFetchMe((StgClosure *)pap);
    return;
  }

  /* record offset of the closure and allocate a GA */
  AmPacking((StgClosure *)pap);
  /* Checks for globalisation scheme; default: globalise everything thunks */
  if ( RtsFlags.ParFlags.globalising == 0 || 
       (closure_THUNK(pap) && !closure_UNPOINTED(pap)) )
    GlobaliseAndPackGA((StgClosure *)pap);
  else
    Pack((StgWord)2);  // marker for unglobalised closure

  /* Pack the PAP header */
  Pack((StgWord)(pap->header.info));
  Pack((StgWord)(pap->n_args));
  Pack((StgWord)(pap->fun));
  pack_start = pack_locn;   // to compute size of PAP in buffer
  Pack((StgWord)0);    // this will be filled in later (size of PAP in buffer)

  /* Pack the payload of a PAP i.e. a stack chunk */
  /* pointers to start of stack chunk */
  p = (StgPtr)(pap->payload);
  end = (StgPtr)((nat)pap+pap_sizeW(pap)*sizeof(StgWord)); // (StgPtr)((nat)pap+sizeof(StgPAP)+sizeof(StgPtr)*n);
  while (p<end) {
    /* the loop body has been borrowed from scavenge_stack */
    q = (StgPtr)*p;

    /* If we've got a tag, pack all words in that block */
    if (IS_ARG_TAG((W_)q)) {   // q stands for the no. of non-ptrs to follow
      nat m = ARG_TAG((W_)q);      // first word after this block
      IF_PAR_DEBUG(pack,
		   belch("*>**    PackPAP @ %p: packing %d words (tagged), starting @ %p", 
			 p, m, p));
      for (i=0; i<m+1; i++)
	Pack((StgWord)*(p+i));
      p += m+1;                // m words + the tag
      continue;
    }
     
    /* If q is is a pointer to a (heap allocated) closure we pack a FETCH_ME
       ToDo: provide RTS flag to also pack these closures
    */
    if (! LOOKS_LIKE_GHC_INFO(q) ) {
      /* distinguish static closure (PLC) from other closures (FM) */
      switch (get_itbl((StgClosure*)q)->type) {
      case CONSTR_CHARLIKE:
	IF_PAR_DEBUG(pack,
		     belch("*>**    PackPAP: packing a charlike closure %d", 
			   ((StgIntCharlikeClosure*)q)->data));
    
	PackPLC((StgPtr)CHARLIKE_CLOSURE(((StgIntCharlikeClosure*)q)->data));
	p++;
	break;
      
      case CONSTR_INTLIKE:
	{
	  StgInt val = ((StgIntCharlikeClosure*)q)->data;
      
	  if ((val <= MAX_INTLIKE) && (val >= MIN_INTLIKE)) {
	    IF_PAR_DEBUG(pack,
			 belch("*>**    PackPAP: Packing ptr to a small intlike %d as a PLC", val));
	    PackPLC((StgPtr)INTLIKE_CLOSURE(val));
	    p++;
	    break;
	  } else {
	    IF_PAR_DEBUG(pack,
			 belch("*>**    PackPAP: Packing a ptr to a big intlike %d as a FM", 
			       val));
	    Pack((StgWord)(ARGTAG_MAX+1));
	    PackFetchMe((StgClosure *)q);
	    p++;
	    IF_DEBUG(sanity, FMs_in_PAP++);
	    break;
	  }
	}
	case THUNK_STATIC:       // ToDo: check whether that's ok
	case FUN_STATIC:       // ToDo: check whether that's ok
	case CONSTR_STATIC:
	case CONSTR_NOCAF_STATIC:
	  {
	    IF_PAR_DEBUG(pack,
			 belch("*>**    PackPAP: packing a ptr to a %p (%s) as a PLC", 
			       q, info_type((StgClosure *)q)));
	    
	    PackPLC((StgPtr)q);
	    p++;
	    break;
	  }
      default:
	  IF_PAR_DEBUG(pack,
		       belch("*>**    PackPAP @ %p: packing FM to %p (%s)", 
			     p, q, info_type((StgClosure*)q)));
	  Pack((StgWord)(ARGTAG_MAX+1));
	  PackFetchMe((StgClosure *)q);
	  p++;
	  IF_DEBUG(sanity, FMs_in_PAP++);
	  break;
      }
      continue;
    }
	
    /* 
     * Otherwise, q must be the info pointer of an activation
     * record.  All activation records have 'bitmap' style layout
     * info.
     */
    info  = get_itbl((StgClosure *)p);
    switch (info->type) {
	
      /* Dynamic bitmap: the mask is stored on the stack */
    case RET_DYN:
      IF_PAR_DEBUG(pack,
		   belch("*>**    PackPAP @ %p: RET_DYN", 
			 p));

      /* Pack the header as is */
      Pack((StgWord)(((StgRetDyn *)p)->info));
      Pack((StgWord)(((StgRetDyn *)p)->liveness));
      Pack((StgWord)(((StgRetDyn *)p)->ret_addr));

      bitmap = ((StgRetDyn *)p)->liveness;
      p      = (P_)&((StgRetDyn *)p)->payload[0];
      goto small_bitmap;

      /* probably a slow-entry point return address: */
    case FUN:
    case FUN_STATIC:
      {
      IF_PAR_DEBUG(pack,
		   belch("*>**    PackPAP @ %p: FUN or FUN_STATIC", 
			 p));

      Pack((StgWord)(((StgClosure *)p)->header.info));
      p++;

      goto follow_srt; //??
      }

      /* Using generic code here; could inline as in scavenge_stack */
    case UPDATE_FRAME:
      {
	StgUpdateFrame *frame = (StgUpdateFrame *)p;
	nat type = get_itbl(frame->updatee)->type;

	ASSERT(type==BLACKHOLE || type==CAF_BLACKHOLE || type==BLACKHOLE_BQ);

	IF_PAR_DEBUG(pack,
		     belch("*>**    PackPAP @ %p: UPDATE_FRAME (updatee=%p; link=%p)", 
			   p, frame->updatee, frame->link));

	Pack((StgWord)(frame->header.info));
	Pack((StgWord)(frame->link));     // ToDo: fix intra-stack pointer
	Pack((StgWord)(frame->updatee));  // ToDo: follow link 

	p += 3;
      }

      /* small bitmap (< 32 entries, or 64 on a 64-bit machine) */
    case STOP_FRAME:
      {
	IF_PAR_DEBUG(pack,
		     belch("*>**    PackPAP @ %p: STOP_FRAME", 
			   p));
	Pack((StgWord)((StgStopFrame *)p)->header.info);
	p++;
      }

    case CATCH_FRAME:
      {
	IF_PAR_DEBUG(pack,
		     belch("*>**    PackPAP @ %p: CATCH_FRAME (handler=%p)", 
			   p, ((StgCatchFrame *)p)->handler));

	Pack((StgWord)((StgCatchFrame *)p)->header.info);
	Pack((StgWord)((StgCatchFrame *)p)->link); // ToDo: fix intra-stack pointer
	Pack((StgWord)((StgCatchFrame *)p)->exceptions_blocked);
	Pack((StgWord)((StgCatchFrame *)p)->handler);
	p += 4;
      }

    case SEQ_FRAME:
      {
	IF_PAR_DEBUG(pack,
		     belch("*>**    PackPAP @ %p: UPDATE_FRAME (link=%p)", 
			   p, ((StgSeqFrame *)p)->link));

	Pack((StgWord)((StgSeqFrame *)p)->header.info);
	Pack((StgWord)((StgSeqFrame *)p)->link); // ToDo: fix intra-stack pointer

        // ToDo: handle bitmap
        bitmap = info->layout.bitmap;

        p = (StgPtr)&(((StgClosure *)p)->payload);
        goto small_bitmap;
      }
    case RET_BCO:
    case RET_SMALL:
    case RET_VEC_SMALL:
      IF_PAR_DEBUG(pack,
		   belch("*>**    PackPAP @ %p: RET_{BCO,SMALL,VEC_SMALL} (bitmap=%o)", 
			 p, info->layout.bitmap));


      Pack((StgWord)((StgClosure *)p)->header.info);
      p++;
      // ToDo: handle bitmap
      bitmap = info->layout.bitmap;
      /* this assumes that the payload starts immediately after the info-ptr */

    small_bitmap:
      while (bitmap != 0) {
	if ((bitmap & 1) == 0) {
	  Pack((StgWord)(ARGTAG_MAX+1));
	  PackFetchMe((StgClosure *)*p++); // pack a FetchMe to the closure
	  IF_DEBUG(sanity, FMs_in_PAP++);
	} else {
	  Pack((StgWord)*p++);
	}
	bitmap = bitmap >> 1;
      }
      
    follow_srt:
	IF_PAR_DEBUG(pack,
	             belch("*>--    PackPAP: nothing to do for follow_srt"));
      continue;

      /* large bitmap (> 32 entries) */
    case RET_BIG:
    case RET_VEC_BIG:
      {
	StgPtr q;
	StgLargeBitmap *large_bitmap;

	IF_PAR_DEBUG(pack,
		     belch("*>**    PackPAP @ %p: RET_{BIG,VEC_BIG} (large_bitmap=%p)", 
			   p, info->layout.large_bitmap));


	Pack((StgWord)((StgClosure *)p)->header.info);
	p++;

	large_bitmap = info->layout.large_bitmap;

	for (j=0; j<large_bitmap->size; j++) {
	  bitmap = large_bitmap->bitmap[j];
	  q = p + BITS_IN(W_);
	  while (bitmap != 0) {
	    if ((bitmap & 1) == 0) {
	      Pack((StgWord)(ARGTAG_MAX+1));
	      PackFetchMe((StgClosure *)*p++); // ToDo: pack pointer(StgClosure *)*p = evacuate((StgClosure *)*p);
	      IF_DEBUG(sanity, FMs_in_PAP++);
	    } else {
	      Pack((StgWord)*p++);
	    }
	    bitmap = bitmap >> 1;
	  }
	  if (j+1 < large_bitmap->size) {
	    while (p < q) {
	      Pack((StgWord)(ARGTAG_MAX+1));
	      PackFetchMe((StgClosure *)*p++); // ToDo: pack pointer (StgClosure *)*p = evacuate((StgClosure *)*p);
	      IF_DEBUG(sanity, FMs_in_PAP++);
	    }
	  }
	}

	/* and don't forget to follow the SRT */
	goto follow_srt;
      }

    default:
      barf("PackPAP: weird activation record found on stack (@ %p): %d", 
	   p, (int)(info->type));
    }
  }
  // fill in size of the PAP (only the payload!) in buffer
  globalPackBuffer->buffer[pack_start] = (StgWord)(pack_locn - pack_start - 1*sizeofW(StgWord));
  /*
    We can use the generic pap_sizeW macro to compute the size of the
    unpacked PAP because whenever we pack a new FETCHME as part of the
    PAP's payload we also adjust unpacked_size accordingly (smart, aren't we?)

    NB: the current PAP (un-)packing code  relies on the fact that
    the size of the unpacked PAP + size of all unpacked FMs is the same as
    the size of the packed PAP!!
  */
  unpacked_size += pap_sizeW(pap); // sizeofW(pap) + (nat)(globalPackBuffer->buffer[pack_start]);
  IF_DEBUG(sanity,
	   ASSERT(unpacked_size-unpacked_size_before_PAP==pap_sizeW(pap)+FMs_in_PAP*sizeofW(StgFetchMe)));
}
# else  /* GRAN */

/* Fake the packing of a closure */

void
PackClosure(closure)
StgClosure *closure;
{
  StgInfoTable *info, *childInfo;
  nat size, ptrs, nonptrs, vhs;
  char info_hdr_ty[80];
  nat i;
  StgClosure *indirectee, *rbh;
  char str[80];
  rtsBool is_mutable, will_be_rbh, no_more_thunks_please;

  is_mutable = rtsFalse;

  /* In GranSim we don't pack and unpack closures -- we just simulate
     packing by updating the bitmask. So, the graph structure is unchanged
     i.e. we don't short out indirections here. -- HWL */

  /* Nothing to do with packing but good place to (sanity) check closure;
     if the closure is a thunk, it must be unique; otherwise we have copied
     work at some point before that which violates one of our main global
     assertions in GranSim/GUM */
  ASSERT(!closure_THUNK(closure) || is_unique(closure));

  IF_GRAN_DEBUG(pack,
		belch("**  Packing closure %p (%s)",
		      closure, info_type(closure)));

  if (where_is(closure) != where_is(graph_root)) {
    IF_GRAN_DEBUG(pack,
		  belch("**   faking a FETCHME [current PE: %d, closure's PE: %d]",
			where_is(graph_root), where_is(closure)));

    /* GUM would pack a FETCHME here; simulate that by increasing the */
    /* unpacked size accordingly but don't pack anything -- HWL */
    unpacked_size += _HS + 2 ; // sizeofW(StgFetchMe);
    return; 
  }

  /* If the closure's not already being packed */
  if (!NotYetPacking(closure)) 
    /* Don't have to do anything in GrAnSim if closure is already */
    /* packed -- HWL */
    {
      IF_GRAN_DEBUG(pack,
		    belch("**    Closure %p is already packed and omitted now!",
			    closure));
      return;
    }

  switch (get_itbl(closure)->type) {
    /* ToDo: check for sticky bit here? */
    /* BH-like closures which must not be moved to another PE */
    case CAF_BLACKHOLE:       /* # of ptrs, nptrs: 0,2 */
    case SE_BLACKHOLE:        /* # of ptrs, nptrs: 0,2 */
    case SE_CAF_BLACKHOLE:    /* # of ptrs, nptrs: 0,2 */
    case BLACKHOLE:           /* # of ptrs, nptrs: 0,2 */
    case BLACKHOLE_BQ:        /* # of ptrs, nptrs: 1,1 */
    case RBH:                 /* # of ptrs, nptrs: 1,1 */
      /* same for these parallel specific closures */
    case BLOCKED_FETCH:
    case FETCH_ME:
    case FETCH_ME_BQ:
      IF_GRAN_DEBUG(pack,
	belch("**    Avoid packing BH-like closures (%p, %s)!", 
	      closure, info_type(closure)));
      /* Just ignore RBHs i.e. they stay where they are */
      return;

    case THUNK_SELECTOR:
      {
	StgClosure *selectee = ((StgSelector *)closure)->selectee;

	IF_GRAN_DEBUG(pack,
		      belch("**    Avoid packing THUNK_SELECTOR (%p, %s) but queuing %p (%s)!", 
			    closure, info_type(closure), selectee, info_type(selectee)));
	QueueClosure(selectee);
	IF_GRAN_DEBUG(pack,
		      belch("**    [%p (%s) (Queueing closure) ....]",
			    selectee, info_type(selectee)));
      }
      return;

    case CONSTR_STATIC:
    case CONSTR_NOCAF_STATIC:
                                  /* For now we ship indirections to CAFs:
				   * They are evaluated on each PE if needed */
      IF_GRAN_DEBUG(pack,
	belch("**    Nothing to pack for %p (%s)!", 
	      closure, info_type(closure)));
      // Pack(closure); GUM only
      return;

    case CONSTR_CHARLIKE:
    case CONSTR_INTLIKE:
      IF_GRAN_DEBUG(pack,
	belch("**    Nothing to pack for %s (%p)!", 
	      closure, info_type(closure)));
      // PackPLC(((StgIntCharlikeClosure *)closure)->data); GUM only
      return;

    case AP_UPD:   
    case PAP:
      /* partial applications; special treatment necessary? */
      break;

    case MVAR:
      barf("{PackClosure}Daq Qagh: found an MVAR (%p, %s); ToDo: implement proper treatment of MVARs",
	   closure, info_type(closure));

    case ARR_WORDS:
    case MUT_VAR:
    case MUT_ARR_PTRS:
    case MUT_ARR_PTRS_FROZEN:
      /* Mutable objects; require special treatment to ship all data */
      is_mutable = rtsTrue;
      break;	  

    case WEAK:
    case FOREIGN:
    case STABLE_NAME:
	  /* weak pointers and other FFI objects */
      barf("{PackClosure}Daq Qagh: found an FFI object (%p, %s); FFI not yet supported by GranSim, sorry",
	   closure, info_type(closure));

    case TSO:
      /* parallel objects */
      barf("{PackClosure}Daq Qagh: found a TSO when packing (%p, %s); thread migration not yet implemented, sorry",
	   closure, info_type(closure));

    case BCO:
      /* Hugs objects (i.e. closures used by the interpreter) */
      barf("{PackClosure}Daq Qagh: found a Hugs closure when packing (%p, %s); GranSim not yet integrated with Hugs, sorry",
	   closure, info_type(closure));
      
    case IND:              /* # of ptrs, nptrs: 1,0 */
    case IND_STATIC:       /* # of ptrs, nptrs: 1,0 */
    case IND_PERM:         /* # of ptrs, nptrs: 1,1 */
    case IND_OLDGEN:       /* # of ptrs, nptrs: 1,1 */
    case IND_OLDGEN_PERM:  /* # of ptrs, nptrs: 1,1 */
      /* we shouldn't find an indirection here, because we have shorted them
	 out at the beginning of this functions already.
      */
      break;
      /* should be:
      barf("{PackClosure}Daq Qagh: found indirection when packing (%p, %s)",
	   closure, info_type(closure));
      */

    case UPDATE_FRAME:
    case CATCH_FRAME:
    case SEQ_FRAME:
    case STOP_FRAME:
      /* stack frames; should never be found when packing for now;
	 once we support thread migration these have to be covered properly
      */
      barf("{PackClosure}Daq Qagh: found stack frame when packing (%p, %s)",
	   closure, info_type(closure));

    case RET_BCO:
    case RET_SMALL:
    case RET_VEC_SMALL:
    case RET_BIG:
    case RET_VEC_BIG:
    case RET_DYN:
      /* vectored returns; should never be found when packing; */
      barf("{PackClosure}Daq Qagh: found vectored return (%p, %s)",
	   closure, info_type(closure));

    case INVALID_OBJECT:
      barf("{PackClosure}Daq Qagh: found Invalid object (%p, %s)",
	   closure, info_type(closure));

    default:
      /* 
	 Here we know that the closure is a CONSTR, FUN or THUNK (maybe
	 a specialised version with wired in #ptr/#nptr info; currently
	 we treat these specialised versions like the generic version)
      */
    }     /* switch */

    /* Otherwise it's not Fixed */

    info = get_closure_info(closure, &size, &ptrs, &nonptrs, &vhs, str);
    will_be_rbh = closure_THUNK(closure) && !closure_UNPOINTED(closure);

    IF_GRAN_DEBUG(pack,
		belch("**    Info on closure %p (%s): size=%d; ptrs=%d",
		      closure, info_type(closure),
		      size, ptrs, 
		      (will_be_rbh) ? "will become RBH" : "will NOT become RBH"));
    
    // check whether IS_UPDATABLE(closure) == !closure_UNPOINTED(closure) -- HWL
    no_more_thunks_please = 
      (RtsFlags.GranFlags.ThunksToPack>0) && 
      (packed_thunks>=RtsFlags.GranFlags.ThunksToPack);

    /*
      should be covered by get_closure_info
    if (info->type == FETCH_ME || info->type == FETCH_ME_BQ || 
	info->type == BLACKHOLE || info->type == RBH )
      size = ptrs = nonptrs = vhs = 0;
    */
    /* Now peek ahead to see whether the closure has any primitive */
    /* array children */ 
    /* 
       ToDo: fix this code
       for (i = 0; i < ptrs; ++i) {
       P_ childInfo;
       W_ childSize, childPtrs, childNonPtrs, childVhs;
       
       childInfo = get_closure_info(((StgPtrPtr) (closure))[i + _HS + vhs],
       &childSize, &childPtrs, &childNonPtrs,
       &childVhs, junk_str);
       if (IS_BIG_MOTHER(childInfo)) {
       reservedPAsize += PACK_GA_SIZE + _HS + 
       childVhs + childNonPtrs +
       childPtrs * PACK_FETCHME_SIZE;
       PAsize += PACK_GA_SIZE + _HS + childSize;
       PAptrs += childPtrs;
       }
       }
    */
    /* Don't pack anything (GrAnSim) if it's a black hole, or the buffer
     * is full and it isn't a primitive array. N.B. Primitive arrays are
     * always packed (because their parents index into them directly) */

    if (IS_BLACK_HOLE(closure))
	/*
	  ToDo: fix this code
	  || 
	  !(RoomToPack(PACK_GA_SIZE + _HS + vhs + nonptrs, ptrs) 
	  || IS_BIG_MOTHER(info))) 
	  */
      return;

    /* At last! A closure we can actually pack! */

    if (closure_MUTABLE(closure)) // not nec. && (info->type != FETCHME))
      belch("ghuH: Replicated a Mutable closure!");

    if (RtsFlags.GranFlags.GranSimStats.Global &&  
	no_more_thunks_please && will_be_rbh) {
      globalGranStats.tot_cuts++;
      if ( RtsFlags.GranFlags.Debug.pack ) 
	belch("**    PackClosure (w/ ThunksToPack=%d): Cutting tree with root at %#x\n",
		RtsFlags.GranFlags.ThunksToPack, closure);
    } else if (will_be_rbh || (closure==graph_root) ) {
      packed_thunks++;
      globalGranStats.tot_thunks++;
    }

    if (no_more_thunks_please && will_be_rbh) 
      return; /* don't pack anything */

    /* actual PACKING done here --  HWL */
    Pack(closure);         
    for (i = 0; i < ptrs; ++i) {
      /* extract i-th pointer from closure */
      QueueClosure((StgClosure *)(closure->payload[i]));
      IF_GRAN_DEBUG(pack,
		    belch("**    [%p (%s) (Queueing closure) ....]",
			  closure->payload[i], 
	                  info_type(*stgCast(StgPtr*,((closure)->payload+(i))))));
                                  //^^^^^^^^^^^ payloadPtr(closure,i))));
    }

    /* 
       for packing words (GUM only) do something like this:

       for (i = 0; i < ptrs; ++i) {
         Pack(payloadWord(obj,i+j));
       }
    */
    /* Turn thunk into a revertible black hole. */
    if (will_be_rbh) { 
	rbh = convertToRBH(closure);
	ASSERT(rbh != NULL);
    }
}
# endif  /* PAR */

//@node Low level packing routines, Unpacking routines, Packing Functions, Graph packing
//@subsection Low level packing routines

/*
   @Pack@ is the basic packing routine.  It just writes a word of data into
   the pack buffer and increments the pack location.  */

//@cindex Pack

# if defined(PAR)
static  void
Pack(data)
StgWord data;
{
  ASSERT(pack_locn < RtsFlags.ParFlags.packBufferSize);
  globalPackBuffer->buffer[pack_locn++] = data;
}
#endif

#if defined(GRAN)
static  void
Pack(closure)
StgClosure *closure;
{
  StgInfoTable *info;
  nat size, ptrs, nonptrs, vhs;
  char str[80];

  /* This checks the size of the GrAnSim internal pack buffer. The simulated
     pack buffer is checked via RoomToPack (as in GUM) */
  if (pack_locn >= (int)globalPackBuffer->size+sizeofW(rtsPackBuffer)) 
    reallocPackBuffer();

  if (closure==(StgClosure*)NULL) 
    belch("Qagh {Pack}Daq: Trying to pack 0");
  globalPackBuffer->buffer[pack_locn++] = closure;
  /* ASSERT: Data is a closure in GrAnSim here */
  info = get_closure_info(closure, &size, &ptrs, &nonptrs, &vhs, str);
  // ToDo: is check for MIN_UPD_SIZE really needed? */
  unpacked_size += _HS + (size < MIN_UPD_SIZE ? 
				        MIN_UPD_SIZE : 
				        size);
}
# endif  /* GRAN */

/*
   If a closure is local, make it global.  Then, divide its weight for
   export.  The GA is then packed into the pack buffer.  */

# if defined(PAR)
//@cindex GlobaliseAndPackGA
static void
GlobaliseAndPackGA(closure)
StgClosure *closure;
{
  globalAddr *ga;
  globalAddr packGA;

  if ((ga = LAGAlookup(closure)) == NULL) {
    ga = makeGlobal(closure, rtsTrue);

    // Global statistics: increase amount of global data by closure-size
    if (RtsFlags.ParFlags.ParStats.Global &&
	RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
      StgInfoTable *info;
      nat size, ptrs, nonptrs, vhs, i, m; // stats only!!
      char str[80]; // stats only!!

      info = get_closure_info(closure, &size, &ptrs, &nonptrs, &vhs, str);
      globalParStats.tot_global += size;
    }
  }
  ASSERT(ga->weight==MAX_GA_WEIGHT || ga->weight > 2);
  
  if(dest_gtid==ga->payload.gc.gtid)
  {  packGA.payload = ga->payload;
     packGA.weight = 0xFFFFFFFF; // 0,1,2 are used already
  }
  else
  { splitWeight(&packGA, ga);
    ASSERT(packGA.weight > 0);
  }  
 
  IF_PAR_DEBUG(pack,
	       fprintf(stderr, "*>## %p (%s): Globalising (%s) closure with GA ",
		       closure, info_type(closure),
		       ( (ga->payload.gc.gtid==dest_gtid)?"returning":
		           ( (ga->payload.gc.gtid==mytid)?"creating":"sharing" ) ));
	       printGA(&packGA);
	       fputc('\n', stderr));


  Pack((StgWord) packGA.weight);
  Pack((StgWord) packGA.payload.gc.gtid);
  Pack((StgWord) packGA.payload.gc.slot);
}

/*
   @PackPLC@ makes up a bogus GA for a PLC. Weight 0 implies that a PLC
   address follows instead of PE, slot.  */

//@cindex PackPLC

static void
PackPLC(addr)
StgPtr addr;
{
  Pack(0L);			/* weight */
  Pack((StgWord) addr);		/* address */
}

/*
   @PackOffset@ packs a special GA value that will be interpreted as an
   offset to a closure in the pack buffer.  This is used to avoid unfolding
   the graph structure into a tree.  */

static void
PackOffset(offset)
int offset;
{
  /*
  IF_PAR_DEBUG(pack,
	       belch("** Packing Offset %d at pack location %u",
		     offset, pack_locn));
  */
  Pack(1L);			/* weight */
  Pack(0L);			/* pe */
  Pack(offset);		        /* slot/offset */
}
# endif  /* PAR */

//@node Unpacking routines, Aux fcts for packing, Low level packing routines, Graph packing
//@subsection Unpacking routines

/*
  This was formerly in the (now deceased) module Unpack.c

  Unpacking closures which have been exported to remote processors

  This module defines routines for unpacking closures in the parallel
  runtime system (GUM).

  In the case of GrAnSim, this module defines routines for *simulating* the
  unpacking of closures as it is done in the parallel runtime system.
*/

//@node GUM code, GranSim Code, Unpacking routines, Unpacking routines
//@subsubsection GUM code

#if defined(PAR) 

//@cindex InitPendingGABuffer
void
InitPendingGABuffer(size)
nat size; 
{
  if (PendingGABuffer==(globalAddr *)NULL)
    PendingGABuffer = (globalAddr *) 
      stgMallocBytes(size*2*sizeof(globalAddr),
		     "InitPendingGABuffer");

  /* current location in the buffer */
  gaga = PendingGABuffer; 
}

/*
  @CommonUp@ commons up two closures which we have discovered to be
  variants of the same object.  One is made an indirection to the other.  */

//@cindex CommonUp
void
CommonUp(StgClosure *src, StgClosure *dst)
{
  StgBlockingQueueElement *bqe;
#if defined(DEBUG)
  StgInfoTable *info;
  nat size, ptrs, nonptrs, vhs, i;
  char str[80];

  /* get info about basic layout of the closure */
  info = get_closure_info(src, &size, &ptrs, &nonptrs, &vhs, str);
#endif

  ASSERT(src != (StgClosure *)NULL && dst != (StgClosure *)NULL);
  ASSERT(src != dst);

  IF_PAR_DEBUG(pack,
	       belch("*___  CommonUp %p (%s) --> %p (%s)",
		     src, info_type(src), dst, info_type(dst)));
  
  switch (get_itbl(src)->type) {
  case BLACKHOLE_BQ:
    bqe = ((StgBlockingQueue *)src)->blocking_queue;
    break;

  case FETCH_ME_BQ:
    bqe = ((StgFetchMeBlockingQueue *)src)->blocking_queue;
    break;
    
  case RBH:
    bqe = ((StgRBH *)src)->blocking_queue;
    break;
    
  case BLACKHOLE:
  case FETCH_ME:
    bqe = END_BQ_QUEUE;
    break;

    /* These closures are too small to be updated with an indirection!!! */
  case CONSTR_1_0:
  case CONSTR_0_1:
    ASSERT(size<_HS+MIN_UPD_SIZE); // that's why we have to avoid UPD_IND
    return;

    /* currently we also common up 2 CONSTRs; this should reduce heap 
     * consumption but also does more work; not sure whether it's worth doing 
     */ 
  case CONSTR:
  case CONSTR_2_0:
  case CONSTR_1_1:
  case CONSTR_0_2:
  case ARR_WORDS:
  case MUT_ARR_PTRS:
  case MUT_ARR_PTRS_FROZEN:
  case MUT_VAR:
    break;

  default:
    /* Don't common up anything else */
    return;
  }

  /* closure must be big enough to permit update with ind */
  ASSERT(size>=_HS+MIN_UPD_SIZE);
  /* NB: this also awakens the blocking queue for src */
  UPD_IND(src, dst);
}

/*
 * Common up the new closure with any existing closure having the same
 * GA
 */
//@cindex SetGAandCommonUp
static StgClosure *
SetGAandCommonUp(globalAddr *ga, StgClosure *closure, rtsBool hasGA)
{
  StgClosure *existing;
  StgInfoTable *ip, *oldip;
  globalAddr *newGA;

  if (!hasGA)
    return closure;
  
  /* should we already have a local copy? */
  if (ga->weight==0xFFFFFFFF) { 
    ASSERT(ga->payload.gc.gtid==mytid); //sanity
    ga->weight=0;
    /* probably should also ASSERT that a commonUp takes place...*/
  }
  
  ip = get_itbl(closure);
  if ((existing = GALAlookup(ga)) == NULL) {
    /* Just keep the new object */
    IF_PAR_DEBUG(pack,
		 belch("*<##  New local object for GA ((%x, %d, %x)) is %p (%s)", 
		       ga->payload.gc.gtid, ga->payload.gc.slot, ga->weight,
		       closure, info_type(closure)));

    // make an entry binding closure to ga in the RemoteGA table
    newGA = setRemoteGA(closure, ga, rtsTrue);
    // if local closure is a FETCH_ME etc fill in the global indirection
    if (ip->type == FETCH_ME || ip->type == REMOTE_REF)
      ((StgFetchMe *)closure)->ga = newGA;
  } else {
    

#ifdef DIST 
// ***************************************************************************
// ***************************************************************************
// REMOTE_REF HACK - dual is in PackRemoteRef  
// - prevents the weight ever being updated
  if (ip->type == REMOTE_REF)
    ga->weight=0;
// ***************************************************************************
// ***************************************************************************
#endif /* DIST */
    
    /* Two closures, one global name.  Someone loses */
    oldip = get_itbl(existing);
    if ((oldip->type == FETCH_ME || 
	 IS_BLACK_HOLE(existing) ||
	 /* try to share evaluated closures */
         oldip->type == CONSTR ||
	 oldip->type == CONSTR_1_0 ||
	 oldip->type == CONSTR_0_1 ||
	 oldip->type == CONSTR_2_0 ||
	 oldip->type == CONSTR_1_1 ||
	 oldip->type == CONSTR_0_2 
	) &&
	ip->type != FETCH_ME) 
    {
      IF_PAR_DEBUG(pack,
		   belch("*<#-  Duplicate local object for GA ((%x, %d, %x)); redirecting %p (%s) -> %p (%s)",
			 ga->payload.gc.gtid, ga->payload.gc.slot, ga->weight,
			 existing, info_type(existing), closure, info_type(closure)));

      /* 
       * What we had wasn't worth keeping, so make the old closure an
       * indirection to the new closure (copying BQs if necessary) and
       * make sure that the old entry is not the preferred one for this
       * closure.
       */
      CommonUp(existing, closure);
      //GALAdeprecate(ga);
#if defined(DEBUG)
      { 
      	 StgInfoTable *info;
      	 nat size, ptrs, nonptrs, vhs, i;
      	 char str[80];
      
      	 /* get info about basic layout of the closure */
      	 info = get_closure_info(GALAlookup(ga), &size, &ptrs, &nonptrs, &vhs, str);
      
      	 /* now ga indirectly refers to the new closure */
      	 ASSERT(size<_HS+MIN_UPD_SIZE || 
      		UNWIND_IND(GALAlookup(ga))==closure);
      }
#endif
    } else {
      /*
       * Either we already had something worthwhile by this name or
       * the new thing is just another FetchMe.  However, the thing we
       * just unpacked has to be left as-is, or the child unpacking
       * code will fail.  Remember that the way pointer words are
       * filled in depends on the info pointers of the parents being
       * the same as when they were packed.
       */
      IF_PAR_DEBUG(pack,
		   belch("*<#@  Duplicate local object for GA ((%x, %d, %x)); keeping %p (%s) nuking unpacked %p (%s)", 
			 ga->payload.gc.gtid, ga->payload.gc.slot, ga->weight,
			 existing, info_type(existing), closure, info_type(closure)));

      /* overwrite 2nd word; indicates that the closure is garbage */
      IF_DEBUG(sanity,
	       ((StgFetchMe*)closure)->ga = (globalAddr*)GARBAGE_MARKER;
	       IF_PAR_DEBUG(pack,
			    belch("++++  unpacked closure %p (%s) is garbage: %p",
				  closure, info_type(closure), *(closure+1))));

      closure = existing;
#if 0
      // HACK
      ty = get_itbl(closure)->type;
      if (ty == CONSTR ||
	  ty == CONSTR_1_0 ||
	  ty == CONSTR_0_1 ||
	  ty == CONSTR_2_0 ||
	  ty == CONSTR_1_1 ||
	  ty == CONSTR_0_2)
	CommonUp(closure, graph);
#endif
    }
    /* We don't use this GA after all, so give back the weight */
    (void) addWeight(ga);
  }

  /* if we have unpacked a FETCH_ME, we have a GA, too */
  ASSERT(get_itbl(closure)->type!=FETCH_ME || 
	 looks_like_ga(((StgFetchMe*)closure)->ga));

  /* Sort out the global address mapping */
  if (ip_THUNK(ip)){ 
    // || // (ip_THUNK(ip) && !ip_UNPOINTED(ip)) || 
    //(ip_MUTABLE(ip) && ip->type != FETCH_ME)) {
    /* Make up new GAs for single-copy closures */
    globalAddr *newGA = makeGlobal(closure, rtsTrue);
    
    // It's a new GA and therefore has the full weight
    ASSERT(newGA->weight==0);

    /* Create an old GA to new GA mapping */
    *gaga++ = *ga;
    splitWeight(gaga, newGA);
    /* inlined splitWeight; we know that newGALA has full weight 
    newGA->weight = gaga->weight = 1L << (BITS_IN(unsigned) - 1);    
    gaga->payload = newGA->payload;
    */
    ASSERT(gaga->weight == 1U << (BITS_IN(unsigned) - 1));
    gaga++;
  }
  return closure;
}

/*
  Copies a segment of the buffer, starting at @bufptr@, representing a closure
  into the heap at @graph@.
 */
//@cindex FillInClosure
static nat
FillInClosure(StgWord ***bufptrP, StgClosure *graph)
{
  StgInfoTable *ip;
  StgWord **bufptr = *bufptrP;
  nat ptrs, nonptrs, vhs, i, size;
  char str[80];

  ASSERT(LOOKS_LIKE_GHC_INFO(((StgClosure*)bufptr)->header.info));

  /*
   * Close your eyes.  You don't want to see where we're looking. You
   * can't get closure info until you've unpacked the variable header,
   * but you don't know how big it is until you've got closure info.
   * So...we trust that the closure in the buffer is organized the
   * same way as they will be in the heap...at least up through the
   * end of the variable header.
   */
  ip = get_closure_info((StgClosure *)bufptr, &size, &ptrs, &nonptrs, &vhs, str);
	  
  /* Make sure that nothing sans the fixed header is filled in
     The ga field of the FETCH_ME is filled in in SetGAandCommonUp */
  if (ip->type == FETCH_ME || ip->type == REMOTE_REF) {
    ASSERT(size>=_HS+MIN_UPD_SIZE);    // size of the FM in the heap
    ptrs = nonptrs = vhs = 0;      // i.e. only unpack FH from buffer
  }
  /* ToDo: check whether this is really needed */
  if (ip->type == ARR_WORDS) {
    UnpackArray(bufptrP, graph);
    return arr_words_sizeW((StgArrWords *)bufptr);
  }

  if (ip->type == PAP || ip->type == AP_UPD) {
    return UnpackPAP(bufptrP, graph); // includes size of unpackes FMs
  }

  /* 
     Remember, the generic closure layout is as follows:
     +-------------------------------------------------+
     | FIXED HEADER | VARIABLE HEADER | PTRS | NON-PRS |
     +-------------------------------------------------+
  */
  /* Fill in the fixed header */
  for (i = 0; i < _HS; i++)
    ((StgPtr)graph)[i] = (StgWord)*bufptr++;

  /* Fill in the packed variable header */
  for (i = 0; i < vhs; i++)
    ((StgPtr)graph)[_HS + i] = (StgWord)*bufptr++;
  
  /* Pointers will be filled in later */
  
  /* Fill in the packed non-pointers */
  for (i = 0; i < nonptrs; i++)
    ((StgPtr)graph)[_HS + i + vhs + ptrs] = (StgWord)*bufptr++;

  /* Indirections are never packed */
  // ASSERT(INFO_PTR(graph) != (W_) Ind_info_TO_USE);
  // return bufptr;
   *bufptrP = bufptr;
   ASSERT(((ip->type==FETCH_ME || ip->type==REMOTE_REF)&& sizeofW(StgFetchMe)==size) ||
	  _HS+vhs+ptrs+nonptrs == size);
   return size; 
}

/*
  Find the next pointer field in the parent closure.
  If the current parent has been completely unpacked already, get the
  next closure from the global closure queue.
*/
//@cindex LocateNextParent
static void
LocateNextParent(parentP, pptrP, pptrsP, sizeP)
StgClosure **parentP;
nat *pptrP, *pptrsP, *sizeP;
{
  StgInfoTable *ip; // debugging
  nat nonptrs, pvhs;
  char str[80];

  /* pptr as an index into the current parent; find the next pointer field
     in the parent by increasing pptr; if that takes us off the closure
     (i.e. *pptr + 1 > *pptrs) grab a new parent from the closure queue
  */
  (*pptrP)++;
  while (*pptrP + 1 > *pptrsP) {
    /* *parentP has been constructed (all pointer set); so check it now */
    IF_DEBUG(sanity,
	     if ((*parentP!=(StgClosure*)NULL) &&         // not root
		 (*((StgPtr)(*parentP)+1)!=GARBAGE_MARKER) && // not commoned up
		 (get_itbl(*parentP)->type != FETCH_ME))
	       checkClosure(*parentP));

    *parentP = DeQueueClosure();
    
    if (*parentP == NULL)
      break;
    else {
      ip = get_closure_info(*parentP, sizeP, pptrsP, &nonptrs,
			    &pvhs, str);
      *pptrP = 0;
    }
  }
  /* *parentP points to the new (or old) parent; */
  /* *pptr, *pptrs and *size have been updated referring to the new parent */
}

/* 
   UnpackClosure is the heart of the unpacking routine. It is called for 
   every closure found in the packBuffer. Any prefix such as GA, PLC marker
   etc has been unpacked into the *ga structure. 
   UnpackClosure does the following:
     - check for the kind of the closure (PLC, Offset, std closure)
     - copy the contents of the closure from the buffer into the heap
     - update LAGA tables (in particular if we end up with 2 closures 
       having the same GA, we make one an indirection to the other)
     - set the GAGA map in order to send back an ACK message

   At the end of this function *graphP has been updated to point to the
   next free word in the heap for unpacking the rest of the graph and
   *bufptrP points to the next word in the pack buffer to be unpacked.
*/

static  StgClosure*
UnpackClosure (StgWord ***bufptrP, StgClosure **graphP, globalAddr *ga) {
  StgClosure *closure;
  nat size;
  rtsBool hasGA = rtsFalse, unglobalised = rtsFalse;

  /* Now unpack the closure body, if there is one; three cases:
     - PLC: closure is just a pointer to a static closure
     - Offset: closure has been unpacked already
     - else: copy data from packet into closure
  */
  if (isFixed(ga)) {
    closure = UnpackPLC(ga);
  } else if (isOffset(ga)) {
    closure = UnpackOffset(ga);
  } else {
    /* if not PLC or Offset it must be a GA and then the closure */
    ASSERT(RtsFlags.ParFlags.globalising!=0 || LOOKS_LIKE_GA(ga));
    /* check whether this is an unglobalised closure */
    unglobalised = isUnglobalised(ga);
    /* Now we have to build something. */
    hasGA = !isConstr(ga);
    /* the new closure will be built here */
    closure = *graphP;

    /* fill in the closure from the buffer */
    size = FillInClosure(/*in/out*/bufptrP, /*in*/closure);
    /* if it is unglobalised, it may not be a thunk!! */
    ASSERT(!unglobalised || !closure_THUNK(closure));
    
   /* Add to queue for processing */
    QueueClosure(closure);

    /* common up with other graph if necessary */
    if (!unglobalised)
      closure = SetGAandCommonUp(ga, closure, hasGA);

    /* if we unpacked a THUNK, check that it is large enough to update */
    ASSERT(!closure_THUNK(closure) || size>=_HS+MIN_UPD_SIZE);
    /* graph shall point to next free word in the heap */
    *graphP += size;
    //*graphP += (size < _HS+MIN_UPD_SIZE) ? _HS+MIN_UPD_SIZE : size; // see ASSERT
  }
  return closure;
}

/*
  @UnpackGraph@ unpacks the graph contained in a message buffer.  It
  returns a pointer to the new graph.  The @gamap@ parameter is set to
  point to an array of (oldGA,newGA) pairs which were created as a result
  of unpacking the buffer; @nGAs@ is set to the number of GA pairs which
  were created.

  The format of graph in the pack buffer is as defined in @Pack.lc@.  */

//@cindex UnpackGraph
StgClosure *
UnpackGraph(packBuffer, gamap, nGAs)
rtsPackBuffer *packBuffer;
globalAddr **gamap;
nat *nGAs;
{
  StgWord **bufptr, **slotptr;
  globalAddr gaS;
  StgClosure *closure, *graphroot, *graph, *parent;
  nat size, heapsize, bufsize, 
      pptr = 0, pptrs = 0, pvhs = 0;
  nat unpacked_closures = 0, unpacked_thunks = 0; // stats only

  IF_PAR_DEBUG(resume,
	       graphFingerPrint[0] = '\0');

  ASSERT(_HS==1);  // HWL HACK; compile time constant

#if defined(PAR_TICKY) // HWL HAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACK
  PAR_TICKY_UNPACK_GRAPH_START();
#endif
  
  /* Initialisation */
  InitPacking(rtsTrue);      // same as in PackNearbyGraph
  globalUnpackBuffer = packBuffer;

  IF_DEBUG(sanity, // do a sanity check on the incoming packet
	   checkPacket(packBuffer));

  ASSERT(gaga==PendingGABuffer); 
  graphroot = (StgClosure *)NULL;

  /* Unpack the header */
  bufsize = packBuffer->size;
  heapsize = packBuffer->unpacked_size;
  bufptr = packBuffer->buffer;

  /* allocate heap */
  if (heapsize > 0) {
    graph = (StgClosure *)allocate(heapsize);
    ASSERT(graph != NULL);
    // parallel global statistics: increase amount of global data
    if (RtsFlags.ParFlags.ParStats.Global &&
	RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
      globalParStats.tot_global += heapsize;
    }
  }

  /* iterate over the buffer contents and unpack all closures */
  parent = (StgClosure *)NULL;
  do {
    /* check that we aren't at the end of the buffer, yet */
    IF_DEBUG(sanity, ASSERT(*bufptr != END_OF_BUFFER_MARKER));

    /* This is where we will ultimately save the closure's address */
    slotptr = bufptr;

    /* fill in gaS from buffer; gaS may receive GA, PLC- or offset-marker */
    bufptr = UnpackGA(/*in*/bufptr, /*out*/&gaS);

    /* this allocates heap space, updates LAGA tables etc */
    closure = UnpackClosure (/*in/out*/&bufptr, /*in/out*/&graph, /*in*/&gaS);
    unpacked_closures++; // stats only; doesn't count FMs in PAP!!!
    unpacked_thunks += (closure_THUNK(closure)) ? 1 : 0; // stats only

    /*
     * Set parent pointer to point to chosen closure.  If we're at the top of
     * the graph (our parent is NULL), then we want to arrange to return the
     * chosen closure to our caller (possibly in place of the allocated graph
     * root.)
     */
    if (parent == NULL)
      graphroot = closure;
    else
      ((StgPtr)parent)[_HS + pvhs + pptr] = (StgWord) closure;

    /* Save closure pointer for resolving offsets */
    *slotptr = (StgWord*) closure;

    /* Locate next parent pointer */
    LocateNextParent(&parent, &pptr, &pptrs, &size);

    IF_DEBUG(sanity,
	     gaS.weight          = 0xdeadffff;
	     gaS.payload.gc.gtid = 0xdead;
	     gaS.payload.gc.slot = 0xdeadbeef;);
  } while (parent != NULL);

  IF_PAR_DEBUG(resume,
	       GraphFingerPrint(graphroot, graphFingerPrint);
	       ASSERT(strlen(graphFingerPrint)<=MAX_FINGER_PRINT_LEN);
	       belch(">>> Fingerprint of graph rooted at %p (after unpacking <<%d>>:\n    {%s}",
		     graphroot, packBuffer->id, graphFingerPrint));

  /* we unpacked exactly as many words as there are in the buffer */
  ASSERT(bufsize == (nat) (bufptr-(packBuffer->buffer)));
  /* we filled no more heap closure than we allocated at the beginning; 
     ideally this should be a ==; 
     NB: test is only valid if we unpacked anything at all (graphroot might
         end up to be a PLC!), therfore the strange test for HEAP_ALLOCED 
  */

  /*
  {
   StgInfoTable *info = get_itbl(graphroot);
   ASSERT(!HEAP_ALLOCED(graphroot) || heapsize >= (nat) (graph-graphroot) ||
          // ToDo: check whether CAFs are really a special case here!!
          info->type==CAF_BLACKHOLE || info->type==FETCH_ME || info->type==FETCH_ME_BQ); 
  }
  */

  /* check for magic end-of-buffer word */
  IF_DEBUG(sanity, ASSERT(*bufptr == END_OF_BUFFER_MARKER));

  *gamap = PendingGABuffer;
  *nGAs = (gaga - PendingGABuffer) / 2;

  IF_PAR_DEBUG(tables,
	       belch("**   LAGA table after unpacking closure %p:",
		     graphroot);
	       printLAGAtable());

  /* ToDo: are we *certain* graphroot has been set??? WDP 95/07 */
  ASSERT(graphroot!=NULL);

  IF_DEBUG(sanity,
           {
	     StgPtr p;

	     /* check the unpacked graph */
	     //checkHeapChunk(graphroot,graph-sizeof(StgWord));

	     // if we do sanity checks, then wipe the pack buffer after unpacking
	     for (p=(StgPtr)packBuffer->buffer; p<(StgPtr)(packBuffer->buffer)+(packBuffer->size); )
	       *p++ = 0xdeadbeef;
            });

  /* reset the global variable */
  globalUnpackBuffer = (rtsPackBuffer*)NULL;

#if defined(PAR_TICKY) // HWL HAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACK
  PAR_TICKY_UNPACK_GRAPH_END(unpacked_closures, unpacked_thunks);
#endif

  return (graphroot);
}

//@cindex UnpackGA
static  StgWord **
UnpackGA(StgWord **bufptr, globalAddr *ga)
{
  /* First, unpack the next GA or PLC */
  ga->weight = (rtsWeight) *bufptr++;

  if (ga->weight == 2) {  // unglobalised closure to follow
    // nothing to do; closure starts at *bufptr
  } else if (ga->weight > 0) { // fill in GA
    ga->payload.gc.gtid = (GlobalTaskId) *bufptr++;
    ga->payload.gc.slot = (int) *bufptr++;
  } else {
    ga->payload.plc = (StgPtr) *bufptr++;
  }
  return bufptr;
}

//@cindex UnpackPLC
static  StgClosure *
UnpackPLC(globalAddr *ga)
{
  /* No more to unpack; just set closure to local address */
  IF_PAR_DEBUG(pack,
	       belch("*<^^ Unpacked PLC at %x", ga->payload.plc)); 
  return (StgClosure*)ga->payload.plc;
}

//@cindex UnpackOffset
static  StgClosure *
UnpackOffset(globalAddr *ga)
{
  /* globalUnpackBuffer is a global var init in UnpackGraph */
  ASSERT(globalUnpackBuffer!=(rtsPackBuffer*)NULL);
  /* No more to unpack; just set closure to cached address */
  IF_PAR_DEBUG(pack,
	       belch("*<__ Unpacked indirection to %p (was OFFSET %d)", 
		     (StgClosure *)((globalUnpackBuffer->buffer)[ga->payload.gc.slot]),
		     ga->payload.gc.slot)); 
  return (StgClosure *)(globalUnpackBuffer->buffer)[ga->payload.gc.slot];
}

/*
  Input: *bufptrP, *graphP  ... ptrs to the pack buffer and into the heap.

  *bufptrP points to something that should be unpacked as a FETCH_ME:
    |
    v
    +-------------------------------
    |    GA    | FH of FM
    +-------------------------------

  The first 3 words starting at *bufptrP are the GA address; the next
  word is the generic FM info ptr followed by the remaining FH (if any)
  The result after unpacking will be a FETCH_ME closure, pointed to by
  *graphP at the start of the fct;
    |
    v
    +------------------------+
    | FH of FM | ptr to a GA |
    +------------------------+

   The ptr field points into the RemoteGA table, which holds the actual GA.
   *bufptrP has been updated to point to the next word in the buffer.
   *graphP has been updated to point to the first free word at the end.
*/

static StgClosure*
UnpackFetchMe (StgWord ***bufptrP, StgClosure **graphP) {
  StgClosure *closure, *foo;
  globalAddr gaS;

  /* This fct relies on size of FM < size of FM in pack buffer */
  ASSERT(sizeofW(StgFetchMe)<=PACK_FETCHME_SIZE);

  /* fill in gaS from buffer */
  *bufptrP = UnpackGA(*bufptrP, &gaS);
  /* might be an offset to a closure in the pack buffer */
  if (isOffset(&gaS)) {
    belch("*<   UnpackFetchMe: found OFFSET to %d when unpacking FM at buffer loc %p",
		  gaS.payload.gc.slot, *bufptrP);

    closure = UnpackOffset(&gaS);
    /* return address of previously unpacked closure; leaves *graphP unchanged */
    return closure;
  }

  /* we have a proper GA at hand */
  ASSERT(LOOKS_LIKE_GA(&gaS));

  IF_DEBUG(sanity,
	   if (isFixed(&gaS)) 
	   barf("*<   UnpackFetchMe: found PLC where FM was expected %p (%s)",
		*bufptrP, info_type((StgClosure*)*bufptrP)));

  IF_PAR_DEBUG(pack,
	       belch("*<_- Unpacked @ %p a FETCH_ME to GA ", 
		     *graphP);
	       printGA(&gaS);
	       fputc('\n', stderr));

  /* the next thing must be the IP to a FETCH_ME closure */
  ASSERT(get_itbl((StgClosure *)*bufptrP)->type == FETCH_ME);
  
  closure = *graphP;
  /* fill in the closure from the buffer */
  FillInClosure(bufptrP, closure);
  
  /* the newly built closure is a FETCH_ME */
  ASSERT(get_itbl(closure)->type == FETCH_ME);
  
  /* common up with other graph if necessary 
     this also assigns the contents of gaS to the ga field of the FM closure */
  foo = SetGAandCommonUp(&gaS, closure, rtsTrue);
  
  ASSERT(foo!=closure || LOOKS_LIKE_GA(((StgFetchMe*)closure)->ga));
  
  IF_PAR_DEBUG(pack,
	       if (foo==closure) {  // only if not commoned up 
	         belch("*<_- current FM @ %p next FM @ %p; unpacked FM @ %p is ", 
		       *graphP, *graphP+sizeofW(StgFetchMe), closure);
	         printClosure(closure);
               });
  *graphP += sizeofW(StgFetchMe);
  return foo;
}

/*
  Unpack an array of words.
  Could use generic unpack most of the time, but cleaner to separate it.
  ToDo: implement packing of MUT_ARRAYs
*/

//@cindex UnpackArray
static void
UnpackArray(StgWord ***bufptrP, StgClosure *graph)
{
  StgInfoTable *info;
  StgWord **bufptr=*bufptrP;
  nat size, ptrs, nonptrs, vhs, i, n;
  char str[80];

  /* yes, I know I am paranoid; but who's asking !? */
  IF_DEBUG(sanity,
	   info = get_closure_info((StgClosure*)bufptr, 
				   &size, &ptrs, &nonptrs, &vhs, str);
	   ASSERT(info->type == ARR_WORDS || info->type == MUT_ARR_PTRS ||
		  info->type == MUT_ARR_PTRS_FROZEN || info->type == MUT_VAR));

  n = arr_words_words(((StgArrWords *)bufptr));
  // this includes the header!: arr_words_sizeW(stgCast(StgArrWords*,q)); 

  IF_PAR_DEBUG(pack,
               if (n<100) 
	         belch("*<== unpacking an array of %d words %p (%s) (size=%d) |%s|\n",
		     n, (StgClosure*)bufptr, info_type((StgClosure*)bufptr), 
		     arr_words_sizeW((StgArrWords *)bufptr), 
		       /* print array (string?) */
                     ((StgArrWords *)graph)->payload);
               else
	         belch("*<== unpacking an array of %d words %p (%s) (size=%d)\n",
		     n, (StgClosure*)bufptr, info_type((StgClosure*)bufptr), 
		     arr_words_sizeW((StgArrWords *)bufptr)));

  /* Unpack the header (2 words: info ptr and the number of words to follow) */
  ((StgArrWords *)graph)->header.info = (StgInfoTable*)*bufptr++;  // assumes _HS==1; yuck!
  ((StgArrWords *)graph)->bytes = ((StgWord)*bufptr++) * sizeof(StgWord);

  /* unpack the payload of the closure (all non-ptrs) */
  for (i=0; i<n; i++)
    ((StgArrWords *)graph)->payload[i] = (StgWord)*bufptr++;

  ASSERT(bufptr==*bufptrP+arr_words_sizeW((StgArrWords *)*bufptrP));
  *bufptrP = bufptr;
}

/* 
   Unpack a PAP in the buffer into a heap closure.
   For each FETCHME we find in the packed PAP we have to unpack a separate
   FETCHME closure and insert a pointer to this closure into the PAP. 
   We unpack all FETCHMEs into an area after the PAP proper (the `FM area').
   Note that the size of a FETCHME in the buffer is exactly the same as
   the size of an unpacked FETCHME plus 1 word for the pointer to it.
   Therefore, we just allocate packed_size words in the heap for the unpacking.
   After this routine the heap starting from *graph looks like this:

   graph
     |
     v             PAP closure                 |   FM area        |
     +------------------------------------------------------------+
     | PAP header | n_args | fun | payload ... | FM_1 | FM_2 .... |
     +------------------------------------------------------------+

   where payload contains pointers to each of the unpacked FM_1, FM_2 ...
   The size of the PAP closure plus all FMs is _HS+2+packed_size.
*/

//@cindex UnpackPAP
static nat
UnpackPAP(StgWord ***bufptrP, StgClosure *graph) 
{
  nat n, i, j, packed_size = 0;
  StgPtr p, q, end, payload_start, p_FMs;
  const StgInfoTable* info;
  StgWord bitmap;
  StgWord **bufptr = *bufptrP;
#if defined(DEBUG)
  nat FMs_in_PAP=0;
  void checkPAPSanity(StgPAP *graph, StgPtr p_FM_begin, StgPtr p_FM_end);
#endif

  IF_PAR_DEBUG(pack,
	       belch("*<** UnpackPAP: unpacking PAP @ %p with %d words to closure %p", 
			 *bufptr, *(bufptr+1), graph));

  /* Unpack the PAP header (both fixed and variable) */
  ((StgPAP *)graph)->header.info = (StgInfoTable*)*bufptr++;
  n = ((StgPAP *)graph)->n_args = (StgWord)*bufptr++;
  ((StgPAP *)graph)->fun = (StgClosure*)*bufptr++;
  packed_size = (nat)*bufptr++;

  IF_PAR_DEBUG(pack,
	       belch("*<** UnpackPAP: PAP header is [%p, %d, %p] %d",
		     ((StgPAP *)graph)->header.info,
		     ((StgPAP *)graph)->n_args,
		     ((StgPAP *)graph)->fun,
		     packed_size));

  payload_start = (StgPtr)bufptr;
  /* p points to the current word in the heap */
  p = (StgPtr)((StgPAP *)graph)->payload;      // payload of PAP will be unpacked here
  p_FMs = (StgPtr)graph+pap_sizeW((StgPAP*)graph);  // FMs will be unpacked here
  end = (StgPtr) payload_start+packed_size;
  /*
    The main loop unpacks the PAP in *bufptr into *p, with *p_FMS as the
    FM area for unpacking all FETCHMEs encountered during unpacking.
  */
  while ((StgPtr)bufptr<end) {
    /* be sure that we don't write more than we allocated for this closure */
    ASSERT(p_FMs <= (StgPtr)(graph+_HS+2+packed_size));
    /* be sure that the unpacked PAP doesn't run into the FM area */
    ASSERT(p < (StgPtr)(graph+pap_sizeW((StgPAP*)graph)));
    /* the loop body has been borrowed from scavenge_stack */
    q = *bufptr; // let q be the contents of the current pointer into the buffer

    /* Test whether the next thing is a FETCH_ME.
       In PAPs FETCH_ME are encoded via a starting marker of ARGTAG_MAX+1
    */
    if (q==(StgPtr)(ARGTAG_MAX+1)) {
      IF_PAR_DEBUG(pack,
		   belch("*<** UnpackPAP @ %p: unpacking FM; filling in ptr to FM area: %p", 
			 p, p_FMs));
      bufptr++;         // skip ARGTAG_MAX+1 marker
      // Unpack a FM into the FM area after the PAP proper and insert pointer
      *p++ = (StgWord)UnpackFetchMe(&bufptr, (StgClosure**)&p_FMs); 
      IF_DEBUG(sanity, FMs_in_PAP++);
      continue;
    }

    /* Test whether it is a PLC */
    if (q==(StgPtr)0) { // same as isFixed(q)
      IF_PAR_DEBUG(pack,
		   belch("*<** UnpackPAP @ %p: unpacking PLC to %p", 
			 p, *(bufptr+1)));
      bufptr++;          // skip 0 marker
      *p++ = (StgWord)*bufptr++;
      continue;
    }

    /* If we've got a tag, pack all words in that block */
    if (IS_ARG_TAG((W_)q)) {   // q stands for the no. of non-ptrs to follow
      nat m = ARG_SIZE(q);     // first word after this block
      IF_PAR_DEBUG(pack,
		   belch("*<** UnpackPAP @ %p: unpacking %d words (tagged), starting @ %p", 
			 p, m, p));
      for (i=0; i<m+1; i++)
	*p++ = (StgWord)*bufptr++;
      continue;
    }

    /* 
     * Otherwise, q must be the info pointer of an activation
     * record.  All activation records have 'bitmap' style layout
     * info.
     */
    info  = get_itbl((StgClosure *)q);
    switch (info->type) {
	
      /* Dynamic bitmap: the mask is stored on the stack */
    case RET_DYN:
      IF_PAR_DEBUG(pack,
		   belch("*<** UnpackPAP @ %p: RET_DYN", 
			 p));

      /* Pack the header as is */
      ((StgRetDyn *)p)->info     = (StgWord)*bufptr++;
      ((StgRetDyn *)p)->liveness = (StgWord)*bufptr++;
      ((StgRetDyn *)p)->ret_addr = (StgWord)*bufptr++;
      p += 3;

      //bitmap = ((StgRetDyn *)p)->liveness;
      //p      = (P_)&((StgRetDyn *)p)->payload[0];
      goto small_bitmap;

      /* probably a slow-entry point return address: */
    case FUN:
    case FUN_STATIC:
      {
      IF_PAR_DEBUG(pack,
		   belch("*<** UnpackPAP @ %p: FUN or FUN_STATIC", 
			 p));

      ((StgClosure *)p)->header.info = (StgInfoTable*)*bufptr;
      p++;

      goto follow_srt; //??
      }

      /* Using generic code here; could inline as in scavenge_stack */
    case UPDATE_FRAME:
      {
	StgUpdateFrame *frame = (StgUpdateFrame *)p;
	//nat type = get_itbl(frame->updatee)->type;

	//ASSERT(type==BLACKHOLE || type==CAF_BLACKHOLE || type==BLACKHOLE_BQ);

	IF_PAR_DEBUG(pack,
		     belch("*<** UnpackPAP @ %p: UPDATE_FRAME", 
			   p));

	((StgUpdateFrame *)p)->header.info = (StgInfoTable*)*bufptr++;
	((StgUpdateFrame *)p)->link        = (StgUpdateFrame*)*bufptr++;     // ToDo: fix intra-stack pointer
	((StgUpdateFrame *)p)->updatee     = (StgClosure*)*bufptr++;   // ToDo: follow link 

	p += 3;
      }

      /* small bitmap (< 32 entries, or 64 on a 64-bit machine) */
    case STOP_FRAME:
      {
	IF_PAR_DEBUG(pack,
		     belch("*<** UnpackPAP @ %p: STOP_FRAME", 
			   p));
	((StgStopFrame *)p)->header.info = (StgInfoTable*)*bufptr;
	p++;
      }

    case CATCH_FRAME:
      {
	IF_PAR_DEBUG(pack,
		     belch("*<** UnpackPAP @ %p: CATCH_FRAME",
			   p));

	((StgCatchFrame *)p)->header.info = (StgInfoTable*)*bufptr++;
	((StgCatchFrame *)p)->link        = (StgUpdateFrame*)*bufptr++;
	((StgCatchFrame *)p)->exceptions_blocked = (StgInt)*bufptr++;
	((StgCatchFrame *)p)->handler     = (StgClosure*)*bufptr++;
	p += 4;
      }

    case SEQ_FRAME:
      {
	IF_PAR_DEBUG(pack,
		     belch("*<** UnpackPAP @ %p: UPDATE_FRAME",
			   p));

	((StgSeqFrame *)p)->header.info = (StgInfoTable*)*bufptr++;
	((StgSeqFrame *)p)->link        = (StgUpdateFrame*)*bufptr++;

        // ToDo: handle bitmap
        bitmap = info->layout.bitmap;

        p = (StgPtr)&(((StgClosure *)p)->payload);
        goto small_bitmap;
      }
    case RET_BCO:
    case RET_SMALL:
    case RET_VEC_SMALL:
      IF_PAR_DEBUG(pack,
		   belch("*<** UnpackPAP @ %p: RET_{BCO,SMALL,VEC_SMALL}",
			 p));


      ((StgClosure *)p)->header.info = (StgInfoTable*)*bufptr++;
      p++;
      // ToDo: handle bitmap
      bitmap = info->layout.bitmap;
      /* this assumes that the payload starts immediately after the info-ptr */

    small_bitmap:
      while (bitmap != 0) {
	if ((bitmap & 1) == 0) {
	  *p++ = (StgWord)UnpackFetchMe(&bufptr, (StgClosure**)&p_FMs);
	  IF_DEBUG(sanity, FMs_in_PAP++);
	} else {
	  *p++ = (StgWord)*bufptr++;
	}
	bitmap = bitmap >> 1;
      }
      
    follow_srt:
      belch("*<-- UnpackPAP: nothing to do for follow_srt");
      continue;

      /* large bitmap (> 32 entries) */
    case RET_BIG:
    case RET_VEC_BIG:
      {
	StgPtr q;
	StgLargeBitmap *large_bitmap;

	IF_PAR_DEBUG(pack,
		     belch("*<** UnpackPAP @ %p: RET_{BIG,VEC_BIG} (large_bitmap=%p)", 
			   p, info->layout.large_bitmap));


	((StgClosure *)p)->header.info = (StgInfoTable*)*bufptr++;
	p++;

	large_bitmap = info->layout.large_bitmap;

	for (j=0; j<large_bitmap->size; j++) {
	  bitmap = large_bitmap->bitmap[j];
	  q = p + BITS_IN(W_);
	  while (bitmap != 0) {
	    if ((bitmap & 1) == 0) {
	      *p++ = (StgWord)UnpackFetchMe(&bufptr, (StgClosure**)&p_FMs);
	      IF_DEBUG(sanity, FMs_in_PAP++);
	    } else {
	      *p++ = (StgWord)*bufptr;
	    }
	    bitmap = bitmap >> 1;
	  }
	  if (j+1 < large_bitmap->size) {
	    while (p < q) {
	      *p++ = (StgWord)UnpackFetchMe(&bufptr, (StgClosure**)&p_FMs);
	      IF_DEBUG(sanity, FMs_in_PAP++);
	    }
	  }
	}

	/* and don't forget to follow the SRT */
	goto follow_srt;
      }

    default:
      barf("UnpackPAP: weird activation record found on stack: %d", 
	   (int)(info->type));
    }
  }
  IF_PAR_DEBUG(pack,
	       belch("*<** UnpackPAP finished; unpacked closure @ %p is:",
		     (StgClosure *)graph);
	       printClosure((StgClosure *)graph));

  IF_DEBUG(sanity,               /* check sanity of unpacked PAP */
	   checkClosure(graph));

  *bufptrP = bufptr;
  /* 
     Now p points to the first word after the PAP proper and p_FMs points 
     to the next free word in the heap; everything between p and p_FMs are 
     FETCHMEs 
  */
  IF_DEBUG(sanity,
	   checkPAPSanity(graph, p, p_FMs));

  /* we have to return the size of PAP + FMs as size of the unpacked thing */
  ASSERT(graph+pap_sizeW((StgPAP*)graph)==p);
  return (nat)((StgClosure*)p_FMs-graph);
}

#if defined(DEBUG)
/* 
   Check sanity of a PAP after unpacking the PAP.
   This means that there is slice of heap after the PAP containing FETCHMEs
*/
void
checkPAPSanity(StgPAP *graph, StgPtr p_FM_begin, StgPtr p_FM_end)
{
  StgPtr xx;

  /* check that the main unpacked closure is a PAP */
  ASSERT(graph->header.info = &stg_PAP_info);
  checkClosure(graph);
  /* check that all of the closures in the FM-area are FETCHMEs */
  for (xx=p_FM_begin; xx<p_FM_end; xx += sizeofW(StgFetchMe)) {
    /* must be a FETCHME closure */
    ASSERT(((StgClosure*)xx)->header.info == &stg_FETCH_ME_info);
    /* it might have been commoned up (=> marked as garbage);
       otherwise it points to a GA */
    ASSERT((((StgFetchMe*)xx)->ga)==GARBAGE_MARKER ||
	   LOOKS_LIKE_GA(((StgFetchMe*)xx)->ga));
  }
  /* traverse the payload of the PAP */
  for (xx=graph->payload; xx-(StgPtr)(graph->payload)<graph->n_args; xx++) {
    /* if the current elem is a pointer into the FM area, check that
       the GA field is ok */
    ASSERT(!(p_FM_begin<(StgPtr)*xx && (StgPtr)*xx<p_FM_end) ||
	   LOOKS_LIKE_GA(((StgFetchMe*)*xx)->ga));
  }
}
#endif  /* DEBUG */
#endif  /* PAR */

//@node GranSim Code,  , GUM code, Unpacking routines
//@subsubsection GranSim Code

/*
   For GrAnSim: No actual unpacking should be necessary. We just
   have to walk over the graph and set the bitmasks appropriately.
   Since we use RBHs similarly to GUM but without an ACK message/event
   we have to revert the RBH from within the UnpackGraph routine (good luck!)
   -- HWL 
*/

#if defined(GRAN)
void
CommonUp(StgClosure *src, StgClosure *dst)
{
  barf("CommonUp: should never be entered in a GranSim setup");
}

StgClosure*
UnpackGraph(buffer)
rtsPackBuffer* buffer;
{
  nat size, ptrs, nonptrs, vhs,
      bufptr = 0;
  StgClosure *closure, *graphroot, *graph;
  StgInfoTable *ip;
  StgWord bufsize, unpackedsize,
          pptr = 0, pptrs = 0, pvhs;
  StgTSO* tso;
  char str[240], str1[80];
  int i;

  bufptr = 0;
  graphroot = buffer->buffer[0];

  tso = buffer->tso;

  /* Unpack the header */
  unpackedsize = buffer->unpacked_size;
  bufsize = buffer->size;

  IF_GRAN_DEBUG(pack,
		belch("<<< Unpacking <<%d>> (buffer @ %p):\n    (root @ %p, PE %d,size=%d), demanded by TSO %d (%p)[PE %d]",
		      buffer->id, buffer, graphroot, where_is(graphroot), 
		      bufsize, tso->id, tso, 
		      where_is((StgClosure *)tso)));

  do {
    closure = buffer->buffer[bufptr++]; /* that's all we need for GrAnSim -- HWL */
      
    /* Actually only ip is needed; rest is useful for TESTING -- HWL */
    ip = get_closure_info(closure, 
			  &size, &ptrs, &nonptrs, &vhs, str);
      
    IF_GRAN_DEBUG(pack,
		  sprintf(str, "**    (%p): Changing bitmask[%s]: 0x%x ",
			  closure, (closure_HNF(closure) ? "NF" : "__"),
			  PROCS(closure)));

    if (get_itbl(closure)->type == RBH) {
      /* if it's an RBH, we have to revert it into a normal closure, thereby
	 awakening the blocking queue; not that this is code currently not
	 needed in GUM, but it should be added with the new features in
	 GdH (and the implementation of an NACK message)
      */
      // closure->header.gran.procs = PE_NUMBER(CurrentProc);
      SET_GRAN_HDR(closure, PE_NUMBER(CurrentProc));    /* Move node */

      IF_GRAN_DEBUG(pack,
		    strcat(str, " (converting RBH) ")); 

      convertFromRBH(closure);   /* In GUM that's done by convertToFetchMe */

      IF_GRAN_DEBUG(pack,
		    belch("::  closure %p (%s) is a RBH; after reverting: IP=%p",
			  closure, info_type(closure), get_itbl(closure)));
    } else if (IS_BLACK_HOLE(closure)) {
      IF_GRAN_DEBUG(pack,
		    belch("::  closure %p (%s) is a BH; copying node to %d",
			  closure, info_type(closure), CurrentProc));
      closure->header.gran.procs |= PE_NUMBER(CurrentProc); /* Copy node */
    } else if ( (closure->header.gran.procs & PE_NUMBER(CurrentProc)) == 0 ) {
      if (closure_HNF(closure)) {
	IF_GRAN_DEBUG(pack,
		      belch("::  closure %p (%s) is a HNF; copying node to %d",
			    closure, info_type(closure), CurrentProc));
	closure->header.gran.procs |= PE_NUMBER(CurrentProc); /* Copy node */
      } else { 
	IF_GRAN_DEBUG(pack,
		      belch("::  closure %p (%s) is no (R)BH or HNF; moving node to %d",
			    closure, info_type(closure), CurrentProc));
	closure->header.gran.procs = PE_NUMBER(CurrentProc);  /* Move node */
      }
    }

    IF_GRAN_DEBUG(pack,
		  sprintf(str1, "0x%x",   PROCS(closure)); strcat(str, str1));
    IF_GRAN_DEBUG(pack, belch(str));
    
  } while (bufptr<buffer->size) ;   /*  (parent != NULL);  */

  /* In GrAnSim we allocate pack buffers dynamically! -- HWL */
  free(buffer->buffer);
  free(buffer);

  IF_GRAN_DEBUG(pack,
		belch("PrintGraph of %p is:", graphroot); PrintGraph(graphroot,0));

  return (graphroot);
}
#endif  /* GRAN */

//@node Aux fcts for packing, Printing Packet Contents, Unpacking routines, Graph packing
//@subsection Aux fcts for packing

//@menu
//* Offset table::		
//* Packet size::		
//* Types of Global Addresses::	 
//* Closure Info::		
//@end menu

//@node Offset table, Packet size, Aux fcts for packing, Aux fcts for packing
//@subsubsection Offset table

/*
   DonePacking is called when we've finished packing.  It releases memory
   etc.  */

//@cindex DonePacking

# if defined(PAR)

static void
DonePacking(void)
{
  freeHashTable(offsetTable, NULL);
  offsetTable = NULL;
}

/*
   AmPacking records that the closure is being packed.  Note the abuse of
   the data field in the hash table -- this saves calling @malloc@!  */

//@cindex AmPacking

static void
AmPacking(closure)
StgClosure *closure;
{
  insertHashTable(offsetTable, (StgWord) closure, (void *) (StgWord) pack_locn);
}

/*
   OffsetFor returns an offset for a closure which is already being packed.  */

//@cindex OffsetFor

static int
OffsetFor(closure)
StgClosure *closure;
{
  return (int) (StgWord) lookupHashTable(offsetTable, (StgWord) closure);
}

/*
   NotYetPacking determines whether the closure's already being packed.
   Offsets $<$ @PACK_HDR_SIZE@ (e.g. 0) mean no.  */

//@cindex NotYetPacking

static rtsBool
NotYetPacking(offset)
int offset;
{
  return(offset == 0); // ToDo: what if root is found again?? FIX 
}

# else  /* GRAN */

static void
DonePacking(void)
{
  /* nothing */
}

/* 
   NotYetPacking searches through the whole pack buffer for closure.  */

static rtsBool
NotYetPacking(closure)
StgClosure *closure;
{ nat i;
  rtsBool found = rtsFalse;

  for (i=0; (i<pack_locn) && !found; i++)
    found = globalPackBuffer->buffer[i]==closure;

  return (!found);
}
# endif

//@node Packet size, Closure Info, Offset table, Aux fcts for packing
//@subsubsection Packet size

/* 
   The size needed if all currently queued closures are packed as FETCH_ME
   closures. This represents the headroom we must have when packing the
   buffer in order to maintain all links in the graphs.
*/
// ToDo: check and merge cases
#if defined(PAR)
static nat
QueuedClosuresMinSize (nat ptrs) {
  return ((clq_size - clq_pos) + ptrs) * PACK_FETCHME_SIZE;
}
#else /* GRAN */
static nat
QueuedClosuresMinSize (nat ptrs) {
  return ((clq_size - clq_pos) + ptrs) * PACK_FETCHME_SIZE;
}
#endif 

/*
  RoomToPack determines whether there's room to pack the closure into
  the pack buffer based on 

  o how full the buffer is already,
  o the closures' size and number of pointers (which must be packed as GAs),
  o the size and number of pointers held by any primitive arrays that it 
    points to
  
    It has a *side-effect* (naughty, naughty) in assigning roomInBuffer 
    to rtsFalse.
*/

//@cindex RoomToPack
static rtsBool
RoomToPack(size, ptrs)
nat size, ptrs;
{
# if defined(PAR)
  if (roomInBuffer &&
      (pack_locn +                 // where we are in the buffer right now
       size +                      // space needed for the current closure
       QueuedClosuresMinSize(ptrs) // space for queued closures as FETCH_MEs
       + 1                         // headroom (DEBUGGING only)
       >= 
       RTS_PACK_BUFFER_SIZE))
    {
      roomInBuffer = rtsFalse;
    }
# else   /* GRAN */
  if (roomInBuffer &&
      (unpacked_size + 
       size +
       QueuedClosuresMinSize(ptrs)
       >= 
       RTS_PACK_BUFFER_SIZE))
    {
      roomInBuffer = rtsFalse;
    }
# endif
  return (roomInBuffer);
}

//@node Closure Info,  , Packet size, Aux fcts for packing
//@subsubsection Closure Info

/*
   Closure Info

   @get_closure_info@ determines the size, number of pointers etc. for this
   type of closure -- see @SMInfoTables.lh@ for the legal info. types etc.

[Can someone please keep this function up to date.  I keep needing it
 (or something similar) for interpretive code, and it keeps
 bit-rotting.  {\em It really belongs somewhere else too}.  KH @@ 17/2/95] */

#if 0

// {Parallel.h}Daq ngoqvam vIroQpu'

# if defined(GRAN) || defined(PAR)
/* extracting specific info out of closure; currently only used in GRAN -- HWL */
//@cindex get_closure_info
StgInfoTable*
get_closure_info(node, size, ptrs, nonptrs, vhs, info_hdr_ty)
StgClosure* node;
nat *size, *ptrs, *nonptrs, *vhs;
char *info_hdr_ty;
{
  StgInfoTable *info;

  info = get_itbl(node);
  /* the switch shouldn't be necessary, really; just use default case */
  switch (info->type) {
#if 0
   case CONSTR_1_0:
   case THUNK_1_0:
   case FUN_1_0:
     *size = sizeW_fromITBL(info);
     *ptrs = (nat) 1; // (info->layout.payload.ptrs);
     *nonptrs = (nat) 0; // (info->layout.payload.nptrs);
     *vhs = (nat) 0; // unknown
     info_hdr_type(node, info_hdr_ty);
     return info;
     
  case CONSTR_0_1:
  case THUNK_0_1:
  case FUN_0_1:
     *size = sizeW_fromITBL(info);
     *ptrs = (nat) 0; // (info->layout.payload.ptrs);
     *nonptrs = (nat) 1; // (info->layout.payload.nptrs);
     *vhs = (nat) 0; // unknown
     info_hdr_type(node, info_hdr_ty);
     return info;

  case CONSTR_2_0:
  case THUNK_2_0:
  case FUN_2_0:
     *size = sizeW_fromITBL(info);
     *ptrs = (nat) 2; // (info->layout.payload.ptrs);
     *nonptrs = (nat) 0; // (info->layout.payload.nptrs);
     *vhs = (nat) 0; // unknown
     info_hdr_type(node, info_hdr_ty);
     return info;

  case CONSTR_1_1:
  case THUNK_1_1:
  case FUN_1_1:
     *size = sizeW_fromITBL(info);
     *ptrs = (nat) 1; // (info->layout.payload.ptrs);
     *nonptrs = (nat) 1; // (info->layout.payload.nptrs);
     *vhs = (nat) 0; // unknown
     info_hdr_type(node, info_hdr_ty);
     return info;

  case CONSTR_0_2:
  case THUNK_0_2:
  case FUN_0_2:
     *size = sizeW_fromITBL(info);
     *ptrs = (nat) 0; // (info->layout.payload.ptrs);
     *nonptrs = (nat) 2; // (info->layout.payload.nptrs);
     *vhs = (nat) 0; // unknown
     info_hdr_type(node, info_hdr_ty);
     return info;
#endif
  case RBH:
    {
      StgInfoTable *rip = REVERT_INFOPTR(info); // closure to revert to
      *size = sizeW_fromITBL(rip);
      *ptrs = (nat) (rip->layout.payload.ptrs);
      *nonptrs = (nat) (rip->layout.payload.nptrs);
      *vhs = (nat) 0; // unknown
      info_hdr_type(node, info_hdr_ty);
      return rip;  // NB: we return the reverted info ptr for a RBH!!!!!!
    }

  default:
    *size = sizeW_fromITBL(info);
    *ptrs = (nat) (info->layout.payload.ptrs);
    *nonptrs = (nat) (info->layout.payload.nptrs);
    *vhs = (nat) 0; // unknown
    info_hdr_type(node, info_hdr_ty);
    return info;
  }
} 

//@cindex IS_BLACK_HOLE
rtsBool
IS_BLACK_HOLE(StgClosure* node)          
{ 
  StgInfoTable *info;
  info = get_itbl(node);
  return ((info->type == BLACKHOLE || info->type == RBH) ? rtsTrue : rtsFalse);
}

//@cindex IS_INDIRECTION
StgClosure *
IS_INDIRECTION(StgClosure* node)          
{ 
  StgInfoTable *info;
  info = get_itbl(node);
  switch (info->type) {
    case IND:
    case IND_OLDGEN:
    case IND_PERM:
    case IND_OLDGEN_PERM:
    case IND_STATIC:
      /* relies on indirectee being at same place for all these closure types */
      return (((StgInd*)node) -> indirectee);
    default:
      return NULL;
  }
}

/*
rtsBool
IS_THUNK(StgClosure* node)
{
  StgInfoTable *info;
  info = get_itbl(node);
  return ((info->type == THUNK ||
	   info->type == THUNK_STATIC ||
	   info->type == THUNK_SELECTOR) ? rtsTrue : rtsFalse);
}
*/

# endif /* GRAN */
#endif /* 0 */

# if 0
/* ngoq ngo' */

P_
get_closure_info(closure, size, ptrs, nonptrs, vhs, type)
P_ closure;
W_ *size, *ptrs, *nonptrs, *vhs;
char *type;
{
   P_ ip = (P_) INFO_PTR(closure);

   if (closure==NULL) {
     fprintf(stderr, "Qagh {get_closure_info}Daq: NULL closure\n");
     *size = *ptrs = *nonptrs = *vhs = 0; 
     strcpy(type,"ERROR in get_closure_info");
     return;
   } else if (closure==PrelBase_Z91Z93_closure) {
     /* fprintf(stderr, "Qagh {get_closure_info}Daq: PrelBase_Z91Z93_closure closure\n"); */
     *size = *ptrs = *nonptrs = *vhs = 0; 
     strcpy(type,"PrelBase_Z91Z93_closure");
     return;
   };

    ip = (P_) INFO_PTR(closure);

    switch (INFO_TYPE(ip)) {
    case INFO_SPEC_U_TYPE:
    case INFO_SPEC_S_TYPE:
    case INFO_SPEC_N_TYPE:
	*size = SPEC_CLOSURE_SIZE(closure);
	*ptrs = SPEC_CLOSURE_NoPTRS(closure);
	*nonptrs = SPEC_CLOSURE_NoNONPTRS(closure);
	*vhs = 0 /*SPEC_VHS*/;
	strcpy(type,"SPEC");
	break;

    case INFO_GEN_U_TYPE:
    case INFO_GEN_S_TYPE:
    case INFO_GEN_N_TYPE:
	*size = GEN_CLOSURE_SIZE(closure);
	*ptrs = GEN_CLOSURE_NoPTRS(closure);
	*nonptrs = GEN_CLOSURE_NoNONPTRS(closure);
	*vhs = GEN_VHS;
	strcpy(type,"GEN");
	break;

    case INFO_DYN_TYPE:
	*size = DYN_CLOSURE_SIZE(closure);
	*ptrs = DYN_CLOSURE_NoPTRS(closure);
	*nonptrs = DYN_CLOSURE_NoNONPTRS(closure);
	*vhs = DYN_VHS;
	strcpy(type,"DYN");
	break;

    case INFO_TUPLE_TYPE:
	*size = TUPLE_CLOSURE_SIZE(closure);
	*ptrs = TUPLE_CLOSURE_NoPTRS(closure);
	*nonptrs = TUPLE_CLOSURE_NoNONPTRS(closure);
	*vhs = TUPLE_VHS;
	strcpy(type,"TUPLE");
	break;

    case INFO_DATA_TYPE:
	*size = DATA_CLOSURE_SIZE(closure);
	*ptrs = DATA_CLOSURE_NoPTRS(closure);
	*nonptrs = DATA_CLOSURE_NoNONPTRS(closure);
	*vhs = DATA_VHS;
	strcpy(type,"DATA");
	break;

    case INFO_IMMUTUPLE_TYPE:
    case INFO_MUTUPLE_TYPE:
	*size = MUTUPLE_CLOSURE_SIZE(closure);
	*ptrs = MUTUPLE_CLOSURE_NoPTRS(closure);
	*nonptrs = MUTUPLE_CLOSURE_NoNONPTRS(closure);
	*vhs = MUTUPLE_VHS;
	strcpy(type,"(IM)MUTUPLE");
	break;

    case INFO_STATIC_TYPE:
	*size = STATIC_CLOSURE_SIZE(closure);
	*ptrs = STATIC_CLOSURE_NoPTRS(closure);
	*nonptrs = STATIC_CLOSURE_NoNONPTRS(closure);
	*vhs = STATIC_VHS;
	strcpy(type,"STATIC");
	break;

    case INFO_CAF_TYPE:
    case INFO_IND_TYPE:
	*size = IND_CLOSURE_SIZE(closure);
	*ptrs = IND_CLOSURE_NoPTRS(closure);
	*nonptrs = IND_CLOSURE_NoNONPTRS(closure);
	*vhs = IND_VHS;
	strcpy(type,"CAF|IND");
	break;

    case INFO_CONST_TYPE:
	*size = CONST_CLOSURE_SIZE(closure);
	*ptrs = CONST_CLOSURE_NoPTRS(closure);
	*nonptrs = CONST_CLOSURE_NoNONPTRS(closure);
	*vhs = CONST_VHS;
	strcpy(type,"CONST");
	break;

    case INFO_SPEC_RBH_TYPE:
	*size = SPEC_RBH_CLOSURE_SIZE(closure);
	*ptrs = SPEC_RBH_CLOSURE_NoPTRS(closure);
	*nonptrs = SPEC_RBH_CLOSURE_NoNONPTRS(closure);
	if (*ptrs <= 2) {
	    *nonptrs -= (2 - *ptrs);
	    *ptrs = 1;
	} else
	    *ptrs -= 1;
	*vhs = SPEC_RBH_VHS;
	strcpy(type,"SPEC_RBH");
	break;

    case INFO_GEN_RBH_TYPE:
	*size = GEN_RBH_CLOSURE_SIZE(closure);
	*ptrs = GEN_RBH_CLOSURE_NoPTRS(closure);
	*nonptrs = GEN_RBH_CLOSURE_NoNONPTRS(closure);
	if (*ptrs <= 2) {
	    *nonptrs -= (2 - *ptrs);
	    *ptrs = 1;
	} else
	    *ptrs -= 1;
	*vhs = GEN_RBH_VHS;
	strcpy(type,"GEN_RBH");
	break;

    case INFO_CHARLIKE_TYPE:
	*size = CHARLIKE_CLOSURE_SIZE(closure);
	*ptrs = CHARLIKE_CLOSURE_NoPTRS(closure);
	*nonptrs = CHARLIKE_CLOSURE_NoNONPTRS(closure);
	*vhs = CHARLIKE_VHS;
	strcpy(type,"CHARLIKE");
	break;

    case INFO_INTLIKE_TYPE:
	*size = INTLIKE_CLOSURE_SIZE(closure);
	*ptrs = INTLIKE_CLOSURE_NoPTRS(closure);
	*nonptrs = INTLIKE_CLOSURE_NoNONPTRS(closure);
	*vhs = INTLIKE_VHS;
	strcpy(type,"INTLIKE");
	break;

#  if !defined(GRAN)
    case INFO_FETCHME_TYPE:
	*size = FETCHME_CLOSURE_SIZE(closure);
        *ptrs = FETCHME_CLOSURE_NoPTRS(closure);
        *nonptrs = FETCHME_CLOSURE_NoNONPTRS(closure);
        *vhs = FETCHME_VHS;
	strcpy(type,"FETCHME");
	break;

    case INFO_FMBQ_TYPE:
	*size = FMBQ_CLOSURE_SIZE(closure);
        *ptrs = FMBQ_CLOSURE_NoPTRS(closure);
        *nonptrs = FMBQ_CLOSURE_NoNONPTRS(closure);
        *vhs = FMBQ_VHS;
	strcpy(type,"FMBQ");
	break;
#  endif

    case INFO_BQ_TYPE:
	*size = BQ_CLOSURE_SIZE(closure);
        *ptrs = BQ_CLOSURE_NoPTRS(closure);
        *nonptrs = BQ_CLOSURE_NoNONPTRS(closure);
        *vhs = BQ_VHS;
	strcpy(type,"BQ");
	break;

    case INFO_BH_TYPE:
	*size = BH_CLOSURE_SIZE(closure);
        *ptrs = BH_CLOSURE_NoPTRS(closure);
        *nonptrs = BH_CLOSURE_NoNONPTRS(closure);
        *vhs = BH_VHS;
	strcpy(type,"BH");
	break;

    case INFO_TSO_TYPE:
	*size = 0; /* TSO_CLOSURE_SIZE(closure); */
        *ptrs = 0; /* TSO_CLOSURE_NoPTRS(closure); */
        *nonptrs = 0; /* TSO_CLOSURE_NoNONPTRS(closure); */
        *vhs = TSO_VHS;
	strcpy(type,"TSO");
	break;

    case INFO_STKO_TYPE:
        *size = 0;
    	*ptrs = 0;
        *nonptrs = 0;
    	*vhs = STKO_VHS;
    	strcpy(type,"STKO");
        break;

    default:
	fprintf(stderr, "get_closure_info:  Unexpected closure type (%lu), closure %lx\n",
	  INFO_TYPE(ip), (StgWord) closure);
	EXIT(EXIT_FAILURE);
    }

    return ip;
}
# endif

# if 0
// Use allocate in Storage.c instead
/*
   @AllocateHeap@ will bump the heap pointer by @size@ words if the space
   is available, but it will not perform garbage collection.
   ToDo: check whether we can use an existing STG allocation routine -- HWL
*/


//@cindex AllocateHeap
StgPtr
AllocateHeap(size)
nat size;
{
  StgPtr newClosure;
  
  /* Allocate a new closure */
  if (Hp + size > HpLim)
    return NULL;
  
  newClosure = Hp + 1;
  Hp += size;
  
  return newClosure;
}
# endif

# if defined(PAR)

//@cindex doGlobalGC
void
doGlobalGC(void)
{
  fprintf(stderr,"Splat -- we just hit global GC!\n");
  stg_exit(EXIT_FAILURE);
  //fishing = rtsFalse;
  outstandingFishes--;
}

# endif /* PAR */

//@node Printing Packet Contents, End of file, Aux fcts for packing, Graph packing
//@subsection Printing Packet Contents
/*
  Printing Packet Contents
  */

#if defined(DEBUG) || defined(GRAN_CHECK)

//@cindex PrintPacket

#if defined(PAR)
void
PrintPacket(packBuffer)
rtsPackBuffer *packBuffer;
{
  StgClosure *parent, *graphroot, *closure_start;
  const StgInfoTable *ip;
  globalAddr ga;
  StgWord **bufptr, **slotptr;

  nat bufsize;
  nat pptr = 0, pptrs = 0, pvhs;
  nat locn = 0;
  nat i;
  nat size, ptrs, nonptrs, vhs;
  char str[80];

  /* disable printing if a non-std globalisation scheme is used; ToDo: FIX */
  if (RtsFlags.ParFlags.globalising != 0)
    return;

  /* NB: this whole routine is more or less a copy of UnpackGraph with all
     unpacking components replaced by printing fcts
     Long live higher-order fcts!
  */
  /* Initialisation */
  //InitPackBuffer();                  /* in case it isn't already init'd */
  InitClosureQueue();
  // ASSERT(gaga==PendingGABuffer); 
  graphroot = (StgClosure *)NULL;

  /* Unpack the header */
  bufsize = packBuffer->size;
  bufptr = packBuffer->buffer;

  fprintf(stderr, "*. Printing <<%d>> (buffer @ %p):\n", 
	  packBuffer->id, packBuffer);
  fprintf(stderr, "*.   size: %d; unpacked_size: %d; tso: %p; buffer: %p\n",
	  packBuffer->size, packBuffer->unpacked_size, 
	  packBuffer->tso, packBuffer->buffer);

  parent = (StgClosure *)NULL;

  do {
    /* This is where we will ultimately save the closure's address */
    slotptr = bufptr;
    locn = slotptr-(packBuffer->buffer); // index of closure in buffer

    /* First, unpack the next GA or PLC */
    ga.weight = (rtsWeight) *bufptr++;

    if (ga.weight == 2) {  // unglobalised closure to follow
      // nothing to do; closure starts at *bufptr
    } else if (ga.weight > 0) { // fill in GA
      ga.payload.gc.gtid = (GlobalTaskId) *bufptr++;
      ga.payload.gc.slot = (int) *bufptr++;
    } else
      ga.payload.plc = (StgPtr) *bufptr++;
    
    /* Now unpack the closure body, if there is one */
    if (isFixed(&ga)) {
      fprintf(stderr, "*. %u: PLC @ %p\n", locn, ga.payload.plc);
      // closure = ga.payload.plc;
    } else if (isOffset(&ga)) {
      fprintf(stderr, "*. %u: OFFSET TO %d\n", locn, ga.payload.gc.slot);
      // closure = (StgClosure *) buffer[ga.payload.gc.slot];
    } else {
      /* Print normal closures */

      ASSERT(bufsize > 0);

      fprintf(stderr, "*. %u: ((%x, %d, %x)) ", locn,
              ga.payload.gc.gtid, ga.payload.gc.slot, ga.weight);

      closure_start = (StgClosure*)bufptr;
      ip = get_closure_info((StgClosure *)bufptr, 
			    &size, &ptrs, &nonptrs, &vhs, str);
	  
      /* ToDo: check whether this is really needed */
      if (ip->type == FETCH_ME || ip->type == REMOTE_REF) {
	size = _HS;
	ptrs = nonptrs = vhs = 0;
      }
      /* ToDo: check whether this is really needed */
      if (ip->type == ARR_WORDS) {
	ptrs = vhs = 0;
	nonptrs = arr_words_words(((StgArrWords *)bufptr));
	size = arr_words_sizeW((StgArrWords *)bufptr);
      }

      /* special code for printing a PAP in a buffer */
      if (ip->type == PAP || ip->type == AP_UPD) {
        vhs = 3; 
	ptrs = 0;
        nonptrs = (nat)((StgPAP *)bufptr)->payload[0];
	size = _HS+vhs+ptrs+nonptrs;
      }

      /* 
	 Remember, the generic closure layout is as follows:
	 +-------------------------------------------------+
	 | FIXED HEADER | VARIABLE HEADER | PTRS | NON-PRS |
	 +-------------------------------------------------+
      */
      /* Print fixed header */
      fprintf(stderr, "FH ["); 
      for (i = 0; i < _HS; i++)
	fprintf(stderr, " %p", *bufptr++);

      if (ip->type == FETCH_ME || ip->type == REMOTE_REF)
	size = ptrs = nonptrs = vhs = 0;

      // VH is always empty in the new RTS
      ASSERT(vhs==0 ||
             ip->type == PAP || ip->type == AP_UPD);
      /* Print variable header */
      fprintf(stderr, "] VH ["); 
      for (i = 0; i < vhs; i++)
	fprintf(stderr, " %p", *bufptr++);

      //fprintf(stderr, "] %d PTRS [", ptrs); 
      /* Pointers will be filled in later */

      fprintf(stderr, " ] (%d, %d) [", ptrs, nonptrs); 
      /* Print non-pointers */
      for (i = 0; i < nonptrs; i++)
	fprintf(stderr, " %p", *bufptr++);

      fprintf(stderr, "] (%s)\n", str);

      /* Indirections are never packed */
      // ASSERT(INFO_PTR(graph) != (W_) Ind_info_TO_USE);

      /* Add to queue for processing 
	 When just printing the packet we do not have an unpacked closure
	 in hand, so we feed it the packet entry; 
	 again, this assumes that at least the fixed header of the closure
	 has the same layout in the packet; also we may not overwrite entries
	 in the packet (done in Unpack), but for printing that's a bad idea
	 anyway */
      QueueClosure((StgClosure *)closure_start);
	
      /* No Common up needed for printing */

      /* No Sort out the global address mapping for printing */

    } /* normal closure case */

    /* Locate next parent pointer */
    pptr++;
    while (pptr + 1 > pptrs) {
      parent = DeQueueClosure();

      if (parent == NULL)
	break;
      else {
	(void) get_closure_info(parent, &size, &pptrs, &nonptrs,
					&pvhs, str);
	pptr = 0;
      }
    }
  } while (parent != NULL);
  fprintf(stderr, "*. --- End packet <<%d>> (claimed size=%d; real size=%d)---\n", 
	  packBuffer->id, packBuffer->size, size);

}

/*
  Doing a sanity check on a packet.
  This does a full iteration over the packet, as in PrintPacket.
*/
//@cindex checkPacket
void
checkPacket(packBuffer)
rtsPackBuffer *packBuffer;
{
  StgClosure *parent, *graphroot, *closure_start;
  const StgInfoTable *ip;
  globalAddr ga;
  StgWord **bufptr, **slotptr;

  nat bufsize;
  nat pptr = 0, pptrs = 0, pvhs;
  nat locn = 0;
  nat size, ptrs, nonptrs, vhs;
  char str[80];

  /* NB: this whole routine is more or less a copy of UnpackGraph with all
     unpacking components replaced by printing fcts
     Long live higher-order fcts!
  */
  /* Initialisation */
  //InitPackBuffer();                  /* in case it isn't already init'd */
  InitClosureQueue();
  // ASSERT(gaga==PendingGABuffer); 
  graphroot = (StgClosure *)NULL;

  /* Unpack the header */
  bufsize = packBuffer->size;
  bufptr = packBuffer->buffer;
  parent = (StgClosure *)NULL;
  ASSERT(bufsize > 0);
  do {
    /* check that we are not at the end of the buffer, yet */
    IF_DEBUG(sanity, ASSERT(*bufptr != END_OF_BUFFER_MARKER));

    /* This is where we will ultimately save the closure's address */
    slotptr = bufptr;
    locn = slotptr-(packBuffer->buffer); // index of closure in buffer
    ASSERT(locn<=bufsize);
  
    /* First, check whether we have a GA, a PLC, or an OFFSET at hand */
    ga.weight = (rtsWeight) *bufptr++;

    if (ga.weight == 2) {  // unglobalised closure to follow
      // nothing to do; closure starts at *bufptr
    } else if (ga.weight > 0) { // fill in GA
      ga.payload.gc.gtid = (GlobalTaskId) *bufptr++;
      ga.payload.gc.slot = (int) *bufptr++;
    } else
      ga.payload.plc = (StgPtr) *bufptr++;
    
    /* Now unpack the closure body, if there is one */
    if (isFixed(&ga)) {
      /* It's a PLC */
      ASSERT(LOOKS_LIKE_STATIC(ga.payload.plc));
    } else if (isOffset(&ga)) {
      ASSERT(ga.payload.gc.slot<=(int)bufsize);
    } else {
      /* normal closure */
      ASSERT(!RtsFlags.ParFlags.globalising==0 || LOOKS_LIKE_GA(&ga));

      closure_start = (StgClosure*)bufptr;
      ASSERT(LOOKS_LIKE_GHC_INFO((StgPtr)*bufptr));
      ip = get_closure_info((StgClosure *)bufptr, 
			    &size, &ptrs, &nonptrs, &vhs, str);

      /* ToDo: check whether this is really needed */
      if (ip->type == FETCH_ME || ip->type == REMOTE_REF) {
	size = _HS;
	ptrs = nonptrs = vhs = 0;
      }
      /* ToDo: check whether this is really needed */
      if (ip->type == ARR_WORDS) {
	ptrs = vhs = 0;
	nonptrs = arr_words_words(((StgArrWords *)bufptr))+1; // payload+words
	size = arr_words_sizeW((StgArrWords *)bufptr);
	ASSERT(size==_HS+vhs+nonptrs);
      }
      /* special code for printing a PAP in a buffer */
      if (ip->type == PAP || ip->type == AP_UPD) {
        vhs = 3; 
	ptrs = 0;
        nonptrs = (nat)((StgPAP *)bufptr)->payload[0];
	size = _HS+vhs+ptrs+nonptrs;
      }

      /* no checks on contents of closure (pointers aren't packed anyway) */
      ASSERT(_HS+vhs+nonptrs>=MIN_NONUPD_SIZE);
      bufptr += _HS+vhs+nonptrs;

      /* Add to queue for processing */
      QueueClosure((StgClosure *)closure_start);
	
      /* No Common up needed for checking */

      /* No Sort out the global address mapping for checking */

    } /* normal closure case */

    /* Locate next parent pointer */
    pptr++;
    while (pptr + 1 > pptrs) {
      parent = DeQueueClosure();

      if (parent == NULL)
	break;
      else {
	//ASSERT(LOOKS_LIKE_GHC_INFO((StgPtr)*parent));
	(void) get_closure_info(parent, &size, &pptrs, &nonptrs,
					&pvhs, str);
	pptr = 0;
      }
    }
  } while (parent != NULL);
  /* we unpacked exactly as many words as there are in the buffer */
  ASSERT(packBuffer->size == bufptr-(packBuffer->buffer));
  /* check for magic end-of-buffer word */  
  IF_DEBUG(sanity, ASSERT(*bufptr == END_OF_BUFFER_MARKER));
}
#else  /* GRAN */
void
PrintPacket(buffer)
rtsPackBuffer *buffer;
{
    // extern char *info_hdr_type(P_ infoptr);  /* defined in Threads.lc */
    // extern char *display_info_type(P_ infoptr);      /* defined in Threads.lc */

    StgInfoTable *info;
    nat size, ptrs, nonptrs, vhs;
    char info_hdr_ty[80];
    char str1[80], str2[80], junk_str[80];

    /* globalAddr ga; */

    nat bufsize, unpacked_size ;
    StgClosure *parent;
    nat pptr = 0, pptrs = 0, pvhs;

    nat unpack_locn = 0;
    nat gastart = unpack_locn;
    nat closurestart = unpack_locn;

    StgTSO *tso;
    StgClosure *closure, *p;

    nat i;

    fprintf(stderr, "*** Printing <<%d>> (buffer @ %p):\n", buffer->id, buffer);
    fprintf(stderr, "  size: %d; unpacked_size: %d; tso: %d (%p); buffer: %p\n",
	    buffer->size, buffer->unpacked_size, buffer->tso, buffer->buffer);
    fputs("  contents: ", stderr);
    for (unpack_locn=0; unpack_locn<buffer->size; unpack_locn++) {
      closure = buffer->buffer[unpack_locn];
      fprintf(stderr, ", %p (%s)", 
	      closure, info_type(closure)); 
    }
    fputc('\n', stderr);

#if 0
    /* traverse all elements of the graph; omitted for now, but might be usefule */
    InitClosureQueue();

    tso = buffer->tso;

    /* Unpack the header */
    unpacked_size = buffer->unpacked_size;
    bufsize = buffer->size;

    fprintf(stderr, "Packet %p, size %u (unpacked size is %u); demanded by TSO %d (%p)[PE %d]\n--- Begin ---\n", 
	            buffer, bufsize, unpacked_size,  
	            tso->id, tso, where_is((StgClosure*)tso));

    do {
	closurestart = unpack_locn;
	closure = buffer->buffer[unpack_locn++];
	
	fprintf(stderr, "[%u]: (%p) ", closurestart, closure);

	info = get_closure_info(closure, &size, &ptrs, &nonptrs, &vhs, str1);
	strcpy(str2, str1);
	fprintf(stderr, "(%s|%s) ", str1, str2);
	
        if (info->type == FETCH_ME || info->type == FETCH_ME_BQ || 
	    IS_BLACK_HOLE(closure))
	  size = ptrs = nonptrs = vhs = 0;
	
	if (closure_THUNK(closure)) {
		if (closure_UNPOINTED(closure))
		    fputs("UNPOINTED ", stderr);
		else
		    fputs("POINTED ", stderr);
	} 
        if (IS_BLACK_HOLE(closure)) {
		fputs("BLACK HOLE\n", stderr);
	} else {
		/* Fixed header */
		fprintf(stderr, "FH ["); 
		for (i = 0, p = (StgClosure*)&(closure->header); i < _HS; i++, p++)
		    fprintf(stderr, " %p", *p);
	
		/* Variable header 
		if (vhs > 0) {
		    fprintf(stderr, "] VH [%p", closure->payload[_HS]);
	
		    for (i = 1; i < vhs; i++)
			fprintf(stderr, " %p", closure->payload[_HS+i]);
		}
		*/
		fprintf(stderr, "] PTRS %u", ptrs);
	
		/* Non-pointers */
		if (nonptrs > 0) {
		    fprintf(stderr, " NPTRS [%p", closure->payload[_HS+vhs]);
		
		    for (i = 1; i < nonptrs; i++)
			fprintf(stderr, " %p", closure->payload[_HS+vhs+i]);
	
		    putc(']', stderr);
		}
		putc('\n', stderr);
	}
    } while (unpack_locn<bufsize) ;  /* (parent != NULL); */

    fprintf(stderr, "--- End ---\n\n");
#endif /* 0 */
}
#endif /* PAR */
#endif /* DEBUG || GRAN_CHECK */

#endif /* PAR  || GRAN  -- whole file */

//@node End of file,  , Printing Packet Contents, Graph packing
//@subsection End of file

//@index
//* AllocateHeap::  @cindex\s-+AllocateHeap
//* AmPacking::  @cindex\s-+AmPacking
//* CommonUp::  @cindex\s-+CommonUp
//* DeQueueClosure::  @cindex\s-+DeQueueClosure
//* DeQueueClosure::  @cindex\s-+DeQueueClosure
//* DonePacking::  @cindex\s-+DonePacking
//* FillInClosure::  @cindex\s-+FillInClosure
//* IS_BLACK_HOLE::  @cindex\s-+IS_BLACK_HOLE
//* IS_INDIRECTION::  @cindex\s-+IS_INDIRECTION
//* InitClosureQueue::  @cindex\s-+InitClosureQueue
//* InitPendingGABuffer::  @cindex\s-+InitPendingGABuffer
//* LocateNextParent::  @cindex\s-+LocateNextParent
//* NotYetPacking::  @cindex\s-+NotYetPacking
//* OffsetFor::  @cindex\s-+OffsetFor
//* Pack::  @cindex\s-+Pack
//* PackArray::  @cindex\s-+PackArray
//* PackClosure::  @cindex\s-+PackClosure
//* PackFetchMe::  @cindex\s-+PackFetchMe
//* PackGeneric::  @cindex\s-+PackGeneric
//* PackNearbyGraph::  @cindex\s-+PackNearbyGraph
//* PackOneNode::  @cindex\s-+PackOneNode
//* PackPAP::  @cindex\s-+PackPAP
//* PackPLC::  @cindex\s-+PackPLC
//* PackStkO::  @cindex\s-+PackStkO
//* PackTSO::  @cindex\s-+PackTSO
//* PendingGABuffer::  @cindex\s-+PendingGABuffer
//* PrintPacket::  @cindex\s-+PrintPacket
//* QueueClosure::  @cindex\s-+QueueClosure
//* QueueEmpty::  @cindex\s-+QueueEmpty
//* RoomToPack::  @cindex\s-+RoomToPack
//* SetGAandCommonUp::  @cindex\s-+SetGAandCommonUp
//* UnpackGA::  @cindex\s-+UnpackGA
//* UnpackGraph::  @cindex\s-+UnpackGraph
//* UnpackOffset::  @cindex\s-+UnpackOffset
//* UnpackPLC::  @cindex\s-+UnpackPLC
//* doGlobalGC::  @cindex\s-+doGlobalGC
//* get_closure_info::  @cindex\s-+get_closure_info
//* InitPackBuffer::  @cindex\s-+initPackBuffer
//* isFixed::  @cindex\s-+isFixed
//* isOffset::  @cindex\s-+isOffset
//* offsetTable::  @cindex\s-+offsetTable
//@end index

