/* 
   Time-stamp: <Thu Dec 16 1999 18:21:17 Stardate: [-30]4058.61 software>
   $Id: Pack.c,v 1.3 2000/03/17 14:37:22 simonmar Exp $

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

   Note that in GranSim we need many buffers, not just one per PE.  */

//@node Graph packing, , ,
//@section Graph packing

#if defined(PAR) || defined(GRAN)   /* whole file */

#define _HS (sizeofW(StgHeader))

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

static inline void    	  AllocClosureQueue(nat size);
static inline void    	  InitClosureQueue(void);
static inline rtsBool 	  QueueEmpty(void);
static inline void    	  QueueClosure(StgClosure *closure);
static inline StgClosure *DeQueueClosure(void);

//@node Init for packing, Packing routines, ADT of closure queues, Prototypes
//@subsubsection Init for packing

static void     initPacking(void);
# if defined(PAR)
rtsBool         initPackBuffer(void);
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
static inline void    Pack (StgClosure *data);
# else
static inline void    Pack (StgWord data);

static void    PackPLC (StgPtr addr);
static void    PackOffset (int offset);
static void    GlobaliseAndPackGA (StgClosure *closure);
# endif

//@node Unpacking routines, Aux fcts for packing, Low level packing fcts, Prototypes
//@subsubsection Unpacking routines

# if defined(PAR)
void        InitPendingGABuffer(nat size); 
void        CommonUp(StgClosure *src, StgClosure *dst);
StgClosure *UnpackGraph(rtsPackBuffer *packBuffer,
			globalAddr **gamap,
			nat *nGAs);
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
static rtsBool  RoomToPack (nat size, nat ptrs);
       rtsBool  isOffset(globalAddr *ga);
       rtsBool  isFixed(globalAddr *ga);
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
static nat     reservedPAsize;        /* Space reserved for primitive arrays */
static rtsBool RoomInBuffer;

# if defined(GRAN)
/* 
   The pack buffer
   To be pedantic: in GrAnSim we're packing *addresses* of closures,
   not the closures themselves.
*/
static rtsPackBuffer *Bonzo = NULL;                /* size: can be set via option */
# else
static rtsPackBuffer *Bonzo = NULL;                /* size: can be set via option */
# endif

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
static globalAddr *PendingGABuffer;  
/* is initialised in main; */
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

//@node Init routines, Basic routines, Closure Queues, ADT of Closure Queues
//@subsubsection Init routines

/* @InitClosureQueue@ initialises the closure queue. */

//@cindex AllocClosureQueue
static inline void
AllocClosureQueue(size)
nat size;
{
  ASSERT(ClosureQueue == NULL);
  ClosureQueue = (StgClosure**) stgMallocWords(size, "AllocClosureQueue");
}

//@cindex InitClosureQueue
static inline void
InitClosureQueue(void)
{
  clq_pos = clq_size = 0;

  if ( ClosureQueue == NULL ) 
     AllocClosureQueue(RTS_PACK_BUFFER_SIZE);
}

//@node Basic routines,  , Init routines, ADT of Closure Queues
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
  if(clq_size < RTS_PACK_BUFFER_SIZE )
    ClosureQueue[clq_size++] = closure;
  else
    barf("Closure Queue Overflow (EnQueueing %p (%s))", 
	 closure, info_type(closure));
}

/* DeQueueClosure returns the head of the closure queue. */

//@cindex DeQueueClosure
static inline StgClosure * 
DeQueueClosure(void)
{
  if(!QueueEmpty())
    return(ClosureQueue[clq_pos++]);
  else
    return((StgClosure*)NULL);
}

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
  usual.  -- HWL */

# if defined(GRAN)
rtsPackBuffer *
InstantiatePackBuffer (void) {
  extern rtsPackBuffer *Bonzo;

  Bonzo = (rtsPackBuffer *) stgMallocWords(sizeofW(rtsPackBuffer), 
			 "InstantiatePackBuffer: failed to alloc packBuffer");
  Bonzo->size = RtsFlags.GranFlags.packBufferSize_internal;
  Bonzo->buffer = (StgWord **) stgMallocWords(RtsFlags.GranFlags.packBufferSize_internal,
				 "InstantiatePackBuffer: failed to alloc GranSim internal packBuffer");
  /* NB: gransim_pack_buffer_size instead of pack_buffer_size -- HWL */
  /* stgMallocWords is now simple allocate in Storage.c */

  return (Bonzo);
}

/* 
   Reallocate the GranSim internal pack buffer to make room for more closure
   pointers. This is independent of the check for packet overflow as in GUM
*/
static void
reallocPackBuffer (void) {

  ASSERT(pack_locn >= (int)Bonzo->size+sizeofW(rtsPackBuffer));

  IF_GRAN_DEBUG(packBuffer,
		belch("** Increasing size of PackBuffer %p to %d words (PE %u @ %d)\n",
		      Bonzo, Bonzo->size+REALLOC_SZ,
		      CurrentProc, CurrentTime[CurrentProc]));
  
  Bonzo = (rtsPackBuffer*)realloc(Bonzo, 
				  sizeof(StgClosure*)*(REALLOC_SZ +
						       (int)Bonzo->size +
						       sizeofW(rtsPackBuffer))) ;
  if (Bonzo==(rtsPackBuffer*)NULL) 
    barf("Failing to realloc %d more words for PackBuffer %p (PE %u @ %d)\n", 
	 REALLOC_SZ, Bonzo, CurrentProc, CurrentTime[CurrentProc]);
  
  Bonzo->size += REALLOC_SZ;

  ASSERT(pack_locn < Bonzo->size+sizeofW(rtsPackBuffer));
}
# endif

# if defined(PAR)
/* @initPacking@ initialises the packing buffer etc. */
//@cindex initPackBuffer
rtsBool
initPackBuffer(void)
{
  if (Bonzo == NULL) { /* not yet allocated */

      if ((Bonzo = (rtsPackBuffer *) 
	             stgMallocWords(sizeofW(rtsPackBuffer)+RtsFlags.ParFlags.packBufferSize,
					       "initPackBuffer")) == NULL)
	return rtsFalse;
      
      InitPendingGABuffer(RtsFlags.ParFlags.packBufferSize);
      AllocClosureQueue(RtsFlags.ParFlags.packBufferSize);
  }
  return rtsTrue;
}
# endif 

static void
initPacking(void)
{
# if defined(GRAN)
  Bonzo = InstantiatePackBuffer();     /* for GrAnSim only -- HWL */
                                       /* NB: free in UnpackGraph */
# endif

  Bonzo->id = buf_id++;  /* buffer id are only used for debugging! */
  pack_locn = 0;         /* the index into the actual pack buffer */
  unpacked_size = 0;     /* the size of the whole graph when unpacked */
  reservedPAsize = 0;
  RoomInBuffer = rtsTrue;
  InitClosureQueue();
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
PackNearbyGraph(closure, tso, packBufferSize)
StgClosure* closure;
StgTSO* tso;
nat *packBufferSize;
{
  extern rtsPackBuffer *Bonzo;
  /* Ensure enough heap for all possible RBH_Save closures */

  ASSERT(RTS_PACK_BUFFER_SIZE > 0);

  /* ToDo: check that we have enough heap for the packet
     ngoq ngo'
     if (Hp + PACK_HEAP_REQUIRED > HpLim) 
     return NULL;
  */

  initPacking();
# if defined(GRAN)
  graph_root = closure;
# endif

  IF_GRAN_DEBUG(pack,
		belch(">>> Packing <<%d>> (buffer @ %p); graph root @ %p [PE %d]\n    demanded by TSO %d (%p) [PE %u]",
		      Bonzo->id, Bonzo, closure, where_is(closure), 
		      tso->id, tso, where_is((StgClosure*)tso)));

  IF_GRAN_DEBUG(pack,
		belch("** PrintGraph of %p is:", closure); 
		PrintGraph(closure,0));

  IF_PAR_DEBUG(pack,
	       belch(">>> Packing <<%d>> (buffer @ %p); graph root @ %p [%x]\n    demanded by TSO %d (%p)",
		     Bonzo->id, Bonzo, closure, mytid,
		     tso->id, tso)); 

  IF_PAR_DEBUG(pack,
	       belch("** PrintGraph of %p is:", closure); 
	       belch("** pack_locn=%d", pack_locn);
	       PrintGraph(closure,0));

  QueueClosure(closure);
  do {
    PackClosure(DeQueueClosure());
  } while (!QueueEmpty());
  
# if defined(PAR)

  /* Record how much space is needed to unpack the graph */
  Bonzo->tso = tso; // ToDo: check: used in GUM or only for debugging?
  Bonzo->unpacked_size = unpacked_size;
  Bonzo->size = pack_locn;

  /* Set the size parameter */
  ASSERT(pack_locn <= RtsFlags.ParFlags.packBufferSize);
  *packBufferSize = pack_locn;

# else  /* GRAN */

  /* Record how much space is needed to unpack the graph */
  // PackBuffer[PACK_FLAG_LOCN] = (P_) MAGIC_PACK_FLAG;  for testing
  Bonzo->tso = tso;
  Bonzo->unpacked_size = unpacked_size;

  // ASSERT(pack_locn <= PackBuffer[PACK_SIZE_LOCN]+PACK_HDR_SIZE);
  /* ToDo: Print an earlier, more meaningful message */
  if (pack_locn==0)   /* i.e. packet is empty */
    barf("EMPTY PACKET! Can't transfer closure %p at all!!\n",
	 closure);
  Bonzo->size = pack_locn;
  *packBufferSize = pack_locn;

# endif

  DonePacking();                               /* {GrAnSim}vaD 'ut'Ha' */

# if defined(GRAN)
  IF_GRAN_DEBUG(pack ,
		belch("** Finished <<%d>> packing graph %p; closures packed: %d; thunks packed: %d; size of graph: %d",
		      Bonzo->id, closure, Bonzo->size, packed_thunks, Bonzo->unpacked_size));
  if (RtsFlags.GranFlags.GranSimStats.Global) {
    globalGranStats.tot_packets++; 
    globalGranStats.tot_packet_size += pack_locn; 
  }
  
  IF_GRAN_DEBUG(pack, PrintPacket(Bonzo));
# elif defined(PAR)
  IF_GRAN_DEBUG(pack ,
		belch("** Finished <<%d>> packing graph %p; closures packed: %d; thunks packed: %d; size of graph: %d",
		      Bonzo->id, closure, Bonzo->size, packed_thunks, Bonzo->unpacked_size);
		PrintPacket(Bonzo));
# endif   /* GRAN */

  return (Bonzo);
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
  extern rtsPackBuffer *Bonzo;
  int i, clpack_locn;

  initPacking();

  IF_GRAN_DEBUG(pack,
		belch("** PackOneNode: %p (%s)[PE %d] requested by TSO %d (%p) [PE %d]",
		      closure, info_type(closure),
		      where_is(closure), tso->id, tso, where_is((StgClosure *)tso)));

  Pack(closure);

  /* Record how much space is needed to unpack the graph */
  Bonzo->tso = tso;
  Bonzo->unpacked_size = unpacked_size;

  /* Set the size parameter */
  ASSERT(pack_locn <= RTS_PACK_BUFFER_SIZE);
  Bonzo->size =  pack_locn;
  *packBufferSize = pack_locn;

  if (RtsFlags.GranFlags.GranSimStats.Global) {
    globalGranStats.tot_packets++; 
    globalGranStats.tot_packet_size += pack_locn; 
  }
  IF_GRAN_DEBUG(pack,
    PrintPacket(Bonzo));

  return (Bonzo);
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
  extern rtsPackBuffer *Bonzo;
  IF_GRAN_DEBUG(pack,
		belch("** Packing TSO %d (%p)", tso->id, tso));
  *packBufferSize = 0;
  // PackBuffer[0] = PackBuffer[1] = 0; ???
  return(Bonzo);
}

//@cindex PackStkO
rtsPackBuffer*
PackStkO(stko, packBufferSize)
StgPtr stko;
nat *packBufferSize;
{
  extern rtsPackBuffer *Bonzo;
  IF_GRAN_DEBUG(pack,
		belch("** Packing STKO %p", stko));
  *packBufferSize = 0;
  // PackBuffer[0] = PackBuffer[1] = 0;
  return(Bonzo);
}

void
PackFetchMe(StgClosure *closure)
{
  barf("{PackFetchMe}Daq Qagh: no FetchMe closures in GRAN!");
}

#elif defined(PAR)

rtsPackBuffer*
PackTSO(tso, packBufferSize)
StgTSO *tso;
nat *packBufferSize;
{
  barf("{PackTSO}Daq Qagh: trying to pack a TSO; thread migrations not supported, yet");
}

rtsPackBuffer*
PackStkO(stko, packBufferSize)
StgPtr stko;
nat *packBufferSize;
{
  barf("{PackStkO}Daq Qagh: trying to pack a STKO; thread migrations not supported, yet");
}

//@cindex PackFetchMe
void
PackFetchMe(StgClosure *closure)
{
  StgInfoTable *ip;
  nat i;

#if defined(GRAN)
  barf("{PackFetchMe}Daq Qagh: no FetchMe closures in GRAN!");
#else
  /* Pack a FetchMe closure instead of closure */
  ip = &FETCH_ME_info;
  /* this assumes that the info ptr is always the first word in a closure*/
  Pack((StgWord)ip);
  for (i = 1; i < _HS; ++i)               // pack rest of fixed header
    Pack((StgWord)*(((StgPtr)closure)+i));
  
  unpacked_size += _HS; // ToDo: check
#endif
}

#endif

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
  StgClosure *indirectee, *rbh;
  nat size, ptrs, nonptrs, vhs, i, clpack_locn;
  rtsBool is_CONSTR = rtsFalse;
  char str[80];

  ASSERT(closure!=NULL);
  indirectee = closure;
  do {
    /* Don't pack indirection closures */
    closure =  indirectee;
    indirectee = IS_INDIRECTION(closure);
    IF_PAR_DEBUG(pack,
		 if (indirectee) 
		   belch("** Shorted an indirection (%s) at %p (-> %p)", 
			 info_type(closure), closure, indirectee));
  } while (indirectee);

  clpack_locn = OffsetFor(closure);

  /* If the closure has been packed already, just pack an indirection to it
     to guarantee that the graph doesn't become a tree when unpacked */
  if (!NotYetPacking(clpack_locn)) {
    StgInfoTable *info;

    PackOffset(clpack_locn);
    return;
  }

  /*
   * PLCs reside on all of the PEs already. Just pack the
   * address as a GA (a bit of a kludge, since an address may
   * not fit in *any* of the individual GA fields). Const,
   * charlike and small intlike closures are converted into
   * PLCs.
   */
  switch (get_itbl(closure)->type) {

#  ifdef DEBUG
    // check error cases only in a debugging setup
  case RET_BCO:
  case RET_SMALL:
  case RET_VEC_SMALL:
  case RET_BIG:
  case RET_VEC_BIG:
  case RET_DYN:
    barf("** {Pack}Daq Qagh: found return vector %p (%s) when packing (thread migration not implemented)", 
	 closure, info_type(closure));
    /* never reached */
    
  case UPDATE_FRAME:
  case STOP_FRAME:
  case CATCH_FRAME:
  case SEQ_FRAME:
    barf("** {Pack}Daq Qagh: found stack frame %p (%s) when packing (thread migration not implemented)", 
	 closure, info_type(closure));
    /* never reached */

  case TSO:
  case BLOCKED_FETCH:
  case EVACUATED:
    /* something's very wrong */
    barf("** {Pack}Daq Qagh: found %s (%p) when packing", 
	 info_type(closure), closure);
    /* never reached */
#  endif

  case CONSTR_CHARLIKE:
    IF_PAR_DEBUG(pack,
		 belch("** Packing a charlike closure %d", 
		       ((StgIntCharlikeClosure*)closure)->data));
    
    PackPLC(CHARLIKE_CLOSURE(((StgIntCharlikeClosure*)closure)->data));
    return;
      
  case CONSTR_INTLIKE:
    {
      StgInt val = ((StgIntCharlikeClosure*)closure)->data;
      
      if ((val <= MAX_INTLIKE) && (val >= MIN_INTLIKE)) {
	IF_PAR_DEBUG(pack,
		     belch("** Packing a small intlike %d as a PLC", val));
	PackPLC(INTLIKE_CLOSURE(val));
	return;
      } else {
	IF_PAR_DEBUG(pack,
		     belch("** Packing a big intlike %d as a normal closure", 
			   val));
	break;
      }
    }

  case CONSTR:
  case CONSTR_1_0:
  case CONSTR_0_1:
  case CONSTR_2_0:
  case CONSTR_1_1:
  case CONSTR_0_2:
    /* it's a constructor (i.e. plain data) but we don't know 
       how many ptrs, non-ptrs there are => use generic code */
    IF_PAR_DEBUG(pack,
		 belch("** Packing a CONSTR %p (%s) using generic packing with GA", 
		       closure, info_type(closure)));
    // is_CONSTR = rtsTrue;
    break;
    /* fall through to generic packing code */

  case CONSTR_STATIC:
  case CONSTR_NOCAF_STATIC:// For now we ship indirections to CAFs: They are
			   // evaluated on each PE if needed
    IF_PAR_DEBUG(pack,
      belch("** Packing a %p (%s) as a PLC", 
	    closure, info_type(closure)));

    PackPLC(closure);
    return;

  case MVAR:
    /* MVARs may not be copied; they are sticky objects in the new RTS */
    /* therefore we treat them just as RBHs etc (what a great system!) */
    IF_PAR_DEBUG(pack,
		 belch("** Found an MVar at %p (%s)", 
		       closure, info_type(closure)));
    /* fall through !! */

  case THUNK_SELECTOR: // ToDo: fix packing of this strange beast
    IF_PAR_DEBUG(pack,
		 belch("** Found an THUNK_SELECTORE at %p (%s)", 
		       closure, info_type(closure)));
    /* fall through !! */

  case CAF_BLACKHOLE:
  case SE_CAF_BLACKHOLE:
  case SE_BLACKHOLE:
  case BLACKHOLE:
  case RBH:
  case FETCH_ME:
  case FETCH_ME_BQ:

    /* If it's a (revertible) black-hole, pack a FetchMe closure to it */
    //ASSERT(pack_locn > PACK_HDR_SIZE);
    
    IF_PAR_DEBUG(pack,
		 belch("** Packing a BH or FM at %p (%s) of (fixed size %d)", 
		       closure, info_type(closure), _HS));

    /* Need a GA even when packing a constructed FETCH_ME (cruel world!) */
    GlobaliseAndPackGA(closure);

    PackFetchMe(closure);
    return;

  default:
/*      IF_PAR_DEBUG(pack, */
/*  		 belch("** Not a PLC or BH ... ")); */
  } /* switch */

  /* get info about basic layout of the closure */
  info = get_closure_info(closure, &size, &ptrs, &nonptrs, &vhs, str);

  ASSERT(!IS_BLACK_HOLE(closure));

  IF_PAR_DEBUG(pack,
	       fprintf(stderr, "** packing %p (%s) (size=%d, ptrs=%d, nonptrs=%d)\n",
		       closure, info_type(closure), size, ptrs, nonptrs));

  /*
   * Now peek ahead to see whether the closure has any primitive array
   * children
   */
  /*
      ToDo: fix this code -- HWL
    for (i = 0; i < ptrs; ++i) {
      StgInfoTable * childInfo;
      nat childSize, childPtrs, childNonPtrs, childVhs;
      
      // extract i-th pointer out of closure 
      childInfo = get_closure_info(((PP_) (closure))[i + FIXED_HS + vhs],
				   &childSize, &childPtrs, &childNonPtrs, &childVhs, str);
      if (IS_BIG_MOTHER(childInfo)) {
	reservedPAsize += PACK_GA_SIZE + FIXED_HS + childVhs + childNonPtrs
	  + childPtrs * PACK_FETCHME_SIZE;
      }
    }
    */
  /* Record the location of the GA */
  AmPacking(closure);

  /* Pack the global address */
  if (!is_CONSTR) {
    GlobaliseAndPackGA(closure);
  } else {
    IF_PAR_DEBUG(pack,
		 belch("** No GA allocated for CONSTR %p (%s)",
		       closure, info_type(closure)));
  }

  /*
   * Pack a fetchme to the closure if it's a black hole, or the buffer is full
   * and it isn't a primitive array. N.B. Primitive arrays are always packed
   * (because their parents index into them directly)
   */

  // ToDo: pack FMs if no more room available in packet (see below)
  if (!(RoomToPack(PACK_GA_SIZE + _HS + vhs + nonptrs, ptrs)))
    barf("** Qagh: Pack: not enough room in packet to pack closure %p (%s)",
	 closure, info_type(closure));

  /*
    Has been moved into the switch statement
    
    if (IS_BLACK_HOLE(closure)) 
    !(RoomToPack(PACK_GA_SIZE + FIXED_HS + vhs + nonptrs, ptrs)
    || IS_BIG_MOTHER(info))) 
    {
      
      ASSERT(pack_locn > PACK_HDR_SIZE);
      
      info = FetchMe_info;
      for (i = 0; i < FIXED_HS; ++i) {
	if (i == INFO_HDR_POSN)
	  Pack((StgWord) FetchMe_info);
	else
	  Pack(closure[i]);
      }

      unpacked_size += FIXED_HS + FETCHME_CLOSURE_SIZE(dummy);

    } else {
  */
  if (info->type == ARR_WORDS || info->type == MUT_ARR_PTRS ||
      info->type == MUT_ARR_PTRS_FROZEN || info->type == MUT_VAR)
    belch("** ghuH: found %s; packing of primitive arrays not yet implemented",
	  info_type(closure));

  /* At last! A closure we can actually pack! */
  if (ip_MUTABLE(info) && (info->type != FETCH_ME))
    fprintf(stderr, "** ghuH: Replicated a Mutable closure!\n");
      
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
    QueueClosure(((StgClosure *) *(((StgPtr)closure)+(i+_HS+vhs))));

  /* pack non-ptrs */
  for (i = 0; i < nonptrs; ++i)
    Pack((StgWord)*(((StgPtr)closure)+(i+_HS+vhs+ptrs)));
      
  unpacked_size += _HS + (size < MIN_UPD_SIZE ? MIN_UPD_SIZE : size);

  /*
   * Record that this is a revertable black hole so that we can fill in
   * its address from the fetch reply.  Problem: unshared thunks may cause
   * space leaks this way, their GAs should be deallocated following an
   * ACK.
   */
      
  // IS_UPDATABLE(closure) == !closure_UNPOINTED(closure) !? HWL
  if (closure_THUNK(closure) && !closure_UNPOINTED(closure)) { 
    rbh = convertToRBH(closure);
    ASSERT(rbh == closure); // rbh at the same position (minced version)
    packed_thunks++;
  }
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
	StgClosure *sel = ((StgSelector *)closure)->selectee;

	IF_GRAN_DEBUG(pack,
		      belch("**    Avoid packing THUNK_SELECTOR (%p, %s) but queuing %p (%s)!", 
			    closure, info_type(closure), sel, info_type(sel)));
	QueueClosure(sel);
	IF_GRAN_DEBUG(pack,
		      belch("**    [%p (%s) (Queueing closure) ....]",
			    sel, info_type(sel)));
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

    case CAF_UNENTERED:    /* # of ptrs, nptrs: 1,3 */
    case CAF_ENTERED:      /* # of ptrs, nptrs: 0,4  (allegedly bogus!!) */
      /* CAFs; special treatment necessary? */
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
       
       childInfo = get_closure_info(((StgPtrPtr) (closure))[i + FIXED_HS + vhs],
       &childSize, &childPtrs, &childNonPtrs,
       &childVhs, junk_str);
       if (IS_BIG_MOTHER(childInfo)) {
       reservedPAsize += PACK_GA_SIZE + FIXED_HS + 
       childVhs + childNonPtrs +
       childPtrs * PACK_FETCHME_SIZE;
       PAsize += PACK_GA_SIZE + FIXED_HS + childSize;
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
	  !(RoomToPack(PACK_GA_SIZE + FIXED_HS + vhs + nonptrs, ptrs) 
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
			  closure->payload[i], info_type(payloadPtr(closure,i))));
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
static inline void
Pack(data)
StgWord data;
{
  ASSERT(pack_locn < RtsFlags.ParFlags.packBufferSize);
  Bonzo->buffer[pack_locn++] = data;
}
#endif

#if defined(GRAN)
static inline void
Pack(closure)
StgClosure *closure;
{
  StgInfoTable *info;
  nat size, ptrs, nonptrs, vhs;
  char str[80];

  /* This checks the size of the GrAnSim internal pack buffer. The simulated
     pack buffer is checked via RoomToPack (as in GUM) */
  if (pack_locn >= (int)Bonzo->size+sizeofW(rtsPackBuffer)) 
    reallocPackBuffer();

  if (closure==(StgClosure*)NULL) 
    belch("Qagh {Pack}Daq: Trying to pack 0");
  Bonzo->buffer[pack_locn++] = closure;
  /* ASSERT: Data is a closure in GrAnSim here */
  info = get_closure_info(closure, &size, &ptrs, &nonptrs, &vhs, str);
  unpacked_size += _HS + (size < MIN_UPD_SIZE ? 
				        MIN_UPD_SIZE : 
				        size);
}
# endif  /* GRAN */

/*
   If a closure is local, make it global.  Then, divide its weight for
   export.  The GA is then packed into the pack buffer.  */

# if defined(PAR)

static void
GlobaliseAndPackGA(closure)
StgClosure *closure;
{
  globalAddr *ga;
  globalAddr packGA;

  if ((ga = LAGAlookup(closure)) == NULL)
    ga = makeGlobal(closure, rtsTrue);
  splitWeight(&packGA, ga);
  ASSERT(packGA.weight > 0);

  IF_PAR_DEBUG(pack,
	       fprintf(stderr, "** Globalising closure %p (%s) with GA", 
		       closure, info_type(closure));
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
  IF_PAR_DEBUG(pack,
	       belch("** Packing Offset %d at pack location %u",
		     offset, pack_locn));
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

//@node GUM code, Local Definitions, Unpacking routines, Unpacking routines
//@subsubsection GUM code

#if defined(PAR) 

//@cindex InitPendingGABuffer
void
InitPendingGABuffer(size)
nat size; 
{
  PendingGABuffer = (globalAddr *) 
                      stgMallocBytes(size*2*sizeof(globalAddr),
				     "InitPendingGABuffer");
}

/*
  @CommonUp@ commons up two closures which we have discovered to be
  variants of the same object.  One is made an indirection to the other.  */

//@cindex CommonUp
void
CommonUp(StgClosure *src, StgClosure *dst)
{
  StgBlockingQueueElement *bqe;

  ASSERT(src != (StgClosure *)NULL && dst != (StgClosure *)NULL);
  ASSERT(src != dst);

  IF_PAR_DEBUG(verbose,
	       belch("__ CommonUp %p (%s) with %p (%s)",
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

  default:
    /* Don't common up anything else */
    return;
  }
  /* NB: this also awakens the blocking queue for src */
  UPD_IND(src, dst);
  // updateWithIndirection(src, dst);
  /*
    ASSERT(!IS_BIG_MOTHER(INFO_PTR(dst)));
    if (bqe != END_BQ_QUEUE)
    awaken_blocked_queue(bqe, src);
  */
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
  nat size, ptrs, nonptrs, vhs;
  StgWord **buffer, **bufptr, **slotptr;
  globalAddr ga, *gaga;
  StgClosure *closure, *existing,
             *graphroot, *graph, *parent;
  StgInfoTable *ip, *oldip;
  nat bufsize, i,
      pptr = 0, pptrs = 0, pvhs;
  rtsBool hasGA;
  char str[80];

  initPackBuffer();                  /* in case it isn't already init'd */
  graphroot = (StgClosure *)NULL;

  gaga = PendingGABuffer;

  InitClosureQueue();

  /* Unpack the header */
  bufsize = packBuffer->size;
  buffer = packBuffer->buffer;
  bufptr = buffer;

  /* allocate heap */
  if (bufsize > 0) {
    graph = allocate(bufsize);
    ASSERT(graph != NULL);
  }

  parent = (StgClosure *)NULL;

  do {
    /* This is where we will ultimately save the closure's address */
    slotptr = bufptr;

    /* First, unpack the next GA or PLC */
    ga.weight = (rtsWeight) *bufptr++;

    if (ga.weight > 0) {
      ga.payload.gc.gtid = (GlobalTaskId) *bufptr++;
      ga.payload.gc.slot = (int) *bufptr++;
    } else {
      ga.payload.plc = (StgPtr) *bufptr++;
    }

    /* Now unpack the closure body, if there is one */
    if (isFixed(&ga)) {
      /* No more to unpack; just set closure to local address */
      IF_PAR_DEBUG(pack,
		   belch("_* Unpacked PLC at %x", ga.payload.plc)); 
      hasGA = rtsFalse;
      closure = ga.payload.plc;
    } else if (isOffset(&ga)) {
      /* No more to unpack; just set closure to cached address */
      IF_PAR_DEBUG(pack,
		   belch("_* Unpacked indirection to %p (was offset %x)", 
			 (StgClosure *) buffer[ga.payload.gc.slot],
			 ga.payload.gc.slot)); 
      ASSERT(parent != (StgClosure *)NULL);
      hasGA = rtsFalse;
      closure = (StgClosure *) buffer[ga.payload.gc.slot];
    } else {
      /* Now we have to build something. */
      hasGA = rtsTrue;

      ASSERT(bufsize > 0);

      /*
       * Close your eyes.  You don't want to see where we're looking. You
       * can't get closure info until you've unpacked the variable header,
       * but you don't know how big it is until you've got closure info.
       * So...we trust that the closure in the buffer is organized the
       * same way as they will be in the heap...at least up through the
       * end of the variable header.
       */
      ip = get_closure_info(bufptr, &size, &ptrs, &nonptrs, &vhs, str);
	  
      /* 
	 Remember, the generic closure layout is as follows:
	 +-------------------------------------------------+
	 | FIXED HEADER | VARIABLE HEADER | PTRS | NON-PRS |
	 +-------------------------------------------------+
      */
      /* Fill in the fixed header */
      for (i = 0; i < _HS; i++)
	((StgPtr)graph)[i] = (StgWord)*bufptr++;

      if (ip->type == FETCH_ME)
	size = ptrs = nonptrs = vhs = 0;

      /* Fill in the packed variable header */
      for (i = 0; i < vhs; i++)
	((StgPtr)graph)[_HS + i] = (StgWord)*bufptr++;

      /* Pointers will be filled in later */

      /* Fill in the packed non-pointers */
      for (i = 0; i < nonptrs; i++)
	((StgPtr)graph)[_HS + i + vhs + ptrs] = (StgWord)*bufptr++;
                
      /* Indirections are never packed */
      // ASSERT(INFO_PTR(graph) != (W_) Ind_info_TO_USE);

      /* Add to queue for processing */
      QueueClosure(graph);
	
      /*
       * Common up the new closure with any existing closure having the same
       * GA
       */

      if ((existing = GALAlookup(&ga)) == NULL) {
	globalAddr *newGA;
	/* Just keep the new object */
	IF_PAR_DEBUG(pack,
		     belch("_* Unpacking new GA ((%x, %d, %x))", 
			   ga.payload.gc.gtid, ga.payload.gc.slot, ga.weight));

	closure = graph;
	newGA = setRemoteGA(graph, &ga, rtsTrue);
	if (ip->type == FETCH_ME)
	  // FETCHME_GA(closure) = newGA;
	  ((StgFetchMe *)closure)->ga = newGA;
      } else {
	/* Two closures, one global name.  Someone loses */
	oldip = get_itbl(existing);

	if ((oldip->type == FETCH_ME || 
	     // ToDo: don't pack a GA for these in the first place
             oldip->type == CONSTR ||
             oldip->type == CONSTR_1_0 ||
             oldip->type == CONSTR_0_1 ||
             oldip->type == CONSTR_2_0 ||
             oldip->type == CONSTR_1_1 ||
             oldip->type == CONSTR_0_2 ||
	     IS_BLACK_HOLE(existing)) &&
	    ip->type != FETCH_ME) {

	  /* What we had wasn't worth keeping */
	  closure = graph;
	  CommonUp(existing, graph);
	} else {
	  StgWord ty;

	  /*
	   * Either we already had something worthwhile by this name or
	   * the new thing is just another FetchMe.  However, the thing we
	   * just unpacked has to be left as-is, or the child unpacking
	   * code will fail.  Remember that the way pointer words are
	   * filled in depends on the info pointers of the parents being
	   * the same as when they were packed.
	   */
	  IF_PAR_DEBUG(pack,
		       belch("_* Unpacking old GA ((%x, %d, %x)), keeping %#lx", 
			     ga.payload.gc.gtid, ga.payload.gc.slot, ga.weight,
			     existing));

	  closure = existing;
	  // HACK
	  ty = get_itbl(closure)->type;
	  if (ty == CONSTR ||
	      ty == CONSTR_1_0 ||
	      ty == CONSTR_0_1 ||
	      ty == CONSTR_2_0 ||
	      ty == CONSTR_1_1 ||
	      ty == CONSTR_0_2)
	    CommonUp(closure, graph);
	  
	}
	/* Pool the total weight in the stored ga */
	(void) addWeight(&ga);
      }

      /* Sort out the global address mapping */
      if (hasGA || // (ip_THUNK(ip) && !ip_UNPOINTED(ip)) || 
	  (ip_MUTABLE(ip) && ip->type != FETCH_ME)) {
	/* Make up new GAs for single-copy closures */
	globalAddr *newGA = makeGlobal(closure, rtsTrue);
	
	// keep this assertion!
	// ASSERT(closure == graph);

	/* Create an old GA to new GA mapping */
	*gaga++ = ga;
	splitWeight(gaga, newGA);
	ASSERT(gaga->weight == 1L << (BITS_IN(unsigned) - 1));
	gaga++;
      }
      graph += _HS + (size < MIN_UPD_SIZE ? MIN_UPD_SIZE : size);
    }

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
    *slotptr = (StgWord) closure;

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

  //ASSERT(bufsize == 0 || graph - 1 <= SAVE_Hp);

  *gamap = PendingGABuffer;
  *nGAs = (gaga - PendingGABuffer) / 2;

  /* ToDo: are we *certain* graphroot has been set??? WDP 95/07 */
  ASSERT(graphroot!=NULL);
  return (graphroot);
}
#endif  /* PAR */

//@node GranSim Code,  , Local Definitions, Unpacking routines
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
/*    IF_PAR_DEBUG(pack, */
/*  	       fprintf(stderr, "** AmPacking %p (%s)(IP %p) at %u\n",  */
/*  		       closure, info_type(closure), get_itbl(closure), pack_locn)); */

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
    found = Bonzo->buffer[i]==closure;

  return (!found);
}
# endif

//@node Packet size, Types of Global Addresses, Offset table, Aux fcts for packing
//@subsubsection Packet size

/*
  RoomToPack determines whether there's room to pack the closure into
  the pack buffer based on 

  o how full the buffer is already,
  o the closures' size and number of pointers (which must be packed as GAs),
  o the size and number of pointers held by any primitive arrays that it 
    points to
  
    It has a *side-effect* (naughty, naughty) in assigning RoomInBuffer 
    to rtsFalse.
*/

//@cindex RoomToPack
static rtsBool
RoomToPack(size, ptrs)
nat size, ptrs;
{
# if defined(PAR)
  if (RoomInBuffer &&
      (pack_locn + reservedPAsize + size +
       ((clq_size - clq_pos) + ptrs) * PACK_FETCHME_SIZE >= RTS_PACK_BUFFER_SIZE))
    {
      IF_PAR_DEBUG(pack,
		   fprintf(stderr, "Buffer full\n"));

      RoomInBuffer = rtsFalse;
    }
# else   /* GRAN */
  if (RoomInBuffer &&
      (unpacked_size + reservedPAsize + size +
       ((clq_size - clq_pos) + ptrs) * PACK_FETCHME_SIZE >= RTS_PACK_BUFFER_SIZE))
    {
      IF_GRAN_DEBUG(packBuffer,
		    fprintf(stderr, "Buffer full\n"));
      RoomInBuffer = rtsFalse;
    }
# endif
  return (RoomInBuffer);
}

//@node Types of Global Addresses, Closure Info, Packet size, Aux fcts for packing
//@subsubsection Types of Global Addresses

/*
  Types of Global Addresses

  These routines determine whether a GA is one of a number of special types
  of GA.
*/

# if defined(PAR)
//@cindex isOffset
rtsBool
isOffset(ga)
globalAddr *ga;
{
    return (ga->weight == 1 && ga->payload.gc.gtid == 0);
}

//@cindex isFixed
rtsBool
isFixed(ga)
globalAddr *ga;
{
    return (ga->weight == 0);
}
# endif

//@node Closure Info,  , Types of Global Addresses, Aux fcts for packing
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
  StgInfoTable *ip, *oldip;
  globalAddr ga;
  StgWord **buffer, **bufptr, **slotptr;

  nat bufsize;
  nat pptr = 0, pptrs = 0, pvhs;
  nat unpack_locn = 0;
  nat gastart = unpack_locn;
  nat closurestart = unpack_locn;
  nat i;
  nat size, ptrs, nonptrs, vhs;
  char str[80];

  /* NB: this whole routine is more or less a copy of UnpackGraph with all
     unpacking components replaced by printing fcts
     Long live higher-order fcts!
  */
  initPackBuffer();                  /* in case it isn't already init'd */
  graphroot = (StgClosure *)NULL;

  // gaga = PendingGABuffer;

  InitClosureQueue();

  /* Unpack the header */
  bufsize = packBuffer->size;
  buffer = packBuffer->buffer;
  bufptr = buffer;

  /* allocate heap 
  if (bufsize > 0) {
    graph = allocate(bufsize);
    ASSERT(graph != NULL);
  }
  */

  fprintf(stderr, ".* Printing <<%d>> (buffer @ %p):\n", 
	  packBuffer->id, packBuffer);
  fprintf(stderr, ".*   size: %d; unpacked_size: %d; tso: %p; buffer: %p\n",
	  packBuffer->size, packBuffer->unpacked_size, 
	  packBuffer->tso, packBuffer->buffer);

  parent = (StgClosure *)NULL;

  do {
    /* This is where we will ultimately save the closure's address */
    slotptr = bufptr;

    /* First, unpack the next GA or PLC */
    ga.weight = (rtsWeight) *bufptr++;

    if (ga.weight > 0) {
      ga.payload.gc.gtid = (GlobalTaskId) *bufptr++;
      ga.payload.gc.slot = (int) *bufptr++;
    } else
      ga.payload.plc = (StgPtr) *bufptr++;
    
    /* Now unpack the closure body, if there is one */
    if (isFixed(&ga)) {
      fprintf(stderr, ".* [%u]: PLC @ %p\n", gastart, ga.payload.plc);
      // closure = ga.payload.plc;
    } else if (isOffset(&ga)) {
      fprintf(stderr, ".* [%u]: OFFSET TO [%d]\n", gastart, ga.payload.gc.slot);
      // closure = (StgClosure *) buffer[ga.payload.gc.slot];
    } else {
      /* Print normal closures */

      ASSERT(bufsize > 0);

      fprintf(stderr, ".* [%u]: ((%x, %d, %x)) ", gastart, 
              ga.payload.gc.gtid, ga.payload.gc.slot, ga.weight);

      closure_start = bufptr;
      ip = get_closure_info(bufptr, &size, &ptrs, &nonptrs, &vhs, str);
	  
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

      if (ip->type == FETCH_ME)
	size = ptrs = nonptrs = vhs = 0;

      /* Print variable header */
      fprintf(stderr, "] VH ["); 
      for (i = 0; i < vhs; i++)
	fprintf(stderr, " %p", *bufptr++);

      fprintf(stderr, "] %d PTRS [", ptrs); 

      /* Pointers will be filled in later */

      fprintf(stderr, " ] %d NON-PTRS [", nonptrs); 
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
  fprintf(stderr, ".* --- End packet <<%d>> ---\n", packBuffer->id);
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
//* AllocClosureQueue::  @cindex\s-+AllocClosureQueue
//* AllocateHeap::  @cindex\s-+AllocateHeap
//* AmPacking::  @cindex\s-+AmPacking
//* CommonUp::  @cindex\s-+CommonUp
//* DeQueueClosure::  @cindex\s-+DeQueueClosure
//* DonePacking::  @cindex\s-+DonePacking
//* IS_BLACK_HOLE::  @cindex\s-+IS_BLACK_HOLE
//* IS_INDIRECTION::  @cindex\s-+IS_INDIRECTION
//* InitClosureQueue::  @cindex\s-+InitClosureQueue
//* InitPendingGABuffer::  @cindex\s-+InitPendingGABuffer
//* NotYetPacking::  @cindex\s-+NotYetPacking
//* OffsetFor::  @cindex\s-+OffsetFor
//* Pack::  @cindex\s-+Pack
//* PackClosure::  @cindex\s-+PackClosure
//* PackNearbyGraph::  @cindex\s-+PackNearbyGraph
//* PackOneNode::  @cindex\s-+PackOneNode
//* PackPLC::  @cindex\s-+PackPLC
//* PackStkO::  @cindex\s-+PackStkO
//* PackTSO::  @cindex\s-+PackTSO
//* PendingGABuffer::  @cindex\s-+PendingGABuffer
//* PrintPacket::  @cindex\s-+PrintPacket
//* QueueClosure::  @cindex\s-+QueueClosure
//* QueueEmpty::  @cindex\s-+QueueEmpty
//* RoomToPack::  @cindex\s-+RoomToPack
//* UnpackGraph::  @cindex\s-+UnpackGraph
//* doGlobalGC::  @cindex\s-+doGlobalGC
//* get_closure_info::  @cindex\s-+get_closure_info
//* get_closure_info::  @cindex\s-+get_closure_info
//* initPackBuffer::  @cindex\s-+initPackBuffer
//* isFixed::  @cindex\s-+isFixed
//* isOffset::  @cindex\s-+isOffset
//* offsetTable::  @cindex\s-+offsetTable
//@end index
