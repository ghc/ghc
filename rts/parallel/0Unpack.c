/*
  Time-stamp: <Wed Jan 12 2000 13:29:08 Stardate: [-30]4193.85 hwloidl>

  Unpacking closures which have been exported to remote processors

  This module defines routines for unpacking closures in the parallel
  runtime system (GUM).

  In the case of GrAnSim, this module defines routines for *simulating* the
  unpacking of closures as it is done in the parallel runtime system.
*/

/* 
   Code in this file has been merged with Pack.c 
*/

#if 0

//@node Unpacking closures, , ,
//@section Unpacking closures

//@menu
//* Includes::			
//* Prototypes::		
//* GUM code::			
//* GranSim Code::		
//* Index::			
//@end menu
//*/

//@node Includes, Prototypes, Unpacking closures, Unpacking closures
//@subsection Includes

#include "Rts.h"
#include "RtsFlags.h"
#include "GranSimRts.h"
#include "ParallelRts.h"
#include "ParallelDebug.h"
#include "FetchMe.h"
#include "Storage.h"

//@node Prototypes, GUM code, Includes, Unpacking closures
//@subsection Prototypes

void     InitPacking(void);
# if defined(PAR)
void            InitPackBuffer(void);
# endif
/* Interface for ADT of closure queues */
void    	  AllocClosureQueue(nat size);
void    	  InitClosureQueue(void);
rtsBool 	  QueueEmpty(void);
void    	  QueueClosure(StgClosure *closure);
StgClosure *DeQueueClosure(void);

StgPtr AllocateHeap(nat size);

//@node GUM code, GranSim Code, Prototypes, Unpacking closures
//@subsection GUM code

#if defined(PAR) 

//@node Local Definitions,  , GUM code, GUM code
//@subsubsection Local Definitions

//@cindex PendingGABuffer
static globalAddr *PendingGABuffer;  
/* is initialised in main; */

//@cindex InitPendingGABuffer
void
InitPendingGABuffer(size)
nat size; 
{
  PendingGABuffer = (globalAddr *) 
                      stgMallocBytes((size-PACK_HDR_SIZE)*2*sizeof(globalAddr),
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

  ASSERT(src != dst);
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
  /* We do not use UPD_IND because that would awaken the bq, too */
  // UPD_IND(src, dst);
  updateWithIndirection(get_itbl(src), src, dst);
  //ASSERT(!IS_BIG_MOTHER(INFO_PTR(dst)));
  if (bqe != END_BQ_QUEUE)
    awaken_blocked_queue(bqe, src);
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
  char str[80];

  InitPackBuffer();                  /* in case it isn't already init'd */
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
    } else
      ga.payload.plc = (StgPtr) *bufptr++;
    
    /* Now unpack the closure body, if there is one */
    if (isFixed(&ga)) {
      /* No more to unpack; just set closure to local address */
      IF_PAR_DEBUG(pack,
		   belch("Unpacked PLC at %x", ga.payload.plc)); 
      closure = ga.payload.plc;
    } else if (isOffset(&ga)) {
      /* No more to unpack; just set closure to cached address */
      ASSERT(parent != (StgClosure *)NULL);
      closure = (StgClosure *) buffer[ga.payload.gc.slot];
    } else {
      /* Now we have to build something. */

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
      for (i = 0; i < FIXED_HS; i++)
	((StgPtr)graph)[i] = *bufptr++;

      if (ip->type == FETCH_ME)
	size = ptrs = nonptrs = vhs = 0;

      /* Fill in the packed variable header */
      for (i = 0; i < vhs; i++)
	((StgPtr)graph)[FIXED_HS + i] = *bufptr++;

      /* Pointers will be filled in later */

      /* Fill in the packed non-pointers */
      for (i = 0; i < nonptrs; i++)
	((StgPtr)graph)[FIXED_HS + i + vhs + ptrs] = *bufptr++;
                
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
		     belch("Unpacking new (%x, %d, %x)\n", 
			   ga.payload.gc.gtid, ga.payload.gc.slot, ga.weight));

	closure = graph;
	newGA = setRemoteGA(graph, &ga, rtsTrue);
	if (ip->type == FETCH_ME)
	  // FETCHME_GA(closure) = newGA;
	  ((StgFetchMe *)closure)->ga = newGA;
      } else {
	/* Two closures, one global name.  Someone loses */
	oldip = get_itbl(existing);

	if ((oldip->type == FETCH_ME || IS_BLACK_HOLE(existing)) &&
	    ip->type != FETCH_ME) {

	  /* What we had wasn't worth keeping */
	  closure = graph;
	  CommonUp(existing, graph);
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
		       belch("Unpacking old (%x, %d, %x), keeping %#lx", 
			     ga.payload.gc.gtid, ga.payload.gc.slot, ga.weight,
			     existing));

	  closure = existing;
	}
	/* Pool the total weight in the stored ga */
	(void) addWeight(&ga);
      }

      /* Sort out the global address mapping */
      if ((ip_THUNK(ip) && !ip_UNPOINTED(ip)) || 
	  (ip_MUTABLE(ip) && ip->type != FETCH_ME)) {
	/* Make up new GAs for single-copy closures */
	globalAddr *newGA = makeGlobal(closure, rtsTrue);
	
	ASSERT(closure == graph);

	/* Create an old GA to new GA mapping */
	*gaga++ = ga;
	splitWeight(gaga, newGA);
	ASSERT(gaga->weight == 1L << (BITS_IN(unsigned) - 1));
	gaga++;
      }
      graph += FIXED_HS + (size < MIN_UPD_SIZE ? MIN_UPD_SIZE : size);
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
      ((StgPtr)parent)[FIXED_HS + pvhs + pptr] = (StgWord) closure;

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

  ASSERT(bufsize == 0 || graph - 1 <= SAVE_Hp);

  *gamap = PendingGABuffer;
  *nGAs = (gaga - PendingGABuffer) / 2;

  /* ToDo: are we *certain* graphroot has been set??? WDP 95/07 */
  ASSERT(graphroot!=NULL);
  return (graphroot);
}
#endif  /* PAR */

//@node GranSim Code, Index, GUM code, Unpacking closures
//@subsection GranSim Code

/*
   For GrAnSim: In general no actual unpacking should be necessary. We just
   have to walk over the graph and set the bitmasks appropriately. -- HWL */

//@node Unpacking,  , GranSim Code, GranSim Code
//@subsubsection Unpacking

#if defined(GRAN)
void
CommonUp(StgClosure *src, StgClosure *dst)
{
  barf("CommonUp: should never be entered in a GranSim setup");
}

/* This code fakes the unpacking of a somewhat virtual buffer */
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

    if (ip->type == RBH) {
      closure->header.gran.procs = PE_NUMBER(CurrentProc);    /* Move node */
      
      IF_GRAN_DEBUG(pack,
		    strcat(str, " (converting RBH) ")); 

      convertFromRBH(closure);   /* In GUM that's done by convertToFetchMe */
    } else if (IS_BLACK_HOLE(closure)) {
      closure->header.gran.procs |= PE_NUMBER(CurrentProc); /* Copy node */
    } else if ( closure->header.gran.procs & PE_NUMBER(CurrentProc) == 0 ) {
      if (closure_HNF(closure))
	closure->header.gran.procs |= PE_NUMBER(CurrentProc); /* Copy node */
      else
	closure->header.gran.procs = PE_NUMBER(CurrentProc);  /* Move node */
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
#endif

//@node Index,  , GranSim Code, Unpacking closures
//@subsection Index

//@index
//* CommonUp::  @cindex\s-+CommonUp
//* InitPendingGABuffer::  @cindex\s-+InitPendingGABuffer
//* PendingGABuffer::  @cindex\s-+PendingGABuffer
//* UnpackGraph::  @cindex\s-+UnpackGraph
//@end index
