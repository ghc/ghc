
/************************************************************************
 *                                                                      *
 * [Parallel.h]{Definitions for parallel machines}
 *									*
 ************************************************************************/

#ifndef Parallel_H
#define Parallel_H

/*
 * This section contains definitions applicable only to programs compiled
 * to run on a parallel machine.  
 *
 * These basic definitions need to be around, one way or the other:
 */

#include "ParTypes.h"

# ifdef PAR
#  define MAX_PES	256		/* Maximum number of processors */
	/* MAX_PES is enforced by SysMan, which does not
	   allow more than this many "processors".
	   This is important because PackGA [GlobAddr.lc]
	   **assumes** that a PE# can fit in 8+ bits.
	*/

extern I_ do_sp_profile;

extern P_ PendingFetches;
extern GLOBAL_TASK_ID *PEs;

extern rtsBool IAmMainThread, GlobalStopPending;
extern rtsBool fishing;
extern GLOBAL_TASK_ID SysManTask;
extern int seed;			/*pseudo-random-number generator seed:*/
					/*Initialised in ParInit*/
extern I_ threadId;                     /*Number of Threads that have existed on a PE*/
extern GLOBAL_TASK_ID mytid;

extern int  nPEs;

extern rtsBool InGlobalGC;  	/* Are we in the midst of performing global GC */

extern HashTable *pGAtoGALAtable;
extern HashTable *LAtoGALAtable;
extern GALA *freeIndirections;
extern GALA *liveIndirections;
extern GALA *freeGALAList;
extern GALA *liveRemoteGAs;
extern int thisPE;

void RunParallelSystem (StgPtr program_closure);
void initParallelSystem(void);
void SynchroniseSystem(void);

void registerTask (GLOBAL_TASK_ID gtid);
globalAddr *LAGAlookup (P_ addr);
P_ GALAlookup (globalAddr *ga);
globalAddr *MakeGlobal (P_ addr, rtsBool preferred);
globalAddr *setRemoteGA (P_ addr, globalAddr *ga, rtsBool preferred);
void splitWeight (globalAddr *to, globalAddr *from);
globalAddr *addWeight (globalAddr *ga);
void initGAtables(void);
W_ taskIDtoPE (GLOBAL_TASK_ID gtid);
void RebuildLAGAtable(void);

void *lookupHashTable (HashTable *table, StgWord key);
void insertHashTable (HashTable *table, StgWord key, void *data);
void freeHashTable (HashTable *table, void (*freeDataFun) (void *data));
HashTable *allocHashTable(void);
void *removeHashTable (HashTable *table, StgWord key, void *data);

extern void par_exit (I_);
#endif

/************************************************************************
 *									*
[anti-parallel-SM]{But if we're {\em not} compiling for a parallel system...}
 *									*
 ************************************************************************
 *
 *Get this out of the way.  These are all null definitions.
 */

#if defined(GRAN)

#  define GA_HDR_SIZE			1

#  define PROCS_HDR_POSN		PAR_HDR_POSN
#  define PROCS_HDR_SIZE		1

/* Accessing components of the field */
#  define	PROCS(closure)	        (*((P_)(closure)+PROCS_HDR_POSN))

#  define SET_PROCS(closure, procs) \
	PROCS(closure) = (W_)(procs)   	/* Set closure's location */
#  define SET_GRAN_HDR(closure,pe)	SET_PROCS(closure,pe)

#   define SET_STATIC_PROCS(closure)	, (W_) (Everywhere)

#  define SET_TASK_ACTIVITY(act)    	/* nothing */
#endif

/************************************************************************
 *									*
 *[parallel-GAs]{Parallel-only part of fixed headers (global addresses)}
 *									*
 ************************************************************************
 *
 *Definitions relating to the entire parallel-only fixed-header field.
 *
 *On GUM, the global addresses for each local closure are stored in a separate
 *hash table, rather then with the closure in the heap.  We call @getGA@ to
 *look up the global address associated with a local closure (0 is returned
 *for local closures that have no global address), and @setGA@ to store a new
 *global address for a local closure which did not previously have one.
 */

#if defined(PAR) 

#  define GA(closure)		        getGA(closure)
#  define SET_GA(closure, ga)             setGA(closure,ga)

#  define SET_STATIC_GA(closure)
#  define SET_STATIC_PROCS(closure)
  
#  define MAX_GA_WEIGHT			0	/* Treat as 2^n */
  
W_ PackGA (W_, int);
/* There was a PACK_GA macro here; but we turned it into the PackGA
 *      routine [GlobAddr.lc] (because it needs to do quite a bit of
 *      paranoia checking.  Phil & Will (95/08)
 */

/** 
 *At the moment, there is no activity profiling for GUM:
 */

#  define SET_TASK_ACTIVITY(act)



/************************************************************************
 *									*
 *[parallel-heap-objs]{Special parallel-only heap objects (`closures')}
 *									*
 ************************************************************************
 *
 *  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 *  NB: The following definitons are BOTH for GUM and GrAnSim -- HWL
 *  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 *
 *The rest of  this  file contains definitions  for  {\it  GUM  and GrAnSim}.
 *Although  we don't  create FetchMe   nodes in  GrAnSim  (we  simulate it by
 *bitmask  twiddling)  we use FetchMe_info   when converting  nodes into RBHs
 *(mainly  to keep the code as  close to GUM as  possible). So, we define all
 *the FetchMe related stuff in GrAnSim, too. % -- HWL
 *
 ************************************************************************
 *									*
 * [FETCHME-closures]{@FETCHME@ heap objects (`closures')}
 *									*
 ************************************************************************

 ... Zapped for now PWT ...
*/


/************************************************************************
 *									*
 [parallel-pack-defs]{Parallel-only Packing definitions}
 *									*
 ************************************************************************
 *
 *
 *Symbolic constants for the packing code.
 *
 *This constant defines how many words of data we can pack into a single
 *packet in the parallel (GUM) system.
 */

void	InitPackBuffer(void);
P_      PackTSO (P_ tso, W_ *size);
P_      PackStkO (P_ stko, W_ *size);
P_	AllocateHeap (W_ size); /* Doesn't belong */

void    InitClosureQueue (void);
P_      DeQueueClosure(void);
void    QueueClosure (P_ closure);
rtsBool QueueEmpty(void);
void    PrintPacket (P_ buffer);

P_      get_closure_info (P_ closure, W_ *size, W_ *ptrs, W_ *nonptrs, W_ *vhs, char *type);

rtsBool isOffset (globalAddr *ga),
	isFixed (globalAddr *ga);

void    doGlobalGC(void);

P_      PackNearbyGraph (P_ closure,W_ *size);
P_      UnpackGraph (W_ *buffer, globalAddr **gamap, W_ *nGAs);

#    define PACK_HEAP_REQUIRED  \
      ((RTSflags.ParFlags.packBufferSize - PACK_HDR_SIZE) / (PACK_GA_SIZE + _FHS) * (SPEC_HS + 2))

extern W_      *PackBuffer;      /* size: can be set via option */
extern long *buffer;             /* HWL_ */
extern W_ *freeBuffer;           /* HWL_ */
extern W_ *packBuffer;           /* HWL_ */

extern void    InitPackBuffer(void);
extern void    InitMoreBuffers(void);
extern void    InitPendingGABuffer(W_ size); 
extern void    AllocClosureQueue(W_ size);

#  define MAX_GAS 	(RTSflags.ParFlags.packBufferSize / PACK_GA_SIZE)


#  define PACK_GA_SIZE	3	/* Size of a packed GA in words */
			        /* Size of a packed fetch-me in words */
#  define PACK_FETCHME_SIZE (PACK_GA_SIZE + FIXED_HS)

#  define PACK_HDR_SIZE	1	/* Words of header in a packet */

#  define PACK_PLC_SIZE	2	/* Size of a packed PLC in words */

#if defined(GRAN)
/* ToDo: Check which of the PAR routines are needed in GranSim -- HWL */
void  InitPackBuffer(void);
P_    AllocateHeap (W_ size); /* Doesn't belong */
P_    PackNearbyGraph (P_ closure, P_ tso, W_ *packbuffersize);
P_    PackOneNode (P_ closure, P_ tso, W_ *packbuffersize);
P_    UnpackGraph (P_ buffer);

void    InitClosureQueue (void);
P_      DeQueueClosure(void);
void    QueueClosure (P_ closure);
rtsBool QueueEmpty(void);
void    PrintPacket (P_ buffer);

P_      get_closure_info (P_ closure, W_ *size, W_ *ptrs, W_ *nonptrs, W_ *vhs, char *type);

/* These are needed in the packing code to get the size of the packet
   right. The closures itself are never built in GrAnSim. */
#  define FETCHME_VHS				IND_VHS
#  define FETCHME_HS				IND_HS
  
#  define FETCHME_GA_LOCN                       FETCHME_HS
  
#  define FETCHME_CLOSURE_SIZE(closure)		IND_CLOSURE_SIZE(closure)
#  define FETCHME_CLOSURE_NoPTRS(closure)		0L
#  define FETCHME_CLOSURE_NoNONPTRS(closure)	(IND_CLOSURE_SIZE(closure)-IND_VHS)
  
#  define MAX_GAS 	(RTSflags.GranFlags.packBufferSize / PACK_GA_SIZE)
#  define PACK_GA_SIZE	3	/* Size of a packed GA in words */
			        /* Size of a packed fetch-me in words */
#  define PACK_FETCHME_SIZE (PACK_GA_SIZE + FIXED_HS)
#  define PACK_HDR_SIZE	4	/* Words of header in a packet */

#    define PACK_HEAP_REQUIRED  \
      ((RTSflags.GranFlags.packBufferSize - PACK_HDR_SIZE) / (PACK_GA_SIZE \
      + _FHS) * (SPEC_HS + 2)) 

#    define PACK_FLAG_LOCN           0  
#    define PACK_TSO_LOCN            1
#    define PACK_UNPACKED_SIZE_LOCN  2
#    define PACK_SIZE_LOCN           3
#    define MAGIC_PACK_FLAG          0xfabc
#endif /* GRAN */

#endif /* PAR */
#endif /* Parallel_H */



