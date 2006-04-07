/*
  Time-stamp: <Mon Oct 04 1999 14:50:28 Stardate: [-30]3692.88 hwloidl>
 
  Definitions for parallel machines.

This section contains definitions applicable only to programs compiled
to run on a parallel machine, i.e. on GUM. Some of these definitions
are also used when simulating parallel execution, i.e. on GranSim.
  */

/*
  ToDo: Check the PAR specfic part of this file 
        Move stuff into Closures.h and ClosureMacros.h 
	Clean-up GRAN specific code
  -- HWL
  */

#ifndef PARALLEL_H
#define PARALLEL_H

#if defined(PAR) || defined(GRAN)        /* whole file */

#include "Rts.h"
#include "GranSim.h"
//#include "ClosureTypes.h"

//@menu
//* Basic definitions::		
//* Externs and types::		
//* Dummy defs::		
//* Par specific fixed headers::  
//* Parallel only heap objects::  
//* Packing definitions::	
//* End of File::		
//@end menu
//*/

//@node Basic definitions, Externs and types
//@section Basic definitions

/* SET_PAR_HDR and SET_STATIC_PAR_HDR now live in ClosureMacros.h */

/* Needed for dumping routines */
#if defined(PAR)
# define TIME                      ullong
# define CURRENT_TIME              msTime()
# define TIME_ON_PROC(p)           msTime()
# define CURRENT_PROC              thisPE
# define BINARY_STATS              RtsFlags.ParFlags.granSimStats_Binary
#elif defined(GRAN)
# define TIME                      rtsTime
# define CURRENT_TIME              CurrentTime[CurrentProc]
# define TIME_ON_PROC(p)           CurrentTime[p]
# define CURRENT_PROC              CurrentProc
# define BINARY_STATS              RtsFlags.GranFlags.granSimStats_Binary
#endif

#if defined(PAR)
#  define MAX_PES	256		/* Maximum number of processors */
	/* MAX_PES is enforced by SysMan, which does not
	   allow more than this many "processors".
	   This is important because PackGA [GlobAddr.lc]
	   **assumes** that a PE# can fit in 8+ bits.
	*/
#endif

//@node Externs and types, Dummy defs, Basic definitions
//@section Externs and types

#if defined(PAR)
/* GUM: one spark queue on each PE, and each PE sees only its own spark queue */
extern rtsSparkQ pending_sparks_hd;
extern rtsSparkQ pending_sparks_tl;
#elif defined(GRAN)
/* GranSim: a globally visible array of spark queues */
extern rtsSparkQ pending_sparks_hds[];
extern rtsSparkQ pending_sparks_tls[];
#endif
extern unsigned int /* nat */ spark_queue_len(PEs proc);

extern StgInt SparksAvail;     /* How many sparks are available */

/* prototypes of spark routines */
/* ToDo: check whether all have to be visible -- HWL */
#if defined(GRAN)
rtsSpark *newSpark(StgClosure *node, StgInt name, StgInt gran_info, StgInt size_info, StgInt par_info, StgInt local);
void disposeSpark(rtsSpark *spark);
void disposeSparkQ(rtsSparkQ spark);
void add_to_spark_queue(rtsSpark *spark);
void delete_from_spark_queue (rtsSpark *spark);
#endif

#define STATS_FILENAME_MAXLEN	128

/* Where to write the log file */
//extern FILE *gr_file;
extern char gr_filename[STATS_FILENAME_MAXLEN];

#if defined(GRAN)
int init_gr_simulation(char *rts_argv[], int rts_argc, char *prog_argv[], int prog_argc);
void end_gr_simulation(void);
#endif 

#if defined(PAR)
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
void initParallelSystem();
void SynchroniseSystem();

void registerTask (GLOBAL_TASK_ID gtid);
globalAddr *LAGAlookup (P_ addr);
P_ GALAlookup (globalAddr *ga);
globalAddr *MakeGlobal (P_ addr, rtsBool preferred);
globalAddr *setRemoteGA (P_ addr, globalAddr *ga, rtsBool preferred);
void splitWeight (globalAddr *to, globalAddr *from);
globalAddr *addWeight (globalAddr *ga);
void initGAtables();
W_ taskIDtoPE (GLOBAL_TASK_ID gtid);
void RebuildLAGAtable();

void *lookupHashTable (HashTable *table, StgWord key);
void insertHashTable (HashTable *table, StgWord key, void *data);
void freeHashTable (HashTable *table, void (*freeDataFun) ((void *data)));
HashTable *allocHashTable();
void *removeHashTable (HashTable *table, StgWord key, void *data);
#endif /* PAR */

/* Interface for dumping routines (i.e. writing to log file) */
void DumpGranEvent(GranEventType name, StgTSO *tso);
void DumpRawGranEvent(PEs proc, PEs p, GranEventType name, 
 	              StgTSO *tso, StgClosure *node, StgInt sparkname, StgInt len);
//void DumpEndEvent(PEs proc, StgTSO *tso, rtsBool mandatory_thread);

//@node Dummy defs, Par specific fixed headers, Externs and types
//@section Dummy defs

/*
Get this out of the way.  These are all null definitions.
*/


//#  define GA_HDR_SIZE			0 
//#  define GA(closure)	        	/*nothing */ 
  
//#  define SET_GA(closure,ga)		/* nothing */ 
//#  define SET_STATIC_GA(closure)	/* nothing */ 
//#  define SET_GRAN_HDR(closure,pe)      /* nothing */ 
//#  define SET_STATIC_PROCS(closure)	/* nothing */ 
  
//#  define SET_TASK_ACTIVITY(act)	/* nothing */ 

#if defined(GRAN)

#  define GA_HDR_SIZE			1

#  define PROCS_HDR_POSN		PAR_HDR_POSN
#  define PROCS_HDR_SIZE		1

/* Accessing components of the field */
#  define PROCS(closure)	        ((closure)->header.gran.procs)
/* SET_PROCS is now SET_GRAN_HEADER in ClosureMacros.h. */
#endif


//@node Par specific fixed headers, Parallel only heap objects, Dummy defs
//@section Par specific fixed headers

/*
Definitions relating to the entire parallel-only fixed-header field.

On GUM, the global addresses for each local closure are stored in a separate
hash table, rather then with the closure in the heap.  We call @getGA@ to
look up the global address associated with a local closure (0 is returned
for local closures that have no global address), and @setGA@ to store a new
global address for a local closure which did not previously have one.
*/

#if defined(PAR) 

#  define GA_HDR_SIZE			0
  
#  define GA(closure)		        getGA(closure)
  
#  define SET_GA(closure, ga)             setGA(closure,ga)
#  define SET_STATIC_GA(closure)
#  define SET_GRAN_HDR(closure,pe)
#  define SET_STATIC_PROCS(closure)
  
#  define MAX_GA_WEIGHT			0	/* Treat as 2^n */
  
W_ PackGA ((W_, int));
   /* There was a PACK_GA macro here; but we turned it into the PackGA
      routine [GlobAddr.lc] (because it needs to do quite a bit of
      paranoia checking.  Phil & Will (95/08)
   */

/* At the moment, there is no activity profiling for GUM.  This may change. */
#  define SET_TASK_ACTIVITY(act)        /* nothing */
#endif

//@node Parallel only heap objects, Packing definitions, Par specific fixed headers
//@section Parallel only heap objects

// NB: The following definitons are BOTH for GUM and GrAnSim -- HWL

/*   All in Closures.h and CLosureMacros.h */

//@node Packing definitions, End of File, Parallel only heap objects
//@section Packing definitions

//@menu
//* GUM::			
//* GranSim::			
//@end menu
//*/

//@node GUM, GranSim, Packing definitions, Packing definitions
//@subsection GUM

#if defined(PAR) 
/*
Symbolic constants for the packing code.

This constant defines how many words of data we can pack into a single
packet in the parallel (GUM) system.
*/

//@menu
//* Externs::			
//* Prototypes::		
//* Macros::			
//@end menu
//*/

//@node Externs, Prototypes, GUM, GUM
//@subsubsection Externs

extern W_      *PackBuffer;      /* size: can be set via option */
extern long *buffer;             /* HWL_ */
extern W_ *freeBuffer;           /* HWL_ */
extern W_ *packBuffer;           /* HWL_ */

extern void    InitPackBuffer(STG_NO_ARGS);
extern void    InitMoreBuffers(STG_NO_ARGS);
extern void    InitPendingGABuffer(W_ size); 
extern void    AllocClosureQueue(W_ size);

//@node Prototypes, Macros, Externs, GUM
//@subsubsection Prototypes

void	InitPackBuffer();
P_      PackTSO (P_ tso, W_ *size);
P_      PackStkO (P_ stko, W_ *size);
P_	AllocateHeap (W_ size);          /* Doesn't belong */

void    InitClosureQueue ();
P_      DeQueueClosure();
void    QueueClosure (P_ closure);
rtsBool QueueEmpty();
void    PrintPacket (P_ buffer);

P_      get_closure_info (P_ closure, W_ *size, W_ *ptrs, W_ *nonptrs, W_ *vhs, char *type);

rtsBool isOffset (globalAddr *ga),
	isFixed (globalAddr *ga);

void    doGlobalGC();

P_      PackNearbyGraph (P_ closure,W_ *size);
P_      UnpackGraph (W_ *buffer, globalAddr **gamap, W_ *nGAs);


//@node Macros,  , Prototypes, GUM
//@subsubsection Macros

#    define PACK_HEAP_REQUIRED  \
      ((RtsFlags.ParFlags.packBufferSize - PACK_HDR_SIZE) / (PACK_GA_SIZE + _FHS) * (SPEC_HS + 2))

#  define MAX_GAS 	(RtsFlags.ParFlags.packBufferSize / PACK_GA_SIZE)


#  define PACK_GA_SIZE	3	/* Size of a packed GA in words */
			        /* Size of a packed fetch-me in words */
#  define PACK_FETCHME_SIZE (PACK_GA_SIZE + FIXED_HS)

#  define PACK_HDR_SIZE	1	/* Words of header in a packet */

#  define PACK_PLC_SIZE	2	/* Size of a packed PLC in words */

#endif /* PAR */

//@node GranSim,  , GUM, Packing definitions
//@subsection GranSim

#if defined(GRAN)
/* ToDo: Check which of the PAR routines are needed in GranSim -- HWL */

//@menu
//* Types::			
//* Prototypes::		
//* Macros::			
//@end menu
//*/

//@node Types, Prototypes, GranSim, GranSim
//@subsubsection Types

typedef struct rtsPackBuffer_ {
  StgInt /* nat */           size;
  StgInt /* nat */           unpacked_size;
  StgTSO       *tso;
  StgClosure  **buffer;  
} rtsPackBuffer;

//@node Prototypes, Macros, Types, GranSim
//@subsubsection Prototypes


/* main packing functions */
/*
rtsPackBuffer *PackNearbyGraph(StgClosure* closure, StgTSO* tso, nat *packbuffersize);
rtsPackBuffer *PackOneNode(StgClosure* closure, StgTSO* tso, nat *packbuffersize);
void PrintPacket(rtsPackBuffer *buffer);
StgClosure *UnpackGraph(rtsPackBuffer* buffer);
*/
/* important auxiliary functions */

//StgInfoTable *get_closure_info(StgClosure* node, nat *size, nat *ptrs, nat *nonptrs, nat *vhs, char *info_hdr_ty);
int IS_BLACK_HOLE(StgClosure* node);
StgClosure *IS_INDIRECTION(StgClosure* node);
int IS_THUNK(StgClosure* closure);
char *display_info_type(StgClosure* closure, char *str);

/* 
OLD CODE -- HWL
void  InitPackBuffer(void);
P_    AllocateHeap (W_ size);
P_    PackNearbyGraph (P_ closure, P_ tso, W_ *packbuffersize);
P_    PackOneNode (P_ closure, P_ tso, W_ *packbuffersize);
P_    UnpackGraph (P_ buffer);

void    InitClosureQueue (void);
P_      DeQueueClosure(void);
void    QueueClosure (P_ closure);
// rtsBool QueueEmpty();
void    PrintPacket (P_ buffer);
*/

// StgInfoTable *get_closure_info(StgClosure* node, unsigned int /* nat */ *size, unsigned int /* nat */ *ptrs, unsigned int /* nat */ *nonptrs, unsigned int /* nat */ *vhs, char *info_hdr_ty);
// int /* rtsBool */ IS_BLACK_HOLE(StgClosure* node)          ;

//@node Macros,  , Prototypes, GranSim
//@subsubsection Macros

/* These are needed in the packing code to get the size of the packet
   right. The closures itself are never built in GrAnSim. */
#  define FETCHME_VHS				IND_VHS
#  define FETCHME_HS				IND_HS
  
#  define FETCHME_GA_LOCN                       FETCHME_HS
  
#  define FETCHME_CLOSURE_SIZE(closure)		IND_CLOSURE_SIZE(closure)
#  define FETCHME_CLOSURE_NoPTRS(closure)		0L
#  define FETCHME_CLOSURE_NoNONPTRS(closure)	(IND_CLOSURE_SIZE(closure)-IND_VHS)
  
#  define MAX_GAS 	(RtsFlags.GranFlags.packBufferSize / PACK_GA_SIZE)
#  define PACK_GA_SIZE	3	/* Size of a packed GA in words */
			        /* Size of a packed fetch-me in words */
#  define PACK_FETCHME_SIZE (PACK_GA_SIZE + FIXED_HS)
#  define PACK_HDR_SIZE	4	/* Words of header in a packet */

#    define PACK_HEAP_REQUIRED  \
      (RtsFlags.GranFlags.packBufferSize * sizeofW(StgClosure*) + \
      2 * sizeofW(StgInt) + sizeofW(StgTSO*))

#    define PACK_FLAG_LOCN           0  
#    define PACK_TSO_LOCN            1
#    define PACK_UNPACKED_SIZE_LOCN  2
#    define PACK_SIZE_LOCN           3
#    define MAGIC_PACK_FLAG          0xfabc

#endif   /* GRAN */

//@node End of File,  , Packing definitions
//@section End of File

#endif /* defined(PAR) || defined(GRAN)         whole file */
#endif /* Parallel_H */


