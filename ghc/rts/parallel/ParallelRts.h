/* --------------------------------------------------------------------------
   Time-stamp: <Wed Mar 29 2000 19:10:29 Stardate: [-30]4578.78 hwloidl>
   $Id: ParallelRts.h,v 1.3 2000/03/31 03:09:37 hwloidl Exp $

   Variables and functions specific to the parallel RTS (i.e. GUM or GranSim)
   ----------------------------------------------------------------------- */

#ifndef PARALLEL_RTS_H
#define PARALLEL_RTS_H

#if defined(GRAN) || defined(PAR)

#if defined(GRAN)

/* Statistics info */
extern nat tot_packets, tot_packet_size, tot_cuts, tot_thunks;

/* Pack.c */
rtsPackBuffer *PackNearbyGraph(StgClosure* closure, StgTSO* tso, 
			       nat *packBufferSize);
rtsPackBuffer *PackOneNode(StgClosure* closure, StgTSO* tso, 
			   nat *packBufferSize);
rtsPackBuffer *PackTSO(StgTSO *tso, nat *packBufferSize);
rtsPackBuffer *PackStkO(StgPtr stko, nat *packBufferSize);
void           PackFetchMe(StgClosure *closure);

/* Unpack.c */
StgClosure*    UnpackGraph(rtsPackBuffer* buffer);
void           InitPendingGABuffer(nat size); 

/* RBH.c */
StgClosure    *convertToRBH(StgClosure *closure);
void           convertFromRBH(StgClosure *closure);

/* HLComms.c */
rtsFetchReturnCode blockFetch(StgTSO* tso, PEs proc, StgClosure* bh);
void           blockThread(StgTSO *tso);

/* General closure predicates */
/*
    {Parallel.h}Daq ngoqvam vIroQpu'

StgInfoTable *get_closure_info(StgClosure* node, nat *size, nat *ptrs, nat *nonptrs, nat *vhs, char *info_hdr_ty);
rtsBool      IS_BLACK_HOLE(StgClosure* node);
StgClosure  *IS_INDIRECTION(StgClosure* node);
rtsBool      IS_THUNK(StgClosure* closure);
*/

#endif
#if defined(PAR)

/* Pack.c */
rtsPackBuffer *PackNearbyGraph(StgClosure* closure, StgTSO* tso, 
			       nat *packBufferSize); 

/* Unpack.c */
void           CommonUp(StgClosure *src, StgClosure *dst);
StgClosure    *UnpackGraph(rtsPackBuffer *buffer, globalAddr **gamap, 
			   nat *nGAs);

/* RBH.c */
StgClosure    *convertToRBH(StgClosure *closure);
void           convertToFetchMe(StgRBH *rbh, globalAddr *ga);

/* HLComms.c */
void           blockFetch(StgBlockedFetch *bf, StgClosure *bh);
void           blockThread(StgTSO *tso);

/* Global.c */
void           GALAdeprecate(globalAddr *ga);

/* General closure predicates */
/* 
  {Parallel.h}Daq ngoqvam vIroQpu'

StgInfoTable *get_closure_info(StgClosure* node, nat *size, nat *ptrs, nat *nonptrs, nat *vhs, char *info_hdr_ty);
rtsBool      IS_BLACK_HOLE(StgClosure* node);
StgClosure  *IS_INDIRECTION(StgClosure* node);
rtsBool      IS_THUNK(StgClosure* closure);
*/

#endif

/* this routine should be moved to a more general module; currently in Pack.c 
StgInfoTable* get_closure_info(StgClosure* node, 
			       nat *size, nat *ptrs, nat *nonptrs, nat *vhs, 
			       char *info_hdr_ty);
*/
void doGlobalGC(void); 

//@node GC routines, Debugging routines, Spark handling routines
//@subsection GC routines

#if defined(PAR)
/* HLComms.c */
void      freeRemoteGA(int pe, globalAddr *ga);
void      sendFreeMessages(void);

/* Global.c */
void      markLocalGAs(rtsBool full);
void      RebuildGAtables(rtsBool full);
void      RebuildLAGAtable(void);
#endif

//@node Debugging routines, Generating .gr profiles, GC routines
//@subsection Debugging routines

#if defined(PAR)
void      printGA (globalAddr *ga);
void      printGALA (GALA *gala);
void      printLAGAtable(void);

rtsBool   isOnLiveIndTable(globalAddr *ga);
rtsBool   isOnRemoteGATable(globalAddr *ga);
void      checkFreeGALAList(void);
void      checkFreeIndirectionsList(void);
#endif

//@node Generating .gr profiles, Index, Debugging routines
//@subsection Generating .gr profiles

#define STATS_FILENAME_MAXLEN	128

/* Where to write the log file */
//@cindex gr_file
//@cindex gr_filename
extern FILE *gr_file;
extern char gr_filename[STATS_FILENAME_MAXLEN];

//@cindex init_gr_stats
//@cindex init_gr_simulation
//@cindex end_gr_simulation
void init_gr_stats (void);
void init_gr_simulation(int rts_argc, char *rts_argv[], 
			int prog_argc, char *prog_argv[]);
void end_gr_simulation(void);

// TODO: move fcts in here (as static inline)
StgInfoTable* get_closure_info(StgClosure* node, nat *size, nat *ptrs, nat *nonptrs, nat *vhs, char *info_hdr_ty);
rtsBool IS_BLACK_HOLE(StgClosure* node);
StgClosure *IS_INDIRECTION(StgClosure* node)          ;
StgClosure *UNWIND_IND (StgClosure *closure);


#endif /* defined(PAR) || defined(GRAN) */

//@node Common macros, Index, Generating .gr profiles
//@subsection Common macros

#define LOOKS_LIKE_PTR(r)    \
        (LOOKS_LIKE_STATIC_CLOSURE(r) ||  \
         ((HEAP_ALLOCED(r) && Bdescr((P_)r)->free != (void *)-1)))

/* see Sanity.c for this kind of test; doing this in these basic fcts
   is paranoid (nuke it after debugging!)
*/

/* pathetic version of the check whether p can be a closure */
#define LOOKS_LIKE_COOL_CLOSURE(p)  1

//LOOKS_LIKE_GHC_INFO(get_itbl(p))

    /* Is it a static closure (i.e. in the data segment)? */ \
    /*
#define LOOKS_LIKE_COOL_CLOSURE(p)  \
    ((LOOKS_LIKE_STATIC(p)) ?                                   \
	closure_STATIC(p)                               \
      : !closure_STATIC(p) && LOOKS_LIKE_PTR(p))
    */

#endif /* PARALLEL_RTS_H */

//@node Index,  , Index
//@subsection Index

//@index
//* IS_BLACK_HOLE::  @cindex\s-+IS_BLACK_HOLE
//* IS_INDIRECTION::  @cindex\s-+IS_INDIRECTION
//* end_gr_simulation::  @cindex\s-+end_gr_simulation
//* get_closure_info::  @cindex\s-+get_closure_info
//* gr_file::  @cindex\s-+gr_file
//* gr_filename::  @cindex\s-+gr_filename
//* init_gr_simulation::  @cindex\s-+init_gr_simulation
//* unwindInd::  @cindex\s-+unwindInd
//@end index
