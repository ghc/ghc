/* --------------------------------------------------------------------------
   Time-stamp: <Tue Mar 06 2001 00:25:50 Stardate: [-30]6285.08 hwloidl>

   Variables and functions specific to the parallel RTS (i.e. GUM or GranSim)
   ----------------------------------------------------------------------- */

#ifndef PARALLEL_RTS_H
#define PARALLEL_RTS_H

#include "ParTicky.h"

/* HWL HACK: compile time sanity checks; shouldn't be necessary at all */
#if defined(PAR) && defined(GRAN)
# error "Both PAR and GRAN defined"
#endif

#if defined(DEBUG)
/* Paranoia debugging: we add an end-of-buffer marker to every pack buffer 
                       (only when sanity checking RTS is enabled, of course) */
#define  DEBUG_HEADROOM        1
#define  END_OF_BUFFER_MARKER  0x1111bbbb
#define  GARBAGE_MARKER        0x1111eeee
#else
#define  DEBUG_HEADROOM        0
#endif /* DEBUG */

#if defined(GRAN) || defined(PAR)

#if defined(GRAN)

/* Statistics info */
extern nat tot_packets, tot_packet_size, tot_cuts, tot_thunks;

/* Pack.c */
rtsPackBuffer *PackNearbyGraph(StgClosure* closure, StgTSO* tso, 
			       nat *packBufferSize, GlobalTaskId dest); 
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
void               blockThread(StgTSO *tso);

#endif
#if defined(PAR)

/* Statistics info */

/* global structure for collecting statistics */
typedef struct GlobalParStats_ {
  /* GALA and LAGA table info */
  nat tot_mark_GA, tot_rebuild_GA, tot_free_GA,
      res_mark_GA, res_rebuild_GA, res_free_GA,
      cnt_mark_GA, cnt_rebuild_GA, cnt_free_GA,
      res_size_GA, tot_size_GA, local_alloc_GA, tot_global, tot_local;

  /* time spent managing the GAs */
  double time_mark_GA, time_rebuild_GA;

  /* spark queue stats */
  nat res_sp, tot_sp, cnt_sp, emp_sp;
  // nat tot_sq_len, tot_sq_probes, tot_sparks;
  /* thread queue stats */
  nat res_tp, tot_tp, cnt_tp, emp_tp;
  //nat tot_add_threads, tot_tq_len, non_end_add_threads;

  /* packet statistics */
  nat tot_packets, tot_packet_size, tot_thunks,
      res_packet_size, res_thunks,
      rec_packets, rec_packet_size, rec_thunks,
      rec_res_packet_size, rec_res_thunks;
  /* time spent packing stuff */
  double time_pack, time_unpack;

  /* thread stats */
  nat tot_threads_created;

  /* spark stats */
  //nat pruned_sparks, withered_sparks;
  nat tot_sparks_created, tot_sparks_ignored, tot_sparks_marked,
      res_sparks_created, res_sparks_ignored, res_sparks_marked; // , sparks_created_on_PE[MAX_PROC];
  double time_sparks;

  /* scheduling stats */
  nat tot_yields, tot_stackover, tot_heapover;

  /* message statistics */
  nat tot_fish_mess, tot_fetch_mess, tot_resume_mess, tot_schedule_mess;
  nat rec_fish_mess, rec_fetch_mess, rec_resume_mess, rec_schedule_mess;
#if defined(DIST)
  nat tot_reval_mess;
  nat rec_reval_mess;
#endif

  /* blocking queue statistics
  rtsTime tot_bq_processing_time;
  nat tot_bq_len, tot_bq_len_local, tot_awbq, tot_FMBQs;
  */

  /* specialised info on arrays (for GPH/Maple mainly) */
  nat tot_arrs, tot_arr_size;
} GlobalParStats;

extern GlobalParStats globalParStats;

void  globalParStat_exit(void);

/* Pack.c */
rtsBool        InitPackBuffer(void);
rtsPackBuffer *PackNearbyGraph(StgClosure* closure, StgTSO* tso, 
			       nat *packBufferSize, GlobalTaskId dest); 

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

/* HLComms.c */
nat            pending_fetches_len(void);

/* ParInit.c */
void 	       initParallelSystem(void);
void 	       shutdownParallelSystem(StgInt n);
void 	       synchroniseSystem(void);
void 	       par_exit(I_);

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
void      markPendingFetches(rtsBool major_gc);

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
void 	  	init_gr_stats (void);
void 	  	init_gr_simulation(int rts_argc, char *rts_argv[], 
 	 			   int prog_argc, char *prog_argv[]);
void 	  	end_gr_simulation(void);

// TODO: move fcts in here (as static inline)
StgInfoTable*   get_closure_info(StgClosure* node, nat *size, nat *ptrs, nat *nonptrs, nat *vhs, char *info_hdr_ty);
rtsBool         IS_BLACK_HOLE(StgClosure* node);
StgClosure     *IS_INDIRECTION(StgClosure* node)          ;
StgClosure     *UNWIND_IND (StgClosure *closure);


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
