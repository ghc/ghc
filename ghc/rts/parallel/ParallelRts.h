/* --------------------------------------------------------------------------
   Time-stamp: <Wed Jan 12 2000 16:22:43 Stardate: [-30]4194.45 hwloidl>
   $Id: ParallelRts.h,v 1.2 2000/01/13 14:34:09 hwloidl Exp $

   Variables and functions specific to the parallel RTS (i.e. GUM or GranSim)
   ----------------------------------------------------------------------- */

#ifndef PARALLEL_RTS_H
#define PARALLEL_RTS_H

#if defined(GRAN) || defined(PAR)

//@menu
//* Packing routines::		
//* Spark handling routines::	
//* GC routines::		
//* Debugging routines::	
//* Generating .gr profiles::	
//* Common macros::		
//* Index::			
//@end menu

#ifndef GRAN
// Dummy def for NO_PRI if not in GranSim
#define NO_PRI  0
#endif

//@node Packing routines, Spark handling routines
//@subsection Packing routines

#if defined(GRAN)
/* Statistics info */
extern nat tot_packets, tot_packet_size, tot_cuts, tot_thunks;
#endif

#if defined(GRAN)
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

/* General closure predicates */
/*
    {Parallel.h}Daq ngoqvam vIroQpu'

StgInfoTable *get_closure_info(StgClosure* node, nat *size, nat *ptrs, nat *nonptrs, nat *vhs, char *info_hdr_ty);
rtsBool      IS_BLACK_HOLE(StgClosure* node);
StgClosure  *IS_INDIRECTION(StgClosure* node);
rtsBool      IS_THUNK(StgClosure* closure);
*/

#elif defined(PAR)

/* Pack.c */
rtsPackBuffer *PackNearbyGraph(StgClosure* closure, StgTSO* tso, 
			       nat *packBufferSize); 

rtsPackBuffer *PackTSO(StgTSO *tso, nat *packBufferSize);
rtsPackBuffer *PackStkO(StgPtr stko, nat *packBufferSize);
void           PackFetchMe(StgClosure *closure);

/* Unpack.c */
void           CommonUp(StgClosure *src, StgClosure *dst);
StgClosure    *UnpackGraph(rtsPackBuffer *buffer, globalAddr **gamap, 
			   nat *nGAs);

/* RBH.c */
StgClosure    *convertToRBH(StgClosure *closure);
void           convertToFetchMe(StgRBH *rbh, globalAddr *ga);

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

//@node Spark handling routines, GC routines, Packing routines
//@subsection Spark handling routines

/* now in ../Sparks.c */

#if 0

#if defined(PAR)

rtsSpark  findLocalSpark(rtsBool forexport);
StgTSO*   activateSpark (rtsSpark spark); 
void      disposeSpark(rtsSpark spark);
rtsBool   add_to_spark_queue(StgClosure *closure, rtsBool required);
rtsBool   initSparkPools (void);

nat       spark_queue_len(nat pool);
void      markSparkQueue(void);
void      print_sparkq(void);

#elif defined(GRAN)

void      findLocalSpark (rtsEvent *event, 
			  rtsBool *found_res, rtsSparkQ *spark_res);
rtsBool   activateSpark (rtsEvent *event, rtsSparkQ spark);
rtsSpark *newSpark (StgClosure *node, StgInt name, StgInt gran_info, 
		    StgInt size_info, StgInt par_info, StgInt local);
void      disposeSpark(rtsSpark *spark);
void      disposeSparkQ(rtsSparkQ spark);
void      add_to_spark_queue(rtsSpark *spark);
void      print_spark(rtsSpark *spark);
nat       spark_queue_len(PEs proc);
rtsSpark *delete_from_sparkq (rtsSpark *spark, PEs p, rtsBool dispose_too);
void      markSparkQueue(void);
void      print_sparkq(PEs proc);
void      print_sparkq_stats(void);

#endif
#endif /* 0 */

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
#endif

//@node Generating .gr profiles, Common macros, Debugging routines
//@subsection Generating .gr profiles

#define STATS_FILENAME_MAXLEN	128

/* Where to write the log file */
//@cindex gr_file
//@cindex gr_filename
extern FILE *gr_file;
extern char gr_filename[STATS_FILENAME_MAXLEN];

//@cindex init_gr_simulation
//@cindex end_gr_simulation
void init_gr_simulation(int rts_argc, char *rts_argv[], 
			int prog_argc, char *prog_argv[]);
void end_gr_simulation(void);

//@node Common macros, Index, Generating .gr profiles
//@subsection Common macros

/* 
   extracting specific info out of a closure; used in packing (GranSim, GUM)
*/
//@cindex get_closure_info
static inline StgInfoTable*
get_closure_info(node, size, ptrs, nonptrs, vhs, info_hdr_ty)
StgClosure* node;
nat *size, *ptrs, *nonptrs, *vhs;
char *info_hdr_ty;
{
  StgInfoTable *info;

  info = get_itbl(node);
  /* the switch shouldn't be necessary, really; just use default case */
  switch (info->type) {
  case RBH:
    {
      StgInfoTable *rip = REVERT_INFOPTR(info); // closure to revert to
      *size = sizeW_fromITBL(rip);
      *ptrs = (nat) (rip->layout.payload.ptrs);
      *nonptrs = (nat) (rip->layout.payload.nptrs);
      *vhs = (nat) 0; // unknown
#if 0 /* DEBUG */
      info_hdr_type(node, info_hdr_ty);
#else
      strcpy(info_hdr_ty, "UNKNOWN");
#endif
      return rip;  // NB: we return the reverted info ptr for a RBH!!!!!!
    }

  default:
    *size = sizeW_fromITBL(info);
    *ptrs = (nat) (info->layout.payload.ptrs);
    *nonptrs = (nat) (info->layout.payload.nptrs);
    *vhs = (nat) 0; // unknown
#if 0 /* DEBUG */
      info_hdr_type(node, info_hdr_ty);
#else
      strcpy(info_hdr_ty, "UNKNOWN");
#endif
    return info;
  }
} 

//@cindex IS_BLACK_HOLE
static inline rtsBool
IS_BLACK_HOLE(StgClosure* node)          
{ 
  StgInfoTable *info;
  switch (get_itbl(node)->type) {
  case BLACKHOLE:
  case BLACKHOLE_BQ:
  case RBH:
  case FETCH_ME:
  case FETCH_ME_BQ:
    return rtsTrue;
  default:
    return rtsFalse;
  }
//return ((info->type == BLACKHOLE || info->type == RBH) ? rtsTrue : rtsFalse);
}

//@cindex IS_INDIRECTION
static inline StgClosure *
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

//@cindex unwindInd
static inline StgClosure *
UNWIND_IND (StgClosure *closure)
{
  StgClosure *next;

  while ((next = IS_INDIRECTION((StgClosure *)closure)) != NULL) 
    closure = next;

  ASSERT(next==(StgClosure *)NULL);
  return closure;
}

#endif /* defined(PAR) || defined(GRAN) */

#endif /* PARALLEL_RTS_H */

//@node Index,  , Common macros
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
