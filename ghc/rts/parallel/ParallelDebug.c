/*
  Time-stamp: <Sun Mar 18 2001 19:32:56 Stardate: [-30]6349.07 hwloidl>

  Various debugging routines for GranSim and GUM
*/

#if defined(DEBUG) && (defined(GRAN) || defined(PAR))        /* whole file */

//@node Debugging routines for GranSim and GUM, , ,
//@section Debugging routines for GranSim and GUM

//@menu
//* Includes::			
//* Constants and Variables::	
//* Closures::			
//* Threads::			
//* Events::			
//* Sparks::			
//* Processors::		
//* Shortcuts::			
//* Printing info type::	
//* Printing Pack:et Contents::	
//* End of File::		
//@end menu
//*/

//@node Includes, Prototypes, Debugging routines for GranSim and GUM, Debugging routines for GranSim and GUM
//@subsection Includes

#include "Rts.h"
#include "RtsFlags.h"
#include "GranSimRts.h"
#include "ParallelRts.h"
#include "StgMiscClosures.h"
#include "Printer.h"
# if defined(DEBUG)
# include "Hash.h" 
# include "Storage.h"
# include "ParallelDebug.h"
# endif

//@node Prototypes, Constants and Variables, Includes, Debugging routines for GranSim and GUM
//@subsection Prototypes
/*
rtsBool  isOffset(globalAddr *ga);
rtsBool  isFixed(globalAddr *ga);
*/
//@node Constants and Variables, Closures, Prototypes, Debugging routines for GranSim and GUM
//@subsection Constants and Variables

static HashTable *tmpClosureTable;  // used in GraphFingerPrint and PrintGraph

#if defined(PAR)
static char finger_print_char[] = {
 '/',  /* INVALID_OBJECT          0 */
 'C', /* CONSTR                  1 */
 'C', /*	CONSTR_1_0		2 */
 'C', /*	CONSTR_0_1		3 */
 'C', /*	CONSTR_2_0		4 */
 'C', /*	CONSTR_1_1		5 */
 'C', /*	CONSTR_0_2		6 */
 'I', /* CONSTR_INTLIKE	        7  */
 'I', /* CONSTR_CHARLIKE	        8  */
 'S', /* CONSTR_STATIC	        9  */
 'S', /* CONSTR_NOCAF_STATIC     10 */
 'F', /* FUN		        11 */
 'F', /*	FUN_1_0		  	12 */
 'F', /*	FUN_0_1		  	13 */
 'F', /*	FUN_2_0		  	14 */
 'F', /*	FUN_1_1		  	15 */
 'F', /*	FUN_0_2			16 */
 'S', /* FUN_STATIC	        17 */
 'T', /* THUNK		        18 */
 'T', /*	THUNK_1_0	19 */
 'T', /*	THUNK_0_1	20 */
 'T', /*	THUNK_2_0	21 */
 'T', /*	THUNK_1_1	22 */
 'T', /*	THUNK_0_2	23 */
 'S', /* THUNK_STATIC	        24 */
 'E', /* THUNK_SELECTOR	        25 */
 'b', /* BCO		        26 */
 'p', /* AP_UPD		        27 */
 'p', /* PAP			28 */
 '_', /* IND		        29 */
 '_', /* IND_OLDGEN	        30 */
 '_', /* IND_PERM	        31 */
 '_', /* IND_OLDGEN_PERM	32 */
 '_', /* IND_STATIC	        33 */
 '?', /* ***unused***	        34 */
 '?', /* ***unused***	        35 */
 '^', /* RET_BCO                36 */
 '^', /* RET_SMALL	        37 */
 '^', /* RET_VEC_SMALL	        38 */
 '^', /* RET_BIG		39 */
 '^', /* RET_VEC_BIG	        40 */
 '^', /* RET_DYN		41 */
 '~', /* UPDATE_FRAME	        42 */
 '~', /* CATCH_FRAME	        43 */
 '~', /* STOP_FRAME	        44 */
 '~', /* SEQ_FRAME	        45 */
 'o', /* CAF_BLACKHOLE	        46 */
 'o', /* BLACKHOLE	        47 */
 'o', /* BLACKHOLE_BQ	        48 */
 'o', /* SE_BLACKHOLE		49 */
 'o', /* SE_CAF_BLACKHOLE	50 */
 'm', /* MVAR		        51 */
 'a', /* ARR_WORDS	        52 */
 'a', /* MUT_ARR_PTRS	        53 */
 'a', /* MUT_ARR_PTRS_FROZEN    54 */
 'q', /* MUT_VAR		55 */
 'w', /* WEAK		        56 */
 'f', /* FOREIGN		57 */
 's', /* STABLE_NAME	        58 */
 '@', /* TSO		        59 */
 '#', /* BLOCKED_FETCH	        60 */
 '>', /* FETCH_ME               61 */
 '>', /* FETCH_ME_BQ            62 */
 '$', /* RBH                    63 */
 'v', /* EVACUATED              64 */
 '>' /* REMOTE_REF              65 */  
     /* ASSERT(there are N_CLOSURE_TYPES (==66) in this arrary) */
};
#endif /* PAR */

#if defined(GRAN) && defined(GRAN_CHECK)
//@node Closures, Threads, Constants and Variables, Debugging routines for GranSim and GUM
//@subsection Closures

void
G_PRINT_NODE(node)
StgClosure* node;
{
   StgInfoTable *info_ptr;
   StgTSO* bqe;
   nat size = 0, ptrs = 0, nonptrs = 0, i, vhs = 0;
   char info_hdr_ty[80], info_ty[80];

   if (node==NULL) {
     fprintf(stderr,"NULL\n");
     return;
   } else if (node==END_TSO_QUEUE) {
     fprintf(stderr,"END_TSO_QUEUE\n");
     return;
   }
   /* size_and_ptrs(node,&size,&ptrs); */
   info_ptr = get_closure_info(node, &size, &ptrs, &nonptrs, &vhs, info_hdr_ty);

   /* vhs = var_hdr_size(node); */
   display_info_type(info_ptr,info_ty);

   fprintf(stderr,"Node: 0x%lx", node);

#if defined(PAR)
   fprintf(stderr," [GA: 0x%lx]",GA(node));
#endif

#if defined(USE_COST_CENTRES)
   fprintf(stderr," [CC: 0x%lx]",CC_HDR(node));
#endif

#if defined(GRAN)
   fprintf(stderr," [Bitmask: 0%lo]",PROCS(node));
#endif

   if (info_ptr->type==TSO) 
     fprintf(stderr," TSO: 0x%lx (%x) IP: 0x%lx (%s), type %s \n     ",
	     (StgTSO*)node, ((StgTSO*)node)->id, info_ptr, info_hdr_ty, info_ty);
   else
     fprintf(stderr," IP: 0x%lx (%s), type %s \n       VHS: %d, size: %ld, ptrs:%ld, nonptrs:  %ld\n     ",
	     info_ptr,info_hdr_ty,info_ty,vhs,size,ptrs,nonptrs);

   /* For now, we ignore the variable header */

   fprintf(stderr," Ptrs: ");
   for(i=0; i < ptrs; ++i)
     {
     if ( (i+1) % 6 == 0)
       fprintf(stderr,"\n      ");
     fprintf(stderr," 0x%lx[P]",node->payload[i]);
     };

   fprintf(stderr," Data: ");
   for(i=0; i < nonptrs; ++i)
     {
       if( (i+1) % 6 == 0)
         fprintf(stderr,"\n      ");
       fprintf(stderr," %lu[D]",node->payload[ptrs+i]);
     }
   fprintf(stderr, "\n");


   switch (info_ptr->type)
    {
     case TSO: 
      fprintf(stderr,"\n TSO_LINK: %#lx", 
	      ((StgTSO*)node)->link);
      break;

    case BLACKHOLE:
    case RBH:
      bqe = ((StgBlockingQueue*)node)->blocking_queue;
      fprintf(stderr," BQ of %#lx: ", node);
      G_PRINT_BQ(bqe);
      break;
    case FETCH_ME:
    case FETCH_ME_BQ:
      printf("Panic: found FETCH_ME or FETCH_ME_BQ Infotable in GrAnSim system.\n");
      break;
    default:
      /* do nothing */
    }
}

void
G_PPN(node)  /* Extracted from PrintPacket in Pack.lc */
StgClosure* node;
{
   StgInfoTable *info ;
   nat size = 0, ptrs = 0, nonptrs = 0, i, vhs = 0, locn = 0;
   char info_type[80];

   /* size_and_ptrs(node,&size,&ptrs); */
   info = get_closure_info(node, &size, &ptrs, &nonptrs, &vhs, info_type);

   if (info->type == FETCH_ME || info->type == FETCH_ME_BQ || 
       info->type == BLACKHOLE || info->type == RBH )
     size = ptrs = nonptrs = vhs = 0;

   if (closure_THUNK(node)) {
     if (!closure_UNPOINTED(node))
       fputs("SHARED ", stderr);
     else
       fputs("UNSHARED ", stderr);
   } 
   if (info->type==BLACKHOLE) {
     fputs("BLACK HOLE\n", stderr);
   } else {
     /* Fixed header */
     fprintf(stderr, "(%s) FH [%#lx", info_type, node[locn++]);
     for (i = 1; i < _HS; i++)
       fprintf(stderr, " %#lx", node[locn++]);
     
     /* Variable header */
     if (vhs > 0) {
       fprintf(stderr, "] VH [%#lx", node->payload[0]);
       
       for (i = 1; i < vhs; i++)
	 fprintf(stderr, " %#lx", node->payload[i]);
     }
     
     fprintf(stderr, "] PTRS %u", ptrs);
     
     /* Non-pointers */
     if (nonptrs > 0) {
       fprintf(stderr, " NPTRS [%#lx", node->payload[ptrs]);
       
       for (i = 1; i < nonptrs; i++)
	 fprintf(stderr, " %#lx", node->payload[ptrs+i]);
       
       putc(']', stderr);
     }
     putc('\n', stderr);
   }
   
}

#if 0
// ToDo: fix this!! -- HWL
void
G_INFO_TABLE(node)
StgClosure *node;
{
  StgInfoTable *info_ptr;
  nat size = 0, ptrs = 0, nonptrs = 0, vhs = 0;
  char info_type[80], hdr_type[80];

  info_hdr_type(info_ptr, hdr_type);

  // get_itbl(node);
  info_ptr = get_closure_info(node, &size, &ptrs, &nonptrs, &vhs, info_type);
  fprintf(stderr,"%s Info Ptr @0x%lx; Entry: 0x%lx; Size: %lu; Ptrs: %lu\n\n",
                 info_type,info_ptr,(W_) ENTRY_CODE(info_ptr),
	         size, ptrs);
	         // INFO_SIZE(info_ptr),INFO_NoPTRS(info_ptr));

  if (closure_THUNK(node) && !closure_UNPOINTED(node) ) {
    fprintf(stderr,"  RBH InfoPtr: %#lx\n",
	    RBH_INFOPTR(info_ptr));
  }

#if defined(PAR)
  fprintf(stderr,"Enter Flush Entry: 0x%lx;\tExit Flush Entry: 0x%lx\n",INFO_FLUSHENT(info_ptr),INFO_FLUSH(info_ptr));
#endif

#if defined(USE_COST_CENTRES)
  fprintf(stderr,"Cost Centre (?):       0x%lx\n",INFO_CAT(info_ptr));
#endif

#if defined(_INFO_COPYING)
  fprintf(stderr,"Evacuate Entry:    0x%lx;\tScavenge Entry: 0x%lx\n",
          INFO_EVAC_2S(info_ptr),INFO_SCAV_2S(info_ptr));
#endif

#if defined(_INFO_COMPACTING)
  fprintf(stderr,"Scan Link:         0x%lx;\tScan Move:      0x%lx\n",
          (W_) INFO_SCAN_LINK_1S(info_ptr), (W_) INFO_SCAN_MOVE_1S(info_ptr));
  fprintf(stderr,"Mark:              0x%lx;\tMarked:         0x%lx;\t",
          (W_) INFO_MARK_1S(info_ptr), (W_) INFO_MARKED_1S(info_ptr));
#if 0 /* avoid INFO_TYPE */
  if(BASE_INFO_TYPE(info_ptr)==INFO_SPEC_TYPE)
    fprintf(stderr,"plus specialised code\n");
  else
    fprintf(stderr,"Marking:           0x%lx\n",(W_) INFO_MARKING_1S(info_ptr));
#endif /* 0 */
#endif /* _INFO_COMPACTING */
}
#endif /* 0 */

//@cindex G_PRINT_BQ
void
G_PRINT_BQ(node)
StgClosure* node;
{
    StgInfoTable *info;
    StgTSO *tso, *last;
    char str[80], str0[80];

    fprintf(stderr,"\n[PE %d] @ %lu BQ: ",
	            CurrentProc,CurrentTime[CurrentProc]);
    if ( node == (StgClosure*)NULL ) {
      fprintf(stderr," NULL.\n");
      return;
    }
    if ( node == END_TSO_QUEUE ) {
      fprintf(stderr," _|_\n");
      return;
    }
    tso = ((StgBlockingQueue*)node)->blocking_queue;
    while (node != END_TSO_QUEUE) {
      PEs proc;                     
      
      /* Find where the tso lives */
      proc = where_is(node);
      info = get_itbl(node);

      switch (info->type) {
	  case TSO:
	    strcpy(str0,"TSO");
	    break;
	  case BLOCKED_FETCH:
	    strcpy(str0,"BLOCKED_FETCH");
	    break;
	  default:
	    strcpy(str0,"???");
	    break;
	  }

      if(proc == CurrentProc)
	fprintf(stderr," %#lx (%x) L %s,", 
		node, ((StgBlockingQueue*)node)->blocking_queue, str0);
      else
	fprintf(stderr," %#lx (%x) G (PE %d) %s,", 
		node, ((StgBlockingQueue*)node)->blocking_queue, proc, str0);

      last = tso;
      tso = last->link;
    }
    if ( tso == END_TSO_QUEUE ) 
      fprintf(stderr," _|_\n");
}

//@node Threads, Events, Closures, Debugging routines for GranSim and GUM
//@subsection Threads

void
G_CURR_THREADQ(verbose) 
StgInt verbose;
{ 
  fprintf(stderr,"Thread Queue on proc %d: ", CurrentProc);
  G_THREADQ(run_queue_hd, verbose);
}

void 
G_THREADQ(closure, verbose) 
StgTSO* closure;
StgInt verbose;
{
 StgTSO* x;

 fprintf(stderr,"Thread Queue: ");
 for (x=closure; x!=END_TSO_QUEUE; x=x->link)
   if (verbose) 
     G_TSO(x,0);
   else
     fprintf(stderr," %#lx",x);

 if (closure==END_TSO_QUEUE)
   fprintf(stderr,"NIL\n");
 else
   fprintf(stderr,"\n");
}

void 
G_TSO(closure,verbose) 
StgTSO* closure;
StgInt verbose;
{
 
 if (closure==END_TSO_QUEUE) {
   fprintf(stderr,"TSO at %#lx is END_TSO_QUEUE!\n");
   return;
 }

 if ( verbose & 0x08 ) {   /* short info */
   fprintf(stderr,"[TSO @ %#lx, PE %d]: Id: %#lx, Link: %#lx\n",
	   closure,where_is(closure),
	   closure->id,closure->link);
   return;
 }
   
 fprintf(stderr,"TSO at %#lx has the following contents:\n",
                 closure);

 fprintf(stderr,"> Id:   \t%#lx",closure->id);
 // fprintf(stderr,"\tstate: \t%#lx",closure->state);
 fprintf(stderr,"\twhat_next: \t%#lx",closure->what_next);
 fprintf(stderr,"\tlink: \t%#lx\n",closure->link);
 fprintf(stderr,"\twhy_blocked: \t%d", closure->why_blocked);
 fprintf(stderr,"\tblock_info: \t%p\n", closure->block_info);
 // fprintf(stderr,"\tType: \t%s\n",type_name[TSO_TYPE(closure)]);
 fprintf(stderr,">PRI: \t%#lx", closure->gran.pri);
 fprintf(stderr,"\tMAGIC: \t%#lx %s\n", closure->gran.magic, 
	 (closure->gran.magic==TSO_MAGIC ? "it IS a TSO" : "THIS IS NO TSO!!"));
 if ( verbose & 0x04 ) {
   fprintf(stderr, "Stack: stack @ %#lx (stack_size: %u; max_stack_size: %u)\n", 
	   closure->stack, closure->stack_size, closure->max_stack_size);
   fprintf(stderr, "  sp: %#lx, su: %#lx, splim: %#lx\n", 
	   closure->sp, closure->su, closure->splim);
 }
 // fprintf(stderr,"\n");
 if (verbose & 0x01) {
   // fprintf(stderr,"} LOCKED: \t%#lx",closure->locked);
   fprintf(stderr,"} SPARKNAME: \t%#lx\n", closure->gran.sparkname);
   fprintf(stderr,"} STARTEDAT: \t%#lx", closure->gran.startedat);
   fprintf(stderr,"\tEXPORTED: \t%#lx\n", closure->gran.exported);
   fprintf(stderr,"} BASICBLOCKS: \t%#lx", closure->gran.basicblocks);
   fprintf(stderr,"\tALLOCS: \t%#lx\n", closure->gran.allocs);
   fprintf(stderr,"} EXECTIME: \t%#lx", closure->gran.exectime);
   fprintf(stderr,"\tFETCHTIME: \t%#lx\n", closure->gran.fetchtime);
   fprintf(stderr,"} FETCHCOUNT: \t%#lx", closure->gran.fetchcount);
   fprintf(stderr,"\tBLOCKTIME: \t%#lx\n", closure->gran.blocktime);
   fprintf(stderr,"} BLOCKCOUNT: \t%#lx", closure->gran.blockcount);
   fprintf(stderr,"\tBLOCKEDAT: \t%#lx\n", closure->gran.blockedat);
   fprintf(stderr,"} GLOBALSPARKS:\t%#lx", closure->gran.globalsparks);
   fprintf(stderr,"\tLOCALSPARKS:\t%#lx\n", closure->gran.localsparks);
 }
 if ( verbose & 0x02 ) {
   fprintf(stderr,"BQ that starts with this TSO: ");
   G_PRINT_BQ(closure);
 }
}

//@node Events, Sparks, Threads, Debugging routines for GranSim and GUM
//@subsection Events

void 
G_EVENT(event, verbose) 
rtsEventQ event;
StgInt verbose;
{
  if (verbose) {
    print_event(event);
  }else{
    fprintf(stderr," %#lx",event);
  }
}

void
G_EVENTQ(verbose)
StgInt verbose;
{
 extern rtsEventQ EventHd;
 rtsEventQ x;

 fprintf(stderr,"RtsEventQ (hd @%#lx):\n",EventHd);
 for (x=EventHd; x!=NULL; x=x->next) {
   G_EVENT(x,verbose);
 }
 if (EventHd==NULL) 
   fprintf(stderr,"NIL\n");
 else
   fprintf(stderr,"\n");
}

void
G_PE_EQ(pe,verbose)
PEs pe;
StgInt verbose;
{
 extern rtsEventQ EventHd;
 rtsEventQ x;

 fprintf(stderr,"RtsEventQ (hd @%#lx):\n",EventHd);
 for (x=EventHd; x!=NULL; x=x->next) {
   if (x->proc==pe)
     G_EVENT(x,verbose);
 }
 if (EventHd==NULL) 
   fprintf(stderr,"NIL\n");
 else
   fprintf(stderr,"\n");
}

//@node Sparks, Processors, Events, Debugging routines for GranSim and GUM
//@subsection Sparks

void 
G_SPARK(spark, verbose) 
rtsSparkQ spark;
StgInt verbose;
{
 if (spark==(rtsSpark*)NULL) {
   belch("G_SPARK: NULL spark; aborting");
   return;
 }
  if (verbose)
    print_spark(spark);
  else
    fprintf(stderr," %#lx",spark);
}

void 
G_SPARKQ(spark,verbose) 
rtsSparkQ spark;
StgInt verbose;
{
 rtsSparkQ x;

 if (spark==(rtsSpark*)NULL) {
   belch("G_SPARKQ: NULL spark; aborting");
   return;
 }
   
 fprintf(stderr,"RtsSparkQ (hd @%#lx):\n",spark);
 for (x=spark; x!=NULL; x=x->next) {
   G_SPARK(x,verbose);
 }
 if (spark==NULL) 
   fprintf(stderr,"NIL\n");
 else
   fprintf(stderr,"\n");
}

void 
G_CURR_SPARKQ(verbose) 
StgInt verbose;
{
  G_SPARKQ(pending_sparks_hd,verbose);
}

//@node Processors, Shortcuts, Sparks, Debugging routines for GranSim and GUM
//@subsection Processors

void 
G_PROC(proc,verbose)
StgInt proc;
StgInt verbose;
{ 
  extern rtsEventQ EventHd;
  extern char *proc_status_names[];

  fprintf(stderr,"Status of proc %d at time %d (%#lx): %s (%s)\n",
          proc,CurrentTime[proc],CurrentTime[proc],
          (CurrentProc==proc)?"ACTIVE":"INACTIVE",
          proc_status_names[procStatus[proc]]);
  G_THREADQ(run_queue_hds[proc],verbose & 0x2);
  if ( (CurrentProc==proc) )
    G_TSO(CurrentTSO,1);

  if (EventHd!=NULL)
    fprintf(stderr,"Next event (%s) is on proc %d\n",
            event_names[EventHd->evttype],EventHd->proc);

  if (verbose & 0x1) {
    fprintf(stderr,"\nREQUIRED sparks: ");
    G_SPARKQ(pending_sparks_hds[proc],1);
    fprintf(stderr,"\nADVISORY_sparks: ");
    G_SPARKQ(pending_sparks_hds[proc],1);
  }
}

//@node Shortcuts, Printing info type, Processors, Debugging routines for GranSim and GUM
//@subsection Shortcuts

/* Debug Processor */
void 
GP(proc)
StgInt proc;
{ G_PROC(proc,1);
}

/* Debug Current Processor */
void
GCP(){ G_PROC(CurrentProc,2); }

/* Debug TSO */
void
GT(StgPtr tso){ 
  G_TSO(tso,1);
}

/* Debug CurrentTSO */
void
GCT(){ 
  fprintf(stderr,"Current Proc: %d\n",CurrentProc);
  G_TSO(CurrentTSO,1);
}

/* Shorthand for debugging event queue */
void
GEQ() { G_EVENTQ(1); }

/* Shorthand for debugging thread queue of a processor */
void 
GTQ(PEs p) { G_THREADQ(run_queue_hds[p],1); } 

/* Shorthand for debugging thread queue of current processor */
void 
GCTQ() { G_THREADQ(run_queue_hds[CurrentProc],1); } 

/* Shorthand for debugging spark queue of a processor */
void
GSQ(PEs p) { G_SPARKQ(pending_sparks_hds[p],1); }

/* Shorthand for debugging spark queue of current processor */
void
GCSQ() { G_CURR_SPARKQ(1); }

/* Shorthand for printing a node */
void
GN(StgPtr node) { G_PRINT_NODE(node); }

/* Shorthand for printing info table */
#if 0
// ToDo: fix -- HWL
void
GIT(StgPtr node) { G_INFO_TABLE(node); }
#endif

void 
printThreadQPtrs(void)
{
  PEs p;
  for (p=0; p<RtsFlags.GranFlags.proc; p++) {
    fprintf(stderr,", PE %d: (hd=%p,tl=%p)", 
	    run_queue_hds[p], run_queue_tls[p]);
  }
}

void
printThreadQ(StgTSO *tso) { G_THREADQ(tso, 0); };

void
printSparkQ(rtsSpark *spark) { G_SPARKQ(spark, 0); };

void
printThreadQ_verbose(StgTSO *tso) { G_THREADQ(tso, 1); };

void
printSparkQ_verbose(rtsSpark *spark) { G_SPARKQ(spark, 1); };

/* Shorthand for some of ADRs debugging functions */

#endif /* GRAN && GRAN_CHECK*/

#if 0
void
DEBUG_PRINT_NODE(node)
StgPtr node;
{
   W_ info_ptr = INFO_PTR(node);
   StgInt size = 0, ptrs = 0, i, vhs = 0;
   char info_type[80];

   info_hdr_type(info_ptr, info_type);

   size_and_ptrs(node,&size,&ptrs);
   vhs = var_hdr_size(node);

   fprintf(stderr,"Node: 0x%lx", (W_) node);

#if defined(PAR)
   fprintf(stderr," [GA: 0x%lx]",GA(node));
#endif

#if defined(PROFILING)
   fprintf(stderr," [CC: 0x%lx]",CC_HDR(node));
#endif

#if defined(GRAN)
   fprintf(stderr," [Bitmask: 0%lo]",PROCS(node));
#endif

   fprintf(stderr," IP: 0x%lx (%s), size %ld, %ld ptrs\n",
                  info_ptr,info_type,size,ptrs);

   /* For now, we ignore the variable header */

   for(i=0; i < size; ++i)
     {
       if(i == 0)
         fprintf(stderr,"Data: ");

       else if(i % 6 == 0)
         fprintf(stderr,"\n      ");

       if(i < ptrs)
         fprintf(stderr," 0x%lx[P]",*(node+_HS+vhs+i));
       else
         fprintf(stderr," %lu[D]",*(node+_HS+vhs+i));
     }
   fprintf(stderr, "\n");
}


#define INFO_MASK       0x80000000

void
DEBUG_TREE(node)
StgPtr node;
{
  W_ size = 0, ptrs = 0, i, vhs = 0;

  /* Don't print cycles */
  if((INFO_PTR(node) & INFO_MASK) != 0)
    return;

  size_and_ptrs(node,&size,&ptrs);
  vhs = var_hdr_size(node);

  DEBUG_PRINT_NODE(node);
  fprintf(stderr, "\n");

  /* Mark the node -- may be dangerous */
  INFO_PTR(node) |= INFO_MASK;

  for(i = 0; i < ptrs; ++i)
    DEBUG_TREE((StgPtr)node[i+vhs+_HS]);

  /* Unmark the node */
  INFO_PTR(node) &= ~INFO_MASK;
}


void
DEBUG_INFO_TABLE(node)
StgPtr node;
{
  W_ info_ptr = INFO_PTR(node);
  char *iStgPtrtype = info_hdr_type(info_ptr);

  fprintf(stderr,"%s Info Ptr @0x%lx; Entry: 0x%lx; Size: %lu; Ptrs: %lu\n\n",
                 iStgPtrtype,info_ptr,(W_) ENTRY_CODE(info_ptr),INFO_SIZE(info_ptr),INFO_NoPTRS(info_ptr));
#if defined(PAR)
  fprintf(stderr,"Enter Flush Entry: 0x%lx;\tExit Flush Entry: 0x%lx\n",INFO_FLUSHENT(info_ptr),INFO_FLUSH(info_ptr));
#endif

#if defined(PROFILING)
  fprintf(stderr,"Cost Centre (?):       0x%lx\n",INFO_CAT(info_ptr));
#endif

#if defined(_INFO_COPYING)
  fprintf(stderr,"Evacuate Entry:    0x%lx;\tScavenge Entry: 0x%lx\n",
          INFO_EVAC_2S(info_ptr),INFO_SCAV_2S(info_ptr));
#endif

#if defined(_INFO_COMPACTING)
  fprintf(stderr,"Scan Link:         0x%lx;\tScan Move:      0x%lx\n",
          (W_) INFO_SCAN_LINK_1S(info_ptr), (W_) INFO_SCAN_MOVE_1S(info_ptr));
  fprintf(stderr,"Mark:              0x%lx;\tMarked:         0x%lx;\t",
          (W_) INFO_MARK_1S(info_ptr), (W_) INFO_MARKED_1S(info_ptr));
#if 0 /* avoid INFO_TYPE */
  if(BASE_INFO_TYPE(info_ptr)==INFO_SPEC_TYPE)
    fprintf(stderr,"plus specialised code\n");
  else
    fprintf(stderr,"Marking:           0x%lx\n",(W_) INFO_MARKING_1S(info_ptr));
#endif /* 0 */
#endif /* _INFO_COMPACTING */
}
#endif /* 0 */

//@node Printing info type, Printing Packet Contents, Shortcuts, Debugging routines for GranSim and GUM
//@subsection Printing info type

char *
display_info_type(closure, str)
StgClosure *closure;
char *str;
{ 
  strcpy(str,"");
  if ( closure_HNF(closure) )
    strcat(str,"|_HNF ");
  else if ( closure_BITMAP(closure) )
    strcat(str,"|_BTM");
  else if ( !closure_SHOULD_SPARK(closure) )
    strcat(str,"|_NS");
  else if ( closure_STATIC(closure) )
    strcat(str,"|_STA");
  else if ( closure_THUNK(closure) )
    strcat(str,"|_THU");
  else if ( closure_MUTABLE(closure) )
    strcat(str,"|_MUT");
  else if ( closure_UNPOINTED(closure) )
    strcat(str,"|_UPT");
  else if ( closure_SRT(closure) )
    strcat(str,"|_SRT");

  return(str);
}

/*
  PrintPacket is in Pack.c because it makes use of closure queues
*/

#if defined(GRAN) || defined(PAR)

/*
  Print graph rooted at q. The structure of this recursive printing routine
  should be the same as in the graph traversals when packing a graph in
  GUM. Thus, it demonstrates the structure of such a generic graph
  traversal, and in particular, how to extract pointer and non-pointer info
  from the multitude of different heap objects available. 

  {evacuate}Daq ngoqvam nIHlu'pu'!!
*/

void
PrintGraph(StgClosure *p, int indent_level)
{
  void PrintGraph_(StgClosure *p, int indent_level);

  ASSERT(tmpClosureTable==NULL);

  /* init hash table */
  tmpClosureTable = allocHashTable();

  /* now do the real work */
  PrintGraph_(p, indent_level);

  /* nuke hash table */
  freeHashTable(tmpClosureTable, NULL);
  tmpClosureTable = NULL;
}

/*
  This is the actual worker functions. 
  All recursive calls should be made to this function.
*/
void
PrintGraph_(StgClosure *p, int indent_level)
{
  StgPtr x, q;
  rtsBool printed = rtsFalse;
  nat i, j;
  const StgInfoTable *info;
  
  /* check whether we have met this node already to break cycles */
  if (lookupHashTable(tmpClosureTable, (StgWord)p)) { // ie. already touched
    /* indentation */
    for (j=0; j<indent_level; j++)
      fputs(" ", stderr);

    fprintf(stderr, "#### cylce to %p", p);
    return; 
  }

  /* record that we are processing this closure */
  insertHashTable(tmpClosureTable, (StgWord) p, (void *)rtsTrue/*non-NULL*/);

  q = p;			/* save ptr to object */
  
  /* indentation */
  for (j=0; j<indent_level; j++)
    fputs(" ", stderr);

  ASSERT(p!=(StgClosure*)NULL);
  ASSERT(LOOKS_LIKE_STATIC(p) ||
	 LOOKS_LIKE_GHC_INFO(GET_INFO((StgClosure *)p)) ||
         IS_HUGS_CONSTR_INFO(GET_INFO((StgClosure *)p)));

  printClosure(p); // prints contents of this one closure

  /* indentation */
  for (j=0; j<indent_level; j++)
    fputs(" ", stderr);

  info = get_itbl((StgClosure *)p);
  /* the rest of this fct recursively traverses the graph */
  switch (info -> type) {
  
  case BCO:
    {
  	StgBCO* bco = stgCast(StgBCO*,p);
  	nat i;
	fprintf(stderr, "BCO (%p)\n", p);
        /*
  	for (i = 0; i < bco->n_ptrs; i++) {
  	  // bcoConstCPtr(bco,i) = 
	  PrintGraph_(bcoConstCPtr(bco,i), indent_level+1);
  	}
	*/
  	// p += bco_sizeW(bco);
  	break;
    }
  
  case MVAR:
    /* treat MVars specially, because we don't want to PrintGraph the
     * mut_link field in the middle of the closure.
     */
    { 
  	StgMVar *mvar = ((StgMVar *)p);
  	// evac_gen = 0;
	fprintf(stderr, "MVAR (%p) with 3 pointers (head, tail, value)\n", p);
  	// (StgClosure *)mvar->head = 
	PrintGraph_((StgClosure *)mvar->head, indent_level+1);
  	// (StgClosure *)mvar->tail = 
	PrintGraph_((StgClosure *)mvar->tail, indent_level+1);
  	//(StgClosure *)mvar->value = 
	PrintGraph_((StgClosure *)mvar->value, indent_level+1);
  	// p += sizeofW(StgMVar);
  	// evac_gen = saved_evac_gen;
  	break;
    }
  
  case THUNK_2_0:
    if (!printed) {
      fprintf(stderr, "THUNK_2_0 (%p) with 2 pointers\n", p);
      printed = rtsTrue;
    }
  case FUN_2_0:
    if (!printed) {
      fprintf(stderr, "FUN_2_0 (%p) with 2 pointers\n", p);
      printed = rtsTrue;
    }
    // scavenge_srt(info);
  case CONSTR_2_0:
    if (!printed) {
      fprintf(stderr, "CONSTR_2_0 (%p) with 2 pointers\n", p);
      printed = rtsTrue;
    }
    // ((StgClosure *)p)->payload[0] = 
    PrintGraph_(((StgClosure *)p)->payload[0],
	       indent_level+1);
    // ((StgClosure *)p)->payload[1] = 
    PrintGraph_(((StgClosure *)p)->payload[1],
	       indent_level+1);
    // p += sizeofW(StgHeader) + 2;
    break;
  
  case THUNK_1_0:
    // scavenge_srt(info);
    fprintf(stderr, "THUNK_1_0 (%p) with 1 pointer\n", p);
    // ((StgClosure *)p)->payload[0] = 
    PrintGraph_(((StgClosure *)p)->payload[0],
	       indent_level+1);
    // p += sizeofW(StgHeader) + 2; /* MIN_UPD_SIZE */
    break;
  
  case FUN_1_0:
    if (!printed) {
      fprintf(stderr, "FUN_1_0 (%p) with 1 pointer\n", p);
      printed = rtsTrue;
    }
    // scavenge_srt(info);
  case CONSTR_1_0:
    if (!printed) {
      fprintf(stderr, "CONSTR_2_0 (%p) with 2 pointers\n", p);
      printed = rtsTrue;
    }
    // ((StgClosure *)p)->payload[0] = 
    PrintGraph_(((StgClosure *)p)->payload[0],
	       indent_level+1);
    // p += sizeofW(StgHeader) + 1;
    break;
  
  case THUNK_0_1:
    fprintf(stderr, "THUNK_0_1 (%p) with 0 pointers\n", p);
    // scavenge_srt(info);
    // p += sizeofW(StgHeader) + 2; /* MIN_UPD_SIZE */
    break;
  
  case FUN_0_1:
    fprintf(stderr, "FUN_0_1 (%p) with 0 pointers\n", p);
    //scavenge_srt(info);
  case CONSTR_0_1:
    fprintf(stderr, "CONSTR_0_1 (%p) with 0 pointers\n", p);
    //p += sizeofW(StgHeader) + 1;
    break;
  
  case THUNK_0_2:
    if (!printed) {
      fprintf(stderr, "THUNK_0_2 (%p) with 0 pointers\n", p);
      printed = rtsTrue;
    }
  case FUN_0_2:
    if (!printed) {
      fprintf(stderr, "FUN_0_2 (%p) with 0 pointers\n", p);
      printed = rtsTrue;
    }
    // scavenge_srt(info);
  case CONSTR_0_2:
    if (!printed) {
      fprintf(stderr, "CONSTR_0_2 (%p) with 0 pointers\n", p);
      printed = rtsTrue;
    }
    // p += sizeofW(StgHeader) + 2;
    break;
  
  case THUNK_1_1:
    if (!printed) {
      fprintf(stderr, "THUNK_1_1 (%p) with 1 pointer\n", p);
      printed = rtsTrue;
    }
  case FUN_1_1:
    if (!printed) {
      fprintf(stderr, "FUN_1_1 (%p) with 1 pointer\n", p);
      printed = rtsTrue;
    }
    // scavenge_srt(info);
  case CONSTR_1_1:
    if (!printed) {
      fprintf(stderr, "CONSTR_1_1 (%p) with 1 pointer\n", p);
      printed = rtsTrue;
    }
    // ((StgClosure *)p)->payload[0] = 
    PrintGraph_(((StgClosure *)p)->payload[0],
	       indent_level+1);
    // p += sizeofW(StgHeader) + 2;
    break;
  
  case FUN:
    if (!printed) {
      fprintf(stderr, "FUN (%p) with %d pointers\n", p, info->layout.payload.ptrs);
      printed = rtsTrue;
    }
    /* fall through */
  
  case THUNK:
    if (!printed) {
      fprintf(stderr, "THUNK (%p) with %d pointers\n", p, info->layout.payload.ptrs);
      printed = rtsTrue;
    }
    // scavenge_srt(info);
    /* fall through */
  
  case CONSTR:
    if (!printed) {
      fprintf(stderr, "CONSTR (%p) with %d pointers\n", p, info->layout.payload.ptrs);
      printed = rtsTrue;
    }
    /* basically same as loop in STABLE_NAME case  */
    for (i=0; i<info->layout.payload.ptrs; i++)
      PrintGraph_(((StgClosure *)p)->payload[i],
		 indent_level+1);
    break;
    /* NOT fall through */
  
  case WEAK:
    if (!printed) {
      fprintf(stderr, "WEAK (%p) with %d pointers\n", p, info->layout.payload.ptrs);
      printed = rtsTrue;
    }
    /* fall through */
  
  case FOREIGN:
    if (!printed) {
      fprintf(stderr, "FOREIGN (%p) with %d pointers\n", p, info->layout.payload.ptrs);
      printed = rtsTrue;
    }
    /* fall through */
  
  case STABLE_NAME:
    {
      StgPtr end;
      
      if (!printed) {
	fprintf(stderr, "STABLE_NAME (%p) with %d pointers (not followed!)\n", 
		p, info->layout.payload.ptrs);
	printed = rtsTrue;
      }
      end = (StgPtr)((StgClosure *)p)->payload + info->layout.payload.ptrs;
      for (p = (StgPtr)((StgClosure *)p)->payload; p < end; p++) {
	// (StgClosure *)*p = 
	//PrintGraph_((StgClosure *)*p, indent_level+1);
	fprintf(stderr, ", %p", *p); 
      }
      //fputs("\n", stderr);
      // p += info->layout.payload.nptrs;
      break;
    }
  
  case IND_PERM:
    //if (step->gen->no != 0) {
    //	SET_INFO(((StgClosure *)p), &IND_OLDGEN_PERM_info);
    //}
    if (!printed) {
      fprintf(stderr, "IND_PERM (%p) with indirection to\n", 
	      p, ((StgIndOldGen *)p)->indirectee);
      printed = rtsTrue;
    }
    /* fall through */

  case IND_OLDGEN_PERM:
    if (!printed) {
      fprintf(stderr, "IND_OLDGEN_PERM (%p) with indirection to %p\n", 
	      p, ((StgIndOldGen *)p)->indirectee);
      printed = rtsTrue;
    }
    // ((StgIndOldGen *)p)->indirectee = 
    PrintGraph_(((StgIndOldGen *)p)->indirectee,
	       indent_level+1);
    //if (failed_to_evac) {
    //	failed_to_evac = rtsFalse;
    //	recordOldToNewPtrs((StgMutClosure *)p);
    //}
    // p += sizeofW(StgIndOldGen);
    break;
  
  case MUT_VAR:
    /* ignore MUT_CONSs */
    fprintf(stderr, "MUT_VAR (%p) pointing to %p\n", p, ((StgMutVar *)p)->var);
    if (((StgMutVar *)p)->header.info != &stg_MUT_CONS_info) {
      //evac_gen = 0;
      PrintGraph_(((StgMutVar *)p)->var, indent_level+1);
  	//evac_gen = saved_evac_gen;
    }
    //p += sizeofW(StgMutVar);
    break;
  
  case CAF_BLACKHOLE:
    if (!printed) {
      fprintf(stderr, "CAF_BLACKHOLE (%p) with 0 pointers\n", p);
      printed = rtsTrue;
    }
  case SE_CAF_BLACKHOLE:
    if (!printed) {
      fprintf(stderr, "SE_CAF_BLACKHOLE (%p) with 0 pointers\n", p);
      printed = rtsTrue;
    }
  case SE_BLACKHOLE:
    if (!printed) {
      fprintf(stderr, "SE_BLACKHOLE (%p) with 0 pointers\n", p);
      printed = rtsTrue;
    }
  case BLACKHOLE:
    if (!printed) {
      fprintf(stderr, "BLACKHOLE (%p) with 0 pointers\n", p);
      printed = rtsTrue;
    }
    //p += BLACKHOLE_sizeW();
    break;
  
  case BLACKHOLE_BQ:
    { 
      StgBlockingQueue *bh = (StgBlockingQueue *)p;
      // (StgClosure *)bh->blocking_queue = 
      fprintf(stderr, "BLACKHOLE_BQ (%p) pointing to %p\n", 
	      p, (StgClosure *)bh->blocking_queue);
      PrintGraph_((StgClosure *)bh->blocking_queue, indent_level+1);
      //if (failed_to_evac) {
      //  failed_to_evac = rtsFalse;
      //  recordMutable((StgMutClosure *)bh);
      //}
      // p += BLACKHOLE_sizeW();
      break;
    }
  
  case THUNK_SELECTOR:
    { 
      StgSelector *s = (StgSelector *)p;
      fprintf(stderr, "THUNK_SELECTOR (%p) pointing to %p\n", 
	      p, s->selectee);
      PrintGraph_(s->selectee, indent_level+1);
      // p += THUNK_SELECTOR_sizeW();
      break;
    }
  
  case IND:
    fprintf(stderr, "IND (%p) pointing to %p\n", p, ((StgInd*)p)->indirectee);
    PrintGraph_(((StgInd*)p)->indirectee, indent_level+1);
    break;

  case IND_OLDGEN:
    fprintf(stderr, "IND_OLDGEN (%p) pointing to %p\n", 
	    p, ((StgIndOldGen*)p)->indirectee);
    PrintGraph_(((StgIndOldGen*)p)->indirectee, indent_level+1);
    break;
  
  case CONSTR_INTLIKE:
    fprintf(stderr, "CONSTR_INTLIKE (%p) with 0 pointers\n", p);
    break;
  case CONSTR_CHARLIKE:
    fprintf(stderr, "CONSTR_CHARLIKE (%p) with 0 pointers\n", p);
    break;
  case CONSTR_STATIC:
    fprintf(stderr, "CONSTR_STATIC (%p) with 0 pointers\n", p);
    break;
  case CONSTR_NOCAF_STATIC:
    fprintf(stderr, "CONSTR_NOCAF_STATIC (%p) with 0 pointers\n", p);
    break;
  case THUNK_STATIC:
    fprintf(stderr, "THUNK_STATIC (%p) with 0 pointers\n", p);
    break;
  case FUN_STATIC:
    fprintf(stderr, "FUN_STATIC (%p) with 0 pointers\n", p);
    break;
  case IND_STATIC:
    fprintf(stderr, "IND_STATIC (%p) with 0 pointers\n", p);
    break;
  
  case RET_BCO:
    fprintf(stderr, "RET_BCO (%p) with 0 pointers\n", p);
    break;
  case RET_SMALL:
    fprintf(stderr, "RET_SMALL (%p) with 0 pointers\n", p);
    break;
  case RET_VEC_SMALL:
    fprintf(stderr, "RET_VEC_SMALL (%p) with 0 pointers\n", p);
    break;
  case RET_BIG:
    fprintf(stderr, "RET_BIG (%p) with 0 pointers\n", p);
    break;
  case RET_VEC_BIG:
    fprintf(stderr, "RET_VEC_BIG (%p) with 0 pointers\n", p);
    break;
  case RET_DYN:
    fprintf(stderr, "RET_DYN (%p) with 0 pointers\n", p);
    break;
  case UPDATE_FRAME:
    fprintf(stderr, "UPDATE_FRAME (%p) with 0 pointers\n", p);
    break;
  case STOP_FRAME:
    fprintf(stderr, "STOP_FRAME (%p) with 0 pointers\n", p);
    break;
  case CATCH_FRAME:
    fprintf(stderr, "CATCH_FRAME (%p) with 0 pointers\n", p);
    break;
  case SEQ_FRAME:
    fprintf(stderr, "SEQ_FRAME (%p) with 0 pointers\n", p);
    break;
  
  case AP_UPD: /* same as PAPs */
    fprintf(stderr, "AP_UPD (%p) with 0 pointers\n", p);
  case PAP:
    /* Treat a PAP just like a section of stack, not forgetting to
     * PrintGraph_ the function pointer too...
     */
    { 
  	StgPAP* pap = stgCast(StgPAP*,p);
  
	fprintf(stderr, "PAP (%p) pointing to %p\n", p, pap->fun);
  	// pap->fun = 
	//PrintGraph_(pap->fun, indent_level+1);
  	//scavenge_stack((P_)pap->payload, (P_)pap->payload + pap->n_args);
  	//p += pap_sizeW(pap);
  	break;
    }
    
  case ARR_WORDS:
    /* an array of (non-mutable) words */
    fprintf(stderr, "ARR_WORDS (%p) of %d non-ptrs (maybe a string?)\n", 
	    p, ((StgArrWords *)q)->words);
    break;

  case MUT_ARR_PTRS:
    /* follow everything */
    {
  	StgPtr next;
  
	fprintf(stderr, "MUT_ARR_PTRS (%p) with %d pointers (not followed)\n", 
		p, mut_arr_ptrs_sizeW((StgMutArrPtrs*)p));
  	// evac_gen = 0;		/* repeatedly mutable */
  	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
  	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
  	  // (StgClosure *)*p = 
	  // PrintGraph_((StgClosure *)*p, indent_level+1);
	  fprintf(stderr, ", %p", *p); 
  	}
	fputs("\n", stderr);
  	//evac_gen = saved_evac_gen;
  	break;
    }
  
  case MUT_ARR_PTRS_FROZEN:
    /* follow everything */
    {
  	StgPtr start = p, next;
  
	fprintf(stderr, "MUT_ARR_PTRS (%p) with %d pointers (not followed)", 
		p, mut_arr_ptrs_sizeW((StgMutArrPtrs*)p));
  	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
  	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
  	  // (StgClosure *)*p = 
	  // PrintGraph_((StgClosure *)*p, indent_level+1);
	  fprintf(stderr, ", %p", *p); 
  	}
	fputs("\n", stderr);
  	//if (failed_to_evac) {
  	  /* we can do this easier... */
  	//  recordMutable((StgMutClosure *)start);
  	//  failed_to_evac = rtsFalse;
  	//}
  	break;
    }
  
  case TSO:
    { 
  	StgTSO *tso;
  	
  	tso = (StgTSO *)p;
	fprintf(stderr, "TSO (%p) with link field %p\n", p, (StgClosure *)tso->link);
  	// evac_gen = 0;
  	/* chase the link field for any TSOs on the same queue */
  	// (StgClosure *)tso->link = 
	PrintGraph_((StgClosure *)tso->link, indent_level+1);
  	//if (tso->blocked_on) {
  	//  tso->blocked_on = PrintGraph_(tso->blocked_on);
  	//}
  	/* scavenge this thread's stack */
  	//scavenge_stack(tso->sp, &(tso->stack[tso->stack_size]));
  	//evac_gen = saved_evac_gen;
  	//p += tso_sizeW(tso);
  	break;
    }
  
#if defined(GRAN) || defined(PAR)
  case RBH:
    {
    StgInfoTable *rip = REVERT_INFOPTR(get_itbl(p));
    //if (LOOKS_LIKE_GHC_INFO(rip))
    //  fprintf(stderr, "RBH (%p) with 0 pointers (reverted type=%s)\n", 
	//      p, info_type_by_ip(rip)); 
    //else
    fprintf(stderr, "RBH (%p) with 0 pointers (reverted IP=%x)\n", 
	    p, rip); 
    }
    break;
#endif
#if defined(PAR)
  case BLOCKED_FETCH:
    fprintf(stderr, "BLOCKED_FETCH (%p) with 0 pointers (link=%p)\n", 
	    p, ((StgBlockedFetch *)p)->link);
    break;
  case FETCH_ME:
    fprintf(stderr, "FETCH_ME (%p) with 0 pointers\n", p);
    break;
  case FETCH_ME_BQ:
    fprintf(stderr, "FETCH_ME_BQ (%p) with 0 pointers (blocking_queue=%p)\n", 
	    p, ((StgFetchMeBlockingQueue *)p)->blocking_queue);
    break;
#endif
    
#ifdef DIST    
  case REMOTE_REF:
    fprintf(stderr, "REMOTE_REF (%p) with 0 pointers\n", p);
    break;
#endif

  case EVACUATED:
    fprintf(stderr, "EVACUATED (%p) with 0 pointers (evacuee=%p)\n", 
	    p, ((StgEvacuated *)p)->evacuee);
    break;
  
  default:
    barf("PrintGraph_: unknown closure %d (%s)",
	 info -> type, info_type(info));
  }
  
  /* If we didn't manage to promote all the objects pointed to by
   * the current object, then we have to designate this object as
   * mutable (because it contains old-to-new generation pointers).
   */
  //if (failed_to_evac) {
  //  mkMutCons((StgClosure *)q, &generations[evac_gen]);
  //  failed_to_evac = rtsFalse;
  //}
}    

# if defined(PAR)
/*
  Generate a finger-print for a graph.
  A finger-print is a string, with each char representing one node; 
  depth-first traversal
*/

void
GraphFingerPrint(StgClosure *p, char *finger_print)
{
  void GraphFingerPrint_(StgClosure *p, char *finger_print);

  ASSERT(tmpClosureTable==NULL);
  ASSERT(strlen(finger_print)==0);

  /* init hash table */
  tmpClosureTable = allocHashTable();

  /* now do the real work */
  GraphFingerPrint_(p, finger_print);

  /* nuke hash table */
  freeHashTable(tmpClosureTable, NULL);
  tmpClosureTable = NULL;
}

/*
  This is the actual worker functions. 
  All recursive calls should be made to this function.
*/
void
GraphFingerPrint_(StgClosure *p, char *finger_print)
{
  StgPtr x, q;
  rtsBool printed = rtsFalse;
  nat i, j, len;
  const StgInfoTable *info;

  q = p;			/* save ptr to object */
  len = strlen(finger_print);
  ASSERT(len<=MAX_FINGER_PRINT_LEN);
  /* at most 7 chars for this node (I think) */
  if (len+7>=MAX_FINGER_PRINT_LEN)
    return;

  /* check whether we have met this node already to break cycles */
  if (lookupHashTable(tmpClosureTable, (StgWord)p)) { // ie. already touched
    strcat(finger_print, "#");
    return; 
  }

  /* record that we are processing this closure */
  insertHashTable(tmpClosureTable, (StgWord) p, (void *)rtsTrue/*non-NULL*/);

  ASSERT(p!=(StgClosure*)NULL);
  ASSERT(LOOKS_LIKE_STATIC(p) ||
	 LOOKS_LIKE_GHC_INFO(GET_INFO((StgClosure *)p)) ||
         IS_HUGS_CONSTR_INFO(GET_INFO((StgClosure *)p)));

  info = get_itbl((StgClosure *)p);
  // append char for this node
  finger_print[len] = finger_print_char[info->type]; finger_print[len+1] = '\0'; 
  /* the rest of this fct recursively traverses the graph */
  switch (info -> type) {
  
  case BCO:
    {
  	StgBCO* bco = stgCast(StgBCO*,p);
  	nat i;
	//%% fprintf(stderr, "BCO (%p) with %d pointers\n", p, bco->n_ptrs);
        /*
  	for (i = 0; i < bco->n_ptrs; i++) {
  	  // bcoConstCPtr(bco,i) = 
	  GraphFingerPrint_(bcoConstCPtr(bco,i), finger_print);
  	}
	*/
  	// p += bco_sizeW(bco);
  	break;
    }
  
  case MVAR:
    break;
  
  case THUNK_2_0:
  case FUN_2_0:
  case CONSTR_2_0:
    // append char for this node
    strcat(finger_print, "22(");
    GraphFingerPrint_(((StgClosure *)p)->payload[0], finger_print);
    GraphFingerPrint_(((StgClosure *)p)->payload[1], finger_print);
    if (strlen(finger_print)+2<MAX_FINGER_PRINT_LEN)
      strcat(finger_print, ")");
    break;
  
  case THUNK_1_0:
  case FUN_1_0:
  case CONSTR_1_0:
    // append char for this node
    strcat(finger_print, "12(");
    GraphFingerPrint_(((StgClosure *)p)->payload[0], finger_print);
    if (strlen(finger_print)+2<MAX_FINGER_PRINT_LEN)
      strcat(finger_print, ")");
    break;
  
  case THUNK_0_1:
  case FUN_0_1:
  case CONSTR_0_1:
    // append char for this node
    strcat(finger_print, "01");
    break;
  
  case THUNK_0_2:
  case FUN_0_2:
  case CONSTR_0_2:
    // append char for this node
    strcat(finger_print, "02");
    break;
  
  case THUNK_1_1:
  case FUN_1_1:
  case CONSTR_1_1:
    // append char for this node
    strcat(finger_print, "11(");
    GraphFingerPrint_(((StgClosure *)p)->payload[0], finger_print);
    if (strlen(finger_print)+2<MAX_FINGER_PRINT_LEN)
      strcat(finger_print, ")");
    break;
  
  case FUN:
  case THUNK:
  case CONSTR:
    /* basically same as loop in STABLE_NAME case  */
    {
	char str[6];
	sprintf(str,"%d?(",info->layout.payload.ptrs);
	strcat(finger_print,str); 
	for (i=0; i<info->layout.payload.ptrs; i++)
	  GraphFingerPrint_(((StgClosure *)p)->payload[i], finger_print);
	if (strlen(finger_print)+2<MAX_FINGER_PRINT_LEN)
	  strcat(finger_print, ")");
    }
    break;
  
  case WEAK:
  case FOREIGN:
  case STABLE_NAME:
    {
      StgPtr end;
      char str[6];
      sprintf(str,"%d?", info->layout.payload.ptrs);
      strcat(finger_print,str); 

	//end = (StgPtr)((StgClosure *)p)->payload + info->layout.payload.ptrs;
      //for (p = (StgPtr)((StgClosure *)p)->payload; p < end; p++) {
      // GraphFingerPrint_((StgClosure *)*p, finger_print);
      //}
      break;
    }
  
  case IND_PERM:
  case IND_OLDGEN_PERM:
    GraphFingerPrint_(((StgIndOldGen *)p)->indirectee, finger_print);
    break;
  
  case MUT_VAR:
    /* ignore MUT_CONSs */
    if (((StgMutVar *)p)->header.info != &stg_MUT_CONS_info) {
      GraphFingerPrint_(((StgMutVar *)p)->var, finger_print);
    }
    break;
  
  case CAF_BLACKHOLE:
  case SE_CAF_BLACKHOLE:
  case SE_BLACKHOLE:
  case BLACKHOLE:
    break;
  
  case BLACKHOLE_BQ:
    { 
      StgBlockingQueue *bh = (StgBlockingQueue *)p;
      // GraphFingerPrint_((StgClosure *)bh->blocking_queue, finger_print);
      break;
    }
  
  case THUNK_SELECTOR:
    { 
      StgSelector *s = (StgSelector *)p;
      GraphFingerPrint_(s->selectee, finger_print);
      break;
    }
  
  case IND:
    GraphFingerPrint_(((StgInd*)p)->indirectee, finger_print);
    break;

  case IND_OLDGEN:
    GraphFingerPrint_(((StgIndOldGen*)p)->indirectee, finger_print);
    break;

  case IND_STATIC:
    GraphFingerPrint_(((StgIndOldGen*)p)->indirectee, finger_print);
    break;
  
  case CONSTR_INTLIKE:
  case CONSTR_CHARLIKE:
  case CONSTR_STATIC:
  case CONSTR_NOCAF_STATIC:
  case THUNK_STATIC:
  case FUN_STATIC:
    break;
  
  case RET_BCO:
  case RET_SMALL:
  case RET_VEC_SMALL:
  case RET_BIG:
  case RET_VEC_BIG:
  case RET_DYN:
  case UPDATE_FRAME:
  case STOP_FRAME:
  case CATCH_FRAME:
  case SEQ_FRAME:
    break;
  
  case AP_UPD: /* same as PAPs */
  case PAP:
    /* Treat a PAP just like a section of stack, not forgetting to
     * GraphFingerPrint_ the function pointer too...
     */
    { 
  	StgPAP* pap = stgCast(StgPAP*,p);
	char str[6];
	sprintf(str,"%d",pap->n_args);
	strcat(finger_print,str); 
	//GraphFingerPrint_(pap->fun, finger_print); // ??
  	break;
    }
    
  case ARR_WORDS:
    {
	char str[6];
	sprintf(str,"%d",((StgArrWords*)p)->words);
	strcat(finger_print,str); 
    }
    break;

  case MUT_ARR_PTRS:
    /* follow everything */
    {
	char str[6];
	sprintf(str,"%d",((StgMutArrPtrs*)p)->ptrs);
	strcat(finger_print,str); 
    }
    {
  	StgPtr next;
  	//next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
  	//for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	//  GraphFingerPrint_((StgClosure *)*p, finger_print);
  	//}
  	break;
    }
  
  case MUT_ARR_PTRS_FROZEN:
    /* follow everything */
    {
	char str[6];
	sprintf(str,"%d",((StgMutArrPtrs*)p)->ptrs);
	strcat(finger_print,str); 
    }
    {
  	StgPtr start = p, next;
  	//next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
  	//for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	//  GraphFingerPrint_((StgClosure *)*p, finger_print);
  	//}
  	break;
    }
  
  case TSO:
    { 
      StgTSO *tso = (StgTSO *)p;
      char str[6];
      sprintf(str,"%d",tso->id);
      strcat(finger_print,str); 
    }
    //GraphFingerPrint_((StgClosure *)tso->link, indent_level+1);
    break;
  
#if defined(GRAN) || defined(PAR)
  case RBH:
    {
      // use this
      // StgInfoTable *rip = REVERT_INFOPTR(get_itbl(p));
    }
    break;
#endif
#if defined(PAR)
  case BLOCKED_FETCH:
    break;
  case FETCH_ME:
    break;
  case FETCH_ME_BQ:
    break;
#endif
#ifdef DIST    
  case REMOTE_REF:
    break;
#endif
  case EVACUATED:
    break;
  
  default:
    barf("GraphFingerPrint_: unknown closure %d (%s)",
	 info -> type, info_type(info));
  }
 
}    
# endif /* PAR */

/*
  Do a sanity check on the whole graph, down to a recursion level of level.
  Same structure as PrintGraph (nona).
*/
void
checkGraph(StgClosure *p, int rec_level)
{
  StgPtr x, q;
  nat i, j;
  const StgInfoTable *info;
  
  if (rec_level==0)
    return;

  q = p;			/* save ptr to object */

  /* First, the obvious generic checks */
  ASSERT(p!=(StgClosure*)NULL);
  checkClosure(p);              /* see Sanity.c for what's actually checked */

  info = get_itbl((StgClosure *)p);
  /* the rest of this fct recursively traverses the graph */
  switch (info -> type) {
  
  case BCO:
    {
  	StgBCO* bco = stgCast(StgBCO*,p);
  	nat i;
        /*
  	for (i = 0; i < bco->n_ptrs; i++) {
	  checkGraph(bcoConstCPtr(bco,i), rec_level-1);
  	}
	*/
  	break;
    }
  
  case MVAR:
    /* treat MVars specially, because we don't want to PrintGraph the
     * mut_link field in the middle of the closure.
     */
    { 
  	StgMVar *mvar = ((StgMVar *)p);
	checkGraph((StgClosure *)mvar->head, rec_level-1);
	checkGraph((StgClosure *)mvar->tail, rec_level-1);
	checkGraph((StgClosure *)mvar->value, rec_level-1);
  	break;
    }
  
  case THUNK_2_0:
  case FUN_2_0:
  case CONSTR_2_0:
    checkGraph(((StgClosure *)p)->payload[0], rec_level-1);
    checkGraph(((StgClosure *)p)->payload[1], rec_level-1);
    break;
  
  case THUNK_1_0:
    checkGraph(((StgClosure *)p)->payload[0], rec_level-1);
    break;
  
  case FUN_1_0:
  case CONSTR_1_0:
    checkGraph(((StgClosure *)p)->payload[0], rec_level-1);
    break;
  
  case THUNK_0_1:
    break;
  
  case FUN_0_1:
  case CONSTR_0_1:
    break;
  
  case THUNK_0_2:
  case FUN_0_2:
  case CONSTR_0_2:
    break;
  
  case THUNK_1_1:
  case FUN_1_1:
  case CONSTR_1_1:
    checkGraph(((StgClosure *)p)->payload[0], rec_level-1);
    break;
  
  case FUN:
  case THUNK:
  case CONSTR:
    for (i=0; i<info->layout.payload.ptrs; i++)
      checkGraph(((StgClosure *)p)->payload[i], rec_level-1);
    break;
  
  case WEAK:
  case FOREIGN:
  case STABLE_NAME:
    {
      StgPtr end;
      
      end = (StgPtr)((StgClosure *)p)->payload + info->layout.payload.ptrs;
      for (p = (StgPtr)((StgClosure *)p)->payload; p < end; p++) {
	checkGraph(*(StgClosure **)p, rec_level-1);
      }
      break;
    }
  
  case IND_PERM:
  case IND_OLDGEN_PERM:
    checkGraph(((StgIndOldGen *)p)->indirectee, rec_level-1);
    break;
  
  case MUT_VAR:
    /* ignore MUT_CONSs */
    if (((StgMutVar *)p)->header.info != &stg_MUT_CONS_info) {
      checkGraph(((StgMutVar *)p)->var, rec_level-1);
    }
    break;
  
  case CAF_BLACKHOLE:
  case SE_CAF_BLACKHOLE:
  case SE_BLACKHOLE:
  case BLACKHOLE:
    break;
  
  case BLACKHOLE_BQ:
    break;
  
  case THUNK_SELECTOR:
    { 
      StgSelector *s = (StgSelector *)p;
      checkGraph(s->selectee, rec_level-1);
      break;
    }
  
  case IND:
    checkGraph(((StgInd*)p)->indirectee, rec_level-1);
    break;

  case IND_OLDGEN:
    checkGraph(((StgIndOldGen*)p)->indirectee, rec_level-1);
    break;
  
  case CONSTR_INTLIKE:
    break;
  case CONSTR_CHARLIKE:
    break;
  case CONSTR_STATIC:
    break;
  case CONSTR_NOCAF_STATIC:
    break;
  case THUNK_STATIC:
    break;
  case FUN_STATIC:
    break;
  case IND_STATIC:
    break;
  
  case RET_BCO:
    break;
  case RET_SMALL:
    break;
  case RET_VEC_SMALL:
    break;
  case RET_BIG:
    break;
  case RET_VEC_BIG:
    break;
  case RET_DYN:
    break;
  case UPDATE_FRAME:
    break;
  case STOP_FRAME:
    break;
  case CATCH_FRAME:
    break;
  case SEQ_FRAME:
    break;
  
  case AP_UPD: /* same as PAPs */
  case PAP:
    /* Treat a PAP just like a section of stack, not forgetting to
     * checkGraph the function pointer too...
     */
    { 
  	StgPAP* pap = stgCast(StgPAP*,p);
  
	checkGraph(pap->fun, rec_level-1);
  	break;
    }
    
  case ARR_WORDS:
    break;

  case MUT_ARR_PTRS:
    /* follow everything */
    {
  	StgPtr next;
  
  	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
  	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	  checkGraph(*(StgClosure **)p, rec_level-1);
  	}
  	break;
    }
  
  case MUT_ARR_PTRS_FROZEN:
    /* follow everything */
    {
  	StgPtr start = p, next;
  
  	next = p + mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
  	for (p = (P_)((StgMutArrPtrs *)p)->payload; p < next; p++) {
	  checkGraph(*(StgClosure **)p, rec_level-1);
  	}
  	break;
    }
  
  case TSO:
    { 
  	StgTSO *tso;
  	
  	tso = (StgTSO *)p;
	checkGraph((StgClosure *)tso->link, rec_level-1);
  	break;
    }
  
#if defined(GRAN) || defined(PAR)
  case RBH:
    break;
#endif
#if defined(PAR)
  case BLOCKED_FETCH:
    break;
  case FETCH_ME:
    break;
  case FETCH_ME_BQ:
    break;
#endif
  case EVACUATED:
    barf("checkGraph: found EVACUATED closure %p (%s)",
	 p, info_type(p));
    break;
  
  default:
  }
}    

#endif /* GRAN */

#endif /* GRAN || PAR */

//@node End of File,  , Printing Packet Contents, Debugging routines for GranSim and GUM
//@subsection End of File
