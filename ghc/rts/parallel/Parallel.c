/*
  Time-stamp: <Thu Mar 23 2000 18:20:17 Stardate: [-30]4548.82 hwloidl>

  Basic functions for use in either GranSim or GUM.
*/

#if defined(GRAN) || defined(PAR)                              /* whole file */

//@menu
//* Includes::			
//* Variables and constants::	
//* Writing to the log-file::	
//* Dumping routines::		
//@end menu

//@node Includes, Variables and constants
//@subsection Includes

#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "GranSimRts.h"
#include "ParallelRts.h"

//@node Variables and constants, Writing to the log-file, Includes
//@subsection Variables and constants

/* Where to write the log file */
FILE *gr_file = NULL;
char gr_filename[STATS_FILENAME_MAXLEN];

//@node Writing to the log-file, Dumping routines, Variables and constants
//@subsection Writing to the log-file
/*
  Writing to the log-file

  These routines dump event-based info to the main log-file.
  The code for writing log files is shared between GranSim and GUM.
*/

/* 
 * If you're not using GNUC and you're on a 32-bit machine, you're 
 * probably out of luck here.  However, since CONCURRENT currently
 * requires GNUC, I'm not too worried about it.  --JSM
 */

//@cindex init_gr_simulation
#if defined(GRAN)
void
init_gr_simulation(rts_argc, rts_argv, prog_argc, prog_argv)
char *prog_argv[], *rts_argv[];
int prog_argc, rts_argc;
{
  nat i;
  char *extension = RtsFlags.GranFlags.GranSimStats.Binary ? "gb" : "gr";

  if (RtsFlags.GranFlags.GranSimStats.Global)
    init_gr_stats();

  /* init global constants for costs of basic operations */
  gran_arith_cost = RtsFlags.GranFlags.Costs.arith_cost;
  gran_branch_cost = RtsFlags.GranFlags.Costs.branch_cost;
  gran_load_cost = RtsFlags.GranFlags.Costs.load_cost;
  gran_store_cost = RtsFlags.GranFlags.Costs.store_cost;
  gran_float_cost = RtsFlags.GranFlags.Costs.float_cost;

  if (RtsFlags.GranFlags.GranSimStats.Suppressed)
    return;

  if (!RtsFlags.GranFlags.GranSimStats.Full) 
    return;

  sprintf(gr_filename, GR_FILENAME_FMT, prog_argv[0], extension);

  if ((gr_file = fopen(gr_filename, "w")) == NULL) {
    barf("Can't open granularity simulation report file %s\n", 
	 gr_filename);
  }

  setbuf(gr_file, NULL); // for debugging turn buffering off

  /* write header with program name, options and setup to gr_file */
  fputs("Granularity Simulation for ", gr_file);
  for (i = 0; i < prog_argc; ++i) {
    fputs(prog_argv[i], gr_file);
    fputc(' ', gr_file);
  }

  if (rts_argc > 0) {
    fputs("+RTS ", gr_file);
    
    for (i = 0; i < rts_argc; ++i) {
      fputs(rts_argv[i], gr_file);
      fputc(' ', gr_file);
    }
  }

  fputs("\nStart time: ", gr_file);
  fputs(time_str(), gr_file);               /* defined in RtsUtils.c */
  fputc('\n', gr_file);
    
  fputs("\n\n--------------------\n\n", gr_file);

  fputs("General Parameters:\n\n", gr_file);

  if (RtsFlags.GranFlags.Light) 
    fprintf(gr_file, "GrAnSim-Light\nPEs infinite, %s Scheduler, %sMigrate Threads %s, %s\n",
	    RtsFlags.GranFlags.DoFairSchedule?"Fair":"Unfair",
	    RtsFlags.GranFlags.DoThreadMigration?"":"Don't ",
	    RtsFlags.GranFlags.DoThreadMigration && RtsFlags.GranFlags.DoStealThreadsFirst?" Before Sparks":"",
	    RtsFlags.GranFlags.DoAsyncFetch ? "Asynchronous Fetch" :
	    "Block on Fetch");
  else 
    fprintf(gr_file, "PEs %u, %s Scheduler, %sMigrate Threads %s, %s\n",
	    RtsFlags.GranFlags.proc,RtsFlags.GranFlags.DoFairSchedule?"Fair":"Unfair",
	    RtsFlags.GranFlags.DoThreadMigration?"":"Don't ",
	    RtsFlags.GranFlags.DoThreadMigration && RtsFlags.GranFlags.DoStealThreadsFirst?" Before Sparks":"",
	    RtsFlags.GranFlags.DoAsyncFetch ? "Asynchronous Fetch" :
	    "Block on Fetch");
  
  if (RtsFlags.GranFlags.DoBulkFetching) 
    if (RtsFlags.GranFlags.ThunksToPack)
      fprintf(gr_file, "Bulk Fetching: Fetch %d Thunks in Each Packet (Packet Size = %d closures)\n",
	      RtsFlags.GranFlags.ThunksToPack, 
	      RtsFlags.GranFlags.packBufferSize);
    else
      fprintf(gr_file, "Bulk Fetching: Fetch as many closures as possible (Packet Size = %d closures)\n",
	      RtsFlags.GranFlags.packBufferSize);
  else
    fprintf(gr_file, "Incremental Fetching: Fetch Exactly One Closure in Each Packet\n");
  
  fprintf(gr_file, "Fetch Strategy(%u):If outstanding fetches %s\n",
	  RtsFlags.GranFlags.FetchStrategy,
	  RtsFlags.GranFlags.FetchStrategy==0 ?
	    " block (block-on-fetch)":
	  RtsFlags.GranFlags.FetchStrategy==1 ?
	    "only run runnable threads":
	  RtsFlags.GranFlags.FetchStrategy==2 ? 
	    "create threads only from local sparks":
	  RtsFlags.GranFlags.FetchStrategy==3 ? 
	    "create threads from local or global sparks":
	  RtsFlags.GranFlags.FetchStrategy==4 ?
	    "create sparks and steal threads if necessary":
	  "unknown");

  if (RtsFlags.GranFlags.DoPrioritySparking)
    fprintf(gr_file, "Priority Sparking (i.e. keep sparks ordered by priority)\n");

  if (RtsFlags.GranFlags.DoPriorityScheduling)
    fprintf(gr_file, "Priority Scheduling (i.e. keep threads ordered by priority)\n");

  fprintf(gr_file, "Thread Creation Time %u, Thread Queue Time %u\n",
	  RtsFlags.GranFlags.Costs.threadcreatetime, 
	  RtsFlags.GranFlags.Costs.threadqueuetime);
  fprintf(gr_file, "Thread DeSchedule Time %u, Thread Schedule Time %u\n",
	  RtsFlags.GranFlags.Costs.threaddescheduletime, 
	  RtsFlags.GranFlags.Costs.threadscheduletime);
  fprintf(gr_file, "Thread Context-Switch Time %u\n",
	  RtsFlags.GranFlags.Costs.threadcontextswitchtime);
  fputs("\n\n--------------------\n\n", gr_file);

  fputs("Communication Metrics:\n\n", gr_file);
  fprintf(gr_file,
	  "Latency %u (1st) %u (rest), Fetch %u, Notify %u (Global) %u (Local)\n",
	  RtsFlags.GranFlags.Costs.latency, 
	  RtsFlags.GranFlags.Costs.additional_latency, 
	  RtsFlags.GranFlags.Costs.fetchtime,
	  RtsFlags.GranFlags.Costs.gunblocktime, 
	  RtsFlags.GranFlags.Costs.lunblocktime);
  fprintf(gr_file,
	  "Message Creation %u (+ %u after send), Message Read %u\n",
	  RtsFlags.GranFlags.Costs.mpacktime, 
	  RtsFlags.GranFlags.Costs.mtidytime, 
	  RtsFlags.GranFlags.Costs.munpacktime);
  fputs("\n\n--------------------\n\n", gr_file);

  fputs("Instruction Metrics:\n\n", gr_file);
  fprintf(gr_file, "Arith %u, Branch %u, Load %u, Store %u, Float %u, Alloc %u\n",
	  RtsFlags.GranFlags.Costs.arith_cost, 
	  RtsFlags.GranFlags.Costs.branch_cost,
	  RtsFlags.GranFlags.Costs.load_cost, 
	  RtsFlags.GranFlags.Costs.store_cost, 
	  RtsFlags.GranFlags.Costs.float_cost, 
	  RtsFlags.GranFlags.Costs.heapalloc_cost);
  fputs("\n\n++++++++++++++++++++\n\n", gr_file);

# if 0
  /* binary log files are currently not supported */
  if (RtsFlags.GranFlags.GranSimStats.Binary)
    grputw(sizeof(rtsTime));
# endif

  return (0);
}

#elif defined(PAR)

void
init_gr_simulation(rts_argc, rts_argv, prog_argc, prog_argv)
char *prog_argv[], *rts_argv[];
int prog_argc, rts_argc;
{
  nat i;
  char time_string[TIME_STR_LEN], node_str[NODE_STR_LEN];
  char *extension = RtsFlags.ParFlags.ParStats.Binary ? "gb" : "gr";

  sprintf(gr_filename, GR_FILENAME_FMT_GUM, prog_argv[0], thisPE, extension);

  if (!RtsFlags.ParFlags.ParStats.Full) 
    return;

  if ((gr_file = fopen(gr_filename, "w")) == NULL)
    barf("Can't open activity report file %s\n", gr_filename);

  setbuf(gr_file, NULL); // for debugging turn buffering off

  /* write header with program name, options and setup to gr_file */
  for (i = 0; i < prog_argc; ++i) {
    fputs(prog_argv[i], gr_file);
    fputc(' ', gr_file);
  }

  if (rts_argc > 0) {
    fputs("+RTS ", gr_file);
    
    for (i = 0; i < rts_argc; ++i) {
      fputs(rts_argv[i], gr_file);
      fputc(' ', gr_file);
    }
  }
  fputc('\n', gr_file);

  /* record the absolute start time to allow synchronisation of log-files */
  fputs("Start-Time: ", gr_file);
  fputs(time_str(), gr_file);
  fputc('\n', gr_file);
    
  startTime = CURRENT_TIME;
  ullong_format_string(CURRENT_TIME, time_string, rtsFalse/*no commas!*/);
  fprintf(gr_file, "PE %2u [%s]: TIME\n", thisPE, time_string);

# if 0
    ngoq Dogh'q' vImuS
  IF_PAR_DEBUG(verbose,
	       belch("== Start-time: %ld (%s)",
		     startTime, time_string));

    if (startTime > LL(1000000000)) {
      fprintf(gr_file, "PE %2u [%lu%lu]: TIME\n", thisPE, 
	    (rtsTime) (startTime / LL(1000000000)),
	    (rtsTime) (startTime % LL(1000000000)));
    } else {
      fprintf(gr_file, "PE %2u [%lu]: TIME\n", thisPE, (TIME) startTime);
    } 
    /* binary log files are currently not supported */
    if (RtsFlags.GranFlags.GranSimStats.Binary)
	grputw(sizeof(rtsTime));
# endif

    return;
}
#endif /* PAR */

//@cindex end_gr_simulation
#if defined(GRAN)
void
end_gr_simulation(void)
{
   char time_string[TIME_STR_LEN];

   ullong_format_string(CURRENT_TIME, time_string, rtsFalse/*no commas!*/);

   if (RtsFlags.GranFlags.GranSimStats.Suppressed)
     return;

   /* Print event stats */
   if (RtsFlags.GranFlags.GranSimStats.Global) {
     nat i;
   
     fprintf(stderr,"Total yields: %d\n",
             globalGranStats.tot_yields);

     fprintf(stderr,"Total number of threads created: %d ; per PE:\n",
             globalGranStats.tot_threads_created);
     for (i=0; i<RtsFlags.GranFlags.proc; i++) {
       fprintf(stderr,"  PE %d: %d\t", 
	       i, globalGranStats.threads_created_on_PE[i]);
       if (i+1 % 4 == 0) fprintf(stderr,"\n");
     }
     if (RtsFlags.GranFlags.proc+1 % 4 != 0) fprintf(stderr,"\n");
     fprintf(stderr,"Total number of threads migrated: %d\n",
             globalGranStats.tot_TSOs_migrated);

     fprintf(stderr,"Total number of sparks created: %d ; per PE:\n",
             globalGranStats.tot_sparks_created);
     for (i=0; i<RtsFlags.GranFlags.proc; i++) {
       fprintf(stderr,"  PE %d: %d\t", 
	       i, globalGranStats.sparks_created_on_PE[i]);
       if (i+1 % 4 == 0) fprintf(stderr,"\n");
     }
     if (RtsFlags.GranFlags.proc+1 % 4 != 0) fprintf(stderr,"\n");

     fprintf(stderr,"Event statistics (number of events: %d):\n",
             globalGranStats.noOfEvents);
     for (i=0; i<=MAX_EVENT; i++) {
       fprintf(stderr,"  %s (%d): \t%d \t%f%%\t%f%%\n",
               event_names[i],i,globalGranStats.event_counts[i],
               (float)(100*globalGranStats.event_counts[i])/(float)(globalGranStats.noOfEvents),
               (i==ContinueThread ? 0.0 :
   		   (float)(100*(globalGranStats.event_counts[i])/(float)(globalGranStats.noOfEvents-globalGranStats.event_counts[ContinueThread])) ));
     }
     fprintf(stderr,"Randomized steals: %ld sparks, %ld threads \n \t(Sparks: #%u (avg ntimes=%f; avg fl=%f)\n\t(Threads: %ld)", 
   	             globalGranStats.rs_sp_count, 
	             globalGranStats.rs_t_count, 
	             globalGranStats.no_of_steals, 
   	             (float)globalGranStats.ntimes_total/(float)stg_max(globalGranStats.no_of_steals,1),
   	             (float)globalGranStats.fl_total/(float)stg_max(globalGranStats.no_of_steals,1),
	             globalGranStats.no_of_migrates);
     fprintf(stderr,"Moved sparks: %d  Withered sparks: %d (%.2f %%)\n",
   	      globalGranStats.tot_sparks, globalGranStats.withered_sparks,
             ( globalGranStats.tot_sparks == 0 ? 0 :
                  (float)(100*globalGranStats.withered_sparks)/(float)(globalGranStats.tot_sparks)) );
     /* Print statistics about priority sparking */
     if (RtsFlags.GranFlags.DoPrioritySparking) {
   	fprintf(stderr,"About Priority Sparking:\n");
   	fprintf(stderr,"  Total no. NewThreads: %d   Avg. spark queue len: %.2f \n", globalGranStats.tot_sq_probes, (float)globalGranStats.tot_sq_len/(float)globalGranStats.tot_sq_probes);
     }
     /* Print statistics about priority sparking */
     if (RtsFlags.GranFlags.DoPriorityScheduling) {
   	fprintf(stderr,"About Priority Scheduling:\n");
   	fprintf(stderr,"  Total no. of StartThreads: %d (non-end: %d) Avg. thread queue len: %.2f\n", 
   		globalGranStats.tot_add_threads, globalGranStats.non_end_add_threads, 
   		(float)globalGranStats.tot_tq_len/(float)globalGranStats.tot_add_threads);
     }
     /* Blocking queue statistics */
     if (1) {
   	fprintf(stderr,"Blocking queue statistcs:\n");
   	fprintf(stderr,"  Total no. of FMBQs generated: %d\n",
		globalGranStats.tot_FMBQs);
   	fprintf(stderr,"  Total no. of bqs awakened: %d\n",
		globalGranStats.tot_awbq);
   	fprintf(stderr,"  Total length of all bqs: %d\tAvg length of bqs: %.2f\n",
		globalGranStats.tot_bq_len, (float)globalGranStats.tot_bq_len/(float)globalGranStats.tot_awbq);
   	fprintf(stderr,"  Percentage of local TSOs in BQs: %.2f\n",
		(float)globalGranStats.tot_bq_len*100.0/(float)globalGranStats.tot_bq_len);
   	fprintf(stderr,"  Total time spent processing BQs: %lx\n",
		globalGranStats.tot_bq_processing_time);
     }

     /* Fetch misses and thunk stealing */
     fprintf(stderr,"Number of fetch misses: %d\n", 
	     globalGranStats.fetch_misses);

     /* Print packet statistics if GUMM fetching is turned on */
     if (RtsFlags.GranFlags.DoBulkFetching) {
   	fprintf(stderr,"Packet statistcs:\n");
   	fprintf(stderr,"  Total no. of packets: %d   Avg. packet size: %.2f \n", globalGranStats.tot_packets, (float)globalGranStats.tot_packet_size/(float)globalGranStats.tot_packets);
   	fprintf(stderr,"  Total no. of thunks: %d   Avg. thunks/packet: %.2f \n", globalGranStats.tot_thunks, (float)globalGranStats.tot_thunks/(float)globalGranStats.tot_packets);
   	fprintf(stderr,"  Total no. of cuts: %d   Avg. cuts/packet: %.2f\n", globalGranStats.tot_cuts, (float)globalGranStats.tot_cuts/(float)globalGranStats.tot_packets);
        /* 
   	if (closure_queue_overflows>0) 
   	  fprintf(stderr,"  Number of closure queue overflows: %u\n",
   		  closure_queue_overflows);
	*/
     }
   } /* RtsFlags.GranFlags.GranSimStats.Global */

#  if defined(GRAN_COUNT)
#  error "GRAN_COUNT not supported; should be parallel ticky profiling, really"
    fprintf(stderr,"Update count statistics:\n");
    fprintf(stderr,"  Total number of updates: %u\n",nUPDs);
    fprintf(stderr,"  Needed to awaken BQ: %u with avg BQ len of: %f\n",
	    nUPDs_BQ,(float)BQ_lens/(float)nUPDs_BQ);
    fprintf(stderr,"  Number of PAPs: %u\n",nPAPs);
#  endif

    fprintf(stderr, "Simulation finished after @ %s @ cycles. %d sparks created, %d sparks ignored. Check %s for details.\n",
	    time_string, sparksCreated, sparksIgnored, gr_filename);

    if (RtsFlags.GranFlags.GranSimStats.Full) 
      fclose(gr_file);
}

#elif defined(PAR)

/*
  Under GUM we print only one line. 
*/
void
end_gr_simulation(void)
{
  char time_string[TIME_STR_LEN];

  ullong_format_string(CURRENT_TIME-startTime, time_string, rtsFalse/*no commas!*/);

  fprintf(stderr, "Computation finished after @ %s @ ms. %d sparks created, %d sparks ignored. Check %s for details.\n",
	    time_string, sparksCreated, sparksIgnored, gr_filename);

  if (RtsFlags.ParFlags.ParStats.Full) 
    fclose(gr_file);
}
#endif /* PAR */

//@node Dumping routines,  , Writing to the log-file
//@subsection Dumping routines

//@cindex DumpGranEvent
void
DumpGranEvent(name, tso)
GranEventType name;
StgTSO *tso;
{
    DumpRawGranEvent(CURRENT_PROC, (PEs)0, name, tso, END_TSO_QUEUE, (StgInt)0, (StgInt)0);
}

//@cindex DumpRawGranEvent
void
DumpRawGranEvent(proc, p, name, tso, node, sparkname, len)
PEs proc, p;         /* proc ... where it happens; p ... where node lives */
GranEventType name;
StgTSO *tso;
StgClosure *node;
StgInt sparkname, len;
{
  FILE *output_file; // DEBUGGING ONLY !!!!!!!!!!!!!!!!!!!!!!!!!1
  StgWord id;
  char time_string[TIME_STR_LEN], node_str[NODE_STR_LEN];
# if defined(GRAN)
  ullong_format_string(TIME_ON_PROC(proc), 
		       time_string, rtsFalse/*no commas!*/);
# elif defined(PAR)
  ullong_format_string(CURRENT_TIME,
		       time_string, rtsFalse/*no commas!*/);
# endif
  output_file = gr_file;
# if defined(GRAN)
  if (RtsFlags.GranFlags.GranSimStats.Full) 
    ASSERT(output_file!=NULL);

  IF_DEBUG(gran,
	   fprintf(stderr, "GRAN: Dumping info to file with handle %#x\n", output_file))
		   
  if (RtsFlags.GranFlags.GranSimStats.Suppressed)
    return;
# elif defined(PAR)
  if (RtsFlags.ParFlags.ParStats.Full) 
    ASSERT(output_file!=NULL);
# endif

  id = tso == NULL ? -1 : tso->id;
  if (node==stgCast(StgClosure*,&END_TSO_QUEUE_closure))
      strcpy(node_str,"________");  /* "END_TSO_QUEUE"); */
  else
      sprintf(node_str,"0x%-6lx",node);

  if (name > GR_EVENT_MAX)
	name = GR_EVENT_MAX;

  if (BINARY_STATS)
    barf("binary log files not yet supported");
#if 0
    /* ToDo: fix code for writing binary GrAnSim statistics */
    switch (name) { 
      case GR_START:
      case GR_STARTQ:
                      grputw(name);
		      grputw(proc);
		      abort();        /* die please: a single word */
				      /* doesn't represent long long times */
		      grputw(TIME_ON_PROC(proc));
		      grputw((StgWord)node);
		      break;
      case GR_FETCH:
      case GR_REPLY:
      case GR_BLOCK:
		      grputw(name);
		      grputw(proc);
		      abort();        /* die please: a single word */
				      /* doesn't represent long long times */
		      grputw(TIME_ON_PROC(proc));  /* this line is bound to */
		      grputw(id);                  /*   do the wrong thing */
		      break;
      default: 
                      grputw(name);
		      grputw(proc);
		      abort();        /* die please: a single word */
				      /* doesn't represent long long times */
		      grputw(TIME_ON_PROC(proc));
		      grputw((StgWord)node);
    }
#endif
  else /* !BINARY_STATS */
    switch (name) { 
     case GR_START:
     case GR_STARTQ:
        fprintf(output_file,"PE %2u [%s]: %-9s\t%lx\t%s\t[SN %u]\t[sparks %u]\n", 
	        proc,time_string,gran_event_names[name],
	        id,node_str,sparkname,len);
        break;
     case GR_FETCH:
     case GR_REPLY:
     case GR_BLOCK:
     case GR_STOLEN:
     case GR_STOLENQ:
	fprintf(output_file, "PE %2u [%s]: %-9s\t%lx \t%s\t(from %2u)\n",
	        proc, time_string, gran_event_names[name], 
		id,node_str,p);
	break;
     case GR_RESUME:
     case GR_RESUMEQ:
     case GR_SCHEDULE:
     case GR_DESCHEDULE:
        fprintf(output_file,"PE %2u [%s]: %-9s\t%lx \n",
	        proc,time_string,gran_event_names[name],id);
        break;
     case GR_STEALING:
        fprintf(output_file,"PE %2u [%s]: %-9s\t%lx\t        \t(by %2u)\n",
	        proc,time_string,gran_event_names[name],id,p);
        break;
     case GR_ALLOC:
        fprintf(output_file,"PE %2u [%s]: %-9s\t%lx\t        \tallocating %u words\n",
	        proc,time_string,gran_event_names[name],id,len);
        break;
     default:
        fprintf(output_file,"PE %2u [%s]: %-9s\t%lx\t%s\t[sparks %u]\n",
	        proc,time_string,gran_event_names[name],id,node_str,len);
    }
}

//@cindex DumpGranInfo
void
DumpEndEvent(proc, tso, mandatory_thread)
PEs proc;
StgTSO *tso;
rtsBool mandatory_thread;
{
  FILE *output_file; // DEBUGGING ONLY !!!!!!!!!!!!!!!!!!!!!!!!!1
  char time_string[TIME_STR_LEN];
# if defined(GRAN)
  ullong_format_string(TIME_ON_PROC(proc), 
		       time_string, rtsFalse/*no commas!*/);
# elif defined(PAR)
  ullong_format_string(CURRENT_TIME,
		       time_string, rtsFalse/*no commas!*/);
# endif

  output_file = gr_file;
  ASSERT(output_file!=NULL);
#if defined(GRAN)
    if (RtsFlags.GranFlags.GranSimStats.Suppressed)
      return;
#endif

    if (BINARY_STATS) {
    barf("binary log files not yet supported");
#if 0
	grputw(GR_END);
	grputw(proc);
	abort(); /* die please: a single word doesn't represent long long times */
	grputw(CURRENT_TIME); /* this line is bound to fail */
	grputw(tso->id);
#ifdef PAR
	grputw(0);
	grputw(0);
	grputw(0);
	grputw(0);
	grputw(0);
	grputw(0);
	grputw(0);
	grputw(0);
	grputw(0);
	grputw(0);
	grputw(0);
	grputw(0);
#else
	grputw(tso->gran.sparkname);
	grputw(tso->gran.startedat);
	grputw(tso->gran.exported);
	grputw(tso->gran.basicblocks);
	grputw(tso->gran.allocs);
	grputw(tso->gran.exectime);
	grputw(tso->gran.blocktime);
	grputw(tso->gran.blockcount);
	grputw(tso->gran.fetchtime);
	grputw(tso->gran.fetchcount);
	grputw(tso->gran.localsparks);
	grputw(tso->gran.globalsparks);
#endif
	grputw(mandatory_thread);
#endif /* 0 */
    } else {

	/*
	 * NB: DumpGranEvent cannot be used because PE may be wrong 
	 * (as well as the extra info)
	 */
	fprintf(output_file, "PE %2u [%s]: END %lx, SN %u, ST %lu, EXP %c, BB %u, HA %u, RT %u, BT %u (%u), FT %u (%u), LS %u, GS %u, MY %c\n"
	  ,proc
	  ,time_string
	  ,tso->id
#if defined(GRAN)		
	  ,tso->gran.sparkname
	  ,tso->gran.startedat
	  ,tso->gran.exported ? 'T' : 'F'
	  ,tso->gran.basicblocks
	  ,tso->gran.allocs
	  ,tso->gran.exectime
	  ,tso->gran.blocktime
	  ,tso->gran.blockcount
	  ,tso->gran.fetchtime
	  ,tso->gran.fetchcount
	  ,tso->gran.localsparks
	  ,tso->gran.globalsparks
#elif defined(PAR)
	  ,tso->par.sparkname
	  ,tso->par.startedat
	  ,tso->par.exported ? 'T' : 'F'
	  ,tso->par.basicblocks
	  ,tso->par.allocs
	  ,tso->par.exectime
	  ,tso->par.blocktime
	  ,tso->par.blockcount
	  ,tso->par.fetchtime
	  ,tso->par.fetchcount
	  ,tso->par.localsparks
	  ,tso->par.globalsparks
#endif
	  ,mandatory_thread ? 'T' : 'F'
	  );
    }
}

//@cindex DumpTSO
void
DumpTSO(tso)
StgTSO *tso;
{
  FILE *output_file; // DEBUGGING ONLY !!!!!!!!!!!!!!!!!!!!!!!!!1

  output_file = gr_file;
  ASSERT(output_file!=NULL);
  fprintf(stderr,"TSO 0x%lx, NAME 0x%lx, ID %u, LINK 0x%lx, TYPE %s\n"
          ,tso
#if defined(GRAN)
          ,tso->gran.sparkname
#elif defined(PAR)
          ,tso->par.sparkname
#endif
          ,tso->id
          ,tso->link
          ,/*tso->state==T_MAIN?"MAIN":
           TSO_TYPE(tso)==T_FAIL?"FAIL":
           TSO_TYPE(tso)==T_REQUIRED?"REQUIRED":
           TSO_TYPE(tso)==T_ADVISORY?"ADVISORY":
	   */
           "???"
          );
          
  fprintf(output_file,"TSO %lx: SN %u, ST %u, GBL %c, BB %u, HA %u, RT %u, BT %u (%u), FT %u (%u) LS %u, GS %u\n"
	  ,tso->id
#if defined(GRAN)
          ,tso->gran.sparkname
          ,tso->gran.startedat
          ,tso->gran.exported?'T':'F'
          ,tso->gran.basicblocks
          ,tso->gran.allocs
          ,tso->gran.exectime
          ,tso->gran.blocktime
          ,tso->gran.blockcount
          ,tso->gran.fetchtime
          ,tso->gran.fetchcount
          ,tso->gran.localsparks
          ,tso->gran.globalsparks
#elif defined(PAR)
          ,tso->par.sparkname
          ,tso->par.startedat
          ,tso->par.exported?'T':'F'
          ,tso->par.basicblocks
          ,tso->par.allocs
          ,tso->par.exectime
          ,tso->par.blocktime
          ,tso->par.blockcount
          ,tso->par.fetchtime
          ,tso->par.fetchcount
          ,tso->par.localsparks
          ,tso->par.globalsparks
#endif
          );
}

#if 0
/*
  ToDo: fix binary output of log files, and support new log file format.
*/
/*
   Output a terminate event and an 8-byte time.
*/

//@cindex grterminate
void
grterminate(v)
rtsTime v;
{
  if (!BINARY_STATS) 
    barf("grterminate: binary statistics not enabled\n");

# if defined(GRAN)
    if (RtsFlags.GranFlags.GranSimStats.Suppressed)
      return;
# endif

    DumpGranEvent(GR_TERMINATE, stgCast(StgTSO*,&END_TSO_QUEUE_closure));

    if (sizeof(rtsTime) == 4) {
      putc('\0', gr_file);
      putc('\0', gr_file);
      putc('\0', gr_file);
      putc('\0', gr_file);
    } else {
      putc(v >> 56l, gr_file);
      putc((v >> 48l) & 0xffl, gr_file);
      putc((v >> 40l) & 0xffl, gr_file);
      putc((v >> 32l) & 0xffl, gr_file);
    }
    putc((v >> 24l) & 0xffl, gr_file);
    putc((v >> 16l) & 0xffl, gr_file);
    putc((v >> 8l) & 0xffl, gr_file);
    putc(v & 0xffl, gr_file);
}

/*
   Length-coded output: first 3 bits contain length coding

     00x        1 byte
     01x        2 bytes
     10x        4 bytes
     110        8 bytes
     111        5 or 9 bytes
*/

//@cindex grputw
void
grputw(v)
rtsTime v;
{
  if (!BINARY_STATS) 
    barf("grputw: binary statistics not enabled\n");

# if defined(GRAN)
    if (RtsFlags.GranFlags.GranSimStats.Suppressed)
      return;
# endif

    if (v <= 0x3fl) {                           /* length v = 1 byte */ 
	fputc(v & 0x3f, gr_file);
    } else if (v <= 0x3fffl) {                  /* length v = 2 byte */ 
	fputc((v >> 8l) | 0x40l, gr_file);
	fputc(v & 0xffl, gr_file);
    } else if (v <= 0x3fffffffl) {              /* length v = 4 byte */ 
	fputc((v >> 24l) | 0x80l, gr_file);
	fputc((v >> 16l) & 0xffl, gr_file);
	fputc((v >> 8l) & 0xffl, gr_file);
	fputc(v & 0xffl, gr_file);
    } else if (sizeof(TIME) == 4) {
	fputc(0x70, gr_file);
	fputc((v >> 24l) & 0xffl, gr_file);
	fputc((v >> 16l) & 0xffl, gr_file);
	fputc((v >> 8l) & 0xffl, gr_file);
	fputc(v & 0xffl, gr_file);
    } else {
	if (v <= 0x3fffffffffffffl)
	    putc((v >> 56l) | 0x60l, gr_file);
	else {
	    putc(0x70, gr_file);
	    putc((v >> 56l) & 0xffl, gr_file);
	}

	putc((v >> 48l) & 0xffl, gr_file);
	putc((v >> 40l) & 0xffl, gr_file);
	putc((v >> 32l) & 0xffl, gr_file);
	putc((v >> 24l) & 0xffl, gr_file);
	putc((v >> 16l) & 0xffl, gr_file);
	putc((v >> 8l) & 0xffl, gr_file);
	putc(v & 0xffl, gr_file);
    }
}
#endif /* 0 */

/* 
   extracting specific info out of a closure; used in packing (GranSim, GUM)
*/
//@cindex get_closure_info
StgInfoTable*
get_closure_info(StgClosure* node, nat *size, nat *ptrs, nat *nonptrs, 
		 nat *vhs, char *info_hdr_ty)
{
  StgInfoTable *info;

  ASSERT(LOOKS_LIKE_COOL_CLOSURE(node)); 
  info = get_itbl(node);
  /* the switch shouldn't be necessary, really; just use default case */
  switch (info->type) {
  case RBH:
    {
      StgInfoTable *rip = REVERT_INFOPTR(info); // closure to revert to
      *size = sizeW_fromITBL(rip);
      *ptrs = (nat) (rip->layout.payload.ptrs);
      *nonptrs = (nat) (rip->layout.payload.nptrs);
      *vhs = *size - *ptrs - *nonptrs - sizeofW(StgHeader);
#if 0 /* DEBUG */
      info_hdr_type(node, info_hdr_ty);
#else
      strcpy(info_hdr_ty, "RBH");
#endif
      return rip;  // NB: we return the reverted info ptr for a RBH!!!!!!
    }

#if defined(PAR)
  /* Closures specific to GUM */
  case FETCH_ME:
    *size = sizeofW(StgFetchMe);
    *ptrs = (nat)0;
    *nonptrs = (nat)0;
    *vhs = *size - *ptrs - *nonptrs - sizeofW(StgHeader);
#if 0 /* DEBUG */
    info_hdr_type(node, info_hdr_ty);
#else
    strcpy(info_hdr_ty, "FETCH_ME");
#endif
    return info;

  case FETCH_ME_BQ:
    *size = sizeofW(StgFetchMeBlockingQueue);
    *ptrs = (nat)0;
    *nonptrs = (nat)0;
    *vhs = *size - *ptrs - *nonptrs - sizeofW(StgHeader);
#if 0 /* DEBUG */
    info_hdr_type(node, info_hdr_ty);
#else
    strcpy(info_hdr_ty, "FETCH_ME_BQ");
#endif
    return info;

  case BLOCKED_FETCH:
    *size = sizeofW(StgBlockedFetch);
    *ptrs = (nat)0;
    *nonptrs = (nat)0;
    *vhs = *size - *ptrs - *nonptrs - sizeofW(StgHeader);
#if 0 /* DEBUG */
    info_hdr_type(node, info_hdr_ty);
#else
    strcpy(info_hdr_ty, "BLOCKED_FETCH");
#endif
    return info;
#endif /* PAR */
    
  /* these magic constants are outrageous!! why does the ITBL lie about it? */
  case THUNK_SELECTOR:
    *size = THUNK_SELECTOR_sizeW();
    *ptrs = 1;
    *nonptrs = MIN_UPD_SIZE-*ptrs;   // weird
    *vhs = *size - *ptrs - *nonptrs - sizeofW(StgHeader);
    return info;

  case ARR_WORDS:
    /* ToDo: check whether this can be merged with the default case */
    *size = arr_words_sizeW((StgArrWords *)node); 
    *ptrs = 0;
    *nonptrs = ((StgArrWords *)node)->words;
    *vhs = *size - *ptrs - *nonptrs - sizeofW(StgHeader);
    return info;

  case PAP:
    /* ToDo: check whether this can be merged with the default case */
    *size = pap_sizeW((StgPAP *)node); 
    *ptrs = 0;
    *nonptrs = 0;
    *vhs = *size - *ptrs - *nonptrs - sizeofW(StgHeader);
    return info;

  case AP_UPD:
    /* ToDo: check whether this can be merged with the default case */
    *size = AP_sizeW(((StgAP_UPD *)node)->n_args); 
    *ptrs = 0;
    *nonptrs = 0;
    *vhs = *size - *ptrs - *nonptrs - sizeofW(StgHeader);
    return info;

  default:
    *size = sizeW_fromITBL(info);
    *ptrs = (nat) (info->layout.payload.ptrs);
    *nonptrs = (nat) (info->layout.payload.nptrs);
    *vhs = *size - *ptrs - *nonptrs - sizeofW(StgHeader);
#if 0 /* DEBUG */
      info_hdr_type(node, info_hdr_ty);
#else
      strcpy(info_hdr_ty, "UNKNOWN");
#endif
    return info;
  }
} 

//@cindex IS_BLACK_HOLE
rtsBool
IS_BLACK_HOLE(StgClosure* node)          
{ 
  // StgInfoTable *info;
  ASSERT(LOOKS_LIKE_COOL_CLOSURE(node));
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
StgClosure *
IS_INDIRECTION(StgClosure* node)          
{ 
  StgInfoTable *info;
  ASSERT(LOOKS_LIKE_COOL_CLOSURE(node));
  info = get_itbl(node);
  switch (info->type) {
    case IND:
    case IND_OLDGEN:
    case IND_PERM:
    case IND_OLDGEN_PERM:
    case IND_STATIC:
      /* relies on indirectee being at same place for all these closure types */
      return (((StgInd*)node) -> indirectee);
#if 0
    case EVACUATED:           // counting as ind to use in GC routines, too
      // could use the same code as above (evacuee is at same pos as indirectee)
      return (((StgEvacuated *)node) -> evacuee);
#endif
    default:
      return NULL;
  }
}

//@cindex unwindInd
StgClosure *
UNWIND_IND (StgClosure *closure)
{
  StgClosure *next;

  while ((next = IS_INDIRECTION((StgClosure *)closure)) != NULL) 
    closure = next;

  ASSERT(next==(StgClosure *)NULL);
  ASSERT(LOOKS_LIKE_COOL_CLOSURE(closure)); 
  return closure;
}

#endif /* GRAN || PAR   whole file */
