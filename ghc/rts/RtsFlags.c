/* -----------------------------------------------------------------------------
 * $Id: RtsFlags.c,v 1.20 1999/11/02 15:06:00 simonmar Exp $
 *
 * (c) The AQUA Project, Glasgow University, 1994-1997
 * (c) The GHC Team, 1998-1999
 *
 * Functions for parsing the argument list.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "BlockAlloc.h"
#include "ProfRts.h"

#if defined(PROFILING) 
#include "Itimer.h"
#endif

#if HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

extern struct RTS_FLAGS RtsFlags;

/*
 * Split argument lists
 */
int     prog_argc; /* an "int" so as to match normal "argc" */
char  **prog_argv = NULL;
int     rts_argc;  /* ditto */
char   *rts_argv[MAX_RTS_ARGS];

/*
 * constants, used later 
 */
#define RTS 1
#define PGM 0

/* -----------------------------------------------------------------------------
   Static function decls
   -------------------------------------------------------------------------- */

static FILE *		/* return NULL on error */
open_stats_file (
    I_ arg,
    int argc, char *argv[],
    int rts_argc, char *rts_argv[],
    const char *FILENAME_FMT);

static I_ decode(const char *s);
static void bad_option(const char *s);

/* -----------------------------------------------------------------------------
 * Command-line option parsing routines.
 * ---------------------------------------------------------------------------*/

void initRtsFlagsDefaults(void)
{
    RtsFlags.GcFlags.statsFile		= NULL;
    RtsFlags.GcFlags.giveStats		= NO_GC_STATS;

    RtsFlags.GcFlags.maxStkSize		= (1024 * 1024) / sizeof(W_);
    RtsFlags.GcFlags.initialStkSize	= 1024 / sizeof(W_);

    RtsFlags.GcFlags.minAllocAreaSize   = (256 * 1024)        / BLOCK_SIZE;
    RtsFlags.GcFlags.minOldGenSize      = (1024 * 1024)       / BLOCK_SIZE;
    RtsFlags.GcFlags.maxHeapSize	= (64  * 1024 * 1024) / BLOCK_SIZE;
    RtsFlags.GcFlags.heapSizeSuggestion	= 0;    /* none */
    RtsFlags.GcFlags.pcFreeHeap		= 3;	/* 3% */
    RtsFlags.GcFlags.oldGenFactor       = 2;
    RtsFlags.GcFlags.generations        = 2;
    RtsFlags.GcFlags.steps              = 2;

    RtsFlags.GcFlags.forceGC		= rtsFalse;
    RtsFlags.GcFlags.forcingInterval	= 5000000; /* 5MB (or words?) */
    RtsFlags.GcFlags.ringBell		= rtsFalse;

    RtsFlags.GcFlags.squeezeUpdFrames	= rtsTrue;

#if defined(PROFILING) || defined(PAR)
    RtsFlags.CcFlags.doCostCentres	= 0;
    RtsFlags.CcFlags.sortBy		= SORTCC_TIME;
#endif /* PROFILING or PAR */

#ifdef PROFILING
    RtsFlags.ProfFlags.doHeapProfile = rtsFalse;

    RtsFlags.ProfFlags.ccSelector    = NULL;
    RtsFlags.ProfFlags.modSelector   = NULL;
    RtsFlags.ProfFlags.grpSelector   = NULL;
    RtsFlags.ProfFlags.descrSelector = NULL;
    RtsFlags.ProfFlags.typeSelector  = NULL;
    RtsFlags.ProfFlags.kindSelector  = NULL;
#elif defined(DEBUG)
    RtsFlags.ProfFlags.doHeapProfile = rtsFalse;
#endif

    RtsFlags.ConcFlags.ctxtSwitchTime	= CS_MIN_MILLISECS;  /* In milliseconds */
#ifdef SMP
    RtsFlags.ConcFlags.nNodes	= 1;
#endif
#ifdef PAR
    RtsFlags.ParFlags.parallelStats	= rtsFalse;
    RtsFlags.ParFlags.granSimStats	= rtsFalse;
    RtsFlags.ParFlags.granSimStats_Binary = rtsFalse;

    RtsFlags.ParFlags.outputDisabled	= rtsFalse;

    RtsFlags.ParFlags.packBufferSize	= 1024;
    RtsFlags.ParFlags.maxLocalSparks	= 4096;
#endif /* PAR */

#ifdef GRAN
    RtsFlags.GranFlags.granSimStats	= rtsFalse;
    RtsFlags.GranFlags.granSimStats_suppressed	= rtsFalse;
    RtsFlags.GranFlags.granSimStats_Binary = rtsFalse;
    RtsFlags.GranFlags.granSimStats_Sparks = rtsFalse;
    RtsFlags.GranFlags.granSimStats_Heap = rtsFalse;
    RtsFlags.GranFlags.labelling	= rtsFalse;
    RtsFlags.GranFlags.packBufferSize	= 1024;
    RtsFlags.GranFlags.packBufferSize_internal = GRANSIM_DEFAULT_PACK_BUFFER_SIZE;

    RtsFlags.GranFlags.proc  = MAX_PROC;
    RtsFlags.GranFlags.max_fishes = MAX_FISHES;
    RtsFlags.GranFlags.time_slice = GRAN_TIME_SLICE;
    RtsFlags.GranFlags.Light = rtsFalse;

    RtsFlags.GranFlags.gran_latency =             LATENCY;          
    RtsFlags.GranFlags.gran_additional_latency =  ADDITIONAL_LATENCY; 
    RtsFlags.GranFlags.gran_fetchtime =           FETCHTIME; 
    RtsFlags.GranFlags.gran_lunblocktime =        LOCALUNBLOCKTIME; 
    RtsFlags.GranFlags.gran_gunblocktime =        GLOBALUNBLOCKTIME;
    RtsFlags.GranFlags.gran_mpacktime =           MSGPACKTIME;      
    RtsFlags.GranFlags.gran_munpacktime =         MSGUNPACKTIME;
    RtsFlags.GranFlags.gran_mtidytime =           MSGTIDYTIME;

    RtsFlags.GranFlags.gran_threadcreatetime =         THREADCREATETIME;
    RtsFlags.GranFlags.gran_threadqueuetime =          THREADQUEUETIME;
    RtsFlags.GranFlags.gran_threaddescheduletime =     THREADDESCHEDULETIME;
    RtsFlags.GranFlags.gran_threadscheduletime =       THREADSCHEDULETIME;
    RtsFlags.GranFlags.gran_threadcontextswitchtime =  THREADCONTEXTSWITCHTIME;

    RtsFlags.GranFlags.gran_arith_cost =         ARITH_COST;       
    RtsFlags.GranFlags.gran_branch_cost =        BRANCH_COST; 
    RtsFlags.GranFlags.gran_load_cost =          LOAD_COST;        
    RtsFlags.GranFlags.gran_store_cost =         STORE_COST; 
    RtsFlags.GranFlags.gran_float_cost =         FLOAT_COST;       

    RtsFlags.GranFlags.gran_heapalloc_cost =     HEAPALLOC_COST;

    RtsFlags.GranFlags.gran_pri_spark_overhead = PRI_SPARK_OVERHEAD;        
    RtsFlags.GranFlags.gran_pri_sched_overhead = PRI_SCHED_OVERHEAD;        

    RtsFlags.GranFlags.DoFairSchedule = rtsFalse;             
    RtsFlags.GranFlags.DoReScheduleOnFetch = rtsFalse;        
    RtsFlags.GranFlags.DoStealThreadsFirst = rtsFalse;        
    RtsFlags.GranFlags.SimplifiedFetch = rtsFalse;            
    RtsFlags.GranFlags.DoAlwaysCreateThreads = rtsFalse;      
    RtsFlags.GranFlags.DoGUMMFetching = rtsFalse;             
    RtsFlags.GranFlags.DoThreadMigration = rtsFalse;          
    RtsFlags.GranFlags.FetchStrategy = 2;                     
    RtsFlags.GranFlags.PreferSparksOfLocalNodes = rtsFalse;   
    RtsFlags.GranFlags.DoPrioritySparking = rtsFalse;         
    RtsFlags.GranFlags.DoPriorityScheduling = rtsFalse;       
    RtsFlags.GranFlags.SparkPriority = 0;
    RtsFlags.GranFlags.SparkPriority2 = 0; 
    RtsFlags.GranFlags.RandomPriorities = rtsFalse;           
    RtsFlags.GranFlags.InversePriorities = rtsFalse;          
    RtsFlags.GranFlags.IgnorePriorities = rtsFalse;           
    RtsFlags.GranFlags.ThunksToPack = 0;                      
    RtsFlags.GranFlags.RandomSteal = rtsTrue;
    RtsFlags.GranFlags.NoForward = rtsFalse;
    RtsFlags.GranFlags.PrintFetchMisses = rtsFalse;

    RtsFlags.GranFlags.debug = 0x0;
    RtsFlags.GranFlags.event_trace = rtsFalse;
    RtsFlags.GranFlags.event_trace_all = rtsFalse;
#endif

#ifdef TICKY_TICKY
    RtsFlags.TickyFlags.showTickyStats	 = rtsFalse;
    RtsFlags.TickyFlags.tickyFile	 = NULL;
#endif
}

static const char *
usage_text[] = {
"",
"Usage: <prog> <args> [+RTS <rtsopts> | -RTS <args>] ... --RTS <args>",
"",
"   +RTS    Indicates run time system options follow",
"   -RTS    Indicates program arguments follow",
"  --RTS    Indicates that ALL subsequent arguments will be given to the",
"           program (including any of these RTS flags)",
"",
"The following run time system options are available:",
"",
"  -? -f    Prints this message and exits; the program is not executed",
"",
"  -K<size> Sets the maximum stack size (default 1M)  Egs: -K32k   -K512k",
"  -k<size> Sets the initial thread stack size (default 1k)  Egs: -K4k   -K2m",
"",
"  -A<size> Sets the minimum allocation area size (default 256k) Egs: -A1m -A10k",
"  -M<size> Sets the maximum heap size (default 64M)  Egs: -M256k -M1G",
"  -H<size> Sets the minimum heap size (default 0M)   Egs: -H24m  -H1G",
"  -m<n>%   Minimum % of heap which must be available (default 3%)",
"  -G<n>    Number of generations (default: 2)",
"  -T<n>    Number of steps in younger generations (default: 2)",
"  -s<file> Summary GC statistics   (default file: <program>.stat)",
"  -S<file> Detailed GC statistics  (with -Sstderr going to stderr)",
"",
"",
"  -Z       Don't squeeze out update frames on stack overflow",
"  -B       Sound the bell at the start of each garbage collection",
#if defined(PROFILING) || defined(PAR)
"",
"  -p<sort> Produce cost centre time profile  (output file <program>.prof)",
"             sort: T = time (default), A = alloc, C = cost centre label",
"  -P<sort> Produce serial time profile (output file <program>.time)",
"             and a -p profile with detailed tick/alloc info",
# if defined(PROFILING)
"",
"  -h<break-down> Heap residency profile      (output file <program>.hp)",
"     break-down: C = cost centre stack (default), M = module, G = group",
"                 D = closure description, Y = type description",
"                 T<ints>,<start> = time closure created",
"                    ints:  no. of interval bands plotted (default 18)",
"                    start: seconds after which intervals start (default 0.0)",
"  A subset of closures may be selected by the attached cost centre using:",
"    -c{mod:lab,mod:lab...}, specific module:label cost centre(s)",
"    -m{mod,mod...} all cost centres from the specified modules(s)",
"    -g{grp,grp...} all cost centres from the specified group(s)",
"  Selections can also be made by description, type, kind and age:",
"    -d{des,des...} closures with specified closure descriptions",
"    -y{typ,typ...} closures with specified type descriptions",
"    -k{knd,knd...} closures of the specified kinds",
"    -a<age>        closures which survived <age> complete intervals",
"  The selection logic used is summarised as follows:",
"    ([-c] or [-m] or [-g]) and ([-d] or [-y] or [-k]) and [-a]",
"    where an option is true if not specified",
# endif
"",
"  -z<tbl><size>  set hash table <size> for <tbl> (C, M, G, D or Y)",
"",
"  -i<secs> Number of seconds in a profiling interval (default 1.0):",
"           heap profile (-h) and/or serial time profile (-P) frequency",
#endif /* PROFILING or PAR */
#if !defined(PROFILING) && defined(DEBUG)
"",
"  -h<break-down> Debugging Heap residency profile",
"                 (output file <program>.hp)",
"     break-down: L = closure label (default)",
"                 T = closure type (constructor, thunk etc.)",
#endif
"",
#if defined(TICKY_TICKY)
"  -r<file>  Produce reduction profiling statistics (with -rstderr for stderr)",
"",
#endif
# ifdef PAR
"  -N<n>     Use <n> PVMish processors in parallel (default: 2)",
/* NB: the -N<n> is implemented by the driver!! */
# endif
"  -C<secs>  Context-switch interval in seconds",
"                (0 or no argument means switch as often as possible)",
"                the default is .01 sec; resolution is .01 sec",
# ifdef SMP
"  -N<n>     Use <n> OS threads (default: 1)",
# endif
# ifdef PAR
"  -q        Enable activity profile (output files in ~/<program>*.gr)",
"  -qb       Enable binary activity profile (output file /tmp/<program>.gb)",
"  -Q<size>  Set pack-buffer size (default: 1024)",
# endif
# ifdef PAR
"  -d        Turn on PVM-ish debugging",
"  -O        Disable output for performance measurement",
# endif /* PAR */
# ifdef GRAN  /* ToDo: fill in decent Docu here */
"  -b...     All GranSim options start with -b; see GranSim User's Guide for details",
# endif
"",
"Other RTS options may be available for programs compiled a different way.",
"The GHC User's Guide has full details.",
"",
0
};

static __inline__ rtsBool
strequal(const char *a, const char * b)
{
    return(strcmp(a, b) == 0);
}

void
setupRtsFlags(int *argc, char *argv[], int *rts_argc, char *rts_argv[])
{
    rtsBool error = rtsFalse;
    I_ mode;
    I_ arg, total_arg;
    char *last_slash;

    /* Remove directory from argv[0] -- default files in current directory */

    if ((last_slash = (char *) strrchr(argv[0], '/')) != NULL)
	strcpy(argv[0], last_slash+1);

    /* Split arguments (argv) into PGM (argv) and RTS (rts_argv) parts */
    /*   argv[0] must be PGM argument -- leave in argv                 */

    total_arg = *argc;
    arg = 1;

    *argc = 1;
    *rts_argc = 0;

    for (mode = PGM; arg < total_arg && ! strequal("--RTS", argv[arg]); arg++) {
	if (strequal("+RTS", argv[arg])) {
	    mode = RTS;
	}
	else if (strequal("-RTS", argv[arg])) {
	    mode = PGM;
	}
	else if (mode == RTS && *rts_argc < MAX_RTS_ARGS-1) {
	    rts_argv[(*rts_argc)++] = argv[arg];
	}
	else if (mode == PGM) {
	    argv[(*argc)++] = argv[arg];
	}
	else {
	  barf("too many RTS arguments (max %d)", MAX_RTS_ARGS-1);
	}
    }
    if (arg < total_arg) {
	/* arg must be --RTS; process remaining program arguments */
	while (++arg < total_arg) {
	    argv[(*argc)++] = argv[arg];
	}
    }
    argv[*argc] = (char *) 0;
    rts_argv[*rts_argc] = (char *) 0;

    /* Process RTS (rts_argv) part: mainly to determine statsfile */

    for (arg = 0; arg < *rts_argc; arg++) {
	if (rts_argv[arg][0] != '-') {
	    fflush(stdout);
	    fprintf(stderr, "setupRtsFlags: Unexpected RTS argument: %s\n",
		    rts_argv[arg]);
	    error = rtsTrue;

        } else {
	    switch(rts_argv[arg][1]) {

	      /* process: general args, then PROFILING-only ones,
		 then CONCURRENT-only, PARallel-only, GRAN-only,
		 TICKY-only (same order as defined in RtsFlags.lh);
		 within those groups, mostly in case-insensitive
		 alphabetical order.
	      */

#ifdef TICKY_TICKY
# define TICKY_BUILD_ONLY(x) x
#else
# define TICKY_BUILD_ONLY(x) \
fprintf(stderr, "setupRtsFlags: GHC not built for: ticky-ticky stats\n"); \
error = rtsTrue;
#endif

#if defined(PROFILING) 
# define COST_CENTRE_USING_BUILD_ONLY(x) x
#else
# define COST_CENTRE_USING_BUILD_ONLY(x) \
fprintf(stderr, "setupRtsFlags: GHC not built for: -prof or -parallel\n"); \
error = rtsTrue;
#endif

#ifdef PROFILING
# define PROFILING_BUILD_ONLY(x)   x
#else
# define PROFILING_BUILD_ONLY(x) \
fprintf(stderr, "setupRtsFlags: GHC not built for: -prof\n"); \
error = rtsTrue;
#endif

#ifdef PAR
# define PAR_BUILD_ONLY(x)      x
#else
# define PAR_BUILD_ONLY(x) \
fprintf(stderr, "setupRtsFlags: GHC not built for: -parallel\n"); \
error = rtsTrue;
#endif

#ifdef GRAN
# define GRAN_BUILD_ONLY(x)     x
#else
# define GRAN_BUILD_ONLY(x) \
fprintf(stderr, "setupRtsFlags: GHC not built for: -gransim\n"); \
error = rtsTrue;
#endif

	      /* =========== GENERAL ========================== */
	      case '?':
	      case 'f':
		error = rtsTrue;
		break;

	      case 'A':
		RtsFlags.GcFlags.minAllocAreaSize
		  = decode(rts_argv[arg]+2) / BLOCK_SIZE;
		if (RtsFlags.GcFlags.minAllocAreaSize <= 0) {
		  bad_option(rts_argv[arg]);
		}
		break;

	      case 'B':
		RtsFlags.GcFlags.ringBell = rtsTrue;
		break;

	      case 'F':
	        RtsFlags.GcFlags.oldGenFactor = atof(rts_argv[arg]+2);
	      
		if (RtsFlags.GcFlags.oldGenFactor < 0)
		  bad_option( rts_argv[arg] );
		break;
	      
#ifdef DEBUG
	      case 'D':
  	        /* hack warning: interpret the flags as a binary number */
		{ 
                   I_ n = decode(rts_argv[arg]+2);
                   if (n     &1) RtsFlags.DebugFlags.scheduler   = rtsTrue;
                   if ((n>>1)&1) RtsFlags.DebugFlags.evaluator   = rtsTrue;
                   if ((n>>2)&1) RtsFlags.DebugFlags.codegen     = rtsTrue;
                   if ((n>>3)&1) RtsFlags.DebugFlags.weak        = rtsTrue;
                   if ((n>>4)&1) RtsFlags.DebugFlags.gccafs      = rtsTrue;
                   if ((n>>5)&1) RtsFlags.DebugFlags.gc          = rtsTrue;
                   if ((n>>6)&1) RtsFlags.DebugFlags.block_alloc = rtsTrue;
                   if ((n>>7)&1) RtsFlags.DebugFlags.sanity      = rtsTrue;
                   if ((n>>8)&1) RtsFlags.DebugFlags.stable      = rtsTrue;
                   if ((n>>9)&1) RtsFlags.DebugFlags.prof        = rtsTrue;
                }
		break;
#endif

	      case 'K':
		RtsFlags.GcFlags.maxStkSize = 
		  decode(rts_argv[arg]+2) / sizeof(W_);

		if (RtsFlags.GcFlags.maxStkSize == 0) 
		  bad_option( rts_argv[arg] );
		break;

	      case 'k':
		RtsFlags.GcFlags.initialStkSize = 
		  decode(rts_argv[arg]+2) / sizeof(W_);

		if (RtsFlags.GcFlags.initialStkSize == 0) 
		  bad_option( rts_argv[arg] );
		break;

	      case 'M':
		RtsFlags.GcFlags.maxHeapSize = 
		  decode(rts_argv[arg]+2) / BLOCK_SIZE;
		/* user give size in *bytes* but "maxHeapSize" is in *blocks* */

		if (RtsFlags.GcFlags.maxHeapSize <= 0) {
		  bad_option(rts_argv[arg]);
		}
		break;

	      case 'm':
		RtsFlags.GcFlags.pcFreeHeap = atof(rts_argv[arg]+2);

		if (RtsFlags.GcFlags.pcFreeHeap < 0 || 
		    RtsFlags.GcFlags.pcFreeHeap > 100)
		  bad_option( rts_argv[arg] );
		break;

	      case 'G':
		RtsFlags.GcFlags.generations = decode(rts_argv[arg]+2);
		if (RtsFlags.GcFlags.generations < 1) {
		  bad_option(rts_argv[arg]);
		}
		break;

	      case 'T':
		RtsFlags.GcFlags.steps = decode(rts_argv[arg]+2);
		if (RtsFlags.GcFlags.steps < 1) {
		  bad_option(rts_argv[arg]);
		}
		break;

	      case 'H':
		RtsFlags.GcFlags.heapSizeSuggestion = 
		  decode(rts_argv[arg]+2) / BLOCK_SIZE;

		if (RtsFlags.GcFlags.heapSizeSuggestion <= 0) {
		  bad_option(rts_argv[arg]);
		}
		break;

	      case 'j': /* force GC option */
		RtsFlags.GcFlags.forceGC = rtsTrue;
		if (rts_argv[arg][2]) {
		    RtsFlags.GcFlags.forcingInterval
			= decode(rts_argv[arg]+2) / sizeof(W_);
		}
		break;

	      case 'S':
		RtsFlags.GcFlags.giveStats ++;

	      case 's':
		RtsFlags.GcFlags.giveStats ++;
#ifdef PAR
		/* Opening all those files would almost certainly fail... */
		RtsFlags.ParFlags.parallelStats = rtsTrue;
		RtsFlags.GcFlags.statsFile = stderr; /* temporary; ToDo: rm */
#else
		RtsFlags.GcFlags.statsFile
		  = open_stats_file(arg, *argc, argv,
			*rts_argc, rts_argv, STAT_FILENAME_FMT);

		if (RtsFlags.GcFlags.statsFile == NULL) error = rtsTrue;
#endif
		break;

	      case 'Z':
		RtsFlags.GcFlags.squeezeUpdFrames = rtsFalse;
		break;

	      /* =========== PROFILING ========================== */

	      case 'P': /* detailed cost centre profiling (time/alloc) */
		COST_CENTRE_USING_BUILD_ONLY(
		RtsFlags.CcFlags.doCostCentres++;
		)
	      case 'p': /* cost centre profiling (time/alloc) */
		COST_CENTRE_USING_BUILD_ONLY(
		RtsFlags.CcFlags.doCostCentres++;

		switch (rts_argv[arg][2]) {
		  case SORTCC_LABEL:
		  case SORTCC_TIME:
		  case SORTCC_ALLOC:
			RtsFlags.CcFlags.sortBy = rts_argv[arg][2];
		    break;
		  default:
		    	RtsFlags.CcFlags.sortBy = SORTCC_TIME;
		    break;
		}
		) break;

	      case 'i': /* serial profiling -- initial timer interval */
		COST_CENTRE_USING_BUILD_ONLY(
		interval_ticks = (I_) ((atof(rts_argv[arg]+2) * TICK_FREQUENCY));
		if (interval_ticks <= 0)
		    interval_ticks = 1;
		) break;

	      case 'h': /* serial heap profile */
#if !defined(PROFILING) && defined(DEBUG)
		switch (rts_argv[arg][2]) {
		  case '\0':
		  case 'L':
		    RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_INFOPTR;
		    break;
		  case 'T':
		    RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_CLOSURE_TYPE;
		    break;
		  default:
		    fprintf(stderr, "Invalid heap profile option: %s\n",
			    rts_argv[arg]);
		    error = rtsTrue;
		}
#else
		PROFILING_BUILD_ONLY(
		switch (rts_argv[arg][2]) {
		  case '\0':
		  case CCchar:
		    RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_CCS;
		    break;
		  case MODchar:
		    RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_MOD;
		    break;
		  case GRPchar:
		    RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_GRP;
		    break;
		  case DESCRchar:
		    RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_DESCR;
		    break;
		  case TYPEchar:
		    RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_TYPE;
		    break;
		  case TIMEchar:
		    RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_TIME;
		    if (rts_argv[arg][3]) {
			char *start_str = strchr(rts_argv[arg]+3, ',');
			I_ intervals;
			if (start_str) *start_str = '\0';

			if ((intervals = decode(rts_argv[arg]+3)) != 0) {
			    time_intervals = (hash_t) intervals;
			    /* ToDo: and what if it *is* zero intervals??? */
			}
			if (start_str) {
			    earlier_ticks = (I_)((atof(start_str + 1) * TICK_FREQUENCY));
			}
		    }
		    break;
		  default:
		    fprintf(stderr, "Invalid heap profile option: %s\n",
			    rts_argv[arg]);
		    error = rtsTrue;
		}
		) 
#endif
	        break;

	      case 'z': /* size of index tables */
		PROFILING_BUILD_ONLY(
		switch (rts_argv[arg][2]) {
		  case CCchar:
		    max_cc_no = (hash_t) decode(rts_argv[arg]+3);
		    if (max_cc_no == 0) {
			fprintf(stderr, "Bad number of cost centres %s\n", rts_argv[arg]);
			error = rtsTrue;
		    }
		    break;
		  case MODchar:
		    max_mod_no = (hash_t) decode(rts_argv[arg]+3);
		    if (max_mod_no == 0) {
			fprintf(stderr, "Bad number of modules %s\n", rts_argv[arg]);
			error = rtsTrue;
		    }
		    break;
		  case GRPchar:
		    max_grp_no = (hash_t) decode(rts_argv[arg]+3);
		    if (max_grp_no == 0) {
			fprintf(stderr, "Bad number of groups %s\n", rts_argv[arg]);
			error = rtsTrue;
		    }
		    break;
		  case DESCRchar:
		    max_descr_no = (hash_t) decode(rts_argv[arg]+3);
		    if (max_descr_no == 0) {
			fprintf(stderr, "Bad number of closure descriptions %s\n", rts_argv[arg]);
			error = rtsTrue;
		    }
		    break;
		  case TYPEchar:
		    max_type_no = (hash_t) decode(rts_argv[arg]+3);
		    if (max_type_no == 0) {
			fprintf(stderr, "Bad number of type descriptions %s\n", rts_argv[arg]);
			error = rtsTrue;
		    }
		    break;
		  default:
		    fprintf(stderr, "Invalid index table size option: %s\n",
			    rts_argv[arg]);
		    error = rtsTrue;
		}
		) break;

	      case 'c': /* cost centre label select */
	      case 'g': /* cost centre group select */
	      case 'd': /* closure descr select */
	      case 'y': /* closure type select */
		PROFILING_BUILD_ONLY(
		{char *left  = strchr(rts_argv[arg], '{');
		 char *right = strrchr(rts_argv[arg], '}');

		if (! left || ! right ||
		        strrchr(rts_argv[arg], '{') != left ||
		         strchr(rts_argv[arg], '}') != right) {
		    fprintf(stderr, "Invalid heap profiling selection bracketing\n   %s\n", rts_argv[arg]);
		    error = rtsTrue;
		} else {
		    *right = '\0';
		    switch (rts_argv[arg][1]) {
		      case 'c': /* cost centre label select */
			RtsFlags.ProfFlags.ccSelector = left + 1;
			break;
		      case 'm': /* cost centre module select */
			RtsFlags.ProfFlags.modSelector = left + 1;
			break;
		      case 'g': /* cost centre group select */
			RtsFlags.ProfFlags.grpSelector = left + 1;
			break;
		      case 'd': /* closure descr select */
			RtsFlags.ProfFlags.descrSelector = left + 1;
			break;
		      case 'y': /* closure type select */
			RtsFlags.ProfFlags.typeSelector = left + 1;
			break;
		      case 'k': /* closure kind select */
			RtsFlags.ProfFlags.kindSelector = left + 1;
			break;
		    }
		}}
		) break;

	      /* =========== CONCURRENT ========================= */
    	      case 'C':	/* context switch interval */
		if (rts_argv[arg][2] == '\0')
    	    	    RtsFlags.ConcFlags.ctxtSwitchTime = 0;
		else {
		    I_ cst; /* tmp */

		    /* Convert to milliseconds */
		    cst = (I_) ((atof(rts_argv[arg]+2) * 1000));
		    cst = (cst / CS_MIN_MILLISECS) * CS_MIN_MILLISECS;
		    if (cst < CS_MIN_MILLISECS)
			cst = CS_MIN_MILLISECS;

		    RtsFlags.ConcFlags.ctxtSwitchTime = cst;
		}
    	    	break;

#ifdef SMP
	      case 'N':
		if (rts_argv[arg][2] != '\0') {
		    RtsFlags.ConcFlags.nNodes
		      = strtol(rts_argv[arg]+2, (char **) NULL, 10);
		    if (RtsFlags.ConcFlags.nNodes <= 0) {
			fprintf(stderr, "setupRtsFlags: bad value for -N\n");
			error = rtsTrue;
		    }
		}
		break;
#endif
	      /* =========== PARALLEL =========================== */
	      case 'e':
		PAR_BUILD_ONLY(
		if (rts_argv[arg][2] != '\0') { /* otherwise, stick w/ the default */

		    RtsFlags.ParFlags.maxLocalSparks
		      = strtol(rts_argv[arg]+2, (char **) NULL, 10);

		    if (RtsFlags.ParFlags.maxLocalSparks <= 0) {
			fprintf(stderr, "setupRtsFlags: bad value for -e\n");
			error = rtsTrue;
		    }
		}
		) break;

	      case 'O':
		PAR_BUILD_ONLY(
		RtsFlags.ParFlags.outputDisabled = rtsTrue;
		) break;

	      case 'q': /* activity profile option */
    	    	PAR_BUILD_ONLY(
		if (rts_argv[arg][2] == 'b')
		    RtsFlags.ParFlags.granSimStats_Binary = rtsTrue;
    	    	else
    	    	    RtsFlags.ParFlags.granSimStats = rtsTrue;
		) break;

#if 0 /* or??? */
	      case 'q': /* quasi-parallel profile option */
    	    	GRAN_BUILD_ONLY (
		if (rts_argv[arg][2] == 'v')
		    do_qp_prof = 2;
    	    	else
    	    	    do_qp_prof++;
		) break;
#endif /* 0??? */

	      case 'Q': /* Set pack buffer size */
		PAR_BUILD_ONLY(
		if (rts_argv[arg][2] != '\0') {
		    RtsFlags.ParFlags.packBufferSize = decode(rts_argv[arg]+2);
		} else {
    	    	    fprintf(stderr, "setupRtsFlags: missing size of PackBuffer (for -Q)\n");
    	    	    error = rtsTrue;
    	    	}
		) break;

	      /* =========== GRAN =============================== */

    	      case 'b':
		GRAN_BUILD_ONLY(
		process_gran_option(arg, rts_argc, rts_argv, &error);
		) break;

	      /* =========== TICKY ============================== */

	      case 'r': /* Basic profiling stats */
		TICKY_BUILD_ONLY(

		RtsFlags.TickyFlags.showTickyStats = rtsTrue;
		RtsFlags.TickyFlags.tickyFile
		  = open_stats_file(arg, *argc, argv,
			*rts_argc, rts_argv, TICKY_FILENAME_FMT);

		if (RtsFlags.TickyFlags.tickyFile == NULL) error = rtsTrue;
	        ) break;

	      /* =========== OH DEAR ============================ */
	      default:
		fprintf(stderr, "setupRtsFlags: Unknown RTS option: %s\n",rts_argv[arg]);
		error = rtsTrue;
		break;
	    }
	}
    }
    if (error) {
	const char **p;

        fflush(stdout);
	for (p = usage_text; *p; p++)
	    fprintf(stderr, "%s\n", *p);
	stg_exit(EXIT_FAILURE);
    }

}

static FILE *		/* return NULL on error */
open_stats_file (
    I_ arg,
    int argc, char *argv[],
    int rts_argc, char *rts_argv[],
    const char *FILENAME_FMT)
{
    FILE *f = NULL;

    if (strequal(rts_argv[arg]+2, "stderr")) /* use real stderr */
	f = stderr;
    else if (rts_argv[arg][2] != '\0')	    /* stats file specified */
	f = fopen(rts_argv[arg]+2,"w");
    else {
	char stats_filename[STATS_FILENAME_MAXLEN]; /* default <program>.<ext> */
	sprintf(stats_filename, FILENAME_FMT, argv[0]);
	f = fopen(stats_filename,"w");
    }
    if (f == NULL) {
	fprintf(stderr, "Can't open stats file %s\n", rts_argv[arg]+2);
    } else {
	/* Write argv and rtsv into start of stats file */
	I_ count;
	for(count = 0; count < argc; count++)
	    fprintf(f, "%s ", argv[count]);
	fprintf(f, "+RTS ");
	for(count = 0; count < rts_argc; count++)
	    fprintf(f, "%s ", rts_argv[count]);
	fprintf(f, "\n");
    }

    return(f);
}

static I_
decode(const char *s)
{
    I_ c;
    StgDouble m;

    if (!*s)
	return 0;

    m = atof(s);
    c = s[strlen(s)-1];

    if (c == 'g' || c == 'G')
	m *= 1000*1000*1000;	/* UNchecked! */
    else if (c == 'm' || c == 'M')
	m *= 1000*1000;			/* We do not use powers of 2 (1024) */
    else if (c == 'k' || c == 'K')	/* to avoid possible bad effects on */
	m *= 1000;			/* a direct-mapped cache.   	    */ 
    else if (c == 'w' || c == 'W')
	m *= sizeof(W_);

    return (I_)m;
}

static void
bad_option(const char *s)
{
  fflush(stdout);
  fprintf(stderr, "initSM: Bad RTS option: %s\n", s);
  stg_exit(EXIT_FAILURE);
}		
