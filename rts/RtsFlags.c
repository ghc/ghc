/* -----------------------------------------------------------------------------
 *
 * (c) The AQUA Project, Glasgow University, 1994-1997
 * (c) The GHC Team, 1998-2006
 *
 * Functions for parsing the argument list.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "RtsOpts.h"
#include "RtsUtils.h"
#include "Profiling.h"

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

#include <string.h>

// Flag Structure
RTS_FLAGS RtsFlags;

/*
 * Split argument lists
 */
int     prog_argc = 0;    /* an "int" so as to match normal "argc" */
char  **prog_argv = NULL;
int     full_prog_argc = 0;    /* an "int" so as to match normal "argc" */
char  **full_prog_argv = NULL;
char   *prog_name = NULL; /* 'basename' of prog_argv[0] */
int     rts_argc = 0;  /* ditto */
char   *rts_argv[MAX_RTS_ARGS];

/*
 * constants, used later 
 */
#define RTS 1
#define PGM 0

/* -----------------------------------------------------------------------------
   Static function decls
   -------------------------------------------------------------------------- */

static int		/* return NULL on error */
open_stats_file (
    I_ arg,
    int argc, char *argv[],
    int rts_argc, char *rts_argv[],
    const char *FILENAME_FMT,
    FILE **file_ret);

static StgWord64 decodeSize(const char *flag, nat offset, StgWord64 min, StgWord64 max);
static void bad_option(const char *s);
#ifdef TRACING
static void read_trace_flags(char *arg);
#endif

/* -----------------------------------------------------------------------------
 * Command-line option parsing routines.
 * ---------------------------------------------------------------------------*/

void initRtsFlagsDefaults(void)
{
    RtsFlags.GcFlags.statsFile		= NULL;
    RtsFlags.GcFlags.giveStats		= NO_GC_STATS;

    RtsFlags.GcFlags.maxStkSize		= (8 * 1024 * 1024) / sizeof(W_);
    RtsFlags.GcFlags.initialStkSize	= 1024 / sizeof(W_);

    RtsFlags.GcFlags.minAllocAreaSize   = (512 * 1024)        / BLOCK_SIZE;
    RtsFlags.GcFlags.minOldGenSize      = (1024 * 1024)       / BLOCK_SIZE;
    RtsFlags.GcFlags.maxHeapSize	= 0;    /* off by default */
    RtsFlags.GcFlags.heapSizeSuggestion	= 0;    /* none */
    RtsFlags.GcFlags.heapSizeSuggestionAuto = rtsFalse;
    RtsFlags.GcFlags.pcFreeHeap		= 3;	/* 3% */
    RtsFlags.GcFlags.oldGenFactor       = 2;
    RtsFlags.GcFlags.generations        = 2;
    RtsFlags.GcFlags.squeezeUpdFrames	= rtsTrue;
    RtsFlags.GcFlags.compact            = rtsFalse;
    RtsFlags.GcFlags.compactThreshold   = 30.0;
    RtsFlags.GcFlags.sweep              = rtsFalse;
#ifdef RTS_GTK_FRONTPANEL
    RtsFlags.GcFlags.frontpanel         = rtsFalse;
#endif
    RtsFlags.GcFlags.idleGCDelayTime    = 300; /* millisecs */

#if osf3_HOST_OS
/* ToDo: Perhaps by adjusting this value we can make linking without
 * -static work (i.e., not generate a core-dumping executable)? */
# if SIZEOF_VOID_P == 8
    RtsFlags.GcFlags.heapBase           = 0x180000000L;
# else
#  error I have no idea where to begin the heap on a non-64-bit osf3 machine.
# endif
#else
    RtsFlags.GcFlags.heapBase           = 0;   /* means don't care */
#endif

#ifdef DEBUG
    RtsFlags.DebugFlags.scheduler	= rtsFalse;
    RtsFlags.DebugFlags.interpreter	= rtsFalse;
    RtsFlags.DebugFlags.weak		= rtsFalse;
    RtsFlags.DebugFlags.gccafs		= rtsFalse;
    RtsFlags.DebugFlags.gc		= rtsFalse;
    RtsFlags.DebugFlags.block_alloc	= rtsFalse;
    RtsFlags.DebugFlags.sanity		= rtsFalse;
    RtsFlags.DebugFlags.stable		= rtsFalse;
    RtsFlags.DebugFlags.stm             = rtsFalse;
    RtsFlags.DebugFlags.prof		= rtsFalse;
    RtsFlags.DebugFlags.apply		= rtsFalse;
    RtsFlags.DebugFlags.linker		= rtsFalse;
    RtsFlags.DebugFlags.squeeze		= rtsFalse;
    RtsFlags.DebugFlags.hpc		= rtsFalse;
    RtsFlags.DebugFlags.sparks		= rtsFalse;
#endif

#if defined(PROFILING)
    RtsFlags.CcFlags.doCostCentres	= 0;
#endif /* PROFILING */

    RtsFlags.ProfFlags.doHeapProfile      = rtsFalse;
    RtsFlags.ProfFlags.profileInterval    = 100;

#ifdef PROFILING
    RtsFlags.ProfFlags.includeTSOs        = rtsFalse;
    RtsFlags.ProfFlags.showCCSOnException = rtsFalse;
    RtsFlags.ProfFlags.maxRetainerSetSize = 8;
    RtsFlags.ProfFlags.ccsLength          = 25;
    RtsFlags.ProfFlags.modSelector        = NULL;
    RtsFlags.ProfFlags.descrSelector      = NULL;
    RtsFlags.ProfFlags.typeSelector       = NULL;
    RtsFlags.ProfFlags.ccSelector         = NULL;
    RtsFlags.ProfFlags.ccsSelector        = NULL;
    RtsFlags.ProfFlags.retainerSelector   = NULL;
    RtsFlags.ProfFlags.bioSelector        = NULL;
#endif

#ifdef TRACING
    RtsFlags.TraceFlags.tracing       = TRACE_NONE;
    RtsFlags.TraceFlags.timestamp     = rtsFalse;
    RtsFlags.TraceFlags.scheduler     = rtsFalse;
#endif

    RtsFlags.MiscFlags.tickInterval	= 20;  /* In milliseconds */
    RtsFlags.ConcFlags.ctxtSwitchTime	= 20;  /* In milliseconds */

    RtsFlags.MiscFlags.install_signal_handlers = rtsTrue;
    RtsFlags.MiscFlags.machineReadable = rtsFalse;
    RtsFlags.MiscFlags.linkerMemBase    = 0;

#ifdef THREADED_RTS
    RtsFlags.ParFlags.nNodes	        = 1;
    RtsFlags.ParFlags.migrate           = rtsTrue;
    RtsFlags.ParFlags.wakeupMigrate     = rtsFalse;
    RtsFlags.ParFlags.parGcEnabled      = 1;
    RtsFlags.ParFlags.parGcGen          = 0;
    RtsFlags.ParFlags.parGcLoadBalancingEnabled = rtsTrue;
    RtsFlags.ParFlags.parGcLoadBalancingGen = 1;
    RtsFlags.ParFlags.setAffinity       = 0;
#endif

#if defined(THREADED_RTS)
    RtsFlags.ParFlags.maxLocalSparks	= 4096;
#endif /* THREADED_RTS */

#ifdef TICKY_TICKY
    RtsFlags.TickyFlags.showTickyStats	 = rtsFalse;
    RtsFlags.TickyFlags.tickyFile	 = NULL;
#endif

#ifdef USE_PAPI
    /* By default no special measurements taken */
    RtsFlags.PapiFlags.eventType        = 0;
    RtsFlags.PapiFlags.numUserEvents    = 0;
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
"  -?       Prints this message and exits; the program is not executed",
"  --info   Print information about the RTS used by this program",
"",
"  -K<size> Sets the maximum stack size (default 8M)  Egs: -K32k   -K512k",
"  -k<size> Sets the initial thread stack size (default 1k)  Egs: -k4k   -k2m",
"",
"  -A<size> Sets the minimum allocation area size (default 512k) Egs: -A1m -A10k",
"  -M<size> Sets the maximum heap size (default unlimited)  Egs: -M256k -M1G",
"  -H<size> Sets the minimum heap size (default 0M)   Egs: -H24m  -H1G",
"  -m<n>    Minimum % of heap which must be available (default 3%)",
"  -G<n>    Number of generations (default: 2)",
"  -c<n>    Use in-place compaction instead of copying in the oldest generation",
"           when live data is at least <n>% of the maximum heap size set with",
"           -M (default: 30%)",
"  -c       Use in-place compaction for all oldest generation collections",
"           (the default is to use copying)",
"  -w       Use mark-region for the oldest generation (experimental)",
#if defined(THREADED_RTS)
"  -I<sec>  Perform full GC after <sec> idle time (default: 0.3, 0 == off)",
#endif
"",
"  -t[<file>] One-line GC statistics (if <file> omitted, uses stderr)",
"  -s[<file>] Summary  GC statistics (if <file> omitted, uses stderr)",
"  -S[<file>] Detailed GC statistics (if <file> omitted, uses stderr)",
#ifdef RTS_GTK_FRONTPANEL
"  -f       Display front panel (requires X11 & GTK+)",
#endif
"",
"",
"  -Z       Don't squeeze out update frames on stack overflow",
"  -B       Sound the bell at the start of each garbage collection",
#if defined(PROFILING)
"",
"  -px      Time/allocation profile (XML)  (output file <program>.prof)",
"  -p       Time/allocation profile        (output file <program>.prof)",
"  -P       More detailed Time/Allocation profile",
"  -Pa      Give information about *all* cost centres",

# if defined(PROFILING)
"",
"  -hx            Heap residency profile (XML)   (output file <program>.prof)",
"  -h<break-down> Heap residency profile (hp2ps) (output file <program>.hp)",
"     break-down: c = cost centre stack (default)",
"                 m = module",
"                 d = closure description",
"                 y = type description",
"                 r = retainer",
"                 b = biography (LAG,DRAG,VOID,USE)",
"  A subset of closures may be selected thusly:",
"    -hc<cc>,...  specific cost centre(s) (top of stack only)",
"    -hC<cc>,...  specific cost centre(s) (anywhere in stack)",
"    -hm<mod>...  all cost centres from the specified modules(s)",
"    -hd<des>,... closures with specified closure descriptions",
"    -hy<typ>...  closures with specified type descriptions",
"    -hr<cc>...   closures with specified retainers",
"    -hb<bio>...  closures with specified biographies (lag,drag,void,use)",
"",
"  -R<size>       Set the maximum retainer set size (default: 8)",
"", 
"  -L<chars>      Maximum length of a cost-centre stack in a heap profile",
"                 (default: 25)",
"",
"  -xt            Include threads (TSOs) in a heap profile",
"",
"  -xc      Show current cost centre stack on raising an exception",
# endif
#endif /* PROFILING or PAR */

#ifdef TRACING
"",
"  -l[flags]  Log events in binary format to the file <program>.eventlog",
#  ifdef DEBUG
"  -v[flags]  Log events to stderr",
#  endif
"             where [flags] can contain:",
"                s    scheduler events",
#  ifdef DEBUG
"                t    add time stamps (only useful with -v)",
#  endif
#endif

#if !defined(PROFILING)
"",
"  -hT      Heap residency profile (output file <program>.hp)",
#endif
"  -i<sec>  Time between heap samples (seconds, default: 0.1)",
"",
#if defined(TICKY_TICKY)
"  -r<file>  Produce ticky-ticky statistics (with -rstderr for stderr)",
"",
#endif
"  -C<secs>  Context-switch interval in seconds.",
"            0 or no argument means switch as often as possible.",
"            Default: 0.02 sec; resolution is set by -V below.",
"  -V<secs>  Master tick interval in seconds (0 == disable timer).",
"            This sets the resolution for -C and the profile timer -i.",
"            Default: 0.02 sec.",
"",
#if defined(DEBUG)
"  -Ds  DEBUG: scheduler",
"  -Di  DEBUG: interpreter",
"  -Dw  DEBUG: weak",
"  -DG  DEBUG: gccafs",
"  -Dg  DEBUG: gc",
"  -Db  DEBUG: block",
"  -DS  DEBUG: sanity",
"  -Dt  DEBUG: stable",
"  -Dp  DEBUG: prof",
"  -De  DEBUG: event logging",
"  -Da  DEBUG: apply",
"  -Dl  DEBUG: linker",
"  -Dm  DEBUG: stm",
"  -Dz  DEBUG: stack squezing",
"  -Dc  DEBUG: program coverage",
"  -Dr  DEBUG: sparks",
"",
"     NOTE: DEBUG events are sent to stderr by default; add -l to create a",
"     binary event log file instead.",
"",
#endif /* DEBUG */
#if defined(THREADED_RTS) && !defined(NOSMP)
"  -N<n>     Use <n> processors (default: 1)",
"  -N        Determine the number of processors to use automatically",
"  -qg[<n>]  Use parallel GC only for generations >= <n>",
"            (default: 0, -qg alone turns off parallel GC)",
"  -qb[<n>]  Use load-balancing in the parallel GC only for generations >= <n>",
"            (default: 1, -qb alone turns off load-balancing)",
"  -qa       Use the OS to set thread affinity (experimental)",
"  -qm       Don't automatically migrate threads between CPUs",
"  -qw       Migrate a thread to the current CPU when it is woken up",
#endif
"  --install-signal-handlers=<yes|no>",
"            Install signal handlers (default: yes)",
#if defined(THREADED_RTS)
"  -e<n>     Maximum number of outstanding local sparks (default: 4096)",
#endif
#if defined(x86_64_HOST_ARCH)
"  -xm       Base address to mmap memory in the GHCi linker",
"            (hex; must be <80000000)",
#endif
#if defined(USE_PAPI)
"  -aX       CPU performance counter measurements using PAPI",
"            (use with the -s<file> option).  X is one of:",
"",
/* "            y - cycles", */
"            1 - level 1 cache misses",
"            2 - level 2 cache misses",
"            b - branch mispredictions",
"            s - stalled cycles",
"            e - cache miss and branch misprediction events",
"            +PAPI_EVENT   - collect papi preset event PAPI_EVENT",
"            #NATIVE_EVENT - collect native event NATIVE_EVENT (in hex)",
#endif
"",
"RTS options may also be specified using the GHCRTS environment variable.",
"",
"Other RTS options may be available for programs compiled a different way.",
"The GHC User's Guide has full details.",
"",
0
};

STATIC_INLINE rtsBool
strequal(const char *a, const char * b)
{
    return(strcmp(a, b) == 0);
}

static void
splitRtsFlags(char *s, int *rts_argc, char *rts_argv[])
{
    char *c1, *c2;

    c1 = s;
    do {
	while (isspace(*c1)) { c1++; };
	c2 = c1;
	while (!isspace(*c2) && *c2 != '\0') { c2++; };
	
	if (c1 == c2) { break; }
	
	if (*rts_argc < MAX_RTS_ARGS-1) {
	    s = stgMallocBytes(c2-c1+1, "RtsFlags.c:splitRtsFlags()");
	    strncpy(s, c1, c2-c1);
	    s[c2-c1] = '\0';
	    rts_argv[(*rts_argc)++] = s;
	} else {
	    barf("too many RTS arguments (max %d)", MAX_RTS_ARGS-1);
	}
	
	c1 = c2;
    } while (*c1 != '\0');
}
    
void
setupRtsFlags(int *argc, char *argv[], int *rts_argc, char *rts_argv[])
{
    rtsBool error = rtsFalse;
    I_ mode;
    I_ arg, total_arg;

    setProgName (argv);
    total_arg = *argc;
    arg = 1;

    *argc = 1;
    *rts_argc = 0;

    // process arguments from the ghc_rts_opts global variable first.
    // (arguments from the GHCRTS environment variable and the command
    // line override these).
    {
	if (ghc_rts_opts != NULL) {
	    splitRtsFlags(ghc_rts_opts, rts_argc, rts_argv);
	}
    }

    // process arguments from the GHCRTS environment variable next
    // (arguments from the command line override these).
    {
	char *ghc_rts = getenv("GHCRTS");

	if (ghc_rts != NULL) {
            if (rtsOptsEnabled != rtsOptsNone) {
                splitRtsFlags(ghc_rts, rts_argc, rts_argv);
            }
            else {
                errorBelch("Warning: Ignoring GHCRTS variable as RTS options are disabled.\n         Link with -rtsopts to enable them.");
                // We don't actually exit, just warn
            }
	}
    }

    // Split arguments (argv) into PGM (argv) and RTS (rts_argv) parts
    //   argv[0] must be PGM argument -- leave in argv

    for (mode = PGM; arg < total_arg; arg++) {
	// The '--RTS' argument disables all future +RTS ... -RTS processing.
	if (strequal("--RTS", argv[arg])) {
	    arg++;
	    break;
	}
	// The '--' argument is passed through to the program, but
	// disables all further +RTS ... -RTS processing.
	else if (strequal("--", argv[arg])) {
	    break;
	}
	else if (strequal("+RTS", argv[arg])) {
            if (rtsOptsEnabled != rtsOptsNone) {
                mode = RTS;
            }
            else {
                errorBelch("RTS options are disabled. Link with -rtsopts to enable them.");
                stg_exit(EXIT_FAILURE);
            }
	}
	else if (strequal("-RTS", argv[arg])) {
	    mode = PGM;
	}
	else if (mode == RTS && *rts_argc < MAX_RTS_ARGS-1) {
	    if ((rtsOptsEnabled == rtsOptsAll) ||
            strequal(argv[arg], "--info")) {
            rts_argv[(*rts_argc)++] = argv[arg];
        }
        else {
            errorBelch("Most RTS options are disabled. Link with -rtsopts to enable them.");
            stg_exit(EXIT_FAILURE);
        }
	}
	else if (mode == PGM) {
	    argv[(*argc)++] = argv[arg];
	}
	else {
	  barf("too many RTS arguments (max %d)", MAX_RTS_ARGS-1);
	}
    }
    // process remaining program arguments
    for (; arg < total_arg; arg++) {
	argv[(*argc)++] = argv[arg];
    }
    argv[*argc] = (char *) 0;
    rts_argv[*rts_argc] = (char *) 0;

    // Process RTS (rts_argv) part: mainly to determine statsfile
    for (arg = 0; arg < *rts_argc; arg++) {
	if (rts_argv[arg][0] != '-') {
	    fflush(stdout);
	    errorBelch("unexpected RTS argument: %s", rts_argv[arg]);
	    error = rtsTrue;

        } else {
	    switch(rts_argv[arg][1]) {

	      /* process: general args, then PROFILING-only ones, then
		 CONCURRENT-only, TICKY-only (same order as defined in
		 RtsFlags.lh); within those groups, mostly in
		 case-insensitive alphabetical order.  Final group is
		 x*, which allows for more options.
	      */

#ifdef TICKY_TICKY
# define TICKY_BUILD_ONLY(x) x
#else
# define TICKY_BUILD_ONLY(x) \
errorBelch("the flag %s requires the program to be built with -ticky", rts_argv[arg]); \
error = rtsTrue;
#endif

#ifdef PROFILING
# define PROFILING_BUILD_ONLY(x)   x
#else
# define PROFILING_BUILD_ONLY(x) \
errorBelch("the flag %s requires the program to be built with -prof", rts_argv[arg]); \
error = rtsTrue;
#endif

#ifdef TRACING
# define TRACING_BUILD_ONLY(x)   x
#else
# define TRACING_BUILD_ONLY(x) \
errorBelch("the flag %s requires the program to be built with -eventlog or -debug", rts_argv[arg]); \
error = rtsTrue;
#endif

#ifdef THREADED_RTS
# define THREADED_BUILD_ONLY(x)      x
#else
# define THREADED_BUILD_ONLY(x) \
errorBelch("the flag %s requires the program to be built with -threaded", rts_argv[arg]); \
error = rtsTrue;
#endif

#ifdef DEBUG
# define DEBUG_BUILD_ONLY(x) x
#else
# define DEBUG_BUILD_ONLY(x) \
errorBelch("the flag %s requires the program to be built with -debug", rts_argv[arg]); \
error = rtsTrue;
#endif

	      /* =========== GENERAL ========================== */
	      case '?':
		error = rtsTrue;
		break;

              /* This isn't going to allow us to keep related options
                 together as we add more --* flags. We really need a
                 proper options parser. */
	      case '-':
                  if (strequal("install-signal-handlers=yes",
                               &rts_argv[arg][2])) {
                      RtsFlags.MiscFlags.install_signal_handlers = rtsTrue;
                  }
                  else if (strequal("install-signal-handlers=no",
                               &rts_argv[arg][2])) {
                      RtsFlags.MiscFlags.install_signal_handlers = rtsFalse;
                  }
                  else if (strequal("machine-readable",
                               &rts_argv[arg][2])) {
                      RtsFlags.MiscFlags.machineReadable = rtsTrue;
                  }
                  else if (strequal("info",
                               &rts_argv[arg][2])) {
                      printRtsInfo();
                      stg_exit(0);
                  }
                  else {
		      errorBelch("unknown RTS option: %s",rts_argv[arg]);
		      error = rtsTrue;
                  }
		  break;
	      case 'A':
                  RtsFlags.GcFlags.minAllocAreaSize
                      = decodeSize(rts_argv[arg], 2, BLOCK_SIZE, HS_INT_MAX)
                           / BLOCK_SIZE;
                  break;

#ifdef USE_PAPI
	      case 'a':
		switch(rts_argv[arg][2]) {
		case '1':
		  RtsFlags.PapiFlags.eventType = PAPI_FLAG_CACHE_L1;
		  break;
		case '2':
		  RtsFlags.PapiFlags.eventType = PAPI_FLAG_CACHE_L2;
		  break;
		case 'b':
		  RtsFlags.PapiFlags.eventType = PAPI_FLAG_BRANCH;
		  break;
		case 's':
		  RtsFlags.PapiFlags.eventType = PAPI_FLAG_STALLS;
		  break;
		case 'e':
		  RtsFlags.PapiFlags.eventType = PAPI_FLAG_CB_EVENTS;
		  break;
                case '+':
                case '#':
                  if (RtsFlags.PapiFlags.numUserEvents >= MAX_PAPI_USER_EVENTS) {
                      errorBelch("maximum number of PAPI events reached");
                      stg_exit(EXIT_FAILURE);
                  }
                  nat eventNum  = RtsFlags.PapiFlags.numUserEvents++;
                  char kind     = rts_argv[arg][2];
                  nat eventKind = kind == '+' ? PAPI_PRESET_EVENT_KIND : PAPI_NATIVE_EVENT_KIND;

                  RtsFlags.PapiFlags.userEvents[eventNum] = rts_argv[arg] + 3;
                  RtsFlags.PapiFlags.eventType = PAPI_USER_EVENTS;
                  RtsFlags.PapiFlags.userEventsKind[eventNum] = eventKind;
                  break;
		default:
		  bad_option( rts_argv[arg] );
		}
		break;
#endif

	      case 'B':
		RtsFlags.GcFlags.ringBell = rtsTrue;
		break;

	      case 'c':
		  if (rts_argv[arg][2] != '\0') {
		      RtsFlags.GcFlags.compactThreshold =
			  atof(rts_argv[arg]+2);
		  } else {
		      RtsFlags.GcFlags.compact = rtsTrue;
		  }
		  break;

              case 'w':
		RtsFlags.GcFlags.sweep = rtsTrue;
		break;

	      case 'F':
	        RtsFlags.GcFlags.oldGenFactor = atof(rts_argv[arg]+2);
	      
		if (RtsFlags.GcFlags.oldGenFactor < 0)
		  bad_option( rts_argv[arg] );
		break;
	      
	      case 'D':
              DEBUG_BUILD_ONLY(
	      { 
		  char *c;

		  for (c  = rts_argv[arg] + 2; *c != '\0'; c++) {
		      switch (*c) {
		      case 's':
			  RtsFlags.DebugFlags.scheduler = rtsTrue;
			  break;
		      case 'i':
			  RtsFlags.DebugFlags.interpreter = rtsTrue;
			  break;
		      case 'w':
			  RtsFlags.DebugFlags.weak = rtsTrue;
			  break;
		      case 'G':
			  RtsFlags.DebugFlags.gccafs = rtsTrue;
			  break;
		      case 'g':
			  RtsFlags.DebugFlags.gc = rtsTrue;
			  break;
		      case 'b':
			  RtsFlags.DebugFlags.block_alloc = rtsTrue;
			  break;
		      case 'S':
			  RtsFlags.DebugFlags.sanity = rtsTrue;
			  break;
		      case 't':
			  RtsFlags.DebugFlags.stable = rtsTrue;
			  break;
		      case 'p':
			  RtsFlags.DebugFlags.prof = rtsTrue;
			  break;
		      case 'l':
			  RtsFlags.DebugFlags.linker = rtsTrue;
			  break;
		      case 'a':
			  RtsFlags.DebugFlags.apply = rtsTrue;
			  break;
		      case 'm':
			  RtsFlags.DebugFlags.stm = rtsTrue;
			  break;
		      case 'z':
			  RtsFlags.DebugFlags.squeeze = rtsTrue;
			  break;
		      case 'c':
			  RtsFlags.DebugFlags.hpc = rtsTrue;
			  break;
		      case 'r':
			  RtsFlags.DebugFlags.sparks = rtsTrue;
			  break;
		      default:
			  bad_option( rts_argv[arg] );
		      }
		  }
                  // -Dx also turns on -v.  Use -l to direct trace
                  // events to the .eventlog file instead.
                  RtsFlags.TraceFlags.tracing = TRACE_STDERR;
	      })
              break;

	      case 'K':
                  RtsFlags.GcFlags.maxStkSize =
                      decodeSize(rts_argv[arg], 2, 1, HS_WORD_MAX) / sizeof(W_);
                  break;

	      case 'k':
                  RtsFlags.GcFlags.initialStkSize =
                      decodeSize(rts_argv[arg], 2, 1, HS_WORD_MAX) / sizeof(W_);
                  break;

	      case 'M':
                  RtsFlags.GcFlags.maxHeapSize =
                      decodeSize(rts_argv[arg], 2, BLOCK_SIZE, HS_WORD_MAX) / BLOCK_SIZE;
                  /* user give size in *bytes* but "maxHeapSize" is in *blocks* */
                  break;

	      case 'm':
                  RtsFlags.GcFlags.pcFreeHeap = atof(rts_argv[arg]+2);

                  if (RtsFlags.GcFlags.pcFreeHeap < 0 ||
                      RtsFlags.GcFlags.pcFreeHeap > 100)
                      bad_option( rts_argv[arg] );
                  break;

	      case 'G':
                  RtsFlags.GcFlags.generations =
                      decodeSize(rts_argv[arg], 2, 1, HS_INT_MAX);
                  break;

	      case 'H':
                  if (rts_argv[arg][2] == '\0') {
                      RtsFlags.GcFlags.heapSizeSuggestionAuto = rtsTrue;
                  } else {
                      RtsFlags.GcFlags.heapSizeSuggestion =
                          (nat)(decodeSize(rts_argv[arg], 2, BLOCK_SIZE, HS_WORD_MAX) / BLOCK_SIZE);
                  }
                  break;

#ifdef RTS_GTK_FRONTPANEL
	      case 'f':
		  RtsFlags.GcFlags.frontpanel = rtsTrue;
		  break;
#endif

    	      case 'I':	/* idle GC delay */
		if (rts_argv[arg][2] == '\0') {
		  /* use default */
		} else {
		    I_ cst; /* tmp */

		    /* Convert to millisecs */
		    cst = (I_) ((atof(rts_argv[arg]+2) * 1000));
		    RtsFlags.GcFlags.idleGCDelayTime = cst;
		}
		break;

	      case 'S':
		  RtsFlags.GcFlags.giveStats = VERBOSE_GC_STATS;
		  goto stats;

	      case 's':
		  RtsFlags.GcFlags.giveStats = SUMMARY_GC_STATS;
		  goto stats;

	      case 't':
		  RtsFlags.GcFlags.giveStats = ONELINE_GC_STATS;
		  goto stats;

	    stats:
		{ 
		    int r;
		    r = open_stats_file(arg, *argc, argv,
					*rts_argc, rts_argv, NULL,
					&RtsFlags.GcFlags.statsFile);
		    if (r == -1) { error = rtsTrue; }
		}
                break;

	      case 'Z':
		RtsFlags.GcFlags.squeezeUpdFrames = rtsFalse;
		break;

	      /* =========== PROFILING ========================== */

	      case 'P': /* detailed cost centre profiling (time/alloc) */
	      case 'p': /* cost centre profiling (time/alloc) */
		PROFILING_BUILD_ONLY(
		switch (rts_argv[arg][2]) {
		  case 'x':
		    RtsFlags.CcFlags.doCostCentres = COST_CENTRES_XML;
		    break;
		  case 'a':
		    RtsFlags.CcFlags.doCostCentres = COST_CENTRES_ALL;
		    break;
		  default:
		      if (rts_argv[arg][1] == 'P') {
			  RtsFlags.CcFlags.doCostCentres =
			      COST_CENTRES_VERBOSE;
		      } else {
			  RtsFlags.CcFlags.doCostCentres =
			      COST_CENTRES_SUMMARY;
		      }
		      break;
		}
		) break;

	      case 'R':
		  PROFILING_BUILD_ONLY(
		      RtsFlags.ProfFlags.maxRetainerSetSize = atof(rts_argv[arg]+2);
  	          ) break;
	      case 'L':
		  PROFILING_BUILD_ONLY(
		      RtsFlags.ProfFlags.ccsLength = atof(rts_argv[arg]+2);
                      if(RtsFlags.ProfFlags.ccsLength <= 0) {
			bad_option(rts_argv[arg]);
                      }
		  ) break;
	      case 'h': /* serial heap profile */
#if !defined(PROFILING)
		switch (rts_argv[arg][2]) {
		  case '\0':
		  case 'T':
		    RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_CLOSURE_TYPE;
		    break;
		  default:
		    errorBelch("invalid heap profile option: %s",rts_argv[arg]);
		    error = rtsTrue;
		}
#else
		PROFILING_BUILD_ONLY(
		switch (rts_argv[arg][2]) {
		case '\0':
		case 'C':
		case 'c':
		case 'M':
		case 'm':
		case 'D':
		case 'd':
		case 'Y':
		case 'y':
		case 'R':
		case 'r':
		case 'B':
		case 'b':
		    if (rts_argv[arg][2] != '\0' && rts_argv[arg][3] != '\0') {
			{
			    char *left  = strchr(rts_argv[arg], '{');
			    char *right = strrchr(rts_argv[arg], '}');

			    // curly braces are optional, for
			    // backwards compat.
			    if (left)
				left = left+1;
			    else
				left = rts_argv[arg] + 3;

			    if (!right)
				right = rts_argv[arg] + strlen(rts_argv[arg]);

			    *right = '\0';

			    switch (rts_argv[arg][2]) {
			    case 'c': // cost centre label select
				RtsFlags.ProfFlags.ccSelector = left;
				break;
			    case 'C':
				RtsFlags.ProfFlags.ccsSelector = left;
				break;
			    case 'M':
			    case 'm': // cost centre module select
				RtsFlags.ProfFlags.modSelector = left;
				break;
			    case 'D':
			    case 'd': // closure descr select 
				RtsFlags.ProfFlags.descrSelector = left;
				break;
			    case 'Y':
			    case 'y': // closure type select
				RtsFlags.ProfFlags.typeSelector = left;
				break;
			    case 'R':
			    case 'r': // retainer select
				RtsFlags.ProfFlags.retainerSelector = left;
				break;
			    case 'B':
			    case 'b': // biography select
				RtsFlags.ProfFlags.bioSelector = left;
				break;
			    }
			}
			break;
		    }

		    if (RtsFlags.ProfFlags.doHeapProfile != 0) {
			errorBelch("multiple heap profile options");
			error = rtsTrue;
			break;
		    }

		    switch (rts_argv[arg][2]) {
		    case '\0':
		    case 'C':
		    case 'c':
			RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_CCS;
			break;
		    case 'M':
		    case 'm':
			  RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_MOD;
			  break;
		    case 'D':
		    case 'd':
			  RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_DESCR;
			  break;
		    case 'Y':
		    case 'y':
			  RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_TYPE;
			  break;
		    case 'R':
		    case 'r':
			  RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_RETAINER;
			  break;
		    case 'B':
		    case 'b':
			  RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_LDV;
			  break;
		    }
		    break;
		      
		default:
		    errorBelch("invalid heap profile option: %s",rts_argv[arg]);
		    error = rtsTrue;
		}
		) 
#endif /* PROFILING */
    	    	break;

    	      case 'i':	/* heap sample interval */
		if (rts_argv[arg][2] == '\0') {
		  /* use default */
		} else {
		    I_ cst; /* tmp */

		    /* Convert to milliseconds */
		    cst = (I_) ((atof(rts_argv[arg]+2) * 1000));
		    RtsFlags.ProfFlags.profileInterval = cst;
		}
		break;

	      /* =========== CONCURRENT ========================= */
    	      case 'C':	/* context switch interval */
		if (rts_argv[arg][2] == '\0')
    	    	    RtsFlags.ConcFlags.ctxtSwitchTime = 0;
		else {
		    I_ cst; /* tmp */

		    /* Convert to milliseconds */
		    cst = (I_) ((atof(rts_argv[arg]+2) * 1000));
		    RtsFlags.ConcFlags.ctxtSwitchTime = cst;
		}
    	    	break;

              case 'V': /* master tick interval */
                if (rts_argv[arg][2] == '\0') {
                    // turns off ticks completely
                    RtsFlags.MiscFlags.tickInterval = 0;
                } else {
                    I_ cst; /* tmp */

                    /* Convert to milliseconds */
                    cst = (I_) ((atof(rts_argv[arg]+2) * 1000));
                    RtsFlags.MiscFlags.tickInterval = cst;
                }
                break;

#if !defined(NOSMP)
	      case 'N':
		THREADED_BUILD_ONLY(
		if (rts_argv[arg][2] == '\0') {
#if defined(PROFILING)
		    RtsFlags.ParFlags.nNodes = 1;
#else
                    RtsFlags.ParFlags.nNodes = getNumberOfProcessors();
#endif
		} else {
		    RtsFlags.ParFlags.nNodes
		      = strtol(rts_argv[arg]+2, (char **) NULL, 10);
		    if (RtsFlags.ParFlags.nNodes <= 0) {
		      errorBelch("bad value for -N");
		      error = rtsTrue;
		    }
#if defined(PROFILING)
                    if (RtsFlags.ParFlags.nNodes > 1) {
                        errorBelch("bad option %s: only -N1 is supported with profiling", rts_argv[arg]);
		      error = rtsTrue;
                    }
#endif
		}
		) break;

	      case 'g':
		THREADED_BUILD_ONLY(
		    switch (rts_argv[arg][2]) {
                    case '1':
                        // backwards compat only
                        RtsFlags.ParFlags.parGcEnabled = rtsFalse;
                        break;
		    default:
			errorBelch("unknown RTS option: %s",rts_argv[arg]);
			error = rtsTrue;
			break;
                    }
                    ) break;

	      case 'q':
		THREADED_BUILD_ONLY(
		    switch (rts_argv[arg][2]) {
		    case '\0':
			errorBelch("incomplete RTS option: %s",rts_argv[arg]);
			error = rtsTrue;
			break;
                    case 'g':
                        if (rts_argv[arg][3] == '\0') {
                            RtsFlags.ParFlags.parGcEnabled = rtsFalse;
                        } else {
                            RtsFlags.ParFlags.parGcEnabled = rtsTrue;
                            RtsFlags.ParFlags.parGcGen
                                = strtol(rts_argv[arg]+3, (char **) NULL, 10);
                        }
                        break;
		    case 'b':
                        if (rts_argv[arg][3] == '\0') {
                            RtsFlags.ParFlags.parGcLoadBalancingEnabled = rtsFalse;
                        }
                        else {
                            RtsFlags.ParFlags.parGcLoadBalancingEnabled = rtsTrue;
                            RtsFlags.ParFlags.parGcLoadBalancingGen
                                = strtol(rts_argv[arg]+3, (char **) NULL, 10);
                        }
                        break;
		    case 'a':
			RtsFlags.ParFlags.setAffinity = rtsTrue;
			break;
		    case 'm':
			RtsFlags.ParFlags.migrate = rtsFalse;
			break;
		    case 'w':
			RtsFlags.ParFlags.wakeupMigrate = rtsTrue;
			break;
		    default:
			errorBelch("unknown RTS option: %s",rts_argv[arg]);
			error = rtsTrue;
			break;
		    }
                    ) break;
#endif
	      /* =========== PARALLEL =========================== */
	      case 'e':
		THREADED_BUILD_ONLY(
		if (rts_argv[arg][2] != '\0') {
		    RtsFlags.ParFlags.maxLocalSparks
		      = strtol(rts_argv[arg]+2, (char **) NULL, 10);
		    if (RtsFlags.ParFlags.maxLocalSparks <= 0) {
		      errorBelch("bad value for -e");
		      error = rtsTrue;
		    }
		}
		) break;

	      /* =========== TICKY ============================== */

	      case 'r': /* Basic profiling stats */
		TICKY_BUILD_ONLY(

		RtsFlags.TickyFlags.showTickyStats = rtsTrue;

		{ 
		    int r;
		    r = open_stats_file(arg, *argc, argv,
					*rts_argc, rts_argv, TICKY_FILENAME_FMT,
					&RtsFlags.TickyFlags.tickyFile);
		    if (r == -1) { error = rtsTrue; }
		}
	        ) break;

	      /* =========== TRACING ---------=================== */

              case 'l':
                  TRACING_BUILD_ONLY(
                      RtsFlags.TraceFlags.tracing = TRACE_EVENTLOG;
                      read_trace_flags(&rts_argv[arg][2]);
                      );
                  break;

	      case 'v':
                  DEBUG_BUILD_ONLY(
                      RtsFlags.TraceFlags.tracing = TRACE_STDERR;
                      read_trace_flags(&rts_argv[arg][2]);
                      );
                  break;

	      /* =========== EXTENDED OPTIONS =================== */

              case 'x': /* Extend the argument space */
                switch(rts_argv[arg][2]) {
                  case '\0':
		    errorBelch("incomplete RTS option: %s",rts_argv[arg]);
		    error = rtsTrue;
		    break;

                case 'b': /* heapBase in hex; undocumented */
                    if (rts_argv[arg][3] != '\0') {
                        RtsFlags.GcFlags.heapBase
                            = strtol(rts_argv[arg]+3, (char **) NULL, 16);
                    } else {
                        errorBelch("-xb: requires argument");
                        error = rtsTrue;
                    }
                    break;

#if defined(x86_64_HOST_ARCH)
                case 'm': /* linkerMemBase */
                    if (rts_argv[arg][3] != '\0') {
                        RtsFlags.MiscFlags.linkerMemBase
                            = strtol(rts_argv[arg]+3, (char **) NULL, 16);
                        if (RtsFlags.MiscFlags.linkerMemBase > 0x80000000) {
                            errorBelch("-xm: value must be <80000000");
                            error = rtsTrue;
                        }
                    } else {
                        RtsFlags.MiscFlags.linkerMemBase = 0;
                    }
                    break;
#endif

                case 'c': /* Debugging tool: show current cost centre on an exception */
                    PROFILING_BUILD_ONLY(
			RtsFlags.ProfFlags.showCCSOnException = rtsTrue;
			);
		    break;

		case 't':  /* Include memory used by TSOs in a heap profile */
		    PROFILING_BUILD_ONLY(
			RtsFlags.ProfFlags.includeTSOs = rtsTrue;
			);
		    break;

                  /* The option prefix '-xx' is reserved for future extension.  KSW 1999-11. */

	          default:
		    errorBelch("unknown RTS option: %s",rts_argv[arg]);
		    error = rtsTrue;
		    break;
                }
                break;  /* defensive programming */

	      /* =========== OH DEAR ============================ */
	      default:
		errorBelch("unknown RTS option: %s",rts_argv[arg]);
		error = rtsTrue;
		break;
	    }
	}
    }

    if (RtsFlags.MiscFlags.tickInterval < 0) {
        RtsFlags.MiscFlags.tickInterval = 50;
    }

    // If the master timer is disabled, turn off the other timers.
    if (RtsFlags.MiscFlags.tickInterval == 0) {
        RtsFlags.ConcFlags.ctxtSwitchTime  = 0;
        RtsFlags.GcFlags.idleGCDelayTime   = 0;
        RtsFlags.ProfFlags.profileInterval = 0;
    }

    // Determine what tick interval we should use for the RTS timer
    // by taking the shortest of the various intervals that we need to
    // monitor.
    if (RtsFlags.ConcFlags.ctxtSwitchTime > 0) {
        RtsFlags.MiscFlags.tickInterval =
            stg_min(RtsFlags.ConcFlags.ctxtSwitchTime,
                    RtsFlags.MiscFlags.tickInterval);
    }

    if (RtsFlags.GcFlags.idleGCDelayTime > 0) {
        RtsFlags.MiscFlags.tickInterval =
            stg_min(RtsFlags.GcFlags.idleGCDelayTime,
                    RtsFlags.MiscFlags.tickInterval);
    }

    if (RtsFlags.ProfFlags.profileInterval > 0) {
        RtsFlags.MiscFlags.tickInterval =
            stg_min(RtsFlags.ProfFlags.profileInterval,
                    RtsFlags.MiscFlags.tickInterval);
    }

    if (RtsFlags.ConcFlags.ctxtSwitchTime > 0) {
        RtsFlags.ConcFlags.ctxtSwitchTicks =
            RtsFlags.ConcFlags.ctxtSwitchTime /
            RtsFlags.MiscFlags.tickInterval;
    } else {
        RtsFlags.ConcFlags.ctxtSwitchTicks = 0;
    }

    if (RtsFlags.ProfFlags.profileInterval > 0) {
        RtsFlags.ProfFlags.profileIntervalTicks =
            RtsFlags.ProfFlags.profileInterval / 
            RtsFlags.MiscFlags.tickInterval;
    } else {
        RtsFlags.ProfFlags.profileIntervalTicks = 0;
    }

    if (error) {
	const char **p;

        fflush(stdout);
	for (p = usage_text; *p; p++)
	    errorBelch("%s", *p);
	stg_exit(EXIT_FAILURE);
    }
}


static void
stats_fprintf(FILE *f, char *s, ...)
{
    va_list ap;
    va_start(ap,s);
    if (f == NULL) {
	vdebugBelch(s, ap);
    } else {
	vfprintf(f, s, ap);
    }
    va_end(ap);
}

static int		/* return -1 on error */
open_stats_file (
    I_ arg,
    int argc, char *argv[],
    int rts_argc, char *rts_argv[],
    const char *FILENAME_FMT,
    FILE **file_ret)
{
    FILE *f = NULL;

    if (strequal(rts_argv[arg]+2, "stderr")
        || (FILENAME_FMT == NULL && rts_argv[arg][2] == '\0')) {
        f = NULL; /* NULL means use debugBelch */
    } else {
        if (rts_argv[arg][2] != '\0') {  /* stats file specified */
            f = fopen(rts_argv[arg]+2,"w");
        } else {
            char stats_filename[STATS_FILENAME_MAXLEN]; /* default <program>.<ext> */
            sprintf(stats_filename, FILENAME_FMT, argv[0]);
            f = fopen(stats_filename,"w");
        }
	if (f == NULL) {
	    errorBelch("Can't open stats file %s\n", rts_argv[arg]+2);
	    return -1;
	}
    }
    *file_ret = f;

    {
	/* Write argv and rtsv into start of stats file */
	int count;
	for(count = 0; count < argc; count++) {
	    stats_fprintf(f, "%s ", argv[count]);
	}
	stats_fprintf(f, "+RTS ");
	for(count = 0; count < rts_argc; count++)
	    stats_fprintf(f, "%s ", rts_argv[count]);
	stats_fprintf(f, "\n");
    }
    return 0;
}



static StgWord64
decodeSize(const char *flag, nat offset, StgWord64 min, StgWord64 max)
{
    char c;
    const char *s;
    StgDouble m;
    StgWord64 val;

    s = flag + offset;

    if (!*s)
    {
        m = 0;
    }
    else
    {
        m = atof(s);
        c = s[strlen(s)-1];

        if (c == 'g' || c == 'G') 
            m *= 1024*1024*1024;
        else if (c == 'm' || c == 'M')
            m *= 1024*1024;
        else if (c == 'k' || c == 'K')
            m *= 1024;
        else if (c == 'w' || c == 'W')
            m *= sizeof(W_);
    }

    val = (StgWord64)m;

    if (m < 0 || val < min || val > max) {
        // printf doesn't like 64-bit format specs on Windows
        // apparently, so fall back to unsigned long.
        errorBelch("error in RTS option %s: size outside allowed range (%lu - %lu)", flag, (lnat)min, (lnat)max);
        stg_exit(EXIT_FAILURE);
    }

    return val;
}

#if defined(TRACING)
static void read_trace_flags(char *arg)
{
    char *c;

    for (c  = arg; *c != '\0'; c++) {
        switch(*c) {
        case '\0':
            break;
        case 's':
            RtsFlags.TraceFlags.scheduler = rtsTrue;
            break;
        case 't':
            RtsFlags.TraceFlags.timestamp = rtsTrue;
            break;
        case 'g':
            // ignored for backwards-compat
            break;
        default:
            errorBelch("unknown trace option: %c",*c);
            break;
        }
    }
}
#endif

static void GNU_ATTRIBUTE(__noreturn__)
bad_option(const char *s)
{
  errorBelch("bad RTS option: %s", s);
  stg_exit(EXIT_FAILURE);
}

/* -----------------------------------------------------------------------------
   Getting/Setting the program's arguments.

   These are used by System.Environment, and parts of the RTS.
   -------------------------------------------------------------------------- */

void
setProgName(char *argv[])
{
    /* Remove directory from argv[0] -- default files in current directory */
#if !defined(mingw32_HOST_OS)
    char *last_slash;
    if ( (last_slash = (char *) strrchr(argv[0], '/')) != NULL ) {
	prog_name = last_slash+1;
   } else {
	prog_name = argv[0];
   }
#else
    char* last_slash = argv[0] + (strlen(argv[0]) - 1);
    while ( last_slash > argv[0] ) {
	if ( *last_slash == '/' || *last_slash == '\\' ) {
	    prog_name = last_slash+1;
	    return;
	}
	last_slash--;
    }
    prog_name = argv[0];
#endif
}

void
getProgArgv(int *argc, char **argv[])
{
    if (argc) { *argc = prog_argc; }
    if (argv) { *argv = prog_argv; }
}

void
setProgArgv(int argc, char *argv[])
{
   /* Usually this is done by startupHaskell, so we don't need to call this. 
      However, sometimes Hugs wants to change the arguments which Haskell
      getArgs >>= ... will be fed.  So you can do that by calling here
      _after_ calling startupHaskell.
   */
   prog_argc = argc;
   prog_argv = argv;
   setProgName(prog_argv);
}

/* These functions record and recall the full arguments, including the
   +RTS ... -RTS options. The reason for adding them was so that the
   ghc-inplace program can pass /all/ the arguments on to the real ghc. */
void
getFullProgArgv(int *argc, char **argv[])
{
    if (argc) { *argc = full_prog_argc; }
    if (argv) { *argv = full_prog_argv; }
}

void
setFullProgArgv(int argc, char *argv[])
{
    int i;
    full_prog_argc = argc;
    full_prog_argv = stgCallocBytes(argc + 1, sizeof (char *),
                                    "setFullProgArgv 1");
    for (i = 0; i < argc; i++) {
        full_prog_argv[i] = stgMallocBytes(strlen(argv[i]) + 1,
                                           "setFullProgArgv 2");
        strcpy(full_prog_argv[i], argv[i]);
    }
    full_prog_argv[argc] = NULL;
}

void
freeFullProgArgv (void)
{
    int i;

    if (full_prog_argv != NULL) {
        for (i = 0; i < full_prog_argc; i++) {
            stgFree(full_prog_argv[i]);
        }
        stgFree(full_prog_argv);
    }

    full_prog_argc = 0;
    full_prog_argv = NULL;
}
