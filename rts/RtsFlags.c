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

#include "RtsUtils.h"
#include "Profiling.h"
#include "RtsFlags.h"
#include "sm/OSMem.h"
#include "hooks/Hooks.h"
#include "Capability.h"

#if defined(HAVE_CTYPE_H)
#include <ctype.h>
#endif

#include <string.h>

#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif

#if defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif

#include <fs_rts.h>

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
char  **rts_argv = NULL;
int     rts_argv_size = 0;
#if defined(mingw32_HOST_OS)
// On Windows hs_main uses GetCommandLineW to get Unicode arguments and
// passes them along UTF8 encoded as argv. We store them here in order to
// free them on exit.
int       win32_full_utf8_argc = 0;
char**    win32_utf8_argv = NULL;
#endif

// The global rtsConfig, set from the RtsConfig supplied by the call
// to hs_init_ghc().
RtsConfig rtsConfig;

const RtsConfig defaultRtsConfig  = {
    .rts_opts_enabled = RtsOptsSafeOnly,
    .rts_opts_suggestions = true,
    .rts_opts = NULL,
    .rts_hs_main = false,
    .keep_cafs = false,
    .eventlog_writer = &FileEventLogWriter,
    .defaultsHook = FlagDefaultsHook,
    .onExitHook = OnExitHook,
    .stackOverflowHook = StackOverflowHook,
    .outOfHeapHook = OutOfHeapHook,
    .mallocFailHook = MallocFailHook,
    .gcDoneHook = NULL,
    .longGCSync = LongGCSync,
    .longGCSyncEnd = LongGCSyncEnd
};

/*
 * constants, used later
 */
#define RTS 1
#define PGM 0

/* -----------------------------------------------------------------------------
   Static function decls
   -------------------------------------------------------------------------- */

static void procRtsOpts (int rts_argc0, RtsOptsEnabledEnum enabled);

static void normaliseRtsOpts (void);

static void initStatsFile (FILE *f);

static int  openStatsFile (
    char *filename, const char *FILENAME_FMT, FILE **file_ret);

static StgWord64 decodeSize (
    const char *flag, uint32_t offset, StgWord64 min, StgWord64 max);

static void bad_option (const char *s);

#if defined(DEBUG)
static void read_debug_flags(const char *arg);
#endif

#if defined(PROFILING)
static bool read_heap_profiling_flag(const char *arg);
#endif

#if defined(TRACING)
static void read_trace_flags(const char *arg);
#endif

static void errorUsage (void) GNU_ATTRIBUTE(__noreturn__);

#if defined(mingw32_HOST_OS)
static char** win32_full_utf8_argv;
#endif
static char *  copyArg (char *arg);
static char ** copyArgv (int argc, char *argv[]);
static void    freeArgv (int argc, char *argv[]);
static void setProgName (char *argv[]);

static void errorRtsOptsDisabled (const char *s);

/* -----------------------------------------------------------------------------
 * Command-line option parsing routines.
 * ---------------------------------------------------------------------------*/

void initRtsFlagsDefaults(void)
{
    StgWord64 maxStkSize = 8 * getPhysicalMemorySize() / 10;
    // if getPhysicalMemorySize fails just move along with an 8MB limit
    if (maxStkSize == 0)
        maxStkSize = 8 * 1024 * 1024;

    RtsFlags.GcFlags.statsFile          = NULL;
    RtsFlags.GcFlags.giveStats          = NO_GC_STATS;

    RtsFlags.GcFlags.maxStkSize         = maxStkSize / sizeof(W_);
    RtsFlags.GcFlags.initialStkSize     = 1024 / sizeof(W_);
    RtsFlags.GcFlags.stkChunkSize       = (32 * 1024) / sizeof(W_);
    RtsFlags.GcFlags.stkChunkBufferSize = (1 * 1024) / sizeof(W_);

    RtsFlags.GcFlags.minAllocAreaSize   = (1024 * 1024)       / BLOCK_SIZE;
    RtsFlags.GcFlags.largeAllocLim      = 0; /* defaults to minAllocAreasize */
    RtsFlags.GcFlags.nurseryChunkSize   = 0;
    RtsFlags.GcFlags.minOldGenSize      = (1024 * 1024)       / BLOCK_SIZE;
    RtsFlags.GcFlags.maxHeapSize        = 0;    /* off by default */
    RtsFlags.GcFlags.heapLimitGrace     = (1024 * 1024);
    RtsFlags.GcFlags.heapSizeSuggestion = 0;    /* none */
    RtsFlags.GcFlags.heapSizeSuggestionAuto = false;
    RtsFlags.GcFlags.pcFreeHeap         = 3;    /* 3% */
    RtsFlags.GcFlags.oldGenFactor       = 2;
    RtsFlags.GcFlags.generations        = 2;
    RtsFlags.GcFlags.squeezeUpdFrames   = true;
    RtsFlags.GcFlags.compact            = false;
    RtsFlags.GcFlags.compactThreshold   = 30.0;
    RtsFlags.GcFlags.sweep              = false;
    RtsFlags.GcFlags.idleGCDelayTime    = USToTime(300000); // 300ms
#if defined(THREADED_RTS)
    RtsFlags.GcFlags.doIdleGC           = true;
#else
    RtsFlags.GcFlags.doIdleGC           = false;
#endif
    RtsFlags.GcFlags.heapBase           = 0;   /* means don't care */
    RtsFlags.GcFlags.allocLimitGrace    = (100*1024) / BLOCK_SIZE;
    RtsFlags.GcFlags.numa               = false;
    RtsFlags.GcFlags.numaMask           = 1;
    RtsFlags.GcFlags.ringBell           = false;
    RtsFlags.GcFlags.longGCSync         = 0; /* detection turned off */

    RtsFlags.DebugFlags.scheduler       = false;
    RtsFlags.DebugFlags.interpreter     = false;
    RtsFlags.DebugFlags.weak            = false;
    RtsFlags.DebugFlags.gccafs          = false;
    RtsFlags.DebugFlags.gc              = false;
    RtsFlags.DebugFlags.block_alloc     = false;
    RtsFlags.DebugFlags.sanity          = false;
    RtsFlags.DebugFlags.stable          = false;
    RtsFlags.DebugFlags.stm             = false;
    RtsFlags.DebugFlags.prof            = false;
    RtsFlags.DebugFlags.apply           = false;
    RtsFlags.DebugFlags.linker          = false;
    RtsFlags.DebugFlags.squeeze         = false;
    RtsFlags.DebugFlags.hpc             = false;
    RtsFlags.DebugFlags.sparks          = false;
    RtsFlags.DebugFlags.numa            = false;
    RtsFlags.DebugFlags.compact         = false;

#if defined(PROFILING)
    RtsFlags.CcFlags.doCostCentres      = COST_CENTRES_NONE;
    RtsFlags.CcFlags.outputFileNameStem = NULL;
#endif /* PROFILING */

    RtsFlags.ProfFlags.doHeapProfile      = false;
    RtsFlags.ProfFlags.heapProfileInterval = USToTime(100000); // 100ms

#if defined(PROFILING)
    RtsFlags.ProfFlags.includeTSOs        = false;
    RtsFlags.ProfFlags.showCCSOnException = false;
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

#if defined(TRACING)
    RtsFlags.TraceFlags.tracing       = TRACE_NONE;
    RtsFlags.TraceFlags.timestamp     = false;
    RtsFlags.TraceFlags.scheduler     = false;
    RtsFlags.TraceFlags.gc            = false;
    RtsFlags.TraceFlags.sparks_sampled= false;
    RtsFlags.TraceFlags.sparks_full   = false;
    RtsFlags.TraceFlags.user          = false;
    RtsFlags.TraceFlags.trace_output  = NULL;
#endif

#if defined(PROFILING)
    // When profiling we want a lot more ticks
    RtsFlags.MiscFlags.tickInterval     = USToTime(1000);  // 1ms
#else
    RtsFlags.MiscFlags.tickInterval     = DEFAULT_TICK_INTERVAL;
#endif
    RtsFlags.ConcFlags.ctxtSwitchTime   = USToTime(20000); // 20ms

    RtsFlags.MiscFlags.install_signal_handlers = true;
    RtsFlags.MiscFlags.install_seh_handlers    = true;
    RtsFlags.MiscFlags.generate_stack_trace    = true;
    RtsFlags.MiscFlags.generate_dump_file      = false;
    RtsFlags.MiscFlags.machineReadable         = false;
    RtsFlags.MiscFlags.internalCounters        = false;
    RtsFlags.MiscFlags.linkerMemBase           = 0;

#if defined(THREADED_RTS)
    RtsFlags.ParFlags.nCapabilities     = 1;
    RtsFlags.ParFlags.migrate           = true;
    RtsFlags.ParFlags.parGcEnabled      = 1;
    RtsFlags.ParFlags.parGcGen          = 0;
    RtsFlags.ParFlags.parGcLoadBalancingEnabled = true;
    RtsFlags.ParFlags.parGcLoadBalancingGen = ~0u; /* auto, based on -A */
    RtsFlags.ParFlags.parGcNoSyncWithIdle   = 0;
    RtsFlags.ParFlags.parGcThreads      = 0; /* defaults to -N */
    RtsFlags.ParFlags.setAffinity       = 0;
#endif

#if defined(THREADED_RTS)
    RtsFlags.ParFlags.maxLocalSparks    = 4096;
#endif /* THREADED_RTS */

#if defined(TICKY_TICKY)
    RtsFlags.TickyFlags.showTickyStats   = false;
    RtsFlags.TickyFlags.tickyFile        = NULL;
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
"  -K<size>  Sets the maximum stack size (default: 80% of the heap)",
"            Egs: -K32k -K512k -K8M",
"  -ki<size> Sets the initial thread stack size (default 1k)  Egs: -ki4k -ki2m",
"  -kc<size> Sets the stack chunk size (default 32k)",
"  -kb<size> Sets the stack chunk buffer size (default 1k)",
"",
"  -A<size>  Sets the minimum allocation area size (default 1m) Egs: -A20m -A10k",
"  -AL<size> Sets the amount of large-object memory that can be allocated",
"            before a GC is triggered (default: the value of -A)",
"  -n<size>  Allocation area chunk size (0 = disabled, default: 0)",
"  -O<size>  Sets the minimum size of the old generation (default 1M)",
"  -M<size>  Sets the maximum heap size (default unlimited)  Egs: -M256k -M1G",
"  -H<size>  Sets the minimum heap size (default 0M)   Egs: -H24m  -H1G",
"  -xb<addr> Sets the address from which a suitable start for the heap memory",
"            will be searched from. This is useful if the default address",
"            clashes with some third-party library.",
"  -m<n>     Minimum % of heap which must be available (default 3%)",
"  -G<n>     Number of generations (default: 2)",
"  -c<n>     Use in-place compaction instead of copying in the oldest generation",
"           when live data is at least <n>% of the maximum heap size set with",
"           -M (default: 30%)",
"  -c       Use in-place compaction for all oldest generation collections",
"           (the default is to use copying)",
"  -w       Use mark-region for the oldest generation (experimental)",
#if defined(THREADED_RTS)
"  -I<sec>  Perform full GC after <sec> idle time (default: 0.3, 0 == off)",
#endif
"",
"  -T         Collect GC statistics (useful for in-program statistics access)",
"  -t[<file>] One-line GC statistics (if <file> omitted, uses stderr)",
"  -s[<file>] Summary  GC statistics (if <file> omitted, uses stderr)",
"  -S[<file>] Detailed GC statistics (if <file> omitted, uses stderr)",
"",
"",
"  -Z         Don't squeeze out update frames on stack overflow",
"  -B         Sound the bell at the start of each garbage collection",
#if defined(PROFILING)
"",
"  -p         Time/allocation profile in tree format ",
"             (output file <output prefix>.prof)",
"  -po<file>  Override profiling output file name prefix (program name by default)",
"  -P         More detailed Time/Allocation profile in tree format",
"  -Pa        Give information about *all* cost centres in tree format",
"  -pj        Output cost-center profile in JSON format",
"",
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
#endif /* PROFILING */
"",
"  -hT            Produce a heap profile grouped by closure type"

#if defined(TRACING)
"",
"  -ol<file>  Send binary eventlog to <file> (default: <program>.eventlog)",
"  -l[flags]  Log events to a file",
#  if defined(DEBUG)
"  -v[flags]  Log events to stderr",
#  endif
"             where [flags] can contain:",
"                s    scheduler events",
"                g    GC and heap events",
"                p    par spark events (sampled)",
"                f    par spark events (full detail)",
"                u    user events (emitted from Haskell code)",
"                a    all event classes above",
#  if defined(DEBUG)
"                t    add time stamps (only useful with -v)",
#  endif
"               -x    disable an event class, for any flag above",
"             the initial enabled event classes are 'sgpu'",
#endif

#if !defined(PROFILING)
"",
"  -h       Heap residency profile (output file <program>.hp)",
#endif
"  -i<sec>  Time between heap profile samples (seconds, default: 0.1)",
"",
#if defined(TICKY_TICKY)
"  -r<file>  Produce ticky-ticky statistics (with -rstderr for stderr)",
"",
#endif
"  -C<secs>  Context-switch interval in seconds.",
"            0 or no argument means switch as often as possible.",
"            Default: 0.02 sec.",
"  -V<secs>  Master tick interval in seconds (0 == disable timer).",
"            This sets the resolution for -C and the heap profile timer -i,",
"            and is the frequency of time profile samples.",
#if defined(PROFILING)
"            Default: 0.001 sec.",
#else
"            Default: 0.01 sec.",
#endif
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
"  -Da  DEBUG: apply",
"  -Dl  DEBUG: linker",
"  -Dm  DEBUG: stm",
"  -Dz  DEBUG: stack squeezing",
"  -Dc  DEBUG: program coverage",
"  -Dr  DEBUG: sparks",
"  -DC  DEBUG: compact",
"",
"     NOTE: DEBUG events are sent to stderr by default; add -l to create a",
"     binary event log file instead.",
"",
#endif /* DEBUG */
#if defined(THREADED_RTS) && !defined(NOSMP)
"  -N[<n>]    Use <n> processors (default: 1, -N alone determines",
"             the number of processors to use automatically)",
"  -maxN[<n>] Use up to <n> processors automatically",
"  -qg[<n>]  Use parallel GC only for generations >= <n>",
"            (default: 0, -qg alone turns off parallel GC)",
"  -qb[<n>]  Use load-balancing in the parallel GC only for generations >= <n>",
"            (default: 1 for -A < 32M, 0 otherwise;"
"             -qb alone turns off load-balancing)",
"  -qn<n>    Use <n> threads for parallel GC (defaults to value of -N)",
"  -qa       Use the OS to set thread affinity (experimental)",
"  -qm       Don't automatically migrate threads between CPUs",
"  -qi<n>    If a processor has been idle for the last <n> GCs, do not",
"            wake it up for a non-load-balancing parallel GC.",
"            (0 disables,  default: 0)",
"  --numa[=<node_mask>]",
"            Use NUMA, nodes given by <node_mask> (default: off)",
#if defined(DEBUG)
"  --debug-numa[=<num_nodes>]",
"            Pretend NUMA: like --numa, but without the system calls.",
"            Can be used on non-NUMA systems for debugging.",
"",
#endif
#endif
"  --install-signal-handlers=<yes|no>",
"            Install signal handlers (default: yes)",
#if defined(mingw32_HOST_OS)
"  --install-seh-handlers=<yes|no>",
"            Install exception handlers (default: yes)",
"  --generate-crash-dumps",
"            Generate Windows crash dumps, requires exception handlers",
"            to be installed. Implies --install-signal-handlers=yes.",
"            (default: no)",
"  --generate-stack-traces=<yes|no>",
"            Generate a stack trace when your application encounters a",
"            fatal error. When symbols are available an attempt will be",
"            made to resolve addresses to names. (default: yes)",
#endif
#if defined(THREADED_RTS)
"  -e<n>     Maximum number of outstanding local sparks (default: 4096)",
#endif
#if defined(x86_64_HOST_ARCH)
"  -xm       Base address to mmap memory in the GHCi linker",
"            (hex; must be <80000000)",
#endif
"  -xq       The allocation limit given to a thread after it receives",
"            an AllocationLimitExceeded exception. (default: 100k)",
"",
"  -Mgrace=<n>",
"            The amount of allocation after the program receives a",
"            HeapOverflow exception before the exception is thrown again, if",
"            the program is still exceeding the heap limit.",
"",
"RTS options may also be specified using the GHCRTS environment variable.",
"",
"Other RTS options may be available for programs compiled a different way.",
"The GHC User's Guide has full details.",
"",
0
};

/**
Note [Windows Unicode Arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~
On Windows argv is usually encoded in the current Codepage which might not
support unicode.

Instead of ignoring the arguments to hs_init we expect them to be utf-8
encoded when coming from a custom main function. In the regular hs_main we
get the unicode arguments from the windows API and pass them along utf8
encoded instead.

This reduces special casing of arguments in later parts of the RTS and base
libraries to dealing with slash differences and using utf8 instead of the
current locale on Windows when decoding arguments.

*/

#if defined(mingw32_HOST_OS)
//Allocate a buffer and return the string utf8 encoded.
char* lpcwstrToUTF8(const wchar_t* utf16_str)
{
    //Check the utf8 encoded size first
    int res = WideCharToMultiByte(CP_UTF8, 0, utf16_str, -1, NULL, 0,
                                  NULL, NULL);
    if (res == 0) {
        return NULL;
    }
    char* buffer = (char*) stgMallocBytes((size_t)res, "getUTF8Args 2");
    res = WideCharToMultiByte(CP_UTF8, 0, utf16_str, -1, buffer, res,
                              NULL, NULL);
    return buffer;
}

char** getUTF8Args(int* argc)
{
    LPCWSTR cmdLine = GetCommandLineW();
    LPWSTR* argvw = CommandLineToArgvW(cmdLine, argc);

    // We create two argument arrays, one which is later permutated by the RTS
    // instead of the main argv.
    // The other one is used to free the allocted memory later.
    char** argv = (char**) stgMallocBytes(sizeof(char*) * (*argc + 1),
                                          "getUTF8Args 1");
    win32_full_utf8_argv = (char**) stgMallocBytes(sizeof(char*) * (*argc + 1),
                                                   "getUTF8Args 1");

    for (int i = 0; i < *argc; i++)
    {
        argv[i] = lpcwstrToUTF8(argvw[i]);
    }
    argv[*argc] = NULL;
    memcpy(win32_full_utf8_argv, argv, sizeof(char*) * (*argc + 1));

    LocalFree(argvw);
    win32_utf8_argv = argv;
    win32_full_utf8_argc = *argc;
    return argv;
}
#endif

STATIC_INLINE bool strequal(const char *a, const char * b)
{
    return(strcmp(a, b) == 0);
}

// We can't predict up front how much space we'll need for rts_argv,
// because it involves parsing ghc_rts_opts and GHCRTS, so we
// expand it on demand.
static void appendRtsArg (char *arg)
{
    if (rts_argc == rts_argv_size) {
        rts_argv_size *= 2;
        rts_argv = stgReallocBytes(rts_argv, rts_argv_size * sizeof (char *),
                                   "RtsFlags.c:appendRtsArg");
    }
    rts_argv[rts_argc++] = arg;
}

static void splitRtsFlags(const char *s)
{
    const char *c1, *c2;
    char *t;

    c1 = s;
    do {
        while (isspace(*c1)) { c1++; };
        c2 = c1;
        while (!isspace(*c2) && *c2 != '\0') { c2++; };

        if (c1 == c2) { break; }

        t = stgMallocBytes(c2-c1+1, "RtsFlags.c:splitRtsFlags()");
        strncpy(t, c1, c2-c1);
        t[c2-c1] = '\0';
        appendRtsArg(t);

        c1 = c2;
    } while (*c1 != '\0');
}

static void errorRtsOptsDisabled(const char *s)
{
    char *advice;
    if (rtsConfig.rts_hs_main) {
        advice = "Link with -rtsopts to enable them.";
    } else {
        advice = "Use hs_init_with_rtsopts() to enable them.";
    }
    errorBelch(s, advice);
}

/* -----------------------------------------------------------------------------
   Parse the command line arguments, collecting options for the RTS.

   On return:
     - argv[] is *modified*, any RTS options have been stripped out
     - *argc  contains the new count of arguments in argv[]

     - rts_argv[]  (global) contains a copy of the collected RTS args
     - rts_argc    (global) contains the count of args in rts_argv

     - prog_argv[] (global) contains a copy of the non-RTS args (== argv)
     - prog_argc   (global) contains the count of args in prog_argv

     - prog_name   (global) contains the basename of prog_argv[0]

     - rtsConfig   (global) contains the supplied RtsConfig

  On Windows argv is assumed to be utf8 encoded for unicode compatibility.
  See Note [Windows Unicode Arguments]

  -------------------------------------------------------------------------- */

void setupRtsFlags (int *argc, char *argv[], RtsConfig rts_config)
{
    uint32_t mode;
    uint32_t total_arg;
    uint32_t arg, rts_argc0;

    rtsConfig = rts_config;

    setProgName (argv);
    total_arg = *argc;
    arg = 1;

    if (*argc > 1) { *argc = 1; };
    rts_argc = 0;

    rts_argv_size = total_arg + 1;
    rts_argv = stgMallocBytes(rts_argv_size * sizeof (char *), "setupRtsFlags");

    rts_argc0 = rts_argc;

    // process arguments from the -with-rtsopts compile-time flag first
    // (arguments from the GHCRTS environment variable and the command
    // line override these).
    {
        if (rtsConfig.rts_opts != NULL) {
            splitRtsFlags(rtsConfig.rts_opts);
            // opts from rts_opts are always enabled:
            procRtsOpts(rts_argc0, RtsOptsAll);
            rts_argc0 = rts_argc;
        }
    }

    // process arguments from the GHCRTS environment variable next
    // (arguments from the command line override these).
    // If we ignore all non-builtin rtsOpts we skip these.
    if(rtsConfig.rts_opts_enabled != RtsOptsIgnoreAll)
    {
        char *ghc_rts = getenv("GHCRTS");

        if (ghc_rts != NULL) {
            if (rtsConfig.rts_opts_enabled == RtsOptsNone) {
                errorRtsOptsDisabled(
                    "Warning: Ignoring GHCRTS variable as RTS options are disabled.\n         %s");
                // We don't actually exit, just warn
            } else {
                splitRtsFlags(ghc_rts);
                procRtsOpts(rts_argc0, rtsConfig.rts_opts_enabled);
                rts_argc0 = rts_argc;
            }
        }
    }


    // If we ignore all commandline rtsOpts we skip processing of argv by
    // the RTS completely
    if(!(rtsConfig.rts_opts_enabled == RtsOptsIgnoreAll ||
         rtsConfig.rts_opts_enabled == RtsOptsIgnore)
    )
    {
        // Split arguments (argv) into PGM (argv) and RTS (rts_argv) parts
        //   argv[0] must be PGM argument -- leave in argv
        //
        for (mode = PGM; arg < total_arg; arg++) {
            // The '--RTS' argument disables all future
            // +RTS ... -RTS processing.
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
                mode = RTS;
            }
            else if (strequal("-RTS", argv[arg])) {
                mode = PGM;
            }
            else if (mode == RTS) {
                appendRtsArg(copyArg(argv[arg]));
            }
            else {
                argv[(*argc)++] = argv[arg];
            }
        }

    }

    // process remaining program arguments
    for (; arg < total_arg; arg++) {
        argv[(*argc)++] = argv[arg];
    }
    argv[*argc] = (char *) 0;

    procRtsOpts(rts_argc0, rtsConfig.rts_opts_enabled);

    appendRtsArg((char *)0);
    rts_argc--; // appendRtsArg will have bumped it for the NULL (#7227)

    normaliseRtsOpts();

    setProgArgv(*argc, argv);

    if (RtsFlags.GcFlags.statsFile != NULL) {
        initStatsFile (RtsFlags.GcFlags.statsFile);
    }
#if defined(TICKY_TICKY)
    if (RtsFlags.TickyFlags.tickyFile != NULL) {
        initStatsFile (RtsFlags.TickyFlags.tickyFile);
    }
#endif
}

/* -----------------------------------------------------------------------------
 * procRtsOpts: Process rts_argv between rts_argc0 and rts_argc.
 * -------------------------------------------------------------------------- */

#if defined(HAVE_UNISTD_H) && defined(HAVE_SYS_TYPES_H) && !defined(mingw32_HOST_OS)
static void checkSuid(RtsOptsEnabledEnum enabled)
{
    if (enabled == RtsOptsSafeOnly) {
        /* This doesn't cover linux/posix capabilities like CAP_DAC_OVERRIDE,
           we'd have to link with -lcap for that. */
        if ((getuid() != geteuid()) || (getgid() != getegid())) {
            errorRtsOptsDisabled(
                "RTS options are disabled for setuid binaries. %s");
            stg_exit(EXIT_FAILURE);
        }
    }
}
#else
static void checkSuid (RtsOptsEnabledEnum enabled STG_UNUSED)
{
}
#endif

static void checkUnsafe(RtsOptsEnabledEnum enabled)
{
    if (enabled == RtsOptsSafeOnly) {
        errorRtsOptsDisabled("Most RTS options are disabled. %s");
        stg_exit(EXIT_FAILURE);
    }
}

static void procRtsOpts (int rts_argc0,
                         RtsOptsEnabledEnum rtsOptsEnabled)
{
    bool error = false;
    int arg;
    int unchecked_arg_start;

    if (!(rts_argc0 < rts_argc)) return;

    if (rtsOptsEnabled == RtsOptsNone) {
        errorRtsOptsDisabled("RTS options are disabled. %s");
        stg_exit(EXIT_FAILURE);
    }

    checkSuid(rtsOptsEnabled);

    // Process RTS (rts_argv) part: mainly to determine statsfile
    for (arg = rts_argc0; arg < rts_argc; arg++) {

        /* We handle RtsOptsSafeOnly mode by declaring each option as
           either OPTION_SAFE or OPTION_UNSAFE. To make sure we cover
           every branch we use an option_checked flag which is reset
           at the start each iteration and checked at the end. */
        bool option_checked = false;

// See Note [OPTION_SAFE vs OPTION_UNSAFE].
#define OPTION_SAFE option_checked = true;
#define OPTION_UNSAFE checkUnsafe(rtsOptsEnabled); option_checked = true;

        if (rts_argv[arg][0] != '-') {
            fflush(stdout);
            errorBelch("unexpected RTS argument: %s", rts_argv[arg]);
            error = true;

        } else {
            /* 0 is dash, 1 is first letter */
            /* see Trac #9839 */
            unchecked_arg_start = 1;
            switch(rts_argv[arg][1]) {

              /* process: general args, then PROFILING-only ones, then
                 CONCURRENT-only, TICKY-only (same order as defined in
                 RtsFlags.lh); within those groups, mostly in
                 case-insensitive alphabetical order.  Final group is
                 x*, which allows for more options.
              */

#if defined(TICKY_TICKY)
# define TICKY_BUILD_ONLY(x) x
#else
# define TICKY_BUILD_ONLY(x) \
errorBelch("the flag %s requires the program to be built with -ticky", \
           rts_argv[arg]);                                             \
error = true;
#endif

#if defined(PROFILING)
# define PROFILING_BUILD_ONLY(x)   x
#else
# define PROFILING_BUILD_ONLY(x) \
errorBelch("the flag %s requires the program to be built with -prof", \
           rts_argv[arg]);                                            \
error = true;
#endif

#if defined(TRACING)
# define TRACING_BUILD_ONLY(x)   x
#else
# define TRACING_BUILD_ONLY(x) \
errorBelch("the flag %s requires the program to be built with -eventlog or -debug", \
           rts_argv[arg]);                                              \
error = true;
#endif

#if defined(THREADED_RTS)
# define THREADED_BUILD_ONLY(x)      x
#else
# define THREADED_BUILD_ONLY(x) \
errorBelch("the flag %s requires the program to be built with -threaded", \
           rts_argv[arg]);                                              \
error = true;
#endif

#if defined(DEBUG)
# define DEBUG_BUILD_ONLY(x) x
#else
# define DEBUG_BUILD_ONLY(x) \
errorBelch("the flag %s requires the program to be built with -debug", \
           rts_argv[arg]);                                             \
error = true;
#endif

              /* =========== GENERAL ========================== */
              case '?':
                OPTION_SAFE;
                error = true;
                break;

              /* This isn't going to allow us to keep related options
                 together as we add more --* flags. We really need a
                 proper options parser. */
              case '-':
                  if (strequal("install-signal-handlers=yes",
                               &rts_argv[arg][2])) {
                      OPTION_UNSAFE;
                      RtsFlags.MiscFlags.install_signal_handlers = true;
                  }
                  else if (strequal("install-signal-handlers=no",
                               &rts_argv[arg][2])) {
                      OPTION_UNSAFE;
                      RtsFlags.MiscFlags.install_signal_handlers = false;
                  }
                  else if (strequal("install-seh-handlers=yes",
                              &rts_argv[arg][2])) {
                      OPTION_UNSAFE;
                      RtsFlags.MiscFlags.install_seh_handlers = true;
                  }
                  else if (strequal("install-seh-handlers=no",
                              &rts_argv[arg][2])) {
                      OPTION_UNSAFE;
                      RtsFlags.MiscFlags.install_seh_handlers = false;
                  }
                  else if (strequal("generate-stack-traces=yes",
                               &rts_argv[arg][2])) {
                      OPTION_UNSAFE;
                      RtsFlags.MiscFlags.generate_stack_trace = true;
                  }
                  else if (strequal("generate-stack-traces=no",
                              &rts_argv[arg][2])) {
                      OPTION_UNSAFE;
                      RtsFlags.MiscFlags.generate_stack_trace = false;
                  }
                  else if (strequal("generate-crash-dumps",
                              &rts_argv[arg][2])) {
                      OPTION_UNSAFE;
                      RtsFlags.MiscFlags.generate_dump_file = true;
                  }
                  else if (strequal("machine-readable",
                               &rts_argv[arg][2])) {
                      OPTION_UNSAFE;
                      RtsFlags.MiscFlags.machineReadable = true;
                  }
                  else if (strequal("internal-counters",
                                    &rts_argv[arg][2])) {
                      OPTION_SAFE;
                      RtsFlags.MiscFlags.internalCounters = true;
                  }
                  else if (strequal("info",
                               &rts_argv[arg][2])) {
                      OPTION_SAFE;
                      printRtsInfo(rtsConfig);
                      stg_exit(0);
                  }
#if defined(THREADED_RTS)
                  else if (!strncmp("numa", &rts_argv[arg][2], 4)) {
                      if (!osBuiltWithNumaSupport()) {
                          errorBelch("%s: This GHC build was compiled without NUMA support.",
                                     rts_argv[arg]);
                          error = true;
                          break;
                      }
                      OPTION_SAFE;
                      StgWord mask;
                      if (rts_argv[arg][6] == '=') {
                          mask = (StgWord)strtol(rts_argv[arg]+7,
                                                 (char **) NULL, 10);
                      } else {
                          mask = (StgWord)~0;
                      }
                      if (!osNumaAvailable()) {
                          errorBelch("%s: OS reports NUMA is not available",
                                     rts_argv[arg]);
                          error = true;
                          break;
                      }

                      RtsFlags.GcFlags.numa = true;
                      RtsFlags.GcFlags.numaMask = mask;
                  }
#endif
#if defined(DEBUG) && defined(THREADED_RTS)
                  else if (!strncmp("debug-numa", &rts_argv[arg][2], 10)) {
                      OPTION_SAFE;
                      size_t nNodes;
                      if (rts_argv[arg][12] == '=' &&
                          isdigit(rts_argv[arg][13])) {
                          nNodes = (StgWord)strtol(rts_argv[arg]+13,
                                                 (char **) NULL, 10);
                      } else {
                          errorBelch("%s: missing number of nodes",
                                     rts_argv[arg]);
                          error = true;
                          break;
                      }
                      if (nNodes > MAX_NUMA_NODES) {
                          errorBelch("%s: Too many NUMA nodes (max %d)",
                                     rts_argv[arg], MAX_NUMA_NODES);
                          error = true;
                      } else {
                          RtsFlags.GcFlags.numa = true;
                          RtsFlags.DebugFlags.numa = true;
                          RtsFlags.GcFlags.numaMask = (1<<nNodes) - 1;
                      }
                  }
#endif
                  else if (!strncmp("long-gc-sync=", &rts_argv[arg][2], 13)) {
                      OPTION_SAFE;
                      if (rts_argv[arg][2] == '\0') {
                          /* use default */
                      } else {
                          RtsFlags.GcFlags.longGCSync =
                              fsecondsToTime(atof(rts_argv[arg]+16));
                      }
                      break;
                  }
                  else {
                      OPTION_SAFE;
                      errorBelch("unknown RTS option: %s",rts_argv[arg]);
                      error = true;
                  }
                  break;
              case 'A':
                  OPTION_UNSAFE;
                  if (rts_argv[arg][2] == 'L') {
                      RtsFlags.GcFlags.largeAllocLim
                          = decodeSize(rts_argv[arg], 3, 2*BLOCK_SIZE,
                                       HS_INT_MAX) / BLOCK_SIZE;
                  } else {
                      // minimum two blocks in the nursery, so that we have one
                      // to grab for allocate().
                      RtsFlags.GcFlags.minAllocAreaSize
                          = decodeSize(rts_argv[arg], 2, 2*BLOCK_SIZE,
                                       HS_INT_MAX) / BLOCK_SIZE;
                  }
                  break;
              case 'n':
                  OPTION_UNSAFE;
                  RtsFlags.GcFlags.nurseryChunkSize
                      = decodeSize(rts_argv[arg], 2, 2*BLOCK_SIZE, HS_INT_MAX)
                           / BLOCK_SIZE;
                  break;

              case 'B':
                OPTION_UNSAFE;
                RtsFlags.GcFlags.ringBell = true;
                unchecked_arg_start++;
                goto check_rest;

              case 'c':
                  OPTION_UNSAFE;
                  if (rts_argv[arg][2] != '\0') {
                      RtsFlags.GcFlags.compactThreshold =
                          atof(rts_argv[arg]+2);
                  } else {
                      RtsFlags.GcFlags.compact = true;
                  }
                  break;

              case 'w':
                OPTION_UNSAFE;
                RtsFlags.GcFlags.sweep = true;
                unchecked_arg_start++;
                goto check_rest;

              case 'F':
                OPTION_UNSAFE;
                RtsFlags.GcFlags.oldGenFactor = atof(rts_argv[arg]+2);

                if (RtsFlags.GcFlags.oldGenFactor < 0)
                  bad_option( rts_argv[arg] );
                break;

              case 'D':
              OPTION_SAFE;
              DEBUG_BUILD_ONLY(read_debug_flags(rts_argv[arg]);)
              break;

              case 'K':
                  OPTION_UNSAFE;
                  RtsFlags.GcFlags.maxStkSize =
                      decodeSize(rts_argv[arg], 2, 0, HS_WORD_MAX)
                      / sizeof(W_);
                  break;

              case 'k':
                OPTION_UNSAFE;
                switch(rts_argv[arg][2]) {
                case 'c':
                  RtsFlags.GcFlags.stkChunkSize =
                      decodeSize(rts_argv[arg], 3, sizeof(W_), HS_WORD_MAX)
                      / sizeof(W_);
                  break;
                case 'b':
                  RtsFlags.GcFlags.stkChunkBufferSize =
                      decodeSize(rts_argv[arg], 3, sizeof(W_), HS_WORD_MAX)
                      / sizeof(W_);
                  break;
                case 'i':
                  RtsFlags.GcFlags.initialStkSize =
                      decodeSize(rts_argv[arg], 3, sizeof(W_), HS_WORD_MAX)
                      / sizeof(W_);
                  break;
                default:
                  RtsFlags.GcFlags.initialStkSize =
                      decodeSize(rts_argv[arg], 2, sizeof(W_), HS_WORD_MAX)
                      / sizeof(W_);
                  break;
                }
                break;

              case 'M':
                  OPTION_UNSAFE;
                  if (0 == strncmp("grace=", rts_argv[arg] + 2, 6)) {
                      RtsFlags.GcFlags.heapLimitGrace =
                          decodeSize(rts_argv[arg], 8, BLOCK_SIZE, HS_WORD_MAX);
                  } else {
                      RtsFlags.GcFlags.maxHeapSize =
                          decodeSize(rts_argv[arg], 2, BLOCK_SIZE, HS_WORD_MAX)
                          / BLOCK_SIZE;
                      // user give size in *bytes* but "maxHeapSize" is in
                      // *blocks*
                  }
                  break;

              case 'm':
                /* Case for maxN feature request ticket #10728, it's a little
                   odd being so far from the N case. */
#if !defined(NOSMP)
                if (strncmp("maxN", &rts_argv[arg][1], 4) == 0) {
                  OPTION_SAFE;
                  THREADED_BUILD_ONLY(
                    int nCapabilities;
                    int proc = (int)getNumberOfProcessors();

                    nCapabilities = strtol(rts_argv[arg]+5, (char **) NULL, 10);
                    if (nCapabilities > proc) { nCapabilities = proc; }

                    if (nCapabilities <= 0) {
                      errorBelch("bad value for -maxN");
                      error = true;
                    }
#if defined(PROFILING)
                    RtsFlags.ParFlags.nCapabilities = 1;
#else
                    RtsFlags.ParFlags.nCapabilities = (uint32_t)nCapabilities;
#endif
                  ) break;
                } else {
#endif
                    OPTION_UNSAFE;
                    RtsFlags.GcFlags.pcFreeHeap = atof(rts_argv[arg]+2);

                    /* -m was allowing bad flags to go unreported */
                    if (RtsFlags.GcFlags.pcFreeHeap == 0.0 &&
                           rts_argv[arg][2] != '0')
                      bad_option( rts_argv[arg] );

                    if (RtsFlags.GcFlags.pcFreeHeap < 0 ||
                        RtsFlags.GcFlags.pcFreeHeap > 100)
                        bad_option( rts_argv[arg] );
                    break;
#if !defined(NOSMP)
                }
#endif
              case 'G':
                  OPTION_UNSAFE;
                  RtsFlags.GcFlags.generations =
                      decodeSize(rts_argv[arg], 2, 1, HS_INT_MAX);
                  break;

              case 'H':
                  OPTION_UNSAFE;
                  if (rts_argv[arg][2] == '\0') {
                      RtsFlags.GcFlags.heapSizeSuggestionAuto = true;
                  } else {
                      RtsFlags.GcFlags.heapSizeSuggestion = (uint32_t)
                          (decodeSize(rts_argv[arg], 2, BLOCK_SIZE, HS_WORD_MAX)
                          / BLOCK_SIZE);
                  }
                  break;

              case 'O':
                  OPTION_UNSAFE;
                  RtsFlags.GcFlags.minOldGenSize =
                      (uint32_t)(decodeSize(rts_argv[arg], 2, BLOCK_SIZE,
                                       HS_WORD_MAX)
                            / BLOCK_SIZE);
                  break;

              case 'I': /* idle GC delay */
                OPTION_UNSAFE;
                if (rts_argv[arg][2] == '\0') {
                  /* use default */
                } else {
                    Time t = fsecondsToTime(atof(rts_argv[arg]+2));
                    if (t == 0) {
                        RtsFlags.GcFlags.doIdleGC = false;
                    } else {
                        RtsFlags.GcFlags.doIdleGC = true;
                        RtsFlags.GcFlags.idleGCDelayTime = t;
                    }
                }
                break;

              case 'T':
                  OPTION_SAFE;
                  RtsFlags.GcFlags.giveStats = COLLECT_GC_STATS;
                  unchecked_arg_start++;
                  goto check_rest; /* Don't initialize statistics file. */

              case 'S':
                  OPTION_SAFE; /* but see below */
                  RtsFlags.GcFlags.giveStats = VERBOSE_GC_STATS;
                  goto stats;

              case 's':
                  OPTION_SAFE; /* but see below */
                  RtsFlags.GcFlags.giveStats = SUMMARY_GC_STATS;
                  goto stats;

              case 't':
                  OPTION_SAFE; /* but see below */
                  RtsFlags.GcFlags.giveStats = ONELINE_GC_STATS;
                  goto stats;

            stats:
                {
                    int r;
                    if (rts_argv[arg][2] != '\0') {
                      OPTION_UNSAFE;
                    }
                    r = openStatsFile(rts_argv[arg]+2, NULL,
                                      &RtsFlags.GcFlags.statsFile);
                    if (r == -1) { error = true; }
                }
                break;

              case 'Z':
                OPTION_UNSAFE;
                RtsFlags.GcFlags.squeezeUpdFrames = false;
                unchecked_arg_start++;
                goto check_rest;

              /* =========== PROFILING ========================== */

              case 'P': /* detailed cost centre profiling (time/alloc) */
              case 'p': /* cost centre profiling (time/alloc) */
                OPTION_SAFE;
                PROFILING_BUILD_ONLY(
                switch (rts_argv[arg][2]) {
                  case 'a':
                    RtsFlags.CcFlags.doCostCentres = COST_CENTRES_ALL;
                    if (rts_argv[arg][3] != '\0') {
                      errorBelch("flag -Pa given an argument"
                                 " when none was expected: %s"
                                ,rts_argv[arg]);
                      error = true;
                    }
                    break;
                  case 'j':
                      RtsFlags.CcFlags.doCostCentres = COST_CENTRES_JSON;
                      break;
                  case 'o':
                      if (rts_argv[arg][3] == '\0') {
                        errorBelch("flag -po expects an argument");
                        error = true;
                        break;
                      }
                      RtsFlags.CcFlags.outputFileNameStem = rts_argv[arg]+3;
                      break;
                  case '\0':
                      if (rts_argv[arg][1] == 'P') {
                          RtsFlags.CcFlags.doCostCentres = COST_CENTRES_VERBOSE;
                      } else {
                          RtsFlags.CcFlags.doCostCentres = COST_CENTRES_SUMMARY;
                      }
                      break;
                  default:
                    unchecked_arg_start++;
                    goto check_rest;
                }
                ) break;

              case 'R':
                  OPTION_SAFE;
                  PROFILING_BUILD_ONLY(
                      RtsFlags.ProfFlags.maxRetainerSetSize =
                        atof(rts_argv[arg]+2);
                  ) break;
              case 'L':
                  OPTION_SAFE;
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
                    OPTION_UNSAFE;
                    RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_CLOSURE_TYPE;
                    break;
                  default:
                    OPTION_SAFE;
                    PROFILING_BUILD_ONLY();
                }
#else
                OPTION_SAFE;
                PROFILING_BUILD_ONLY(
                    error = read_heap_profiling_flag(rts_argv[arg]);
                );
#endif /* PROFILING */
                break;

              case 'i': /* heap sample interval */
                OPTION_UNSAFE;
                if (rts_argv[arg][2] == '\0') {
                  /* use default */
                } else {
                    RtsFlags.ProfFlags.heapProfileInterval =
                        fsecondsToTime(atof(rts_argv[arg]+2));
                }
                break;

              /* =========== CONCURRENT ========================= */
              case 'C': /* context switch interval */
                OPTION_UNSAFE;
                if (rts_argv[arg][2] == '\0')
                    RtsFlags.ConcFlags.ctxtSwitchTime = 0;
                else {
                    RtsFlags.ConcFlags.ctxtSwitchTime =
                        fsecondsToTime(atof(rts_argv[arg]+2));
                }
                break;

              case 'V': /* master tick interval */
                OPTION_UNSAFE;
                if (rts_argv[arg][2] == '\0') {
                    // turns off ticks completely
                    RtsFlags.MiscFlags.tickInterval = 0;
                } else {
                    RtsFlags.MiscFlags.tickInterval =
                        fsecondsToTime(atof(rts_argv[arg]+2));
                }
                break;

#if !defined(NOSMP)
              case 'N':
                OPTION_SAFE;
                THREADED_BUILD_ONLY(
                if (rts_argv[arg][2] == '\0') {
                    RtsFlags.ParFlags.nCapabilities = getNumberOfProcessors();
                } else {
                    int nCapabilities;
                    OPTION_SAFE; /* but see extra checks below... */

                    nCapabilities = strtol(rts_argv[arg]+2, (char **) NULL, 10);

                    if (nCapabilities <= 0) {
                      errorBelch("bad value for -N");
                      error = true;
                    }
                    if (rtsOptsEnabled == RtsOptsSafeOnly &&
                      nCapabilities > (int)getNumberOfProcessors()) {
                      errorRtsOptsDisabled("Using large values for -N is not allowed by default. %s");
                      stg_exit(EXIT_FAILURE);
                    }
                    RtsFlags.ParFlags.nCapabilities = (uint32_t)nCapabilities;
                }
                ) break;

              case 'g':
                OPTION_UNSAFE;
                THREADED_BUILD_ONLY(
                    switch (rts_argv[arg][2]) {
                    case '1':
                        // backwards compat only
                        RtsFlags.ParFlags.parGcEnabled = false;
                        break;
                    default:
                        errorBelch("unknown RTS option: %s",rts_argv[arg]);
                        error = true;
                        break;
                    }
                    ) break;

              case 'q':
                OPTION_UNSAFE;
                THREADED_BUILD_ONLY(
                    switch (rts_argv[arg][2]) {
                    case '\0':
                        errorBelch("incomplete RTS option: %s",rts_argv[arg]);
                        error = true;
                        break;
                    case 'g':
                        if (rts_argv[arg][3] == '\0') {
                            RtsFlags.ParFlags.parGcEnabled = false;
                        } else {
                            RtsFlags.ParFlags.parGcEnabled = true;
                            RtsFlags.ParFlags.parGcGen
                                = strtol(rts_argv[arg]+3, (char **) NULL, 10);
                        }
                        break;
                    case 'b':
                        if (rts_argv[arg][3] == '\0') {
                            RtsFlags.ParFlags.parGcLoadBalancingEnabled =
                                false;
                        }
                        else {
                            RtsFlags.ParFlags.parGcLoadBalancingEnabled =
                                true;
                            RtsFlags.ParFlags.parGcLoadBalancingGen
                                = strtol(rts_argv[arg]+3, (char **) NULL, 10);
                        }
                        break;
                    case 'i':
                        RtsFlags.ParFlags.parGcNoSyncWithIdle
                            = strtol(rts_argv[arg]+3, (char **) NULL, 10);
                        break;
                    case 'n': {
                        int threads;
                        threads = strtol(rts_argv[arg]+3, (char **) NULL, 10);
                        if (threads <= 0) {
                            errorBelch("-qn must be 1 or greater");
                            error = true;
                        } else {
                            RtsFlags.ParFlags.parGcThreads = threads;
                        }
                        break;
                    }
                    case 'a':
                        RtsFlags.ParFlags.setAffinity = true;
                        break;
                    case 'm':
                        RtsFlags.ParFlags.migrate = false;
                        break;
                    case 'w':
                        // -qw was removed; accepted for backwards compat
                        break;
                    default:
                        errorBelch("unknown RTS option: %s",rts_argv[arg]);
                        error = true;
                        break;
                    }
                    ) break;
#endif
              /* =========== PARALLEL =========================== */
              case 'e':
                OPTION_UNSAFE;
                THREADED_BUILD_ONLY(
                if (rts_argv[arg][2] != '\0') {
                    RtsFlags.ParFlags.maxLocalSparks
                      = strtol(rts_argv[arg]+2, (char **) NULL, 10);
                    if (RtsFlags.ParFlags.maxLocalSparks <= 0) {
                      errorBelch("bad value for -e");
                      error = true;
                    }
                }
                ) break;

              /* =========== TICKY ============================== */

              case 'r': /* Basic profiling stats */
                OPTION_SAFE;
                TICKY_BUILD_ONLY(

                RtsFlags.TickyFlags.showTickyStats = true;

                {
                    int r;
                    if (rts_argv[arg][2] != '\0') {
                      OPTION_UNSAFE;
                    }
                    r = openStatsFile(rts_argv[arg]+2,
                                      TICKY_FILENAME_FMT,
                                      &RtsFlags.TickyFlags.tickyFile);
                    if (r == -1) { error = true; }
                }
                ) break;

              /* =========== OUTPUT ============================ */

              case 'o':
                  switch(rts_argv[arg][2]) {
                  case 'l':
                      OPTION_SAFE;
                      TRACING_BUILD_ONLY(
                          if (strlen(&rts_argv[arg][3]) == 0) {
                              errorBelch("-ol expects filename");
                              error = true;
                          } else {
                              RtsFlags.TraceFlags.trace_output =
                                  strdup(&rts_argv[arg][3]);
                          }
                          );
                      break;

                  default:
                      errorBelch("Unknown output flag -o%c", rts_argv[arg][2]);
                      error = true;
                  }
                  break;

              /* =========== TRACING ============================ */

              case 'l':
                  OPTION_SAFE;
                  TRACING_BUILD_ONLY(
                      RtsFlags.TraceFlags.tracing = TRACE_EVENTLOG;
                      read_trace_flags(&rts_argv[arg][2]);
                      );
                  break;

              case 'v':
                  OPTION_SAFE;
                  DEBUG_BUILD_ONLY(
                      RtsFlags.TraceFlags.tracing = TRACE_STDERR;
                      read_trace_flags(&rts_argv[arg][2]);
                      );
                  break;

              /* =========== EXTENDED OPTIONS =================== */

              case 'x': /* Extend the argument space */
                unchecked_arg_start++;
                switch(rts_argv[arg][2]) {
                  case '\0':
                    OPTION_SAFE;
                    errorBelch("incomplete RTS option: %s",rts_argv[arg]);
                    error = true;
                    break;

                case 'b': /* heapBase in hex; undocumented */
                    OPTION_UNSAFE;
                    if (rts_argv[arg][3] != '\0') {
                        RtsFlags.GcFlags.heapBase
                            = strToStgWord(rts_argv[arg]+3, (char **) NULL, 0);
                    } else {
                        errorBelch("-xb: requires argument");
                        error = true;
                    }
                    break;

#if defined(x86_64_HOST_ARCH)
                case 'm': /* linkerMemBase */
                    OPTION_UNSAFE;
                    if (rts_argv[arg][3] != '\0') {
                        RtsFlags.MiscFlags.linkerMemBase
                            = strtol(rts_argv[arg]+3, (char **) NULL, 16);
                        if (RtsFlags.MiscFlags.linkerMemBase > 0x80000000) {
                            errorBelch("-xm: value must be <80000000");
                            error = true;
                        }
                    } else {
                        RtsFlags.MiscFlags.linkerMemBase = 0;
                    }
                    break;
#endif

                case 'c': /* Debugging tool: show current cost centre on
                           an exception */
                    OPTION_SAFE;
                    PROFILING_BUILD_ONLY(
                        RtsFlags.ProfFlags.showCCSOnException = true;
                        );
                    unchecked_arg_start++;
                    goto check_rest;

                case 't':  /* Include memory used by TSOs in a heap profile */
                    OPTION_SAFE;
                    PROFILING_BUILD_ONLY(
                        RtsFlags.ProfFlags.includeTSOs = true;
                        );
                    unchecked_arg_start++;
                    goto check_rest;

                  /*
                   * The option prefix '-xx' is reserved for future
                   * extension.  KSW 1999-11.
                   */

                case 'q':
                  OPTION_UNSAFE;
                  RtsFlags.GcFlags.allocLimitGrace
                      = decodeSize(rts_argv[arg], 3, BLOCK_SIZE, HS_INT_MAX)
                          / BLOCK_SIZE;
                  break;

                  default:
                    OPTION_SAFE;
                    errorBelch("unknown RTS option: %s",rts_argv[arg]);
                    error = true;
                    break;
                }
                break;  /* defensive programming */

            /* check the rest to be sure there is nothing afterwards.*/
            /* see Trac #9839 */
            check_rest:
                {
                    /* start checking from the first unchecked position,
                     * not from index 2*/
                    /* see Trac #9839 */
                    if (rts_argv[arg][unchecked_arg_start] != '\0') {
                      errorBelch("flag -%c given an argument"
                                 " when none was expected: %s",
                                 rts_argv[arg][1],rts_argv[arg]);
                      error = true;
                    }
                    break;
                }

              /* =========== OH DEAR ============================ */
              default:
                OPTION_SAFE;
                errorBelch("unknown RTS option: %s",rts_argv[arg]);
                error = true;
                break;
            }

            if (!option_checked) {
                /* Naughty! Someone didn't use OPTION_UNSAFE / OPTION_SAFE for
                   an option above */
                errorBelch("Internal error in the RTS options parser");
                stg_exit(EXIT_FAILURE);
            }
        }
    }

    if (error) errorUsage();
}

/* -----------------------------------------------------------------------------
 * normaliseRtsOpts: Set some derived values, and make sure things are
 * within sensible ranges.
 * -------------------------------------------------------------------------- */

static void normaliseRtsOpts (void)
{
    if (RtsFlags.MiscFlags.tickInterval < 0) {
        RtsFlags.MiscFlags.tickInterval = DEFAULT_TICK_INTERVAL;
    }

    // If the master timer is disabled, turn off the other timers.
    if (RtsFlags.MiscFlags.tickInterval == 0) {
        RtsFlags.ConcFlags.ctxtSwitchTime  = 0;
        RtsFlags.GcFlags.idleGCDelayTime   = 0;
        RtsFlags.ProfFlags.heapProfileInterval = 0;
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

    if (RtsFlags.ProfFlags.heapProfileInterval > 0) {
        RtsFlags.MiscFlags.tickInterval =
            stg_min(RtsFlags.ProfFlags.heapProfileInterval,
                    RtsFlags.MiscFlags.tickInterval);
    }

    if (RtsFlags.ConcFlags.ctxtSwitchTime > 0) {
        RtsFlags.ConcFlags.ctxtSwitchTicks =
            RtsFlags.ConcFlags.ctxtSwitchTime /
            RtsFlags.MiscFlags.tickInterval;
    } else {
        RtsFlags.ConcFlags.ctxtSwitchTicks = 0;
    }

    if (RtsFlags.ProfFlags.heapProfileInterval > 0) {
        RtsFlags.ProfFlags.heapProfileIntervalTicks =
            RtsFlags.ProfFlags.heapProfileInterval /
            RtsFlags.MiscFlags.tickInterval;
    } else {
        RtsFlags.ProfFlags.heapProfileIntervalTicks = 0;
    }

    if (RtsFlags.GcFlags.stkChunkBufferSize >
        RtsFlags.GcFlags.stkChunkSize / 2) {
        errorBelch("stack chunk buffer size (-kb) must be less than 50%%\n"
                   "of the stack chunk size (-kc)");
        errorUsage();
    }

    if (RtsFlags.GcFlags.maxHeapSize != 0 &&
        RtsFlags.GcFlags.heapSizeSuggestion >
        RtsFlags.GcFlags.maxHeapSize) {
        RtsFlags.GcFlags.maxHeapSize = RtsFlags.GcFlags.heapSizeSuggestion;
    }

    if (RtsFlags.GcFlags.maxHeapSize != 0 &&
        RtsFlags.GcFlags.minAllocAreaSize >
        RtsFlags.GcFlags.maxHeapSize) {
        errorBelch("maximum heap size (-M) is smaller than minimum alloc area size (-A)");
        RtsFlags.GcFlags.minAllocAreaSize = RtsFlags.GcFlags.maxHeapSize;
    }

    // If we have -A16m or larger, use -n4m.
    if (RtsFlags.GcFlags.minAllocAreaSize >= (16*1024*1024) / BLOCK_SIZE) {
        RtsFlags.GcFlags.nurseryChunkSize = (4*1024*1024) / BLOCK_SIZE;
    }

    if (RtsFlags.ParFlags.parGcLoadBalancingGen == ~0u) {
        StgWord alloc_area_bytes
            = RtsFlags.GcFlags.minAllocAreaSize * BLOCK_SIZE;

        // If allocation area is larger that CPU cache
        // we can finish scanning quicker doing work-stealing
        // scan. Trac #9221
        // 32M looks big enough not to fit into L2 cache
        // of popular modern CPUs.
        if (alloc_area_bytes >= 32 * 1024 * 1024) {
            RtsFlags.ParFlags.parGcLoadBalancingGen = 0;
        } else {
            RtsFlags.ParFlags.parGcLoadBalancingGen = 1;
        }
    }

    // We can't generate dumps without signal handlers
    if (RtsFlags.MiscFlags.generate_dump_file) {
        RtsFlags.MiscFlags.install_seh_handlers = true;
    }
}

static void errorUsage (void)
{
    const char **p;

    fflush(stdout);
    for (p = usage_text; *p; p++)
        errorBelch("%s", *p);
    stg_exit(EXIT_FAILURE);
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

/* -----------------------------------------------------------------------------
 * openStatsFile: open a file in which to put some runtime stats
 * -------------------------------------------------------------------------- */

static int // return -1 on error
openStatsFile (char *filename,           // filename, or NULL
               const char *filename_fmt, // if filename == NULL, use
                                         // this fmt with sprintf to
                                         // generate the filename.  %s
                                         // expands to the program name.
               FILE **file_ret)          // return the FILE*
{
    FILE *f = NULL;

    if (strequal(filename, "stderr")
        || (filename_fmt == NULL && *filename == '\0')) {
        f = NULL; /* NULL means use debugBelch */
    } else {
        if (*filename != '\0') {  /* stats file specified */
            f = __rts_fopen (filename,"w");
        } else {
            if (filename_fmt == NULL) {
                errorBelch("Invalid stats filename format (NULL)\n");
                return -1;
            }
            /* default <program>.<ext> */
            char stats_filename[STATS_FILENAME_MAXLEN];
            snprintf(stats_filename, STATS_FILENAME_MAXLEN, filename_fmt,
                prog_name);
            f = __rts_fopen (stats_filename,"w");
        }
        if (f == NULL) {
            errorBelch("Can't open stats file %s\n", filename);
            return -1;
        }
    }
    *file_ret = f;

    return 0;
}

/* -----------------------------------------------------------------------------
 * initStatsFile: write a line to the file containing the program name
 * and the arguments it was invoked with.
-------------------------------------------------------------------------- */

static void initStatsFile (FILE *f)
{
    /* Write prog_argv and rts_argv into start of stats file */
    int count;
    for (count = 0; count < prog_argc; count++) {
        stats_fprintf(f, "%s ", prog_argv[count]);
    }
    stats_fprintf(f, "+RTS ");
    for (count = 0; count < rts_argc; count++)
        stats_fprintf(f, "%s ", rts_argv[count]);
    stats_fprintf(f, "\n");
}

/* -----------------------------------------------------------------------------
 * decodeSize: parse a string containing a size, like 300K or 1.2M
-------------------------------------------------------------------------- */

static StgWord64
decodeSize(const char *flag, uint32_t offset, StgWord64 min, StgWord64 max)
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
        errorBelch("error in RTS option %s: size outside allowed range (%" FMT_Word " - %" FMT_Word ")", flag, (W_)min, (W_)max);
        stg_exit(EXIT_FAILURE);
    }

    return val;
}

#if defined(DEBUG)
static void read_debug_flags(const char* arg)
{
    // Already parsed "-D"
    const char *c;
    for (c  = arg + 2; *c != '\0'; c++) {
        switch (*c) {
        case 's':
            RtsFlags.DebugFlags.scheduler = true;
            break;
        case 'i':
            RtsFlags.DebugFlags.interpreter = true;
            break;
        case 'w':
            RtsFlags.DebugFlags.weak = true;
            break;
        case 'G':
            RtsFlags.DebugFlags.gccafs = true;
            break;
        case 'g':
            RtsFlags.DebugFlags.gc = true;
            break;
        case 'b':
            RtsFlags.DebugFlags.block_alloc = true;
            break;
        case 'S':
            RtsFlags.DebugFlags.sanity = true;
            break;
        case 't':
            RtsFlags.DebugFlags.stable = true;
            break;
        case 'p':
            RtsFlags.DebugFlags.prof = true;
            break;
        case 'l':
            RtsFlags.DebugFlags.linker = true;
            break;
        case 'a':
            RtsFlags.DebugFlags.apply = true;
            break;
        case 'm':
            RtsFlags.DebugFlags.stm = true;
            break;
        case 'z':
            RtsFlags.DebugFlags.squeeze = true;
            break;
        case 'c':
            RtsFlags.DebugFlags.hpc = true;
            break;
        case 'r':
            RtsFlags.DebugFlags.sparks = true;
            break;
        case 'C':
            RtsFlags.DebugFlags.compact = true;
            break;
        default:
            bad_option( arg );
        }
    }
    // -Dx also turns on -v.  Use -l to direct trace
    // events to the .eventlog file instead.
    RtsFlags.TraceFlags.tracing = TRACE_STDERR;
}
#endif

#if defined(PROFILING)
// Parse a "-h" flag, returning whether the parse resulted in an error.
static bool read_heap_profiling_flag(const char *arg)
{
    // Already parsed "-h"

    bool error = false;
    switch (arg[2]) {
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
        if (arg[2] != '\0' && arg[3] != '\0') {
            {
                const char *left  = strchr(arg, '{');
                const char *right = strrchr(arg, '}');

                // curly braces are optional, for
                // backwards compat.
                if (left)
                    left = left+1;
                else
                    left = arg + 3;

                if (!right)
                    right = arg + strlen(arg);

                char *selector = stgStrndup(left, right - left + 1);

                switch (arg[2]) {
                case 'c': // cost centre label select
                    RtsFlags.ProfFlags.ccSelector = selector;
                    break;
                case 'C':
                    RtsFlags.ProfFlags.ccsSelector = selector;
                    break;
                case 'M':
                case 'm': // cost centre module select
                    RtsFlags.ProfFlags.modSelector = selector;
                    break;
                case 'D':
                case 'd': // closure descr select
                    RtsFlags.ProfFlags.descrSelector = selector;
                    break;
                case 'Y':
                case 'y': // closure type select
                    RtsFlags.ProfFlags.typeSelector = selector;
                    break;
                case 'R':
                case 'r': // retainer select
                    RtsFlags.ProfFlags.retainerSelector = selector;
                    break;
                case 'B':
                case 'b': // biography select
                    RtsFlags.ProfFlags.bioSelector = selector;
                    break;
                default:
                    free(selector);
                }
            }
            break;
        }

        if (RtsFlags.ProfFlags.doHeapProfile != 0) {
            errorBelch("multiple heap profile options");
            error = true;
            break;
        }

        switch (arg[2]) {
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
        case 'T':
            RtsFlags.ProfFlags.doHeapProfile = HEAP_BY_CLOSURE_TYPE;
            break;
        }
        break;

    default:
        errorBelch("invalid heap profile option: %s", arg);
        error = true;
    }

    return error;
}
#endif

#if defined(TRACING)
static void read_trace_flags(const char *arg)
{
    const char *c;
    bool enabled = true;
    /* Syntax for tracing flags currently looks like:
     *
     *   -l    To turn on eventlog tracing with default trace classes
     *   -lx   Turn on class 'x' (for some class listed below)
     *   -l-x  Turn off class 'x'
     *   -la   Turn on all classes
     *   -l-a  Turn off all classes
     *
     * This lets users say things like:
     *   -la-p    "all but sparks"
     *   -l-ap    "only sparks"
     */

    /* Start by turning on the default tracing flags.
     *
     * Currently this is all the trace classes, except full-detail sparks.
     * Similarly, in future we might default to slightly less verbose
     * scheduler or GC tracing.
     */
    RtsFlags.TraceFlags.scheduler      = true;
    RtsFlags.TraceFlags.gc             = true;
    RtsFlags.TraceFlags.sparks_sampled = true;
    RtsFlags.TraceFlags.user           = true;

    for (c  = arg; *c != '\0'; c++) {
        switch(*c) {
        case '\0':
            break;
        case '-':
            enabled = false;
            break;
        case 'a':
            RtsFlags.TraceFlags.scheduler      = enabled;
            RtsFlags.TraceFlags.gc             = enabled;
            RtsFlags.TraceFlags.sparks_sampled = enabled;
            RtsFlags.TraceFlags.sparks_full    = enabled;
            RtsFlags.TraceFlags.user           = enabled;
            enabled = true;
            break;

        case 's':
            RtsFlags.TraceFlags.scheduler = enabled;
            enabled = true;
            break;
        case 'p':
            RtsFlags.TraceFlags.sparks_sampled = enabled;
            enabled = true;
            break;
        case 'f':
            RtsFlags.TraceFlags.sparks_full = enabled;
            enabled = true;
            break;
        case 't':
            RtsFlags.TraceFlags.timestamp = enabled;
            enabled = true;
            break;
        case 'g':
            RtsFlags.TraceFlags.gc        = enabled;
            enabled = true;
            break;
        case 'u':
            RtsFlags.TraceFlags.user      = enabled;
            enabled = true;
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

/* ----------------------------------------------------------------------------
   Copying and freeing argc/argv
   ------------------------------------------------------------------------- */

static char * copyArg(char *arg)
{
    char *new_arg = stgMallocBytes(strlen(arg) + 1, "copyArg");
    strcpy(new_arg, arg);
    return new_arg;
}

static char ** copyArgv(int argc, char *argv[])
{
    int i;
    char **new_argv;

    new_argv = stgCallocBytes(argc + 1, sizeof (char *), "copyArgv 1");
    for (i = 0; i < argc; i++) {
        new_argv[i] = copyArg(argv[i]);
    }
    new_argv[argc] = NULL;
    return new_argv;
}

static void freeArgv(int argc, char *argv[])
{
    int i;
    if (argv != NULL) {
        for (i = 0; i < argc; i++) {
            stgFree(argv[i]);
        }
        stgFree(argv);
    }
}

/* -----------------------------------------------------------------------------
   Getting/Setting the program's arguments.

   These are used by System.Environment, and parts of the RTS.
   -------------------------------------------------------------------------- */

void
setProgName(char *argv[])
{
    char *last_slash;

    if (argv[0] == NULL) { // #7037
        prog_name = "";
        return;
    }

    /* Remove directory from argv[0] -- default files in current directory */
#if !defined(mingw32_HOST_OS)
    if ( (last_slash = (char *) strrchr(argv[0], '/')) != NULL ) {
        prog_name = last_slash+1;
   } else {
        prog_name = argv[0];
   }
#else
    last_slash = argv[0] + (strlen(argv[0]) - 1);
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
    freeArgv(prog_argc,prog_argv);
    prog_argc = argc;
    prog_argv = copyArgv(argc,argv);
    setProgName(prog_argv);
}

static void
freeProgArgv(void)
{
    freeArgv(prog_argc,prog_argv);
    prog_argc = 0;
    prog_argv = NULL;
}

/* ----------------------------------------------------------------------------
   The full argv - a copy of the original program's argc/argv
   ------------------------------------------------------------------------- */

void
setFullProgArgv(int argc, char *argv[])
{
    full_prog_argc = argc;
    full_prog_argv = copyArgv(argc,argv);
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
freeFullProgArgv (void)
{
    freeArgv(full_prog_argc, full_prog_argv);
    full_prog_argc = 0;
    full_prog_argv = NULL;
}

/* ----------------------------------------------------------------------------
   The Win32 argv
   ------------------------------------------------------------------------- */

#if defined(mingw32_HOST_OS)
void freeWin32ProgArgv (void);

void
freeWin32ProgArgv (void)
{
    if(win32_utf8_argv == NULL) {
        return;
    }
    else
    {
        freeArgv(win32_full_utf8_argc, win32_full_utf8_argv);
        stgFree(win32_utf8_argv);
    }


}

#endif

/* ----------------------------------------------------------------------------
   The RTS argv
   ------------------------------------------------------------------------- */

static void
freeRtsArgv(void)
{
    freeArgv(rts_argc,rts_argv);
    rts_argc = 0;
    rts_argv = NULL;
    rts_argv_size = 0;
}

/* ----------------------------------------------------------------------------
   All argvs
   ------------------------------------------------------------------------- */

void freeRtsArgs(void)
{
#if defined(mingw32_HOST_OS)
    freeWin32ProgArgv();
#endif
    freeFullProgArgv();
    freeProgArgv();
    freeRtsArgv();
}


/*
Note [OPTION_SAFE vs OPTION_UNSAFE]

Ticket #3910 originally pointed out that the RTS options are a potential
security problem. For example the -t -s or -S flags can be used to
overwrite files. This would be bad in the context of CGI scripts or
setuid binaries. So we introduced a system where +RTS processing is more
or less disabled unless you pass the -rtsopts flag at link time.

This scheme is safe enough but it also really annoyes users. They have
to use -rtsopts in many circumstances: with -threaded to use -N, with
-eventlog to use -l, with -prof to use any of the profiling flags. Many
users just set -rtsopts globally or in project .cabal files. Apart from
annoying users it reduces security because it means that deployed
binaries will have all RTS options enabled rather than just profiling
ones.

So now, we relax the set of RTS options that are available in the
default -rtsopts=some case. For "deployment" ways like vanilla and
-threaded we remain quite conservative. Only --info -? --help are
allowed for vanilla. For -threaded, -N and -N<x> are allowed with a
check that x <= num cpus.

For "developer" ways like -debug, -eventlog, -prof, we allow all the
options that are special to that way. Some of these allow writing files,
but the file written is not directly under the control of the attacker.
For the setuid case (where the attacker would have control over binary
name, current dir, local symlinks etc) we check if the process is
running setuid/setgid and refuse all RTS option processing. Users would
need to use -rtsopts=all in this case.

We are making the assumption that developers will not deploy binaries
built in the -debug, -eventlog, -prof ways. And even if they do, the
damage should be limited to DOS, information disclosure and writing
files like <progname>.eventlog, not arbitrary files.
*/
