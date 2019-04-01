/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * API for invoking Haskell functions via the RTS
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * --------------------------------------------------------------------------*/

#pragma once

#if defined(__cplusplus)
extern "C" {
#endif

#include "HsFFI.h"
#include "rts/Time.h"
#include "rts/EventLogWriter.h"

/*
 * Running the scheduler
 */
typedef enum {
    NoStatus,    /* not finished yet */
    Success,     /* completed successfully */
    Killed,      /* uncaught exception */
    Interrupted, /* stopped in response to a call to interruptStgRts */
    HeapExhausted /* out of memory */
} SchedulerStatus;

typedef struct StgClosure_ *HaskellObj;

/*
 * An abstract type representing the token returned by rts_lock() and
 * used when allocating objects and threads in the RTS.
 */
typedef struct Capability_ Capability;

/*
 * The public view of a Capability: we can be sure it starts with
 * these two components (but it may have more private fields).
 */
typedef struct CapabilityPublic_ {
    StgFunTable f;
    StgRegTable r;
} CapabilityPublic;

/* ----------------------------------------------------------------------------
   RTS configuration settings, for passing to hs_init_ghc()
   ------------------------------------------------------------------------- */

typedef enum {
    RtsOptsNone,         // +RTS causes an error
    RtsOptsIgnore,       // Ignore command line arguments
    RtsOptsIgnoreAll,    // Ignore command line and Environment arguments
    RtsOptsSafeOnly,     // safe RTS options allowed; others cause an error
    RtsOptsAll           // all RTS options allowed
  } RtsOptsEnabledEnum;

struct GCDetails_;

// The RtsConfig struct is passed (by value) to hs_init_ghc().  The
// reason for using a struct is extensibility: we can add more
// fields to this later without breaking existing client code.
typedef struct {

    // Whether to interpret +RTS options on the command line
    RtsOptsEnabledEnum rts_opts_enabled;

    // Whether to give RTS flag suggestions
    HsBool rts_opts_suggestions;

    // additional RTS options
    const char *rts_opts;

    // True if GHC was not passed -no-hs-main
    HsBool rts_hs_main;

    // Whether to retain CAFs (default: false)
    HsBool keep_cafs;

    // Writer a for eventlog.
    const EventLogWriter *eventlog_writer;

    // Called before processing command-line flags, so that default
    // settings for RtsFlags can be provided.
    void (* defaultsHook) (void);

    // Called just before exiting
    void (* onExitHook) (void);

    // Called on a stack overflow, before exiting
    void (* stackOverflowHook) (W_ stack_size);

    // Called on heap overflow, before exiting
    void (* outOfHeapHook) (W_ request_size, W_ heap_size);

    // Called when malloc() fails, before exiting
    void (* mallocFailHook) (W_ request_size /* in bytes */, const char *msg);

    // Called for every GC
    void (* gcDoneHook) (const struct GCDetails_ *stats);

    // Called when GC sync takes too long (+RTS --long-gc-sync=<time>)
    void (* longGCSync) (uint32_t this_cap, Time time_ns);
    void (* longGCSyncEnd) (Time time_ns);
} RtsConfig;

// Clients should start with defaultRtsConfig and then customise it.
// Bah, I really wanted this to be a const struct value, but it seems
// you can't do that in C (it generates code).
extern const RtsConfig defaultRtsConfig;

/* -----------------------------------------------------------------------------
   Statistics
   -------------------------------------------------------------------------- */

//
// Stats about a single GC
//
typedef struct GCDetails_ {
    // The generation number of this GC
  uint32_t gen;
    // Number of threads used in this GC
  uint32_t threads;
    // Number of bytes allocated since the previous GC
  uint64_t allocated_bytes;
    // Total amount of live data in the heap (incliudes large + compact data).
    // Updated after every GC. Data in uncollected generations (in minor GCs)
    // are considered live.
  uint64_t live_bytes;
    // Total amount of live data in large objects
  uint64_t large_objects_bytes;
    // Total amount of live data in compact regions
  uint64_t compact_bytes;
    // Total amount of slop (wasted memory)
  uint64_t slop_bytes;
    // Total amount of memory in use by the RTS
  uint64_t mem_in_use_bytes;
    // Total amount of data copied during this GC
  uint64_t copied_bytes;
    // In parallel GC, the max amount of data copied by any one thread
  uint64_t par_max_copied_bytes;
  // In parallel GC, the amount of balanced data copied by all threads
  uint64_t par_balanced_copied_bytes;
    // The time elapsed during synchronisation before GC
  Time sync_elapsed_ns;
    // The CPU time used during GC itself
  Time cpu_ns;
    // The time elapsed during GC itself
  Time elapsed_ns;
} GCDetails;

//
// Stats about the RTS currently, and since the start of execution
//
typedef struct _RTSStats {

  // -----------------------------------
  // Cumulative stats about memory use

    // Total number of GCs
  uint32_t gcs;
    // Total number of major (oldest generation) GCs
  uint32_t major_gcs;
    // Total bytes allocated
  uint64_t allocated_bytes;
    // Maximum live data (including large objects + compact regions) in the
    // heap. Updated after a major GC.
  uint64_t max_live_bytes;
    // Maximum live data in large objects
  uint64_t max_large_objects_bytes;
    // Maximum live data in compact regions
  uint64_t max_compact_bytes;
    // Maximum slop
  uint64_t max_slop_bytes;
    // Maximum memory in use by the RTS
  uint64_t max_mem_in_use_bytes;
    // Sum of live bytes across all major GCs.  Divided by major_gcs
    // gives the average live data over the lifetime of the program.
  uint64_t cumulative_live_bytes;
    // Sum of copied_bytes across all GCs
  uint64_t copied_bytes;
    // Sum of copied_bytes across all parallel GCs
  uint64_t par_copied_bytes;
    // Sum of par_max_copied_bytes across all parallel GCs
  uint64_t cumulative_par_max_copied_bytes;
    // Sum of par_balanced_copied_byes across all parallel GCs.
  uint64_t cumulative_par_balanced_copied_bytes;

  // -----------------------------------
  // Cumulative stats about time use
  // (we use signed values here because due to inaccuracies in timers
  // the values can occasionally go slightly negative)

    // Total CPU time used by the init phase
  Time init_cpu_ns;
    // Total elapsed time used by the init phase
  Time init_elapsed_ns;
    // Total CPU time used by the mutator
  Time mutator_cpu_ns;
    // Total elapsed time used by the mutator
  Time mutator_elapsed_ns;
    // Total CPU time used by the GC
  Time gc_cpu_ns;
    // Total elapsed time used by the GC
  Time gc_elapsed_ns;
    // Total CPU time (at the previous GC)
  Time cpu_ns;
    // Total elapsed time (at the previous GC)
  Time elapsed_ns;

  // -----------------------------------
  // Stats about the most recent GC

  GCDetails gc;

  // -----------------------------------
  // Internal Counters

    // The number of times a GC thread spun on its 'gc_spin' lock.
    // Will be zero if the rts was not built with PROF_SPIN
  uint64_t gc_spin_spin;
    // The number of times a GC thread yielded on its 'gc_spin' lock.
    // Will be zero if the rts was not built with PROF_SPIN
  uint64_t gc_spin_yield;
    // The number of times a GC thread spun on its 'mut_spin' lock.
    // Will be zero if the rts was not built with PROF_SPIN
  uint64_t mut_spin_spin;
    // The number of times a GC thread yielded on its 'mut_spin' lock.
    // Will be zero if the rts was not built with PROF_SPIN
  uint64_t mut_spin_yield;
    // The number of times a GC thread has checked for work across all parallel
    // GCs
  uint64_t any_work;
    // The number of times a GC thread has checked for work and found none
    // across all parallel GCs
  uint64_t no_work;
    // The number of times a GC thread has iterated it's outer loop across all
    // parallel GCs
  uint64_t scav_find_work;
} RTSStats;

void getRTSStats (RTSStats *s);
int getRTSStatsEnabled (void);

// Returns the total number of bytes allocated since the start of the program.
// TODO: can we remove this?
uint64_t getAllocations (void);

/* ----------------------------------------------------------------------------
   Starting up and shutting down the Haskell RTS.
   ------------------------------------------------------------------------- */

/* DEPRECATED, use hs_init() or hs_init_ghc() instead  */
extern void startupHaskell         ( int argc, char *argv[],
                                     void (*init_root)(void) );

/* DEPRECATED, use hs_exit() instead  */
extern void shutdownHaskell        ( void );

/* Like hs_init(), but allows rtsopts. For more complicated usage,
 * use hs_init_ghc. */
extern void hs_init_with_rtsopts (int *argc, char **argv[]);

/*
 * GHC-specific version of hs_init() that allows specifying whether
 * +RTS ... -RTS options are allowed or not (default: only "safe"
 * options are allowed), and allows passing an option string that is
 * to be interpreted by the RTS only, not passed to the program.
 */
extern void hs_init_ghc (int *argc, char **argv[],   // program arguments
                         RtsConfig rts_config);      // RTS configuration

extern void shutdownHaskellAndExit (int exitCode, int fastExit)
    GNUC3_ATTRIBUTE(__noreturn__);

#if !defined(mingw32_HOST_OS)
extern void shutdownHaskellAndSignal (int sig, int fastExit)
     GNUC3_ATTRIBUTE(__noreturn__);
#endif

extern void getProgArgv            ( int *argc, char **argv[] );
extern void setProgArgv            ( int argc, char *argv[] );
extern void getFullProgArgv        ( int *argc, char **argv[] );
extern void setFullProgArgv        ( int argc, char *argv[] );
extern void freeFullProgArgv       ( void ) ;

/* exit() override */
extern void (*exitFn)(int);

/* ----------------------------------------------------------------------------
   Locking.

   You have to surround all access to the RtsAPI with these calls.
   ------------------------------------------------------------------------- */

// acquires a token which may be used to create new objects and
// evaluate them.
Capability *rts_lock (void);

// releases the token acquired with rts_lock().
void rts_unlock (Capability *token);

// If you are in a context where you know you have a current capability but
// do not know what it is, then use this to get it. Basically this only
// applies to "unsafe" foreign calls (as unsafe foreign calls are made with
// the capability held).
//
// WARNING: There is *no* guarantee this returns anything sensible (eg NULL)
// when there is no current capability.
Capability *rts_unsafeGetMyCapability (void);

/* ----------------------------------------------------------------------------
   Which cpu should the OS thread and Haskell thread run on?

   1. Run the current thread on the given capability:
     rts_setInCallCapability(cap, 0);

   2. Run the current thread on the given capability and set the cpu affinity
      for this thread:
     rts_setInCallCapability(cap, 1);

   3. Run the current thread on the given numa node:
     rts_pinThreadToNumaNode(node);

   4. Run the current thread on the given capability and on the given numa node:
     rts_setInCallCapability(cap, 0);
     rts_pinThreadToNumaNode(cap);
   ------------------------------------------------------------------------- */

// Specify the Capability that the current OS thread should run on when it calls
// into Haskell.  The actual capability will be calculated as the supplied
// value modulo the number of enabled Capabilities.
//
// Note that the thread may still be migrated by the RTS scheduler, but that
// will only happen if there are multiple threads running on one Capability and
// another Capability is free.
//
// If affinity is non-zero, the current thread will be bound to
// specific CPUs according to the prevailing affinity policy for the
// specified capability, set by either +RTS -qa or +RTS --numa.
void rts_setInCallCapability (int preferred_capability, int affinity);

// Specify the CPU Node that the current OS thread should run on when it calls
// into Haskell. The argument can be either a node number or capability number.
// The actual node will be calculated as the supplied value modulo the number
// of numa nodes.
void rts_pinThreadToNumaNode (int node);

/* ----------------------------------------------------------------------------
   Building Haskell objects from C datatypes.
   ------------------------------------------------------------------------- */
HaskellObj   rts_mkChar       ( Capability *, HsChar   c );
HaskellObj   rts_mkInt        ( Capability *, HsInt    i );
HaskellObj   rts_mkInt8       ( Capability *, HsInt8   i );
HaskellObj   rts_mkInt16      ( Capability *, HsInt16  i );
HaskellObj   rts_mkInt32      ( Capability *, HsInt32  i );
HaskellObj   rts_mkInt64      ( Capability *, HsInt64  i );
HaskellObj   rts_mkWord       ( Capability *, HsWord   w );
HaskellObj   rts_mkWord8      ( Capability *, HsWord8  w );
HaskellObj   rts_mkWord16     ( Capability *, HsWord16 w );
HaskellObj   rts_mkWord32     ( Capability *, HsWord32 w );
HaskellObj   rts_mkWord64     ( Capability *, HsWord64 w );
HaskellObj   rts_mkPtr        ( Capability *, HsPtr    a );
HaskellObj   rts_mkFunPtr     ( Capability *, HsFunPtr a );
HaskellObj   rts_mkFloat      ( Capability *, HsFloat  f );
HaskellObj   rts_mkDouble     ( Capability *, HsDouble f );
HaskellObj   rts_mkStablePtr  ( Capability *, HsStablePtr s );
HaskellObj   rts_mkBool       ( Capability *, HsBool   b );
HaskellObj   rts_mkString     ( Capability *, char    *s );

HaskellObj   rts_apply        ( Capability *, HaskellObj, HaskellObj );

/* ----------------------------------------------------------------------------
   Deconstructing Haskell objects
   ------------------------------------------------------------------------- */
HsChar       rts_getChar      ( HaskellObj );
HsInt        rts_getInt       ( HaskellObj );
HsInt8       rts_getInt8      ( HaskellObj );
HsInt16      rts_getInt16     ( HaskellObj );
HsInt32      rts_getInt32     ( HaskellObj );
HsInt64      rts_getInt64     ( HaskellObj );
HsWord       rts_getWord      ( HaskellObj );
HsWord8      rts_getWord8     ( HaskellObj );
HsWord16     rts_getWord16    ( HaskellObj );
HsWord32     rts_getWord32    ( HaskellObj );
HsWord64     rts_getWord64    ( HaskellObj );
HsPtr        rts_getPtr       ( HaskellObj );
HsFunPtr     rts_getFunPtr    ( HaskellObj );
HsFloat      rts_getFloat     ( HaskellObj );
HsDouble     rts_getDouble    ( HaskellObj );
HsStablePtr  rts_getStablePtr ( HaskellObj );
HsBool       rts_getBool      ( HaskellObj );

/* ----------------------------------------------------------------------------
   Evaluating Haskell expressions

   The versions ending in '_' allow you to specify an initial stack size.
   Note that these calls may cause Garbage Collection, so all HaskellObj
   references are rendered invalid by these calls.

   All of these functions take a (Capability **) - there is a
   Capability pointer both input and output.  We use an inout
   parameter because this is less error-prone for the client than a
   return value - the client could easily forget to use the return
   value, whereas incorrectly using an inout parameter will usually
   result in a type error.
   ------------------------------------------------------------------------- */

void rts_eval (/* inout */ Capability **,
               /* in    */ HaskellObj p,
               /* out */   HaskellObj *ret);

void rts_eval_ (/* inout */ Capability **,
                /* in    */ HaskellObj p,
                /* in    */ unsigned int stack_size,
                /* out   */ HaskellObj *ret);

void rts_evalIO (/* inout */ Capability **,
                 /* in    */ HaskellObj p,
                 /* out */   HaskellObj *ret);

void rts_evalStableIOMain (/* inout */ Capability **,
                           /* in    */ HsStablePtr s,
                           /* out */   HsStablePtr *ret);

void rts_evalStableIO (/* inout */ Capability **,
                       /* in    */ HsStablePtr s,
                       /* out */   HsStablePtr *ret);

void rts_evalLazyIO (/* inout */ Capability **,
                     /* in    */ HaskellObj p,
                     /* out */   HaskellObj *ret);

void rts_evalLazyIO_ (/* inout */ Capability **,
                      /* in    */ HaskellObj p,
                      /* in    */ unsigned int stack_size,
                      /* out   */ HaskellObj *ret);

void rts_checkSchedStatus (char* site, Capability *);

SchedulerStatus rts_getSchedStatus (Capability *cap);

/*
 * The RTS allocates some thread-local data when you make a call into
 * Haskell using one of the rts_eval() functions.  This data is not
 * normally freed until hs_exit().  If you want to free it earlier
 * than this, perhaps because the thread is about to exit, then call
 * rts_done() from the thread.
 *
 * It is safe to make more rts_eval() calls after calling rts_done(),
 * but the next one will cause allocation of the thread-local memory
 * again.
 */
void rts_done (void);

/* --------------------------------------------------------------------------
   Wrapper closures

   These are used by foreign export and foreign import "wrapper" stubs.
   ----------------------------------------------------------------------- */

// When producing Windows DLLs the we need to know which symbols are in the
//      local package/DLL vs external ones.
//
//      Note that RtsAPI.h is also included by foreign export stubs in
//      the base package itself.
//
#if defined(COMPILING_WINDOWS_DLL) && !defined(COMPILING_BASE_PACKAGE)
__declspec(dllimport) extern StgWord base_GHCziTopHandler_runIO_closure[];
__declspec(dllimport) extern StgWord base_GHCziTopHandler_runNonIO_closure[];
#else
extern StgWord base_GHCziTopHandler_runIO_closure[];
extern StgWord base_GHCziTopHandler_runNonIO_closure[];
#endif

#define runIO_closure     base_GHCziTopHandler_runIO_closure
#define runNonIO_closure  base_GHCziTopHandler_runNonIO_closure

/* ------------------------------------------------------------------------ */

#if defined(__cplusplus)
}
#endif
