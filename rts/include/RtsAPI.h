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
#include "rts/Types.h"

/*
 * Running the scheduler
 */
typedef enum {
    NoStatus,    /* not finished yet */
    Success,     /* completed successfully */
    Killed,      /* uncaught exception */
    Interrupted, /* stopped in response to a call to interruptStgRts */
    HeapExhausted, /* out of memory */
    SchedulerStatus_End, /* last value in enum */
} SchedulerStatus;

typedef struct StgClosure_ *HaskellObj;

/*
 * An abstract type representing the token returned by rts_lock() and
 * used when allocating objects and threads in the RTS.
 */
typedef struct Capability_ Capability;

/*
 * An abstract type representing the token returned by rts_pause().
 */
typedef struct PauseToken_ PauseToken;

/*
 * From a PauseToken, get a Capability token used when allocating objects and
 * threads in the RTS.
 */
Capability *pauseTokenCapability(PauseToken *pauseToken);

/*
 * The public view of a Capability: we can be sure it starts with
 * these two components (but it may have more private fields).
 */
typedef struct CapabilityPublic_ {
    StgFunTable f;
    StgRegTable r;
} CapabilityPublic;

/* N.B. this needs the Capability declaration above. */
#include "rts/EventLogWriter.h"

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
    // Total amount of live data in the heap (includes large + compact data).
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
    // Memory lost due to block fragmentation
  uint64_t block_fragmentation_bytes;
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

    //
    // Concurrent garbage collector
    //

    // The CPU time used during the post-mark pause phase of the concurrent
    // nonmoving GC.
  Time nonmoving_gc_sync_cpu_ns;
    // The time elapsed during the post-mark pause phase of the concurrent
    // nonmoving GC.
  Time nonmoving_gc_sync_elapsed_ns;
    // The total CPU time used by the nonmoving GC.
  Time nonmoving_gc_cpu_ns;
    // The total time elapsed during which there is a nonmoving GC active.
  Time nonmoving_gc_elapsed_ns;
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

  uint64_t any_work;
    // The number of times a GC thread has iterated it's outer loop across all
    // parallel GCs
  uint64_t scav_find_work;

  uint64_t max_n_todo_overflow;

  // ----------------------------------
  // Concurrent garbage collector

    // The total CPU time used during the post-mark pause phase of the
    // concurrent nonmoving GC.
  Time nonmoving_gc_sync_cpu_ns;
    // The total time elapsed during the post-mark pause phase of the
    // concurrent nonmoving GC.
  Time nonmoving_gc_sync_elapsed_ns;
    // The maximum time elapsed during the post-mark pause phase of the
    // concurrent nonmoving GC.
  Time nonmoving_gc_sync_max_elapsed_ns;
    // The total CPU time used by the nonmoving GC.
  Time nonmoving_gc_cpu_ns;
    // The total time elapsed during which there is a nonmoving GC active.
  Time nonmoving_gc_elapsed_ns;
    // The maximum time elapsed during any nonmoving GC cycle.
  Time nonmoving_gc_max_elapsed_ns;
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
    STG_NORETURN;

#if !defined(mingw32_HOST_OS)
extern void shutdownHaskellAndSignal (int sig, int fastExit)
    STG_NORETURN;
#endif

extern void getProgArgv            ( int *argc, char **argv[] );
extern void setProgArgv            ( int argc, char *argv[] );
extern void getFullProgArgv        ( int *argc, char **argv[] );
extern void setFullProgArgv        ( int argc, char *argv[] );
extern void freeFullProgArgv       ( void ) ;

/* exit() override */
extern void (*exitFn)(int);

/* Note [Locking and Pausing the RTS]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You have to surround all access to the RtsAPI with rts_lock/rts_unlock or
with rts_pause/rts_resume.


# rts_lock / rts_unlock

Use `rts_lock` to acquire a token which may be used to call other RtsAPI
functions and call `rts_unlock` to return the token. When locked, garbage
collection will not occur. As long as 1 or more capabilities are not locked,
haskell threads will continue to execute. If you want to pause execution of
all haskell threads then use rts_pause/rts_resume instead.

The implementation of `rts_lock` acquires a capability for this thread. Hence,
at most n locks can be held simultaneously, where n is the number of
capabilities. It is an error to call `rts_lock` when the rts is already
paused by the current OS thread (see rts_pause/rts_resume below).


# rts_pause / rts_resume

Use `rts_pause` to pause execution of all Haskell threads and `rts_resume` to
resume them. The implementation acquires all capabilities. `rts_resume`
must be called on the same thread as `rts_pause`. `rts_pause`, much like
rts_lock, returns a token. A `Capability` can be extracted from that token using
`pauseTokenCapability()`. The `Capability` can then be used to call other RtsAPI
functions.

* With the RTS paused, garbage collections will not occur and haskell threads
  will not execute, allocate, nor mutate their stacks.
* Non-Haskell (i.e. non-worker) threads such as those running safe FFI calls
  will NOT be paused and can still mutate pinned mutable data such as pinned
  `MutableByteArray#`s.
* You may call `rts_pause` from within a non-worker OS thread.
* You may call `rts_pause` from within a *safe* FFI call. In this case, make
  sure to call `rts_resume` within the same FFI call or the RTS will deadlock.
* Calling `rts_pause` from an *unsafe* FFI call will cause an error.
* On return, the rts will be fully paused: all haskell threads are stopped
  and all capabilities are acquired by the current OS thread.
* Calling `rts_pause` in between rts_lock/rts_unlock on the same thread will
  cause an error.
* Calling `rts_pause` results in an error if the RTS is already paused by the
  current OS thread.
* Only one OS thread at a time can keep the rts paused.
* `rts_pause` will block while another thread is pausing the RTS, and
  continue when the current thread is given exclusive permission to pause the
  RTS.

## Note on implementation.

Thread safety is achieved almost entirely by the mechanism of acquiring and
releasing Capabilities, resulting in a sort of mutex / critical section pattern.
This has the following consequences:

* There are at most `n_capabilities` threads currently in a
  rts_lock/rts_unlock section.
* There is at most 1 threads in a rts_pause/rts_resume section. In that case
  there will be no threads in a rts_lock/rts_unlock section.
* rts_pause and rts_lock may block in order to enforce the above 2
  invariants.

*/

// Acquires a token which may be used to create new objects and evaluate them.
// See Note [Locking and Pausing the RTS] for correct usage.
Capability *rts_lock (void);

// releases the token acquired with rts_lock().
// See Note [Locking and Pausing the RTS] for correct usage.
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

void rts_inCall (/* inout */ Capability **,
                 /* in    */ HaskellObj p,
                 /* out */   HaskellObj *ret);

void rts_checkSchedStatus (char* site, Capability *);

SchedulerStatus rts_getSchedStatus (Capability *cap);

// Halt execution of all Haskell threads.
// See Note [Locking and Pausing the RTS] for correct usage.
PauseToken *rts_pause (void);

// Counterpart of rts_pause: Continue from a pause.
// See Note [Locking and Pausing the RTS] for correct usage.
// [in] pauseToken: the token returned by rts_pause.
void rts_resume (PauseToken *pauseToken);

// Returns true if the rts is paused. See rts_pause() and rts_resume().
bool rts_isPaused(void);

// List all live threads. The RTS must be paused and this must be called on the
// same thread that called rts_pause().
typedef void (*ListThreadsCb)(void *user, StgTSO *);
void rts_listThreads(ListThreadsCb cb, void *user);

// List all non-thread GC roots. The RTS must be paused and this must be called
// on the same thread that called rts_pause().
typedef void (*ListRootsCb)(void *user, StgClosure *);
void rts_listMiscRoots(ListRootsCb cb, void *user);

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
#if defined(COMPILING_WINDOWS_DLL) && !defined(COMPILING_GHC_INTERNAL_PACKAGE)
__declspec(dllimport) extern StgClosure ghczminternal_GHCziInternalziTopHandler_runIO_closure;
__declspec(dllimport) extern StgClosure ghczminternal_GHCziInternalziTopHandler_runNonIO_closure;
#else
extern StgClosure ghczminternal_GHCziInternalziTopHandler_runIO_closure;
extern StgClosure ghczminternal_GHCziInternalziTopHandler_runNonIO_closure;
#endif

#define runIO_closure     DLL_IMPORT_DATA_REF(ghczminternal_GHCziInternalziTopHandler_runIO_closure)
#define runNonIO_closure  DLL_IMPORT_DATA_REF(ghczminternal_GHCziInternalziTopHandler_runNonIO_closure)

/* ------------------------------------------------------------------------ */

// This is a public RTS API function that does its best to zero out
// unused RTS memory. rts_clearMemory() takes the storage manager
// lock. It's only safe to call rts_clearMemory() when all mutators
// have stopped and either minor/major garbage collection has just
// been run.
//
// rts_clearMemory() works for all RTS ways on all platforms, though
// the main intended use case is the pre-initialization of a
// wasm32-wasi reactor module (#22920). A reactor module is like
// shared library on other platforms, with foreign exported Haskell
// functions as entrypoints. At run-time, the user calls hs_init_ghc()
// to initialize the RTS, after that they can invoke Haskell
// computation by calling the exported Haskell functions, persisting
// the memory state across these invocations.
//
// Besides hs_init_ghc(), the user may want to invoke some Haskell
// function to initialize some global state in the user code, this
// global state is used by subsequent invocations. Now, it's possible
// to run hs_init_ghc() & custom init logic in Haskell, then snapshot
// the entire memory into a new wasm module! And the user can call the
// new wasm module's exports directly, thus eliminating the
// initialization overhead at run-time entirely.
//
// There's one problem though. After the custom init logic runs, the
// RTS memory contains a lot of garbage data in various places. These
// garbage data will be snapshotted into the new wasm module, causing
// a significant size bloat. Therefore, we need an RTS API function
// that zeros out unused RTS memory.
//
// At the end of the day, the custom init function will be a small C
// function that first calls hs_init_ghc(), then calls a foreign
// exported Haskell function to initialize whatever global state the
// other Haskell functions need, followed by a hs_perform_gc() call to
// do a major GC, and finally an rts_clearMemory() call to zero out
// the unused RTS memory.
//
// Why add rts_clearMemory(), where there's the -DZ RTS flag that
// zeros freed memory on GC? The -DZ flag actually fills freed memory
// with a garbage byte like 0xAA, and the flag only works in debug
// RTS. Why not add a new RTS flag that zeros freed memory on the go?
// Because it only makes sense to do the zeroing once before
// snapshotting the memory, but there's no point to pay for the
// zeroing overhead at the new module's run-time.
void rts_clearMemory(void);

#if defined(__cplusplus)
}
#endif
