/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * API for invoking Haskell functions via the RTS
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * --------------------------------------------------------------------------*/

#ifndef RTSAPI_H
#define RTSAPI_H

#ifdef __cplusplus
extern "C" {
#endif

#include "HsFFI.h"

/*
 * Running the scheduler
 */
typedef enum {
    NoStatus,    /* not finished yet */
    Success,	 /* completed successfully */
    Killed,	 /* uncaught exception */
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
    RtsOptsSafeOnly,     // safe RTS options allowed; others cause an error
    RtsOptsAll           // all RTS options allowed
  } RtsOptsEnabledEnum;

// The RtsConfig struct is passed (by value) to hs_init_ghc().  The
// reason for using a struct is extensibility: we can add more
// fields to this later without breaking existing client code.
typedef struct {
    RtsOptsEnabledEnum rts_opts_enabled;
    const char *rts_opts;
    HsBool rts_hs_main;
} RtsConfig;

// Clients should start with defaultRtsConfig and then customise it.
// Bah, I really wanted this to be a const struct value, but it seems
// you can't do that in C (it generates code).
extern const RtsConfig defaultRtsConfig;

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

#ifndef mingw32_HOST_OS
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
//	local package/DLL vs external ones. 
//
//	Note that RtsAPI.h is also included by foreign export stubs in
//	the base package itself.
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

#ifdef __cplusplus
}
#endif

#endif /* RTSAPI_H */
