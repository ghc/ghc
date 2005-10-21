/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * API for invoking Haskell functions via the RTS
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
    Interrupted  /* stopped in response to a call to interruptStgRts */
} SchedulerStatus;

typedef StgClosure *HaskellObj;

/*
 * An abstract type representing the token returned by rts_lock() and
 * used when allocating objects and threads in the RTS.
 */
typedef struct Capability_ Capability;

/* ----------------------------------------------------------------------------
   Starting up and shutting down the Haskell RTS.
   ------------------------------------------------------------------------- */
extern void startupHaskell         ( int argc, char *argv[], 
				     void (*init_root)(void) );
extern void shutdownHaskell        ( void );
extern void shutdownHaskellAndExit ( int exitCode );
extern void getProgArgv            ( int *argc, char **argv[] );
extern void setProgArgv            ( int argc, char *argv[] );


/* ----------------------------------------------------------------------------
   Locking.
   
   You have to surround all access to the RtsAPI with these calls.
   ------------------------------------------------------------------------- */
   
// acquires a token which may be used to create new objects and
// evaluate them.
Capability *rts_lock (void);

// releases the token acquired with rts_lock().
void rts_unlock (Capability *token);

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
   ------------------------------------------------------------------------- */
Capability * 
rts_eval (Capability *, HaskellObj p, /*out*/HaskellObj *ret);

Capability * 
rts_eval_ (Capability *, HaskellObj p, unsigned int stack_size, 
	   /*out*/HaskellObj *ret);

Capability * 
rts_evalIO (Capability *, HaskellObj p, /*out*/HaskellObj *ret);

Capability *
rts_evalStableIO (Capability *, HsStablePtr s, /*out*/HsStablePtr *ret);

Capability * 
rts_evalLazyIO (Capability *, HaskellObj p, /*out*/HaskellObj *ret);

Capability * 
rts_evalLazyIO_ (Capability *, HaskellObj p, unsigned int stack_size, 
		 /*out*/HaskellObj *ret);

void
rts_checkSchedStatus (char* site, Capability *);

SchedulerStatus
rts_getSchedStatus (Capability *cap);

/* --------------------------------------------------------------------------
   Wrapper closures

   These are used by foreign export and foreign import "wrapper" stubs.
   ----------------------------------------------------------------------- */

extern StgWord GHCziTopHandler_runIO_closure[];
extern StgWord GHCziTopHandler_runNonIO_closure[];
#define runIO_closure		  GHCziTopHandler_runIO_closure
#define runNonIO_closure	  GHCziTopHandler_runNonIO_closure

/* ------------------------------------------------------------------------ */

#ifdef __cplusplus
}
#endif

#endif /* RTSAPI_H */
