/* ----------------------------------------------------------------------------
 * $Id: RtsAPI.h,v 1.22 2001/08/03 16:30:13 sof Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * API for invoking Haskell functions via the RTS
 *
 * --------------------------------------------------------------------------*/

#ifndef RTSAPI_H
#define RTSAPI_H

#include "HsFFI.h"

/*
 * Running the scheduler
 */
typedef enum {
    NoStatus,    /* not finished yet */
    Success,	 /* completed successfully */
    Killed,	 /* uncaught exception */
    Interrupted, /* stopped in response to a call to interruptStgRts */
    Deadlock	 /* no threads to run, but main thread hasn't finished */
} SchedulerStatus;

typedef StgClosure *HaskellObj;

/* ----------------------------------------------------------------------------
   Starting up and shutting down the Haskell RTS.
   ------------------------------------------------------------------------- */
extern void startupHaskell         ( int argc, char *argv[], 
				     void (*init_root)(void) );
extern void shutdownHaskell        ( void );
extern void shutdownHaskellAndExit ( int exitCode );
extern void setProgArgv            ( int argc, char *argv[] );
extern void getProgArgv            ( int *argc, char **argv[] );

/* ----------------------------------------------------------------------------
   Building Haskell objects from C datatypes.
   ------------------------------------------------------------------------- */
HaskellObj   rts_mkChar       ( HsChar   c );
HaskellObj   rts_mkInt        ( HsInt    i );
HaskellObj   rts_mkInt8       ( HsInt8   i );
HaskellObj   rts_mkInt16      ( HsInt16  i );
HaskellObj   rts_mkInt32      ( HsInt32  i );
HaskellObj   rts_mkInt64      ( HsInt64  i );
HaskellObj   rts_mkWord       ( HsWord   w );
HaskellObj   rts_mkWord8      ( HsWord8  w );
HaskellObj   rts_mkWord16     ( HsWord16 w );
HaskellObj   rts_mkWord32     ( HsWord32 w );
HaskellObj   rts_mkWord64     ( HsWord64 w );
HaskellObj   rts_mkPtr        ( HsPtr    a );
HaskellObj   rts_mkFloat      ( HsFloat  f );
HaskellObj   rts_mkDouble     ( HsDouble f );
HaskellObj   rts_mkStablePtr  ( HsStablePtr s );
HaskellObj   rts_mkBool       ( HsBool   b );
HaskellObj   rts_mkString     ( char    *s );

HaskellObj   rts_apply        ( HaskellObj, HaskellObj );

/* DEPRECATED (use rts_mkPtr): */
HaskellObj   rts_mkAddr       ( HsAddr   a );

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
HsFloat      rts_getFloat     ( HaskellObj );
HsDouble     rts_getDouble    ( HaskellObj );
HsStablePtr  rts_getStablePtr ( HaskellObj );
HsBool       rts_getBool      ( HaskellObj );

/* DEPRECATED (use rts_getPtr): */
HsAddr       rts_getAddr      ( HaskellObj );

/* ----------------------------------------------------------------------------
   Evaluating Haskell expressions

   The versions ending in '_' allow you to specify an initial stack size.
   ------------------------------------------------------------------------- */
SchedulerStatus 
rts_eval ( HaskellObj p, /*out*/HaskellObj *ret );

SchedulerStatus 
rts_eval_ ( HaskellObj p, unsigned int stack_size, /*out*/HaskellObj *ret );

SchedulerStatus 
rts_evalIO ( HaskellObj p, /*out*/HaskellObj *ret );

SchedulerStatus 
rts_evalLazyIO ( HaskellObj p, unsigned int stack_size, /*out*/HaskellObj *ret );

void
rts_checkSchedStatus ( char* site, SchedulerStatus rc);

#endif /* RTSAPI_H */
