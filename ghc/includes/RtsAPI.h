/* ----------------------------------------------------------------------------
 * $Id: RtsAPI.h,v 1.7 1999/07/06 09:42:39 sof Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * API for invoking Haskell functions via the RTS
 *
 * --------------------------------------------------------------------------*/

#ifndef RTSAPI_H
#define RTSAPI_H

/*
 * Running the scheduler
 */
typedef enum {
    Success,      
    Killed,	 /* another thread killed us                           */
    Interrupted, /* stopped in response to a call to interruptStgRts   */
    Deadlock,   
    AllBlocked,  /* subtly different from Deadlock                     */
} SchedulerStatus;
      
typedef StgClosure *HaskellObj;

/* ----------------------------------------------------------------------------
   Starting up and shutting down the Haskell RTS.
   ------------------------------------------------------------------------- */
extern void startupHaskell  ( int argc, char *argv[] );
extern void shutdownHaskell ( void );
extern void shutdownHaskellAndExit ( int exitCode );

/* ----------------------------------------------------------------------------
   Building Haskell objects from C datatypes.
   ------------------------------------------------------------------------- */
HaskellObj   rts_mkChar       ( char c );
HaskellObj   rts_mkInt        ( int i );
HaskellObj   rts_mkInt8       ( int i );
HaskellObj   rts_mkInt16      ( int i );
HaskellObj   rts_mkInt32      ( int i );
HaskellObj   rts_mkInt64      ( long long i );
HaskellObj   rts_mkWord       ( unsigned int w );
HaskellObj   rts_mkWord8      ( unsigned int w );
HaskellObj   rts_mkWord16     ( unsigned int w );
HaskellObj   rts_mkWord32     ( unsigned int w );
HaskellObj   rts_mkWord64     ( unsigned long long w );
HaskellObj   rts_mkFloat      ( float f );
HaskellObj   rts_mkDouble     ( double f );
HaskellObj   rts_mkStablePtr  ( StgStablePtr s );
HaskellObj   rts_mkAddr       ( void *a );
HaskellObj   rts_mkBool       ( int b );
HaskellObj   rts_mkString     ( char *s );

HaskellObj   rts_apply        ( HaskellObj, HaskellObj );

/* ----------------------------------------------------------------------------
   Deconstructing Haskell objects
   ------------------------------------------------------------------------- */
char         rts_getChar      ( HaskellObj );
int          rts_getInt       ( HaskellObj );
int          rts_getInt32     ( HaskellObj );
unsigned int rts_getWord      ( HaskellObj );
unsigned int rts_getWord32    ( HaskellObj );
float        rts_getFloat     ( HaskellObj );
double       rts_getDouble    ( HaskellObj );
StgStablePtr rts_getStablePtr ( HaskellObj );
void *       rts_getAddr      ( HaskellObj );
int          rts_getBool      ( HaskellObj );

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
