/* ----------------------------------------------------------------------------
 * $Id: RtsAPI.h,v 1.14 2000/06/15 13:16:16 daan Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * API for invoking Haskell functions via the RTS
 *
 * --------------------------------------------------------------------------*/

#ifndef RTSAPI_H
#define RTSAPI_H

/* Make this compilable with Visual C++ */
#ifndef HAVE_INT64
#define HAVE_INT64
#ifdef _MSC_VER 
typedef __int64            int64;
typedef unsigned __int64   nat64;
#else
typedef long long          int64;
typedef unsigned long long nat64;
#endif
#endif

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
extern void startupHaskell         ( int argc, char *argv[], void *init_root );
extern void shutdownHaskell        ( void );
extern void shutdownHaskellAndExit ( int exitCode );
extern void setProgArgv            ( int argc, char *argv[] );
extern void getProgArgv            ( int *argc, char **argv[] );

/* ----------------------------------------------------------------------------
   Building Haskell objects from C datatypes.
   ------------------------------------------------------------------------- */
HaskellObj   rts_mkChar       ( char c );
HaskellObj   rts_mkInt        ( int i );
HaskellObj   rts_mkInt8       ( int i );
HaskellObj   rts_mkInt16      ( int i );
HaskellObj   rts_mkInt32      ( int i );
HaskellObj   rts_mkInt64      ( int64 i );
HaskellObj   rts_mkWord       ( unsigned int w );
HaskellObj   rts_mkWord8      ( unsigned int w );
HaskellObj   rts_mkWord16     ( unsigned int w );
HaskellObj   rts_mkWord32     ( unsigned int w );
HaskellObj   rts_mkWord64     ( nat64 w );
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

#if defined(PAR) || defined(SMP)
SchedulerStatus
rts_evalNothing(unsigned int stack_size);
#endif

void
rts_checkSchedStatus ( char* site, SchedulerStatus rc);

#endif /* RTSAPI_H */
