/* -----------------------------------------------------------------------------
 * $Id: Sparks.h,v 1.1 2000/01/12 15:15:18 simonmar Exp $
 *
 * (c) The GHC Team, 2000
 *
 * Sparking support for PAR and SMP versions of the RTS.
 *
 * ---------------------------------------------------------------------------*/

void         initSparkPools( void );
void         markSparkQueue( void );
StgClosure * findSpark( void );
rtsBool      add_to_spark_queue( StgClosure *closure, StgSparkPool *pool );
void         markSparkQueue( void );
