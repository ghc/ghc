/* -----------------------------------------------------------------------------
 * $Id: Stats.h,v 1.10 2000/12/19 14:30:39 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Statistics and timing-related functions.
 *
 * ---------------------------------------------------------------------------*/

extern void      stat_startInit(void);
extern void      stat_endInit(void);

extern void      stat_startGC(void);
extern void      stat_endGC(lnat alloc, lnat collect, lnat live, 
			    lnat copied, lnat gen);

extern void      stat_startExit(void);
extern void      stat_endExit(void);

extern void      stat_exit(int alloc);
extern void      stat_workerStop(void);

extern void      initStats(void);

extern void      stat_describe_gens(void);
extern double    mut_user_time_during_GC(void);
extern double    mut_user_time(void);

extern HsInt     getAllocations( void );
