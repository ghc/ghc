/* -----------------------------------------------------------------------------
 * $Id: Stats.h,v 1.8 1999/11/09 15:46:58 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Statistics and timing-related functions.
 *
 * ---------------------------------------------------------------------------*/

extern StgDouble elapsedtime(void);
extern void      start_time(void);
extern StgDouble usertime(void);
extern void      end_init(void);
extern void      stat_exit(int alloc);
extern void      stat_workerStop(void);

extern void      stat_startGC(void);
extern void      stat_endGC(lnat alloc, lnat collect, lnat live, 
			    lnat copied, lnat gen);

extern void      stat_startExit(void);
extern void      stat_endExit(void);

extern void      initStats(void);
extern void      stat_describe_gens(void);
extern double    mut_user_time_during_GC(void);
extern double    mut_user_time(void);
