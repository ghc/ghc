/* -----------------------------------------------------------------------------
 * $Id: Stats.h,v 1.3 1999/01/13 17:25:46 simonm Exp $
 *
 * Statistics and timing-related functions.
 *
 * ---------------------------------------------------------------------------*/

extern StgDouble elapsedtime(void);
extern void      start_time(void);
extern StgDouble usertime(void);
extern void      end_init(void);
extern void      stat_exit(int alloc);
extern void      stat_startGC(void);
extern void      stat_endGC(lnat alloc, lnat collect, lnat live, lnat gen);
extern void      initStats(void);
extern void      stat_describe_gens(void);
