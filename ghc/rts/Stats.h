/* -----------------------------------------------------------------------------
 * $Id: Stats.h,v 1.2 1998/12/02 13:28:50 simonm Exp $
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
extern void      stat_endGC(lnat alloc, lnat collect, lnat live, 
			    char *comment);
extern void      initStats(void);
