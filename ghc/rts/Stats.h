/* -----------------------------------------------------------------------------
 * $Id: Stats.h,v 1.5 1999/02/23 15:45:08 simonm Exp $
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
extern void      stat_startGC(void);
extern void      stat_endGC(lnat alloc, lnat collect, lnat live, 
			    lnat copied, lnat gen);
extern void      initStats(void);
extern void      stat_describe_gens(void);
