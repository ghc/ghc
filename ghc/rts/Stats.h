/* -----------------------------------------------------------------------------
 * $Id: Stats.h,v 1.12 2001/11/22 14:25:12 simonmar Exp $
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

extern void      stat_startRP(void);
extern void      stat_endRP(nat, 
#ifdef DEBUG_RETAINER
                            nat, int, 
#endif
                            double, nat, nat);

extern void      stat_startLDV(void);
extern void      stat_endLDV(void);

extern void      stat_startExit(void);
extern void      stat_endExit(void);

extern void      stat_exit(int alloc);
extern void      stat_workerStop(void);

extern void      initStats(void);

extern double    mut_user_time_during_GC(void);
#ifdef PROFILING
// @retainer profiling
extern double    mut_user_time_during_RP(void);
extern double    mut_user_time_during_LDV(void);
#endif
extern double    mut_user_time(void);

extern void      statDescribeGens( void );
extern HsInt     getAllocations( void );
