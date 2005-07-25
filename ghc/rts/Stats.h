/* -----------------------------------------------------------------------------
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
			    lnat copied, lnat scavd_copied, lnat gen);

#ifdef PROFILING
extern void      stat_startRP(void);
extern void      stat_endRP(nat, 
#ifdef DEBUG_RETAINER
                            nat, int, 
#endif
                            double);
#endif /* PROFILING */

#if defined(PROFILING) || defined(DEBUG)
extern void      stat_startHeapCensus(void);
extern void      stat_endHeapCensus(void);
#endif

extern void      stat_startExit(void);
extern void      stat_endExit(void);

extern void      stat_exit(int alloc);
extern void      stat_workerStop(void);

extern void      initStats(void);

extern double    mut_user_time_during_GC(void);
extern double    mut_user_time(void);

#ifdef PROFILING
extern double    mut_user_time_during_RP(void);
extern double    mut_user_time_during_heap_census(void);
#endif /* PROFILING */

extern void      statDescribeGens( void );
extern HsInt64   getAllocations( void );

extern void      stat_getTimes ( long *currentElapsedTime, 
				 long *currentUserTime,
				 long *elapsedGCTime );
