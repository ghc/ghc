/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000
 *
 * Sparking support for GRAN, PAR and SMP versions of the RTS.
 * 
 * ---------------------------------------------------------------------------*/

#ifndef SPARKS_H
#define SPARKS_H

#if defined(GRAN)

void      findLocalSpark (rtsEvent *event, rtsBool *found_res, rtsSparkQ *spark_res);
rtsBool   activateSpark (rtsEvent *event, rtsSparkQ spark);
rtsSpark *newSpark(StgClosure *node, nat name, nat gran_info, 
		   nat size_info, nat par_info, nat local);
void      add_to_spark_queue(rtsSpark *spark);
rtsSpark *delete_from_sparkq (rtsSpark *spark, PEs p, rtsBool dispose_too);
void 	  disposeSpark(rtsSpark *spark);
void 	  disposeSparkQ(rtsSparkQ spark);
void 	  print_spark(rtsSpark *spark);
void      print_sparkq(PEs proc);
void 	  print_sparkq_stats(void);
nat  	  spark_queue_len(PEs proc);
void      markSparkQueue(void);

#elif defined(PAR) || defined(SMP)

StgClosure  *findSpark( rtsBool );
void         initSparkPools( void );
void         markSparkQueue( void );
#if defined(PAR)
StgTSO      *activateSpark (rtsSpark spark) ;
rtsBool      add_to_spark_queue( StgClosure *closure, StgSparkPool *pool );
void         markSparkQueue( void );
nat          spark_queue_len( StgSparkPool *pool );
void         disposeSpark( StgClosure *spark );
#endif

#endif

#endif /* SPARKS_H */
