/* --------------------------------------------------------------------------
 *
 * (c) Hans-Wolfgang Loidl, 2000-
 *
 * Header for ParTicky.c
 *
 * --------------------------------------------------------------------------*/

#if defined(PAR_TICKY)

/* macros */
#define PAR_TICKY_PAR_START()              par_ticky_Par_start () 
#define PAR_TICKY_PAR_END()                globalParStat_exit () 
#define PAR_TICKY_REBUILD_GA_TABLES_START()  par_ticky_rebuildGAtables_start() 
#define PAR_TICKY_REBUILD_GA_TABLES_END(n, size_GA) par_ticky_rebuildGAtables_end(n, size_GA) 
#define PAR_TICKY_MARK_LOCAL_GAS_START()     par_ticky_markLocalGAs_start() 
#define PAR_TICKY_MARK_LOCAL_GAS_END(n)      par_ticky_markLocalGAs_end(n) 
#define PAR_TICKY_MARK_SPARK_QUEUE_START()   par_ticky_markSparkQueue_start() 
#define PAR_TICKY_MARK_SPARK_QUEUE_END(n)    par_ticky_markSparkQueue_end(n) 
#define PAR_TICKY_PACK_NEARBY_GRAPH_START()  (par_ticky_PackNearbyGraph_start())
#define PAR_TICKY_PACK_NEARBY_GRAPH_END(n, thunks) par_ticky_PackNearbyGraph_end(n, thunks) 
#define PAR_TICKY_UNPACK_GRAPH_START()      par_ticky_UnpackGraph_start() 
#define PAR_TICKY_UNPACK_GRAPH_END(n,thunks) par_ticky_UnpackGraph_end(n,thunks)
#define PAR_TICKY_TP()                     par_ticky_TP() 
#define PAR_TICKY_CNT_FREE_GA()            stats_CntFreeGA()

/* prototypes */
extern void par_ticky_Par_start (void) ;
extern void par_ticky_rebuildGAtables_start(void) ;
extern void par_ticky_rebuildGAtables_end(nat n, nat size_GA) ;
extern void par_ticky_markLocalGAs_start(void) ;
extern void par_ticky_markLocalGAs_end(nat n) ;
extern void par_ticky_markSparkQueue_start(void) ;
extern void par_ticky_markSparkQueue_end(nat n) ;
extern void par_ticky_PackNearbyGraph_start (void) ;
extern void par_ticky_PackNearbyGraph_end(nat n, nat thunks) ;
extern void par_ticky_UnpackGraph_start (void) ;
extern void par_ticky_UnpackGraph_end(nat n, nat thunks) ;
extern void par_ticky_TP (void) ;
extern void globalParStat_exit(void);

#else

#define PAR_TICKY_PAR_START()
#define PAR_TICKY_PAR_END()  
#define PAR_TICKY_REBUILD_GA_TABLES_START()
#define PAR_TICKY_REBUILD_GA_TABLES_END(n, size_GA)
#define PAR_TICKY_MARK_LOCAL_GAS_START()
#define PAR_TICKY_MARK_LOCAL_GAS_END(n) 
#define PAR_TICKY_MARK_SPARK_QUEUE_START()
#define PAR_TICKY_MARK_SPARK_QUEUE_END(n) 
#define PAR_TICKY_PACK_NEARBY_GRAPH_START () 
#define PAR_TICKY_PACK_NEARBY_GRAPH_END(n, thunks)
#define PAR_TICKY_UNPACK_GRAPH_START ()    
#define PAR_TICKY_UNPACK_GRAPH_END(n, thunks) 
#define PAR_TICKY_TP ()                    
#define PAR_TICKY_CNT_FREE_GA()            

#endif

