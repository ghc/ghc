/* 
   Time-stamp: <Tue Mar 06 2001 00:25:14 Stardate: [-30]6285.08 hwloidl>

   Prototypes of all parallel debugging functions.
*/

#ifndef PARALLEL_DEBUG_H
#define PARALLEL_DEBUG_H

#if defined(DEBUG) && (defined(GRAN) || defined(PAR))
/* max length of the string holding a finger-print for a graph */
#define MAX_FINGER_PRINT_LEN  10000
// (10*RtsFlags.ParFlags.packBufferSize)
#endif

#if defined(DEBUG) && defined(GRAN)
void G_PRINT_NODE(StgClosure* node);
void G_PPN(StgClosure* node);
void G_INFO_TABLE(StgClosure* node);
void G_CURR_THREADQ(StgInt verbose);
void G_THREADQ(StgTSO* closure, StgInt verbose);
void G_TSO(StgTSO* closure, StgInt verbose);
void G_EVENT(rtsEventQ event, StgInt verbose);
void G_EVENTQ(StgInt verbose);
void G_PE_EQ(PEs pe, StgInt verbose);
void G_SPARK(rtsSparkQ spark, StgInt verbose);
void G_SPARKQ(rtsSparkQ spark, StgInt verbose);
void G_CURR_SPARKQ(StgInt verbose);
void G_PROC(StgInt proc, StgInt verbose);
void GP(StgInt proc);
void GCP(void);
void GT(StgPtr tso);
void GCT(void);
void GEQ(void);
void GTQ(PEs p);
void GCTQ(void);
void GSQ(PEs p);
void GCSQ(void);
void GN(StgPtr node);
void GIT(StgPtr node);
#endif

#if defined(GRAN) || defined(PAR)

char  *display_info_type(StgClosure *closure, char *str);
void   info_hdr_type(StgClosure *closure, char *res);
char  *info_type(StgClosure *closure);
char  *info_type_by_ip(StgInfoTable *ip);

void   PrintPacket(rtsPackBuffer *buffer);
void   PrintGraph(StgClosure *p, int indent_level);
void   GraphFingerPrint(StgClosure *p, char *finger_print);
void   checkGraph(StgClosure *p, int rec_level);

void   checkPacket(rtsPackBuffer *packBuffer);

#endif /* GRAN || PAR */

#if defined(PAR)

/* don't want to import Schedule.h and Sanity.h everywhere */
extern void print_bq (StgClosure *node);
extern void checkBQ (StgBlockingQueueElement *bqe, StgClosure *closure);

void   checkGAGAMap(globalAddr *gagamap, int nGAs);
extern rtsBool isOnLiveIndTable(globalAddr *ga);
extern void rebuildGAtables(rtsBool full);
extern void rebuildLAGAtable(void);
extern void checkLAGAtable(rtsBool check_closures);
extern void checkHeapChunk(StgPtr start, StgPtr end);
extern void printGA (globalAddr *ga);
extern void printGALA (GALA *gala);
extern void printLiveIndTable(void);
extern void printRemoteGATable(void);
extern void printLAGAtable(void);

#endif

#endif /* PARALLEL_DEBUG_H */
