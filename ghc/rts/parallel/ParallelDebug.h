/* 
   Time-stamp: <Fri Jan 14 2000 13:47:43 Stardate: [-30]4202.87 hwloidl>

   Prototypes of all parallel debugging functions.
   */

#ifndef PARALLEL_DEBUG_H
#define PARALLEL_DEBUG_H

#if defined(GRAN) // || defined(PAR)
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

#endif /* GRAN || PAR */

#if !defined(GRAN) && !defined(PAR)

void   info_hdr_type(StgClosure *closure, char *res);
char  *info_type(StgClosure *closure);
char  *info_type_by_ip(StgInfoTable *ip);

#endif

#endif /* PARALLEL_DEBUG_H */
