/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2008-2009
 *
 * Support for fast binary event logging.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "rts/EventLogFormat.h"
#include "rts/EventLogWriter.h"
#include "Capability.h"
#include "sm/NonMovingCensus.h"

#include "BeginPrivate.h"

#if defined(TRACING)

extern bool eventlog_enabled;

void initEventLogging(void);
void restartEventLogging(void);
void finishCapEventLogging(void);
void freeEventLogging(void);
void abortEventLogging(void); // #4512 - after fork child needs to abort
void moreCapEventBufs (uint32_t from, uint32_t to);
void flushLocalEventsBuf(Capability *cap);
void flushAllCapsEventsBufs(void);
void flushAllEventsBufs(Capability *cap);

typedef void (*EventlogInitPost)(void);

// Events which are emitted during program start-up should be wrapped with
// postInitEvent so that when the eventlog is restarted (possibly by an external
// writer) then these events appear again at the start of the log.
void postInitEvent(EventlogInitPost post_init);

// Clear the init events buffer on program exit
void resetInitEvents(void);

typedef struct eventlog_init_func {
    EventlogInitPost init_func;
    struct eventlog_init_func * next;
} eventlog_init_func_t;


/*
 * Post a scheduler event to the capability's event buffer (an event
 * that has an associated thread).
 */
void postSchedEvent(Capability *cap, EventTypeNum tag,
                    StgThreadID id, StgWord info1, StgWord info2);

/*
 * Post a nullary event.
 */
void postEvent(Capability *cap, EventTypeNum tag);
void postEventNoCap(EventTypeNum tag);

void postEventAtTimestamp (Capability *cap, EventTimestamp ts,
                           EventTypeNum tag);

void postMsg(char *msg, va_list ap);

void postUserEvent(Capability *cap, EventTypeNum type, char *msg);

void postUserBinaryEvent(Capability *cap, EventTypeNum type,
                         uint8_t *msg, size_t size);

void postCapMsg(Capability *cap, char *msg, va_list ap);

/*
 * Post an event relating to a capability itself (create/delete/etc)
 */
void postCapEvent (EventTypeNum  tag,
                   EventCapNo    capno);

/*
 * Post an event that is associated with a capability set
 */
void postCapsetEvent (EventTypeNum tag,
                      EventCapsetID capset,
                      StgWord info);

/*
 * Post a capability set event with a string payload
 */
void postCapsetStrEvent (EventTypeNum tag,
                         EventCapsetID capset,
                         char *msg);

/*
 * Post a capability set event with several strings payload
 */
void postCapsetVecEvent (EventTypeNum tag,
                         EventCapsetID capset,
                         int argc,
                         char *msg[]);

void postWallClockTime (EventCapsetID capset);

/*
 * Post a `par` spark event
 */
void postSparkEvent(Capability *cap, EventTypeNum tag, StgWord info1);

/*
 * Post an event with several counters relating to `par` sparks.
 */
void postSparkCountersEvent (Capability *cap,
                             SparkCounters counters,
                             StgWord remaining);

/*
 * Post an event to annotate a thread with a label
 */
void postThreadLabel(Capability    *cap,
                     EventThreadID  id,
                     char          *label,
                     size_t         len);

/*
 * Various GC and heap events
 */
void postHeapEvent (Capability    *cap,
                    EventTypeNum   tag,
                    EventCapsetID  heap_capset,
                    W_           info1);

void postEventHeapInfo (EventCapsetID heap_capset,
                        uint32_t    gens,
                        W_          maxHeapSize,
                        W_          allocAreaSize,
                        W_          mblockSize,
                        W_          blockSize);

void postEventGcStats  (Capability    *cap,
                        EventCapsetID  heap_capset,
                        uint32_t     gen,
                        W_           copied,
                        W_           slop,
                        W_           fragmentation,
                        uint32_t     par_n_threads,
                        W_           par_max_copied,
                        W_           par_tot_copied,
                        W_           par_balanced_copied);

void postEventMemReturn (Capability *cap,
                        EventCapsetID  heap_capset,
                         uint32_t current_mblocks,
                         uint32_t needed_mblocks,
                         uint32_t returned_mblocks
                        );

void postTaskCreateEvent (EventTaskId taskId,
                          EventCapNo cap,
                          EventKernelThreadId tid);

void postTaskMigrateEvent (EventTaskId taskId,
                           EventCapNo capno,
                           EventCapNo new_capno);

void postTaskDeleteEvent (EventTaskId taskId);

void postHeapProfBegin(void);

void postHeapProfSampleBegin(StgInt era);
void postHeapBioProfSampleBegin(StgInt era, StgWord64 time_ns);
void postHeapProfSampleEnd(StgInt era);

void postHeapProfSampleString(const char *label,
                              StgWord64 residency);

#if defined(PROFILING)
void postHeapProfCostCentre(StgWord32 ccID,
                            const char *label,
                            const char *module,
                            const char *srcloc,
                            StgBool is_caf);

void postHeapProfSampleCostCentre(CostCentreStack *stack,
                                  StgWord64 residency);

void postProfSampleCostCentre(Capability *cap,
                              CostCentreStack *stack,
                              StgWord64 ticks);
void postProfBegin(void);
#endif /* PROFILING */

void postIPE(const InfoProvEnt *ipe);

void postConcUpdRemSetFlush(Capability *cap);
void postConcMarkEnd(StgWord32 marked_obj_count);
void postNonmovingHeapCensus(uint16_t blk_size,
                             const struct NonmovingAllocCensus *census);
void postNonmovingPrunedSegments(uint32_t pruned_segments, uint32_t free_segments);

#if defined(TICKY_TICKY)
void postTickyCounterDefs(StgEntCounter *p);
void postTickyCounterSamples(StgEntCounter *p);
#endif /* TICKY_TICKY */

#else /* !TRACING */

INLINE_HEADER void finishCapEventLogging(void) {}

INLINE_HEADER void flushLocalEventsBuf(Capability *cap STG_UNUSED)
{ /* nothing */ }

INLINE_HEADER void postSchedEvent (Capability *cap  STG_UNUSED,
                                   EventTypeNum tag STG_UNUSED,
                                   StgThreadID id   STG_UNUSED,
                                   StgWord info1    STG_UNUSED,
                                   StgWord info2    STG_UNUSED)
{ /* nothing */ }

INLINE_HEADER void postEvent (Capability *cap  STG_UNUSED,
                              EventTypeNum tag STG_UNUSED)
{ /* nothing */ }

typedef void (*EventlogInitPost)(void);

INLINE_HEADER void postInitEvent(EventlogInitPost f STG_UNUSED)
{ /* nothing */ } ;



INLINE_HEADER void postEventNoCap (EventTypeNum tag STG_UNUSED)
{ /* nothing */ }

INLINE_HEADER void postMsg (char *msg STG_UNUSED,
                            va_list ap STG_UNUSED)
{ /* nothing */ }

INLINE_HEADER void postCapMsg (Capability *cap STG_UNUSED,
                               char *msg STG_UNUSED,
                               va_list ap STG_UNUSED)
{ /* nothing */ }


INLINE_HEADER void postThreadLabel(Capability    *cap   STG_UNUSED,
                                   EventThreadID  id    STG_UNUSED,
                                   char          *label STG_UNUSED)
{ /* nothing */ }

#endif

#include "EndPrivate.h"
