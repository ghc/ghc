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

#include "BeginPrivate.h"

#if defined(TRACING)

/*
 * Descriptions of EventTags for events.
 */
extern char *EventTagDesc[];

void initEventLogging(const EventLogWriter *writer);
void endEventLogging(void);
void freeEventLogging(void);
void abortEventLogging(void); // #4512 - after fork child needs to abort
void flushEventLog(void);     // event log inherited from parent
void moreCapEventBufs (uint32_t from, uint32_t to);

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

void postEventAtTimestamp (Capability *cap, EventTimestamp ts,
                           EventTypeNum tag);

void postMsg(char *msg, va_list ap);

void postUserEvent(Capability *cap, EventTypeNum type, char *msg);

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
                     char          *label);

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

void postTaskCreateEvent (EventTaskId taskId,
                          EventCapNo cap,
                          EventKernelThreadId tid);

void postTaskMigrateEvent (EventTaskId taskId,
                           EventCapNo capno,
                           EventCapNo new_capno);

void postTaskDeleteEvent (EventTaskId taskId);

void postHeapProfBegin(StgWord8 profile_id);

void postHeapProfSampleBegin(StgInt era);

void postHeapProfSampleString(StgWord8 profile_id,
                              const char *label,
                              StgWord64 residency);

#if defined(PROFILING)
void postHeapProfCostCentre(StgWord32 ccID,
                            const char *label,
                            const char *module,
                            const char *srcloc,
                            StgBool is_caf);

void postHeapProfSampleCostCentre(StgWord8 profile_id,
                                  CostCentreStack *stack,
                                  StgWord64 residency);
#endif /* PROFILING */

#else /* !TRACING */

INLINE_HEADER void postSchedEvent (Capability *cap  STG_UNUSED,
                                   EventTypeNum tag STG_UNUSED,
                                   StgThreadID id   STG_UNUSED,
                                   StgWord info1    STG_UNUSED,
                                   StgWord info2    STG_UNUSED)
{ /* nothing */ }

INLINE_HEADER void postEvent (Capability *cap  STG_UNUSED,
                              EventTypeNum tag STG_UNUSED)
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
