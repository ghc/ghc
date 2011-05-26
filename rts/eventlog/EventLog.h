/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2008-2009
 *
 * Support for fast binary event logging.
 *
 * ---------------------------------------------------------------------------*/

#ifndef EVENTLOG_H
#define EVENTLOG_H

#include "rts/EventLogFormat.h"
#include "Capability.h"

#include "BeginPrivate.h"

#ifdef TRACING

/*
 * Descriptions of EventTags for events.
 */
extern char *EventTagDesc[];

void initEventLogging(void);
void endEventLogging(void);
void freeEventLogging(void);
void abortEventLogging(void); // #4512 - after fork child needs to abort
void flushEventLog(void);     // event log inherited from parent

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

void postMsg(char *msg, va_list ap);

void postUserMsg(Capability *cap, char *msg, va_list ap);

void postCapMsg(Capability *cap, char *msg, va_list ap);

void postEventStartup(EventCapNo n_caps);

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

#endif

#include "EndPrivate.h"

#endif /* TRACING_H */
