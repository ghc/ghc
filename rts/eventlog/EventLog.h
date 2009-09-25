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

BEGIN_RTS_PRIVATE

#ifdef TRACING

/*
 * Descriptions of EventTags for events.
 */
extern char *EventTagDesc[];

void initEventLogging(void);
void endEventLogging(void);
void freeEventLogging(void);

/* 
 * Post an event to the capability's event buffer.
 */
void postSchedEvent(Capability *cap, EventTypeNum tag, 
                    StgThreadID id, StgWord64 other);

void postMsg(char *msg, va_list ap);

void postUserMsg(Capability *cap, char *msg);

void postCapMsg(Capability *cap, char *msg, va_list ap);

#else /* !TRACING */

INLINE_HEADER void postSchedEvent (Capability *cap  STG_UNUSED,
                                   EventTypeNum tag STG_UNUSED,
                                   StgThreadID id   STG_UNUSED,
                                   StgWord64 other  STG_UNUSED)
{ /* nothing */ }

INLINE_HEADER void postMsg (char *msg STG_UNUSED, 
                            va_list ap STG_UNUSED)
{ /* nothing */ }

INLINE_HEADER void postCapMsg (Capability *cap,
                               char *msg STG_UNUSED, 
                               va_list ap STG_UNUSED)
{ /* nothing */ }

#endif

END_RTS_PRIVATE

#endif /* TRACING_H */
