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

#pragma GCC visibility push(hidden)

#ifdef EVENTLOG

/*
 * Descriptions of EventTags for events.
 */
extern char *EventTagDesc[];

void initEventLogging(void);
void endEventLogging(void);
void freeEventLogging(void);

void postEvent_(Capability *cap, EventTypeNum tag, StgThreadID id, StgWord64 other);

/* 
 * Post an event to the capability's event buffer.
 */
INLINE_HEADER void postEvent(Capability *cap, EventTypeNum tag, StgThreadID id, StgWord64 other)
{
    if (RtsFlags.EventLogFlags.doEventLogging) {
        postEvent_(cap, tag, id, other);
    }
}

void printAndClearEventLog(Capability *cap);

#else /* !EVENTLOG */

INLINE_HEADER void postEvent(Capability *cap  STG_UNUSED,
                             EventTypeNum tag STG_UNUSED,
                             StgThreadID id   STG_UNUSED,
                             StgWord64 other  STG_UNUSED)
{
    /* nothing */
}

#endif

#pragma GCC visibility pop

#endif /* EVENTLOG_H */
