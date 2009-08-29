/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2008-2009
 *
 * Support for fast binary event logging.
 *
 * ---------------------------------------------------------------------------*/

#ifndef TRACE_H
#define TRACE_H

#include "rts/EventLogFormat.h"
#include "Capability.h"

#pragma GCC visibility push(hidden)

// -----------------------------------------------------------------------------
// Posting events
// -----------------------------------------------------------------------------

INLINE_HEADER void trace (StgWord32 class, char *msg, ...);

#ifdef DEBUG
INLINE_HEADER void debugTrace (StgWord32 class, char *msg, ...);
#endif

INLINE_HEADER void traceSchedEvent (Capability *cap, EventTypeNum tag, 
                                    StgTSO *tso, StgWord64 other);

INLINE_HEADER void traceCap (StgWord32 class, Capability *cap,
                             char *msg, ...);

INLINE_HEADER void traceThreadStatus (StgWord32 class, StgTSO *tso);

INLINE_HEADER rtsBool traceClass (StgWord32 class);

#ifdef DEBUG
void traceBegin (const char *str, ...);
void traceEnd (void);
#endif

// -----------------------------------------------------------------------------
// EventLog API
// -----------------------------------------------------------------------------

#if defined(TRACING)

void initTracing (void);
void endTracing  (void);
void freeTracing (void);

#endif /* TRACING */

// -----------------------------------------------------------------------------
// Message classes, these may be OR-ed together
// -----------------------------------------------------------------------------

// debugging flags, set with +RTS -D<something>
#define DEBUG_sched		   (1<<0)
#define DEBUG_interp		   (1<<1)
#define DEBUG_weak		   (1<<2)
#define DEBUG_gccafs		   (1<<3) 
#define DEBUG_gc		   (1<<4) 
#define DEBUG_block_alloc	   (1<<5) 
#define DEBUG_sanity		   (1<<6) 
#define DEBUG_stable		   (1<<7) 
#define DEBUG_stm   		   (1<<8) 
#define DEBUG_prof		   (1<<9) 
#define DEBUG_gran		   (1<<10)
#define DEBUG_par		   (1<<11)
#define DEBUG_linker		   (1<<12)
#define DEBUG_squeeze              (1<<13)
#define DEBUG_hpc                  (1<<14)
#define DEBUG_sparks		   (1<<15)

// events
#define TRACE_sched                (1<<16)

// -----------------------------------------------------------------------------
// PRIVATE below here
// -----------------------------------------------------------------------------

#ifdef TRACING

extern StgWord32 classes_enabled;

INLINE_HEADER rtsBool traceClass (StgWord32 class) 
{ return (classes_enabled & class); }

void traceSchedEvent_ (Capability *cap, EventTypeNum tag, 
                       StgTSO *tso, StgWord64 other);

/* 
 * Trace an event to the capability's event buffer.
 */
INLINE_HEADER void traceSchedEvent(Capability *cap, EventTypeNum tag, 
                                   StgTSO *tso, StgWord64 other)
{
    if (traceClass(TRACE_sched)) {
        traceSchedEvent_(cap, tag, tso, other);
    }
}

void traceCap_(Capability *cap, char *msg, va_list ap);

/* 
 * Trace a log message
 */
INLINE_HEADER void traceCap (StgWord32 class, Capability *cap, char *msg, ...)
{
    va_list ap;
    va_start(ap,msg);
    if (traceClass(class)) {
        traceCap_(cap, msg, ap);
    }
    va_end(ap);
}

void trace_(char *msg, va_list ap);

/* 
 * Trace a log message
 */
INLINE_HEADER void trace (StgWord32 class, char *msg, ...)
{
    va_list ap;
    va_start(ap,msg);
    if (traceClass(class)) {
        trace_(msg, ap);
    }
    va_end(ap);
}

#ifdef DEBUG
INLINE_HEADER void debugTrace (StgWord32 class, char *msg, ...)
{
    va_list ap;
    va_start(ap,msg);
    if (traceClass(class)) {
        trace_(msg, ap);
    }
    va_end(ap);
}
#else

#define debugTrace(class, str, ...) /* nothing */
// variable arg macros are C99, and supported by gcc.

#endif

void traceThreadStatus_ (StgTSO *tso);

INLINE_HEADER void traceThreadStatus (StgWord32 class, StgTSO *tso)
{
    if (traceClass(class)) {
        traceThreadStatus_(tso);
    }
}    

#else /* !TRACING */

INLINE_HEADER rtsBool traceClass (StgWord32 class STG_UNUSED) 
{ return rtsFalse; }

INLINE_HEADER void traceSchedEvent (Capability *cap STG_UNUSED,
                                    EventTypeNum tag STG_UNUSED, 
                                    StgTSO *tso STG_UNUSED,
                                    StgWord64 other STG_UNUSED)
{ /* nothing */ }

INLINE_HEADER void traceCap (StgWord32 class STG_UNUSED,
                             Capability *cap STG_UNUSED,
                             char *msg STG_UNUSED, ...)
{ /* nothing */ }

INLINE_HEADER void trace (StgWord32 class STG_UNUSED, 
                          char *msg STG_UNUSED, ...)
{ /* nothing */ }

#define debugTrace(class, str, ...) /* nothing */
// variable arg macros are C99, and supported by gcc.

INLINE_HEADER void traceThreadStatus (StgWord32 class STG_UNUSED, 
                                      StgTSO *tso STG_UNUSED)
{ /* nothing */ }

#endif /* TRACING */

#pragma GCC visibility pop

#endif /* TRACE_H */
