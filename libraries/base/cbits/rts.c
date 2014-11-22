#include "Rts.h"
#include "rts/Flags.h"

GC_FLAGS *getGcFlags()
{
    return &RtsFlags.GcFlags;
}

CONCURRENT_FLAGS *getConcFlags()
{
    return &RtsFlags.ConcFlags;
}

MISC_FLAGS *getMiscFlags()
{
    return &RtsFlags.MiscFlags;
}

DEBUG_FLAGS *getDebugFlags()
{
    return &RtsFlags.DebugFlags;
}

COST_CENTRE_FLAGS *getCcFlags()
{
    return &RtsFlags.CcFlags;
}

PROFILING_FLAGS *getProfFlags()
{
    return &RtsFlags.ProfFlags;
}

TRACE_FLAGS *getTraceFlags()
{
    return &RtsFlags.TraceFlags;
}

TICKY_FLAGS *getTickyFlags()
{
    return &RtsFlags.TickyFlags;
}
