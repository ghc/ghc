/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2003-2016
 *
 * Statistical profiling
 *
 * --------------------------------------------------------------------------*/

#pragma once

#include "Rts.h"
#include "Trace.h"

#include "BeginPrivate.h"

#if defined(STAT_PROFILE)

INLINE_HEADER void
statProfileDumpHeapSamples(Capability *cap)
{
    // See Note [Statistical profiling of heap allocations]
    if (cap->heap_sample_count) {
        traceStatProfileSamples(cap, true, SAMPLE_BY_HEAP_ALLOC,
                                SAMPLE_TYPE_INSTR_PTR,
                                cap->heap_sample_count,
                                cap->heap_samples, NULL);
        cap->heap_sample_count = 0;
    }
}

INLINE_HEADER void
statProfileDumpBlackholeSamples(Capability *cap)
{
    // See Note [Statistical profiling of black-hole allocations]
    if (cap->blackhole_sample_count) {
        traceStatProfileSamples(cap, true, SAMPLE_BY_BLACKHOLE,
                                SAMPLE_TYPE_INSTR_PTR,
                                cap->blackhole_sample_count,
                                cap->blackhole_samples, NULL);
        cap->blackhole_sample_count = 0;
    }
}

#else

INLINE_HEADER void
statProfileDumpHeapSamples(Capability *cap STG_UNUSED) { }

INLINE_HEADER void
statProfileDumpBlackholeSamples(Capability *cap STG_UNUSED) { }

#endif /* STAT_PROFILE */


#if defined(STAT_PROFILE) && defined(HAVE_PERF_EVENT)

INLINE_HEADER void
statProfileDumpPerfEventSamples(Capability *cap)
{
    // See Note [Statistical profiling of black-hole allocations]
    if (cap->perf_event_sample_count) {
        traceStatProfileSamples(cap, true, SAMPLE_BY_PERF_EVENT,
                                SAMPLE_TYPE_INSTR_PTR,
                                cap->perf_event_sample_count,
                                cap->perf_event_samples, NULL);
        cap->perf_event_sample_count = 0;
    }
}

#else

INLINE_HEADER void
statProfileDumpPerfEventSamples(Capability *cap STG_UNUSED) { }

#endif /* STAT_PROFILE && HAVE_PERF_EVENT */


INLINE_HEADER void
statProfileDumpSamples(Capability *cap)
{
    statProfileDumpHeapSamples(cap);
    statProfileDumpBlackholeSamples(cap);
    statProfileDumpPerfEventSamples(cap);
}

#include "EndPrivate.h"
