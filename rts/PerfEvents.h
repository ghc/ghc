/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2008-2009
 *
 * Support for profiling with the Linux perf_events interface
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(HAVE_PERF_EVENT)

#include "Rts.h"
#include <linux/perf_event.h>

/* A perf_events fd */
typedef struct {
    int fd;
} PerfEvent;

PerfEvent *
setup_event(enum perf_hw_id counter, StgWord64 sample_period);

void
disable_event(PerfEvent *ev);

#endif
