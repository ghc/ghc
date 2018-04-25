/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2017
 *
 * Generating cost-center profiler report
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include <stdio.h>

#include "Rts.h"
#include "Profiling.h"

#include "BeginPrivate.h"

#if defined(PROFILING)

void writeCCSReport( FILE *prof_file, CostCentreStack const *ccs,
                     ProfilerTotals totals );

#endif

#include "EndPrivate.h"
