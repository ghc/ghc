/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2017
 *
 * Generating cost-center profiler report
 *
 * ---------------------------------------------------------------------------*/

#ifndef PROFILER_REPORT_JSON_H
#define PROFILER_REPORT_JSON_H

#include <stdio.h>

#include "Rts.h"
#include "Profiling.h"

#include "BeginPrivate.h"

#ifdef PROFILING

void writeCCSReportJson(FILE *prof_file,
                        CostCentreStack const *ccs,
                        ProfilerTotals totals );

#endif

#include "EndPrivate.h"

#endif /* PROFILER_REPORT_JSON_H */
