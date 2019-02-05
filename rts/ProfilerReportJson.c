/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2017
 *
 * Generating cost-centre profiler JSON report
 *
 * ---------------------------------------------------------------------------*/

#if defined(PROFILING)

#include "PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "ProfilerReportJson.h"
#include "Profiling.h"

// This only handles characters that you might see in a Haskell cost-centre
// name.
static void escapeString(char const* str, char *out, int len)
{
    len--; // reserve character in output for terminating NUL
    for (; *str != '\0' && len > 0; str++) {
        char c = *str;
        if (c == '\\') {
            if (len < 2) break;
            *out = '\\'; out++; len--;
            *out = '\\'; out++; len--;
        } else if (c == '\n') {
            if (len < 2) break;
            *out = '\\'; out++; len--;
            *out = 'n';  out++; len--;
        } else {
            *out = c; out++; len--;
        }
    }
    *out = '\0';
}

static void
logCostCentres(FILE *prof_file)
{
    char tmp[256];
    bool needs_comma = false;
    fprintf(prof_file, "[\n");
    for (CostCentre *cc = CC_LIST; cc != NULL; cc = cc->link) {
        escapeString(cc->label, tmp, sizeof(tmp));
        fprintf(prof_file,
                "%s"
                "{\"id\": %" FMT_Int ", "
                "\"label\": \"%s\", "
                "\"module\": \"%s\", "
                "\"src_loc\": \"%s\", "
                "\"is_caf\": %s}",
                needs_comma ? ", " : "",
                cc->ccID, tmp, cc->module, cc->srcloc,
                cc->is_caf ? "true" : "false");
        needs_comma = true;
    }
    fprintf(prof_file, "]\n");
}

static void
logCostCentreStack(FILE *prof_file, CostCentreStack const *ccs)
{
    fprintf(prof_file,
            "{\"id\": %" FMT_Int ", "
            "\"entries\": %" FMT_Word64 ", "
            "\"alloc\": %" FMT_Word64 ", "
            "\"ticks\": %" FMT_Word ", ",
            ccs->cc->ccID,
            ccs->scc_count,
            ccs->mem_alloc * sizeof(W_),
            ccs->time_ticks);

    bool need_comma = false;
    fprintf(prof_file, "\"children\": [");
    for (IndexTable *i = ccs->indexTable; i != 0; i = i->next) {
        if (!i->back_edge) {
            if (need_comma) {
                fprintf(prof_file, ",");
            }
            logCostCentreStack(prof_file, i->ccs);
            need_comma = true;
        }
    }
    fprintf(prof_file, "]}\n");
}

void
writeCCSReportJson(FILE *prof_file,
                   CostCentreStack const *stack,
                   ProfilerTotals totals)
{
    fprintf(prof_file, "{\n\"program\": \"%s\",\n", prog_name);
    fprintf(prof_file, "\"arguments\": [");
    for (int count = 0; prog_argv[count]; count++)
        fprintf(prof_file, "%s\"%s\"",
                count == 0 ? "" : ", ", prog_argv[count]);
    fprintf(prof_file, "],\n\"rts_arguments\": [");
    for (int count = 0; rts_argv[count]; count++)
        fprintf(prof_file, "%s\"%s\"",
                count == 0 ? "" : ", ", rts_argv[count]);
    fprintf(prof_file, "],\n");

    fprintf(prof_file, "\"end_time\": \"%s\",\n", time_str());
    fprintf(prof_file, "\"initial_capabilities\": %d,\n",
            RtsFlags.ParFlags.nCapabilities);
    fprintf(prof_file, "\"total_time\": %11.2f,\n",
            ((double) totals.total_prof_ticks *
             (double) RtsFlags.MiscFlags.tickInterval) / (TIME_RESOLUTION * n_capabilities));
    fprintf(prof_file, "\"total_ticks\": %lu,\n",
            (unsigned long) totals.total_prof_ticks);
    fprintf(prof_file, "\"tick_interval\": %d,\n",
            (int) TimeToUS(RtsFlags.MiscFlags.tickInterval));
    fprintf(prof_file, "\"total_alloc\":%" FMT_Word64 ",\n",
            totals.total_alloc * sizeof(W_));

    fprintf(prof_file, "\"cost_centres\": ");
    logCostCentres(prof_file);
    fprintf(prof_file, ",\n\"profile\": ");
    logCostCentreStack(prof_file, stack);
    fprintf(prof_file, "}\n");
}


#endif /* PROFILING */
