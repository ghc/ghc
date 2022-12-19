/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2017
 *
 * Generating cost-centre profiler JSON report
 *
 * ---------------------------------------------------------------------------*/

#if defined(PROFILING)

#include "rts/PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "ProfilerReportJson.h"
#include "Profiling.h"

#include <string.h>

// I don't think this code is all that perf critical.
// So we just allocate a new buffer each time around.
static void escapeString(char const* str, char **buf)
{
    char *out;
    size_t req_size; //Max required size for decoding.
    size_t in_size;  //Input size, including zero.

    in_size = strlen(str) + 1;
    // The strings are generally small and short
    // lived so should be ok to just double the size.
    req_size = in_size * 2;
    out = stgMallocBytes(req_size, "writeCCSReportJson");
    *buf = out;
    // We provide an outputbuffer twice the size of the input,
    // and at worse double the output size. So we can skip
    // length checks.
    for (; *str != '\0'; str++) {
        char c = *str;
        if (c == '\\') {
            *out = '\\'; out++;
            *out = '\\'; out++;
        } else if (c == '\n') {
            *out = '\\'; out++;
            *out = 'n';  out++;
        } else {
            *out = c; out++;
        }
    }
    *out = '\0';
}

static void
logCostCentres(FILE *prof_file)
{
    bool needs_comma = false;
    fprintf(prof_file, "[\n");
    for (CostCentre *cc = CC_LIST; cc != NULL; cc = cc->link) {
        char *lbl, *src_loc;
        escapeString(cc->label, &lbl);
        escapeString(cc->srcloc, &src_loc);
        fprintf(prof_file,
                "%s"
                "{\"id\": %" FMT_Int ", "
                "\"label\": \"%s\", "
                "\"module\": \"%s\", "
                "\"src_loc\": \"%s\", "
                "\"is_caf\": %s}",
                needs_comma ? ", " : "",
                cc->ccID, lbl, cc->module, src_loc,
                cc->is_caf ? "true" : "false");
        needs_comma = true;
        stgFree(lbl);
        stgFree(src_loc);
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
    for (int count = 0; prog_argv[count]; count++) {
        char* arg;
        escapeString(prog_argv[count], &arg);
        fprintf(prof_file, "%s\"%s\"",
                count == 0 ? "" : ", ", arg);
        stgFree(arg);
    }
    fprintf(prof_file, "],\n\"rts_arguments\": [");
    for (int count = 0; rts_argv[count]; count++) {
        char* arg;
        escapeString(rts_argv[count], &arg);
        fprintf(prof_file, "%s\"%s\"",
                count == 0 ? "" : ", ", arg);
        stgFree(arg);
    }
    fprintf(prof_file, "],\n");

    fprintf(prof_file, "\"end_time\": \"%s\",\n", time_str());
    fprintf(prof_file, "\"initial_capabilities\": %d,\n",
            RtsFlags.ParFlags.nCapabilities);
    fprintf(prof_file, "\"total_time\": %11.2f,\n",
            ((double) totals.total_prof_ticks *
             (double) RtsFlags.MiscFlags.tickInterval) / (TIME_RESOLUTION * getNumCapabilities()));
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
