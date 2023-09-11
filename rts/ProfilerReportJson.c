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

// Including zero byte
static size_t escaped_size(char const* str)
{
    size_t escaped_size = 0;
    for (; *str != '\0'; str++) {
        const unsigned char c = *str;
        switch (c)
            {
                // quotation mark (0x22)
                case '"':
                {
                    escaped_size += 2;
                    break;
                }

                case '\\':
                {
                    escaped_size += 2;
                    break;
                }

                // backspace (0x08)
                case '\b':
                {
                    escaped_size += 2;
                    break;
                }

                // formfeed (0x0c)
                case '\f':
                {
                    escaped_size += 2;
                    break;
                }

                // newline (0x0a)
                case '\n':
                {
                    escaped_size += 2;
                    break;
                }

                // carriage return (0x0d)
                case '\r':
                {
                    escaped_size += 2;
                    break;
                }

                // horizontal tab (0x09)
                case '\t':
                {
                    escaped_size += 2;
                    break;
                }

                default:
                {
                    if (c <= 0x1f)
                    {
                        // print character c as \uxxxx
                        escaped_size += 6;
                    }
                    else
                    {
                        escaped_size ++;
                    }
                    break;
                }
            }
    }
    escaped_size++; // null byte

    return escaped_size;
}

static void escapeString(char const* str, char **buf)
{
    char *out;
    size_t out_size; //Max required size for decoding.
    size_t pos = 0;

    out_size = escaped_size(str); //includes trailing zero byte
    out = stgMallocBytes(out_size, "writeCCSReportJson");
    for (; *str != '\0'; str++) {
        const unsigned char c = *str;
        switch (c)
            {
                // quotation mark (0x22)
                case '"':
                {
                    out[pos] = '\\';
                    out[pos + 1] = '"';
                    pos += 2;
                    break;
                }

                // reverse solidus (0x5c)
                case '\\':
                {
                    out[pos] = '\\';
                    out[pos+1] = '\\';
                    pos += 2;
                    break;
                }

                // backspace (0x08)
                case '\b':
                {
                    out[pos] = '\\';
                    out[pos + 1] = 'b';
                    pos += 2;
                    break;
                }

                // formfeed (0x0c)
                case '\f':
                {
                    out[pos] = '\\';
                    out[pos + 1] = 'f';
                    pos += 2;
                    break;
                }

                // newline (0x0a)
                case '\n':
                {
                    out[pos] = '\\';
                    out[pos + 1] = 'n';
                    pos += 2;
                    break;
                }

                // carriage return (0x0d)
                case '\r':
                {
                    out[pos] = '\\';
                    out[pos + 1] = 'r';
                    pos += 2;
                    break;
                }

                // horizontal tab (0x09)
                case '\t':
                {
                    out[pos] = '\\';
                    out[pos + 1] = 't';
                    pos += 2;
                    break;
                }

                default:
                {
                    if (c <= 0x1f)
                    {
                        // print character c as \uxxxx
                        out[pos] = '\\';
                        sprintf(&out[pos + 1], "u%04x", (int)c);
                        pos += 6;
                    }
                    else
                    {
                        // all other characters are added as-is
                        out[pos++] = c;
                    }
                    break;
                }
            }
    }
    out[pos++] = '\0';
    assert(pos == out_size);
    *buf = out;
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
