/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2017
 *
 * Generating cost-center profiler report
 *
 * ---------------------------------------------------------------------------*/

#if defined(PROFILING)

#include "PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "ProfilerReport.h"
#include "Profiling.h"

static  uint32_t          numDigits       ( StgInt i );
static  void              findCCSMaxLens  ( CostCentreStack const *ccs,
                                            uint32_t indent,
                                            uint32_t *max_label_len,
                                            uint32_t *max_module_len,
                                            uint32_t *max_src_len,
                                            uint32_t *max_id_len );
static  void              logCCS          ( FILE *prof_file,
                                            CostCentreStack const *ccs,
                                            ProfilerTotals totals,
                                            uint32_t indent,
                                            uint32_t max_label_len,
                                            uint32_t max_module_len,
                                            uint32_t max_src_len,
                                            uint32_t max_id_len );
static  void              fprintHeader    ( FILE *prof_file,
                                            uint32_t max_label_len, uint32_t max_module_len,
                                            uint32_t max_src_len, uint32_t max_id_len );
static  void              reportCCS       ( FILE *prof_file, CostCentreStack const *ccs,
                                            ProfilerTotals totals );

static uint32_t
strlen_utf8 (char *s)
{
    uint32_t n = 0;
    unsigned char c;

    for (; *s != '\0'; s++) {
        c = *s;
        if (c < 0x80 || c > 0xBF) n++;
    }
    return n;
}

/* -----------------------------------------------------------------------------
   Generate the cost-centre-stack time/alloc report
   -------------------------------------------------------------------------- */

static void
fprintHeader( FILE *prof_file, uint32_t max_label_len, uint32_t max_module_len,
              uint32_t max_src_len, uint32_t max_id_len )
{
    fprintf(prof_file, "%-*s %-*s %-*s %-*s %11s  %12s   %12s\n",
            max_label_len, "",
            max_module_len, "",
            max_src_len, "",
            max_id_len, "",
            "", "individual", "inherited");

    fprintf(prof_file, "%-*s %-*s %-*s %-*s",
            max_label_len, "COST CENTRE",
            max_module_len, "MODULE",
            max_src_len, "SRC",
            max_id_len, "no.");

    fprintf(prof_file, " %11s  %5s %6s   %5s %6s",
            "entries", "%time", "%alloc", "%time", "%alloc");

    if (RtsFlags.CcFlags.doCostCentres >= COST_CENTRES_VERBOSE) {
        fprintf(prof_file, "  %5s %9s", "ticks", "bytes");
    }

    fprintf(prof_file, "\n\n");
}


/* -----------------------------------------------------------------------------
   Generating the aggregated per-cost-centre time/alloc report.

   This is the sorted list of cost centers appearing at the top of the output.
   -------------------------------------------------------------------------- */

static CostCentre *sorted_cc_list;

static void
insertCCInSortedList( CostCentre *new_cc )
{
    CostCentre **prev, *cc;

    prev = &sorted_cc_list;
    for (cc = sorted_cc_list; cc != NULL; cc = cc->link) {
        if (new_cc->time_ticks > cc->time_ticks) {
            new_cc->link = cc;
            *prev = new_cc;
            return;
        } else {
            prev = &(cc->link);
        }
    }
    new_cc->link = NULL;
    *prev = new_cc;
}


static void
reportPerCCCosts( FILE *prof_file, ProfilerTotals totals )
{
    CostCentre *cc, *next;
    uint32_t max_label_len, max_module_len, max_src_len;

    sorted_cc_list = NULL;

    max_label_len  = 11; // no shorter than the "COST CENTRE" header
    max_module_len = 6;  // no shorter than the "MODULE" header
    max_src_len    = 3;  // no shorter than the "SRC" header

    for (cc = CC_LIST; cc != NULL; cc = next) {
        next = cc->link;
        if (cc->time_ticks > totals.total_prof_ticks/100
            || cc->mem_alloc > totals.total_alloc/100
            || RtsFlags.CcFlags.doCostCentres >= COST_CENTRES_ALL) {
            insertCCInSortedList(cc);

            max_label_len = stg_max(strlen_utf8(cc->label), max_label_len);
            max_module_len = stg_max(strlen_utf8(cc->module), max_module_len);
            max_src_len = stg_max(strlen_utf8(cc->srcloc), max_src_len);
        }
    }

    fprintf(prof_file, "%-*s %-*s %-*s",
            max_label_len, "COST CENTRE", max_module_len, "MODULE", max_src_len, "SRC");
    fprintf(prof_file, " %6s %6s", "%time", "%alloc");
    if (RtsFlags.CcFlags.doCostCentres >= COST_CENTRES_VERBOSE) {
        fprintf(prof_file, "  %5s %9s", "ticks", "bytes");
    }
    fprintf(prof_file, "\n\n");

    for (cc = sorted_cc_list; cc != NULL; cc = cc->link) {
        if (ignoreCC(cc)) {
            continue;
        }
        fprintf(prof_file, "%s%*s %s%*s %s%*s",
                cc->label,
                max_label_len - strlen_utf8(cc->label), "",
                cc->module,
                max_module_len - strlen_utf8(cc->module), "",
                cc->srcloc,
                max_src_len - strlen_utf8(cc->srcloc), "");

        fprintf(prof_file, " %6.1f %6.1f",
                totals.total_prof_ticks == 0 ? 0.0 : (cc->time_ticks / (StgFloat) totals.total_prof_ticks * 100),
                totals.total_alloc == 0 ? 0.0 : (cc->mem_alloc / (StgFloat) totals.total_alloc * 100)
            );

        if (RtsFlags.CcFlags.doCostCentres >= COST_CENTRES_VERBOSE) {
            fprintf(prof_file, "  %5" FMT_Word64 " %9" FMT_Word64,
                    (StgWord64)(cc->time_ticks), cc->mem_alloc*sizeof(W_));
        }
        fprintf(prof_file, "\n");
    }

    fprintf(prof_file,"\n\n");
}

static uint32_t
numDigits(StgInt i) {
    uint32_t result;

    result = 1;

    if (i < 0) i = 0;

    while (i > 9) {
        i /= 10;
        result++;
    }

    return result;
}

static void
findCCSMaxLens(CostCentreStack const *ccs, uint32_t indent, uint32_t *max_label_len,
               uint32_t *max_module_len, uint32_t *max_src_len, uint32_t *max_id_len) {
    CostCentre *cc;
    IndexTable *i;

    cc = ccs->cc;

    *max_label_len = stg_max(*max_label_len, indent + strlen_utf8(cc->label));
    *max_module_len = stg_max(*max_module_len, strlen_utf8(cc->module));
    *max_src_len = stg_max(*max_src_len, strlen_utf8(cc->srcloc));
    *max_id_len = stg_max(*max_id_len, numDigits(ccs->ccsID));

    for (i = ccs->indexTable; i != 0; i = i->next) {
        if (!i->back_edge) {
            findCCSMaxLens(i->ccs, indent+1,
                    max_label_len, max_module_len, max_src_len, max_id_len);
        }
    }
}

static void
logCCS(FILE *prof_file, CostCentreStack const *ccs, ProfilerTotals totals,
       uint32_t indent,
       uint32_t max_label_len, uint32_t max_module_len,
       uint32_t max_src_len, uint32_t max_id_len)
{
    CostCentre *cc;
    IndexTable *i;

    cc = ccs->cc;

    /* Only print cost centres with non 0 data ! */

    if (!ignoreCCS(ccs))
        /* force printing of *all* cost centres if -Pa */
    {

        fprintf(prof_file, "%*s%s%*s %s%*s %s%*s",
                indent, "",
                cc->label,
                max_label_len-indent - strlen_utf8(cc->label), "",
                cc->module,
                max_module_len - strlen_utf8(cc->module), "",
                cc->srcloc,
                max_src_len - strlen_utf8(cc->srcloc), "");

        fprintf(prof_file,
                " %*" FMT_Int " %11" FMT_Word64 "  %5.1f  %5.1f   %5.1f  %5.1f",
                max_id_len, ccs->ccsID, ccs->scc_count,
                totals.total_prof_ticks == 0 ? 0.0 : ((double)ccs->time_ticks / (double)totals.total_prof_ticks * 100.0),
                totals.total_alloc == 0 ? 0.0 : ((double)ccs->mem_alloc / (double)totals.total_alloc * 100.0),
                totals.total_prof_ticks == 0 ? 0.0 : ((double)ccs->inherited_ticks / (double)totals.total_prof_ticks * 100.0),
                totals.total_alloc == 0 ? 0.0 : ((double)ccs->inherited_alloc / (double)totals.total_alloc * 100.0)
            );

        if (RtsFlags.CcFlags.doCostCentres >= COST_CENTRES_VERBOSE) {
            fprintf(prof_file, "  %5" FMT_Word64 " %9" FMT_Word64,
                    (StgWord64)(ccs->time_ticks), ccs->mem_alloc*sizeof(W_));
        }
        fprintf(prof_file, "\n");
    }

    for (i = ccs->indexTable; i != 0; i = i->next) {
        if (!i->back_edge) {
            logCCS(prof_file, i->ccs, totals, indent+1,
                   max_label_len, max_module_len, max_src_len, max_id_len);
        }
    }
}

static void
reportCCS(FILE *prof_file, CostCentreStack const *ccs, ProfilerTotals totals)
{
    uint32_t max_label_len, max_module_len, max_src_len, max_id_len;

    max_label_len = 11; // no shorter than "COST CENTRE" header
    max_module_len = 6; // no shorter than "MODULE" header
    max_src_len = 3; // no shorter than "SRC" header
    max_id_len = 3; // no shorter than "no." header

    findCCSMaxLens(ccs, 0,
                   &max_label_len, &max_module_len, &max_src_len, &max_id_len);

    fprintHeader(prof_file,
                 max_label_len, max_module_len, max_src_len, max_id_len);
    logCCS(prof_file, ccs, totals, 0,
           max_label_len, max_module_len, max_src_len, max_id_len);
}

void
writeCCSReport( FILE *prof_file, CostCentreStack const *stack,
                ProfilerTotals totals )
{
    char temp[128]; /* sigh: magic constant */

    fprintf(prof_file, "\t%s Time and Allocation Profiling Report  (%s)\n",
            time_str(), "Final");

    fprintf(prof_file, "\n\t  ");
    fprintf(prof_file, " %s", prog_name);
    fprintf(prof_file, " +RTS");
    for (int count = 0; rts_argv[count]; count++)
        fprintf(prof_file, " %s", rts_argv[count]);
    fprintf(prof_file, " -RTS");
    for (int count = 1; prog_argv[count]; count++)
        fprintf(prof_file, " %s", prog_argv[count]);
    fprintf(prof_file, "\n\n");

    fprintf(prof_file, "\ttotal time  = %11.2f secs   (%lu ticks @ %d us, %d processor%s)\n",
            ((double) totals.total_prof_ticks *
             (double) RtsFlags.MiscFlags.tickInterval) / (TIME_RESOLUTION * n_capabilities),
            (unsigned long) totals.total_prof_ticks,
            (int) TimeToUS(RtsFlags.MiscFlags.tickInterval),
            n_capabilities, n_capabilities > 1 ? "s" : "");

    fprintf(prof_file, "\ttotal alloc = %11s bytes",
            showStgWord64(totals.total_alloc * sizeof(W_),
                          temp, true/*commas*/));

    fprintf(prof_file, "  (excludes profiling overheads)\n\n");

    reportPerCCCosts(prof_file, totals);
    reportCCS(prof_file, stack, totals);
}


#endif /* PROFILING */
