/* -----------------------------------------------------------------------------
 * $Id: DebugProf.c,v 1.7 1999/09/15 13:45:16 simonmar Exp $
 *
 * (c) The GHC Team 1998-1999
 *
 * Simple Heap Profiling
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "Printer.h"
#include "BlockAlloc.h"
#include "DebugProf.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Stats.h"

#if defined(DEBUG) && ! defined(PROFILING)

char prof_filename[128];
FILE *prof_file;

static void clear_table_data(void);
static void fprint_data(FILE *fp);

/* -----------------------------------------------------------------------------
   The profiler itself
   -------------------------------------------------------------------------- */

void
heapCensus(bdescr *bd)
{
    StgPtr p;
    const StgInfoTable *info;
    StgDouble time;
    nat size;
    
    /* usertime() isn't very accurate, since it includes garbage
     * collection time.  We really want elapsed_mutator_time or
     * something.  ToDo.
     */
    time = usertime();
    fprintf(prof_file, "BEGIN_SAMPLE %0.2f\n", time);

    while (bd != NULL) {
	p = bd->start;
	while (p < bd->free) {
	    info = get_itbl((StgClosure *)p);

	    switch (info->type) {
	    case BCO:
		size = bco_sizeW((StgBCO *)p);
		break;

	    case FUN:
	    case THUNK:
	    case CONSTR:
	    case IND_PERM:
	    case IND_OLDGEN_PERM:
	    case BLACKHOLE:
	    case BLACKHOLE_BQ:
	    case WEAK:
	    case FOREIGN:
	    case MVAR:
	    case MUT_VAR:
	    case CONSTR_INTLIKE:
	    case CONSTR_CHARLIKE:
	    case CONSTR_STATIC:
	    case CONSTR_NOCAF_STATIC:
	    case THUNK_STATIC:
	    case FUN_STATIC:
	    case IND_STATIC:
		size = sizeW_fromITBL(info);
		break;

	    case THUNK_SELECTOR:
		size = sizeofW(StgHeader) + MIN_UPD_SIZE;
		break;

	    case IND:
	    case IND_OLDGEN:
		size = sizeofW(StgInd);
		break;

	    case AP_UPD: /* we can treat this as being the same as a PAP */
	    case PAP:
		size = pap_sizeW((StgPAP *)p);
		break;

	    case ARR_WORDS:
		size = arr_words_sizeW(stgCast(StgArrWords*,p));
		break;

	    case MUT_ARR_PTRS:
	    case MUT_ARR_PTRS_FROZEN:
		size = mut_arr_ptrs_sizeW((StgMutArrPtrs *)p);
		break;

	    case TSO:
		size = tso_sizeW((StgTSO *)p);
		break;

	    default:
		barf("heapCensus");
	    }
	    switch (RtsFlags.ProfFlags.doHeapProfile) {
	    case HEAP_BY_INFOPTR:
	      add_data((void *)(*p), size * sizeof(W_));
	      break;
	    case HEAP_BY_CLOSURE_TYPE:
	      closure_types[info->type] += size * sizeof(W_);
	      break;
	    }
	    p += size;
	}
	bd = bd->link;
    }

    switch (RtsFlags.ProfFlags.doHeapProfile) {
    case HEAP_BY_INFOPTR:
      fprint_data(prof_file);
      break;
    case HEAP_BY_CLOSURE_TYPE:
      fprint_closure_types(prof_file);
      break;
    }
    
    fprintf(prof_file, "END_SAMPLE %0.2f\n", time);
}    

#endif

