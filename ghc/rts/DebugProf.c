/* -----------------------------------------------------------------------------
 * $Id: DebugProf.c,v 1.2 1998/12/02 13:28:14 simonm Exp $
 *
 * (c) The GHC Team 1998
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
   Hash table for symbols.
   -------------------------------------------------------------------------- */

typedef struct {
    const char *name;
    void *ptr;
    nat data;
} symbol_info;

#define SYMBOL_HASH_SIZE 0x3fff

symbol_info symbol_hash[SYMBOL_HASH_SIZE];

static inline nat
hash(void *ptr)
{
    return ((W_)ptr)>>4 & 0x3fff;
}

static void
initSymbolHash(void)
{
    nat i;

    for (i=0; i < SYMBOL_HASH_SIZE; i++) {
	symbol_hash[i].ptr = NULL;
    }
}

static nat
lookup_symbol(void *addr)
{
    nat orig_bucket = hash(addr);
    nat bucket;

    bucket = orig_bucket;
    while (bucket < SYMBOL_HASH_SIZE && symbol_hash[bucket].ptr != NULL) {
	if (symbol_hash[bucket].ptr == addr) {
	    return bucket;
	}
	bucket++;
    }
    if (bucket == SYMBOL_HASH_SIZE) {
	bucket = 0;
	while (bucket < orig_bucket && symbol_hash[bucket].ptr != NULL) {
	    if (symbol_hash[bucket].ptr == addr) {
		return bucket;
	    }
	    bucket++;
	}
	if (bucket == orig_bucket) {
	    barf("out of symbol table space");
	}
    }
    
    symbol_hash[bucket].ptr  = addr;
    lookupGHCName(addr,&symbol_hash[bucket].name);
    symbol_hash[bucket].data = 0;
    return bucket;
}

static void
clear_table_data(void)
{
    nat i;

    for (i = 0; i < SYMBOL_HASH_SIZE; i++) {
	symbol_hash[i].data = 0;
    }
}

static void
fprint_data(FILE *fp)
{
    nat i;
    
    for (i = 0; i < SYMBOL_HASH_SIZE; i++) {
	if (symbol_hash[i].data) {
	    fprintf(fp, "   %s %d\n", symbol_hash[i].name, symbol_hash[i].data);
	}
    }
}

static inline void
add_data(void *addr, nat data)
{
    symbol_hash[lookup_symbol(addr)].data += data;
}

/* -----------------------------------------------------------------------------
   Closure Type Profiling;
   -------------------------------------------------------------------------- */

static nat closure_types[N_CLOSURE_TYPES];

static char *type_names[] = {
      "INVALID_OBJECT"
    , "CONSTR"
    , "CONSTR_INTLIKE"
    , "CONSTR_CHARLIKE"
    , "CONSTR_STATIC"
    , "CONSTR_NOCAF_STATIC"

    , "FUN"
    , "FUN_STATIC"

    , "THUNK"
    , "THUNK_STATIC"
    , "THUNK_SELECTOR"

    , "BCO"
    , "AP_UPD"

    , "PAP"

    , "IND"
    , "IND_OLDGEN"
    , "IND_PERM"
    , "IND_OLDGEN_PERM"
    , "IND_STATIC"

    , "RET_BCO"
    , "RET_SMALL"
    , "RET_VEC_SMALL"
    , "RET_BIG"
    , "RET_VEC_BIG"
    , "RET_DYN"
    , "UPDATE_FRAME"
    , "CATCH_FRAME"
    , "STOP_FRAME"
    , "SEQ_FRAME"

    , "BLACKHOLE"
    , "MVAR"

    , "ARR_WORDS"
    , "ARR_PTRS"

    , "MUT_ARR_WORDS"
    , "MUT_ARR_PTRS"
    , "MUT_ARR_PTRS_FROZEN"
    , "MUT_VAR"

    , "WEAK"
    , "FOREIGN"
  
    , "TSO"

    , "BLOCKED_FETCH"
    , "FETCH_ME"

    , "EVACUATED"
};

static void 
fprint_closure_types(FILE *fp)
{
  nat i;

  for (i = 0; i < N_CLOSURE_TYPES; i++) {
    if (closure_types[i]) {
      fprintf(fp, "   %s %d\n", type_names[i], closure_types[i]);
    }
  }
}

/* -----------------------------------------------------------------------------
   The profiler itself
   -------------------------------------------------------------------------- */

nat
initProfiling(void)
{
    if (! RtsFlags.ProfFlags.doHeapProfile) {
        return 0;
    }

    sprintf(prof_filename, "%.124s.hp", prog_argv[0]);

    prof_file = fopen(prof_filename, "w");
    if (prof_file == NULL) {
	fprintf(stderr, "Can't open heap profiling log file %s\n",
		prof_filename);
	return 1;
    }

    fprintf(prof_file, "JOB \"%s\"\n", prog_argv[0]);
    fprintf(prof_file, "DATE \"%s\"\n", time_str());

    fprintf(prof_file, "SAMPLE_UNIT \"seconds\"\n");
    fprintf(prof_file, "VALUE_UNIT \"bytes\"\n");

    fprintf(prof_file, "BEGIN_SAMPLE 0.00\n");
    fprintf(prof_file, "END_SAMPLE 0.00\n");

    DEBUG_LoadSymbols(prog_argv[0]);

    initSymbolHash();

    return 0;
}

void
endProfiling(void)
{
    StgDouble seconds;

    if (! RtsFlags.ProfFlags.doHeapProfile) {
        return;
    }

    seconds = usertime();
    fprintf(prof_file, "BEGIN_SAMPLE %0.2f\n", seconds);
    fprintf(prof_file, "END_SAMPLE %0.2f\n", seconds);
    fclose(prof_file);
}

void
heapCensus(bdescr *bd)
{
    StgPtr p;
    const StgInfoTable *info;
    StgDouble time;
    nat size;
    
    switch (RtsFlags.ProfFlags.doHeapProfile) {
    case HEAP_BY_INFOPTR:
      clear_table_data();
      break;
    case HEAP_BY_CLOSURE_TYPE:
      memset(closure_types, 0, N_CLOSURE_TYPES * sizeof(nat));
      break;
    default:
      return;
    }

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
	    case MUT_ARR_WORDS:
		size = arr_words_sizeW(stgCast(StgArrWords*,p));
		break;

	    case ARR_PTRS:
	    case MUT_ARR_PTRS:
	    case MUT_ARR_PTRS_FROZEN:
		size = arr_ptrs_sizeW((StgArrPtrs *)p);
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

