/* -----------------------------------------------------------------------------
 * $Id: ProfHeap.c,v 1.10 2000/04/03 15:54:49 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Support for heap profiling
 *
 * ---------------------------------------------------------------------------*/

#if defined(DEBUG) && !defined(PROFILING)
#define DEBUG_HEAP_PROF
#else
#undef DEBUG_HEAP_PROF
#endif

#if defined(PROFILING) || defined(DEBUG_HEAP_PROF)

#include "Rts.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "Profiling.h"
#include "Storage.h"
#include "ProfHeap.h"
#include "Stats.h"
#ifdef DEBUG_HEAP_PROF
#include "Printer.h"
static void initSymbolHash(void);
static void clear_table_data(void);
static void fprint_data(FILE *fp);
#endif

char prof_filename[128];	/* urk */

#ifdef DEBUG_HEAP_PROF
FILE *prof_file;

void initProfiling1( void )
{
}

void initProfiling2( void )
{
  initHeapProfiling();
}

void endProfiling( void )
{
  endHeapProfiling();
}
#endif /* DEBUG_HEAP_PROF */

nat
initHeapProfiling(void)
{
    if (! RtsFlags.ProfFlags.doHeapProfile) {
        return 0;
    }

    fprintf(prof_file, "JOB \"%s\"\n", prog_argv[0]);
    fprintf(prof_file, "DATE \"%s\"\n", time_str());

    fprintf(prof_file, "SAMPLE_UNIT \"seconds\"\n");
    fprintf(prof_file, "VALUE_UNIT \"bytes\"\n");

    fprintf(prof_file, "BEGIN_SAMPLE 0.00\n");
    fprintf(prof_file, "END_SAMPLE 0.00\n");

#ifdef DEBUG_HEAP_PROF
    DEBUG_LoadSymbols(prog_argv[0]);
    initSymbolHash();
#endif

    return 0;
}

void
endHeapProfiling(void)
{
    StgDouble seconds;

    if (! RtsFlags.ProfFlags.doHeapProfile) {
        return;
    }

    seconds = mut_user_time();
    fprintf(prof_file, "BEGIN_SAMPLE %0.2f\n", seconds);
    fprintf(prof_file, "END_SAMPLE %0.2f\n", seconds);
    fclose(prof_file);
}

#ifdef DEBUG_HEAP_PROF
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

   PROBABLY TOTALLY OUT OF DATE -- ToDo (SDM)
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
    , "BLACKHOLE_BQ"
    , "MVAR"

    , "ARR_WORDS"

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

#endif /* DEBUG_HEAP_PROF */


#ifdef PROFILING
static void
clearCCSResid(CostCentreStack *ccs)
{
  IndexTable *i;

  ccs->mem_resid = 0;

  for (i = ccs->indexTable; i != 0; i = i->next) {
    if (!i->back_edge) {
      clearCCSResid(i->ccs);
    }
  }
}

static void
fprint_ccs(FILE *fp, CostCentreStack *ccs, nat components)
{
  CostCentre *cc;
  CostCentreStack *prev;

  cc = ccs->cc;
  prev = ccs->prevStack;

  if (prev == NULL
      || prev->cc->is_caf != CC_IS_BORING
      || components == 1) { 
    fprintf(fp,"%s",cc->label);
    return; 

  } else {
    fprint_ccs(fp, ccs->prevStack,components-1);
    fprintf(fp,"/%s",cc->label);
  }
}

static void
reportCCSResid(FILE *fp, CostCentreStack *ccs)
{
  IndexTable *i;

  if (ccs->mem_resid != 0) {
    fprintf(fp,"   ");
    fprint_ccs(fp,ccs,2/*print 2 components only*/);
    fprintf(fp," %ld\n", ccs->mem_resid * sizeof(W_));
  }

  for (i = ccs->indexTable; i != 0; i = i->next) {
    if (!i->back_edge) {
      reportCCSResid(fp,i->ccs);
    }
  }
}
#endif

void
heapCensus(void)
{
  bdescr *bd;
  const StgInfoTable *info;
  StgDouble time;
  nat size;
  StgPtr p;
  
#ifdef DEBUG_HEAP_PROF
  switch (RtsFlags.ProfFlags.doHeapProfile) {
  case HEAP_BY_INFOPTR:
    clear_table_data();
    break;
  case HEAP_BY_CLOSURE_TYPE:
#if 0
#   error fix me      
    memset(closure_types, 0, N_CLOSURE_TYPES * sizeof(nat));
#endif
    break;
  default:
    return;
  }
#endif

#ifdef PROFILING
  switch (RtsFlags.ProfFlags.doHeapProfile) {
  case NO_HEAP_PROFILING:
    return;
  case HEAP_BY_CCS:
    break;
  default:
    barf("heapCensus; doHeapProfile");
  }
  /* zero all the residency counters */
  clearCCSResid(CCS_MAIN);
#endif

  /* Only do heap profiling in a two-space heap */
  ASSERT(RtsFlags.GcFlags.generations == 1);
  bd = g0s0->to_space;

  time = mut_user_time_during_GC();
  fprintf(prof_file, "BEGIN_SAMPLE %0.2f\n", time);
  
  while (bd != NULL) {
    p = bd->start;
    while (p < bd->free) {
      info = get_itbl((StgClosure *)p);

      switch (info->type) {
      case BCO:
	size = bco_sizeW((StgBCO *)p);
	break;
	
      case CONSTR:
	if (((StgClosure *)p)->header.info == &DEAD_WEAK_info) {
	  size = sizeofW(StgWeak);
	  break;
	}
	/* else, fall through... */

      case FUN:
      case THUNK:
      case IND_PERM:
      case IND_OLDGEN_PERM:
      case CAF_BLACKHOLE:
      case SE_CAF_BLACKHOLE:
      case SE_BLACKHOLE:
      case BLACKHOLE:
      case BLACKHOLE_BQ:
      case WEAK:
      case FOREIGN:
      case STABLE_NAME:
      case MVAR:
      case MUT_VAR:
      case CONSTR_INTLIKE:
      case CONSTR_CHARLIKE:
      case FUN_1_0:
      case FUN_0_1:
      case FUN_1_1:
      case FUN_0_2:
      case FUN_2_0:
      case THUNK_1_1:
      case THUNK_0_2:
      case THUNK_2_0:
      case CONSTR_1_0:
      case CONSTR_0_1:
      case CONSTR_1_1:
      case CONSTR_0_2:
      case CONSTR_2_0:
	size = sizeW_fromITBL(info);
	break;
	
      case THUNK_1_0:		/* ToDo - shouldn't be here */
      case THUNK_0_1:		/* "  ditto  " */
      case THUNK_SELECTOR:
	size = sizeofW(StgHeader) + MIN_UPD_SIZE;
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

#ifdef DEBUG_HEAP_PROF
      switch (RtsFlags.ProfFlags.doHeapProfile) {
      case HEAP_BY_INFOPTR:
	add_data((void *)(*p), size * sizeof(W_));
	break;
      case HEAP_BY_CLOSURE_TYPE:
	closure_types[info->type] += size * sizeof(W_);
	break;
      }
#endif

#ifdef PROFILING      
      ((StgClosure *)p)->header.prof.ccs->mem_resid += size;
#endif
      p += size;
    }
    bd = bd->link;
  }

#ifdef DEBUG_HEAP_PROF
  switch (RtsFlags.ProfFlags.doHeapProfile) {
  case HEAP_BY_INFOPTR:
    fprint_data(prof_file);
    break;
  case HEAP_BY_CLOSURE_TYPE:
    fprint_closure_types(prof_file);
    break;
  }
#endif
    
#ifdef PROFILING
  reportCCSResid(prof_file,CCS_MAIN);
#endif

  fprintf(prof_file, "END_SAMPLE %0.2f\n", time);
}    

#endif /* PROFILING || DEBUG_HEAP_PROF */

