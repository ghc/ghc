/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1994-2000.
 *
 * Heap printer
 * 
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "Printer.h"
#include "RtsUtils.h"

#ifdef DEBUG

#include "RtsFlags.h"
#include "MBlock.h"
#include "Storage.h"
#include "Bytecodes.h"  /* for InstrPtr */
#include "Disassembler.h"
#include "Apply.h"

#include <stdlib.h>
#include <string.h>

#if defined(GRAN) || defined(PAR)
// HWL: explicit fixed header size to make debugging easier
int fixed_hs = sizeof(StgHeader), itbl_sz = sizeofW(StgInfoTable), 
    uf_sz=sizeofW(StgUpdateFrame); 
#endif

/* --------------------------------------------------------------------------
 * local function decls
 * ------------------------------------------------------------------------*/

static void    printStdObjPayload( StgClosure *obj );
#ifdef USING_LIBBFD
static void    reset_table   ( int size );
static void    prepare_table ( void );
static void    insert        ( unsigned value, const char *name );
#endif
#if 0 /* unused but might be useful sometime */
static rtsBool lookup_name   ( char *name, unsigned *result );
static void    enZcode       ( char *in, char *out );
#endif
static char    unZcode       ( char ch );
const char *   lookupGHCName ( void *addr );
static void    printZcoded   ( const char *raw );

/* --------------------------------------------------------------------------
 * Printer
 * ------------------------------------------------------------------------*/

void printPtr( StgPtr p )
{
    const char *raw;
    raw = lookupGHCName(p);
    if (raw != NULL) {
        printZcoded(raw);
    } else {
        debugBelch("%p", p);
    }
}
  
void printObj( StgClosure *obj )
{
    debugBelch("Object "); printPtr((StgPtr)obj); debugBelch(" = ");
    printClosure(obj);
}

STATIC_INLINE void
printStdObjHdr( StgClosure *obj, char* tag )
{
    debugBelch("%s(",tag);
    printPtr((StgPtr)obj->header.info);
#ifdef PROFILING
    debugBelch(", %s", obj->header.prof.ccs->cc->label);
#endif
}

static void
printStdObjPayload( StgClosure *obj )
{
    StgWord i, j;
    const StgInfoTable* info;

    info = get_itbl(obj);
    for (i = 0; i < info->layout.payload.ptrs; ++i) {
        debugBelch(", ");
        printPtr((StgPtr)obj->payload[i]);
    }
    for (j = 0; j < info->layout.payload.nptrs; ++j) {
        debugBelch(", %pd#",obj->payload[i+j]);
    }
    debugBelch(")\n");
}

static void
printThunkPayload( StgThunk *obj )
{
    StgWord i, j;
    const StgInfoTable* info;

    info = get_itbl(obj);
    for (i = 0; i < info->layout.payload.ptrs; ++i) {
        debugBelch(", ");
        printPtr((StgPtr)obj->payload[i]);
    }
    for (j = 0; j < info->layout.payload.nptrs; ++j) {
        debugBelch(", %pd#",obj->payload[i+j]);
    }
    debugBelch(")\n");
}

static void
printThunkObject( StgThunk *obj, char* tag )
{
    printStdObjHdr( (StgClosure *)obj, tag );
    printThunkPayload( obj );
}

void
printClosure( StgClosure *obj )
{
    StgInfoTable *info;
    
    info = get_itbl(obj);

    switch ( info->type ) {
    case INVALID_OBJECT:
            barf("Invalid object");

    case CONSTR:
    case CONSTR_1_0: case CONSTR_0_1:
    case CONSTR_1_1: case CONSTR_0_2: case CONSTR_2_0:
    case CONSTR_INTLIKE:
    case CONSTR_CHARLIKE:
    case CONSTR_STATIC:
    case CONSTR_NOCAF_STATIC:
        {
            StgWord i, j;
#ifdef PROFILING
	    debugBelch("%s(", info->prof.closure_desc);
	    debugBelch("%s", obj->header.prof.ccs->cc->label);
#else
            debugBelch("CONSTR(");
            printPtr((StgPtr)obj->header.info);
            debugBelch("(tag=%d)",info->srt_bitmap);
#endif
            for (i = 0; i < info->layout.payload.ptrs; ++i) {
		debugBelch(", ");
                printPtr((StgPtr)obj->payload[i]);
            }
            for (j = 0; j < info->layout.payload.nptrs; ++j) {
                debugBelch(", %p#", obj->payload[i+j]);
            }
            debugBelch(")\n");
            break;
        }

    case FUN:
    case FUN_1_0: case FUN_0_1: 
    case FUN_1_1: case FUN_0_2: case FUN_2_0:
    case FUN_STATIC:
	debugBelch("FUN/%d(",itbl_to_fun_itbl(info)->f.arity);
	printPtr((StgPtr)obj->header.info);
#ifdef PROFILING
	debugBelch(", %s", obj->header.prof.ccs->cc->label);
#endif
	printStdObjPayload(obj);
	break;

    case THUNK:
    case THUNK_1_0: case THUNK_0_1:
    case THUNK_1_1: case THUNK_0_2: case THUNK_2_0:
    case THUNK_STATIC:
            /* ToDo: will this work for THUNK_STATIC too? */
#ifdef PROFILING
	    printThunkObject((StgThunk *)obj,info->prof.closure_desc);
#else
            printThunkObject((StgThunk *)obj,"THUNK");
#endif
            break;

    case THUNK_SELECTOR:
	printStdObjHdr(obj, "THUNK_SELECTOR");
	debugBelch(", %p)\n", ((StgSelector *)obj)->selectee);
	break;

    case BCO:
            disassemble( (StgBCO*)obj );
            break;

    case AP:
        {
	    StgAP* ap = stgCast(StgAP*,obj);
            StgWord i;
            debugBelch("AP("); printPtr((StgPtr)ap->fun);
            for (i = 0; i < ap->n_args; ++i) {
                debugBelch(", ");
                printPtr((P_)ap->payload[i]);
            }
            debugBelch(")\n");
            break;
        }

    case PAP:
        {
	    StgPAP* pap = stgCast(StgPAP*,obj);
            StgWord i;
            debugBelch("PAP/%d(",pap->arity); 
	    printPtr((StgPtr)pap->fun);
            for (i = 0; i < pap->n_args; ++i) {
                debugBelch(", ");
                printPtr((StgPtr)pap->payload[i]);
            }
            debugBelch(")\n");
            break;
        }

    case AP_STACK:
        {
	    StgAP_STACK* ap = stgCast(StgAP_STACK*,obj);
            StgWord i;
            debugBelch("AP_STACK("); printPtr((StgPtr)ap->fun);
            for (i = 0; i < ap->size; ++i) {
                debugBelch(", ");
                printPtr((P_)ap->payload[i]);
            }
            debugBelch(")\n");
            break;
        }

    case IND:
            debugBelch("IND("); 
            printPtr((StgPtr)stgCast(StgInd*,obj)->indirectee);
            debugBelch(")\n"); 
            break;

    case IND_OLDGEN:
            debugBelch("IND_OLDGEN("); 
            printPtr((StgPtr)stgCast(StgInd*,obj)->indirectee);
            debugBelch(")\n"); 
            break;

    case IND_PERM:
            debugBelch("IND("); 
            printPtr((StgPtr)stgCast(StgInd*,obj)->indirectee);
            debugBelch(")\n"); 
            break;

    case IND_OLDGEN_PERM:
            debugBelch("IND_OLDGEN_PERM("); 
            printPtr((StgPtr)stgCast(StgInd*,obj)->indirectee);
            debugBelch(")\n"); 
            break;

    case IND_STATIC:
            debugBelch("IND_STATIC("); 
            printPtr((StgPtr)stgCast(StgInd*,obj)->indirectee);
            debugBelch(")\n"); 
            break;

    /* Cannot happen -- use default case.
    case RET_BCO:
    case RET_SMALL:
    case RET_VEC_SMALL:
    case RET_BIG:
    case RET_VEC_BIG:
    case RET_DYN:
    case RET_FUN:
    */

    case UPDATE_FRAME:
        {
            StgUpdateFrame* u = stgCast(StgUpdateFrame*,obj);
            debugBelch("UPDATE_FRAME(");
            printPtr((StgPtr)GET_INFO(u));
            debugBelch(",");
            printPtr((StgPtr)u->updatee);
            debugBelch(")\n"); 
            break;
        }

    case CATCH_FRAME:
        {
            StgCatchFrame* u = stgCast(StgCatchFrame*,obj);
            debugBelch("CATCH_FRAME(");
            printPtr((StgPtr)GET_INFO(u));
            debugBelch(",");
            printPtr((StgPtr)u->handler);
            debugBelch(")\n"); 
            break;
        }

    case STOP_FRAME:
        {
            StgStopFrame* u = stgCast(StgStopFrame*,obj);
            debugBelch("STOP_FRAME(");
            printPtr((StgPtr)GET_INFO(u));
            debugBelch(")\n"); 
            break;
        }

    case CAF_BLACKHOLE:
            debugBelch("CAF_BH"); 
            break;

    case BLACKHOLE:
            debugBelch("BH\n"); 
            break;

    case SE_BLACKHOLE:
            debugBelch("SE_BH\n"); 
            break;

    case SE_CAF_BLACKHOLE:
            debugBelch("SE_CAF_BH\n"); 
            break;

    case ARR_WORDS:
        {
            StgWord i;
            debugBelch("ARR_WORDS(\"");
            /* ToDo: we can't safely assume that this is a string! 
            for (i = 0; arrWordsGetChar(obj,i); ++i) {
                putchar(arrWordsGetChar(obj,i));
		} */
	    for (i=0; i<((StgArrWords *)obj)->words; i++)
	      debugBelch("%lu", (lnat)((StgArrWords *)obj)->payload[i]);
            debugBelch("\")\n");
            break;
        }

    case MUT_ARR_PTRS_CLEAN:
	debugBelch("MUT_ARR_PTRS_CLEAN(size=%lu)\n", (lnat)((StgMutArrPtrs *)obj)->ptrs);
	break;

    case MUT_ARR_PTRS_DIRTY:
	debugBelch("MUT_ARR_PTRS_DIRTY(size=%lu)\n", (lnat)((StgMutArrPtrs *)obj)->ptrs);
	break;

    case MUT_ARR_PTRS_FROZEN:
	debugBelch("MUT_ARR_PTRS_FROZEN(size=%lu)\n", (lnat)((StgMutArrPtrs *)obj)->ptrs);
	break;

    case MVAR:
        {
	  StgMVar* mv = (StgMVar*)obj;
	  debugBelch("MVAR(head=%p, tail=%p, value=%p)\n", mv->head, mv->tail, mv->value);
          break;
        }

    case MUT_VAR:
        {
	  StgMutVar* mv = (StgMutVar*)obj;
	  debugBelch("MUT_VAR(var=%p)\n", mv->var);
          break;
        }

    case WEAK:
            debugBelch("WEAK("); 
	    debugBelch(" key=%p value=%p finalizer=%p", 
		    (StgPtr)(((StgWeak*)obj)->key),
		    (StgPtr)(((StgWeak*)obj)->value),
		    (StgPtr)(((StgWeak*)obj)->finalizer));
            debugBelch(")\n"); 
	    /* ToDo: chase 'link' ? */
            break;

    case STABLE_NAME:
            debugBelch("STABLE_NAME(%lu)\n", (lnat)((StgStableName*)obj)->sn); 
            break;

    case TSO:
      debugBelch("TSO("); 
      debugBelch("%d (%p)",((StgTSO*)obj)->id, (StgTSO*)obj);
      debugBelch(")\n"); 
      break;

#if defined(PAR)
    case BLOCKED_FETCH:
      debugBelch("BLOCKED_FETCH("); 
      printGA(&(stgCast(StgBlockedFetch*,obj)->ga));
      printPtr((StgPtr)(stgCast(StgBlockedFetch*,obj)->node));
      debugBelch(")\n"); 
      break;

    case FETCH_ME:
      debugBelch("FETCH_ME("); 
      printGA((globalAddr *)stgCast(StgFetchMe*,obj)->ga);
      debugBelch(")\n"); 
      break;

    case FETCH_ME_BQ:
      debugBelch("FETCH_ME_BQ("); 
      // printGA((globalAddr *)stgCast(StgFetchMe*,obj)->ga);
      printPtr((StgPtr)stgCast(StgFetchMeBlockingQueue*,obj)->blocking_queue);
      debugBelch(")\n"); 
      break;
#endif

#if defined(GRAN) || defined(PAR)
    case RBH:
      debugBelch("RBH("); 
      printPtr((StgPtr)stgCast(StgRBH*,obj)->blocking_queue);
      debugBelch(")\n"); 
      break;

#endif

#if 0
      /* Symptomatic of a problem elsewhere, have it fall-through & fail */
    case EVACUATED:
      debugBelch("EVACUATED("); 
      printClosure((StgEvacuated*)obj->evacuee);
      debugBelch(")\n"); 
      break;
#endif

#if defined(PAR) && defined(DIST)
    case REMOTE_REF:
      debugBelch("REMOTE_REF("); 
      printGA((globalAddr *)stgCast(StgFetchMe*,obj)->ga);
      debugBelch(")\n"); 
      break;
#endif

    default:
            //barf("printClosure %d",get_itbl(obj)->type);
            debugBelch("*** printClosure: unknown type %d ****\n",
                    get_itbl(obj)->type );
            barf("printClosure %d",get_itbl(obj)->type);
            return;
    }
}

/*
void printGraph( StgClosure *obj )
{
 printClosure(obj);
}
*/

StgPtr
printStackObj( StgPtr sp )
{
    /*debugBelch("Stack[%d] = ", &stgStack[STACK_SIZE] - sp); */

        StgClosure* c = (StgClosure*)(*sp);
        printPtr((StgPtr)*sp);
        if (c == (StgClosure*)&stg_ctoi_R1p_info) {
           debugBelch("\t\t\tstg_ctoi_ret_R1p_info\n" );
	} else
        if (c == (StgClosure*)&stg_ctoi_R1n_info) {
           debugBelch("\t\t\tstg_ctoi_ret_R1n_info\n" );
	} else
        if (c == (StgClosure*)&stg_ctoi_F1_info) {
           debugBelch("\t\t\tstg_ctoi_ret_F1_info\n" );
	} else
        if (c == (StgClosure*)&stg_ctoi_D1_info) {
           debugBelch("\t\t\tstg_ctoi_ret_D1_info\n" );
	} else
        if (c == (StgClosure*)&stg_ctoi_V_info) {
           debugBelch("\t\t\tstg_ctoi_ret_V_info\n" );
	} else
        if (get_itbl(c)->type == BCO) {
           debugBelch("\t\t\t");
           debugBelch("BCO(...)\n"); 
        }
        else {
           debugBelch("\t\t\t");
           printClosure ( (StgClosure*)(*sp));
        }
        sp += 1;

    return sp;
    
}

static void
printSmallBitmap( StgPtr spBottom, StgPtr payload, StgWord bitmap, nat size )
{
    StgPtr p;
    nat i;

    p = payload;
    for(i = 0; i < size; i++, bitmap >>= 1 ) {
	debugBelch("   stk[%ld] (%p) = ", (long)(spBottom-(payload+i)), payload+i);
	if ((bitmap & 1) == 0) {
	    printPtr((P_)payload[i]);
	    debugBelch("\n");
	} else {
	    debugBelch("Word# %lu\n", (lnat)payload[i]);
	}
    }
}

static void
printLargeBitmap( StgPtr spBottom, StgPtr payload, StgLargeBitmap* large_bitmap, nat size )
{
    StgWord bmp;
    nat i, j;

    i = 0;
    for (bmp=0; i < size; bmp++) {
	StgWord bitmap = large_bitmap->bitmap[bmp];
	j = 0;
	for(; i < size && j < BITS_IN(W_); j++, i++, bitmap >>= 1 ) {
	    debugBelch("   stk[%lu] (%p) = ", (lnat)(spBottom-(payload+i)), payload+i);
	    if ((bitmap & 1) == 0) {
		printPtr((P_)payload[i]);
		debugBelch("\n");
	    } else {
		debugBelch("Word# %lu\n", (lnat)payload[i]);
	    }
	}
    }
}

void
printStackChunk( StgPtr sp, StgPtr spBottom )
{
    StgWord bitmap;
    const StgInfoTable *info;

    ASSERT(sp <= spBottom);
    for (; sp < spBottom; sp += stack_frame_sizeW((StgClosure *)sp)) {

	info = get_itbl((StgClosure *)sp);

	switch (info->type) {
	    
	case UPDATE_FRAME:
	case CATCH_FRAME:
	case STOP_FRAME:
	    printObj((StgClosure*)sp);
	    continue;

	case RET_DYN:
	{ 
	    StgRetDyn* r;
	    StgPtr p;
	    StgWord dyn;
	    nat size;

	    r = (StgRetDyn *)sp;
	    dyn = r->liveness;
	    debugBelch("RET_DYN (%p)\n", r);

	    p = (P_)(r->payload);
	    printSmallBitmap(spBottom, sp,
			     RET_DYN_LIVENESS(r->liveness), 
			     RET_DYN_BITMAP_SIZE);
	    p += RET_DYN_BITMAP_SIZE + RET_DYN_NONPTR_REGS_SIZE;

	    for (size = RET_DYN_NONPTRS(dyn); size > 0; size--) {
		debugBelch("   stk[%ld] (%p) = ", (long)(spBottom-p), p);
		debugBelch("Word# %ld\n", (long)*p);
		p++;
	    }
	
	    for (size = RET_DYN_PTRS(dyn); size > 0; size--) {
		debugBelch("   stk[%ld] (%p) = ", (long)(spBottom-p), p);
		printPtr(p);
		p++;
	    }
	    continue;
	}

	case RET_SMALL:
	case RET_VEC_SMALL:
	    debugBelch("RET_SMALL (%p)\n", sp);
	    bitmap = info->layout.bitmap;
	    printSmallBitmap(spBottom, sp+1, 
			     BITMAP_BITS(bitmap), BITMAP_SIZE(bitmap));
	    continue;

	case RET_BCO: {
	    StgBCO *bco;
	    
	    bco = ((StgBCO *)sp[1]);

	    debugBelch("RET_BCO (%p)\n", sp);
	    printLargeBitmap(spBottom, sp+2,
			     BCO_BITMAP(bco), BCO_BITMAP_SIZE(bco));
	    continue;
	}

	case RET_BIG:
	case RET_VEC_BIG:
	    barf("todo");

	case RET_FUN:
	{
	    StgFunInfoTable *fun_info;
	    StgRetFun *ret_fun;
	    nat size;

	    ret_fun = (StgRetFun *)sp;
	    fun_info = get_fun_itbl(ret_fun->fun);
	    size = ret_fun->size;
	    debugBelch("RET_FUN (%p) (type=%d)\n", ret_fun, fun_info->f.fun_type);
	    switch (fun_info->f.fun_type) {
	    case ARG_GEN:
		printSmallBitmap(spBottom, sp+1,
				 BITMAP_BITS(fun_info->f.b.bitmap),
				 BITMAP_SIZE(fun_info->f.b.bitmap));
		break;
	    case ARG_GEN_BIG:
		printLargeBitmap(spBottom, sp+2,
				 GET_FUN_LARGE_BITMAP(fun_info),
				 GET_FUN_LARGE_BITMAP(fun_info)->size);
		break;
	    default:
		printSmallBitmap(spBottom, sp+1,
				 BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]),
				 BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]));
		break;
	    }
	    continue;
	}
	   
	default:
	    debugBelch("unknown object %d\n", info->type);
	    barf("printStackChunk");
	}
    }
}

void printTSO( StgTSO *tso )
{
    printStackChunk( tso->sp, tso->stack+tso->stack_size);
}

/* -----------------------------------------------------------------------------
   Closure types
   
   NOTE: must be kept in sync with the closure types in includes/ClosureTypes.h
   -------------------------------------------------------------------------- */

static char *closure_type_names[] = {
    "INVALID_OBJECT",
    "CONSTR",
    "CONSTR_1",
    "CONSTR_0",
    "CONSTR_2",
    "CONSTR_1",
    "CONSTR_0",
    "CONSTR_INTLIKE",
    "CONSTR_CHARLIKE",
    "CONSTR_STATIC",
    "CONSTR_NOCAF_STATIC",
    "FUN",
    "FUN_1_0",
    "FUN_0_1",
    "FUN_2_0",
    "FUN_1_1",
    "FUN_0",
    "FUN_STATIC",
    "THUNK",
    "THUNK_1_0",
    "THUNK_0_1",
    "THUNK_2_0",
    "THUNK_1_1",
    "THUNK_0",
    "THUNK_STATIC",
    "THUNK_SELECTOR",
    "BCO",
    "AP_UPD",
    "PAP",
    "AP_STACK",
    "IND",
    "IND_OLDGEN",
    "IND_PERM",
    "IND_OLDGEN_PERM",
    "IND_STATIC",
    "RET_BCO",
    "RET_SMALL",
    "RET_VEC_SMALL",
    "RET_BIG",
    "RET_VEC_BIG",
    "RET_DYN",
    "RET_FUN",
    "UPDATE_FRAME",
    "CATCH_FRAME",
    "STOP_FRAME",
    "CAF_BLACKHOLE",
    "BLACKHOLE",
    "BLACKHOLE_BQ",
    "SE_BLACKHOLE",
    "SE_CAF_BLACKHOLE",
    "MVAR",
    "ARR_WORDS",
    "MUT_ARR_PTRS",
    "MUT_ARR_PTRS_FROZEN",
    "MUT_VAR",
    "MUT_CONS",
    "WEAK",
    "FOREIGN",
    "STABLE_NAME",
    "TSO",
    "BLOCKED_FETCH",
    "FETCH_ME",
    "FETCH_ME_BQ",
    "RBH",
    "EVACUATED",
    "REMOTE_REF",
    "TVAR_WAIT_QUEUE",
    "TVAR",
    "TREC_CHUNK",
    "TREC_HEADER",
    "ATOMICALLY_FRAME",
    "CATCH_RETRY_FRAME"
};


char *
info_type(StgClosure *closure){ 
  return closure_type_names[get_itbl(closure)->type];
}

char *
info_type_by_ip(StgInfoTable *ip){ 
  return closure_type_names[ip->type];
}

void
info_hdr_type(StgClosure *closure, char *res){ 
  strcpy(res,closure_type_names[get_itbl(closure)->type]);
}

/* --------------------------------------------------------------------------
 * Address printing code
 *
 * Uses symbol table in (unstripped executable)
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Simple lookup table
 *
 * Current implementation is pretty dumb!
 * ------------------------------------------------------------------------*/

struct entry {
    nat value;
    const char *name;
};

static nat table_size;
static struct entry* table;

#ifdef USING_LIBBFD
static nat max_table_size;

static void reset_table( int size )
{
    max_table_size = size;
    table_size = 0;
    table = (struct entry *)stgMallocBytes(size * sizeof(struct entry), "Printer.c:reset_table()");
}

static void prepare_table( void )
{
    /* Could sort it...  */
}

static void insert( unsigned value, const char *name )
{
    if ( table_size >= max_table_size ) {
        barf( "Symbol table overflow\n" );
    }
    table[table_size].value = value;
    table[table_size].name = name;
    table_size = table_size + 1;
}
#endif

#if 0
static rtsBool lookup_name( char *name, unsigned *result )
{
    int i;
    for( i = 0; i < table_size && strcmp(name,table[i].name) != 0; ++i ) {
    }
    if (i < table_size) {
        *result = table[i].value;
        return rtsTrue;
    } else {
        return rtsFalse;
    }
}
#endif

/* Code from somewhere inside GHC (circa 1994)
 * * Z-escapes:
 *     "std"++xs -> "Zstd"++xs
 *     char_to_c 'Z'  = "ZZ"
 *     char_to_c '&'  = "Za"
 *     char_to_c '|'  = "Zb"
 *     char_to_c ':'  = "Zc"
 *     char_to_c '/'  = "Zd"
 *     char_to_c '='  = "Ze"
 *     char_to_c '>'  = "Zg"
 *     char_to_c '#'  = "Zh"
 *     char_to_c '<'  = "Zl"
 *     char_to_c '-'  = "Zm"
 *     char_to_c '!'  = "Zn"
 *     char_to_c '.'  = "Zo"
 *     char_to_c '+'  = "Zp"
 *     char_to_c '\'' = "Zq"
 *     char_to_c '*'  = "Zt"
 *     char_to_c '_'  = "Zu"
 *     char_to_c c    = "Z" ++ show (ord c)
 */
static char unZcode( char ch )
{
    switch (ch) {
    case 'a'  : return ('&');
    case 'b'  : return ('|');
    case 'c'  : return (':');
    case 'd'  : return ('/');
    case 'e'  : return ('=');
    case 'g'  : return ('>');
    case 'h'  : return ('#');
    case 'l'  : return ('<');
    case 'm'  : return ('-');
    case 'n'  : return ('!');
    case 'o'  : return ('.');
    case 'p'  : return ('+');
    case 'q'  : return ('\'');
    case 't'  : return ('*');
    case 'u'  : return ('_');
    case 'Z'  :
    case '\0' : return ('Z');
    default   : return (ch);
    }
}

#if 0
/* Precondition: out big enough to handle output (about twice length of in) */
static void enZcode( char *in, char *out )
{
    int i, j;

    j = 0;
    out[ j++ ] = '_';
    for( i = 0; in[i] != '\0'; ++i ) {
        switch (in[i]) {
        case 'Z'  : 
                out[j++] = 'Z';
                out[j++] = 'Z';
                break;
        case '&'  : 
                out[j++] = 'Z';
                out[j++] = 'a';
                break;
        case '|'  : 
                out[j++] = 'Z';
                out[j++] = 'b';
                break;
        case ':'  : 
                out[j++] = 'Z';
                out[j++] = 'c';
                break;
        case '/'  : 
                out[j++] = 'Z';
                out[j++] = 'd';
                break;
        case '='  : 
                out[j++] = 'Z';
                out[j++] = 'e';
                break;
        case '>'  : 
                out[j++] = 'Z';
                out[j++] = 'g';
                break;
        case '#'  : 
                out[j++] = 'Z';
                out[j++] = 'h';
                break;
        case '<'  : 
                out[j++] = 'Z';
                out[j++] = 'l';
                break;
        case '-'  : 
                out[j++] = 'Z';
                out[j++] = 'm';
                break;
        case '!'  : 
                out[j++] = 'Z';
                out[j++] = 'n';
                break;
        case '.'  : 
                out[j++] = 'Z';
                out[j++] = 'o';
                break;
        case '+'  : 
                out[j++] = 'Z';
                out[j++] = 'p';
                break;
        case '\'' : 
                out[j++] = 'Z';
                out[j++] = 'q';
                break;
        case '*'  : 
                out[j++] = 'Z';
                out[j++] = 't';
                break;
        case '_'  : 
                out[j++] = 'Z';
                out[j++] = 'u';
                break;
        default :
                out[j++] = in[i];
                break;
        }
    }
    out[j] = '\0';
}
#endif

const char *lookupGHCName( void *addr )
{
    nat i;
    for( i = 0; i < table_size && table[i].value != (unsigned) addr; ++i ) {
    }
    if (i < table_size) {
        return table[i].name;
    } else {
        return NULL;
    }
}

static void printZcoded( const char *raw )
{
    nat j = 0;
    
    while ( raw[j] != '\0' ) {
        if (raw[j] == 'Z') {
            debugBelch("%c", unZcode(raw[j+1]));
            j = j + 2;
        } else {
            debugBelch("%c", unZcode(raw[j+1]));
            j = j + 1;
        }
    }
}

/* --------------------------------------------------------------------------
 * Symbol table loading
 * ------------------------------------------------------------------------*/

/* Causing linking trouble on Win32 plats, so I'm
   disabling this for now. 
*/
#ifdef USING_LIBBFD

#include <bfd.h>

/* Fairly ad-hoc piece of code that seems to filter out a lot of
 * rubbish like the obj-splitting symbols
 */

static rtsBool isReal( flagword flags STG_UNUSED, const char *name )
{
#if 0
    /* ToDo: make this work on BFD */
    int tp = type & N_TYPE;    
    if (tp == N_TEXT || tp == N_DATA) {
        return (name[0] == '_' && name[1] != '_');
    } else {
        return rtsFalse;
    }
#else
    if (*name == '\0'  || 
	(name[0] == 'g' && name[1] == 'c' && name[2] == 'c') ||
	(name[0] == 'c' && name[1] == 'c' && name[2] == '.')) {
	return rtsFalse;
    }
    return rtsTrue;
#endif
}

extern void DEBUG_LoadSymbols( char *name )
{
    bfd* abfd;
    char **matching;

    bfd_init();
    abfd = bfd_openr(name, "default");
    if (abfd == NULL) {
	barf("can't open executable %s to get symbol table", name);
    }
    if (!bfd_check_format_matches (abfd, bfd_object, &matching)) {
	barf("mismatch");
    }

    {
	long storage_needed;
	asymbol **symbol_table;
	long number_of_symbols;
        long num_real_syms = 0;
	long i;
     
	storage_needed = bfd_get_symtab_upper_bound (abfd);
     
	if (storage_needed < 0) {
	    barf("can't read symbol table");
	}     
#if 0
	if (storage_needed == 0) {
	    debugBelch("no storage needed");
	}
#endif
	symbol_table = (asymbol **) stgMallocBytes(storage_needed,"DEBUG_LoadSymbols");

	number_of_symbols = bfd_canonicalize_symtab (abfd, symbol_table);
     
	if (number_of_symbols < 0) {
	    barf("can't canonicalise symbol table");
	}

        for( i = 0; i != number_of_symbols; ++i ) {
            symbol_info info;
            bfd_get_symbol_info(abfd,symbol_table[i],&info);
            /*debugBelch("\t%c\t0x%x      \t%s\n",info.type,(nat)info.value,info.name); */
            if (isReal(info.type, info.name)) {
                num_real_syms += 1;
            }
        }
    
        IF_DEBUG(interpreter,
                 debugBelch("Loaded %ld symbols. Of which %ld are real symbols\n", 
                         number_of_symbols, num_real_syms)
                 );

        reset_table( num_real_syms );
    
        for( i = 0; i != number_of_symbols; ++i ) {
            symbol_info info;
            bfd_get_symbol_info(abfd,symbol_table[i],&info);
            if (isReal(info.type, info.name)) {
                insert( info.value, info.name );
            }
        }

        stgFree(symbol_table);
    }
    prepare_table();
}

#else /* HAVE_BFD_H */

extern void DEBUG_LoadSymbols( char *name STG_UNUSED )
{
  /* nothing, yet */
}

#endif /* HAVE_BFD_H */

void findPtr(P_ p, int);		/* keep gcc -Wall happy */

void
findPtr(P_ p, int follow)
{
  nat s, g;
  P_ q, r;
  bdescr *bd;
#if defined(__GNUC__)
  const int arr_size = 1024;
#else
#define arr_size 1024
#endif
  StgPtr arr[arr_size];
  int i = 0;

  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
      for (s = 0; s < generations[g].n_steps; s++) {
	  bd = generations[g].steps[s].blocks;
	  for (; bd; bd = bd->link) {
	      for (q = bd->start; q < bd->free; q++) {
		  if (*q == (W_)p) {
		      if (i < arr_size) {
			  r = q;
			  while (!LOOKS_LIKE_INFO_PTR(*r) || (P_)*r == NULL) {
			      r--;
			  }
			  debugBelch("%p = ", r);
			  printClosure((StgClosure *)r);
			  arr[i++] = r;
		      } else {
			  return;
		      }
		  }
	      }
	  }
      }
  }
  if (follow && i == 1) {
      debugBelch("-->\n");
      findPtr(arr[0], 1);
  }
}

#else /* DEBUG */
void printPtr( StgPtr p )
{
    debugBelch("ptr 0x%p (enable -DDEBUG for more info) " , p );
}
  
void printObj( StgClosure *obj )
{
    debugBelch("obj 0x%p (enable -DDEBUG for more info) " , obj );
}
#endif /* DEBUG */
