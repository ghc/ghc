/* -----------------------------------------------------------------------------
 * $Id: Printer.c,v 1.56 2003/03/25 17:23:05 sof Exp $
 *
 * (c) The GHC Team, 1994-2000.
 *
 * Heap printer
 * 
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "Printer.h"

#include <stdio.h>

#ifdef DEBUG

#include "RtsUtils.h"
#include "RtsFlags.h"
#include "MBlock.h"
#include "Storage.h"
#include "Bytecodes.h"  /* for InstrPtr */
#include "Disassembler.h"

#include <stdlib.h>
#include <string.h>

#if defined(GRAN) || defined(PAR)
// HWL: explicit fixed header size to make debugging easier
int fixed_hs = FIXED_HS, itbl_sz = sizeofW(StgInfoTable), 
    uf_sz=sizeofW(StgUpdateFrame); 
#endif

/* --------------------------------------------------------------------------
 * local function decls
 * ------------------------------------------------------------------------*/

static void    printStdObject( StgClosure *obj, char* tag );
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
        fprintf(stderr, "%p", p);
    }
}
  
void printObj( StgClosure *obj )
{
    fprintf(stderr,"Object "); printPtr((StgPtr)obj); fprintf(stderr," = ");
    printClosure(obj);
}

static inline void
printStdObjHdr( StgClosure *obj, char* tag )
{
    fprintf(stderr,"%s(",tag);
    printPtr((StgPtr)obj->header.info);
#ifdef PROFILING
    fprintf(stderr,", %s", obj->header.prof.ccs->cc->label);
#endif
}

static void
printStdObjPayload( StgClosure *obj )
{
    StgWord i, j;
    const StgInfoTable* info;

    info = get_itbl(obj);
    for (i = 0; i < info->layout.payload.ptrs; ++i) {
        fprintf(stderr,", ");
        printPtr((StgPtr)obj->payload[i]);
    }
    for (j = 0; j < info->layout.payload.nptrs; ++j) {
        fprintf(stderr,", %pd#",obj->payload[i+j]);
    }
    fprintf(stderr,")\n");
}

static void
printStdObject( StgClosure *obj, char* tag )
{
    printStdObjHdr( obj, tag );
    printStdObjPayload( obj );
}

void
printClosure( StgClosure *obj )
{
    StgInfoTable *info;
    
    info = get_itbl(obj);

    switch ( info->type ) {
    case INVALID_OBJECT:
            barf("Invalid object");
    case BCO:
            disassemble( (StgBCO*)obj );
            break;

    case MUT_VAR:
        {
	  StgMutVar* mv = (StgMutVar*)obj;
	  fprintf(stderr,"MUT_VAR(var=%p, link=%p)\n", mv->var, mv->mut_link);
          break;
        }

    case AP_STACK:
        {
	    StgAP_STACK* ap = stgCast(StgAP_STACK*,obj);
            StgWord i;
            fprintf(stderr,"AP_STACK("); printPtr((StgPtr)ap->fun);
            for (i = 0; i < ap->size; ++i) {
                fprintf(stderr,", ");
                printPtr((P_)ap->payload[i]);
            }
            fprintf(stderr,")\n");
            break;
        }

    case AP:
        {
	    StgPAP* ap = stgCast(StgPAP*,obj);
            StgWord i;
            fprintf(stderr,"AP("); printPtr((StgPtr)ap->fun);
            for (i = 0; i < ap->n_args; ++i) {
                fprintf(stderr,", ");
                printPtr((P_)ap->payload[i]);
            }
            fprintf(stderr,")\n");
            break;
        }

    case PAP:
        {
	    StgPAP* pap = stgCast(StgPAP*,obj);
            StgWord i;
            fprintf(stderr,"PAP/%d(",pap->arity); 
	    printPtr((StgPtr)pap->fun);
            for (i = 0; i < pap->n_args; ++i) {
                fprintf(stderr,", ");
                printPtr((StgPtr)pap->payload[i]);
            }
            fprintf(stderr,")\n");
            break;
        }

    case FOREIGN:
            fprintf(stderr,"FOREIGN("); 
            printPtr((StgPtr)( ((StgForeignObj*)obj)->data ));
            fprintf(stderr,")\n"); 
            break;

    case IND:
            fprintf(stderr,"IND("); 
            printPtr((StgPtr)stgCast(StgInd*,obj)->indirectee);
            fprintf(stderr,")\n"); 
            break;

    case IND_PERM:
            fprintf(stderr,"IND("); 
            printPtr((StgPtr)stgCast(StgInd*,obj)->indirectee);
            fprintf(stderr,")\n"); 
            break;

    case IND_STATIC:
            fprintf(stderr,"IND_STATIC("); 
            printPtr((StgPtr)stgCast(StgInd*,obj)->indirectee);
            fprintf(stderr,")\n"); 
            break;

    case IND_OLDGEN:
            fprintf(stderr,"IND_OLDGEN("); 
            printPtr((StgPtr)stgCast(StgInd*,obj)->indirectee);
            fprintf(stderr,")\n"); 
            break;

    case CAF_BLACKHOLE:
            fprintf(stderr,"CAF_BH("); 
            printPtr((StgPtr)stgCast(StgBlockingQueue*,obj)->blocking_queue);
            fprintf(stderr,")\n"); 
            break;

    case SE_BLACKHOLE:
            fprintf(stderr,"SE_BH\n"); 
            break;

    case SE_CAF_BLACKHOLE:
            fprintf(stderr,"SE_CAF_BH\n"); 
            break;

    case BLACKHOLE:
            fprintf(stderr,"BH\n"); 
            break;

    case BLACKHOLE_BQ:
            fprintf(stderr,"BQ("); 
            printPtr((StgPtr)stgCast(StgBlockingQueue*,obj)->blocking_queue);
            fprintf(stderr,")\n"); 
            break;

    case TSO:
      fprintf(stderr,"TSO("); 
      fprintf(stderr,"%d (%p)",((StgTSO*)obj)->id, (StgTSO*)obj);
      fprintf(stderr,")\n"); 
      break;

#if defined(PAR)
    case BLOCKED_FETCH:
      fprintf(stderr,"BLOCKED_FETCH("); 
      printGA(&(stgCast(StgBlockedFetch*,obj)->ga));
      printPtr((StgPtr)(stgCast(StgBlockedFetch*,obj)->node));
      fprintf(stderr,")\n"); 
      break;

    case FETCH_ME:
      fprintf(stderr,"FETCH_ME("); 
      printGA((globalAddr *)stgCast(StgFetchMe*,obj)->ga);
      fprintf(stderr,")\n"); 
      break;

#ifdef DIST      
    case REMOTE_REF:
      fprintf(stderr,"REMOTE_REF("); 
      printGA((globalAddr *)stgCast(StgFetchMe*,obj)->ga);
      fprintf(stderr,")\n"); 
      break;
#endif
  
    case FETCH_ME_BQ:
      fprintf(stderr,"FETCH_ME_BQ("); 
      // printGA((globalAddr *)stgCast(StgFetchMe*,obj)->ga);
      printPtr((StgPtr)stgCast(StgFetchMeBlockingQueue*,obj)->blocking_queue);
      fprintf(stderr,")\n"); 
      break;
#endif
#if defined(GRAN) || defined(PAR)
    case RBH:
      fprintf(stderr,"RBH("); 
      printPtr((StgPtr)stgCast(StgRBH*,obj)->blocking_queue);
      fprintf(stderr,")\n"); 
      break;

#endif

    case CONSTR:
    case CONSTR_1_0: case CONSTR_0_1:
    case CONSTR_1_1: case CONSTR_0_2: case CONSTR_2_0:
    case CONSTR_INTLIKE:
    case CONSTR_CHARLIKE:
    case CONSTR_STATIC:
    case CONSTR_NOCAF_STATIC:
        {
            /* We can't use printStdObject because we want to print the
             * tag as well.
	     */
            StgWord i, j;
#ifdef PROFILING
	    fprintf(stderr,"%s(", info->prof.closure_desc);
	    fprintf(stderr,"%s", obj->header.prof.ccs->cc->label);
#else
            fprintf(stderr,"CONSTR(");
            printPtr((StgPtr)obj->header.info);
            fprintf(stderr,"(tag=%d)",info->srt_len);
#endif
            for (i = 0; i < info->layout.payload.ptrs; ++i) {
		fprintf(stderr,", ");
                printPtr((StgPtr)obj->payload[i]);
            }
            for (j = 0; j < info->layout.payload.nptrs; ++j) {
                fprintf(stderr,", %p#", obj->payload[i+j]);
            }
            fprintf(stderr,")\n");
            break;
        }

#ifdef XMLAMBDA
/* rows are mutarrays in xmlambda, maybe we should make a new type: ROW */
    case MUT_ARR_PTRS_FROZEN:
          {
            StgWord i;
            StgMutArrPtrs* p = stgCast(StgMutArrPtrs*,obj);

            fprintf(stderr,"Row<%i>(",p->ptrs);
            for (i = 0; i < p->ptrs; ++i) {
                if (i > 0) fprintf(stderr,", ");
                printPtr((StgPtr)(p->payload[i]));
            }
            fprintf(stderr,")\n");
            break;
          }
#endif  

    case FUN:
    case FUN_1_0: case FUN_0_1: 
    case FUN_1_1: case FUN_0_2: case FUN_2_0:
    case FUN_STATIC:
	fprintf(stderr,"FUN/%d(",itbl_to_fun_itbl(info)->arity);
	printPtr((StgPtr)obj->header.info);
#ifdef PROFILING
	fprintf(stderr,", %s", obj->header.prof.ccs->cc->label);
#endif
	printStdObjPayload(obj);
	break;

    case THUNK:
    case THUNK_1_0: case THUNK_0_1:
    case THUNK_1_1: case THUNK_0_2: case THUNK_2_0:
    case THUNK_STATIC:
            /* ToDo: will this work for THUNK_STATIC too? */
#ifdef PROFILING
	    printStdObject(obj,info->prof.closure_desc);
#else
            printStdObject(obj,"THUNK");
#endif
            break;

    case THUNK_SELECTOR:
	printStdObjHdr(obj, "THUNK_SELECTOR");
	fprintf(stderr, ", %p)\n", ((StgSelector *)obj)->selectee);
	break;

    case MUT_ARR_PTRS:
	fprintf(stderr,"MUT_ARR_PTRS(size=%d)\n", ((StgMutArrPtrs *)obj)->ptrs);
	break;
    case MUT_ARR_PTRS_FROZEN:
	fprintf(stderr,"MUT_ARR_PTRS_FROZEN(size=%d)\n", ((StgMutArrPtrs *)obj)->ptrs);
	break;

    case ARR_WORDS:
        {
            StgWord i;
            fprintf(stderr,"ARR_WORDS(\"");
            /* ToDo: we can't safely assume that this is a string! 
            for (i = 0; arrWordsGetChar(obj,i); ++i) {
                putchar(arrWordsGetChar(obj,i));
		} */
	    for (i=0; i<((StgArrWords *)obj)->words; i++)
	      fprintf(stderr, "%u", ((StgArrWords *)obj)->payload[i]);
            fprintf(stderr,"\")\n");
            break;
        }

    case UPDATE_FRAME:
        {
            StgUpdateFrame* u = stgCast(StgUpdateFrame*,obj);
            fprintf(stderr,"UPDATE_FRAME(");
            printPtr((StgPtr)GET_INFO(u));
            fprintf(stderr,",");
            printPtr((StgPtr)u->updatee);
            fprintf(stderr,")\n"); 
            break;
        }

    case CATCH_FRAME:
        {
            StgCatchFrame* u = stgCast(StgCatchFrame*,obj);
            fprintf(stderr,"CATCH_FRAME(");
            printPtr((StgPtr)GET_INFO(u));
            fprintf(stderr,",");
            printPtr((StgPtr)u->handler);
            fprintf(stderr,")\n"); 
            break;
        }

    case STOP_FRAME:
        {
            StgStopFrame* u = stgCast(StgStopFrame*,obj);
            fprintf(stderr,"STOP_FRAME(");
            printPtr((StgPtr)GET_INFO(u));
            fprintf(stderr,")\n"); 
            break;
        }
    default:
            //barf("printClosure %d",get_itbl(obj)->type);
            fprintf(stderr, "*** printClosure: unknown type %d ****\n",
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
    /*fprintf(stderr,"Stack[%d] = ", &stgStack[STACK_SIZE] - sp); */

        StgClosure* c = (StgClosure*)(*sp);
        printPtr((StgPtr)*sp);
        if (c == (StgClosure*)&stg_ctoi_ret_R1p_info) {
           fprintf(stderr, "\t\t\tstg_ctoi_ret_R1p_info\n" );
	} else
        if (c == (StgClosure*)&stg_ctoi_ret_R1n_info) {
           fprintf(stderr, "\t\t\tstg_ctoi_ret_R1n_info\n" );
	} else
        if (c == (StgClosure*)&stg_ctoi_ret_F1_info) {
           fprintf(stderr, "\t\t\tstg_ctoi_ret_F1_info\n" );
	} else
        if (c == (StgClosure*)&stg_ctoi_ret_D1_info) {
           fprintf(stderr, "\t\t\tstg_ctoi_ret_D1_info\n" );
	} else
        if (c == (StgClosure*)&stg_ctoi_ret_V_info) {
           fprintf(stderr, "\t\t\tstg_ctoi_ret_V_info\n" );
	} else
        if (get_itbl(c)->type == BCO) {
           fprintf(stderr, "\t\t\t");
           fprintf(stderr, "BCO(...)\n"); 
        }
        else {
           fprintf(stderr, "\t\t\t");
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
	fprintf(stderr,"   stk[%d] (%p) = ", spBottom-(payload+i), payload+i);
	if ((bitmap & 1) == 0) {
	    printPtr((P_)payload[i]);
	    fprintf(stderr,"\n");
	} else {
	    fprintf(stderr,"Word# %d\n", payload[i]);
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
	    fprintf(stderr,"   stk[%d] (%p) = ", spBottom-(payload+i), payload+i);
	    if ((bitmap & 1) == 0) {
		printPtr((P_)payload[i]);
		fprintf(stderr,"\n");
	    } else {
		fprintf(stderr,"Word# %d\n", payload[i]);
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
	    fprintf(stderr, "RET_DYN (%p)\n", r);

	    p = (P_)(r->payload);
	    printSmallBitmap(spBottom, sp,
			     GET_LIVENESS(r->liveness), RET_DYN_SIZE);
	    p += RET_DYN_SIZE;

	    for (size = GET_NONPTRS(dyn); size > 0; size--) {
		fprintf(stderr,"   stk[%ld] (%p) = ", spBottom-p, p);
		fprintf(stderr,"Word# %ld\n", *p);
		p++;
	    }
	
	    for (size = GET_PTRS(dyn); size > 0; size--) {
		fprintf(stderr,"   stk[%ld] (%p) = ", spBottom-p, p);
		printPtr(p);
		p++;
	    }
	    continue;
	}

	case RET_SMALL:
	case RET_VEC_SMALL:
	    fprintf(stderr, "RET_SMALL (%p)\n", sp);
	    bitmap = info->layout.bitmap;
	    printSmallBitmap(spBottom, sp+1, 
			     BITMAP_BITS(bitmap), BITMAP_SIZE(bitmap));
	    continue;

	case RET_BCO: {
	    StgBCO *bco;
	    
	    bco = ((StgBCO *)sp[1]);

	    fprintf(stderr, "RET_BCO (%p)\n", sp);
	    printLargeBitmap(spBottom, sp+2,
			     BCO_BITMAP(bco), BCO_BITMAP_SIZE(bco));
	    continue;
	}

	case RET_BIG:
	case RET_VEC_BIG:
	    barf("todo");

	default:
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
    "REMOTE_REF"
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
            fputc(unZcode(raw[j+1]),stderr);
            j = j + 2;
        } else {
            fputc(raw[j],stderr);
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
	    belch("no storage needed");
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
            /*fprintf(stderr,"\t%c\t0x%x      \t%s\n",info.type,(nat)info.value,info.name); */
            if (isReal(info.type, info.name)) {
                num_real_syms += 1;
            }
        }
    
        IF_DEBUG(interpreter,
                 fprintf(stderr,"Loaded %ld symbols. Of which %ld are real symbols\n", 
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

        free(symbol_table);
    }
    prepare_table();
}

#else /* HAVE_BFD_H */

extern void DEBUG_LoadSymbols( char *name STG_UNUSED )
{
  /* nothing, yet */
}

#endif /* HAVE_BFD_H */

#include "StoragePriv.h"

void findPtr(P_ p, int);		/* keep gcc -Wall happy */

void
findPtr(P_ p, int follow)
{
  nat s, g;
  P_ q, r;
  bdescr *bd;
  const int arr_size = 1024;
  StgPtr arr[arr_size];
  int i = 0;

  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
      for (s = 0; s < generations[g].n_steps; s++) {
	  if (RtsFlags.GcFlags.generations == 1) {
	      bd = generations[g].steps[s].to_blocks;
	  } else {
	      bd = generations[g].steps[s].blocks;
	  }
	  for (; bd; bd = bd->link) {
	      for (q = bd->start; q < bd->free; q++) {
		  if (*q == (W_)p) {
		      if (i < arr_size) {
			  r = q;
			  while (!LOOKS_LIKE_INFO_PTR(*r) || (P_)*r == NULL) {
			      r--;
			  }
			  fprintf(stderr, "%p = ", r);
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
      fprintf(stderr, "-->\n");
      findPtr(arr[0], 1);
  }
}

#else /* DEBUG */
void printPtr( StgPtr p )
{
    fprintf(stderr, "ptr 0x%p (enable -DDEBUG for more info) " , p );
}
  
void printObj( StgClosure *obj )
{
    fprintf(stderr, "obj 0x%p (enable -DDEBUG for more info) " , obj );
}
#endif /* DEBUG */
