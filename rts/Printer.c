/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1994-2000.
 *
 * Heap printer
 * 
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "rts/Bytecodes.h"  /* for InstrPtr */

#include "sm/Storage.h"
#include "Printer.h"
#include "RtsUtils.h"

#include <string.h>

#ifdef DEBUG

#include "Disassembler.h"
#include "Apply.h"

/* --------------------------------------------------------------------------
 * local function decls
 * ------------------------------------------------------------------------*/

static void    printStdObjPayload( StgClosure *obj );
#ifdef USING_LIBBFD
static void    reset_table   ( int size );
static void    prepare_table ( void );
static void    insert        ( StgWord value, const char *name );
#endif
#if 0 /* unused but might be useful sometime */
static rtsBool lookup_name   ( char *name, StgWord *result );
static void    enZcode       ( char *in, char *out );
#endif
static char    unZcode       ( char ch );
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

    info = get_itbl((StgClosure *)obj);
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
    obj = UNTAG_CLOSURE(obj);

    StgInfoTable *info;
    info = get_itbl(obj);

    switch ( info->type ) {
    case INVALID_OBJECT:
            barf("Invalid object");

    case CONSTR:
    case CONSTR_1_0: case CONSTR_0_1:
    case CONSTR_1_1: case CONSTR_0_2: case CONSTR_2_0:
    case CONSTR_STATIC:
    case CONSTR_NOCAF_STATIC:
        {
            StgWord i, j;
            StgConInfoTable *con_info = get_con_itbl (obj);

            debugBelch("%s(", GET_CON_DESC(con_info));
            for (i = 0; i < info->layout.payload.ptrs; ++i) {
		if (i != 0) debugBelch(", ");
                printPtr((StgPtr)obj->payload[i]);
            }
            for (j = 0; j < info->layout.payload.nptrs; ++j) {
		if (i != 0 || j != 0) debugBelch(", ");
                debugBelch("%p#", obj->payload[i+j]);
            }
            debugBelch(")\n");
            break;
        }

    case FUN:
    case FUN_1_0: case FUN_0_1: 
    case FUN_1_1: case FUN_0_2: case FUN_2_0:
    case FUN_STATIC:
	debugBelch("FUN/%d(",(int)itbl_to_fun_itbl(info)->f.arity);
	printPtr((StgPtr)obj->header.info);
#ifdef PROFILING
	debugBelch(", %s", obj->header.prof.ccs->cc->label);
#endif
	printStdObjPayload(obj);
	break;

    case PRIM:
	debugBelch("PRIM(");
	printPtr((StgPtr)obj->header.info);
	printStdObjPayload(obj);
	break;

    case MUT_PRIM:
        debugBelch("MUT_PRIM(");
	printPtr((StgPtr)obj->header.info);
	printStdObjPayload(obj);
	break;

    case THUNK:
    case THUNK_1_0: case THUNK_0_1:
    case THUNK_1_1: case THUNK_0_2: case THUNK_2_0:
    case THUNK_STATIC:
            /* ToDo: will this work for THUNK_STATIC too? */
#ifdef PROFILING
            printThunkObject((StgThunk *)obj,GET_PROF_DESC(info));
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
	    StgAP* ap = (StgAP*)obj;
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
	    StgPAP* pap = (StgPAP*)obj;
            StgWord i;
            debugBelch("PAP/%d(",(int)pap->arity); 
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
	    StgAP_STACK* ap = (StgAP_STACK*)obj;
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
            printPtr((StgPtr)((StgInd*)obj)->indirectee);
            debugBelch(")\n"); 
            break;

    case IND_PERM:
            debugBelch("IND("); 
            printPtr((StgPtr)((StgInd*)obj)->indirectee);
            debugBelch(")\n"); 
            break;

    case IND_STATIC:
            debugBelch("IND_STATIC("); 
            printPtr((StgPtr)((StgInd*)obj)->indirectee);
            debugBelch(")\n"); 
            break;

    case BLACKHOLE:
            debugBelch("BLACKHOLE("); 
            printPtr((StgPtr)((StgInd*)obj)->indirectee);
            debugBelch(")\n"); 
            break;

    /* Cannot happen -- use default case.
    case RET_BCO:
    case RET_SMALL:
    case RET_BIG:
    case RET_FUN:
    */

    case UPDATE_FRAME:
        {
            StgUpdateFrame* u = (StgUpdateFrame*)obj;
            debugBelch("%s(", info_update_frame(obj));
            printPtr((StgPtr)GET_INFO((StgClosure *)u));
            debugBelch(",");
            printPtr((StgPtr)u->updatee);
            debugBelch(")\n"); 
            break;
        }

    case CATCH_FRAME:
        {
            StgCatchFrame* u = (StgCatchFrame*)obj;
            debugBelch("CATCH_FRAME(");
            printPtr((StgPtr)GET_INFO((StgClosure *)u));
            debugBelch(",");
            printPtr((StgPtr)u->handler);
            debugBelch(")\n"); 
            break;
        }

    case UNDERFLOW_FRAME:
        {
            StgUnderflowFrame* u = (StgUnderflowFrame*)obj;
            debugBelch("UNDERFLOW_FRAME(");
            printPtr((StgPtr)u->next_chunk);
            debugBelch(")\n"); 
            break;
        }

    case STOP_FRAME:
        {
            StgStopFrame* u = (StgStopFrame*)obj;
            debugBelch("STOP_FRAME(");
            printPtr((StgPtr)GET_INFO((StgClosure *)u));
            debugBelch(")\n"); 
            break;
        }

    case ARR_WORDS:
        {
            StgWord i;
            debugBelch("ARR_WORDS(\"");
	    for (i=0; i<arr_words_words((StgArrWords *)obj); i++)
	      debugBelch("%" FMT_Word, (W_)((StgArrWords *)obj)->payload[i]);
            debugBelch("\")\n");
            break;
        }

    case MUT_ARR_PTRS_CLEAN:
	debugBelch("MUT_ARR_PTRS_CLEAN(size=%" FMT_Word ")\n", (W_)((StgMutArrPtrs *)obj)->ptrs);
	break;

    case MUT_ARR_PTRS_DIRTY:
	debugBelch("MUT_ARR_PTRS_DIRTY(size=%" FMT_Word ")\n", (W_)((StgMutArrPtrs *)obj)->ptrs);
	break;

    case MUT_ARR_PTRS_FROZEN:
	debugBelch("MUT_ARR_PTRS_FROZEN(size=%" FMT_Word ")\n", (W_)((StgMutArrPtrs *)obj)->ptrs);
	break;

    case MVAR_CLEAN:
    case MVAR_DIRTY:
        {
	  StgMVar* mv = (StgMVar*)obj;
	  debugBelch("MVAR(head=%p, tail=%p, value=%p)\n", mv->head, mv->tail, mv->value);
          break;
        }

    case TVAR:
        {
          StgTVar* tv = (StgTVar*)obj;
          debugBelch("TVAR(value=%p, wq=%p, num_updates=%" FMT_Word ")\n", tv->current_value, tv->first_watch_queue_entry, tv->num_updates);
          break;
        }

    case MUT_VAR_CLEAN:
        {
	  StgMutVar* mv = (StgMutVar*)obj;
	  debugBelch("MUT_VAR_CLEAN(var=%p)\n", mv->var);
          break;
        }

    case MUT_VAR_DIRTY:
        {
	  StgMutVar* mv = (StgMutVar*)obj;
	  debugBelch("MUT_VAR_DIRTY(var=%p)\n", mv->var);
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

    case TSO:
      debugBelch("TSO("); 
      debugBelch("%lu (%p)",(unsigned long)(((StgTSO*)obj)->id), (StgTSO*)obj);
      debugBelch(")\n"); 
      break;

    case STACK:
      debugBelch("STACK");
      break;

#if 0
      /* Symptomatic of a problem elsewhere, have it fall-through & fail */
    case EVACUATED:
      debugBelch("EVACUATED("); 
      printClosure((StgEvacuated*)obj->evacuee);
      debugBelch(")\n"); 
      break;
#endif

    default:
            //barf("printClosure %d",get_itbl(obj)->type);
            debugBelch("*** printClosure: unknown type %d ****\n",
                    (int)get_itbl(obj)->type );
            barf("printClosure %d",get_itbl(obj)->type);
            return;
    }
}

// If you know you have an UPDATE_FRAME, but want to know exactly which.
char *info_update_frame(StgClosure *closure) {
    // Note: We intentionally don't take the info table pointer as
    // an argument. As it will be confusing whether one should pass
    // it pointing to the code or struct members when compiling with
    // TABLES_NEXT_TO_CODE.
    const StgInfoTable *info = closure->header.info;
    if (info == &stg_upd_frame_info) {
        return "NORMAL_UPDATE_FRAME";
    } else if (info == &stg_bh_upd_frame_info) {
        return "BH_UPDATE_FRAME";
    } else if (info == &stg_marked_upd_frame_info) {
        return "MARKED_UPDATE_FRAME";
    } else {
        return "ERROR: Not an update frame!!!";
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
    nat i;

    for(i = 0; i < size; i++, bitmap >>= 1 ) {
	debugBelch("   stk[%ld] (%p) = ", (long)(spBottom-(payload+i)), payload+i);
	if ((bitmap & 1) == 0) {
	    printPtr((P_)payload[i]);
	    debugBelch("\n");
	} else {
	    debugBelch("Word# %" FMT_Word "\n", (W_)payload[i]);
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
	    debugBelch("   stk[%" FMT_Word "] (%p) = ", (W_)(spBottom-(payload+i)), payload+i);
	    if ((bitmap & 1) == 0) {
		printPtr((P_)payload[i]);
		debugBelch("\n");
	    } else {
		debugBelch("Word# %" FMT_Word "\n", (W_)payload[i]);
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
        case UNDERFLOW_FRAME:
        case STOP_FRAME:
            printObj((StgClosure*)sp);
	    continue;

        case RET_SMALL:
	    debugBelch("RET_SMALL (%p)\n", info);
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
	    barf("todo");

	case RET_FUN:
	{
	    StgFunInfoTable *fun_info;
	    StgRetFun *ret_fun;

	    ret_fun = (StgRetFun *)sp;
	    fun_info = get_fun_itbl(ret_fun->fun);
	    debugBelch("RET_FUN (%p) (type=%d)\n", ret_fun->fun, (int)fun_info->f.fun_type);
	    switch (fun_info->f.fun_type) {
	    case ARG_GEN:
		printSmallBitmap(spBottom, sp+2,
				 BITMAP_BITS(fun_info->f.b.bitmap),
				 BITMAP_SIZE(fun_info->f.b.bitmap));
		break;
	    case ARG_GEN_BIG:
		printLargeBitmap(spBottom, sp+2,
				 GET_FUN_LARGE_BITMAP(fun_info),
				 GET_FUN_LARGE_BITMAP(fun_info)->size);
		break;
	    default:
		printSmallBitmap(spBottom, sp+2,
				 BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]),
				 BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]));
		break;
	    }
	    continue;
	}
	   
	default:
	    debugBelch("unknown object %d\n", (int)info->type);
	    barf("printStackChunk");
	}
    }
}

void printTSO( StgTSO *tso )
{
    printStackChunk( tso->stackobj->sp,
                     tso->stackobj->stack+tso->stackobj->stack_size);
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
    StgWord value;
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

static void insert( StgWord value, const char *name )
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
static rtsBool lookup_name( char *name, StgWord *result )
{
    nat i;
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
    for( i = 0; i < table_size && table[i].value != (StgWord) addr; ++i ) {
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

int searched = 0;

static int
findPtrBlocks (StgPtr p, bdescr *bd, StgPtr arr[], int arr_size, int i)
{
    StgPtr q, r, end;
    for (; bd; bd = bd->link) {
        searched++;
        for (q = bd->start; q < bd->free; q++) {
            if (UNTAG_CLOSURE((StgClosure*)*q) == (StgClosure *)p) {
                if (i < arr_size) {
                    for (r = bd->start; r < bd->free; r = end) {
                        // skip over zeroed-out slop
                        while (*r == 0) r++;
                        if (!LOOKS_LIKE_CLOSURE_PTR(r)) {
                            debugBelch("%p found at %p, no closure at %p\n",
                                       p, q, r);
                            break;
                        }
                        end = r + closure_sizeW((StgClosure*)r);
                        if (q < end) {
                            debugBelch("%p = ", r);
                            printClosure((StgClosure *)r);
                            arr[i++] = r;
                            break;
                        }
                    }
                    if (r >= bd->free) {
                        debugBelch("%p found at %p, closure?", p, q);
                    }
                } else {
                    return i;
                }
            }
        }
    }
    return i;
}

void
findPtr(P_ p, int follow)
{
  nat g, n;
  bdescr *bd;
  const int arr_size = 1024;
  StgPtr arr[arr_size];
  int i = 0;
  searched = 0;

  for (n = 0; n < n_capabilities; n++) {
      bd = nurseries[i].blocks;
      i = findPtrBlocks(p,bd,arr,arr_size,i);
      if (i >= arr_size) return;
  }

  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
      bd = generations[g].blocks;
      i = findPtrBlocks(p,bd,arr,arr_size,i);
      bd = generations[g].large_objects;
      i = findPtrBlocks(p,bd,arr,arr_size,i);
      if (i >= arr_size) return;
  }
  if (follow && i == 1) {
      debugBelch("-->\n");
      findPtr(arr[0], 1);
  }
}

/* prettyPrintClosure() is for printing out a closure using the data constructor
   names found in the info tables. Closures are printed in a fashion that resembles
   their Haskell representation. Useful during debugging.

   Todo: support for more closure types, and support for non pointer fields in the
   payload.
*/ 

void prettyPrintClosure_ (StgClosure *);

void prettyPrintClosure (StgClosure *obj)
{
   prettyPrintClosure_ (obj);
   debugBelch ("\n");
}

void prettyPrintClosure_ (StgClosure *obj)
{
    StgInfoTable *info;
    StgConInfoTable *con_info;

    /* collapse any indirections */
    unsigned int type;
    type = get_itbl(obj)->type;
           
    while (type == IND ||
           type == IND_STATIC ||
           type == IND_PERM)
    {
      obj = ((StgInd *)obj)->indirectee;
      type = get_itbl(obj)->type;
    }

    /* find the info table for this object */
    info = get_itbl(obj);

    /* determine what kind of object we have */
    switch (info->type) 
    {
        /* full applications of data constructors */
        case CONSTR:
        case CONSTR_1_0: 
        case CONSTR_0_1:
        case CONSTR_1_1: 
        case CONSTR_0_2: 
        case CONSTR_2_0:
        case CONSTR_STATIC:
        case CONSTR_NOCAF_STATIC: 
        {
           nat i; 
           char *descriptor;

           /* find the con_info for the constructor */
           con_info = get_con_itbl (obj);

           /* obtain the name of the constructor */
           descriptor = GET_CON_DESC(con_info);

           debugBelch ("(%s", descriptor);

           /* process the payload of the closure */
           /* we don't handle non pointers at the moment */
           for (i = 0; i < info->layout.payload.ptrs; i++)
           {
              debugBelch (" ");
              prettyPrintClosure_ ((StgClosure *) obj->payload[i]);
           }
           debugBelch (")");
           break;
        }

        /* if it isn't a constructor then just print the closure type */
        default:
        {
           debugBelch ("<%s>", info_type(obj));
           break;
        }
    }
}

char *what_next_strs[] = {
  [0]               = "(unknown)",
  [ThreadRunGHC]    = "ThreadRunGHC",
  [ThreadInterpret] = "ThreadInterpret",
  [ThreadKilled]    = "ThreadKilled",
  [ThreadComplete]  = "ThreadComplete"
};

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

/* -----------------------------------------------------------------------------
   Closure types
   
   NOTE: must be kept in sync with the closure types in includes/ClosureTypes.h
   -------------------------------------------------------------------------- */

char *closure_type_names[] = {
 [INVALID_OBJECT]        = "INVALID_OBJECT",
 [CONSTR]                = "CONSTR",
 [CONSTR_1_0]            = "CONSTR_1_0",
 [CONSTR_0_1]            = "CONSTR_0_1",
 [CONSTR_2_0]            = "CONSTR_2_0",
 [CONSTR_1_1]            = "CONSTR_1_1",
 [CONSTR_0_2]            = "CONSTR_0_2",
 [CONSTR_STATIC]         = "CONSTR_STATIC",
 [CONSTR_NOCAF_STATIC]   = "CONSTR_NOCAF_STATIC",
 [FUN]                   = "FUN",
 [FUN_1_0]               = "FUN_1_0",
 [FUN_0_1]               = "FUN_0_1",
 [FUN_2_0]               = "FUN_2_0",
 [FUN_1_1]               = "FUN_1_1",
 [FUN_0_2]               = "FUN_0_2",
 [FUN_STATIC]            = "FUN_STATIC",
 [THUNK]                 = "THUNK",
 [THUNK_1_0]             = "THUNK_1_0",
 [THUNK_0_1]             = "THUNK_0_1",
 [THUNK_2_0]             = "THUNK_2_0",
 [THUNK_1_1]             = "THUNK_1_1",
 [THUNK_0_2]             = "THUNK_0_2",
 [THUNK_STATIC]          = "THUNK_STATIC",
 [THUNK_SELECTOR]        = "THUNK_SELECTOR",
 [BCO]                   = "BCO",
 [AP]                    = "AP",
 [PAP]                   = "PAP",
 [AP_STACK]              = "AP_STACK",
 [IND]                   = "IND",
 [IND_PERM]              = "IND_PERM",
 [IND_STATIC]            = "IND_STATIC",
 [RET_BCO]               = "RET_BCO",
 [RET_SMALL]             = "RET_SMALL",
 [RET_BIG]               = "RET_BIG",
 [RET_FUN]               = "RET_FUN",
 [UPDATE_FRAME]          = "UPDATE_FRAME",
 [CATCH_FRAME]           = "CATCH_FRAME",
 [UNDERFLOW_FRAME]       = "UNDERFLOW_FRAME",
 [STOP_FRAME]            = "STOP_FRAME",
 [BLOCKING_QUEUE]        = "BLOCKING_QUEUE",
 [BLACKHOLE]             = "BLACKHOLE",
 [MVAR_CLEAN]            = "MVAR_CLEAN",
 [MVAR_DIRTY]            = "MVAR_DIRTY",
 [TVAR]                  = "TVAR",
 [ARR_WORDS]             = "ARR_WORDS",
 [MUT_ARR_PTRS_CLEAN]    = "MUT_ARR_PTRS_CLEAN",
 [MUT_ARR_PTRS_DIRTY]    = "MUT_ARR_PTRS_DIRTY",
 [MUT_ARR_PTRS_FROZEN0]  = "MUT_ARR_PTRS_FROZEN0",
 [MUT_ARR_PTRS_FROZEN]   = "MUT_ARR_PTRS_FROZEN",
 [MUT_VAR_CLEAN]         = "MUT_VAR_CLEAN",
 [MUT_VAR_DIRTY]         = "MUT_VAR_DIRTY",
 [WEAK]                  = "WEAK",
 [PRIM]	                 = "PRIM",
 [MUT_PRIM]              = "MUT_PRIM",
 [TSO]                   = "TSO",
 [STACK]                 = "STACK",
 [TREC_CHUNK]            = "TREC_CHUNK",
 [ATOMICALLY_FRAME]      = "ATOMICALLY_FRAME",
 [CATCH_RETRY_FRAME]     = "CATCH_RETRY_FRAME",
 [CATCH_STM_FRAME]       = "CATCH_STM_FRAME",
 [WHITEHOLE]             = "WHITEHOLE"
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

