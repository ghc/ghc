/* -*- mode: hugs-c; -*- */
/* -----------------------------------------------------------------------------
 * $Id: Printer.c,v 1.5 1999/02/05 14:44:43 simonm Exp $
 *
 * Copyright (c) 1994-1998.
 *
 * Heap printer
 * 
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

#ifdef DEBUG

#include "RtsUtils.h"
#include "RtsFlags.h"
#include "Bytecodes.h"  /* for InstrPtr */
#include "Disassembler.h"

#include "Printer.h"

/* --------------------------------------------------------------------------
 * local function decls
 * ------------------------------------------------------------------------*/

static void    printStdObject( StgClosure *obj, char* tag );
static void    reset_table   ( int size );
static void    prepare_table ( void );
static void    insert        ( unsigned value, const char *name );
#if 0 /* unused but might be useful sometime */
static rtsBool lookup_name   ( char *name, unsigned *result );
static void    enZcode       ( char *in, char *out );
#endif
static char    unZcode       ( char ch );
rtsBool lookupGHCName ( StgPtr addr, const char **result );
static void    printZcoded   ( const char *raw );

/* --------------------------------------------------------------------------
 * Printer
 * ------------------------------------------------------------------------*/

extern void printPtr( StgPtr p )
{
    const char *raw;
    if (lookupGHCName( p, &raw )) {
        printZcoded(raw);
#ifdef INTERPRETER
    } else if ((raw = lookupHugsName(p)) != 0) {
        fprintf(stderr, "%s", raw);
#endif
    } else {
        fprintf(stderr, "%p", p);
    }
}
  
void printObj( StgClosure *obj )
{
    fprintf(stderr,"Object "); printPtr((StgPtr)obj); fprintf(stderr," = ");
    printClosure(obj);
}

static void printStdObject( StgClosure *obj, char* tag )
{
    StgWord i, j;
    const StgInfoTable* info = get_itbl(obj);
    fprintf(stderr,"%s(",tag);
    printPtr((StgPtr)obj->header.info);
    for (i = 0; i < info->layout.payload.ptrs; ++i) {
        fprintf(stderr,", ");
        printPtr(payloadPtr(obj,i));
    }
    for (j = 0; j < info->layout.payload.nptrs; ++j) {
        fprintf(stderr,", %xd#",payloadWord(obj,i+j));
    }
    fprintf(stderr,")\n");
}

void printClosure( StgClosure *obj )
{
    switch ( get_itbl(obj)->type ) {
    case INVALID_OBJECT:
            barf("Invalid object");
#ifdef INTERPRETER
    case BCO:
            fprintf(stderr,"BCO\n");
            disassemble(stgCast(StgBCO*,obj),"\t");
            break;
#endif

    case AP_UPD:
        {
	    StgAP_UPD* ap = stgCast(StgAP_UPD*,obj);
            StgWord i;
            fprintf(stderr,"AP_UPD("); printPtr((StgPtr)ap->fun);
            for (i = 0; i < ap->n_args; ++i) {
                fprintf(stderr,", ");
                printPtr(payloadPtr(ap,i));
            }
            fprintf(stderr,")\n");
            break;
        }

    case PAP:
        {
	    StgPAP* pap = stgCast(StgPAP*,obj);
            StgWord i;
            fprintf(stderr,"PAP("); printPtr((StgPtr)pap->fun);
            for (i = 0; i < pap->n_args; ++i) {
                fprintf(stderr,", ");
                printPtr(payloadPtr(pap,i));
            }
            fprintf(stderr,")\n");
            break;
        }

    case IND:
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

    case CAF_UNENTERED:
        {
	    StgCAF* caf = stgCast(StgCAF*,obj);
            fprintf(stderr,"CAF_UNENTERED("); 
            printPtr((StgPtr)caf->body);
            fprintf(stderr,", ");
            printPtr((StgPtr)caf->value); /* should be null */
            fprintf(stderr,", ");
            printPtr((StgPtr)caf->link);  /* should be null */
            fprintf(stderr,")\n"); 
            break;
        }

    case CAF_ENTERED:
        {
	    StgCAF* caf = stgCast(StgCAF*,obj);
            fprintf(stderr,"CAF_ENTERED("); 
            printPtr((StgPtr)caf->body);
            fprintf(stderr,", ");
            printPtr((StgPtr)caf->value);
            fprintf(stderr,", ");
            printPtr((StgPtr)caf->link);
            fprintf(stderr,")\n"); 
            break;
        }

    case CAF_BLACKHOLE:
            fprintf(stderr,"CAF_BH("); 
            printPtr((StgPtr)stgCast(StgBlockingQueue*,obj)->blocking_queue);
            fprintf(stderr,")\n"); 
            break;

    case BLACKHOLE:
            fprintf(stderr,"BH\n"); 
            break;

    case BLACKHOLE_BQ:
            fprintf(stderr,"BQ("); 
            printPtr((StgPtr)stgCast(StgBlockingQueue*,obj)->blocking_queue);
            fprintf(stderr,")\n"); 
            break;

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
            const StgInfoTable* info = get_itbl(obj);
            fprintf(stderr,"PACK(");
            printPtr((StgPtr)obj->header.info);
            fprintf(stderr,"(tag=%d)",info->srt_len);
            for (i = 0; i < info->layout.payload.ptrs; ++i) {
                fprintf(stderr,", ");
                printPtr(payloadPtr(obj,i));
            }
            for (j = 0; j < info->layout.payload.nptrs; ++j) {
                fprintf(stderr,", %x#",payloadWord(obj,i+j));
            }
            fprintf(stderr,")\n");
            break;
        }

    case FUN:
    case FUN_1_0: case FUN_0_1: 
    case FUN_1_1: case FUN_0_2: case FUN_2_0:
    case FUN_STATIC:
            printStdObject(obj,"FUN");
            break;

    case THUNK:
    case THUNK_1_0: case THUNK_0_1:
    case THUNK_1_1: case THUNK_0_2: case THUNK_2_0:
    case THUNK_STATIC:
            /* ToDo: will this work for THUNK_STATIC too? */
            printStdObject(obj,"THUNK");
            break;
#if 0
    case ARR_WORDS:
        {
            StgWord i;
            fprintf(stderr,"ARR_WORDS(\"");
            /* ToDo: we can't safely assume that this is a string! */
            for (i = 0; arrWordsGetChar(obj,i); ++i) {
                putchar(arrWordsGetChar(obj,i));
            }
            fprintf(stderr,"\")\n");
            break;
        }
#endif
    case UPDATE_FRAME:
        {
            StgUpdateFrame* u = stgCast(StgUpdateFrame*,obj);
            fprintf(stderr,"UpdateFrame(");
            printPtr((StgPtr)GET_INFO(u));
            fprintf(stderr,",");
            printPtr((StgPtr)u->updatee);
            fprintf(stderr,",");
            printPtr((StgPtr)u->link);
            fprintf(stderr,")\n"); 
            break;
        }

    case CATCH_FRAME:
        {
            StgCatchFrame* u = stgCast(StgCatchFrame*,obj);
            fprintf(stderr,"CatchFrame(");
            printPtr((StgPtr)GET_INFO(u));
            fprintf(stderr,",");
            printPtr((StgPtr)u->handler);
            fprintf(stderr,",");
            printPtr((StgPtr)u->link);
            fprintf(stderr,")\n"); 
            break;
        }

    case SEQ_FRAME:
        {
            StgSeqFrame* u = stgCast(StgSeqFrame*,obj);
            fprintf(stderr,"SeqFrame(");
            printPtr((StgPtr)GET_INFO(u));
            fprintf(stderr,",");
            printPtr((StgPtr)u->link);
            fprintf(stderr,")\n"); 
            break;
        }

    case STOP_FRAME:
        {
            StgStopFrame* u = stgCast(StgStopFrame*,obj);
            fprintf(stderr,"StopFrame(");
            printPtr((StgPtr)GET_INFO(u));
            fprintf(stderr,")\n"); 
            break;
        }
    default:
            barf("printClosure %d",get_itbl(obj)->type);
            return;
    }
}

StgPtr printStackObj( StgPtr sp )
{
    /*fprintf(stderr,"Stack[%d] = ", &stgStack[STACK_SIZE] - sp); */

    if (IS_ARG_TAG(*sp)) {

#ifdef DEBUG_EXTRA
        StackTag tag = (StackTag)*sp;
        switch ( tag ) {
        case ILLEGAL_TAG:
                barf("printStackObj: ILLEGAL_TAG");
                break;
        case REALWORLD_TAG:
                fprintf(stderr,"RealWorld#\n");
                break;
        case INT_TAG:
                fprintf(stderr,"Int# %d\n", *(StgInt*)(sp+1));
                break;
        case INT64_TAG:
                fprintf(stderr,"Int64# %lld\n", *(StgInt64*)(sp+1));
                break;
        case WORD_TAG:
                fprintf(stderr,"Word# %d\n", *(StgWord*)(sp+1));
                break;
        case ADDR_TAG:
                fprintf(stderr,"Addr# "); printPtr(*(StgAddr*)(sp+1)); fprintf(stderr,"\n");
                break;
        case CHAR_TAG:
                fprintf(stderr,"Char# %d\n", *(StgChar*)(sp+1));
                break;
        case FLOAT_TAG:
                fprintf(stderr,"Float# %f\n", PK_FLT(sp+1));
                break;
        case DOUBLE_TAG:
                fprintf(stderr,"Double# %f\n", PK_DBL(sp+1));
                break;
        default:
                barf("printStackObj: unrecognised ARGTAG %d",tag);
        }
        sp += 1 + ARG_SIZE(tag);

#else /* !DEBUG_EXTRA */
	{
	    StgWord tag = *sp++;
	    nat i;
	    fprintf(stderr,"Tag: %d words\n", tag);
	    for (i = 0; i < tag; i++) {
	        fprintf(stderr,"Word# %d\n", *sp++);
	    }
	}
#endif

    } else {
        printPtr((StgPtr)*sp);
        fprintf(stderr,"\n");
        sp += 1;
    }
    return sp;
    
}

void printStackChunk( StgPtr sp, StgPtr spBottom )
{
    StgNat32 bitmap;
    const StgInfoTable *info;

    ASSERT(sp <= spBottom);
    while (sp < spBottom) {
      if (!IS_ARG_TAG(*sp) && LOOKS_LIKE_GHC_INFO(*sp)) {
	info = get_itbl((StgClosure *)sp);
	switch (info->type) {

	case UPDATE_FRAME:
	    printObj( stgCast(StgClosure*,sp) );
	    sp += sizeofW(StgUpdateFrame);
	    continue;

	case SEQ_FRAME:
	    printObj( stgCast(StgClosure*,sp) );
	    sp += sizeofW(StgSeqFrame);
	    continue;

	case CATCH_FRAME:
	    printObj( stgCast(StgClosure*,sp) );
	    sp += sizeofW(StgCatchFrame);
	    continue;

	case STOP_FRAME:
	    /* not quite: ASSERT(stgCast(StgPtr,su) == spBottom); */
	    printObj( stgCast(StgClosure*,sp) );
	    continue;

	case RET_DYN:
	  fprintf(stderr, "RET_DYN (%p)\n", sp);
	  bitmap = *++sp;
	  ++sp;
	  fprintf(stderr, "Bitmap: 0x%x\n", bitmap);
	  goto small_bitmap;

	case RET_SMALL:
	case RET_VEC_SMALL:
	  fprintf(stderr, "RET_SMALL (%p)\n", sp);
	  bitmap = info->layout.bitmap;
	  sp++;
	small_bitmap:
	  while (bitmap != 0) {
	    fprintf(stderr,"Stack[%d] (%p) = ", spBottom-sp, sp);
	    if ((bitmap & 1) == 0) {
	      printPtr((P_)*sp);
	      fprintf(stderr,"\n");
	    } else {
	      fprintf(stderr,"Word# %d\n", *sp++);
	    }	      
	    sp++;
	    bitmap = bitmap >> 1;
	    }
	  continue;

	case RET_BIG:
	case RET_VEC_BIG:
	  barf("todo");

	default:
	  break;
	}
      }
      fprintf(stderr,"Stack[%d] (%p) = ", spBottom-sp, sp);
      sp = printStackObj(sp);
    }
}

void printStack( StgPtr sp, StgPtr spBottom, StgUpdateFrame* su )
{
    /* check everything down to the first update frame */
    printStackChunk( sp, stgCast(StgPtr,su) );
    while ( stgCast(StgPtr,su) < spBottom) {
	sp = stgCast(StgPtr,su);
	switch (get_itbl(su)->type) {
	case UPDATE_FRAME:
                printObj( stgCast(StgClosure*,su) );
                sp += sizeofW(StgUpdateFrame);
		su = su->link;
		break;
	case SEQ_FRAME:
                printObj( stgCast(StgClosure*,su) );
                sp += sizeofW(StgSeqFrame);
		su = stgCast(StgSeqFrame*,su)->link;
		break;
	case CATCH_FRAME:
                printObj( stgCast(StgClosure*,su) );
                sp += sizeofW(StgCatchFrame);
		su = stgCast(StgCatchFrame*,su)->link;
		break;
	case STOP_FRAME:
		/* not quite: ASSERT(stgCast(StgPtr,su) == spBottom); */
                printObj( stgCast(StgClosure*,su) );
		return;
	default:
		barf("printStack: weird record found on update frame list.");
	}
	printStackChunk( sp, stgCast(StgPtr,su) );
    }
    ASSERT(stgCast(StgPtr,su) == spBottom);
}

void printTSO( StgTSO *tso )
{
    printStack( tso->sp, tso->stack+tso->stack_size,tso->su);
    /* printStackChunk( tso->sp, tso->stack+tso->stack_size); */
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

static nat max_table_size;
static nat table_size;
static struct entry* table;

static void reset_table( int size )
{
    max_table_size = size;
    table_size = 0;
    table = (struct entry *) malloc(size * sizeof(struct entry));
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

rtsBool lookupGHCName( StgPtr addr, const char **result )
{
    nat i;
    for( i = 0; i < table_size && table[i].value != (unsigned) addr; ++i ) {
    }
    if (i < table_size) {
        *result = table[i].name;
        return rtsTrue;
    } else {
        return rtsFalse;
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

#ifdef HAVE_BFD_H

#include <bfd.h>

/* Fairly ad-hoc piece of code that seems to filter out a lot of
 * rubbish like the obj-splitting symbols
 */

static rtsBool isReal( flagword flags, const char *name )
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
    
        IF_DEBUG(evaluator,
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

extern void DEBUG_LoadSymbols( char *name )
{
  /* nothing, yet */
}

#endif /* HAVE_BFD_H */

#endif /* DEBUG */
