/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Prototypes for functions in Printer.c
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

extern void        printPtr        ( StgPtr p );
extern void        printObj        ( StgClosure *obj );

extern const char *  closure_type_names[];

void               info_hdr_type   ( const StgClosure *closure, char *res );
const char  *      info_type       ( const StgClosure *closure );
const char  *      info_type_by_ip ( const StgInfoTable *ip );
const char  *      info_update_frame ( const StgClosure *closure );

#if defined(DEBUG)
extern void        printClosure    ( const StgClosure *obj );
extern void        printStack ( StgStack *stack );
extern void        printStackChunk ( StgPtr sp, StgPtr spLim );
extern void        printTSO        ( StgTSO *tso );
extern void        printMutableList( bdescr *bd );
extern void        printStaticObjects ( StgClosure *obj );
extern void        printWeakLists ( void );
extern void        printLargeAndPinnedObjects ( void );

extern const char *what_next_strs[];
#endif

extern const char *lookupDebugSymbol( void *addr );

#include "EndPrivate.h"
