/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Prototypes for functions in Printer.c
 *
 * ---------------------------------------------------------------------------*/

#ifndef PRINTER_H
#define PRINTER_H

#include "BeginPrivate.h"

extern void        printPtr        ( StgPtr p );
extern void        printObj        ( StgClosure *obj );

extern const char *  closure_type_names[];

void               info_hdr_type   ( const StgClosure *closure, char *res );
const char  *      info_type       ( const StgClosure *closure );
const char  *      info_type_by_ip ( const StgInfoTable *ip );
const char  *      info_update_frame ( const StgClosure *closure );

#ifdef DEBUG
extern void        printClosure    ( const StgClosure *obj );
extern void        printStackChunk ( StgPtr sp, StgPtr spLim );
extern void        printTSO        ( StgTSO *tso );

extern void DEBUG_LoadSymbols( const char *name );

extern const char *lookupGHCName( void *addr );

extern const char *what_next_strs[];
#endif

#include "EndPrivate.h"

#endif /* PRINTER_H */

