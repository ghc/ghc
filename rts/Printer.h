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

extern void   	   printPtr        ( StgPtr p );
extern void   	   printObj        ( StgClosure *obj );

extern char *      closure_type_names[];

void   	           info_hdr_type   ( StgClosure *closure, char *res );
char  *	           info_type       ( StgClosure *closure );
char  *	           info_type_by_ip ( StgInfoTable *ip );
char  *            info_update_frame ( StgClosure *closure );

#ifdef DEBUG
extern void        prettyPrintClosure (StgClosure *obj);
extern void   	   printClosure    ( StgClosure *obj );
extern StgPtr      printStackObj   ( StgPtr sp );
extern void        printStackChunk ( StgPtr sp, StgPtr spLim );
extern void        printTSO        ( StgTSO *tso );

extern void DEBUG_LoadSymbols( char *name );

extern const char *lookupGHCName( void *addr );

extern char *what_next_strs[];
#endif

#include "EndPrivate.h"

#endif /* PRINTER_H */

