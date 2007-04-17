/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Prototypes for functions in Printer.c
 *
 * ---------------------------------------------------------------------------*/

#ifndef PRINTER_H
#define PRINTER_H

extern void   	   printPtr        ( StgPtr p );
extern void   	   printObj        ( StgClosure *obj );

#ifdef DEBUG
extern void        prettyPrintClosure (StgClosure *obj);
extern void   	   printClosure    ( StgClosure *obj );
extern StgStackPtr printStackObj   ( StgStackPtr sp );
extern void        printStackChunk ( StgStackPtr sp, StgStackPtr spLim );
extern void        printTSO        ( StgTSO *tso );

void   	           info_hdr_type   ( StgClosure *closure, char *res );
char  *	           info_type       ( StgClosure *closure );
char  *	           info_type_by_ip ( StgInfoTable *ip );

extern void DEBUG_LoadSymbols( char *name );

extern const char *lookupGHCName( void *addr );
#endif

#endif /* PRINTER_H */

