/* -----------------------------------------------------------------------------
 * $Id: Printer.h,v 1.5 2000/01/14 14:56:40 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Prototypes for functions in Printer.c
 *
 * ---------------------------------------------------------------------------*/

extern void   	   printPtr        ( StgPtr p );
extern void   	   printObj        ( StgClosure *obj );

#ifdef DEBUG
extern void   	   printClosure    ( StgClosure *obj );
extern StgStackPtr printStackObj   ( StgStackPtr sp );
extern void        printStackChunk ( StgStackPtr sp, StgStackPtr spLim );
extern void        printStack      ( StgStackPtr sp, StgStackPtr spLim, 
				     StgUpdateFrame* su );
extern void        printTSO        ( StgTSO *tso );

void   	           info_hdr_type   ( StgClosure *closure, char *res );
char  *	           info_type       ( StgClosure *closure );
char  *	           info_type_by_ip ( StgInfoTable *ip );

extern void DEBUG_LoadSymbols( char *name );

extern rtsBool lookupGHCName( StgPtr addr, const char **result );
#endif
