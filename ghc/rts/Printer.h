/* -----------------------------------------------------------------------------
 * $Id: Printer.h,v 1.4 1999/06/29 13:04:40 panne Exp $
 *
 * (c) The GHC Team, 1998-1999
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


extern void DEBUG_LoadSymbols( char *name );

extern rtsBool lookupGHCName( StgPtr addr, const char **result );
#endif
