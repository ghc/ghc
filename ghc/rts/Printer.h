/* -----------------------------------------------------------------------------
 * $Id: Printer.h,v 1.3 1999/02/05 16:02:47 simonm Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Prototypes for functions in Printer.c
 *
 * ---------------------------------------------------------------------------*/

extern void   	   printPtr        ( StgPtr p );
extern void   	   printObj        ( StgClosure *obj );
extern void   	   printClosure    ( StgClosure *obj );
extern StgStackPtr printStackObj   ( StgStackPtr sp );
extern void        printStackChunk ( StgStackPtr sp, StgStackPtr spLim );
extern void        printStack      ( StgStackPtr sp, StgStackPtr spLim, 
				     StgUpdateFrame* su );
extern void        printTSO        ( StgTSO *tso );


extern void DEBUG_LoadSymbols( char *name );

extern rtsBool lookupGHCName( StgPtr addr, const char **result );


