/* -----------------------------------------------------------------------------
 * $Id: Printer.h,v 1.2 1998/12/02 13:28:34 simonm Exp $
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


