/* -----------------------------------------------------------------------------
 * $Id: ForeignCall.h,v 1.7 1999/10/26 17:27:30 sewardj Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Prototypes for functions in ForeignCall.c
 *
 * ---------------------------------------------------------------------------*/

typedef int StablePtr;

extern int ccall ( CFunDescriptor* descriptor, 
                   void            (*fun)(void), 
                   StgBCO**        bco,
                   char            callconv
                 );

extern StgAddr createAdjThunk ( StgStablePtr stableptr,
                                StgAddr      typestr,
                                StgChar      callconv );
