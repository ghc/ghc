/* -----------------------------------------------------------------------------
 * $Id: ForeignCall.h,v 1.6 1999/10/22 15:58:21 sewardj Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Prototypes for functions in ForeignCall.c
 *
 * ---------------------------------------------------------------------------*/

typedef int StablePtr;

extern int ccall ( CFunDescriptor* descriptor, 
                   void            (*fun)(void), 
                   StgBCO**        bco 
                 );

extern StgAddr createAdjThunk ( StgStablePtr stableptr,
                                StgAddr      typestr );
