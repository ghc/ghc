/* -----------------------------------------------------------------------------
 * $Id: ForeignCall.h,v 1.8 1999/11/08 15:30:39 sewardj Exp $
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
                   char            callconv,
                   Capability*     cap
                 );

extern StgAddr createAdjThunk ( StgStablePtr stableptr,
                                StgAddr      typestr,
                                StgChar      callconv );
