/* -----------------------------------------------------------------------------
 * $Id: ForeignCall.h,v 1.4 1999/10/15 11:03:10 sewardj Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Prototypes for functions in ForeignCall.c
 *
 * ---------------------------------------------------------------------------*/

typedef int StablePtr;

extern void ccall ( CFunDescriptor* descriptor, void (*fun)(void), StgBCO** bco );
extern void hcall ( HFunDescriptor* descriptor, StablePtr fun, void* as, void* rs );


