/* -----------------------------------------------------------------------------
 * $Id: ForeignCall.h,v 1.3 1999/02/05 16:02:41 simonm Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Prototypes for functions in ForeignCall.c
 *
 * ---------------------------------------------------------------------------*/

typedef int StablePtr;

extern void ccall ( CFunDescriptor* descriptor, void (*fun)(void) );
extern void hcall ( HFunDescriptor* descriptor, StablePtr fun, void* as, void* rs );


