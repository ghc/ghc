/* -----------------------------------------------------------------------------
 * $Id: ForeignCall.h,v 1.2 1998/12/02 13:28:23 simonm Exp $
 *
 * Prototypes for functions in ForeignCall.c
 *
 * ---------------------------------------------------------------------------*/

typedef int StablePtr;

extern void ccall ( CFunDescriptor* descriptor, void (*fun)(void) );
extern void hcall ( HFunDescriptor* descriptor, StablePtr fun, void* as, void* rs );


