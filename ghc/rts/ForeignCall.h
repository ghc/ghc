/* -----------------------------------------------------------------------------
 * $Id: ForeignCall.h,v 1.5 1999/10/19 11:01:28 sewardj Exp $
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

