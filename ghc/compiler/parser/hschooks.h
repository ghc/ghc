/* -----------------------------------------------------------------------------
 * $Id: hschooks.h,v 1.4 2002/04/22 14:54:10 simonmar Exp $
 *
 * Hooks into the RTS from the compiler.
 *
 * -------------------------------------------------------------------------- */

#include "HsFFI.h"
void enableTimingStats( void );
void setHeapSize( HsInt size );

// Out-of-line string functions, see PrimPacked.lhs
HsInt ghc_strlen( HsAddr a );
HsInt ghc_memcmp( HsAddr a1, HsAddr a2, HsInt len );
HsInt ghc_memcmp_off( HsAddr a1, HsInt i, HsAddr a2, HsInt len );
