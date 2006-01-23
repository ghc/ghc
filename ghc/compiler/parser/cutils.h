/* -----------------------------------------------------------------------------
 *
 * Utility C functions.
 *
 * -------------------------------------------------------------------------- */

#include "HsFFI.h"

// Out-of-line string functions, see PrimPacked.lhs
HsInt ghc_strlen( HsAddr a );
HsInt ghc_memcmp( HsAddr a1, HsAddr a2, HsInt len );
HsInt ghc_memcmp_off( HsAddr a1, HsInt i, HsAddr a2, HsInt len );


void enableTimingStats( void );
void setHeapSize( HsInt size );
