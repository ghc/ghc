/* -----------------------------------------------------------------------------
 * $Id: hschooks.h,v 1.3 2000/12/20 09:56:26 simonmar Exp $
 *
 * Hooks into the RTS from the compiler.
 *
 * -------------------------------------------------------------------------- */

#include "HsFFI.h"
void enableTimingStats( void );
void setHeapSize( HsInt size );
