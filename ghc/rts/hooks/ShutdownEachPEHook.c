/* -----------------------------------------------------------------------------
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

#ifdef PAR
void
ShutdownEachPEHook (void)
{ /* In a GUM setup this routine is called at the end of 
     shutdownParallelSystem on each PE. Useful for
     cleaning up stuff, especially when interfacing 
     with foreign language code.
     -- HWL 
  */
}
#endif
