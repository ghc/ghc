/* -----------------------------------------------------------------------------
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"
#include "Hooks.h"

void
FlagDefaultsHook (void)
{ /* this is called *after* RtsFlags has had
     its defaults set, but *before* we start
     processing the RTS command-line options.

     This default version does *nothing*.
     The user may provide a more interesting
     one.
  */
}

