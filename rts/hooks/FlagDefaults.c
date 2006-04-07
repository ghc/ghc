/* -----------------------------------------------------------------------------
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

void
defaultsHook (void)
{ /* this is called *after* RTSflags has had
     its defaults set, but *before* we start
     processing the RTS command-line options.

     This default version does *nothing*.
     The user may provide a more interesting
     one.
  */
}

