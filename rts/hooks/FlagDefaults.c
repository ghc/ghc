/* -----------------------------------------------------------------------------
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
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


// Local Variables:
// mode: C
// fill-column: 80
// indent-tabs-mode: nil
// c-basic-offset: 4
// buffer-file-coding-system: utf-8-unix
// End:
