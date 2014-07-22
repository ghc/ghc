/* -----------------------------------------------------------------------------
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

/* Note: by the time this hook has been called, Haskell land
 * will have been shut down completely.
 *
 * ToDo: feed the hook info on whether we're shutting down as a result
 * of termination or run-time error ?
 */
 
void
OnExitHook (void)
{
}

// Local Variables:
// mode: C
// fill-column: 80
// indent-tabs-mode: nil
// c-basic-offset: 4
// buffer-file-coding-system: utf-8-unix
// End:
