/* -----------------------------------------------------------------------------
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

/* Note: by the time this hook has been called, Haskell land
 * will have been shut down completely.
 *
 * ToDo: feed the hook info on whether we're shutting down as a result
 * of termination or run-time error ?
 */
 
void
OnExitHook ()
{
}
