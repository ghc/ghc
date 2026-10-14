/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2005
 *
 * Compatibility interface to what historically was the RTS timer signal.
 * None of these functions do anything anymore. Do not use them.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "rts/OldTimer.h" /* See for details of what these functions were for */

#if defined(HAVE_SIGNAL_H)
#include <signal.h>
#endif

void stopTimer(void)  { /* no-op */ }
void startTimer(void) { /* no-op */ }

#if !defined(mingw32_HOST_OS) && !defined(wasm32_HOST_ARCH)
int rtsTimerSignal(void)
{
    return SIGALRM;
}
#endif
