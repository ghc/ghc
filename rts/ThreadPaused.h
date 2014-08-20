/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Tidying up a thread when it stops running
 *
 * ---------------------------------------------------------------------------*/

#ifndef THREADPAUSED_H
#define THREADPAUSED_H

RTS_PRIVATE void threadPaused ( Capability *cap, StgTSO * );

#endif /* THREADPAUSED_H */

// Local Variables:
// mode: C
// fill-column: 80
// indent-tabs-mode: nil
// c-basic-offset: 4
// buffer-file-coding-system: utf-8-unix
// End:
