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
