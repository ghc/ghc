/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Tidying up a thread when it stops running
 *
 * ---------------------------------------------------------------------------*/

#pragma once

RTS_PRIVATE void threadPaused ( Capability *cap, StgTSO * );
