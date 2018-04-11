/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Tidying up a thread when it stops running
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

RTS_PRIVATE void threadPaused ( Capability *cap, StgTSO * );

#include "EndPrivate.h"

#if defined(THREADED_RTS) && defined(PROF_SPIN)
extern volatile StgWord64 whitehole_threadPaused_spin;
#endif
