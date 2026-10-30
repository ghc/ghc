/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2020-2021
 *
 * Stack snapshotting and decoding. (Cloning and unwinding.)
 *
 *---------------------------------------------------------------------------*/

#pragma once

#define StackSnapshot_constructor_closure ghc_hs_iface->StackSnapshot_closure

RTS_PUBLIC StgStack* cloneStack(Capability* capability, const StgStack* stack);

RTS_PUBLIC void sendCloneStackMessage(StgTSO *tso, HsStablePtr mvar);

#include "BeginPrivate.h"

#if defined(THREADED_RTS)
void handleCloneStackMessage(Capability *cap, MessageCloneStack *msg);
#endif

#include "EndPrivate.h"
