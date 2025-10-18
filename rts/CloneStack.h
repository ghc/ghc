/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2020-2021
 *
 * Stack snapshotting and decoding. (Cloning and unwinding.)
 *
 *---------------------------------------------------------------------------*/

#pragma once

extern StgClosure ghczminternal_GHCziInternalziStackziCloneStack_StackSnapshot_closure;
#define StackSnapshot_constructor_closure (&(ghczminternal_GHCziInternalziStackziCloneStack_StackSnapshot_closure))

StgStack* cloneStack(Capability* capability, const StgStack* stack);

void sendCloneStackMessage(StgTSO *tso, HsStablePtr mvar);

StgArrBytes* decodeClonedStack(Capability *cap, StgStack* stack);

#include "BeginPrivate.h"

#if defined(THREADED_RTS)
void handleCloneStackMessage(MessageCloneStack *msg);
#endif

#include "EndPrivate.h"
