#pragma once

#if defined(THREADED_RTS)
void handleCloneStackMessage(MessageCloneStack *msg);
#endif

void sendCloneStackMessage(StgTSO *tso, HsStablePtr mvar);

StgStack* cloneStack(Capability* capability, StgStack* stack);
