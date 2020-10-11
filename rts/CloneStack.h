#pragma once

#if defined(THREADED_RTS)
void handleCloneStackMessage(MessageCloneStack *msg);
#endif

void sendCloneStackMessage(StgTSO *tso, HsStablePtr mvar);

StgStack* cloneStack(Capability* capability, StgStack* stack);

extern StgClosure DLL_IMPORT_DATA_VARNAME(base_GHCziStackziCloneStack_StackSnapshot_closure);
#define StackSnapshot_constructor_closure DLL_IMPORT_DATA_REF(base_GHCziStackziCloneStack_StackSnapshot_closure)
