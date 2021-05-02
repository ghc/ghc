/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2020-2021
 *
 * Stack snapshotting and decoding. (Cloning and unwinding.)
 *
 *---------------------------------------------------------------------------*/

#pragma once

extern StgClosure DLL_IMPORT_DATA_VARNAME(base_GHCziStackziCloneStack_StackSnapshot_closure);
#define StackSnapshot_constructor_closure DLL_IMPORT_DATA_REF(base_GHCziStackziCloneStack_StackSnapshot_closure)

StgStack* cloneStack(Capability* capability, const StgStack* stack);

void sendCloneStackMessage(StgTSO *tso, HsStablePtr mvar);

StgMutArrPtrs* decodeClonedStack(StgStack* stack);

#include "BeginPrivate.h"

#if defined(THREADED_RTS)
void handleCloneStackMessage(MessageCloneStack *msg);
#endif

StgWord getStackFrameCount(StgStack* stack);
StgWord getStackChunkClosureCount(StgStack* stack);
void copyPtrsToArray(StgMutArrPtrs* arr, StgStack* stack);
StgClosure* createPtrClosure(Capability* cap, StgClosure* c);
StgMutArrPtrs* allocateMutableArray(StgWord size);

#include "EndPrivate.h"
