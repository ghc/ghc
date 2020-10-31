/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001-2021
 *
 * Stack snapshotting.
 */

#pragma once

extern StgClosure DLL_IMPORT_DATA_VARNAME(base_GHCziStackziCloneStack_StackSnapshot_closure);
#define StackSnapshot_constructor_closure DLL_IMPORT_DATA_REF(base_GHCziStackziCloneStack_StackSnapshot_closure)

StgStack* cloneStack(Capability* capability, const StgStack* stack);
