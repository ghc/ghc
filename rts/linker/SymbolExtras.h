#pragma once

#include "Rts.h"
#include "LinkerInternals.h"

#include "BeginPrivate.h"

#if defined(NEED_SYMBOL_EXTRAS)

int ocAllocateExtras(ObjectCode* oc, int count, int first, int bssSize);
void ocProtectExtras(ObjectCode* oc);

#if defined(arm_HOST_ARCH)
SymbolExtra* makeArmSymbolExtra( ObjectCode const* oc,
                                 unsigned long symbolNumber,
                                 unsigned long target,
                                 bool fromThumb,
                                 bool toThumb );
#elif defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH) || defined(riscv64_HOST_ARCH)
SymbolExtra* makeSymbolExtra( ObjectCode const* oc,
                              unsigned long symbolNumber,
                              unsigned long target );

#endif /* powerpc_HOST_ARCH || x86_64_HOST_ARCH */
#endif /* NEED_SYMBOL_EXTRAS */

#include "EndPrivate.h"
