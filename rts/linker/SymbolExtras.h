#ifndef LINKER_SYMBOL_EXTRAS_H
#define LINKER_SYMBOL_EXTRAS_H

#include "Rts.h"
#include "LinkerInternals.h"

#include "BeginPrivate.h"

#if NEED_SYMBOL_EXTRAS

int ocAllocateSymbolExtras( ObjectCode* oc, int count, int first );

#ifdef arm_HOST_ARCH
SymbolExtra* makeArmSymbolExtra( ObjectCode const* oc,
                                 unsigned long symbolNumber,
                                 unsigned long target,
                                 bool fromThumb,
                                 bool toThumb );
#else
SymbolExtra* makeSymbolExtra( ObjectCode const* oc,
                              unsigned long symbolNumber,
                              unsigned long target );

#endif /* arm_HOST_ARCH */

#endif /* NEED_SYMBOL_EXTRAS */

#include "EndPrivate.h"

#endif /* LINKER_SYMBOL_EXTRAS_H */
