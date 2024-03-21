#pragma once

#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)

bool
relocateObjectCodeAarch64(ObjectCode * oc);

void flushInstructionCacheAarch64();
#endif /* OBJETFORMAT_ELF */
