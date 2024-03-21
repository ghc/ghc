#pragma once

#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)

bool
relocateObjectCodeRISCV64(ObjectCode * oc);

void flushInstructionCacheRISCV64();
#endif /* OBJETFORMAT_ELF */
