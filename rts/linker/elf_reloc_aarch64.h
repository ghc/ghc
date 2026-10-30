#pragma once

#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)

RTS_PUBLIC bool
relocateObjectCodeAarch64(ObjectCode * oc);

RTS_PUBLIC void flushInstructionCacheAarch64(ObjectCode *oc);
#endif /* OBJETFORMAT_ELF */
