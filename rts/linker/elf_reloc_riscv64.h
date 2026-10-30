#pragma once

#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)

RTS_PUBLIC bool
relocateObjectCodeRISCV64(ObjectCode * oc);

RTS_PUBLIC void flushInstructionCacheRISCV64(ObjectCode *oc);
#endif /* OBJETFORMAT_ELF */
