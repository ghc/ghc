#pragma once

#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)

bool
relocateObjectCodeRISCV64(ObjectCode * oc);

#endif /* OBJETFORMAT_ELF */
