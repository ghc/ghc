#pragma once

#if defined(OBJFORMAT_ELF)

#include "LinkerInternals.h"

bool
relocateObjectCodeAarch64(ObjectCode * oc);

#endif /* OBJETFORMAT_ELF */
