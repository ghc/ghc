#pragma once

#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)

#include "elf_reloc_aarch64.h"

bool
relocateObjectCode(ObjectCode * oc);


#endif /* OBJETFORMAT_ELF */
